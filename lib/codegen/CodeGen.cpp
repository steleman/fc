
#include "codegen/CodeGen.h"
#include "AST/Declaration.h"
#include "AST/ParserTreeCommon.h"
#include "AST/ProgramUnit.h"
#include "AST/Stmt.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "codegen/CGASTHelper.h"
#include "codegen/LLVMUtil.h"
#include "codegen/RuntimeHelper.h"
#include "common/Debug.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Support/FileSystem.h"

#include "llvm-c/Target.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace fc;
using namespace ast;
using namespace llvm;

CodeGen::CodeGen(ASTContext &C, std::unique_ptr<llvm::Module> &M,
                 bool EnableDebug, Standard std,
                 std::unique_ptr<llvm::TargetMachine> &TM)
    : ASTProgramPass(C, "LLVM CodeGenerator Pass"), TheModule(M), TM(TM),
      EnableDebug(EnableDebug), std(std) {
  LLContext = &TheModule->getContext();
}

llvm::LoadInst *CodeGen::emitStructLoadInst(llvm::Value *V,
                                            llvm::Value *structPtr,
                                            llvm::Type *structTy,
                                            uint64_t offset,
                                            const ::llvm::Twine &Name) {

  auto structPtrTy = llvm::dyn_cast<llvm::PointerType>(structTy);
  assert(structPtrTy);
  auto ptrTy = llvm::dyn_cast<llvm::PointerType>(V->getType());
  assert(ptrTy && "Loading a non pointer value");

  auto LI = IRB->CreateLoad(V, Name);
  if (context.isParallelLoop) {
    LI->setMetadata(LLVMContext::MD_mem_parallel_loop_access,
                    llvm::MDNode::get(LI->getContext(), {}));
  }

  if (auto MDN =
          tbaaHelper->getTBAAForStruct(structPtr, structPtrTy->getElementType(),
                                       offset, ptrTy->getElementType())) {
    LI->setMetadata(LLVMContext::MD_tbaa, MDN);
  }

  return LI;
}

void CodeGen::emitStructStoreInst(llvm::Value *V, llvm::Value *Ptr,
                                  llvm::Value *structPtr, llvm::Type *structTy,
                                  uint64_t offset) {
  auto structPtrTy = llvm::dyn_cast<llvm::PointerType>(structTy);
  assert(structPtrTy);
  auto ptrTy = llvm::dyn_cast<llvm::PointerType>(Ptr->getType());
  assert(ptrTy && "Storing to  a non pointer value");

  auto SI = IRB->CreateStore(V, Ptr);
  if (context.isParallelLoop) {
    SI->setMetadata(LLVMContext::MD_mem_parallel_loop_access,
                    llvm::MDNode::get(SI->getContext(), {}));
  }

  if (auto MDN =
          tbaaHelper->getTBAAForStruct(structPtr, structPtrTy->getElementType(),
                                       offset, ptrTy->getElementType())) {
    SI->setMetadata(LLVMContext::MD_tbaa, MDN);
  }
}

llvm::LoadInst *CodeGen::emitLoadInstruction(llvm::Value *V,
                                             const llvm::Twine &Name,
                                             bool disableTBAA) {

  // TODO: remove this. Temporarily doing this since getting "Old-style TBAA is
  // no longer allowed, use struct-path TBAA instead" error during some
  // arr-in-struct accesss
  if (freezeTBAA)
    disableTBAA = true;

  auto ptrTy = llvm::dyn_cast<llvm::PointerType>(V->getType());
  assert(ptrTy && "Loading a non pointer value");

  auto LI = IRB->CreateLoad(V, Name);
  if (context.isParallelLoop) {
    LI->setMetadata(LLVMContext::MD_mem_parallel_loop_access,
                    llvm::MDNode::get(LI->getContext(), {}));
  }

  auto Ptr = llvm::GetUnderlyingObject(V, TheModule->getDataLayout());
  /*
  if (auto MDN = tbaaHelper->getTBAANodeForType(ptrTy->getElementType()))
    LI->setMetadata(LLVMContext::MD_tbaa, MDN);
  */

  if (auto MDN = tbaaHelper->getTBAANodeForPtr(Ptr)) {
    if (!disableTBAA)
      LI->setMetadata(LLVMContext::MD_tbaa, MDN);
  }

  return LI;
}

void CodeGen::emitStoreInstruction(llvm::Value *V, llvm::Value *Ptr,
                                   bool disableTBAA) {

  auto ptrTy = llvm::dyn_cast<llvm::PointerType>(Ptr->getType());
  assert(ptrTy && "Storing to  a non pointer value");

  auto SI = IRB->CreateStore(V, Ptr);
  if (context.isParallelLoop) {
    SI->setMetadata(LLVMContext::MD_mem_parallel_loop_access,
                    llvm::MDNode::get(SI->getContext(), {}));
  }

  auto VPtr = llvm::GetUnderlyingObject(Ptr, TheModule->getDataLayout());
  /*
  if (auto MDN = tbaaHelper->getTBAANodeForType(ptrTy->getElementType()))
    SI->setMetadata(LLVMContext::MD_tbaa, MDN);
  */

  if (auto MDN = tbaaHelper->getTBAANodeForPtr(VPtr)) {
    if (!disableTBAA)
      SI->setMetadata(LLVMContext::MD_tbaa, MDN);
  }
}

void CodeGen::setCurrLineForDebug(SourceLoc loc) {
  if (EnableDebug)
    IRB->SetCurrentDebugLocation(debugHelper->getLoc(loc));
}

BasicBlock *CodeGen::getNewBlock(StringRef name, bool insertToFn) {
  auto BB = BasicBlock::Create(*LLContext, name);
  if (insertToFn)
    BB->insertInto(context.currFn);
  return BB;
}

llvm::Value *CodeGen::getValue(Symbol *symbol) {
  auto parenSym = symbol->getParentSymbol();
  if (parenSym)
    symbol = parenSym;

  return context.symbolMap[symbol->getName()];
}

llvm::AllocaInst *CodeGen::createAlloca(llvm::Type *type,
                                        llvm::StringRef name) {

  auto *I = LLVMUtil::getFirstNonAllocaInst(&context.currFn->getEntryBlock());
  auto alloca = new AllocaInst(type, 0, name, I);
  if (I == nullptr)
    context.currFn->getEntryBlock().getInstList().push_back(alloca);

  return alloca;
}

llvm::AllocaInst *CodeGen::createAlloca(Symbol *symbol) {
  auto llType = cgHelper->getLLVMTypeFor(symbol);
  auto *I = LLVMUtil::getFirstNonAllocaInst(&context.currFn->getEntryBlock());
  auto alloca = new AllocaInst(llType, 0, symbol->getName(), I);
  if (I == nullptr)
    context.currFn->getEntryBlock().getInstList().push_back(alloca);

  if (EnableDebug) {

    debugHelper->getLocalVariable(alloca, symbol, -1,
                                  &context.currFn->getEntryBlock());
  }

  if (symbol->getType()->isDynArrayTy()) {
    llvm::Value *isAllocated = IRB->CreateStructGEP(nullptr, alloca, 2, "flag");
    emitStoreInstruction(IRB->getFalse(), isAllocated, true);
  }
  return alloca;
}

llvm::Argument *CodeGen::getIntentArgForSymbol(Symbol *symbol) {
  unsigned argNum = 0;
  assert(context.currPU->isSubroutine() || context.currPU->isFunction());
  auto sub = static_cast<Function *>(context.currPU);

  auto argsList = sub->getArgsList();
  for (auto arg : argsList) {
    if (arg.compare_lower(symbol->getName()) == 0)
      break;
    argNum++;
  }

  bool hasIntentArg = false;
  if (context.currPU->isNestedUnit()) {
    auto subPUHelper = cgHelper->getSubPUHelper(context.currPU->getParent());
    if (subPUHelper && subPUHelper->hasFrameArg) {
      hasIntentArg = true;
    }
  }

  unsigned I = 0;
  for (auto &LLArg : context.currFn->args()) {
    if (hasIntentArg && I == 0) {
      hasIntentArg = false;
      continue;
    }
    if (I == argNum) {
      return &LLArg;
    }
    I++;
  }
  return nullptr;
}

bool CodeGen::updatesymbolMapForIntentArg(Symbol *symbol) {
  auto Arg = getIntentArgForSymbol(symbol);
  assert(Arg);
  context.symbolMap[symbol->getName()] = Arg;
  return true;
}

void CodeGen::updateSymbolMapForFuncArg(Symbol *symbol) {

  assert(context.currPU->isFunction());
  auto func = static_cast<Function *>(context.currPU);

  unsigned argNum = 0;
  auto argsList = func->getArgsList();
  for (auto arg : argsList) {
    if (arg.compare_lower(symbol->getName()) == 0)
      break;
    argNum++;
  }

  unsigned i = 0;
  llvm::Argument *Arg;
  for (auto &LLArg : context.currFn->args()) {
    if (i == argNum) {
      Arg = &LLArg;
      break;
    }
    ++i;
  }
  context.symbolMap[symbol->getName()] = Arg;
}

// This function handles the initializer for all the local variables.
bool CodeGen::emitEntityDecl(EntityDecl *entityDecl) {
  auto sym = entityDecl->getSymbol();
  auto LLVar = context.getLLVMValueFor(sym->getName());
  assert(LLVar && "Could not find LLVM value allocated!");

  if (sym->getAllocKind() == AllocationKind::Argument) {
    return true;
  }

  auto Init = entityDecl->getInit();
  llvm::Value *InitExpr = nullptr;
  fc::Type *Ty = sym->getType();
  if (Init) {
    if (auto Const = llvm::dyn_cast<ConstantVal>(Init)) {
      auto type = Const->getType();
      if (type->isArrayTy()) {
        llvm::SmallVector<llvm::StringRef, 2> constList;
        for (auto &val : Const->getConstant()->getArrValue()) {
          constList.push_back(val);
        }
        InitExpr = emitConstant(constList, Const->getType(), Ty, true);
      } else {
        InitExpr =
            emitConstant(Const->getValueRef(), Const->getType(), Ty, true);
      }
    } else {
      InitExpr = emitExpression(Init, false);
    }
  }

  switch (sym->getAllocKind()) {
  case StaticLocal:
    if (InitExpr) {
      emitStoreInstruction(InitExpr, LLVar);
    }
    break;
  case StaticGlobal: {
    llvm::Constant *InitValue = nullptr;
    auto gVar = llvm::dyn_cast<llvm::GlobalVariable>(LLVar);
    assert(gVar);
    auto LLType = gVar->getValueType();
    if (!InitExpr) {
      InitValue = llvm::Constant::getNullValue(LLType);
    } else {
      InitValue = llvm::dyn_cast<llvm::Constant>(InitExpr);
    }

    if (!InitValue) {
      error() << "Init expression for global variable is not constant: "
              << sym->getName() << "\n";
      llvm_unreachable("NOn constant init");
    }
    gVar->setInitializer(InitValue);
    break;
  }
  default:
    llvm_unreachable("unhandled Allocation type");
  };
  return true;
}

bool CodeGen::createGlobalExternForSymbol(Symbol *symbol) {
  // These are mostly extern global symbols.
  assert(symbol->getAllocKind() == StaticGlobal);
  auto LLTy = cgHelper->getLLVMTypeFor(symbol->getType());

  auto name = cgHelper->getGlobalSymbolName(symbol);

  auto var = TheModule->getOrInsertGlobal(name, LLTy);
  if (symbol->getAttr().linkKind == fc::Link_Internal) {
    auto gVar = llvm::cast<GlobalVariable>(var);
    gVar->setLinkage(llvm::GlobalValue::PrivateLinkage);
  }

  context.symbolMap[symbol->getName()] = var;
  return true;
}

bool CodeGen::constructFrameArgForProgramUnit() {
  // Look if frame argument needs to be constructed for the nested routines.
  auto helper = cgHelper->getSubPUHelper(context.currPU);
  if (!helper || !helper->hasFrameArg)
    return true;

  auto structName = "FRAME." + context.currPU->getName().str();
  // Allocate the structure type.
  auto structAlloca = createAlloca(helper->frameTy, structName);

  // Now fill the structure with all the corresponding symbols.
  unsigned I = 0;
  for (auto sym : helper->set) {
    auto name = helper->frameTy->getStructName();
    auto GEP = IRB->CreateStructGEP(helper->frameTy, structAlloca, I,
                                    name + "." + sym->getName());

    if (sym->hasIntent()) {
      auto Arg = getIntentArgForSymbol(sym);
      assert(Arg);
      IRB->CreateStore(Arg, GEP, false);
      GEP = Arg;
    }
    context.symbolMap[sym->getName()] = GEP;
    I++;
  }
  context.symbolMap[structName] = structAlloca;
  return true;
}

bool CodeGen::updateArgForNestProgramUnit() {
  // If it is nested unit, update the symbol map to use the
  // structure loads.
  if (!context.currPU->isNestedUnit())
    return true;
  auto parent = context.currPU->getParent();
  assert(parent);
  auto subPUHelper = cgHelper->getSubPUHelper(parent);
  if (!subPUHelper || !subPUHelper->hasFrameArg)
    return true;

  llvm::Argument *Struct = &*context.currFn->args().begin();
  assert(Struct);
  unsigned I = 0;
  for (auto sym : subPUHelper->set) {

    // Some program units may use the local values for the
    // same symbol name. For them, do not use the parent symbol
    // value.
    auto currPUSym =
        context.currPU->getSymbolTable()->getSymbol(sym->getName());
    if (currPUSym && currPUSym->getParentSymbol() == nullptr) {
      I++;
      continue;
    }
    auto GEP = IRB->CreateStructGEP(subPUHelper->frameTy, Struct, I,
                                    sym->getName() + ".ptr");

    if (sym->hasIntent()) {
      GEP = IRB->CreateLoad(GEP, sym->getName());
    }
    context.symbolMap[sym->getName()] = GEP;
    I++;
  }
  return true;
}

// Recursive function to nullify pointers that "belong" to \p addr that is
// associated with fc::Type \p type.
void CodeGen::nullifyPointerAlloca(Type *type, llvm::Value *addr) {
  if (type->isPointerTy()) {
    emitStoreInstruction(
        llvm::Constant::getNullValue(addr->getType()->getPointerElementType()),
        addr, /*disableTBAA*/ true);

  } else if (auto structTy = llvm::dyn_cast<StructType>(type)) {
    llvm::SmallVector<Type *, 2> memberTypes = structTy->getTypeList();
    unsigned i = 0;
    for (Type *memberType : memberTypes) {
      if (memberType->isPointerTy() || memberType->isStructTy())
        nullifyPointerAlloca(memberType,
                             IRB->CreateStructGEP(nullptr, addr, i));
      ++i;
    }
  }
}

bool CodeGen::emitSpecificationPart(SpecificationPart *specPart) {
  // If there are sub programs which accesses the current routine
  // variables , allocate them together in a frame struct.
  if (!constructFrameArgForProgramUnit())
    return false;

  // TODO May be not necessary
  bool isFunction = context.currPU->isFunction();

  // Process remaining symbols which needs memory allocation.
  auto currSymTable = context.currPU->getSymbolTable();
  for (auto sym : currSymTable->getSymbolList()) {

    setCurrLineForDebug(sym->getSourceLoc());

    // Already allocated..
    if (context.getLLVMValueFor(sym->getName()) != nullptr)
      continue;

    // Point to the right argument.
    if (sym->hasIntent()) {
      updatesymbolMapForIntentArg(sym);
      continue;
    }

    if (sym->getAllocKind() == StaticLocal) {
      if (std == Standard::f77) {

        // If it is a dynamic array type in f77. Ignore .
        // Will be malloc'ed later.
        if (sym->getType()->isDynArrayTy() &&
            sym->getParentSymbol() == nullptr) {
          continue;
        }
      }
      auto Alloca = createAlloca(sym);

      nullifyPointerAlloca(sym->getType(), Alloca);

      context.symbolMap[sym->getName()] = Alloca;
      continue;
    }

    // Function types needs no allocation here.
    if (sym->getType()->isFunctionTy())
      continue;

    if (sym->getType()->isUndeclared()) {
      auto parentSym = sym->getParentSymbol();
      if (sym->getType()->isUndeclaredFnTy() && !parentSym) {
        continue;
      }
      assert(parentSym);
      if (parentSym->getType()->isFunctionTy())
        continue;
    }

    // These are mostly extern global symbols.
    if (sym->getAllocKind() == StaticGlobal) {
      if (!createGlobalExternForSymbol(sym))
        return false;
      continue;
    }

    if (sym->getAllocKind() == Argument && isFunction) {
      updateSymbolMapForFuncArg(sym);
      continue;
    }

    // If these are local symbols in the parent
    // routine, ignore it for now. Handled, below.
    auto parentSym = sym->getParentSymbol();
    assert(parentSym);
    sym = parentSym;

    if (sym->getAllocKind() == StaticGlobal) {
      if (!createGlobalExternForSymbol(sym)) {
        return false;
      }
      continue;
    }

    assert(context.currPU->isNestedUnit());
    assert(sym->getSymTable() == context.currPU->getParent()->getSymbolTable());
  }

  if (context.currPU->isNestedUnit()) {
    if (!updateArgForNestProgramUnit()) {
      return false;
    }
  }

  if (!specPart || !specPart->getBlock())
    return true;

  for (auto stmt : specPart->getBlock()->getStmtList()) {
    // DeclEliminatorPass should have removed entityDecls for program-units
    // that doesn't have global-vars (like module, subroutine/function with a
    // child etc.)
    if (auto entityDecl = llvm::dyn_cast<EntityDecl>(stmt)) {
      if (!emitEntityDecl(entityDecl)) {
        return false;
      }
    }
  }

  return true;
}

void CodeGen::emitDebugMetaForFunction() {

  auto PU = context.currPU;
  auto Fn = context.currFn;

  auto EntryBB = &Fn->getEntryBlock();
  auto sym = PU->getSymbolTable()->getParent()->getSymbol(PU->getName());
  assert(sym);

  auto subProg =
      debugHelper->getSubProgram(context.currFn, sym->getSourceLoc());
  debugHelper->pushScope(subProg);

  Fn->setMetadata("dbg", subProg);
  // Now emit arguments.
  auto I = 1;
  for (auto &arg : Fn->args()) {
    auto sym = PU->getSymbolTable()->getSymbol(arg.getName());
    if (!sym) {
      I++;
      continue;
    }
    debugHelper->getLocalVariable(&arg, sym, I, EntryBB);
    I++;
  }
  setCurrLineForDebug(sym->getSourceLoc());
}

bool CodeGen::emitFunctionDeclaration(ProgramUnit *PU) {
  if (llvm::isa<fc::Function>(PU)) {
    cgHelper->emitDeclarationFor(PU);
  }
  for (auto subPU : PU->getProgramUnitList()) {
    emitFunctionDeclaration(subPU);
  }
  return true;
}

// Add target attributes for all the functions in the module.
void CodeGen::visitLLVMModule() {

  // Add TargetAttributes for all the functions.
  for (llvm::Function &Fn : *(TheModule.get())) {
    if (Fn.isDeclaration())
      continue;
    Fn.addFnAttr("target-features", TM->getTargetFeatureString());
    Fn.addFnAttr("target-cpu", TM->getTargetCPU());

    Fn.addFnAttr("no-frame-pointer-elim", "false");
    Fn.addFnAttr(llvm::Attribute::NoUnwind);
    Fn.addFnAttr(llvm::Attribute::UWTable);
    Fn.addFnAttr("correctly-rounded-divide-sqrt-fp-math", "false");
    Fn.addFnAttr("less-precise-fpmad", "false");
    Fn.addFnAttr("no-infs-fp-math", "true");
    Fn.addFnAttr("no-nans-fp-math", "true");
    Fn.addFnAttr("no-signed-zeros-fp-math", "true");
    Fn.addFnAttr("no-trapping-math", "true");
    Fn.addFnAttr("unsafe-fp-math", "true");
    Fn.addFnAttr("use-soft-float", "false");
  }
}

llvm::Function *CodeGen::dumpMain(fc::Function *fcMain) {

  // Find the main program.
  auto MainProgFn = emitFunction(fcMain);
  assert(MainProgFn);

  auto Int32 = IRB->getInt32Ty();
  auto I8PtrTy = IRB->getInt8PtrTy()->getPointerTo();

  auto argcInit = llvm::Constant::getNullValue(Int32);
  auto argVInit = llvm::Constant::getNullValue(I8PtrTy);

  context.ArgC->setLinkage(llvm::GlobalValue::InternalLinkage);
  context.ArgV->setLinkage(llvm::GlobalValue::InternalLinkage);
  context.ArgC->setInitializer(argcInit);
  context.ArgV->setInitializer(argVInit);

  // Create a main program with i32 return type and no arguments
  auto FnType = llvm::FunctionType::get(
      IntegerType::get(TheModule->getContext(), 32), {Int32, I8PtrTy}, false);
  auto mainFn = llvm::Function::Create(
      FnType, llvm::GlobalValue::ExternalLinkage, "main", TheModule.get());

  auto EntryBB = BasicBlock::Create(*LLContext, "entry", mainFn);
  IRB->SetInsertPoint(EntryBB);

  Value *arg0 = nullptr, *arg1 = nullptr;
  unsigned i = 0;
  for (auto &LLArg : mainFn->args()) {
    if (i == 0) {
      arg0 = &LLArg;
      i++;
      continue;
    }

    if (i == 1) {
      arg1 = &LLArg;
      break;
    }
    llvm_unreachable("Never reached");
  }

  assert(arg0 && arg1);

  emitStoreInstruction(arg0, context.ArgC);
  emitStoreInstruction(arg1, context.ArgV);

  auto Call = IRB->CreateCall(MainProgFn);
  IRB->CreateRet(Call);

  return MainProgFn;
}

bool CodeGen::emitASTModule(ast::Module *module) {
  emitSpecificationPart(module->getSpec());
  return true;
}

void CodeGen::deAllocateTemps() {
  auto freeFn = cgHelper->getFreeFunction();
  for (auto II = context.functionAllocMap.begin();
       II != context.functionAllocMap.end(); ++II) {
    if (II->second) {
      auto ptr = IRB->CreateBitCast(II->first, IRB->getInt8PtrTy());
      IRB->CreateCall(freeFn, {ptr});
      II->second = false;
    }
  }
}

llvm::Function *CodeGen::emitFunction(fc::Function *func) {

  auto name = cgHelper->getNameForProgramUnit(func);
  auto fn = (TheModule->getFunction(name));
  assert(fn);

  auto EntryBB = BasicBlock::Create(*LLContext, "entry", fn);
  IRB->SetInsertPoint(EntryBB);

  context.currBB = EntryBB;
  context.currFn = fn;

  if (EnableDebug)
    emitDebugMetaForFunction();

  emitSpecificationPart(func->getSpec());

  emitExecutionPart(func->getExecPart());

  if (func->isSubroutine()) {
    IRB->CreateRetVoid();
  } else if (func->isMainProgram()) {
    if (context.needReturn)
      IRB->CreateRet(IRB->getInt32(0));
  } else {
    auto returnVal = context.getLLVMValueFor(func->getName());
    assert(returnVal);
    auto LI = IRB->CreateLoad(returnVal);
    // emitStoreInstruction(LI, returnGV);
    IRB->CreateRet(LI);
  }

  IRB->SetInsertPoint(context.currBB->getTerminator());
  deAllocateTemps();
  IRB->SetInsertPoint(context.currBB);

  if (EnableDebug)
    debugHelper->popScope();

  return fn;
}

bool CodeGen::emitProgramUnit(ProgramUnit *PU) {

  tbaaHelper->resetTBAA();
  tbaaHelper->initTBAA(PU->getName());
  llvm::StringRef fnName = "";
  llvm::Function *Fn = nullptr;
  auto kind = PU->getKind();
  context.currPU = PU;
  switch (kind) {
  case ProgramUnitKind::SubroutineKind:
  case ProgramUnitKind::FunctionKind: {
    auto function = static_cast<Function *>(PU);
    Fn = emitFunction(function);
    fnName = Fn->getName();
    assert(Fn);
    break;
  }

  case ProgramUnitKind::ModuleKind: {
    auto Mod = static_cast<Module *>(PU);
    fnName = Mod->getName();
    if (!emitASTModule(Mod)) {
      error() << "Error during module emission\n";
      return false;
    }
    break;
  }
  case ProgramUnitKind::MainProgramKind: {
    auto fcMain = static_cast<fc::Function *>(PU);
    fnName = fcMain->getName();
    Fn = dumpMain(fcMain);
    if (!Fn) {
      error() << "\n Error during MainProgram emission\n";
      return false;
    }
    break;
  }
  default:
    llvm_unreachable("unknown program unit found");
  };

  // Reset all the context info.
  context.reset();

  // Now emit all the nested subroutines.
  for (auto subPU : PU->getProgramUnitList()) {
    context.currPU = PU;
    if (!emitProgramUnit(subPU)) {
      error() << "\n Error during sub program emission\n";
      return false;
    }
  }
  return true;
}

bool CodeGen::runOnProgram(ParseTree *parseTree) {
  this->parseTree = parseTree;

  // Set the IR builder for the target.
  IRBuilder<> TheBuilder(*LLContext);
  IRB = &TheBuilder;
  llvm::FastMathFlags flags;
  flags.setFast();
  IRB->setFastMathFlags(flags);
  DIBuilder diBuild(*TheModule);
  this->diBuilder = &diBuild;

  // Set the codegen helper class.
  CGASTHelper cgHelper(parseTree, TheModule.get(), IRB, std);
  this->cgHelper = &cgHelper;

  llvm::DICompileUnit *compileUnit = nullptr;
  if (EnableDebug) {
    llvm::SmallVector<char, 32> currDir;
    llvm::sys::fs::current_path(currDir);
    auto diFile = diBuilder->createFile(
        parseTree->getName(), llvm::StringRef(currDir.begin(), currDir.size()));
    compileUnit = diBuilder->createCompileUnit(
        std == f77 ? dwarf::DW_LANG_Fortran77 : dwarf::DW_LANG_Fortran95,
        diFile, "fc 0.1", true, "", 1, "");
  }
  CGDebugInfo cgDebugInfo(&diBuild, &cgHelper, compileUnit);
  this->debugHelper = &cgDebugInfo;

  debugHelper->pushScope(compileUnit);

  // Initialize tbaa helper
  this->tbaaHelper = new CGTBAAInfo(TheModule.get());

  auto Int32 = IRB->getInt32Ty();
  auto I8PtrTy = IRB->getInt8PtrTy()->getPointerTo();

  auto argc = TheModule->getOrInsertGlobal("fc.internal.argc", Int32);
  auto argv = TheModule->getOrInsertGlobal("fc.internal.argv", I8PtrTy);

  context.ArgC = llvm::cast<GlobalVariable>(argc);
  context.ArgV = llvm::cast<GlobalVariable>(argv);

  // Set runtime helper class.
  RuntimeHelper runtimeHelper(TheModule, *LLContext);
  this->runtimeHelper = &runtimeHelper;

  // Emit all subroutine declarations.
  for (auto PU : parseTree->getProgramUnitList()) {
    if (!emitFunctionDeclaration(PU)) {
      error() << "LLVM CG: Failed to emit Subroutine declarations\n";
      return false;
    }
  }

  // Emit all Program units now.
  for (auto PU : parseTree->getProgramUnitList()) {
    if (!emitProgramUnit(PU)) {
      error() << "\n Error during external PU emission\n";
      return false;
    }
  }

  if (EnableDebug)
    diBuilder->finalize();
  // Post processing module.
  visitLLVMModule();

  // Verify the LLVM IR generated.
  bool broken = llvm::verifyModule(*TheModule.get(), &error());
  if (broken)
    return false;

  for (auto &Fn : *TheModule) {
    broken |= (llvm::verifyFunction(Fn, &error()));
  }
  return !broken;
}
