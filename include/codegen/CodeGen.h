#ifndef FC_CODEGEN_H_
#define FC_CODEGEN_H_

#include "AST/ASTPass.h"
#include "AST/ParserTreeCommon.h"
#include "AST/Type.h"
#include "codegen/CGDebugInfo.h"
#include "codegen/CGTBAAInfo.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include <memory>

namespace llvm {
class Module;
class Function;
class LLVMContext;
class TargetMachine;
} // namespace llvm

namespace fc {

using namespace ast;

struct RuntimeHelper;
class CGASTHelper;

class CGLoop {

private:
  llvm::BasicBlock *header;
  llvm::BasicBlock *latch;
  llvm::BasicBlock *exit;
  llvm::StringRef name;

public:
  CGLoop(llvm::BasicBlock *_header, llvm::BasicBlock *_latch,
         llvm::BasicBlock *_exit, llvm::StringRef _name)
      : header(_header), latch(_latch), exit(_exit), name(_name) {}

  llvm::BasicBlock *getHeaderBB() {
    assert(header);
    return header;
  }

  llvm::BasicBlock *getLatchBB() {
    assert(latch);
    return latch;
  }

  llvm::BasicBlock *getExitBB() {
    assert(exit);
    return exit;
  }

  llvm::StringRef getName() { return name; }
};

class CodeGen : public ASTProgramPass {

public:
  bool freezeTBAA{false}; // TODO remove this! (see emitLoadInstruction())

  struct CGContext {
    llvm::Function *currFn;
    llvm::BasicBlock *currBB;
    fc::ast::ProgramUnit *currPU;
    llvm::StringMap<llvm::Value *> symbolMap;
    // Map holds the file descripter and corresponding file unit number
    std::map<long int, llvm::Value *> FDMap;

    // Map to track the loops
    // TODO May be redundant?
    std::map<Stmt *, CGLoop *> stmtLoopMap;
    std::map<llvm::StringRef, CGLoop *> nameLoopMap;
    llvm::SmallVector<CGLoop *, 2> currLoopVector;
    llvm::GlobalVariable *ArgC;
    llvm::GlobalVariable *ArgV;
    llvm::MapVector<llvm::Value *, bool> functionAllocMap;

    bool needReturn;

    bool isParallelLoop;
    CGContext()
        : currFn(nullptr), currBB(nullptr), currPU(nullptr), needReturn(true),
          isParallelLoop(false) {}

  public:
    void reset() {
      currFn = nullptr;
      currBB = nullptr;
      currPU = nullptr;
      currLoopVector.clear();
      stmtLoopMap.clear();
      nameLoopMap.clear();
      FDMap.clear();
      symbolMap.clear();
      needReturn = true;
      isParallelLoop = false;
      functionAllocMap.clear();
    }

    llvm::Value *getLLVMValueFor(llvm::StringRef ref) {
      auto val = symbolMap.find(ref.str());
      if (val == symbolMap.end())
        return nullptr;
      return val->second;
    }

    llvm::Value *getFDFor(long int unit) { return FDMap[unit]; }
  };

  CodeGen(ASTContext &C, std::unique_ptr<llvm::Module> &M, bool EnableDebug,
          Standard std, std::unique_ptr<llvm::TargetMachine> &TM);

  void deAllocateTemps();

  bool emitProgramUnit(ProgramUnit *PU);

  void setCurrLineForDebug(SourceLoc loc);

  void emitDebugMetaForFunction();

  llvm::Function *emitFunction(Function *func);

  bool emitASTModule(ast::Module *mod);

  llvm::Value *getValue(Symbol *symbol);

  std::string getTypeForProramUnit(ProgramUnit *PU);

  // Emit specification part.
  bool emitSpecificationPart(SpecificationPart *);

  // Emit execution part.
  bool emitExecutionPart(ExecutionPart *execPart);

  bool emitExectubaleConstructList(StmtList &stmtList);

  // Emit EntityDecl
  bool emitEntityDecl(EntityDecl *entityDecl);

  bool createGlobalExternForSymbol(Symbol *sym);

  /// \brief when \p constuctOnlyConstant is true, global constant value
  /// of type \p lhsTy is returned instead of GlobalVariable.
  llvm::Value *emitConstant(llvm::ArrayRef<llvm::StringRef> valueList,
                            fc::Type *type, fc::Type *lhsTy = nullptr,
                            bool constuctOnlyConstant = false);

  llvm::Value *emitSizeForArrBounds(ArrayBounds &);

  // Emit array-element represented by \p arrEle. Set \p addr if this array
  // belongs to some GEP'ed address (eg. in the case of array within a
  // struct-comp), ie. the llvm-arry for this arrEle is in \p addr.
  // This signature is followed by the analogous emitXXXArrayElement() APIs.
  llvm::Value *emitArrayElement(ArrayElement *arrEle, bool isLHS = false,
                                llvm::Value *addr = nullptr);

  llvm::Value *emitStaticArrayElement(ArrayElement *expr, bool isLHS = false,
                                      llvm::Value *addr = nullptr);

  llvm::Value *emitDynArrayElement(ArrayElement *expr, bool isLHS = false,
                                   llvm::Value *addr = nullptr);

  llvm::Value *emitf77DynArrayElement(ArrayElement *expr, bool isLHS = false,
                                      llvm::Value *addr = nullptr);

  llvm::Value *emitExpression(Expr *expr, bool isLHS = false);

  bool emitNullifyStmt(NullifyStmt *stmt);

  bool emitAllocateStmt(AllocateStmt *stmt);

  bool emitDeAllocateStmt(DeAllocateStmt *stmt);

  bool emitAssignment(AssignmentStmt *stmt);

  bool emitPointerAssignment(PointerAssignmentStmt *stmt);

  bool emitAssignment(llvm::Value *lhs, llvm::Value *rhs);

  bool emitStopStmt(StopStmt *stmt);

  bool emitPrintStmt(PrintStmt *stmt);

  bool emitWriteStmt(WriteStmt *stmt);

  bool emitReadStmt(ReadStmt *stmt);

  bool emitInternalReadStmt(ReadStmt *stmt);

  bool emitInternalWriteStmt(WriteStmt *stmt);

  bool emitOpenStmt(OpenStmt *open);

  bool emitCloseStmt(CloseStmt *close);

  bool emitIfElseStmt(IfElseStmt *stmt);

  bool emitDoWhileStmt(DoWhileStmt *stmt);

  bool emitDoStmt(DoStmt *stmt);

  bool emitCallStmt(CallStmt *stmt);

  llvm::Value *expandIntrinsic(FunctionReference *ref);

  llvm::CallInst *emitCall(Symbol *symbol, ExprList &exprList,
                           bool isSubroutineCall = false);

  bool emitMemCpy(CallStmt *stmt);

  bool emitCycleStmt(CycleStmt *stmt);

  bool emitExitStmt(ExitStmt *Stmt);

  bool updatesymbolMapForIntentArg(Symbol *);

  void updateSymbolMapForFuncArg(Symbol *);

  llvm::Value *getArrDimSizeVal(Expr *expr, llvm::Value *exprVal);

  llvm::Value *castIntToFP(llvm::Value *val, llvm::Type *castToTy);

  llvm::Value *getLLVMBinaryOp(llvm::Value *lhsVal, llvm::Value *rhsVal,
                               BinaryOpKind opKind);

  llvm::Value *getLLVMRelationalOp(llvm::Value *lhsVal, llvm::Value *rhsVal,
                                   RelationalOpKind opKind);

  llvm::Value *getLLVMLogicalOp(llvm::Value *lhsVal, llvm::Value *rhsVal,
                                LogicalOpKind opKind);

  llvm::Value *emitStrCmp(llvm::Value *lhs, llvm::Value *rhsVal,
                          RelationalOpKind opKind);

  llvm::BasicBlock *getNewBlock(llvm::StringRef name, bool insertToFn = false);

  llvm::AllocaInst *createAlloca(llvm::Type *type, llvm::StringRef name);

  llvm::AllocaInst *createAlloca(Symbol *symbol);

  void visitLLVMModule();

  llvm::Function *dumpMain(fc::Function *fcMain);

  bool emitFunctionDeclaration(ProgramUnit *PU);

  bool runOnProgram(ParseTree *parseTree) override;

  bool constructFrameArgForProgramUnit();

  void nullifyPointerAlloca(Type *type, llvm::Value *addr);

  bool updateArgForNestProgramUnit();

  llvm::LoadInst *emitStructLoadInst(llvm::Value *V, llvm::Value *structPtr,
                                     llvm::Type *structTy, uint64_t offset,
                                     const llvm::Twine &Name = "load");

  llvm::LoadInst *emitLoadInstruction(llvm::Value *V,
                                      const llvm::Twine &Name = "load",
                                      bool disableTBAA = false);

  void emitStructStoreInst(llvm::Value *V, llvm::Value *Ptr,
                           llvm::Value *structPtr, llvm::Type *structTy,
                           uint64_t offset);

  void emitStoreInstruction(llvm::Value *V, llvm::Value *Ptr,
                            bool disableTBAA = false);

  llvm::Argument *getIntentArgForSymbol(Symbol *symbol);

  llvm::Value *getArgumentFor(llvm::Value *currArg, fc::Type *currArgTy,
                              fc::Type *fcDummyArgTy, llvm::Function *llFn,
                              unsigned argNum);

  llvm::Value *getDynamicArrayFor(llvm::Value *val, fc::ArrayType *staticArrTy,
                                  fc::ArrayType *dynArrTy);
  llvm::Value *emitStructureComponent(fc::StructureComponent *structComp,
                                      bool isLHS);

private:
  std::unique_ptr<llvm::Module> &TheModule;
  ParseTree *parseTree;
  llvm::LLVMContext *LLContext;
  llvm::IRBuilder<> *IRB;
  CGContext context;
  RuntimeHelper *runtimeHelper;
  std::unique_ptr<llvm::TargetMachine> &TM;
  CGASTHelper *cgHelper;
  bool EnableDebug;
  CGDebugInfo *debugHelper;
  CGTBAAInfo *tbaaHelper;
  llvm::DIBuilder *diBuilder;
  Standard std;
};

} // namespace fc

#endif // FC_CODEGEN_H_
