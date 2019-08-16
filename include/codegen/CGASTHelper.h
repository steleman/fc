#ifndef FC_CG_AST_HELPER_H
#define FC_CG_AST_HELPER_H
#include "AST/ParserTreeCommon.h"
#include "common/Source.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include <map>

namespace fc {

class ArrayType;

using namespace ast;

// Helper calss for conversoin from AST types/ names to LLVM IR.
class CGASTHelper {
private:
  ast::ParseTree *parseTree;
  llvm::Module *TheModule;
  ASTContext &C;
  llvm::LLVMContext &LLC;
  llvm::IRBuilder<> *IRB;
  std::map<fc::ArrayType *, llvm::StructType *> dynArrMap;

  std::map<fc::StructType *, llvm::StructType *> structTypeMap;

  // Map to track functions and their return global value
  std::map<std::string, llvm::Constant *> FuncRetMap;

  // Map to keep track of orinal PU name emitted PU name
  std::map<std::string, std::string> PUNameMap;

  // This structure is to help emission of
  // nested subroutines.
  struct SubPUHelper {
    // structure type of the frame argument.
    llvm::StructType *frameTy;
    // Whether there is any frame arg.
    bool hasFrameArg;
    // What are the symbols to be passes to the
    // nested routines.
    SymbolSet set;

    SubPUHelper() : frameTy(nullptr), hasFrameArg(false) {}

    SubPUHelper(const SubPUHelper &other) {
      this->frameTy = other.frameTy;
      this->hasFrameArg = other.hasFrameArg;
      this->set = other.set;
    }
  };

  std::map<ProgramUnit *, SubPUHelper> subPUHelperMap;

  // Fortran standard
  Standard std;

public:
  explicit CGASTHelper(ast::ParseTree *tree, llvm::Module *module,
                       llvm::IRBuilder<> *IRB, Standard std);
  llvm::Type *getLLVMTypeFor(fc::Type *type);

  llvm::Type *getLLVMTypeFor(Symbol *symbol);

  llvm::Type *getLLVMTypeFor(ProgramUnit *PU, bool &hasFrameArg);

  std::string getNameForProgramUnit(ProgramUnit *PU);

  llvm::GlobalValue::LinkageTypes getLinkageTypeFor(ProgramUnit *PU);

  llvm::Function *emitDeclarationFor(ProgramUnit *sub);

  std::string getFunctionNameForSymbol(Symbol *symbol);

  std::string getGlobalSymbolName(Symbol *symbol);

  SymbolList getUsedSymbolsInChildren();

  llvm::StructType *getLLVMStructTypeFor(llvm::StringRef name, SymbolSet &set);

  SubPUHelper *getSubPUHelper(ProgramUnit *PU);

  ProgramUnit *getCalledProgramUnit(Symbol *symbol);

  llvm::Type *getLLVMTypeForDynArray(fc::ArrayType *arrTy);

  std::string getEmittedNameForPU(std::string name);

  llvm::Value *getReturnValueFor(std::string fun);

  llvm::Function *getMallocFunction();

  llvm::Function *getFreeFunction();

  unsigned getSizeForType(Type *Ty);

  llvm::Value *getSExt(llvm::Value *V);
};
} // namespace fc

#endif
