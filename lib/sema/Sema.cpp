#include "sema/Sema.h"
#include "AST/ASTContext.h"
#include "AST/ASTPass.h"
#include "llvm/Support/CommandLine.h"

using namespace fc;
using namespace ast;

llvm::cl::opt<bool> RunConstProp("const-prop",
                                 llvm::cl::desc("Run constant propagation"),
                                 llvm::cl::init(true));

bool Sema::run() {

  ASTPassManager semaPM(parseTree, "Sema Pass Manager");

  // Passes before dumping the module file.
  semaPM.addPass(createTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createExprTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createSymbolResolverPass(semaPM.getContext(), false));
  semaPM.addPass(createConstPropPass(semaPM.getContext()));
  semaPM.addPass(createParamConstPropPass(semaPM.getContext()));
  semaPM.addPass(createStmtTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createFunctionTypeUpdaterPass(semaPM.getContext()));

  // Passes after dumping the mod file.
  semaPM.addPass(createUseStmtHandlerPass(semaPM.getContext()));
  semaPM.addPass(createSymbolResolverPass(semaPM.getContext(), false));
  semaPM.addPass(createTypeUpdaterPass(semaPM.getContext(), true));
  semaPM.addPass(createPartRefSymResolverPass(semaPM.getContext()));
  semaPM.addPass(createConstPropPass(semaPM.getContext()));
  semaPM.addPass(createStmtTypeUpdaterPass(semaPM.getContext()));

  // TODO: This should be called only once. Remove earlier call after
  //       modfile dumper todo is fixed
  semaPM.addPass(createFunctionTypeUpdaterPass(semaPM.getContext()));

  // FIXME : This is a hack as, module is already dumped once in
  //         usestmthandler. UseStmtHandler should not depend on
  //         module file when the module is defined in the same file.
  //
  // Module file has to dumped after all types are resolved!
  semaPM.addPass(createModFileDumperPass(semaPM.getContext()));
  semaPM.addPass(createSymbolResolverPass(semaPM.getContext(), true));
  semaPM.addPass(createUndeclaredErrorPrinter(semaPM.getContext()));

  // Here is where the expansion of the AST starts.
  semaPM.addPass(createBinaryOpHandlerPass(semaPM.getContext()));
  semaPM.addPass(createFormatHandlerPass(semaPM.getContext()));
  semaPM.addPass(createConstructExpanderPass(semaPM.getContext()));
  semaPM.addPass(createConstPropPass(semaPM.getContext()));
  semaPM.addPass(createParamConstPropPass(semaPM.getContext()));
  semaPM.addPass(createFunctionTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createArraySectionReplacerPass(semaPM.getContext()));
  semaPM.addPass(createArraySecExpanderPass(semaPM.getContext()));
  semaPM.addPass(createDeclEliminatorPass(semaPM.getContext()));
  semaPM.addPass(createIntrinsicExpanderPass(semaPM.getContext()));
  semaPM.addPass(createArraySecExpanderPass(semaPM.getContext()));
  semaPM.addPass(createExprTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createArrBoundsIntrinExpanderPass(semaPM.getContext()));
  return semaPM.run();
}
