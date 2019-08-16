#include "AST/ASTContext.h"
#include "AST/ProgramUnit.h"
#include "codegen/CodeGen.h"
#include "common/Debug.h"
#include "common/Diagnostics.h"
#include "common/Source.h"
#include "lex/Lexer.h"
#include "parse/Parser.h"
#include "sema/Sema.h"

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include <fstream>
#include <streambuf>
#include <string>

using namespace llvm;

using namespace fc;

cl::opt<std::string> InputFilename(cl::Positional, cl::desc("<input file>"),
                                   cl::init("-"));

enum Action {
  EmitRawAST,
  EmitAST,
  EmitLLVM,
  EmitBC,
  EmitASM,
  EmitExe,
};

cl::opt<Action>
    ActionOpt(cl::desc("Choose IR Type:"),
              cl::values(clEnumValN(EmitAST, "emit-ast",
                                    "Emit AST (after semantic checks)"),
                         clEnumValN(EmitRawAST, "emit-raw-ast",
                                    "Emit AST (before semantic checks)"),
                         clEnumValN(EmitLLVM, "emit-llvm", "Emit LLVM IR"),
                         clEnumValN(EmitBC, "emit-bc", "Emit LLVM BC"),
                         clEnumValN(EmitASM, "emit-asm", "Emit ASM")),
              cl::init(EmitExe));

cl::opt<Standard>
    FortranStandard("std", cl::desc("Choose fortran standard:"),
                    cl::values(clEnumVal(f77, "Fortran77 standard"),
                               clEnumVal(f95, "Fortran95 standard (default)")),
                    cl::init(Standard::None));

enum OptLevel {
  O0 = 0,
  O1,
  O2,
  O3,
};

cl::opt<OptLevel> OptimizationLevel(
    cl::desc("Choose optimization level:"),
    cl::values(clEnumVal(O0, "No optimization"),
               clEnumVal(O1, "Enable trivial optimizations"),
               clEnumVal(O2, "Enable default optimizations"),
               clEnumVal(O3, "Enable expensive optimizations")),
    cl::init(O0));

cl::opt<std::string> OutputFilename("o", cl::desc("Specify output filename"),
                                    cl::value_desc("filename"), cl::init(""));

cl::opt<std::string> RuntimePath("L", cl::desc("Specify FC runtime path"),
                                 cl::value_desc("<path-to-fc-runtime>"),
                                 cl::init(""));

llvm::cl::opt<bool> StopAtCompile("c", llvm::cl::desc("stop at compilation"),
                                  llvm::cl::init(false));

llvm::cl::opt<bool> EnableDebug("g", llvm::cl::desc("Enable debugging symbols"),
                                llvm::cl::init(false));

llvm::cl::opt<bool> DumpVersion("v", llvm::cl::desc("Version check"),
                                llvm::cl::init(false));

llvm::cl::opt<bool> PrepareForLTO("flto", llvm::cl::desc("Prepare for LTO"),
                                  llvm::cl::init(false));

llvm::cl::opt<std::string> MArchName("march",
                                     cl::desc("Specify target architecture"),
                                     cl::value_desc("marchname"), cl::init(""));

static bool prepareLLVMTarget(std::unique_ptr<llvm::TargetMachine> &TM,
                              llvm::Module *TheModule) {
  // set LLVM target triple.
  // Default to x86_64 for now.
  auto TargetTriple = sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);

  std::string Error;
  std::string Triple = TheModule->getTargetTriple();
  const llvm::Target *TheTarget = TargetRegistry::lookupTarget(Triple, Error);
  if (!TheTarget) {
    error() << "\n could not find target for triple " << Triple;
    return false;
  }

  llvm::Optional<CodeModel::Model> CM = llvm::CodeModel::Small;
  llvm::Optional<Reloc::Model> RM = llvm::Reloc::Static;
  auto OptLevel = CodeGenOpt::Default;
  switch (OptimizationLevel) {
  case OptLevel::O1:
    OptLevel = CodeGenOpt::Less;
    break;
  case OptLevel::O2:
  case OptLevel::O0:
    OptLevel = CodeGenOpt::Default;
    break;
  case OptLevel::O3:
    OptLevel = CodeGenOpt::Aggressive;
    break;
  };

  llvm::TargetOptions Options;
  Options.ThreadModel = llvm::ThreadModel::POSIX;
  Options.FloatABIType = llvm::FloatABI::Default;
  Options.AllowFPOpFusion = llvm::FPOpFusion::Standard;
  Options.UnsafeFPMath = true;

  const char *CPU;
  if (MArchName == "")
    CPU = sys::getHostCPUName().data();
  else
    CPU = MArchName.c_str();
  StringMap<bool> HostFeatures;
  auto status = sys::getHostCPUFeatures(HostFeatures);
  SubtargetFeatures features;
  if (status) {
    for (auto &F : HostFeatures) {
      features.AddFeature(F.first(), F.second);
    }
  }

  TM.reset(TheTarget->createTargetMachine(Triple, CPU, features.getString(),
                                          Options, RM, CM, OptLevel));

  TheModule->setDataLayout(TM->createDataLayout());
  return true;
}
static bool compileFile(std::string &InputFile) {

  if (FortranStandard == Standard::None) {
    auto extension = llvm::sys::path::extension(InputFile);
    if (extension == ".f")
      FortranStandard = f77;
    else
      FortranStandard = f95;
  }

  if (PrepareForLTO) {
    ActionOpt = EmitBC;
  }

  if (OutputFilename == "") {
    std::string extension = "";
    switch (ActionOpt) {
    case EmitRawAST:
    case EmitAST:
      extension = "ast.c";
      break;
    case EmitBC:
      if (PrepareForLTO) {
        extension = "o";
      } else {
        extension = "bc";
      }
      break;
    case EmitLLVM:
      extension = "ll";
      break;
    case EmitASM:
      extension = "s";
      break;
    case EmitExe:
      if (StopAtCompile) {
        extension = "o";
      } else {
        OutputFilename = "a.out";
      }
      break;
    default:
      llvm_unreachable("Unhandled action type");
    };
    if (ActionOpt != EmitExe || (ActionOpt == EmitExe && StopAtCompile)) {
      // Replace the existing extension in the input file to the new one.
      assert(!extension.empty());
      OutputFilename = InputFile;
      llvm::SmallString<128> outputFile(OutputFilename);
      llvm::sys::path::replace_extension(outputFile, extension);
      OutputFilename = outputFile.str();
    }
  }

  FC_DEBUG(debug() << "Started parsing input file " << InputFile << "\n");

  auto file = llvm::MemoryBuffer::getFileOrSTDIN(InputFile);
  if (!file) {
    error() << "Failed to read input file\n";
    return false;
  }
  auto fileRef = file.get()->getBuffer();

  // Initiate lexer.
  SourceLoc loc;
  Lexer lexer(InputFilename, FortranStandard, loc, fileRef.begin(),
              fileRef.end());

  Diagnostics diagEngine(InputFilename);

  ASTContext astContext(diagEngine, InputFile);

  parser::Parser parser(astContext, lexer, diagEngine);

  // Run parser and generate AST.
  FC_DEBUG(debug() << "Started running the Parser\n");
  bool status = parser.parseProgram();
  FC_DEBUG(debug() << "Done with the Parser\n");
  if (!status)
    return false;

  // Runs the semantic check on parser.
  Sema sema(parser.getTree());

  std::error_code EC;
  llvm::raw_fd_ostream OS(OutputFilename, EC, llvm::sys::fs::F_None);

  if (ActionOpt == EmitRawAST) {
    FC_DEBUG(debug() << "Emitting the raw AST\n");
    parser.dumpParseTree(OS);
    OS.flush();
    OS.close();
    return true;
  }

  status = sema.run();
  if (!status) {
    error() << "\n Error while running Sema";
    return false;
  }

  if (ActionOpt == EmitAST) {
    FC_DEBUG(debug() << "Emitting the AST after SEMA\n");
    parser.dumpParseTree(OS);
    OS.flush();
    OS.close();
    return true;
  }

  // Initialize targets first, so that --version shows registered targets.
  LLVMInitializeX86TargetInfo();
  LLVMInitializeX86Target();
  LLVMInitializeX86TargetMC();
  LLVMInitializeX86AsmParser();
  LLVMInitializeX86AsmPrinter();

  std::unique_ptr<llvm::TargetMachine> TM;
  // Emit LLVM IR for the parse tree.
  llvm::LLVMContext Context;
  llvm::StringRef ModuleName{InputFilename};
  std::unique_ptr<llvm::Module> TheModule =
      llvm::make_unique<llvm::Module>(ModuleName, Context);
  prepareLLVMTarget(TM, TheModule.get());

  ASTPassManager astPassManager(parser.getTree(), "AST CodeGen PassManager");
  auto llvmCG =
      new CodeGen(astContext, TheModule, EnableDebug, FortranStandard, TM);
  astPassManager.addPass(llvmCG);

  if (!astPassManager.run()) {
    error() << "\n Error during LLVM IR emission";
    return false;
  }

  if (ActionOpt == EmitBC && OptimizationLevel == O0) {
    FC_DEBUG(debug() << "Emitting LLVM BC before optimizations\n");
    llvm::WriteBitcodeToFile(*TheModule, OS);
    OS.flush();
    OS.close();
    return true;
  }

  if (ActionOpt == EmitLLVM && OptimizationLevel == O0) {
    FC_DEBUG(debug() << "Emitting LLVM IR\n");
    TheModule->print(OS, nullptr);
    OS.flush();
    OS.close();
    return true;
  }

  llvm::Triple TargetTriple(TheModule->getTargetTriple());
  std::unique_ptr<TargetLibraryInfoImpl> TLII(
      new TargetLibraryInfoImpl(TargetTriple));

  PassManagerBuilder PMBuilder;
  PMBuilder.OptLevel = OptimizationLevel;
  PMBuilder.SizeLevel = 0;
  PMBuilder.LoopVectorize = OptimizationLevel > 1;
  PMBuilder.SLPVectorize = OptimizationLevel > 1;
  PMBuilder.PrepareForLTO = PrepareForLTO;
  PMBuilder.DisableUnrollLoops = !(OptimizationLevel > 1);
  PMBuilder.Inliner = createFunctionInliningPass(PMBuilder.OptLevel,
                                                 PMBuilder.SizeLevel, false);

  legacy::FunctionPassManager FPM(TheModule.get());
  FPM.add(new TargetLibraryInfoWrapperPass(*TLII));
  FPM.add(createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));

  legacy::PassManager MPM;

  MPM.add(new TargetLibraryInfoWrapperPass(*TLII));

  MPM.add(createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));

  PMBuilder.populateFunctionPassManager(FPM);
  PMBuilder.populateModulePassManager(MPM);

  // Run all the function passes.
  FPM.doInitialization();
  for (llvm::Function &F : *TheModule)
    if (!F.isDeclaration())
      FPM.run(F);
  FPM.doFinalization();

  // Run all the module passes.
  MPM.run(*TheModule.get());

  if (PrepareForLTO || ActionOpt == EmitBC) {
    FC_DEBUG(debug() << "Emitting LLVM BC after optimizations\n");
    llvm::WriteBitcodeToFile(*TheModule, OS);
    OS.flush();
    OS.close();
    return true;
  }

  if (ActionOpt == EmitLLVM) {
    FC_DEBUG(debug() << "Emitting LLVM IR after optimizations\n");
    TheModule->print(OS, nullptr);
    OS.flush();
    OS.close();
    return true;
  }

  // Create CodeGen Passes.
  legacy::PassManager CGPasses;
  CGPasses.add(createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));

  if (ActionOpt == EmitASM) {
    if (TM->addPassesToEmitFile(CGPasses, OS, nullptr,
                                llvm::TargetMachine::CGFT_AssemblyFile)) {
      error() << "\n Failed to emit Assembly file.";
      return false;
    }

    // Run all codegen passes.
    CGPasses.run(*TheModule.get());
    FC_DEBUG(debug() << "Emitting ASM file\n");
    return true;
  }

  // Generate binary action. Emit object file first and then create exe.
  assert(ActionOpt == EmitExe);
  MachineModuleInfo MMI(TM.get());
  auto MCContext = &MMI.getContext();

  std::string objFile = "";

  // Create temporary object file.
  InputFile = llvm::sys::path::filename(InputFilename);
  std::string tempFilename = "/tmp/" + InputFile;
  auto TmpFile = llvm::sys::fs::TempFile::create(tempFilename + "-%%%%%.o");
  if (!TmpFile) {
    error() << "\n Failed to create temporary file!";
    return false;
  }

  if (StopAtCompile) {
    objFile = OutputFilename;
  } else {
    objFile = TmpFile.get().TmpName;
  }

  llvm::raw_fd_ostream TOS(objFile, EC, llvm::sys::fs::F_None);

  FC_DEBUG(debug() << "Emitting Temp file " << objFile);
  if (TM->addPassesToEmitMC(CGPasses, MCContext, TOS, false)) {
    error() << "\n Failed to generate object code";
    return false;
  }

  // Run all codegen passes.
  CGPasses.run(*TheModule.get());
  TOS.close();

  if (StopAtCompile)
    return true;
  // Create ld command

  // FIXME: Expects clang binary for linking.
  StringRef ldCommand = getenv("CLANG_BINARY");
  if (ldCommand.empty()) {
    error() << "\n CLANG_BINARY env variable not set!";
    return false;
  }

  if (RuntimePath.size() > 0)
    RuntimePath = "-L" + RuntimePath;
  const char *args[8] = {ldCommand.str().c_str(),
                         objFile.c_str(),
                         "-o",
                         OutputFilename.c_str(),
                         "-lFC",
                         "-lm",
                         RuntimePath.c_str(),
                         NULL};

  std::string errorStr;
  bool ExecFailed = false;
  std::vector<Optional<StringRef>> Redirects;
  Redirects = {llvm::NoneType::None, llvm::NoneType::None,
               llvm::NoneType::None};

  llvm::SmallVector<llvm::StringRef, 8> argsArr(args, args + 8);

  llvm::sys::ExecuteAndWait(ldCommand, argsArr, llvm::None, Redirects, 0, 0,
                            &errorStr, &ExecFailed);
  if (ExecFailed) {
    error() << "\n ld tool execution failed : " << errorStr;
    return false;
  }
  FC_DEBUG(debug() << "Emitting bi  ry file" << OutputFilename);
  // Delete the temp file created.
  if (auto E = TmpFile->discard()) {
    return false;
  }
  return true;
}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);

  if (DumpVersion) {
    std::cout << "\nFortran Compiler by Compiler Tree Technologies Ltd";
    std::cout << "\nVersion : 0.1\n";
    return 0;
  }
  return !compileFile(InputFilename);
}
