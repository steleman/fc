// Copyright (c) 2019, Compiler Tree Technologies Pvt Ltd.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#include "AST/ASTContext.h"
#include "AST/Declaration.h"
#include "AST/Expressions.h"

#include "AST/ParserTreeCommon.h"
#include "AST/ProgramUnit.h"
#include "AST/Statements.h"
#include "AST/Stmt.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Util.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_os_ostream.h"

#include <sstream>

using namespace fc;
using namespace ast;

#define os OS.indent(level)
#define os_2 OS.indent(level + 2)
#define os_4 OS.indent(level + 4)

static int varCount = 0;

static std::string getName() {
  varCount++;
  std::string name = "t.";
  return name + std::to_string(varCount);
}

static std::string getLocString(const SourceLoc loc) {
  std::string location =
      "(" + std::to_string(loc.Line) + ", " + std::to_string(loc.Col) + ")";
  return location;
}

template <class T>
static void listDumper(llvm::raw_ostream &OS, int level,
                       llvm::ArrayRef<T *> arr) {
  for (auto stmt : arr) {
    stmt->dump(OS, level);
  }
}

template <class T>
static void listDumper(llvm::raw_ostream &OS, int level,
                       std::list<Stmt *> arr) {
  for (auto stmt : arr) {
    stmt->dump(OS, level);
  }
}

template <class T>
static std::string listDumperString(llvm::raw_ostream &OS, int level,
                                    llvm::ArrayRef<T *> arr,
                                    llvm::StringRef delim = ", ") {
  std::string listString;
  unsigned size = arr.size();
  if (size == 0)
    return "";

  for (unsigned i = 0; i < size; ++i) {
    if (!arr[i])
      continue;
    listString.append(arr[i]->dump(OS, level));
    listString.append(delim);
  }
  if (listString.size() > 1) {
    for (unsigned i = 0; i < delim.size(); i++)
      listString.pop_back();
  }
  return listString;
}

static std::string getStatusKindString(StatusKind kind) {
  switch (kind) {
  case StatusKind::OLD:
    return "OLD";
  case StatusKind::NEW:
    return "NEW";
  case StatusKind::SCRATCH:
    return "SCRATCH";
  case StatusKind::REPLACE:
    return "REPLACE";
  case StatusKind::UNKNOWN:
    return "UNKNOWN";
  case StatusKind::undefined:
    return "undefined";
  default:
    llvm_unreachable("Undefined status kind!");
  }
}

static std::string getBinOpKindString(BinaryOpKind kind) {
  switch (kind) {
  case BinaryOpKind::Addition:
    return "+";
  case BinaryOpKind::Subtraction:
    return "-";
  case BinaryOpKind::Multiplication:
    return "*";
  case BinaryOpKind::Division:
    return "/";
  case BinaryOpKind::Power:
    return "**";
  case BinaryOpKind::Concat:
    return "+";
  default:
    llvm_unreachable("Undefined binary operator");
  }
}

static std::string getRelationalOpString(RelationalOpKind kind) {
  switch (kind) {
  case RelationalOpKind::EQ:
    return "==";
  case RelationalOpKind::NE:
    return "!=";
  case RelationalOpKind::LT:
    return "<";
  case RelationalOpKind::LE:
    return "<=";
  case RelationalOpKind::GT:
    return ">";
  case RelationalOpKind::GE:
    return ">=";
  default:
    llvm_unreachable("Undefined relational operator");
  }
  return NULL;
}

// Unlike other operators, we are printing the name here. This is to avoid
// confusion between relational operators and logical operators.
static std::string getLogicalOpString(LogicalOpKind kind) {
  switch (kind) {
  case LogicalOpKind::NOT:
    return ".NOT.";
  case LogicalOpKind::AND:
    return ".AND.";
  case LogicalOpKind::OR:
    return ".OR.";
  case LogicalOpKind::EQV:
    return ".EQV.";
  case LogicalOpKind::NEQV:
    return ".NEQV.";
  default:
    llvm_unreachable("Undefined logical operator");
  }
  return NULL;
}

// Type ast dump helper
static std::string getString(Type::TypeID ID) {
  switch (ID) {
  case Type::VoidID:
    return "void";
  case Type::Int16ID:
    return "int16";
  case Type::Int32ID:
    return "int32";
  case Type::Int64ID:
    return "int64";
  case Type::Int128ID:
    return "int128";
  case Type::RealID:
    return "real";
  case Type::DoubleID:
    return "double";
  case Type::ComplexID:
    return "complex";
  case Type::LogicalID:
    return "logical";
  case Type::StringCharID:
  case Type::CharacterID:
    return "character";
  case Type::ArrayID:
    return "array";
  case Type::PointerID:
    return "pointer";
  case Type::FunctionID:
    return "Function";
  case Type::DummyArgID:
    return "DummyArgID";
  case Type::ModuleID:
    return "Module";
  case Type::UndeclaredID:
    return "Undeclared";
  case Type::UndeclaredFnID:
    return "UndeclaredFunction";
  case Type::VarArgID:
    return "...";
  case Type::StructID:
    return "Struct";
  default:
    llvm_unreachable("Undefined Type");
  };
}

static std::string getScopeKindStr(ScopeKind scopeKind) {
  switch (scopeKind) {
  case ScopeKind::GlobalScope:
    return "GlobalScope";
  case ScopeKind::MainProgramScope:
    return "MainProgramScope";
  case ScopeKind::SubroutineScope:
    return "SubroutineScope";
  case ScopeKind::ModuleScope:
    return "ModuleScope";
  case ScopeKind::FunctionScope:
    return "FunctionScope";
  default:
    llvm_unreachable("unhandled scope");
  };
}

static std::string getIntentKindStr(IntentKind intentKind) {
  std::string ikindStr;
  switch (intentKind) {
  case In:
    ikindStr = "In";
    break;
  case InOut:
    ikindStr = "InOut";
    break;
  case Out:
    ikindStr = "Out";
    break;
  case Intent_None:
    ikindStr = "Intent_None";
    break;
  default:
    llvm_unreachable("unknown intent !");
  };
  return ikindStr;
}

static std::string getAllocKindStr(AllocationKind allocKind) {
  std::string ikindStr;
  switch (allocKind) {
  case StaticGlobal:
    ikindStr = "StaticGlobal";
    break;
  case StaticLocal:
    ikindStr = "StaticLocal";
    break;
  case Argument:
    ikindStr = "Argument";
    break;
  case Alloc_None:
    ikindStr = "Alloc_None";
    break;
  default:
    llvm_unreachable("unknown allocation kind !");
  };
  return ikindStr;
}

static std::string getString(Type *type) {
  return getString(type->getTypeID());
}

std::string ConstantVal::dump(llvm::raw_ostream &OS, int level) const {

  auto constant = getConstant();
  if (!constant->getType()->isArrayTy()) {
    return getValue();
  }

  std::string constVal = "{";
  for (auto val : constant->getArrValue()) {
    constVal += val + ", ";
  }
  constVal.pop_back();
  constVal.pop_back();
  constVal += "}";
  return constVal;
}

std::string CaseStmt::dump(llvm::raw_ostream &OS, int level) const {
  auto exprList = getExprList();
  auto caseName = exprList.empty() ? "default" : "case ";
  os << caseName;
  if (!exprList.empty()) {
    OS << "(" << listDumperString<Expr>(OS, level, exprList);
    OS << ") ";
  }
  OS << ": {\n";
  getBlock()->dump(OS, level + 2);
  os << "}\n";
  return "";
}

std::string SelectCaseStmt::dump(llvm::raw_ostream &OS, int level) const {

  auto caseExpr = getSelectExpr();
  auto caseStr = caseExpr->dump(OS, level);
  os << "switch (" << caseStr << ") {\n";
  auto caseList = getCaseStmtList();
  for (auto caseStmt : caseList) {
    caseStmt->dump(OS, level + 2);
  }
  os << "};";
  return "";
}

std::string ParseTree::dump(llvm::raw_ostream &OS, int level) const {
  os << "Program: " << llvm::sys::path::filename(Context.inputFileName) << "\n";
  symbolTable->dump(OS, level);
  OS << "\n";
  for (auto PU : programList) {
    PU->dump(OS, level);
  }
  return "";
}

static std::string dumpProgramUnit(const ProgramUnit *PU, llvm::raw_ostream &OS,
                                   int level) {
  PU->getSymbolTable()->dump(OS, level);
  if (PU->getUsedSymTables().size() > 0) {
    os << "UsedSymbolTables {\n";
    for (auto symTable : PU->getUsedSymTables()) {
      symTable->dump(OS, level + 2);
    }
    os << "}\n";
  }
  if (PU->getSpec())
    PU->getSpec()->dump(OS, level);
  if (PU->getExecPart())
    PU->getExecPart()->dump(OS, level);

  auto programList = PU->getProgramUnitList();
  if (!programList.empty()) {
    OS << "\n";
    os << "// Internal SubProgram Lists: \n\n";
    for (auto InnerPU : programList) {
      InnerPU->dump(OS, level);
    }
  }
  return "";
}

std::string Module::dump(llvm::raw_ostream &OS, int level) const {
  os << "Module " << getName() << " { \n\n";
  dumpProgramUnit(this, OS, level + 2);
  os << "}\n\n";

  return "";
}

std::string Function::dump(llvm::raw_ostream &OS, int level) const {

  FunctionType *Fty = static_cast<FunctionType *>(getType());
  auto returnType = Fty->getReturnType();
  std::string returnTy = returnType->dump(OS, level, true);
  os << "// ";

  std::string functionStr = "";
  switch (this->getKind()) {
  case SubroutineKind:
    functionStr = "Subroutine";
    break;

  case FunctionKind:
    functionStr = "Function";
    break;
  case MainProgramKind:
    functionStr = "MainProgram";
    break;
  default:
    llvm_unreachable("Unknown program unit while dumping.");
  };
  OS << functionStr << "\n";

  os << returnTy << " " << getName();
  std::string argStr = "";
  for (auto arg : argsList) {
    auto type = this->symbolTable->getSymbol(arg)->getType();
    std::string typeStr = type->dump(OS, level, true);
    argStr.append(typeStr);
    argStr.append(" ");
    argStr.append(arg);
    argStr.append(", ");
  }
  if (argStr.size() > 1) {
    argStr.pop_back();
    argStr.pop_back();
  }
  OS << "(" << argStr << ") {\n";
  dumpProgramUnit(this, OS, level + 2);
  os << "}\n\n";

  return "";
}

std::string SpecificationPart::dump(llvm::raw_ostream &OS, int level) const {

  OS << "\n";
  os << "// Specification Constructs: \n\n";

  auto useStmtList = getUseStmts();
  if (!useStmtList.empty()) {
    os << "UseStmtList {\n";
    for (auto useStmt : useStmtList) {
      useStmt->dump(OS, level + 2);
    }
    os << "}\n";
  }

  auto dtds = getDTDs();
  if (!dtds.empty()) {
    for (auto dtd : dtds) {
      dtd->dump(OS, level);
    }
  }

  os << "EntityDeclList {\n";
  for (auto stmt : block->getStmtList()) {
    if (llvm::isa<EntityDecl>(stmt)) {
      os_2 << "{\n";
      stmt->dump(OS, level + 4);
      os_2 << "}\n";
      continue;
    }
    if (llvm::isa<UseStmt>(stmt)) {
      continue;
    }
    stmt->dump(OS, level + 2);
  }
  os << "}\n";

  if (!arrSpecMap.empty()) {
    OS << "\n";
    os << "ArraySpecList {\n";
    for (auto pair : arrSpecMap) {
      os_2 << pair.first->getName() << " : " << pair.second->dump(OS, level)
           << "\n";
    }
    os << "}\n";
  }
  return "";
}

std::string DeclarationTypeSpec::dump(llvm::raw_ostream &OS, int level) const {
  typeSpec->dump(OS, level);
  return "";
}

std::string Symbol::dump(llvm::raw_ostream &OS, int level) const {
  const auto attr = attributes;
  os << "// " << getLocString(this->getSourceLoc()) << "\n";

  if (parentSymbol) {
    os << "// ID: " << getID();
    OS << ", ParentSymbol: " << parentSymbol->getID();
    OS << ", ParentSymbolTable: " << parentSymbol->getSymTable()->getName()
       << "\n";
    os << parentSymbol->getType()->dump(OS, level, true) << " "
       << this->getName() << "\n";
    return "";
  }

  os << "// ";
  OS << "ID: " << getID();
  OS << ", " << (attr.isConst ? "Constant" : "NonConstant");
  OS << (attr.isAllocatable ? ", Allocatable" : ", NonAllocatable");
  OS << (attr.isTarget ? ", Target" : ", NonTarget");
  OS << (attr.isPointer ? ", Pointer" : ", NonPointer");
  OS << ", " << getAllocKindStr(attr.allocKind);
  OS << ", " << getIntentKindStr(attr.intentKind);
  OS << ", " << getOriginalModName();
  OS << "\n";
  os << this->getType()->dump(OS, level, true) << " " << this->getName()
     << "\n";
  return "";
}

std::string SymbolTable::dump(llvm::raw_ostream &OS, int level) const {

  os << "// " << getScopeKindStr(getScopeKind()) << ", Parent: "
     << (parent ? getScopeKindStr(parent->getScopeKind()) : "None") << "\n";

  os << "SymbolTable " << getName() << " {\n";
  os_2 << "// Symbol List: \n\n";

  for (auto &pair : map) {
    pair.second->dump(OS, level + 2);
  }
  os << "}\n";
  return "";
}

std::string DerivedTypeDef::dump(llvm::raw_ostream &OS, int level) const {
  os << "DerivedTypeDef " << getName() << " {\n";
  getSymbolTable()->dump(OS, level + 2);
  os << "}\n";

  return "";
}

std::string EntityDecl::dump(llvm::raw_ostream &OS, int level) const {

  os << "// " << getLocString(getSymbol()->getSourceLoc()) << "\n";

  auto arrSpec = getOperand(0);
  auto init = getOperand(1);
  std::string value = "";
  if (arrSpec) {
    os << "DIMS: " << arrSpec->dump(OS, level) << "\n";
  }
  os << "NAME: " << getSymbol()->getName() << "\n";
  os << "SYMBOL ID: " << getSymbol()->getID() << "\n";
  if (init) {
    value = init->dump(OS, level);
    os << "INIT: " << value << "\n";
  }
  return "";
}

std::string IntrinsicTypeSpec::dump(llvm::raw_ostream &OS, int level) const {

  os << "IntrinsicTypeSpec {\n";
  if (arrSpec) {
    os_2 << "DIMS:" << arrSpec->dump(OS, level + 2) << "\n";
  }
  os_2 << "TYPE: " << type->dump(OS, level + 2, true) << "\n";
  os << "}\n";
  return "";
}

std::string DerivedTypeSpec::dump(llvm::raw_ostream &OS, int level) const {

  os << "DerivedTypeSpec(" << getName() << ")\n";
  return "";
}

std::string ArraySpec::dump(llvm::raw_ostream &OS, int level) const {
  std::string bounds;
  bounds.append("[");
  for (auto b : getBoundsList()) {
    bounds.append(b.first ? b.first->dump(OS, level) : "");
    bounds.append(":");
    bounds.append(b.second ? b.second->dump(OS, level) : "");
    bounds.append(", ");
  }
  if (bounds.size() > 1) {
    bounds.pop_back();
    bounds.pop_back();
  }
  bounds.append("]");
  return bounds;
}

std::string ArrayConstructor::dump(llvm::raw_ostream &OS, int level) const {
  ACSpec *spec = getSpec();

  std::string str("(/ ");
  for (Stmt *stmt : spec->getOperands()) {
    auto expr = llvm::cast<Expr>(stmt);
    str.append(expr->dump(OS, level));
    str.append(", ");
  }

  str.erase(str.size() - 2, 2);

  str.append(" /)");
  return str;
}

std::string AttrSpec::dump(llvm::raw_ostream &OS, int level) const {

  std::string kindStr;
  switch (kind) {
  case AttrSpecKind::Dimension:
    kindStr = "Dimension";
    break;
  case AttrSpecKind::Allocatable:
    kindStr = "Allocatable";
    break;
  case AttrSpecKind::Parameter:
    kindStr = "Parameter";
    break;
  case AttrSpecKind::Intent:
    kindStr = "Intent";
    break;
  case AttrSpecKind::Private:
    kindStr = "Private";
    break;
  case AttrSpecKind::Optional:
    kindStr = "Optional";
    break;
  case AttrSpecKind::Public:
    kindStr = "Public";
    break;
  default:
    llvm_unreachable("not implemented yet!");
  };

  if (kind == Dimension) {
    os << "Dimension  {\n";
    std::string dims = arrSpec->dump(OS, level + 2);
    os_2 << "DIMS: " << dims << "\n";
    os << "}\n";
    return "";
  }
  if (kind == Intent) {
    os << "IntentKind = " << getIntentKindStr(intentKind) << "\n";
    return "";
  }

  os << kindStr << "\n";
  return "";
}

// Execution part, builder and dump
std::string ExecutionPart::dump(llvm::raw_ostream &OS, int level) const {
  OS << "\n";
  os << "// Execution Constructs: \n\n";
  listDumper<Stmt>(OS, level, block->getStmtList());
  return "";
}

// Execution part, builder and dump
std::string Block::dump(llvm::raw_ostream &OS, int level) const {
  listDumper<Stmt>(OS, level, stmtList);
  return "";
}

std::string ExitStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  os << "exit " << getConstructName() << "\n";
  return "";
}

std::string CycleStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  os << "cycle " << getConstructName() << "\n";
  return "";
}

std::string AssignmentExpr::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string rhs = this->getExpr()->dump(OS, level);
  auto lhs = getName();
  return "\"" + lhs + "\"" + " = " + rhs;
}

std::string ReturnStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string rhs = getExpr() ? getExpr()->dump(OS, level) : "";
  os << "return " << rhs << "\n";
  return "";
}

std::string AssignmentStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string rhs = this->getRHS()->dump(OS, level);
  auto lhs = this->getLHS()->dump(OS, level);
  os << lhs << " = " << rhs << "\n";
  return "";
}

std::string PointerAssignmentStmt::dump(llvm::raw_ostream &OS,
                                        int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string rhs = this->getRHS()->dump(OS, level);
  auto lhs = this->getLHS()->dump(OS, level);
  os << lhs << " => " << rhs << "\n";
  return "";
}

std::string NullifyStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  os << "nullify(" << listDumperString<Stmt>(OS, level, operands) << ")\n";

  return "";
}

std::string CastExpr::dump(llvm::raw_ostream &OS, int level) const {
  auto name = getName();
  std::string lhs = "";
  lhs = getExpr()->dump(OS, level);
  os << name << " = cast " << lhs << " to " << getType()->dump(OS, level)
     << "\n";
  return name;
}

std::string BinaryExpr::dump(llvm::raw_ostream &OS, int level) const {
  auto name = getName();
  std::string lhs = "";
  auto op1 = getLHS(), op2 = getRHS();
  if (op1)
    lhs = op1->dump(OS, level);
  std::string rhs = op2->dump(OS, level);
  std::string type = getString(op2->getType()->getTypeID());
  os << name << " = " << lhs << " " << getBinOpKindString(opKind) << " " << rhs
     << "\n";
  return name;
}

std::string QuadExpr::dump(llvm::raw_ostream &OS, int level) const {
  std::string doVar = this->getOperand(0)->dump(OS, level);
  std::string initVar = this->getOperand(1)->dump(OS, level);
  std::string endVar = this->getOperand(2)->dump(OS, level);
  std::string incrVar = this->getOperand(3)->dump(OS, level);

  std::stringstream ss;
  auto tmpName = getName();
  os << tmpName << " = "
     << "(/*IndVar=*/" << doVar << ", /*Init=*/" << initVar << ", /*End=*/"
     << endVar << ", /*Incr=*/" << incrVar << ")\n";
  return tmpName;
}

std::string IfStmt::dump(llvm::raw_ostream &OS, int level) const { return ""; }

std::string ObjectName::dump(llvm::raw_ostream &OS, int level) const {
  std::string type = getString(getSymbol()->getType());
  return (std::string)getSymbol()->getName();
}

std::string StructureComponent::dump(llvm::raw_ostream &OS, int level) const {
  return listDumperString<Expr>(OS, level, getPartRefs(), "%");
}

std::string Type::dump(llvm::raw_ostream &OS, int level,
                       bool isEntityType) const {
  if (isEntityType)
    return getString(ID);

  os << getString(ID);
  return "";
}

std::string ArrayType::dump(llvm::raw_ostream &OS, int level,
                            bool isEntity) const {

  std::string val = "";
  val = elementTy->dump(OS, level, isEntity) + "[";
  std::string bounds = "";

  if (!boundsEmpty()) {
    for (auto b : boundsList) {
      bounds.append(std::to_string(b.first));
      bounds.append(":");
      bounds.append(std::to_string(b.second));
      bounds.append(", ");
    }

  } else {
    for (int I = 0; I < numDims; ++I) {
      bounds.append("U, ");
    }
  }
  if (bounds.size() > 1) {
    bounds.pop_back();
    bounds.pop_back();
  }
  val += bounds + "]";

  if (isEntity)
    return val;
  OS << val;
  return "";
}

std::string PointerType::dump(llvm::raw_ostream &OS, int level,
                              bool isEntity) const {
  if (auto strType = llvm::dyn_cast<StructType>(eleTy)) {
    // There'a risk of recursing infinitely if we try expanding the struct type
    // here (for eg. if this is a self-referential pointer)
    OS << strType->getName() << "*";
  } else {
    OS << eleTy->dump(OS, level, isEntity) << "*";
  }

  return "";
}

std::string ComplexType::dump(llvm::raw_ostream &OS, int level,
                              bool isEntity) const {
  OS << "complex(" << kindSize << ")";
  return "";
}

std::string NamedType::dump(llvm::raw_ostream &OS, int level,
                            bool isEntity) const {
  OS << "<" << name << ">";
  return "";
}

std::string StructType::dump(llvm::raw_ostream &OS, int level,
                             bool isEntity) const {
  OS << name;
#if 0
  OS << name << ":{";
  auto argList = getTypeList();
  if (!argList.empty()) {
    for (unsigned i = 0; i < argList.size() - 1; i++) {
      OS << (argList[i]->dump(OS, level, isEntity)) << " " << fieldNameList[i]
         << ", ";
    }
    OS << argList.back()->dump(OS, level, isEntity) << " "
       << fieldNameList.back();
  }
  OS << "}";
#endif
  return "";
}

std::string FunctionType::dump(llvm::raw_ostream &OS, int level,
                               bool isEntity) const {

  OS << "(" << returnTy->dump(OS, level, isEntity) << ")(";
  std::vector<std::string> boundsArr;

  auto argList = getArgList();
  if (!argList.empty()) {
    for (unsigned i = 0; i < argList.size() - 1; i++) {
      OS << (argList[i]->dump(OS, level, isEntity)) << ", ";
    }
    OS << argList.back()->dump(OS, level, isEntity);
  }
  OS << ")";
  return "";
}

std::string ArrayElement::dump(llvm::raw_ostream &OS, int level) const {
  std::string name = (std::string)getSymbol()->getName();
  name.append("(");
  auto list = getSubscriptList();
  name.append(listDumperString<Expr>(OS, level, list));
  name.append(")");
  return name;
}

std::string RangeExpr::dump(llvm::raw_ostream &OS, int level) const {

  auto index = getIndex();
  auto lowerBound = getLowerBound();
  auto upperBound = getUpperBound();
  if (this->isFullRange()) {
    return ":";
  }
  std::string name;
  if (lowerBound) {
    name.append(lowerBound->dump(OS, level));
  }
  name.append(":");
  if (upperBound) {
    name.append(upperBound->dump(OS, level));
  }
  name.append(":");
  if (index) {
    name.append(index->dump(OS, level));
  }
  return name;
}

std::string ArraySection::dump(llvm::raw_ostream &OS, int level) const {
  std::string name = (std::string)getSymbol()->getName();
  name.append("(");
  auto list = getSubscriptList();
  name.append(listDumperString<Expr>(OS, level, list));
  name.append(")");
  return name;
}

std::string IOImpliedDo::dump(llvm::raw_ostream &OS, int level) const {
  std::string str;
  str.append(listDumperString<Expr>(OS, level, getExprList()));
  str.append(" ");
  str.append(listDumperString<Expr>(OS, level, getImpliedDos()));
  return str;
}

std::string RelationalExpr::dump(llvm::raw_ostream &OS, int level) const {
  auto name = getName();
  std::string lhs = this->getLHS()->dump(OS, level);
  std::string rhs = this->getRHS()->dump(OS, level);
  std::string type = getString(this->getLHS()->getType());
  os << name << " = " << lhs << " " << getRelationalOpString(opKind) << " "
     << rhs << "\n";
  return name;
}

std::string LogicalExpr::dump(llvm::raw_ostream &OS, int level) const {
  auto name = getName();
  std::string lhs = "";
  if (this->getLHS())
    lhs = this->getLHS()->dump(OS, level);
  std::string rhs = this->getRHS()->dump(OS, level);
  os << name << " = " << lhs << " " << getLogicalOpString(opKind) << " " << rhs
     << "\n";
  return name;
}

std::string PrintStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string name = listDumperString<Stmt>(OS, level, operands);
  os << "printf(" << name << ")\n";
  return "";
}

// FIXME : Not tested!
std::string CallStmt::dump(llvm::raw_ostream &OS, int level) const {
  std::string fnName = sym->getName();
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string list = listDumperString<Stmt>(OS, level, operands);
  os << "call " << fnName << "(" << list << ")\n";

  return "";
}

std::string FunctionReference::dump(llvm::raw_ostream &OS, int level) const {
  std::string funcRef = (std::string)this->getSymbol()->getName();
  funcRef.append("(");
  funcRef.append(listDumperString<Stmt>(OS, level, operands));
  funcRef.append(")");
  return funcRef;
}

std::string StopStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";

  std::string stopCode = "";
  if (getStopCode())
    stopCode = this->getStopCode()->dump(OS, level);
  os << "stop " << stopCode << "\n";
  return "";
}

std::string IfElseStmt::dump(llvm::raw_ostream &OS, int level) const {

  for (unsigned i = 0; i < operands.size(); ++i) {

    auto ifStmt = getIfStmt(i);
    os << "// " << getLocString(ifStmt->getSourceLoc()) << "\n";
    if (kindList[i] == IfConstructKind::IfKind ||
        kindList[i] == IfConstructKind::IfThenKind) {
      std::string name = ifStmt->getCondition()->dump(OS, level);
      os << "if (" << name << ") {\n";
      ifStmt->getBlock()->dump(OS, level + 2);
      os << "}\n";
    } else if (kindList[i] == IfConstructKind::ElseIfKind) {
      std::string name = ifStmt->getCondition()->dump(OS, level);
      os << "else if (" << name << ") {\n";
      ifStmt->getBlock()->dump(OS, level + 2);
      os << "}\n";
    } else {
      os << "else {\n";
      ifStmt->getBlock()->dump(OS, level + 2);
      os << "}\n";
    }
  }
  return "";
}

std::string DoWhileStmt::dump(llvm::raw_ostream &OS, int level) const {

  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string name = this->getLogicalExpr()->dump(OS, level);
  os << "do while (" << name << ") {\n";

  getBlock()->dump(OS, level + 2);
  os << "}\n";
  return "";
}

std::string DoStmt::dump(llvm::raw_ostream &OS, int level) const {

  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  auto expr = getQuadExpr();

  std::string name = "";
  if (expr)
    name = "(" + expr->dump(OS, level) + ") ";

  os << this->constructName;
  if (!this->constructName.empty()) {
    OS << ": ";
  }
  OS << "do " << name << "{\n";
  getBlock()->dump(OS, level + 2);
  os << "}\n";
  return "";
}

std::string ForAllStmt::dump(llvm::raw_ostream &OS, int level) const {

  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  auto exprList = getExprList();

  std::string name = "";
  if (!exprList.empty())
    name = "(" + listDumperString<Expr>(OS, level, exprList) + ") ";

  os << this->constructName;
  if (!this->constructName.empty()) {
    OS << ": ";
  }
  OS << "forall " << name << "{\n";
  getBlock()->dump(OS, level + 2);
  os << "}\n";
  return "";
}

std::string ReadStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string str;
  // os << "read";
  if (this->getUnit()) {
    std::string unit = this->getUnit()->dump(OS, level);
    str.append("(unit = ");
    str.append(unit);
    str.append(")");
  }

  if (this->getFormat()) {
    std::string format = this->getFormat()->dump(OS, level);
    str.append("(format = ");
    str.append(format);
    str.append(")");
  }

  os << "read" << str << " "
     << listDumperString<Expr>(OS, level, this->getExprList()) << "\n";
  return "";
}

std::string Format::dump(llvm::raw_ostream &OS, int level) const {
  return listDumperString<Expr>(OS, level, this->getExprList());
}

std::string WriteStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << " // " << getLocString(this->getSourceLoc()) << "\n";
  std::string str;
  if (this->getUnit()) {
    std::string unit = this->getUnit()->dump(OS, level);
    str.append("(unit = ");
    str.append(unit);
    str.append(")");
  }

  if (this->getFormat()) {
    std::string format = this->getFormat()->dump(OS, level);
    str.append("(format = ");
    str.append(format);
    str.append(")");
  }

  if (this->getAdvance()) {
    std::string advance = this->getAdvance()->dump(OS, level);
    os << "(Advance = " << advance << ")";
    str.append("(Advance = ");
    str.append(advance);
    str.append(")");
  }

  std::string ops = listDumperString<Expr>(OS, level, this->getExprList());
  os << "write";
  if (!str.empty())
    os << str;
  os << " " << ops << "\n";
  return "";
}

std::string OpenStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string unit = this->getUnit()->dump(OS, level);
  std::string filename = getFile()->dump(OS, level);
  os << unit << " = open(" << filename << ", " << getStatusKindString(status)
     << ")\n";
  return "";
}

std::string CloseStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  std::string unit = this->getUnit()->dump(OS, level);
  os << unit << " = close(/*unit=*/" << unit << ")\n";
  return "";
}

std::string ACSpec::dump(llvm::raw_ostream &OS, int level) const { return ""; }

std::string DeAllocateStmt::dump(llvm::raw_ostream &OS, int level) const {
  std::string objString;
  os << " // " << getLocString(this->getSourceLoc()) << "\n";
  for (unsigned i = 0; i < objList.size(); ++i) {
    objString.append(objList[i]->getName());
    objString.append(", ");
  }
  if (objString.size() > 1) {
    objString.pop_back();
    objString.pop_back();
  }

  if (getStat()) {
    objString.append(", stat");
    objString.append(getStat()->dump(OS, level));
  }
  os << "deallocate " << objString << "\n";
  return "";
}

std::string AllocateStmt::dump(llvm::raw_ostream &OS, int level) const {
  std::string objList;
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  for (unsigned i = 0; i < allocateObjList.size(); ++i) {
    objList.append(allocateObjList[i]->getName());
    objList.append(getAllocateShape(i)->dump(OS, level));
    objList.append(", ");
  }
  if (objList.size() > 1) {
    objList.pop_back();
    objList.pop_back();
  }
  os << "allocate " << objList << "\n";
  return "";
}

// Dummy implementation for resolving merge conflicts
std::string UseStmt::dump(llvm::raw_ostream &OS, int level) const {
  os << "// " << getLocString(this->getSourceLoc()) << "\n";
  os << getString(Type::ModuleID) << " " << modSymTable->getName() << "\n";
  return "";
}

std::string WhereStmt::dump(llvm::raw_ostream &OS, int level) const {
  return "";
}

std::string WhereConstruct::dump(llvm::raw_ostream &OS, int level) const {
  for (unsigned i = 0; i < operands.size(); ++i) {
    auto whereStmt = getWhereStmt(i);
    os << "// " << getLocString(whereStmt->getSourceLoc()) << "\n";
    if (kindList[i] == WhereElseConstructKind::WhereKind) {
      std::string maskExpr = whereStmt->getMaskExpr()->dump(OS, level);
      os << "where (" << maskExpr << ") {\n";
      whereStmt->getBlock()->dump(OS, level + 2);
      os << "}\n";
    } else if (kindList[i] == WhereElseConstructKind::ElseWhereKind) {
      os << "elsewhere {\n"
         << "\n";
      whereStmt->getBlock()->dump(OS, level + 2);
      os << "}\n";
    } else if (kindList[i] == WhereElseConstructKind::ElseWhereWhereKind) {
      std::string maskExpr = whereStmt->getMaskExpr()->dump(OS, level);
      os << "elsewhere (" << maskExpr << ") {\n";
      whereStmt->getBlock()->dump(OS, level + 2);
      os << "}\n";
    } else {
      llvm_unreachable("Unhandled where stmt kind");
    }
  }
  return "";
}
