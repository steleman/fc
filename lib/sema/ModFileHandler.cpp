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
#include "sema/ModFileHandler.h"
#include "AST/ASTContext.h"
#include "AST/Declaration.h"
#include "AST/Expressions.h"
#include "AST/ParseTreeBuilder.h"
#include "AST/ProgramUnit.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Debug.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include <sstream>
#include <string>

using namespace fc;
using namespace ast;

#include <map>

static Type *getBaseTypeFromID(Type::TypeID ID, ASTContext &FC) {
  switch (ID) {
  case Type::VoidID:
    return Type::getVoidTy(FC);
  case Type::Int16ID:
    return Type::getInt16Ty(FC);
  case Type::Int32ID:
    return Type::getInt32Ty(FC);
  case Type::Int64ID:
    return Type::getInt64Ty(FC);
  case Type::Int128ID:
    return Type::getInt128Ty(FC);
  case Type::RealID:
    return Type::getRealTy(FC);
  case Type::DoubleID:
    return Type::getDoubleTy(FC);
  case Type::DummyArgID:
    return Type::getDummyArgTy(FC);
  case Type::UndeclaredID:
    return Type::getUndeclaredTy(FC);
  case Type::LogicalID:
    return Type::getLogicalTy(FC);
  case Type::CharacterID:
    return Type::getCharacterTy(FC);
  case Type::StringCharID:
    return Type::getStringCharTy(FC);
  default:
    return nullptr;
  };
}

static Type *getTypeFromStr(std::stringstream &ss, ASTContext &FC) {
  unsigned int val;
  ss >> val;

  Type::TypeID ID = (Type::TypeID)(val);

  Type *base = getBaseTypeFromID(ID, FC);

  if (base)
    return base;

  if (ID == Type::PointerID) {
    Type *eleTy = getTypeFromStr(ss, FC);
    assert(eleTy);
    return PointerType::get(FC, eleTy);
  }

  // DT-vars can either be NamedType or StructType. We always return NamedType
  // for both and leave the type resolution to the type-updaters in sema.
  if (ID == Type::NamedID || ID == Type::StructID) {
    std::string typeName;
    ss >> typeName;
    return NamedType::get(FC, typeName);
  }

  if (ID == Type::ArrayID) {
    auto base = getTypeFromStr(ss, FC);

    // get num dims;
    unsigned int numDims;
    ss >> numDims;
    unsigned int bSize;
    ss >> bSize;
    if (bSize == 0) {
      return ArrayType::get(FC, base, numDims);
    }
    ArrBoundsList list;
    for (unsigned i = 0; i < bSize; ++i) {
      ArrayBounds bounds;
      ss >> bounds.first;
      ss >> bounds.second;
      list.push_back(bounds);
    }
    return ArrayType::get(FC, base, list);
  }

  if (ID == Type::FunctionID) {
    auto returnTy = getTypeFromStr(ss, FC);

    int argSize;
    ss >> argSize;
    llvm::SmallVector<Type *, 2> typeList;
    for (int i = 0; i < argSize; ++i) {
      auto argTy = getTypeFromStr(ss, FC);
      typeList.push_back(argTy);
    }

    return FunctionType::get(FC, returnTy, typeList);
  }
  llvm_unreachable("unknown type encountered!");
}

static void dumpType(llvm::raw_ostream &OS, Type *type) {
  OS << ((unsigned int)type->getTypeID());

  if (!type->isDerivedTy()) {
    return;
  }
  OS << " ";

  if (auto ptrTy = llvm::dyn_cast<PointerType>(type)) {
    dumpType(OS, ptrTy->getElementType());
    return;
  }

  // In case of DT-vars, either we can have a NamedType in case the DTD for it
  // is unknown, or a StructType otherwise.
  if (auto namedTy = llvm::dyn_cast<NamedType>(type)) {
    OS << namedTy->getName() << " ";
    return;
  }

  if (auto structTy = llvm::dyn_cast<StructType>(type)) {
    llvm::StringRef dtdName = structTy->getDTDName();
    assert(!dtdName.empty());
    OS << dtdName << " ";
    return;
  }

  if (auto arrTy = llvm::dyn_cast<ArrayType>(type)) {
    dumpType(OS, arrTy->getElementTy());
    OS << " ";
    OS << arrTy->getNumDims() << " ";

    OS << arrTy->getBoundsList().size() << " ";

    for (auto b : arrTy->getBoundsList()) {
      OS << b.first << " ";
      OS << b.second << " ";
    }
    return;
  }

  if (auto funcTy = llvm::dyn_cast<FunctionType>(type)) {
    dumpType(OS, funcTy->getReturnType());
    OS << " ";
    OS << funcTy->getArgList().size() << " ";

    for (auto b : funcTy->getArgList()) {
      dumpType(OS, b);
      OS << " ";
    }
    return;
  }
  llvm_unreachable("unknown type encountered!");
}

// For the read/dump of .mod files, read this section to understand the
// structure of our .mod files.
// .mod file structure:
// - Composes of a series of DTD definitions and the module's symtab:
// - All symtabs end in the ".end" line.
// - The ".dt <dtd-name>" line denotes the beginning of a DTD definition,
// followed
//   by the symtab of that DTD.
// - The symtab without a heading ".dt ..." is the module's symtab.
//
// Example:
// .dt fooDTD
// <fooDTD-symbol1> <symbol-attributes,etc>
// <fooDTD-symbol2> <symbol-attributes,etc>
// ...
// .end
// .dt barDTD
// <barDTD-symbol1> <symbol-attributes,etc>
// ...
// .end
// <mod-symbol1> <symbol-attributes,etc>
// <mod-symbol2> <symbol-attributes,etc>
// ...
// .end

// dumps \p symTable to \p OS
static void dumpSymbolTable(SymbolTable *symTable, llvm::raw_fd_ostream &OS) {
  for (auto sym : symTable->getOrderedSymbolList()) {
    // If this is an undeclared function reference, we won't dump it since a
    // module's symTable must only have fortran intrinsics that should be
    // evaluated by now. TODO: Do the evaluation for all intrinsics!
    // Should we ever dump functions to .mod file since they all need to be
    // evaluated to a constant ?
    if (sym->getType()->isUndeclared())
      continue;
    OS << sym->getName() << " ";
    dumpType(OS, sym->getType());
    OS << " ";
    auto attr = sym->getAttr();
    OS << attr.isConst << " " << attr.isAllocatable << " " << attr.isTarget
       << " " << attr.isPointer << " " << ((unsigned int)attr.intentKind) << " "
       << attr.isOptional << " " << ((unsigned int)attr.allocKind);

    auto modName = sym->getOriginalModName();
    OS << " " << modName;

    // Append initConstant if any
    Constant *initConstant = sym->getInitConstant();
    if (initConstant) {
      Type *type = initConstant->getType();

      // TODO: other types
      if (type->isIntegralTy()) {
        OS << " ";
        dumpType(OS, type);
        OS << " " << initConstant->getInt();
      } else if (type->isLogicalTy()) {
        OS << " ";
        dumpType(OS, type);
        OS << " " << initConstant->getBool();
      }
    }
    OS << "\n";
  }
  OS << ".end\n";
}

// populates \p symTable from where \p i is pointing to and updates \p i till
// where it finishes reading. Make sure \p i is pointing to the beginning of
// symbol-table definition.
static void readSymbolTable(SymbolTable *symTable,
                            llvm::ArrayRef<llvm::StringRef>::iterator &i,
                            llvm::ArrayRef<llvm::StringRef>::iterator &e,
                            ASTContext &FC) {

  for (; i != e; ++i) {
    llvm::StringRef line = *i;

    std::stringstream ss(line.str());
    std::string word;

    std::string firstWord;
    ss >> firstWord;
    if (firstWord == ".end") {
      i++;
      return;
    }

    std::string symbolName = firstWord;
    auto Ty = getTypeFromStr(ss, FC);

    SymbolAttributes attr;
    ss >> attr.isConst;
    ss >> attr.isAllocatable;
    ss >> attr.isTarget;
    ss >> attr.isPointer;

    int kind;
    ss >> kind;
    attr.intentKind = ((IntentKind)kind);
    ss >> attr.isOptional;
    ss >> kind;
    attr.allocKind = ((AllocationKind)kind);

    // Read mod name.
    std::string modName;
    ss >> modName;

    SourceLoc loc;
    auto newSym = symTable->addNewSymbol(symbolName, Ty, loc, attr);
    newSym->setOrigModName(modName);

    // Add initConstant to newSym if any
    if (ss.eof()) {
      continue;
    }

    Type *initConstType = getTypeFromStr(ss, FC);
    ParseTreeBuilder builder(FC);

    if (initConstType->isIntegralTy()) {
      unsigned initConstant;
      ss >> initConstant;
      newSym->setInitConstant(
          Constant::Create(std::to_string(initConstant), initConstType));

    } else if (initConstType->isLogicalTy()) {
      bool initConstant;
      ss >> initConstant;
      newSym->setInitConstant(
          Constant::Create(initConstant ? ".true." : ".false.", initConstType));
    }
  }
}

bool ModFileHandler::dumpModFile(Module *mod) {
  std::error_code EC;
  std::string OuputFile = mod->getName().str() + ".mod";
  FC_DEBUG(debug() << "Dumping the mod file " << OuputFile << "\n");
  llvm::raw_fd_ostream OS(OuputFile, EC, llvm::sys::fs::F_None);

  for (DerivedTypeDef *DTD : mod->getSpec()->getDTDs()) {
    OS << ".dt"
       << " " << DTD->getName() << "\n";
    dumpSymbolTable(DTD->getSymbolTable(), OS);
  }

  dumpSymbolTable(mod->getSymbolTable(), OS);

  OS.flush();
  OS.close();
  return true;
}

bool ModFileHandler::readModFile(llvm::StringRef name, UseStmt *useStmt,
                                 ProgramUnit *PU, ASTContext &FC) {

  FC_DEBUG(debug() << "Loading the mod file " << name << "\n");
  SymbolTable *symTable = useStmt->getSymbolTable();

  auto file = llvm::MemoryBuffer::getFileOrSTDIN(name);
  if (!file) {
    return false;
  }
  auto fileRef = file.get()->getBuffer();

  llvm::SmallVector<llvm::StringRef, 2> lines;

  fileRef.split(lines, "\n");

  lines.pop_back();

  ParseTreeBuilder builder(FC);

  llvm::ArrayRef<llvm::StringRef>::iterator i = lines.begin(), e = lines.end();

  for (; i != e;) {

    llvm::StringRef currLine = *i;
    std::string firstWord, dtdName;
    std::stringstream ss(currLine.str());

    ss >> firstWord;
    if (firstWord == ".dt") {
      ss >> dtdName;

      SymbolTable *dtdSymTable =
          builder.buildSymbolTable(FC, PU->getSymbolTable()->getScopeKind(),
                                   dtdName, PU->getSymbolTable());

      ++i; // point to the beginning of the symtab (ie. the next line)
      readSymbolTable(dtdSymTable, i, e, FC);

      llvm::SmallVector<Type *, 2> fieldTypes;
      llvm::SmallVector<std::string, 2> fieldNames;
      // Move this (and other similar type related APIs) to a separate
      // "type-utils".cpp file ?
      for (auto sym : dtdSymTable->getOrderedSymbolList()) {
        fieldNames.push_back(sym->getName());
        fieldTypes.push_back(sym->getType());
      }

      std::string _verboseName("dt." + useStmt->getModuleName() + "." +
                               dtdName);
      llvm::StringRef verboseName(_verboseName);

      auto type = StructType::get(FC, verboseName, fieldTypes, fieldNames,
                                  /* isDynArray = */ false);

      type->setDTDName(dtdName);
      ProgramUnitList emptyPUL;
      DerivedTypeDef *newDTD = builder.buildDerivedTypeDef(
          dtdName, dtdSymTable, PU, /* block = */ nullptr, type, emptyPUL);

      useStmt->addDTD(newDTD);
      continue;
    }

    readSymbolTable(symTable, i, e, FC);
  }

  return true;
}
