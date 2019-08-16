#ifndef FC_AST_PU_H
#define FC_AST_PU_H

#include "AST/ParserTreeCommon.h"
#include "AST/Stmt.h"
#include "common/Source.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"
#include <map>

namespace fc {
namespace ast {
class ProgramUnit {

public:
  inline llvm::StringRef getName() const { return llvm::StringRef(name); }

  virtual std::string dump(llvm::raw_ostream &OS, int level = 0) const = 0;

  inline SpecificationPart *getSpec() const { return specPart; }

  inline ExecutionPart *getExecPart() const { return execPart; }

  inline SymbolTable *getSymbolTable() const { return symbolTable; }

  bool hasUsedSymbolTable(SymbolTable *symTable);

  void addUsedSymTable(SymbolTable *symTable);

  SymbolTableSet getUsedSymTables() const { return usedSymTables; }

  constexpr ProgramUnitKind getKind() const { return kind; }

  inline bool isSubroutine() const { return kind == SubroutineKind; }

  inline bool isFunction() const { return kind == FunctionKind; }

  inline bool isMainProgram() const { return kind == MainProgramKind; }

  inline bool isModule() const { return kind == ModuleKind; }

  inline bool isProgram() const { return kind == ProgramKind; }

  ProgramUnitList &getProgramUnitList() { return programList; }

  const ProgramUnitList &getProgramUnitList() const { return programList; }

  void getUsedSymbolsInChildren(SymbolSet &set);

  ProgramUnit *getParent() const { return parent; }

  void setParent(ProgramUnit *PU) { parent = PU; }

  bool inGlobalScope() const { return (parent->isProgram()); }

  Type *getType() const;

  void setType(Type *type);

  // Nested subroutine or parent is considered as nested unit.
  inline bool isNestedUnit() const {
    if (parent && (!parent->isProgram() && !parent->isModule()))
      return true;
    return false;
  }

  DerivedTypeDef *getDTD(llvm::StringRef name);

  DerivedTypeDef *getDTD(StructType *structType);

  DerivedTypeDefList getDTDs();

  void addDTD(DerivedTypeDef *dtd);

  // This is automatically handled!
  virtual ~ProgramUnit() {
    // delete specPart;
    // delete execPart;

    // for (auto PU : programList) {
    //  delete PU;
    //}
  }

protected:
  explicit ProgramUnit(SymbolTable *symbolTable, ProgramUnitKind kind,
                       llvm::StringRef &name, SpecificationPart *specPart,
                       ExecutionPart *execPart, ProgramUnitList &programList);

  std::string name;
  SpecificationPart *specPart;
  ExecutionPart *execPart;
  ProgramUnitKind kind;
  SymbolTable *symbolTable;
  ProgramUnit *parent;
  ProgramUnitList programList;

  // Symbol table referred for this program unit.
  SymbolTableSet usedSymTables;
};

// A tree which holds the Program!
class ParseTree : public ProgramUnit {
  ASTContext &Context;

public:
  ParseTree(llvm::StringRef name, ASTContext &Context, SymbolTable *symTable,
            ProgramUnitList &programList);

  ASTContext &getContext() { return Context; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  ~ParseTree();
};

// Generic function representation for Fortran Subroutine, Function and
// MainProgram.
class Function : public ProgramUnit {
public:
  constexpr static bool classof(const ProgramUnit *unit) {
    switch (unit->getKind()) {
    case FunctionKind:
    case SubroutineKind:
    case MainProgramKind:
      return true;
    default:
      return false;
    }
  }

  ~Function() {}

  ArgsList getArgsList() { return argsList; }

  Type *getReturnType() const;

  void setReturnType(Type *returnTy);

protected:
  friend class ParseTreeBuilder;

  explicit Function(ProgramUnitKind kind, llvm::StringRef &name,
                    ArgsList &argsList, SymbolTable *symbolTable,
                    SpecificationPart *specPart, ExecutionPart *execPart,
                    ProgramUnitList &programList)
      : ProgramUnit(symbolTable, kind, name, specPart, execPart, programList),
        argsList(argsList) {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

private:
  ArgsList argsList;
};

class Module : public ProgramUnit {
public:
  ~Module() {}

  constexpr static bool classof(const ProgramUnit *unit) {
    return unit->getKind() == ModuleKind;
  }

protected:
  friend class ParseTreeBuilder;

  explicit Module(llvm::StringRef name, SymbolTable *symbolTable,
                  SpecificationPart *specPart, ProgramUnitList &programList)
      : ProgramUnit(symbolTable, ModuleKind, name, specPart, nullptr,
                    programList) {}

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;
};

// R425
class DerivedTypeDef : public ProgramUnit {
private:
  SymbolTable *symbolTable;

  ProgramUnit *parentPU; // the PU that contains this

  Block *block;

  // This must be set explicitly by the parser/sema iff the final StructType is
  // fully concrete with the correct array dims, derived-types within it
  // resolved etc. (This cannot be done at parser currently, only at
  // TypeUpdaterPass)
  bool isCompleteTy;

  // There'a recursive derived-type-def problem here, given:
  // Self-referential derived-type-def, ie:
  // type :: a {...; type(a), pointer; ...}
  //
  // When we get to parsing "type(a), pointer" we have an incomplete type(a), so
  // we need it to point to incomplete-type(a). After we finish parsing the
  // block, we will update the Type of type(a) and this should reflect on the
  // pointer within which should now point to a complete type. Hence we *cannot*
  // setType(new Type()). Once a Type is allocated for derived-type, we must
  // only work on that object since there can be many incomplete pointers
  // pointing to it (manually updating them will be a nightmare). The same
  // problem persist in the mutually recursive case, ie: type :: a {...;
  // type(b), pointer; ...} type :: b {...; type(c), pointer; ...}
  // ...
  // type :: n {...; type(a), pointer; ...}
  //
  // The resultant aggregate type, should *never* be nullptr, once assigned
  // should never be reassigned. The current design disallows reassignment of
  // type after the ctor. Don't change this design!
  StructType *type;

public:
  ~DerivedTypeDef() {}

  constexpr static bool classof(const ProgramUnit *unit) {
    return unit->getKind() == DerivedTypeDefKind;
  }

  ProgramUnit *getParentPU() const { return parentPU; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const override;

  Block *getBlock() const { return block; }

  // There's no setType() here due to the recursive derived-type problem
  // mentioned above, ie if the type needs to be modified, then we need to work
  // on the Type returned by getType().
  StructType *getType() const { return type; }

  bool isCompleteType() const { return isCompleteTy; }

  void setCompleteType(bool b) { isCompleteTy = b; }

protected:
  friend class ParseTreeBuilder;

  explicit DerivedTypeDef(llvm::StringRef name, SymbolTable *symbolTable,
                          ProgramUnit *parentPU, Block *block, StructType *type,
                          ProgramUnitList &programList)
      : ProgramUnit(symbolTable, DerivedTypeDefKind, name, nullptr, nullptr,
                    programList),
        parentPU(parentPU), block(block), isCompleteTy(false), type(type) {}
};

// Can be anything of executable stmt,formatstmt.
class Block : public Stmt {
  StmtList stmtList;

protected:
  friend class ParseTreeBuilder;

  explicit Block(StmtList &stmts, SourceLoc loc);

public:
  ~Block() {
    std::for_each(stmtList.begin(), stmtList.end(),
                  [](auto *stmt) { delete stmt; });
  }

  inline constexpr static bool classof(const Stmt *Stmt) {
    return Stmt->getStmtType() == BlockKind;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  StmtList &getStmtList() { return stmtList; }

  inline stmt_iterator begin() { return stmtList.begin(); }

  inline stmt_iterator end() { return stmtList.end(); }

  void insertFront(Stmt *newStmt);

  // Place the newStmt before the existingStmt in the current block.
  void insertStmtBefore(Stmt *newStmt, Stmt *existingStmt);

  // Insert stmt after the existing stmt in the current block
  void insertStmtAfter(Stmt *newStmt, Stmt *existingStmt);

  void replaceWith(Stmt *newStmt, Stmt *existingStmt);

  void addStmt(Stmt *newStmt);

  void removeStmt(Stmt *stmt);
};

class SpecificationPart {

  std::map<Symbol *, ArraySpec *> arrSpecMap;
  std::map<llvm::StringRef, DerivedTypeDef *> nameToDTDMap;
  std::map<StructType *, DerivedTypeDef *> typeToDTDMap;
  Block *block;

  DerivedTypeDefList derivedTypeDefs;

protected:
  friend class ParseTreeBuilder;

  SpecificationPart(Block *block) : block(block) {}

public:
  Block *getBlock() const { return block; }

  UseStmtList getUseStmts() const;

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  void setarrSpecMap(Symbol *sym, ArraySpec *spec) {
    assert(arrSpecMap.find(sym) == arrSpecMap.end());
    arrSpecMap[sym] = spec;
  }

  ArraySpec *getArraySpecFor(Symbol *sym) { return arrSpecMap[sym]; }

  // We need to be able to iterate through all derived-type-defs, and also be
  // able to look-up them by name/type fast enough during semantics. So adding
  // it to both a vector and a map.
  void addDTD(DerivedTypeDef *derivedTypeDef) {
    derivedTypeDefs.push_back(derivedTypeDef);

    assert(nameToDTDMap.find(derivedTypeDef->getName()) == nameToDTDMap.end());
    nameToDTDMap[derivedTypeDef->getName()] = derivedTypeDef;

    assert(typeToDTDMap.find(derivedTypeDef->getType()) == typeToDTDMap.end());
    typeToDTDMap[derivedTypeDef->getType()] = derivedTypeDef;
  }

  DerivedTypeDefList getDTDs() const { return derivedTypeDefs; }

  // returns the derived-type-def of \p name that belongs to this
  // specification-part's PU.
  DerivedTypeDef *getDTD(llvm::StringRef name) {
    if (nameToDTDMap.find(name) == nameToDTDMap.end())
      return nullptr;
    return nameToDTDMap[name];
  }

  // returns the derived-type-def of \p type that belongs to this
  // specification-part's PU.
  DerivedTypeDef *getDTD(StructType *type) {
    if (typeToDTDMap.find(type) == typeToDTDMap.end())
      return nullptr;
    return typeToDTDMap[type];
  }

  ~SpecificationPart();
};

class ExecutionPart {
  Block *block;

protected:
  friend class ParseTreeBuilder;

  ExecutionPart(Block *block) : block(block) {}

public:
  ~ExecutionPart() { delete block; }

  std::string dump(llvm::raw_ostream &OS, int level = 0) const;

  Block *getBlock() { return block; }
};

} // namespace ast
} // namespace fc
#endif
