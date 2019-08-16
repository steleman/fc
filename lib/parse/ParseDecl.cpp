#include "AST/Declaration.h"
#include "AST/ProgramUnit.h"
#include "parse/Parser.h"

using namespace fc;
using namespace parser;

void Parser::parseParameterStmt() {
  assert(is(tok::kw_parameter));
  if (!expect(tok::l_paren)) {
    Diag.printError(getCurrLoc(), diag::err_paramstmt);
    return;
  }
  auto name = expectIdentifier();
  Symbol *sym = context.currSymTable->getSymbol(name);
  assert(sym && "Using paramter over undeclared variable");

  consumeToken(); // consume =
  auto init = parseExpr();

  // TODO: If it's not constant, Need to update entity decl
  ConstantVal *initConstant = llvm::dyn_cast<ConstantVal>(init);
  assert(init);
  sym->setInitConstant(initConstant->getConstant());
  sym->setConstantAttribute();

  assert(is(tok::r_paren));
  if (!expect(tok::eol)) {
    Diag.printError(getCurrLoc(), diag::err_paramstmt);
    return;
  }
}

ArraySpec *Parser::updateArraySpec(ArraySpec *arraySpec, ArraySpec *charSpec) {
  if (!arraySpec) {
    return charSpec;
  }

  // FIXME:  Handle the following case,
  // currently not pushing anything to bounds list. Do we need to add nullptrs
  // for dimension(:) ?
  // character(line_length), dimension(:), intent(in) :: keywords
  auto boundsList = arraySpec->getBoundsList();
  for (auto bounds : charSpec->getBoundsList()) {
    boundsList.push_back(bounds);
  }

  unsigned numDims = arraySpec->getNumDims() + charSpec->getNumDims();
  auto arrSpec =
      builder.buildArraySpec(boundsList, numDims, arraySpec->getSourceLoc());
  return arrSpec;
}

AttrSpec *Parser::parseAttributeSpec(SymbolAttributes &attributes) {

  assert(is(tok::comma));

  AttrSpecKind kind = expectAttrSpecKind();
  if (kind == AttrSpecKind::None) {
    Diag.printError(getCurrLoc(), diag::decl_exp_attr_spec_kind);
    return nullptr;
  }

  // consume attr spec.
  consumeToken();

  switch (kind) {
  case AttrSpecKind::Dimension: {
    if (isNot(tok::l_paren)) {
      Diag.printError(getCurrLoc(), diag::decl_exp_arr_spec);
      return nullptr;
    }
    auto arrSpec = parseArraySpec();
    if (!arrSpec || Diag.hasError()) {
      return nullptr;
    }
    return builder.buildAttrSpec(kind, arrSpec);
  }
  case AttrSpecKind::Allocatable: {
    attributes.isAllocatable = true;
    return builder.buildAttrSpec(kind);
  }
  case AttrSpecKind::Save: {
    return builder.buildAttrSpec(kind);
  }
  case AttrSpecKind::Parameter: {
    attributes.isConst = true;
    return builder.buildAttrSpec(kind);
  }
  case AttrSpecKind::Pointer: {
    attributes.isPointer = true;
    return builder.buildAttrSpec(kind);
  }
  case AttrSpecKind::Target: {
    attributes.isTarget = true;
    return builder.buildAttrSpec(kind);
  }
  case AttrSpecKind::Optional: {
    attributes.isOptional = true;
    return builder.buildAttrSpec(kind);
  }
  case AttrSpecKind::Private:
  case AttrSpecKind::Public: {
    return builder.buildAttrSpec(kind);
  }
  case AttrSpecKind::Intent: {

    if (isNot(tok::l_paren)) {
      Diag.printError(getCurrLoc(), diag::exp_l_paren);
      return nullptr;
    }
    // consume l_paren.
    consumeToken();
    IntentKind intentKind;
    switch (currTokenKind) {
    case tok::kw_in:
      intentKind = IntentKind::In;
      break;
    case tok::kw_inout:
      intentKind = IntentKind::InOut;
      break;
    case tok::kw_out:
      intentKind = IntentKind::Out;
      break;
    default:
      llvm_unreachable("unknown intent kind");
    };
    // consume intent kind.
    consumeToken();

    if (isNot(tok::r_paren)) {
      Diag.printError(getCurrLoc(), diag::exp_r_paren);
      return nullptr;
    }
    // consume r paren.
    consumeToken();
    attributes.intentKind = intentKind;
    return builder.buildIntentSpec(intentKind);
  }
  case AttrSpecKind::Intrinsic:
  default:
    llvm_unreachable("Unhandled attribute spec found");
  };

  return nullptr;
}

ArraySpec *Parser::parseArraySpec() {
  assert(is(tok::l_paren));

  // consume l_paren.
  consumeToken();

  DynArrBoundsList boundsList;
  unsigned numDims = 0;

  // parse: bounds-list = bounds [, bounds]
  // bounds = [lowerbound :] upperbound
  while (isNot(tok::r_paren)) {
    DynArrayBounds bound;
    numDims++;

    // Look for : in case of allocatable arrays
    if (isOneOf({tok::colon, tok::star})) {
      // Consume comma or r_paren
      consumeToken();

      if (!isOneOf({tok::r_paren, tok::comma})) {
        Diag.printError(getCurrLoc(), diag::arr_spec_expect_another_dim);
        return nullptr;
      }

      if (is(tok::r_paren))
        break;
      if (is(tok::comma)) {
        // consume next available ) or :
        consumeToken();
        continue;
      }
      llvm_unreachable("Should not reach here");
    }

    // Look for scalar constant now.
    auto *constant = parseExpr(false);

    // parse upper bound,
    if (is(tok::colon)) {
      consumeToken(tok::colon);

      bound.first = constant;
      if (!is(tok::star))
        bound.second = parseExpr(false);
      else {
        llvm_unreachable("assumed size as upper bound not supported yet");
      }

    } else {
      if (!isOneOf({tok::r_paren, tok::comma})) {
        Diag.printError(getCurrLoc(), diag::arr_spec_end);
        return nullptr;
      }
      bound.first = builder.buildConstantVal("1",
                                             getTypeFor(tok::integer,
                                                        /*kind =*/4),
                                             getCurrLoc());
      bound.second = constant;
    }
    if (is(tok::comma)) {
      // consume the comma.
      consumeToken();
    } else {
      assert(is(tok::r_paren));
    }
    boundsList.push_back(bound);
  }

  // Consume the ')'
  consumeToken();

  auto arrSpec = builder.buildArraySpec(boundsList, numDims, getCurrLoc());
  return arrSpec;
}

// entity-decl = object-name
EntityDecl *
Parser::parseEntityDecl(DeclarationTypeSpec *declTypeSpec, ArraySpec *arraySpec,
                        llvm::SmallVector<AttrSpec *, 2> &attrSpecList,
                        bool hasDoubleColon, SymbolAttributes &attributes,
                        ArraySpec *charArraySpec) {
  TypeSpec *typeSpec = declTypeSpec->getTypeSpec();
  Type *type = typeSpec->getBaseType();

  if (auto dts = llvm::dyn_cast<DerivedTypeSpec>(typeSpec)) {
    // The issue we have is that we can't get the DerivedTypeDef object here
    // from DerivedTypeSpec. All DTFs are stored in the specification part that
    // can be indexed by their name, but we are in the process of creating that
    // specification part!!!
    type = Type::getUndeclaredTy(FC);
  }

  // parse the name.
  auto name = checkIdentifier();
  if (name.empty()) {
    return nullptr;
  }

  SourceLoc loc = getCurrLoc();

  // consume the identifier token.
  consumeToken();

  ArraySpec *arrSpec = nullptr;
  // Parse array specifier.
  if (is(tok::l_paren)) {
    arrSpec = parseArraySpec();
  }

  // if there's an array spec in typedecl, use it
  // if there's an array spec in entitydecl use it
  // If both are present use type decl array spec,
  // sema will handle it. If either are not present,
  // pass nullptr.
  arraySpec = arrSpec ? arrSpec : arraySpec;

  if (arraySpec) {
    type = ArrayType::get(FC, type, arraySpec->getNumDims());
  }

  if (charArraySpec) {
    arraySpec = updateArraySpec(arraySpec, charArraySpec);
    type =
        ArrayType::get(FC, Type::getStringCharTy(FC), arraySpec->getNumDims());
  }

  Expr *init = nullptr;
  // Look if there is initialization.
  if (is(tok::equals)) {
    if (!hasDoubleColon) {
      Diag.printError(getCurrLoc(), diag::decl_double_colon_error);
      return nullptr;
    }

    init = parseExpr();
  }

  if (Diag.hasError())
    return nullptr;

  Symbol *sym = context.currSymTable->getSymbol(name);

  auto validScope = context.currSymTable->isSubroutineScope() ||
                    context.currSymTable->isFunctionScope();
  if (sym && validScope) {
    if (sym->getType()->getTypeID() != Type::DummyArgID &&
        (attributes.intentKind != Intent_None)) {
      Diag.printError(loc, diag::dup_variable);
      return nullptr;
    }
    attributes.allocKind = AllocationKind::Argument;

    sym->setType(type);
    sym->setAttributes(attributes);
    if (attributes.intentKind == Intent_None) {
      sym->setIntentKind(InOut);
    }
  } else {
    if (sym != nullptr) {
      Diag.printError(loc, diag::dup_variable);
      return nullptr;
    }
  }

  bool hasSave = false;
  for (auto &attrSpec : attrSpecList) {
    if (attrSpec->getKind() == AttrSpecKind::Save) {
      hasSave = true;
    }
  }

  auto currScope = context.currSymTable->getScopeKind();
  if (currScope == ModuleScope || context.isCurrUnitNested() || hasSave) {
    attributes.allocKind = AllocationKind::StaticGlobal;
    if (context.isCurrUnitNested()) {
      attributes.linkKind = LinkageKind::Link_Internal;
    }
  } else {
    attributes.allocKind = AllocationKind::StaticLocal;
  }

  if (sym == nullptr)
    sym = context.currSymTable->addNewSymbol(name, type, loc, attributes);

  if (init) {
    ConstantVal *initConstant = llvm::dyn_cast<ConstantVal>(init);
    if (initConstant)
      sym->setInitConstant(initConstant->getConstant());
  }

  auto entityDecl =
      builder.buildEntityDecl(sym, declTypeSpec, arraySpec, init, loc);
  return entityDecl;
}

// type-declaration-stmt =  declaration-type-spec [[, attr-spec ]... ::]
// entity-decl-list
void Parser::parseTypeDeclStmt(StmtList &stmtList) {
  auto declTypeSpec = parseDeclTypeSpec();
  assert(declTypeSpec);
  if (!declTypeSpec) {
    return;
  }

  auto intrinSpec =
      llvm::dyn_cast<IntrinsicTypeSpec>(declTypeSpec->getTypeSpec());

  ArraySpec *charArraySpec = nullptr;
  if (intrinSpec)
    charArraySpec = intrinSpec->getArraySpec();

  SymbolAttributes attributes;

  // parse optional attribute spec.
  llvm::SmallVector<AttrSpec *, 2> attrSpecList;
  ArraySpec *arraySpec = nullptr;
  while (is(tok::comma)) {
    auto attrSpec = parseAttributeSpec(attributes);
    if (!attrSpec || Diag.hasError())
      return;
    if (attrSpec->getKind() == AttrSpecKind::Dimension) {
      arraySpec = attrSpec->getArraySpec();
    }

    attrSpecList.push_back(attrSpec);
  }

  // Look for double colon. "::"
  // The double colon is optional when there is no
  // initialization of one of the members.
  // But it is compulsory when ther is initialization.
  // C505
  bool hasDoubleColon = false;
  if (is(tok::colon)) {
    if (expect(tok::colon)) {
      // expected for initializer.
      consumeToken();
      hasDoubleColon = true;
    } else {
      Diag.printError(getCurrLoc(), diag::decl_colon_error);
      return;
    }
  }

  // Now parse the entity declaration list
  llvm::SmallVector<EntityDecl *, 2> entityDeclList;
  while (true) {
    auto entityDecl =
        parseEntityDecl(declTypeSpec, arraySpec, attrSpecList, hasDoubleColon,
                        attributes, charArraySpec);

    // Some error occurred. Ignore.
    if (!entityDecl) {
      return;
    }

    stmtList.push_back(entityDecl);

    if (is(tok::comma)) {
      consumeToken();
      continue;
    }
    if (is(tok::eol)) {
      break;
    }

    llvm_unreachable("TypeDeclStatement: should not come here!");
  }

  if (Diag.hasError())
    return;

  // consume the eol token.
  consumeToken();
}

UseStmt *Parser::parseUseStmt() {
  assert(is(kw_use));

  consumeToken();
  bool isIntrinsic = false;

  if (isNot(tok::identifier)) {
    assert(is(tok::comma));
    consumeToken();
    if (is(tok::kw_intrinsic)) {
      isIntrinsic = true;
      if (!expect({tok::colon, tok::colon})) {
        Diag.printError(getCurrLoc(), diag::exp_only);
        return nullptr;
      }
      consumeToken(); // consume the identifier
    } else {
      error() << "Attribute : " << getToken().getRef() << "\n";
      llvm_unreachable("Unhandled attribute in use statement");
    }
  }

  if (isNot(tok::identifier)) {
    Diag.printError(getCurrLoc(), diag::ident_err1);
    return nullptr;
  }

  SourceLoc loc = getCurrLoc();
  llvm::StringRef moduleName = getToken().getRef();

  consumeToken(); // identifier

  ArgsList argList;
  if (is(tok::comma)) {
    if (!expect({tok::kw_only, tok::colon})) {
      Diag.printError(getCurrLoc(), diag::exp_only);
      return nullptr;
    }

    // TODO: not being used now.
    argList = parseArgsList(tok::eol);
  }

  assert(tok::eol);
  consumeToken(); // eol.

  // create new symbol table.
  auto modSymTable =
      builder.buildSymbolTable(FC, ModuleScope, moduleName, nullptr);
  return builder.buildUseStmt(modSymTable, loc, isIntrinsic);
}

void Parser::parseTypeAttributeSpec() {
  consumeToken(); // type attr spec

  // TODO: interpret type-attr-spec like public, private etc. Ignoring for now
}

DerivedTypeDef *Parser::parseDerivedTypeDef() {
  assert(is(tok::kw_type));
  consumeToken(); // type

  while (is(tok::comma)) {
    consumeToken(); // comma
    parseTypeAttributeSpec();
  }

  if (is(tok::colon)) {
    if (expect(tok::colon)) {
      consumeToken();
    } else {
      Diag.printError(getCurrLoc(), diag::decl_colon_error);
      return nullptr;
    }
  }

  auto name = checkIdentifier();
  if (name.empty()) {
    Diag.printError(getCurrLoc(), diag::ident_err1);
    return nullptr;
  }

  consumeToken(); // identifier (type name)

  if (!is(tok::eol)) {
    Diag.printError(getCurrLoc(), diag::exp_eol);
    return nullptr;
  }
  consumeToken(); // eol

  StmtList stmtList;

  SymbolTable *parentSymTable = context.currSymTable;

  // We need to inherit symbols that are accessible at the derived-type-def, ie.
  // whatever symbols in parentSymTable.
  SymbolTable *newSymTable = builder.buildSymbolTable(
      FC, parentSymTable->getScopeKind(), name, parentSymTable);

  context.currSymTable = newSymTable;

  // parse the body of derived-type-def
  while (isNot(tok::kw_end)) {
    if (is(tok::kw_sequence)) {
      if (!expect(tok::eol)) {
        Diag.printError(getCurrLoc(), diag::illegal_derived_type_decl);
        return nullptr;
      }
      consumeToken(); // eol
      continue;
    }

    if (isIntrinsicTypeSpec(currTokenKind)) {
      parseTypeDeclStmt(stmtList);
      continue;
    }

    if (is(tok::kw_type)) {
      // if type(.. declaration statement
      if (lookAhead(1).Kind == tok::l_paren) {
        parseTypeDeclStmt(stmtList);
        continue;
      }
    }

    Diag.printError(getCurrLoc(), diag::illegal_derived_type_decl);
    return nullptr;
  }

  context.currSymTable = parentSymTable;

  if (Diag.hasError())
    return nullptr;

  if (!expect(tok::kw_type)) {
    Diag.printError(getCurrLoc(), diag::mp_end_stmt_err1);
    return nullptr;
  }

  auto endName = expectIdentifier();

  // endName is optional like other fortran program units
  if (!endName.empty()) {
    if (name != endName) {
      Diag.printError(getCurrLoc(), diag::mp_end_stmt_err2);
      return nullptr;
    }
    consumeToken(); // endName
  }

  if (!is(tok::eol)) {
    Diag.printError(getCurrLoc(), diag::exp_eol);
    return nullptr;
  }
  consumeToken(); // eol

  auto block = builder.buildBlock(stmtList, getCurrLoc());
  llvm::SmallVector<Type *, 2> fieldTypes;
  llvm::SmallVector<std::string, 2> fieldNames;

  for (Stmt *stmt : stmtList) {
    if (auto entityDecl = llvm::dyn_cast<EntityDecl>(stmt)) {
      fieldNames.push_back(entityDecl->getSymbol()->getName());

      // To be resolved at the TypeUpdaterPass
      fieldTypes.push_back(Type::getVoidTy(FC));
    }
  }

  // We make the StructType's name globally unique by the following naming
  // scheme since we'll be referring this name for llvm:StructType's name as
  // well.
  // Creating a dt.<parent-PU-name><derived-type-def-name> format for the
  // struct-name to distinguish it from similar named dtd in other PUs.
  std::string _verboseName("dt." + context.currPU.str() + "." + name.str());
  // I'm not sure why concating in the ctor of StringRef produces garbage
  // sometimes, maybe use Twine ?
  llvm::StringRef verboseName(_verboseName);

  auto type = StructType::get(FC, verboseName, fieldTypes, fieldNames,
                              /* isDynArray = */ false);
  type->setDTDName(name);

  ProgramUnitList emptyPUL;
  return builder.buildDerivedTypeDef(name, newSymTable,
                                     parentSymTable->getProgramUnit(), block,
                                     type, emptyPUL);
}

SpecificationPart *Parser::parseSpecification() {

  StmtList stmtList;
  auto parentList = context.currStmtList;
  context.currStmtList = &stmtList;
  // 1. Handle use stmt kind.
  UseStmt *useStmt = nullptr;
  while (is(tok::kw_use)) {
    useStmt = parseUseStmt();
    if (!useStmt) {
      return nullptr;
    }
    stmtList.push_back(useStmt);
  }

  // 2. Handle import none.
  // TODO: add to stmt list.
  if (is(tok::kw_implicit)) {
    if (!expect(tok::kw_none)) {
      Diag.printError(getCurrLoc(), diag::expect_none);
      return nullptr;
    }

    assert(expect(tok::eol));
    consumeToken(); // eol.
  }

  DerivedTypeDefList derivedTypeDefs;

  // Just consume private/ public statements.
  while (isOneOf({tok::kw_private, tok::kw_public})) {
    consumeToken();
    while (isNot(tok::eol)) {
      consumeToken();
    }
    consumeToken(tok::eol);
  }

  // 3. Handle type decl stmts.
  while (true) {
    if (isIntrinsicTypeSpec(currTokenKind)) {
      parseTypeDeclStmt(stmtList);
      continue;
    }

    if (is(tok::kw_private) || is(tok::kw_public)) {
      consumeToken();
      while (isNot(tok::eol))
        consumeToken();
      consumeToken();
      continue;
    }

    if (is(tok::kw_type)) {
      // if type(.. declaration statement
      if (lookAhead(1).Kind == tok::l_paren) {
        parseTypeDeclStmt(stmtList);
        continue;
      }

      DerivedTypeDef *derivedTypeDef = parseDerivedTypeDef();
      if (!derivedTypeDef)
        return nullptr;
      derivedTypeDefs.push_back(derivedTypeDef);
      continue;

      Diag.printError(getCurrLoc(), diag::illegal_derived_type_decl);
      return nullptr;
    }

    if (is(tok::kw_parameter) && match({tok::l_paren, tok::identifier})) {
      parseParameterStmt();
      consumeToken();
      continue;
    }
    break;
  }

  if (Diag.hasError())
    return nullptr;

  auto block = builder.buildBlock(stmtList, getCurrLoc());
  context.currStmtList = parentList;

  SpecificationPart *specPart = builder.buildSpecificationPart(block);

  for (DerivedTypeDef *derivedTypeDef : derivedTypeDefs) {
    specPart->addDTD(derivedTypeDef);
  }

  return specPart;
}

DeclarationTypeSpec *Parser::parseDeclTypeSpec() {
  // Check for intrinsic declaration type.

  TokenKind typeKind = currTokenKind;
  auto loc = getCurrLoc();
  int size = -1;
  Expr *kindExpr = nullptr;

  switch (typeKind) {
  case tok::kw_double:
    assert(expect(tok::kw_precision));
  case tok::kw_integer:
  case tok::kw_real:
  case tok::kw_complex:
  case tok::kw_character:
  case tok::kw_logical:
  case tok::kw_type:
    break;
  default:
    Diag.printError(loc, diag::intrin_type_not_found);
    return nullptr;
  }

  // Check if derived-type-spec
  if (typeKind == tok::kw_type) {
    consumeToken(); // type
    assert(is(tok::l_paren));
    consumeToken(); // l_paren

    // type-name
    auto name = checkIdentifier();
    if (name.empty()) {
      Diag.printError(loc, diag::ident_err1);
      return nullptr;
    }

    if (!expect(tok::r_paren)) {
      Diag.printError(loc, diag::illegal_derived_type_decl);
      return nullptr;
    }
    consumeToken(); // r_paren

    auto typeSpec =
        builder.buildDerivedTypeSpec(Type::getUndeclaredTy(FC), name);

    // TODO maybe: shouldn't KindExpr be part of IntrinsicTypeSpec only and not
    // part of DeclarationTypeSpec ? derived-type-spec doesn't have a kind like
    // intrinsic-type-spec.
    return builder.buildDeclaratoinTypeSpec(typeSpec, loc, /* kindExpr = */
                                            nullptr);

  } else {
    // consume type specifier
    consumeToken();
  }

  ArraySpec *charLength = nullptr;
  if (is(tok::l_paren)) {
    // consume l paren.
    consumeToken();
    if (is(tok::kw_kind)) {
      if (!expect(tok::equals))
        return nullptr;

      // consume Equals.
      consumeToken();
      if (is(tok::integer)) {
        assert(getToken().constantTypeSize == -1);
        size = std::stoi(getToken().getRef().str());
        if (!expect(tok::r_paren)) {
          return nullptr;
        }
      } else {
        kindExpr = parseExpr(false);
        assert(is(tok::r_paren));
      }
    }

    else if (is(tok::kw_len)) {
      if (!expect(tok::equals))
        llvm_unreachable("Expecting =");

      consumeToken();
      DynArrBoundsList bList;
      // Assumed size arrays, len=*
      if (is(tok::star)) {
        charLength = builder.buildArraySpec(bList, 1, getCurrLoc());
        consumeToken(); // consume r_paren
      } else {
        auto val =
            builder.buildConstantVal("0", Type::getInt32Ty(FC), getCurrLoc());
        auto expr = parseExpr(false);
        bList.push_back(std::make_pair(val, expr));
        charLength = builder.buildArraySpec(bList, bList.size(), getCurrLoc());
      }

      assert(is(tok::r_paren) && "Expecting r_paren");
    } else {
      if (typeKind == kw_character) {
        auto val =
            builder.buildConstantVal("0", Type::getInt32Ty(FC), getCurrLoc());
        Expr *ub;

        if (is(tok::integer)) {
          ub = builder.buildConstantVal(getToken().getRef(),
                                        Type::getInt32Ty(FC), getCurrLoc());
          if (!expect(tok::r_paren)) {
            return nullptr;
          }
        } else {
          ub = parseExpr(false);
        }
        DynArrBoundsList bList;
        bList.push_back(std::make_pair(val, ub));
        charLength = builder.buildArraySpec(bList, bList.size(), getCurrLoc());
      } else {
        if (!expect(tok::r_paren)) {
          return nullptr;
        }
      }
    }

    // consume r_paren.
    consumeToken();
  }

  if (Diag.hasError())
    return nullptr;

  if (typeKind == tok::kw_character && charLength != nullptr) {
    typeKind = tok::string;
  }

  auto type = getTypeFor(typeKind, size, false);
  auto typeSpec = builder.buildIntrinsicTypeSpec(type, charLength);
  return builder.buildDeclaratoinTypeSpec(typeSpec, loc, kindExpr);
}
