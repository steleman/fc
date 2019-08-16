#include "codegen/RuntimeHelper.h"
#include "common/Debug.h"

#include "llvm/IR/Module.h"

extern "C" {
#include "runtime/fc_runtime.h"
}

using namespace fc;

static IOTypeKind getPrintTypeKind(llvm::Type *Ty) {
  if (Ty->isIntegerTy(1)) {
    return IOTypeKind::int1;
  }

  if (Ty->isIntegerTy(8)) {
    return IOTypeKind::int8;
  }
  if (Ty->isIntegerTy(32)) {
    return IOTypeKind::int32;
  }
  if (Ty->isIntegerTy(64)) {
    return IOTypeKind::int64;
  }
  if (Ty->isIntegerTy(128)) {
    return IOTypeKind::int128;
  }
  if (Ty->isFloatTy()) {
    return IOTypeKind::float32;
  }
  if (Ty->isDoubleTy()) {
    return IOTypeKind::double_precision;
  }
  if (Ty->isArrayTy()) {
    auto arrayTy = llvm::dyn_cast<llvm::ArrayType>(Ty);
    if (arrayTy->getElementType()->isIntegerTy(8))
      return IOTypeKind::string;
    return IOTypeKind::array;
  }
  if (Ty->isStructTy()) {
    if (Ty->getContainedType(1)->isArrayTy()) {
      return IOTypeKind::array;
    }
  }
  if (Ty->isPointerTy()) {
    auto PTy = llvm::dyn_cast<llvm::PointerType>(Ty);

    if (PTy->getElementType()->isIntegerTy(8))
      return IOTypeKind::string;

    auto arrayTy = llvm::dyn_cast<llvm::ArrayType>(PTy->getElementType());
    if (arrayTy && arrayTy->getElementType()->isIntegerTy(8))
      return IOTypeKind::string;

    auto StructTy = llvm::dyn_cast<llvm::ArrayType>(PTy->getElementType());
    if (StructTy && StructTy->getContainedType(1)->isArrayTy()) {
      return IOTypeKind::array;
    }
    llvm_unreachable("Unhanled string pointer in print!");
  }
  llvm_unreachable("unhandled IOTypeKind");
}

static IOTypeKind getReadTypeKind(llvm::Type *PTy) {
  assert(PTy->isPointerTy() && "Expecting a pointer type");
  auto pointerTy = llvm::dyn_cast<llvm::PointerType>(PTy);
  assert(pointerTy);
  auto Ty = pointerTy->getElementType();

  // TODO : We can instead call getIOTypeKind once everything is stable
  if (Ty->isIntegerTy(1))
    return IOTypeKind::int1;
  if (Ty->isIntegerTy(8))
    return IOTypeKind::int8;
  if (Ty->isIntegerTy(32))
    return IOTypeKind::int32;
  if (Ty->isIntegerTy(64))
    return IOTypeKind::int64;
  if (Ty->isFloatTy())
    return IOTypeKind::float32;
  if (Ty->isDoubleTy())
    return IOTypeKind::double_precision;
  if (Ty->isArrayTy())
    return IOTypeKind::array;
  error() << "Unhandled type " << *Ty << "\n";
  llvm_unreachable("Unhandled read pointer");
}

void RuntimeHelper::fillPrintArgsFor(
    llvm::Value *val, llvm::SmallVectorImpl<llvm::Value *> &argsList,
    llvm::Value *arrDimSize, llvm::IRBuilder<> *IRB, bool isDynArr) {

  llvm::Type *Ty = val->getType();
  if (arrDimSize)
    Ty = Ty->getPointerElementType();

  IOTypeKind typeKind = getPrintTypeKind(Ty);

  // 1. push the element type.
  argsList.push_back(IRB->getInt32((int)typeKind));

  // If scalar, just send the val.
  if (typeKind != IOTypeKind::array) {
    // 2 Push the value in case of scalar and return.
    if (typeKind == IOTypeKind::float32) {
      val = IRB->CreateFPCast(val, IRB->getDoubleTy(), "");
    }

    if (typeKind == IOTypeKind::int1) {
      val = IRB->CreateSExt(val, IRB->getInt32Ty());
    }
    argsList.push_back(val);
    return;
  }

  // if array, do more.
  // Get the base kind
  while (Ty->isArrayTy()) {
    Ty = Ty->getArrayElementType();
  }

  if (Ty->isStructTy()) {
    auto StructTy = llvm::cast<llvm::StructType>(Ty);
    Ty = StructTy->getContainedType(0);
    Ty = Ty->getPointerElementType();
  }

  IOTypeKind baseKind = getPrintTypeKind(Ty);
  assert(baseKind != IOTypeKind::array);

  // 2. Push the base element type of array.
  argsList.push_back(IRB->getInt32((int)baseKind));

  // 3. Push the dimensions
  argsList.push_back(arrDimSize);

  // 4. Push the actual array base.

  // If array, pass the dimension sizes for printing.
  // First, get the bitcast.
  if (isDynArr) {
    auto Load = IRB->CreateStructGEP(nullptr, val, 0, val->getName() + ".ptr");
    val = IRB->CreateLoad(Load, val->getName() + ".base");
  }
  auto BC = IRB->CreateBitCast(val, IRB->getInt8Ty()->getPointerTo());
  argsList.push_back(BC);
}

void RuntimeHelper::fillReadArgsFor(
    llvm::Value *val, llvm::SmallVectorImpl<llvm::Value *> &argsList,
    llvm::Value *arrDimSize, llvm::IRBuilder<> *IRB, bool isString) {

  llvm::Type *Ty = val->getType();

  IOTypeKind typeKind;
  if (isString)
    typeKind = IOTypeKind::string;
  else
    typeKind = getReadTypeKind(Ty);

  // 1. push the element type.
  argsList.push_back(IRB->getInt32((int)typeKind));

  // If scalar, just send the val.
  if (typeKind != IOTypeKind::array) {
    // 2 Push the value in case of scalar and return.
    argsList.push_back(val);
    return;
  }

  // if array, do more.
  // Get the base kind
  Ty = Ty->getPointerElementType();
  while (Ty->isArrayTy()) {
    Ty = Ty->getArrayElementType();
  }

  IOTypeKind baseKind = getPrintTypeKind(Ty);
  assert(baseKind != IOTypeKind::array);

  // 2. Push the base element type of array.
  argsList.push_back(IRB->getInt32((int)baseKind));

  // 3. Push the dimensions
  argsList.push_back(arrDimSize);

  // 4. Push the actual array base.

  // If array, pass the dimension sizes for printing.
  // First, get the bitcast.
  auto BC = IRB->CreateBitCast(val, IRB->getInt8Ty()->getPointerTo());
  argsList.push_back(BC);
}

void RuntimeHelper::fillOpenArgsFor(
    llvm::Value *unit, llvm::Value *fileName,
    llvm::SmallVectorImpl<llvm::Value *> &argList, llvm::IRBuilder<> *IRB) {
  auto BC = IRB->CreateBitCast(fileName, IRB->getInt8Ty()->getPointerTo());
  argList.push_back(unit);
  argList.push_back(BC);
}

llvm::Function *RuntimeHelper::getPrintFunction() {

  auto FnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                                      {llvm::Type::getInt32Ty(Context)}, true);
  auto func =
      static_cast<llvm::Function *>(M->getOrInsertFunction(printFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getWriteFunction() {

  auto FnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                                      {llvm::Type::getInt32Ty(Context)}, true);
  auto func =
      static_cast<llvm::Function *>(M->getOrInsertFunction(writeFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getReadFunction() {

  auto FnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                                      {llvm::Type::getInt32Ty(Context)}, true);
  auto func =
      static_cast<llvm::Function *>(M->getOrInsertFunction(readFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getStrCpyFunction() {
  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto FnTy = llvm::FunctionType::get(I8Ptr, {I8Ptr, I8Ptr}, true);
  auto func =
      static_cast<llvm::Function *>(M->getOrInsertFunction("strcpy", FnTy));

  auto arg1 = func->arg_begin();
  (*arg1).addAttr(llvm::Attribute::WriteOnly);
  arg1++;
  // second argument.
  (*arg1).addAttr(llvm::Attribute::ReadOnly);
  return func;
}

llvm::Function *RuntimeHelper::getStrCatFunction() {
  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto FnTy = llvm::FunctionType::get(I8Ptr, {I8Ptr, I8Ptr}, false);
  auto func =
      static_cast<llvm::Function *>(M->getOrInsertFunction("strcat", FnTy));

  auto arg1 = func->arg_begin();
  arg1++;
  // second argument.
  (*arg1).addAttr(llvm::Attribute::ReadOnly);
  return func;
}

llvm::Function *RuntimeHelper::getOpenFunction() {
  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto I32 = llvm::Type::getInt32Ty(Context);
  auto FnTy = llvm::FunctionType::get(I32, {I32, I8Ptr}, false);
  auto func =
      static_cast<llvm::Function *>(M->getOrInsertFunction(openFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getFileReadFunction() {

  auto I32 = llvm::Type::getInt32Ty(Context);
  auto FnTy = llvm::FunctionType::get(
      I32, {I32, llvm::Type::getInt32Ty(Context)}, true);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(fileReadFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getFileWriteFunction() {

  auto I32 = llvm::Type::getInt32Ty(Context);
  auto FnTy =
      llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                              {I32, llvm::Type::getInt32Ty(Context)}, true);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(fileWriteFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getCloseFunction() {
  auto I32 = llvm::Type::getInt32Ty(Context);
  auto FnTy = llvm::FunctionType::get(I32, {I32}, false);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(fileCloseFnName, FnTy));
  return func;
}

llvm::Function *RuntimeHelper::getStringToIntFunction() {
  auto I32Ty = llvm::Type::getInt32Ty(Context);
  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto FnTy = llvm::FunctionType::get(I32Ty, {I8Ptr}, false);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(stringToIntFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getStringToIntArrayFunction() {
  auto I32Ptr = llvm::Type::getInt32Ty(Context)->getPointerTo();
  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto FnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                                      {I8Ptr, I32Ptr}, false);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(stringToIntArrayFnName, FnTy));

  auto arg1 = func->arg_begin();
  (*arg1).addAttr(llvm::Attribute::ReadOnly);
  arg1++;
  // second argument.
  (*arg1).addAttr(llvm::Attribute::WriteOnly);
  return func;
}

llvm::Function *RuntimeHelper::getIntToStringFunction() {
  auto I32 = llvm::Type::getInt32Ty(Context);
  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto FnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                                      {I8Ptr, I32}, false);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(intToStringFnName, FnTy));

  auto arg1 = func->arg_begin();
  (*arg1).addAttr(llvm::Attribute::WriteOnly);
  return func;
}

llvm::Function *RuntimeHelper::getStrCmpFunction() {
  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto I32 = llvm::Type::getInt32Ty(Context);
  auto FnTy = llvm::FunctionType::get(I32, {I8Ptr, I8Ptr}, true);
  auto func =
      static_cast<llvm::Function *>(M->getOrInsertFunction("strcmp", FnTy));

  auto arg1 = func->arg_begin();
  (*arg1).addAttr(llvm::Attribute::ReadOnly);
  arg1++;
  // second argument.
  (*arg1).addAttr(llvm::Attribute::ReadOnly);
  return func;
}

llvm::Function *RuntimeHelper::getSprintfFunction() {

  auto I8Ptr = llvm::Type::getInt8PtrTy(Context);
  auto FnTy =
      llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                              {llvm::Type::getInt32Ty(Context), I8Ptr}, true);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(sprintfFnName, FnTy));

  return func;
}

llvm::Function *RuntimeHelper::getISysClockFunction() {
  auto I32Ptr = llvm::Type::getInt32PtrTy(Context);
  auto FnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(Context),
                                      {I32Ptr, I32Ptr, I32Ptr}, false);
  auto func = static_cast<llvm::Function *>(
      M->getOrInsertFunction(isysClockFnName, FnTy));
  return func;
}
