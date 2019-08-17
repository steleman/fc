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
#include "common/Diagnostics.h"
#include "common/Debug.h"
#include "common/Source.h"

using namespace fc;

static const char *getErrorMessage(diag::ErrorKind kind) {
  switch (kind) {
#define ERROR(X, Y)                                                            \
  case diag::X:                                                                \
    return #Y;
#include "common/messages.def"
  default:
    llvm_unreachable("unknown error");
  };
  return "";
}

static std::string getCurrLineStr(SourceLoc &loc) {
  auto temp = &loc.currStartOfLine[1];
  std::string str;
  while (temp[0] != '\n' && temp[0] != EOF) {
    str += temp[0];
    ++temp;
  }
  str = "\n\t" + str + "\n\t";
  str.append(loc.Col - 1, ' ');
  str.append("^");
  return str;
}

void Diagnostics::printError(SourceLoc loc, diag::ErrorKind kind) {

  errorSet = true;
  error() << "\n"
          << filename.str() << ":" << loc.Line << ":" << loc.Col
          << ": error: " << getErrorMessage(kind);

  error() << getCurrLineStr(loc);
}
