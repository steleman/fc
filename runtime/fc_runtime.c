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
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "fc_runtime.h"

static int notFirstLine = 0;
static FILE *lastFile = NULL;

// TODO To be decided as per standards and OS limit if any
// TODO All file related operations are brute-force. May not be
//      efficient
#define MAXFILES 20

static int units[MAXFILES];
static FILE *fdArray[MAXFILES];
static FILE *currFile;
static int currUnit;
static int currIndex;
static int totalOpenFiles;

static inline int getFileIndexFor(int unit) {
  for (int i = 0; i < MAXFILES; ++i) {
    if (units[i] == unit)
      return i;
  }
  return -1;
}

static inline int getEmptyIndex() {
  for (int i = 0; i < MAXFILES; ++i) {
    if (units[i] == 0)
      return i;
  }
  return -1;
}

static inline void printInt128(__int128 inp) {
  char inpStr[39] = {
      '\0'}; // 39 is the width of the largest signed 128 bit number
  char *ptr = inpStr + sizeof(inpStr) / sizeof(inpStr[0]);
  int mul = 1;

  if (inp < 0) {
    printf("-");
    mul = -1;
  }

  for (; inp != 0; inp /= 10) { // base = 10
    *(--ptr) = (char)('0' + (inp % 10) * mul);
  }

  printf("%s", ptr);
}

static inline void printScalar(IOTypeKind kind, va_list *list) {
  switch (kind) {
  case int1: {
    unsigned int val = va_arg(*list, int);
    printf("%12c", (val == 0) ? 'F' : 'T');
    break;
  }

  case int8: {
    char ch = va_arg(*list, char);
    printf("%12c", ch);
    break;
  }
  case int32:
    printf("%12d", va_arg(*list, int));
    break;
  case int64:
    printf("%12ld", va_arg(*list, long int));
    break;

  case float32: {
    double d = va_arg(*list, double);
    printf("%12.8f", (float)d);
    break;
  }
  case double_precision:
    printf("%12.8lf", va_arg(*list, double));
    break;
  case string:
    printf(" %-19s", va_arg(*list, char *));
    break;
  };
}

static inline void printFormattedScalar(IOTypeKind kind, va_list *list) {
  switch (kind) {
  case int1: {
    unsigned int val = va_arg(*list, int);
    printf("%c", (val == 0) ? 'F' : 'T');
    break;
  }

  case int32:
    printf("%d", va_arg(*list, int));
    break;
  case int64:
    printf("%ld", va_arg(*list, long int));
    break;

  case float32: {
    double d = va_arg(*list, double);
    printf("%12.8f", (float)d);
    break;
  }
  case double_precision:
    printf("%20.11lf", va_arg(*list, double));
    break;
  case string:
    printf(" %s", va_arg(*list, char *));
    break;
  };
}

static inline void fprintScalar(IOTypeKind kind, va_list *list, FILE *fd) {
  switch (kind) {
  case int1: {
    unsigned int val = va_arg(*list, int);
    fprintf(fd, "%12c", (val == 0) ? 'F' : 'T');
    break;
  }

  case int32: {
    fprintf(fd, "%12d", va_arg(*list, int));
    break;
  }
  case int64:
    fprintf(fd, "%12ld", va_arg(*list, long int));
    break;

  case float32: {
    double d = va_arg(*list, double);
    fprintf(fd, "%12.8f", (float)d);
    break;
  }
  case double_precision:
    fprintf(fd, "%12.8lf", va_arg(*list, double));
    break;
  case string:
    fprintf(fd, " %-19s", va_arg(*list, char *));
    break;
  };
}

static inline void fprintFormattedScalar(IOTypeKind kind, va_list *list,
                                         FILE *fd) {
  switch (kind) {
  case int1: {
    unsigned int val = va_arg(*list, int);
    fprintf(fd, "%c ", (val == 0) ? 'F' : 'T');
    break;
  }

  case int32: {
    fprintf(fd, "%5d ", va_arg(*list, int));
    break;
  }
  case int64:
    fprintf(fd, "%5ld ", va_arg(*list, long int));
    break;

  case float32: {
    double d = va_arg(*list, double);
    fprintf(fd, "%f ", (float)d);
    break;
  }
  case double_precision:
    fprintf(fd, "%f ", va_arg(*list, double));
    break;
  case string:
    fprintf(fd, "%s ", va_arg(*list, char *));
    break;
  case int8:
    fprintf(fd, "%c ", (char)(va_arg(*list, int)));
    break;
  };
}

static inline void readScalar(IOTypeKind kind, va_list *list) {
  char ch;
  switch (kind) {
  case int1: {
    printf("handle int1\n");
    exit(0);
  }
  case int32:
    scanf("%d", va_arg(*list, int *));
    break;
  case int64:
    scanf("%ld", va_arg(*list, long int *));
    break;
  case float32:
    scanf("%f", va_arg(*list, float *));
    break;
  case double_precision:
    scanf("%lf", va_arg(*list, double *));
    break;
  case string:
    scanf("%s", va_arg(*list, char *));
    ch = getchar();
    while (ch != '\n' && ch != EOF) {
      ch = getchar();
    }
    break;
    break;
  };
}

static inline int freadScalar(IOTypeKind kind, va_list *list, FILE *fd) {
  char ch;
  int iostat;
  switch (kind) {
  case int1: {
    printf("handle int1\n");
    exit(0);
  }
  case int32:
    iostat = fscanf(fd, "%d", va_arg(*list, int *));
    break;
  case int64:
    iostat = fscanf(fd, "%ld", va_arg(*list, long int *));
    break;
  case float32:
    iostat = fscanf(fd, "%f", va_arg(*list, float *));
    break;
  case double_precision:
    iostat = fscanf(fd, "%lf", va_arg(*list, double *));
    break;
  case string:
    iostat = fscanf(fd, "%s", va_arg(*list, char *));

    // Ignoring rest of the line while reading from the file.
    // TODO this has to be decided based on the length of string
    //      Remove this once format is handled completely
    ch = (char)fgetc(fd);
    while (ch != '\n' && ch != EOF) {
      ch = (char)fgetc(fd);
    }
    if (ch == EOF)
      return -1;
    break;
  };

  if (iostat == EOF)
    return -1;
  return 0;
}

static inline int handleReadElement(va_list *list, int *numArgs, FILE *fd) {
  // Get the type and kind
  IOTypeKind typeKind = (IOTypeKind)(va_arg(*list, int));
  (*numArgs)--;
  int iostat = 0;

  if (typeKind != array) {
    // Scan the scalara value.
    if (fd == NULL)
      readScalar(typeKind, list);
    else
      iostat = freadScalar(typeKind, list, fd);
    (*numArgs)--;
    return iostat;
  }
  // Get the base elementtype.
  IOTypeKind baseKind = (IOTypeKind)(va_arg(*list, int));
  (*numArgs)--;

  // Get the dims.
  int totalSize = va_arg(*list, int);
  (*numArgs)--;

  // Get the array.
  void *arr = va_arg(*list, void *);
  (*numArgs)--;

  if (fd == NULL) {
    char ch, oldch;
    for (int i = 0; i < totalSize; ++i) {
      switch (baseKind) {
      case int1: {
        printf("handle read for int1");
        exit(0);
        break;
      }
      case int8:
        ch = getchar();
        oldch = '\0';
        while (ch != '\n' && ch != ' ' && ch != EOF) {
          if (oldch == '\0')
            oldch = ch;
          ch = getchar();
        }
        *((char *)arr + i) = oldch;
        // scanf("%1c", ((char *)arr)+i);
        break;
      case int32:
        scanf("%d", ((int *)arr) + i);
        break;
      case int64:
        scanf("%ld", ((long int *)arr) + i);
        break;
      case float32:
        scanf("%f", ((float *)arr) + i);
        break;
      case double_precision:
        scanf("%lf", ((double *)arr) + i);
        break;
      };
    }
  } else {
    char ch, oldch;
    for (int i = 0; i < totalSize; ++i) {
      switch (baseKind) {
      case int1: {
        exit(0);
        break;
      }
      case int8:
        ch = (char)fgetc(fd);
        oldch = '\0';
        while (ch != '\n' && ch != ' ' && ch != EOF) {
          if (oldch == '\0')
            oldch = ch;
          ch = (char)fgetc(fd);
        }
        *((char *)arr + i) = oldch;
        if (ch == EOF)
          return -1;
        break;
      case int32:
        iostat = fscanf(fd, "%d", ((int *)arr) + i);
        break;
      case int64:
        iostat = fscanf(fd, "%ld", ((long int *)arr) + i);
        break;
      case float32:
        iostat = fscanf(fd, "%f", ((float *)arr) + i);
        break;
      case double_precision:
        iostat = fscanf(fd, "%lf", ((double *)arr) + i);
        break;
      };

      if (iostat == EOF)
        return -1;
    }
  }
  return iostat;
}

static inline void handleElement(va_list *list, int *numArgs, FILE *fd,
                                 int numSpaces) {
  // Get the type kind.
  IOTypeKind typeKind = (IOTypeKind)(va_arg(*list, int));
  (*numArgs)--;

  if (typeKind != array) {
    // This is scalar type, print the val.
    if (fd == NULL) {
      if (numSpaces == -1)
        printScalar(typeKind, list);
      else if (numSpaces == 0) {
        putchar(' ');
      } else {
        for (unsigned k = 0; k < numSpaces; ++k)
          putchar(' ');
        printFormattedScalar(typeKind, list);
      }
    } else {
      if (numSpaces == -1)
        fprintScalar(typeKind, list, fd);
      else {
        for (unsigned k = 0; k < numSpaces; ++k)
          fprintf(fd, " ");
        fprintFormattedScalar(typeKind, list, fd);
      }
    }
    (*numArgs)--;
    return;
  }

  // Get the base elementtype.
  IOTypeKind baseKind = (IOTypeKind)(va_arg(*list, int));
  (*numArgs)--;

  // Get the dims.
  int totalSize = va_arg(*list, int);
  (*numArgs)--;

  // Get the array.
  void *arr = va_arg(*list, void *);
  (*numArgs)--;

  if (fd == NULL) {
    for (int i = 0; i < totalSize; ++i) {
      switch (baseKind) {
      case int1: {
        unsigned char val = ((unsigned char *)arr)[i];
        printf("%12c", (val == 0) ? 'F' : 'T');
        break;
      }
      case int8:
        // Char arrays are like strings. To be printed without space.
        printf("%c", ((char *)arr)[i]);
        break;
      case int32:
        printf("%12d", ((int *)arr)[i]);
        break;
      case int64:
        printf("%12ld", ((long int *)arr)[i]);
        break;
      case float32:
        printf("%13.8lf", ((float *)arr)[i]);
        break;
      case double_precision:
        printf("%13.8lf", ((double *)arr)[i]);
        break;
      };

      if (baseKind != int8)
        putchar(' ');
    }
    putchar(' ');
  } else if (numSpaces == -1) {

    for (int i = 0; i < totalSize; ++i) {
      switch (baseKind) {
      case int1: {
        unsigned char val = ((unsigned char *)arr)[i];
        fprintf(fd, "%12c", (val == 0) ? 'F' : 'T');
        break;
      }
      case int8:
        fprintf(fd, "%12c", ((char *)arr)[i]);
        break;
      case int32:
        fprintf(fd, "%12d", ((int *)arr)[i]);
        break;
      case int64:
        fprintf(fd, "%12ld", ((long int *)arr)[i]);
        break;
      case float32:
        fprintf(fd, "%13.8lf", ((float *)arr)[i]);
        break;
      case double_precision:
        fprintf(fd, "%13.8lf", ((double *)arr)[i]);
        break;
      };
      fprintf(fd, " ");
    }
  } else {
    for (int i = 0; i < totalSize; ++i) {
      switch (baseKind) {
      case int1: {
        unsigned char val = ((unsigned char *)arr)[i];
        fprintf(fd, "%c", (val == 0) ? 'F' : 'T');
        break;
      }
      case int8:
        fprintf(fd, "%c", ((char *)arr)[i]);
        break;
      case int32:
        fprintf(fd, "%d", ((int *)arr)[i]);
        break;
      case int64:
        fprintf(fd, "%ld", ((long int *)arr)[i]);
        break;
      case float32:
        fprintf(fd, "%lf", ((float *)arr)[i]);
        break;
      case double_precision:
        fprintf(fd, "%lf", ((double *)arr)[i]);
        break;
      };

      for (int j = 0; j < numSpaces; j++)
        fprintf(fd, " ");
    }
  }
}

// Handling fortran PRINT statements!'
extern void __fc_runtime_print(int numArgs, ...) {
  va_list list;
  int i;

  va_start(list, numArgs);
  while (numArgs > 0) {
    handleElement(&list, &numArgs, NULL, -1);
  }
  va_end(list);
  putchar('\n');
}

extern void __fc_runtime_write(int numArgs, ...) {
  va_list list;
  int i;

  va_start(list, numArgs);
  while (numArgs > 0) {
    int numSpaces = (int)(va_arg(list, int));
    numArgs--;
    handleElement(&list, &numArgs, NULL, numSpaces);
  }
  va_end(list);
  putchar('\n');
}

// Handling fortran READ statement
extern void __fc_runtime_scan(int numArgs, ...) {
  va_list list;
  va_start(list, numArgs);
  while (numArgs > 0) {
    handleReadElement(&list, &numArgs, NULL);
  }
  va_end(list);
}

extern int __fc_runtime_open(int unit, const char *fileName) {
  if (unit == currUnit) {
    fprintf(stderr,
            "\n FC RUNTIME : Trying to use an unit which is not closed\n");
    exit(1);
  }

  if (totalOpenFiles > 20) {
    fprintf(stderr, "\n FC RUNTIME : Reached maximum open file limit\n");
    exit(1);
  }

  if (unit == 5 || unit == 6) {
    fprintf(stderr,
            "\n FC RUNTIME : can not use unit number %d for opening files",
            unit);
    exit(1);
  }

  if (getFileIndexFor(unit) != -1) {
    fprintf(stderr,
            "\n FC RUNTIME : Trying to use an unit which is not closed\n");
    exit(1);
  }

  int newIndex = getEmptyIndex();
  if (newIndex == -1) {
    fprintf(stderr, "\n Something went wrong. Can not reach here\n");
    exit(1);
  }

  FILE *file = fopen(fileName, "ab+");


  if (file == NULL) {
    fprintf(stderr, "\nFC RUNTIME: failed to open the file %s\n", fileName);
    perror(fileName);
    return 1;
    //exit(1);
  }

  units[newIndex] = unit;
  fdArray[newIndex] = file;
  currUnit = unit;
  currFile = file;
  currIndex = newIndex;
  totalOpenFiles++;
  return 0;
}

extern int __fc_runtime_close(int unit) {
  int status;
  if (unit == 5 || unit == 6) {
    fprintf(stderr,
            "\n FC RUNTIME : can not use unit number %d for opening files",
            unit);
    exit(1);
  }

  if (currUnit == unit) {
    totalOpenFiles--;
    status = fclose(currFile);
    if (status != 0) {
      fprintf(stderr, "FC RUNTIME : Failed to close the file\n");
      return 1;
    }
    units[currIndex] = 0;
    fdArray[currIndex] = NULL;
    currFile = NULL;
    currIndex = -1;
    currUnit = -1;
    return 0;
  }

  int newIndex = getFileIndexFor(unit);
  if (newIndex == -1) {
    fprintf(stderr, "FC RUNTIME: Closing unopened index\n");
    return 1;
  }

  status = fclose(fdArray[newIndex]);
  units[newIndex] = 0;
  fdArray[newIndex] = NULL;
  if (status != 0) {
    fprintf(stderr, "FC RUNTIME: Failed to close the file\n");
    totalOpenFiles--;
    return 1;
  }
  totalOpenFiles--;
  return 0;
}

extern int __fc_runtime_fread(int unit, int numArgs, ...) {
  va_list list;
  va_start(list, numArgs);
  FILE *fd;
  int newIndex;

  if (unit == 5) {
    fd = NULL;
  } else {
    if (unit == currUnit) {
      fd = currFile;
    } else {
      newIndex = getFileIndexFor(unit);
      if (newIndex == -1) {
        fprintf(stderr, "FC RUNTIME: Reading unopened index\n");
        return 1;
      }
      fd = fdArray[newIndex];
    }
  }

  int success;
  while (numArgs > 0) {
    success = handleReadElement(&list, &numArgs, fd);
    if (success != 0)
      return success;
  }
  return 0;
}

// Handling fortran file write
extern void __fc_runtime_fwrite(int unit, int numArgs, ...) {
  va_list list;
  va_start(list, numArgs);
  int index;
  FILE *fd;

  if (unit == 6) {
    fd = NULL;
  } else if (unit == currUnit) {
    fd = currFile;
  } else {
    index = getFileIndexFor(unit);
    if (index == -1) {
      fprintf(stderr, "FC RUNTIME writing to a file which is not opened");
      exit(0);
    }
    fd = fdArray[index];
  }

  // TODO More finer handling is required
  if (fd != NULL) {
    if (notFirstLine) {
      fprintf(fd, "\n");
    } else {
      notFirstLine = 1;
    }
  }
  while (numArgs > 0) {
    int numSpaces = (int)(va_arg(list, int));
    numArgs--;
    handleElement(&list, &numArgs, fd, numSpaces);
  }
  va_end(list);

  if (fd == NULL)
    printf("\n");
}

// String to int convertor
extern int __fc_runtime_stoi(char *array) { return atoi(array); }

// string to int array
extern void __fc_runtime_stoia(char *string, int *result) {
  while (string[0] != '\0') {
    if (!isdigit(string[0]))
      fprintf(stderr, "__fc_runtime_stoia: Error: Not a digit");
    result[0] = string[0] - '0';
    string++;
    result++;
  }
}

extern void __fc_runtime_itos(char *string, int val) {
  sprintf(string, "%d", val);
}

// trim() intrinsic in fortran.
extern void __fc_runtime_trim(char *arr, int size) {
  // traverse till you find the NULL
  int i;
  for (i = 0; i < size; ++i) {
    if (arr[i] == '\0')
      break;
  }
  if (i == 0) {
    return;
  }
  i--;
  while (i >= 0) {
    if (arr[i] != ' ' && arr[i] != '\t')
      break;
    i--;
  }
  arr[i + 1] = '\0';
}

extern void __fc_runtime_sprintf(int numArgs, char *dest, ...) {
  sprintf(dest, "");
  va_list list;
  int i;
  va_start(list, dest);
  while(numArgs > 0) {
    IOTypeKind kind = (IOTypeKind)(va_arg(list, int));
    numArgs--;

    switch(kind) {
      case int32:
        sprintf(dest, "%s%d", dest, va_arg(list, int));
        break;
      case int64:
        sprintf(dest, "%s%ld", dest, va_arg(list, long int));
        break;
      case float32: {
        double d = va_arg(list, double);
        sprintf(dest, "%s%f", dest, (float)d);
        break;
      }
      case double_precision:
        sprintf(dest, "%s%lf", dest, va_arg(list, double));
        break;
      case string:
        sprintf(dest, "%s%s", dest, va_arg(list, char *));
        break;
      default:
        fprintf(stderr, "Unhanled type %d in sprintf", (int)kind);
        break;
    }

    numArgs--;
  }
  va_end(list);
}

// Not sure how clock is calculared. ifort, gfortran, flang give
// diffent outputs. We return epoch!
extern void __fc_runtime_isysClock(int *clock, int *rate, int *max) {
  *clock = (int) time(NULL);
  *rate = CLOCKS_PER_SEC;
  *max = (1 << ((sizeof(*max) * 8) - 1)) - 1;
}
