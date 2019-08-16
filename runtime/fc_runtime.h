#ifndef FC_RUNTIME_H
#define FC_RUNTIME_H

// NOTE: This File is used as common interface for both compiler and Runtime.

typedef enum {
  int1 = 1,
  int8,
  int32,
  int64,
  int128,
  float32,
  double_precision,
  array,
  string
} IOTypeKind;

#endif
