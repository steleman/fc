#!/bin/bash
set -e
base=`basename $2 ".f90"`
outfile=$USER
outfile+="_"
outfile+=$3.ast.c
$1 $2 -emit-ast -o  /tmp/$outfile
echo "Parsing is fine."
diff /tmp/$outfile ./expected_ast/$base.ast.c

