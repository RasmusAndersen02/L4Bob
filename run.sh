#!/usr/bin/env bash
set -e

if [ $# -ne 1 ]; then
  echo "Usage: $0 <filename-without-extension>"
  exit 1
fi

name="$1"

make

cd assembler
./assembler "${name}.bob" "${name}.out"

cd ../emulator
gdb --args ./emulator "../assembler/${name}.out"
/emulator ../assembler/$1.out
