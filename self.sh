#!/bin/bash
set -e

TMP=$1
CC=$2
OUTPUT=$3

rm -rf $TMP
mkdir -p $TMP

kcc() {
    (cd $TMP; ../$CC -I.. ../$1 > ${1%.c}.s)
    gcc -c -o $TMP/${1%.c}.o $TMP/${1%.c}.s
}

cc() {
    gcc -c -o $TMP/${1%.c}.o $1
}

kcc main.c
kcc type.c
kcc parse.c
kcc codegen.c
kcc tokenize.c
kcc preprocess.c

(cd $TMP; gcc -static -o ../$OUTPUT *.o)
