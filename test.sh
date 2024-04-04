#!/bin/bash


assert() {
  expected="$1"
  input="$2"

  cargo r -q -- "$input" > tmp.s || exit
  gcc -static -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 38 '12   + 30  - 4'

echo OK

rm tmp tmp.s
