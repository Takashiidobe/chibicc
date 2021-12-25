#!/usr/bin/env bash

first="$(git rev-list HEAD | tac)"
second="$(git rev-list HEAD | tac | tail -n +2)"

first=($first)
second=($second)

second_len=${#second[@]}

for i in $(seq 0 $second_len); do
  git diff ${first[i]} ${second[i]} >> diffs/$i.diff
done
