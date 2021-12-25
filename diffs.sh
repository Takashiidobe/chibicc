#!/usr/bin/env bash

first="$(git rev-list HEAD | tac)"
second="$(git rev-list HEAD | tac | tail -n +2)"

mkdir -p diffs

first=($first)
second=($second)

second_len=${#second[@]}

git diff ${first[0]} >> diffs/0.diff

for i in $(seq 0 $second_len); do
  i=$((i + 1))
  git diff ${first[i]} ${second[i]} >> diffs/$i.diff
done
