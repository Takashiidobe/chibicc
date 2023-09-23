#!/usr/bin/env bash

first="$(git rev-list HEAD | tac)"
second="$(git rev-list HEAD | tac | tail -n +2)"

mkdir -p diffs

first=($first)
second=($second)

second_len=${#second[@]}

git diff 4b825dc642cb6eb9a060e54bf8d69288fbee4904 ${first[0]} >> diffs/start.diff

for i in $(seq 0 $second_len); do
  git diff ${first[i]} ${second[i]} >> diffs/$i.diff
  i=$((i + 1))
done
