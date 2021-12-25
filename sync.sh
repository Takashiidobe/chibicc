#!/usr/bin/env bash

while true; do
  sleep 2s;
  rsync -azP --prune-empty-dirs --include="*/" --include="*.sh" --include="*.c" --exclude="*" ~/Desktop/chibicc "kflqez71@45.79.210.61:/home/kflqez71/"
done
