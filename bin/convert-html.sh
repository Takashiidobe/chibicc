#!/usr/bin/env bash

find site/ -mmin -15 -name '*.html' -exec sed -i 's/\.md/\.html/g' {} +
