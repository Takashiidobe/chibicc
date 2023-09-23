#!/usr/bin/env bash

cd notes
mdbook build
ntl deploy --prod
