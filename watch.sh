#!/usr/bin/env bash

find \( -name "*.md" -o -name "*.css" \) |
    entr -- stack exec -- site
