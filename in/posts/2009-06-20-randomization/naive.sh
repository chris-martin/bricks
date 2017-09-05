#!/bin/bash

all=()
i=0

while read x; do
  all[$i]="$x"
  i=$((i + 1))
done

echo "${all[$((RANDOM % i))]}"
