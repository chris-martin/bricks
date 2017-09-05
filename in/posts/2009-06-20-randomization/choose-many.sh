#!/bin/bash
r=${1:-1}   # number of items (default 1)
RANDOM=$$   # seed the rng with the pid
chosen=()   # will contain selected elements
i=1         # initialize loop counter

# read one line at a time
while read x; do

  # add 1..r sequentially
  if [ $i -le $r ]; then
    chosen[$((i-1))]="$x"

  # add the rest with probability r/i
  elif [ "$(($RANDOM % $i))" -lt "$r" ]; then
    chosen[$(($RANDOM % $r))]="$x"

  fi

  # increment loop counter
  i=$((i + 1))

done

for ((i = 0; i < ${#chosen[@]}; i++)); do
  echo "${chosen[i]}"
done
