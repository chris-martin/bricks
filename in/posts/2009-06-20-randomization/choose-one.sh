#!/bin/bash
RANDOM=$$   # seed the rng with the pid
chosen=''   # will hold selected element
i=1         # initialize loop counter

# read one line at a time
while read x; do

  # select x with probability 1/i
  if [ "$(($RANDOM % $i))" = "0" ]; then
    chosen="$x"
  fi

  # increment loop counter
  i=$((i + 1))

done

echo $chosen
