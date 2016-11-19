#!/usr/bin/env bash

echo "Non-HTML files:"

for x in $(find out -type f -name "*.*"); do
    aws s3 cp $x s3://chris-martin-org-1/${x#*/}
done

echo ""
echo "HTML files:"

for x in $(find out -type f -regex "^[^.]*"); do
    aws s3 cp $x s3://chris-martin-org-1/${x#*/} \
        --content-type text/html
done
