#!/bin/bash

for i in `find . -name '*.tex'`
do
  aspell --dont-backup -l en_GB -p ./report.dict -c $i
done
