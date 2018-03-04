#!/bin/bash

vcount=$(git rev-list --count --first-parent HEAD)
vnum=$(git log -n 1 --pretty=format:"%h")

echo "// git revision" > src/revision.inc
echo "const RevisionStr = '$vcount-$vnum';" >> src/revision.inc

