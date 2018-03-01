#!/bin/bash

vcount=$(git rev-list --count --first-parent HEAD)
vnum=$(git describe --always HEAD)

echo "// git revision" > src/revision.inc
echo "const RevisionStr = '$vcount-$vnum';" >> src/revision.inc

