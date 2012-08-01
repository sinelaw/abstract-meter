#!/bin/bash
set -e
set -u
set -x

mkdir -p temp_dtds
cd temp_dtds
rm * -f
wget -r -np -nd http://www.ncbi.nlm.nih.gov/corehtml/query/DTD/index.shtml
mkdir -p ../dtds
mv *.dtd ../dtds
cd ..
rm -rf temp_dtds

