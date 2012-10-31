#!/bin/bash
set -e
set -u

cd $(dirname $0)/../data/xmls
mkdir -p ../js
pwd
time ls -1 *.xml | xargs -n1 -P4 -t -iyosi bash -c "xml-to-json -s --no-collapse-text yosi > ../js/yosi.js"
