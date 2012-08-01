#!/bin/bash
set -e
set -u
cd dtds
mkdir -p ../../../src/AbstractMeter/XML
DtdToHaskell pubmed_100101.dtd ../../../src/AbstractMeter/XML/PubMed.hs
