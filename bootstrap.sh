#!/bin/bash
set -e
set -u
# sudo apt-get install python python-dev libxml2-dev
sudo apt-get install python-virtualenv python-dev libxml2-dev libxslt1-dev
virtualenv ENV
ENV/bin/pip install amara
ENV/bin/pip install lxml
