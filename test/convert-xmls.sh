time find ../data/xmls/*.xml -type f | xargs -n1 -t -iyosi bash -c "xml-to-json -snm yosi > yosi.js"
cd ../data/xmls
mv *.js ../js
