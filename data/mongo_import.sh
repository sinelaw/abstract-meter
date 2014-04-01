find js -iname "*.js" |sort | xargs -n1 -t mongoimport  --collection articles --file 
