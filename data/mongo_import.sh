find js -iname "*.js" -print0 | xargs -0 -n1 -t mongoimport  --collection articles --file 
