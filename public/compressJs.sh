#!/bin/bash
JSFILES=src/*
CSSFILES=css/*

##########
## Js
##########

rm src/minified.js
touch src/minified.js

for f in $JSFILES
do
  cat f >> src/minified.js
done

yui-compressor src/minified.js > src/minified.js

##########
## Css
##########
rm css/minified.css
touch css/minified.css

for f in CSSFILES
do
  cat f >> css/minified.css
done

yui-compressor css/minified.css > css/minified.css
