#!/bin/sh
set -e


rm -rf dist && mkdir -p dist

cp -v static/* dist/

elm-make src/elm-app.elm --output dist/elm-app.js
gopherjs build src/background.go -m -o dist/background.js
