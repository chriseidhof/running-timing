default: docs/index.html

docs/index.html: index.html docs/elm.js
	cp index.html docs/

docs/elm.js: src/main.elm
	elm make src/main.elm --output=docs/elm.js
