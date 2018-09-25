default: docs/index.html

docs/index.html: src/main.elm
	elm make src/main.elm --output=docs/index.html
