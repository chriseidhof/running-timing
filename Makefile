default: docs/main.html

docs/main.html: src/main.elm
  elm src/main.elm --output=docs/main.html
