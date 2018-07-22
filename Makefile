build: node_modules
	sassc -I node_modules src/main.scss > site.css
	elm-make --debug --yes src/Main.elm --output elm.js

node_modules:
	npm i

clean:
	rm -rf elm-stuff/build-artifacts
	rm -f elm.js
	rm -f site.css

distclean: clean
	rm -rf elm-stuff
