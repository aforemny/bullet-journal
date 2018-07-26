build: node_modules
	mkdir -p public/fonts
	sassc -I node_modules src/main.scss > public/site.css
	elm-make --debug --yes src/Main.elm --output public/elm.js
	cp page.html public/index.html
	cp elm-mdc.js public/
	cp node_modules/normalize.css/normalize.css public/
	cp node_modules/material-design-icons/iconfont/material-icons.css public/
	cp node_modules/material-design-icons/iconfont/MaterialIcons-Regular.* public/
	cp node_modules/roboto-fontface/css/roboto/roboto-fontface.css public/
	cp -r node_modules/roboto-fontface/fonts/roboto public/fonts/

node_modules:
	npm i

clean:
	rm -rf elm-stuff/build-artifacts
	rm -rf public

distclean: clean
	rm -rf elm-stuff