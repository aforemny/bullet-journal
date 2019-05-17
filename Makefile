PREFIX = _site


install: build
	mkdir -p $(PREFIX)
	cp src/index.html $(PREFIX)/index.html
	cp node_modules/material-design-icons/iconfont/MaterialIcons-Regular.* $(PREFIX)/
	mkdir -p $(PREFIX)/fonts
	cp -r node_modules/roboto-fontface/fonts/roboto $(PREFIX)/fonts/
	rsync -r imgs $(PREFIX)
	cp src/elm.js $(PREFIX)/elm.js
	cp src/index.css $(PREFIX)/index.css

build: src/elm.js src/index.css


src/elm.js: src/*.elm src/**/*.elm
	elm make src/Main.elm --output src/elm.js


src/index.css: src/*.scss node_modules
	sassc -I node_modules src/index.scss | ./node_modules/.bin/postcss --use autoprefixer > src/index.css


node_modules:
	npm i


clean:
	rm -rf elm-stuff/build-artifacts
	rm -rf $(PREFIX)
	rm -f src/index.css src/elm.js


distclean: clean
	rm -rf elm-stuff
	rm -rf node_modules
