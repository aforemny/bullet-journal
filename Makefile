PREFIX = _site


build: node_modules
	mkdir -p $(PREFIX)/fonts
	elm make src/Main.elm --output $(PREFIX)/elm.js
	cp page.html $(PREFIX)/index.html
	cp node_modules/normalize.css/normalize.css $(PREFIX)/
	cp node_modules/material-design-icons/iconfont/material-icons.css $(PREFIX)/
	cp node_modules/material-design-icons/iconfont/MaterialIcons-Regular.* $(PREFIX)/
	cp node_modules/roboto-fontface/css/roboto/roboto-fontface.css $(PREFIX)/
	cp -r node_modules/roboto-fontface/fonts/roboto $(PREFIX)/fonts/
	sassc -I node_modules src/main.scss > site.css
	./node_modules/.bin/postcss site.css --use autoprefixer > $(PREFIX)/site.css
	rsync -r imgs $(PREFIX)


node_modules:
	npm i


clean:
	rm -rf elm-stuff/build-artifacts
	rm -rf $(PREFIX)


distclean: clean
	rm -rf elm-stuff
	rm -rf node_modules
