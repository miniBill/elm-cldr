YARN_RUN = node_modules/.bin/elm-codegen

.PHONY:all
all: src/Cldr.elm

build/cldr-json-modern.zip:
	mkdir -p build
	curl -L https://github.com/unicode-org/cldr-json/releases/download/43.1.0/cldr-43.1.0-json-modern.zip -o $@

build/cldr-core/defaultContent.json: build/cldr-json-modern.zip
	(cd build; unzip cldr-json-modern.zip)
	touch $@

build/cldr-localenames-modern/main/defaultContent.json: build/cldr-core/defaultContent.json
	cp $^ $@

build/cldr-localenames-modern/main/likelySubtags.json: build/cldr-core/supplemental/likelySubtags.json
	cp $^ $@

$(YARN_RUN): package.json yarn.lock
	yarn && touch $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json $(YARN_RUN)
	yarn elm-codegen install

src/Cldr.elm: codegen/Generate.elm codegen/elm.json codegen/Gen/Basics.elm $(YARN_RUN) build/cldr-localenames-modern/main/defaultContent.json build/cldr-localenames-modern/main/likelySubtags.json
	rm -rf src/Cldr
	yarn elm-codegen run ${CODEGEN_FLAGS} --flags-from build/cldr-localenames-modern/main --output src
