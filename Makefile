BUN_RUN = node_modules/.bin/elm-codegen

.PHONY:all
all: src/Cldr.elm

build/cldr-json-modern.zip:
	mkdir -p build
	curl -L https://github.com/unicode-org/cldr-json/releases/download/48.0.0/cldr-48.0.0-json-full.zip -o $@

build/cldr-core/defaultContent.json: build/cldr-json-modern.zip
	(cd build; unzip cldr-json-modern.zip)
	touch $@

build/cldr-localenames-modern/main/defaultContent.json: build/cldr-core/defaultContent.json
	cp $^ $@

build/cldr-localenames-modern/main/likelySubtags.json: build/cldr-core/supplemental/likelySubtags.json
	cp $^ $@

$(BUN_RUN): package.json bun.lock
	bun install && touch $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json $(BUN_RUN)
	bunx elm-codegen install

src/Cldr.elm: codegen/Generate.elm codegen/elm.json codegen/Gen/Basics.elm $(BUN_RUN) build/cldr-localenames-modern/main/defaultContent.json build/cldr-localenames-modern/main/likelySubtags.json
	rm -rf src/Cldr
	bunx elm-codegen run ${CODEGEN_FLAGS} --flags-from build/cldr-localenames-modern/main --output src
