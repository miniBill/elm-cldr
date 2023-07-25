YARN_RUN = node_modules/.bin/elm-codegen

.PHONY:all
all: generated/Cldr/Territories.elm

build/cldr-json-modern.zip:
	mkdir -p build
	curl -L https://github.com/unicode-org/cldr-json/releases/download/43.1.0/cldr-43.1.0-json-modern.zip -o $@

$(YARN_RUN): package.json yarn.lock
	yarn && touch $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json $(YARN_RUN)
	yarn elm-codegen install

generated/Cldr/Territories.elm: codegen/Generate.elm codegen/elm.json codegen/Gen/Basics.elm $(YARN_RUN)
	rm -rf generated
	yarn elm-codegen run --flags-from build/cldr-localenames-modern/main
