all: build

build:
	cabal build
	
output: build
	@find examples -type f -name "*.impf" -exec sh -c './impf "$$1" > $${1%.impf}.out' _ {} \;
	
test: output
	@echo "TESTING:" && \
	find examples -type f -name "*.out" -exec sh -c 'cmp "$${1%.out}.ans" "$$1" || echo "Fail: $$1"' _ {} \; -exec rm {} \; -exec echo -n "OK: " \; -exec echo {} \;
