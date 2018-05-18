all: build

build:
	cabal build
	
output: build
	@find examples -type f -name "*.impf" -exec sh -c './impf "$$1" > $${1%.impf}.out' _ {} \;
	
test: output
	@bash -c 'echo -e "\033[0;1mTESTING:\033[0m"' && \
	find examples -type f -name "*.out" -exec bash -c 'cmp "$${1%.out}.ans" "$$1" > /dev/null || echo -e "\033[31;1mFail:\033[0m $$1"' _ {} \; -exec rm {} \; -exec echo -en "\033[32;1mOK:\033[0m " \; -exec echo {} \;
