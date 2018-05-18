# Semi-automatic building/testing system for ImpFun on *nix
# - Vegard Itland

# ANSI escape codes for pretty output
RED   = \033[31;1m
GREEN = \033[32;1m
WHITE = \033[0m
BOLD  = \033[0;1m

# The cmp command given to bash in order to determine if test output is correct
# The cmp command compares <filename>.out with the pre-written output answers
# in <filename>.ans
CMP = 'cmp "$${1%.out}.ans" "$$1" > /dev/null || echo -e "$(RED)Fail:$(WHITE) $$1"'

all: build

# NOTE: only call once
init:
	cabal sandbox init && \
	cabal init && \
	cabal install parsec && \
	cabal install pretty

# Build the project
build:
	cabal build

# Create output from example files so that it can be compared to the correct
# output the files should produce
output: build
	@find examples -type f -name "*.impf" \
	  -exec sh -c './impf "$$1" > $${1%.impf}.out' _ {} \; # store in .out files

# Test that the output corresponds to the correct answers and delete output
# files
test: output
	@bash -c 'echo -e "$(BOLD)TESTING:$(WHITE)"' && \
	find examples -type f -name "*.out" \
	  -exec bash -c $(CMP) _ {} \; \
	  -exec rm {} \; \
	  -exec echo -en "$(GREEN)OK:$(WHITE) " \; \
	  -exec echo {} \;
