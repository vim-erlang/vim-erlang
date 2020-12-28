.PHONY: default
default: test

.PHONY: help
help:
	@echo "See README.md."

.PHONY: test
test: vim-erlang-compiler/README.md \
      vim-erlang-omnicomplete/README.md \
      vim-erlang-tags/README.md
	test/run-tests.sh
	test/evaluate-test.sh

vim-erlang-compiler/README.md:
	git submodule update --init vim-erlang-compiler

vim-erlang-omnicomplete/README.md:
	git submodule update --init vim-erlang-omnicomplete

vim-erlang-tags/README.md:
	git submodule update --init vim-erlang-tags
