.PHONY: default
default: test

.PHONY: help
help:
	@echo "See README.md."

.PHONY: test
test: vim-erlang-tags/README.md
	test/run-tests.sh
	test/evaluate-test.sh

vim-erlang-tags/README.md:
	git submodule update --init vim-erlang-tags
