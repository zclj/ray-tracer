.PHONY: test lint

test:
	stack test --file-watch

lint:
	hlint . --report

lint-default:
	hlint . --default > .hlint.yaml
