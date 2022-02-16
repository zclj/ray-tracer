.PHONY: test lint

test:
	stack test --file-watch

lint:
	hlint . --report

lint-default:
	hlint . --default > .hlint.yaml

build-profiling:
	stack build --profile

profile:
	stack exec --profile -- ray-tracer-exe +RTS -p
