start: ## Start the server
	@cabal run matchmaker

deps: ## Install the dependencies of the backend
	@command -v migrate >/dev/null || cabal install postgresql-simple-migration
	@cabal build --only-dependencies

build: ## Build the project in fast mode
	@cabal build -O0

clean: ## Remove compilation artifacts
	@cabal clean

assets-deps: ## Install the dependencies of the frontend
	@cd assets/ && yarn

assets-build: ## Build the web assets
	@cd assets/ && yarn webpack --config webpack/webpack.config.js

assets-watch: ## Continuously rebuild the web assets
	@cd assets/ && yarn webpack -w --config webpack/webpack.config.js

assets-clean: ## Remove JS artifacts
	@cd assets/ && rm -R node-modules

db-init: ## Initialize the dev database
	@initdb -D db

db-start: ## Start the dev database
	@postgres -D db

db-setup: ## Setup the dev database
	@createdb matchmaker_dev
	@cabal exec -- migrate init "$(echo $PG_CONNSTRING)" migrations
	@cabal exec -- migrate migrate "$(echo $PG_CONNSTRING)" migrations

db-reset: ## Reset the dev database
	@dropdb matchmaker_dev
	@make db-setup

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

lint: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | parallel -j $(PROCS) -- hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (stylish-haskell)
	@stylish-haskell -i -r src app test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
