assets-build: ## Build the web assets
	@cd assets/ && yarn webpack --config webpack/webpack.config.js

assets-watch: ## Continuously rebuild the web assets
	@cd assets/ && yarn webpack -w --config webpack/webpack.config.js

lint: ## Run the code linter (HLint)
	@hlint -g

style: ## Run the code styler (stylish-haskell)
	@stylish-haskell -i -r src app test

build: ## Build the project in fast mode
	@cabal build -O0

db-reset: ## Reset the dev database
	@dropdb matchmaker_dev
	@createdb matchmaker_dev
	@cabal exec -- migrate init "$(PG_CONNSTRING)" migrations
	@cabal exec -- migrate migrate "$(PG_CONNSTRING)" migrations
	@psql "$(PG_URI)" < test/fixtures.sql

test: ## Run the test suite
	@cabal test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
