build: ## Build the project in fast mode
	@cabal build -O0

test: ## Run the test suite on filewatch mode
	@cabal test

db-reset: ## Reset the dev database
	@dropdb matchmaker_dev
	@createdb matchmaker_dev
	@cabal exec -- migrate init "$(PG_CONNSTRING)" migrations
	@cabal exec -- migrate migrate "$(PG_CONNSTRING)" migrations
	@psql "$(PG_URI)" < test/fixtures.sql

assets-build: ## Build the web assets
	@cd assets/ && yarn webpack --config webpack/webpack.config.js

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
