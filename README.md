# Matchmaker [![CI-badge][CI-badge]][CI-url] ![simple-haskell][simple-haskell]

<img src="./resources/matchmaker-frontpage.png">

## Description

*Matchmaker* is a project of the Haskell Foundation to help open-source maintainers and contributors find each-other,
and provide a smoother experience for people wishing to invest themselves in the opens-source Haskell ecosystem.

## Prerequisites

* PostgreSQL 12 or higher
* GHC 8.10.4
* Yarn 1.22 or higher

*Note*
There is a `shell.nix` file provided for convenience. However it is far from perfect.
It will not manage a local installation of PostgreSQL for you.
You should be able to work in a pure shell. If not, file a bug!
The nix shell should source all environment variables when you enter it.

The `Makefile` contains all the development-related scripts you'll need. Please
refer to the output of `make help` for more information.

## Running Matchmaker

### Backend

```bash
# Build `matchmaker` and its dependencies
$ make deps
$ make build

# Initialize database configuration if you haven't already
$ make db-init

# Start the database
$ make db-start

# Run migrations against the running database (in another terminal)
$ make db-setup

# Start `matchmaker`
$ make start
```

### Frontend

```bash
$ make assets-deps
$ make assets-build # or assets-watch if you're working on CSS/JS
```

[simple-haskell]: https://img.shields.io/badge/Simple-Haskell-purple?style=flat-square
[CI-badge]: https://img.shields.io/github/workflow/status/haskellfoundation/matchmaker/CI?style=flat-square
[CI-url]: https://github.com/haskellfoundation/matchmaker/actions
