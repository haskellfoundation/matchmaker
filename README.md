# Matchmaker ![CI-badge][CI-badge]][CI-url] ![simple-haskell][simple-haskell]

## Description

## Build

### Backend

```bash
$ cabal build -O2
```

### Frontend

Go in the `assets/` directory and run

```bash
$ yarn install
$ yarn build

```

## Run

### Backend

```bash
$ cabal run exe matchmaker-server
```

### Frontend

In a development setup, go in the `assets/` directory and run:

```
$ webpack -w --config webpack/webpack.config.js
```

[simple-haskell]: https://img.shields.io/badge/Simple-Haskell-purple?style=flat-square
[CI-badge]: https://img.shields.io/github/workflow/status/haskellfoundation/matchmaker/CI?style=flat-square
[CI-url]: https://github.com/haskellfoundation/matchmaker/actions
