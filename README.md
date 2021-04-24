# Matchmaker [![CI-badge][CI-badge]][CI-url] ![simple-haskell][simple-haskell]

<img src="./resources/matchmaker-frontpage.png">

## Description

*Matchmaker* is a project of the Haskell Foundation to help open-source maintainers and contributors find each-other,
and provide a smoother experience for people wishing to invest themselves in the opens-source Haskell ecosystem.

## Build

### Backend

```bash
$ make build
```
### Frontend

```bash
$ make assets-build
```

## Run

### Backend

```bash
$ cabal run matchmaker
```

### Frontend

```bash
$ make assets-watch
```

[simple-haskell]: https://img.shields.io/badge/Simple-Haskell-purple?style=flat-square
[CI-badge]: https://img.shields.io/github/workflow/status/haskellfoundation/matchmaker/CI?style=flat-square
[CI-url]: https://github.com/haskellfoundation/matchmaker/actions
