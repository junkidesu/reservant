# reservant-exe

Nodemon clone for your Haskell web applications. Use reservant to automatically re-build and restart your Haskell application as you make changes to your code.

## Libraries Used

- [fsnotify](https://hackage-content.haskell.org/package/fsnotify-0.4.4.0) (File watcher)
- [rainbow](https://hackage.haskell.org/package/rainbow) (Colored console output)
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) (CLI)

## Getting Started

First and foremost, clone the repository on your local machine:

```sh
$ git clone https://github.com/junkidesu/reservant
```

### Prerequisites

To build the tool locally, ensure that the following are installed:

- [Stack](https://docs.haskellstack.org/en/stable/)
- [Cabal](https://cabal.readthedocs.io/en/stable/)

Stack and Cabal can be installed either independently or with the [GHCup](https://www.haskell.org/ghcup/) tool.

### Installation

At the root of the repository, run

```sh
$ stack install
```

To ensure that the tool is installed, run

```sh
$ reservant-exe --help
```

## Usage

In the root directory of your Haskell project, run `reservant-exe` executable, providing the relative path of the module that has the "main" method.

Example:

```sh
$ reservant-exe app/Main.hs
```
