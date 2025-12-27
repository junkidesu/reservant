# reservant

Reservant is a utility CLI tool for web development in Haskell. When your Haskell web application is started with reservant, reservant will keep track of the source code of your project, and restart the application once a change is detected.

![Demonstration of Reservant](resources/demonstration.png)

This project was inspired by [Nodemon](https://nodemon.io/) for [Node.js](https://nodejs.org/en).

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
$ reservant --help
```

## Usage

In the root directory of your Haskell project, run `reservant` executable, providing the relative path of the module that has the "main" method.

Example:

```sh
$ reservant app/Main.hs
```

### Usage Limitations

At this moment, the file watcher only keeps track of the `src` directory of the Haskell project.
