# reservant

[![Build and release](https://github.com/junkidesu/reservant/actions/workflows/pipeline.yml/badge.svg)](https://github.com/junkidesu/reservant/actions/workflows/pipeline.yml)

Reservant is a utility CLI tool for web development in Haskell. When your Haskell web application is started with reservant, reservant will keep track of the source code of your project, and restart the application once a change is detected.

![Demonstration of Reservant](resources/demonstration.png)

This project was inspired by [Nodemon](https://nodemon.io/) for [Node.js](https://nodejs.org/en).

## Libraries Used

- [fsnotify](https://hackage-content.haskell.org/package/fsnotify-0.4.4.0) (File watcher)
- [rainbow](https://hackage.haskell.org/package/rainbow) (Colored console output)
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) (CLI)

## Installation

### Ready Executables

You can download the compiled executables from the [latest release page](https://github.com/junkidesu/reservant/releases/latest). At this moment, only the executables for Ubuntu and MacOS are available.

Having downloaded the build file, make it executable on the local machine:

```sh
$ chmod u+x reservant-ubuntu
```

### Build Locally

First and foremost, clone the repository on your local machine:

```sh
$ git clone https://github.com/junkidesu/reservant
```

To build the tool locally, ensure that the following are installed:

- [Stack](https://docs.haskellstack.org/en/stable/)
- [Cabal](https://cabal.readthedocs.io/en/stable/)

Stack and Cabal can be installed either independently or with the [GHCup](https://www.haskell.org/ghcup/) tool.

At the root of the repository, run

```sh
$ stack install reservant
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

### Example

To help with local tests of reservant, there is an example minimal Web application. To build and start the example application, perform the following steps:

```sh
$ stack build reservant-example 
$ cd reservant-example
$ reservant app/Main.hs
```

You may find more details here [here](reservant-example/).

### Usage Limitations

At this moment, the file watcher only keeps track of the `src` directory of the Haskell project.
