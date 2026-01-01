# reservant-example

This directory contains a minimal working example of a REST API with Servant and SQLite, which you can use to test the usage of `reservant`.

All the API definitions and endpoint handlers are in [src/Web.hs](src/Web.hs).

The Web application entry point is in [app/Main.hs](app/Main.hs), which will be used as input to `reservant`.

The folder [requests](requests/) has ready-made HTTP request files to interact with the API.

## Build and Start

If you haven't already, clone the repository to your machine:

```sh
$ git clone https://github.com/junkidesu/reservant.git
```

Install reservant on your local machines as per the [instructions](../README.md#build-locally).

At the root of the repository, run the following:

```sh
$ stack build reservant-example
```

To start the project with `reservant`, navigate to the root directory of the example, and run the provided command:

```sh
$ cd reservant-example
$ reservant app/Main.hs
```

Thus, reservant will start watching the contents of the directory [src](src/). Try making some changes in [src/Web.hs](src/Web.hs) and saving the file. Have fun with `reservant` :)
