# GHC Bome Acceptance Tests

Provides acceptance tests for
[GHC Bome Service](http://github.com/aialferov/ghc-bome).

## Usage

The tests run against running GHC Bome Service, so you will need to provide
its host and port. Defaults are set to "localhost:8080".

Although running the tests from sources requires "make" and
[Erlang](https://www.erlang-solutions.com/resources/download.html), you
can use Docker to run it without any dependency required:

```
$ docker run --rm --net=host aialferov/ghc-bome-at
```

To run the test from source directory:

```
$ [HOST=<host>] [PORT=<port>] make at
```

Besides the console output results could be seen in a browser:

```
$ make at-display
```

If the last command did not work on your machine, you can open browser manually
against the "_build/test/logs/index.html" file.
