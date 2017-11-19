# GHC Bome Acceptance Tests

Provides acceptance tests for
[GHC Bome Service](http://github.com/aialferov/ghc-bome).

## Usage

The tests run against running GHC Bome Service, so you will need to provide
its host and port. Defaults are set to "localhost:8080".

Although running the tests from sources requires "make" and
[Erlang](https://www.erlang-solutions.com/resources/download.html), you
can use Docker to run it without any dependency required.

Assuming you have run the GHC Bome Service as follows:

```
$ docker run --name ghc-bome --rm -it -p 8080:8080 aialferov/ghc-bome
```

then the tests against this instance could be run this way:

```
$ docker run --link ghc-bome --env HOST=ghc-bome --rm -it -p 8088:8088 aialferov/ghc-bome-at
```

Once tests complete the execution continues and serves a web page with test
run details. A link to the page is printed out and you have an option either
give it another test run or exit the execution and lose test run details.

### Sources

To run the tests from the source directory:

```
$ [HOST=<host>] [PORT=<port>] make at
```

Besides the console output results could be seen in a browser:

```
$ make at-display
```

If the last command did not work on your machine, you can open browser manually
against the "_build/logs/index.html" file.

### Package

The test suites could be built into one binary, shipped to any machine with
Erlang installed and run there. To build the binary:

```
$ make
```

The binary is located in "_build/default/bin".

Also the binary could be installed into a current machine:

```
$ make install
```

And uninstalled:

```
$ make uninstall
```

### Docker image

Makefile also provides targets to build, push and run test suites binary based
docker image. 

```
$ make docker-build # build image
$ make docker-push  # push image (you need to be logged in, see "docker login")
$ make docker-run   # run container with console attached
$ make docker-start # run container in background
$ make docker-stop  # stop container
$ make docker-clean # remove dangling (<none>:<none>) images
```
