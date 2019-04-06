# mbta
[![CircleCI](https://circleci.com/gh/goedelsoup/mbta/tree/master.svg?style=svg)](https://circleci.com/gh/goedelsoup/mbta/tree/master)

### Getting Started

The project has an embedded sbt installer which will properly configure SBT for you if you do not yet have it installed. So to get started, simply run:

```bash
./sbt
```

You will now be in an SBT shell.

To compile the program, run:
```sbtshell
test:compile
```

To run the program, run:
```sbtshell
run
```

To run the tests, run:
```sbtshell
test
```

To produce a Docker image of the program, run:
```sbtshell
docker:publishLocal
```
