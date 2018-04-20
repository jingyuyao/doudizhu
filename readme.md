# Rules
The rules implemented will largely follow the one described in: https://www.pagat.com/climbing/doudizhu.html

# Prerequisite
- Java 8 runtime
- sbt

# Run
From command line (arguments must be enclosed in quotes along with the `run` command)
```
sbt "run human smart dumb --debug --log"
```
First three required arguments specifies the name of the agents to use.
Optional arguments include:
- `--log` to save the text output to a file in the `logs/` directory. Note that this redirects all text output
so a human agent will not get any prompt from `stdout`. It is primary used to log bot only games.
- `--init0` can be used to force the game to start the landlord selection process at the first agent
specified in the arguments.
- `--debug` print out debug information.
- `--verbose` print out verbose debug information (use with `--debug`). This will print out a lot
of debug information so use sparingly.

# Analyze result
Here are some simple grep commands to get an overview of the games when logging is enabled:
- `grep hand logs/*` to see hand progressions
- `grep won logs/*` to see a list of winners
- `grep getSuccessor logs/*` to see Expectimax related information
