# hopts - Sane Command Line Argument Parsing For Shell Scripts

Scrap your config files. Command Line Arguments are a universal interface. Unfortunately, the world of command-line argument parsing is horrendous: `getopts` is not portable, `getopt` doesn't provide double-dash/long-form options, and brute-forcing with `shift` is too mind-bending. Furthermore, dealing with quotations and whitespace in shell scripts (and other stringly-typed languages) is dangerous and anxiety-inducing.

There must be another way!

## hopts

What we need is a better way to describe the required command-line arguments for our applications. But how do we accomplish? Enter `hopts`!

    #!/bin/bash
    
    cmdLine() {
      local arg1='(arg-short a AWS_KEY "Your AWS key")'
      local arg2='(arg-long s aws-secret AWS_SECRET "Your AWS secret")'
      local arg3='(arg-switch d debug-mode DEBUG_MODE "Place the app in DEBUG mode")'
      
      local args="[ $arg1, $arg2, $arg3 ]"
     
      hopts args "$@"
    
    }

    cmdLine "$@"

    echo "$AWS_KEY"
    echo "$AWS_SECRET"
    echo "$DEBUG_MOD"

`hopts` works by compiling an [S-expression](http://en.wikipedia.org/wiki/S-expression)-based custom language to the wonderful [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) utility. `hopts` then takes your applications arguments and sets them to the environment variables you specified above.

whoop!
