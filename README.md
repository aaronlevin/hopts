# hopts - Command-Line Arguments For Shell Scripts

Scrap your config files. Command Line Arguments are a universal interface. Unfortunately, the world of command-line argument parsing is horrendous: `getopt` isn't portable, `getopts` doesn't provide double-dash/long-form options, and brute-forcing with `shift` is too mind-bending. Furthermore, dealing with quotations and whitespace in shell scripts (and other stringly-typed languages) is dangerous and anxiety-inducing.

There must be another way!

## hopts

What we need is a better way to describe the required command-line arguments for our applications. But how do we accomplish? Enter `hopts`!

    ☭ cat app.sh
    #!/bin/bash
    
    cmdLineIO() {
      local arg1='(arg-short a AWS_KEY "Your AWS key")'
      local arg2='(arg-long s aws-secret AWS_SECRET "Your AWS secret")'
      local arg3='(arg-switch d debug-mode DEBUG_MODE "Place the app in DEBUG mode")'
      
      local args="[ $arg1, $arg2, $arg3 ]"
     
      hopts "$args" "$@"
    }

    cmdLineIO "$@"

    echo "$AWS_KEY"
    echo "$AWS_SECRET"
    echo "$DEBUG_MOD"

    ☭ ./app.sh -a "my-aws-key" --aws-secret "my-aws-secret" --debug-mode
    my-aws-key
    my-aws-secret
    true


`hopts` works by compiling an [S-expression](http://en.wikipedia.org/wiki/S-expression)-based custom language to the wonderful [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) utility. `hopts` then takes your applications arguments and sets them to the environment variables you specified above.

whoop!

## A Manifesto For Command Line Arguments

Config files are wreckless. The moment you specify your application's config in a file, a number of dangerous things happen:

- you need to parse the config file, which may cause failure, especially if you're using an obscure or custom configuration file format.
- your application now has a dependency on the location of your configuration within the file-system (or JAR-esque equivalent).
- the entry point to your application is now obscured, as fancy Dependency Injection frameworks or imperative, side-effecty languages allow you to configure your components *anywhere* in your application.
- in addition to the above, you now have an unrestricted set of config-file-effects and failures all over your codebase. Without discipline and documentation.
- packaging and running your app in a dynamic or multi-tenant environment requires you to *generate* your config files at runtime (youch!).

Meanwhile, we've been using a universal interface for configuration all along: command line arguments!
