# break-time-hs

This is a rewrite of the core functionality of
[break-time](https://github.com/cdepillabout/break-time) in Haskell.

`break-time` is a Linux daemon that locks your screen and forces you to take
breaks while working at your computer.  I originally wrote `break-time` in Rust,
but it slowly grew more and more complicated.  The code is currently quite messy.
It really could use a big refactoring, or even a complete rewrite.

This repository is a proof-of-concept rewrite in Haskell.  Only the main logic
for deciding when to take a break is implemented.  All the plugins from the
original version are not implemented.

The code from this repository isn't meant to be used as-is, but may be a good
starting place for a rewrite or refactoring.

## Use

This can be built with the following command:

```console
$ stack build
```

`break-time-hs` provides a web server that you can use to start
a break-time thread, query the current status of the break-time thread, as
well as sending various commands.  You can run the web server with the
following command:

```console
$ stack run
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

Start a break-time thread.  This thread will count down for 15 seconds between
breaks, and then break for 10 seconds:

```console
$ curl -L -v -X POST localhost:3000/break-time
...
< HTTP/1.1 201 Created
...
< Location: /break-time/0
...
```

You can look at the status of the break-time thread:

```console
$ curl -L -X GET localhost:3000/break-time/0
BreakTimeEnv: (break every 15s seconds, breaks last for 10s seconds): BTSWork (CountDown {timeLeft = 4.991127679s, prevTarget = 2022-02-20 09:59:33.140554161 UTC, countDownEndsAt = 2022-02-2 0 09:59:38.140554161 UTC}), threadStatus: ThreadBlocked BlockedOnMVar
```

You can pause and unpause the break-time thread work timer.  Breaks won't occur
while it is paused:

```console
$ curl -L -v -X POST localhost:3000/break-time/0/pause
$ curl -L -v -X POST localhost:3000/break-time/0/unpause
```

You can reset the break-time work timer:

```console
$ curl -L -v -X POST localhost:3000/break-time/0/reset
```

You can delete the currently running break-time thread:

```console
$ curl -L -v -X DELETE localhost:3000/break-time/0
```

## Design

The original `break-time` has multiple threads running at once, and uses
[channels](https://doc.rust-lang.org/std/sync/mpsc/index.html) for communication
between threads.  This worked well when `break-time` was simple, but has
slowly grown out of control with all of the current functionality.

`break-time-hs` instead uses [STM](https://hackage.haskell.org/package/stm)
and [`TVar`s](https://hackage.haskell.org/package/stm-2.5.0.2/docs/Control-Concurrent-STM-TVar.html#t:TVar)
to communicate between multiple threads.  This seems to be much simpler,
and works well in practice.

The current implementation doesn't actually use the multiple-value mutation
transation functionality of STM, but does make use of the
[`retry`](https://hackage.haskell.org/package/stm-2.5.0.2/docs/Control-Monad-STM.html#v:retry)
function for blocking and waiting for changes (both directly and through use of
[`TMVar`s](https://hackage.haskell.org/package/stm-2.5.0.2/docs/Control-Concurrent-STM-TMVar.html)).
