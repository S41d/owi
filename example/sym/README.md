# Symbolic interpreter

## Basic example

The symbolic interpreter allows to define *symbolic variables* whose values is considered to be any possible value. Then, we collect informations during the execution and when an error is reached, we try to see if there's a possible value for all the symbolic variables by taking all our informations into account.

In the following file, we define `x` as a symbolic variable. Then if `5 < x`, we fail.

<!-- $MDX file=mini.wat -->
```wat
(module
  (import "symbolic" "i32_symbol" (func $gen_i32 (result i32)))

  (func $start (local $x i32)
    (local.set $x (call $gen_i32))
    (if (i32.lt_s (i32.const 5) (local.get $x)) (then
      unreachable
    )))

  (start $start)
)
```

Let's see if owi is able to find a value for `x` that lead to an error:

```sh
$ owi sym ./mini.wat
Trap: unreachable
model {
  symbol symbol_0 i32 6
}
Reached problem!
[13]
```

Indeed, if `x` is equal to `6` then, the `unreachable` instruction will be reached.

## Man page

```sh
$ owi sym --help=plain
NAME
       owi-sym - Run the symbolic interpreter

SYNOPSIS
       owi sym [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       -d, --debug
           debug mode

       --deterministic-result-order
           Guarantee a fixed deterministic order of found failures. This
           implies --no-stop-at-failure.

       --entry-point=FUNCTION
           entry point of the executable

       --fail-on-assertion-only
           ignore traps and only report assertion violations

       --fail-on-trap-only
           ignore assertion violations and only report traps

       --invoke-with-symbols
           Invoke the entry point of the program with symbolic values instead
           of dummy constants.

       --model-format=VAL (absent=scfg)
            The format of the model ("json" or "scfg")

       --model-out-file=FILE
           Output the generated model to FILE. if --no-stop-at-failure is
           given this is used as a prefix and the ouputed files would have
           PREFIX_%d.

       --no-assert-failure-expression-printing
           do not display the expression in the assert failure

       --no-stop-at-failure
           do not stop when a program failure is encountered

       --no-value
           do not display a value for each symbol

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

       --print-pc
           print path condition

       --profile=FILE
           Profile file.

       --rac
           runtime assertion checking mode

       -s VAL, --solver=VAL (absent=Z3)
           SMT solver to use

       --srac
           symbolic runtime assertion checking mode

       -u, --unsafe
           skip typechecking pass

       -w VAL, --workers=VAL (absent=n)
           number of workers for symbolic execution. Defaults to the number
           of physical cores.

       --with-breadcrumbs
           add breadcrumbs to the generated model

       --workspace=DIR
           write results and intermediate compilation artifacts to dir

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi sym exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
