
## Resubmission

This is a resubmission. The only changes are fixes to URLs in the documentation
that now use https instead of https or redirect to a new URL.

Local CRAN checks pass (Windows 10, R 3.6.3:)

```
> checking for future file timestamps ... NOTE
  unable to verify current time

0 errors v | 0 warnings v | 1 note x

R CMD check succeeded
```

## Submission

This sumbmission fixes failing CRAN checks caused by changes in a dependency
(stringi). 

All checks pass.

```
0 errors v | 0 warnings v | 1 note x

> checking for future file timestamps ... NOTE
unable to verify current time

R CMD check succeeded.
```

## Test environments

* Windows R Release (via R-Hub) and R 3.6.3 (local Windows 10)
* Linux R Release (via Travis) and R Devel (via R-Hub)


## Downstream dependencies

There are currently no downstream dependencies for this package.
