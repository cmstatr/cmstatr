This re-submission does not change functionality. It updates a vignette and the
README, as well as making some minor changes to the code to address lintr
failures.

## Test environments
- win-builder (`devel`, `release`).
- local Ubuntu 20.04, R 4.3.1
- GitHub Action runners:
  - Windows, R `release`
  - MacOS, R `release`
  - Ubuntu latest, R `devel`
  - Ubuntu latest, R `release` (note that `vdiffr` had to be pinned to v1.0.5 to
    prevent the runner from exceeding the resource limitations while building
    `vdiffr`. This is not an issue with the `cmstatr` package.)
  - Ubuntu latest, R `oldrel`

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are no downstream dependencies.
