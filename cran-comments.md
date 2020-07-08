## Test environments
- win-builder (devel and release)
- local Ubuntu 18.04 install, R 3.6.3
- GitHub Action runners:
  - Windows, R 4.0.2
  - MacOS, R 4.0.2
  - Ubuntu 16.04, R 4.0.2
  - Ubuntu 16.04, R 3.6.3

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE (only on win-builder):
> * checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Stefan Kloppenborg <stefan@kloppenborg.ca>'
>
> New submission

Since this is a new package, this note is expected.

## Downstream dependencies
This is a new package. There are no downstream dependencies.
