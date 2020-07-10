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

## CRAN Comment Response (SH, 2020-07-10)
> Thanks, please omit the redundant "in the R language" in your 
> Description text and elaborate what statistical methods your provide in 
> this package.

I've updated DESCRIPTION to address this.

> If there are references describing the (theoretical background of) 
> methods in your package, please add these in the Description field of 
> your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

I've updated DESCRIPTION to reference a handbook commonly used in this field
(this handbook is written by an organization and doesn't list authors or an
editor, so I hope I've referenced it using the correct format). I've also
referenced a JOSS paper about this pacakge.

> Please replace cat() by message() or warning() in your functions (except 
> for print() and summary() functions). Messages and warnings can be 
> suppressed if needed.

There were previouslu three non-exported functions that called cat (apart from
the print methods), which were only called by the various `print` methods.
I've changed the code so that these non-exported functions return a character
vector and the `print` method itself calls `cat`. After this change, the only
calls to `cat` are from `print` methods.

