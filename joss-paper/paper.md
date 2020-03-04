---
affiliations:
- index: 1
  name: 'Comtek Advanced Structures, Ltd.'
authors:
- affiliation: 1
  name: Stefan Kloppenborg
  orcid: '0000-0002-1908-5214'
bibliography: 'paper.bib'
date: '1/29/2020'
output:
  md_document:
    pandoc_args: '--atx-headers'
    preserve_yaml: True
    variant: markdown
  pdf_document: default
tags:
- R
- statistics
- composite materials
- material science
title: |
    cmstatr: An R Package for Statistical Analysis of Composite Material
    Data
---

# Summary

A number of statistical techniques are commonly used when analyzing
strength datat for composite materials used in aerospace applications,
such as carbon fiber and fiberglass. Currently, many users use MS Excel
spreadsheets for performing this analysis. `cmstatr` is an R package
that implements the statistical analysis techniques commonly used for
composite material strength data.

The design standards for civil aviation require that the probability of
structural failure due to material variability is minimized. To do so,
the desginer must select Design Values for each material and compare
those to the stresses experienced by those materials. These Design
Values are selected so that, with $95\%$ confience, the Design Value is
$99\%$ or $90\%$ lower confidnece bound of the material strength,
depending on the type of structure. These one-sided tolerance bounds are
referred to as A-Basis and B-Basis values, respectively. Computing these
A- and B-Basis values is the main problem that `cmstatr` addresses.

A set of statistical methods are described in a publication called the
Composites Materials Handbook, or CMH-17-1G [@CMH171G]. The use of these
methods is widely accepted by industry and civil aviation regulators.
The methods described in CMH-17-1G are implemented in `cmstatr`.

The MS Excel spreadsheets typically used, such as `STAT-17`, `ASAP` and
`CMH-17 STATS`, use password-protected `VBA` macros to perform the
computations. As such, the code cannot be audited by the user. `cmstatr`
aims to address this by providing open-source code for performing these
computations.

The implementation of `cmstatr` aims to avoid the use of loopup tables
and minimize the use of approximations. While this decision leads to
increased computation time, the typically small datasets (tens to
hundreds of observations) associated with composite material test data,
and the speed of modern computers make this practical for interactive
programming.

# Example Use

# Comparison With Existing Tools

...excel tools...

...reproducibility...

...avoidance of lookup tables and approximations...

...example use...

...comparison with existing tools...

# References {#references .unnumbered}
