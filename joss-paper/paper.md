---
affiliations:
- index: 1
  name: 'Comtek Advanced Structures, Ltd.'
authors:
- affiliation: 1
  name: Stefan Kloppenborg
  orcid: '0000-0002-1908-5214'
bibliography: 'paper.bib'
date: '3/4/2020'
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
strength data for composite materials used in aerospace applications,
such as carbon fiber and fiberglass. Currently, many users use MS Excel
spreadsheets for performing this analysis. `cmstatr` is an R package
that implements the statistical analysis techniques commonly used for
composite material strength data.

The design standards for civil aviation require that the probability of
structural failure due to material variability is minimized. To do so,
the designer must select Design Values for each material and compare
those to the stresses experienced by those materials. These Design
Values are selected so that, with $95\%$ confidence, the Design Value is
either the $99\%$ or $90\%$ lower confidence bound of the material
strength, depending on the type of structure. These one-sided tolerance
bounds are referred to as A-Basis and B-Basis values, respectively.
Computing these A- and B-Basis values is the main problem that `cmstatr`
addresses.

A set of statistical methods are described in a publication called the
Composites Materials Handbook, or CMH-17-1G [@CMH171G]. The use of these
methods is widely accepted by industry and civil aviation regulators.
The methods described in CMH-17-1G are implemented in `cmstatr`.

The MS Excel spreadsheets typically used, such as `STAT-17` [@STAT-17],
`ASAP` [@Raju_Tomblin_2008] and `CMH17-STATS` [@CMH17-STATS], use
password-protected `VBA` macros to perform the computations. As such,
the code cannot be audited by the user. `cmstatr` aims to address this
by providing open-source code for performing these computations.

# Statement of Need

The purpose of `cmstatr` is to:

-   Provide a consistent user interface for computing A- and B-Basis
    values and performing the related diagnostic tests in the `R`
    programming environment
-   Allow auditing of the code used to compute A- and B-Basis values
-   Enable users to automate computation workflows or to perform
    simulation studies

# Implementation Goals

`cmstatr` aims give a consistent interface for the user. Most functions
are written to work with the `tidyverse` [@tidyverse] and most functions
have similar argument lists. The intent is to make the package easy to
learn and use.

The implementation of `cmstatr` also aims to avoid the use of look-up
tables and minimize the use of approximations. While this decision leads
to increased computation time, the typically small data sets (tens to
hundreds of observations) associated with composite material test data,
and the speed of modern computers make this practical for interactive
programming.

# Example Usage

Normally, to use `cmstatr` the user will load `cmstatr` itself as well
as the `tidyverse` package.

``` {.r}
library(cmstatr)
library(tidyverse)
```

`cmstatr` contains some example data sets, which can be used to
demonstrate the features of the package. One of those data sets ---
`carbon.fabric.2` --- will be used in the following example. This data
set contains results from several mechanical tests of a typical
composite material. and contains the typical measurements obtained from
a test lab. In the following examples, results from the "warp tension"
(`WT`) test will be used. Part of this data set is shown below.

``` {.r}
carbon.fabric.2 %>%
  filter(test == "WT") %>%
  head(10)
```

    ##    test condition batch thickness nplies strength modulus failure_mode
    ## 1    WT       CTD     A     0.112     14  142.817   9.285          LAT
    ## 2    WT       CTD     A     0.113     14  135.901   9.133          LAT
    ## 3    WT       CTD     A     0.113     14  132.511   9.253          LAT
    ## 4    WT       CTD     A     0.112     14  135.586   9.150          LAB
    ## 5    WT       CTD     A     0.113     14  125.145   9.270          LAB
    ## 6    WT       CTD     A     0.113     14  135.203   9.189          LGM
    ## 7    WT       CTD     A     0.113     14  128.547   9.088          LAB
    ## 8    WT       CTD     B     0.113     14  127.709   9.199          LGM
    ## 9    WT       CTD     B     0.113     14  127.074   9.058          LGM
    ## 10   WT       CTD     B     0.114     14  126.879   9.306          LGM

One common task is to calculate B-Basis values. Depending on the
distribution of the data, this can be done using one of several
functions. Assuming that the data from the warp tension (WT)
elevated-temperature wet (ETW) strength follows a normal distribution,
this can be done as follows:

``` {.r}
carbon.fabric.2 %>%
  filter(test == "WT") %>%
  filter(condition == "ETW") %>%
  basis_normal(strength, batch)
```

    ## Warning: `anderson_darling_normal` failed: Anderson-Darling test rejects
    ## hypothesis that data is drawn from a normal distribution

    ## 
    ## Call:
    ## basis_normal(data = ., x = strength, batch = batch)
    ## 
    ## Distribution:  Normal    ( n =  18 )
    ## The following diagnostic tests failed: 
    ##     `anderson_darling_normal`
    ## B-Basis:   ( p =  0.9 , conf =  0.95 )
    ## 122.9315

All of the various basis functions perform diagnostic tests. If any of
the diagnostic tests fail, a warning is emitted and the test failure is
also recorded in the returned object (and shown in that object's `print`
method). In the example above, the output shows that the
Anderson-Darling test for normality [@Lawless_1982] rejects the
hypothesis that the data is drawn from a normal distribution. The
single-point basis functions perform the following tests: the maximum
normed residual test for outliers within a batch [@CMH171G], the
Anderson-Darling k-Sample test to check if batches are drawn from the
same (unspecified) distribution [@Scholz_Stephens_1987], the maximum
normed residual test for outliers within the data, and the
Anderson-Darling test for a particular distribution [@Lawless_1982].

Two non-parametric basis calculations, based on [@Guenther_1969] and
[@Vangel_1994] are also implemented. These functions perform the same
diagnostic tests, but skip the Anderson-Darling test for a particular
distribution.

The diagnostic test can be run directly using `cmstatr` as well. For
example, the failed diagnostic test above can be run as follows:

``` {.r}
carbon.fabric.2 %>%
  filter(test == "WT") %>%
  filter(condition == "ETW") %>%
  anderson_darling_normal(strength)
```

    ## 
    ## Call:
    ## anderson_darling_normal(data = ., x = strength)
    ## 
    ## Distribution:  Normal ( n =  18 ) 
    ## Test statistic: A =  0.9381665 
    ## Significance:  0.01103075  (assuming unknown parameters)
    ## Conclusion: Sample is not drawn from a Normal distribution (alpha =  0.05 )

If it is decided that the failure of the diagnostic test is acceptable,
the test can be overridden to avoid a warning from being emitted by the
basis function:

``` {.r}
carbon.fabric.2 %>%
  filter(test == "WT") %>%
  filter(condition == "ETW") %>%
  basis_normal(strength, batch, override = c("anderson_darling_normal"))
```

    ## 
    ## Call:
    ## basis_normal(data = ., x = strength, batch = batch, override = c("anderson_darling_normal"))
    ## 
    ## Distribution:  Normal    ( n =  18 )
    ## The following diagnostic tests were overridden: 
    ##     `anderson_darling_normal`
    ## B-Basis:   ( p =  0.9 , conf =  0.95 )
    ## 122.9315

`cmstatr` provides functions for calculating basis values using data
pooled across environments, as recommended by [@CMH171G]. These
functions use the variance observed in different environmental
conditions in the computation, but acknowledge the different mean values
under each environmental condition.

Another common statistical technique is to determine if a sample is
drawn from a particular population. This is often used to determine if
data from a second manufacturing site supports the basis values
determined from test data generated at the first manufacturing source.
The statistical test often recommended for this application considers
both the mean and minimum individual value [@Vangel_2002]. This
statistical test has higher power than some other tests that could be
used. `cmstatr` also provides functions for computing limits based on
this test. For example:

``` {.r}
carbon.fabric.2 %>%
  filter(test == "WT") %>%
  filter(condition == "RTD")  %>%
  equiv_mean_extremum(strength, n_sample = 8, alpha = 0.05)
```

    ## 
    ## Call:
    ## equiv_mean_extremum(df_qual = ., data_qual = strength, n_sample = 8, 
    ##     alpha = 0.05)
    ## 
    ## For alpha = 0.05 and n = 8 
    ## ( k1 = 2.700045 and k2 = 0.6789966 )
    ##                    Min Individual      Sample Mean 
    ##      Thresholds:         121.4921         135.0655

# Validation and Comparison With Existing Tools

Where possible, `cmstatr` has been verified against the examples given
in the articles in which the statistical methods were published. Unit
tests have been written so that this verification is re-checked
routinely to prevent unintended regressions. Agreement between `cmstatr`
and the examples in the original articles is within the expected numeric
accuracy.

`cmstatr` has also been verified against existing software, such as
`STAT-17` [@STAT-17], `ASAP` [@Raju_Tomblin_2008] and `CMH17-STATS`
[@CMH17-STATS] using several example data sets. Agreement between
`cmstatr` and this other software is generally good, but some results
differ slightly, likely due to approximations used in the software.
Comparison between `cmstatr` and this other software is performed within
various unit tests to guard against future regressions.

The tests are automatically run each time a change is made to the code
of `cmstatr` using a continuous integration service. Additionally,
`CRAN` runs `R CMD check` on each package routinely.

# Reproducibility

It is envisioned that many users of `cmstatr` will use it within an R
Notebook or a Jupyter Notebook. It is further envisioned that this
notebook will be directly converted into the statistical analysis
report. If this is done, the reader of the statistical report will be
able to verify all of the detailed steps used in the statistical
analysis.

# References {#references .unnumbered}
