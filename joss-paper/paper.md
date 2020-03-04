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
$99\%$ or $90\%$ lower confidence bound of the material strength,
depending on the type of structure. These one-sided tolerance bounds are
referred to as A-Basis and B-Basis values, respectively. Computing these
A- and B-Basis values is the main problem that `cmstatr` addresses.

A set of statistical methods are described in a publication called the
Composites Materials Handbook, or CMH-17-1G [@CMH171G]. The use of these
methods is widely accepted by industry and civil aviation regulators.
The methods described in CMH-17-1G are implemented in `cmstatr`.

The MS Excel spreadsheets typically used, such as `STAT-17` [@STAT-17],
`ASAP` [@Raju_Tomblin_2008] and `CMH-17 STATS` \[INSERT CITATION\], use
password-protected `VBA` macros to perform the computations. As such,
the code cannot be audited by the user. `cmstatr` aims to address this
by providing open-source code for performing these computations.

# Implementation Goals

`cmstatr` aims give a consistent interface for the user. Most functions
are written to work with the `tidyverse` [@tidyverse] and most functions
have similar argument lists. The intent is to make the package easy to
learn and use.

The implementation of `cmstatr` also aims to avoid the use of lookup
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

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.4
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

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
[@Vangel_1994] are implemented. These functions perform the same
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

`cmstatr` provides functions for calculating basis values based on data
pooled across environments, as recommended by [@CMH171G]. These
functions use the variance observed in different environmental
conditions in the computation, but acknowledge the different mean values
under each environmental condition.

Another common statistical technique is to determine if a sample is
drawn from a particular population. This is often used to determine if
data from a second manufacturing site supports the same basis values.
The test often recommended for this is a test considering both the mean
and minimum individual value [@Vangel_2002]. This test has higher power
than some other tests that could be used. `cmstatr` also provides
functions for computing limits based on this test. For example:

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

# Comparison With Existing Tools

# Reproducibility

It is envisioned that many users of `cmstatr` will...

# References {#references .unnumbered}
