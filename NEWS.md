# Version x.x.x
- Fixed bug in `glance.equiv_mean_extremum` where it would include empty
  values when a sample was not specified.

# Version 0.7.0
- Added optional argument to `glance.basis` to add diagnostic test results
  to resulting `data.frame`

# Version 0.6.0
- Improved the documentation for several functions
- Made minor formatting changes to the `print` methods for:
  - `ad_ksample`
  - `anderson_darling`
  - `basis`
  - `equiv_mean_extremum`
  - `equiv_chage_mean`
  - `levene_test`
  - `maximum_normed_residual`
- Added `alpha` into the `mnr` object, and updated `print` and `glance`
  methods to show the value of `alpha` specified by the user

# Version 0.5.2
- Internally use `vapply` instead of `sapply` to improve code safety
- Increased coverage of unit tests

# Version 0.5.1
- Fixed the title of the graphing vignette

# Version 0.5.0
- Renamed `transform_mod_cv_2` to `transform_mod_cv_ad` to better describe
  the purpose of this function.
- Removed the optional argument from `transform_mod_cv`. Now if several
  groups are to be transformed separately, this needs to be done explicitly
  using `dplyr::group_by` or a similar strategy.
- Fixed bug related to the automated diagnostic tests of pooled basis methods
  when `modcv = TRUE`. Previously, the diagnostic tests were performed with
  the unmodified data. After this bug fix, the the data after the modified
  CV transform is used for the diagnostic tests.
- Added `stat` extensions to `ggplot2`:
  - `stat_normal_surv_func` to plot a normal survival function based on
    the data given
  - `stat_esf` to plot an empirical survival function
- Updated cmstatr_Tutorial vignette
- Created cmstatr_Graphing vignette
- Various documentation improvements

# Version 0.4.0
- Added automated diagnostic tests to basis_... methods
- Updated argument names for functions:
  - `transform_mod_cv`
  - `transform_mod_cv_2`
  - `normalize_group_mean`
- Updated cmstatr_Tutorial vignette


# Version 0.3.0
- Added modified CV functionality
- Added glance and augment methods for most objects
- Added function for calculating CV of a sample
- Breaking changes:
  - Renamed function `basis_nonparametric_large_sample` to
    `basis_nonpara_large_sample`
  - Renamed function `nonparametric_binomial_rank` to
    `nonpara_binomial_rank`

# Version 0.2.0
- Added ANOVA basis calculation
- Added non-parametric basis calculations

# Version 0.1.0
- Initial release
