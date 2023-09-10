# Version 0.9.2
- Update to Anderson Darling k-Sample vignette to explain differences with
  SciPy implementation
- Updated README
- Update `plot_nested` to use `linewidth` instead of `size` internally due
  to update to `ggplot2`

# Version 0.9.1
- Updated tests to accommodate upcoming changes to the rlang package.
  No change to test coverage was made.

# Version 0.9.0
- Added the vignette `cmstatr_Validation`
- Updated the expected value of the order statistic of a normally
  distributed variable in the implementation of `hk_ext_z_j_opt`.
  This affects the Basis values computed by `basis_hk_ext` when
  `method="optimum-order"`. Both the new and old implementations appear to
  perform equally well. See the vignette `hk_ext` for more information.
- Added the function `nested_data_plot` for producing nested data plots.
- Added the vignette `hk_ext`
- Updated the vignette `cmstatr_Graphing` to show some examples of the use
  of `nested_data_plot`.
- Added the additional column `batch` to the `carbon.data.2` example data set.
- In `k_factor_normal`, suppress warnings emitted by `qt` when the non-central
  parameter is large.
- Updated the test to use `testthat` edition 3.

# Version 0.8.0
- Updated `basis_anova` so that in cases where the between-batch variance
  is small compared with the within-batch variance, a tolerance factor
  that doesn't consider the structure of the data is used. This matches the
  recommendation of Vangel (1992).
- Added the alias `override="all"` to allow overriding all applicable
  diagnostic tests that are automatically run by the `basis_...` functions.
- Improved documentation of diagnostic tests
- Added `na.rm` argument to `cv` with identical behavior to the `na.rm`
  argument of `mean` and `sd`.
- Fixed bug causing `maximum_normed_residual` to fail with small data sets
  where all but two observations would be considered outliers.
- When diagnostic tests produce an error (when automatically run by the
  `basis_...` functions), the error message now identifies which test
  produced the error.

# Version 0.7.1
- Fixed bug in `glance.equiv_mean_extremum` where it would include empty
  values when a sample was not specified.
- Moved `dplyr` from Suggests to Depends. It is expected that nearly all
  users will use this package in their workflow, and a future version of
  `cmstatr` will also rely on functionality from `dplyr`.
- Changed tests and vignettes such that tests and vignette code
  is not re-run when the necessary packages are not available. Test coverage
  and re-building of vignettes is unchanged when all packages in Depends and
  Suggests are available.

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
