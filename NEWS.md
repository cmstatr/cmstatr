# Version x.x.x
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
