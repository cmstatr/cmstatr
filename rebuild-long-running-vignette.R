# The idea of pre-computing long-running vignettes is from here:
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/

old_wd <- getwd()

setwd("vignettes/")
knitr::knit("hk_ext.Rmd.orig", output = "hk_ext.Rmd")
knitr::purl("hk_ext.Rmd.orig", output = "hk_ext.R")

setwd(old_wd)
