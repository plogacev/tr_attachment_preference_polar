
library(plyr)
library(dplyr)
library(magrittr)
library(brms)
library(ggplot2)


fit_ahg_en <- readRDS("../workspace/models/ahg_fit_ranef_en.rds")
fit_r1r2hg_en <- readRDS("../workspace/models/r1r2hg_fit_ranef_en.rds")

fit_ahg_tr <- readRDS("../workspace/models/ahg_fit_ranef_tr.rds")
fit_r1r2hg_tr <- readRDS("../workspace/models/r1r2hg_fit_ranef_tr.rds")

brms::expose_functions(fit_ahg_tr, vectorize = T)

loo_ahg_r1r2hg_en <- loo(fit_ahg_en, fit_r1r2hg_en, moment_match = TRUE)
loo_ahg_r1r2hg_tr <- loo(fit_ahg_tr, fit_r1r2hg_tr, moment_match = TRUE)

#waic_ahg_r1r2hg_en <- waic(fit_ahg_en, fit_r1r2hg_en)
#waic_ahg_r1r2hg_tr <- waic(fit_ahg_tr, fit_r1r2hg_tr)

save(loo_ahg_r1r2hg_en, loo_ahg_r1r2hg_tr, file = "../workspace/model_comparisons_loo.rda")

