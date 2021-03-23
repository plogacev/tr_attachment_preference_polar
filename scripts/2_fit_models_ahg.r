
library(plyr)
library(dplyr)
library(magrittr)
library(brms)
library(ggplot2)


model_id <- "ahg"

path_responses <- "../workspace/data/question_responses_tr_en.rda"
path_posterior_predictive <- sprintf("../workspace/models/%s_posterior_predictive.rds", model_id)


(load(path_responses))


read_file <- function(fname) {
  readChar(fname, nchars = file.size(fname))
}

model_functions <- read_file("../stan_functions/functions.stan") %>% 
  stanvar(block = "functions", scode = .)


formula <- bf( n_yes | trials(n) ~  logodds_yes_model_ahg(condition_id, probe_n1, inv_logit(a), inv_logit(g), inv_logit(h) ),
               family = binomial(link = "logit"), nl = TRUE)

formula_fixef <- formula + lf(g ~ 1) + 
  lf(h ~ 1) + 
  lf(a ~ 1)

formula_ranef <- formula + lf(g ~ 1 + (1|x|subject)) + 
  lf(h ~ 1 + (1|x|subject)) + 
  lf(a ~ 1 + (1|x|subject))

priors <- c( set_prior(nlpar = "a", prior = "normal(1, 1.5)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "g", prior = "normal(0, 1.5)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "h", prior = "normal(0, 1.5)", class = "b", coef = "Intercept")
             )


### Fit models and generate predictions ###
source("./fit_models.r")
