
library(plyr)
library(dplyr)
library(magrittr)
library(brms)
library(ggplot2)



model_id <- "r1r2hg"

path_responses <- "../workspace/data/question_responses_tr_en.rda"
path_posterior_predictive <- sprintf("../workspace/models/%s_posterior_predictive.rds", model_id)


load(path_responses)


read_file <- function(fname) {
  readChar(fname, nchars = file.size(fname))
}

# model formula
# adj: r0*p_n1 + (1-r0)*g
# n1: r1*p_n1 + (1-r1)*g
# n2: r2*(1-p_n1) + (1-r2)*g
# amb: h * (r1*p_n1 + (1-r1)*g) +  (1-h) * (r1*p_n1 + (1-r1)*g)
# justification for the rs: (i) inattentiveness, (ii) parsing failure, (iii) memory 
# justification for inattentiveness: error rate on adjective-sentences
# justifications for r1 > r2: (i)  / (ii)

model_functions <- read_file("../stan_functions/functions.stan") %>% 
  stanvar(block = "functions", scode = .)

formula <- bf( n_yes | trials(n) ~  logodds_yes_model(condition_id, probe_n1, inv_logit(r1), inv_logit(r2), inv_logit(g), inv_logit(h) ),
               family = binomial(link = "logit"), nl = TRUE)

formula_fixef <- formula + lf(g ~ 1) + 
  lf(h ~ 1) + 
  lf(r1 ~ 1) + 
  lf(r2 ~ 1)

formula_ranef <- formula + lf(g ~ 1 + (1|x|subject)) + 
  lf(h ~ 1 + (1|x|subject)) + 
  lf(r1 ~ 1 + (1|x|subject)) + 
  lf(r2 ~ 1 + (1|x|subject))

priors <- c( set_prior(nlpar = "r1", prior = "normal(1, 1.5)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "r2", prior = "normal(1, 1.5)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "g", prior = "normal(0, 1.5)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "h", prior = "normal(0, 1.5)", class = "b", coef = "Intercept")
             )



### Fit models and generate predictions ###
source("./fit_models.r")

