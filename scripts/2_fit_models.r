
library(plyr)
library(dplyr)
library(magrittr)
library(brms)
library(ggplot2)



model_id <- "r1r2hg"

path_responses <- "../workspace/data/question_responses_tr_en.rda"
path_posterior_predictive <- sprintf("../workspace/models/posterior_predictive.rds", model_id)


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

priors <- c( set_prior(nlpar = "r1", prior = "normal(1, 1)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "r2", prior = "normal(1, 1)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "g", prior = "normal(0, 1)", class = "b", coef = "Intercept"),
             set_prior(nlpar = "h", prior = "normal(0, 1)", class = "b", coef = "Intercept")
)


##################
### Fit models ###
##################

fit_tr_fixef <- brm(formula_fixef,
                    prior = priors, data = resp_averages_n_tr, 
                    stanvars = model_functions,
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = "../workspace/models/fit_fixef_tr"
)

fit_tr_ranef <- brm(formula_ranef,
                    prior = priors, data = resp_averages_n_tr, 
                    stanvars = model_functions,
                    iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = "../workspace/models/fit_ranef_tr"
)


fit_en_fixef <- brm(formula_fixef,
                    prior = priors, data = question_responses_n_en, 
                    stanvars = model_functions,
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = "../workspace/models/fit_fixef_en"
)

fit_en_ranef <- brm(formula_ranef,
                    prior = priors, data = question_responses_n_en, 
                    stanvars = model_functions,
                    iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = "../workspace/models/fit_ranef_en"
)


############################
### Generate predictions ###
############################


brms::expose_functions(fit_en_ranef, vectorize = T)

aggregate_average_predictions <- function(predictions, indices) { 
  predictions[,indices] %>% apply(MARGIN = 1, mean) %>% quantile(., c(.025, .05, .1, .5, .9, .95, .975)) %>% t() %>% 
    as.data.frame() %T>% { colnames(.) <- c("lower95", "lower90", "lower80", "mid", "upper80", "upper90", "upper95") }
}

posterior_predictive_check <- function(responses, fit) {
  responses$index <- 1:nrow(responses)
  predictions <- posterior_predict(fit, responses)
  predictions <- t( t(predictions) / responses$n )
  
  responses_posterior_predictive <-
    responses %>% group_by(attachment, question_np) %>% 
        dplyr::do( cbind(aggregate_average_predictions(predictions, .$index), obs = mean(.$perc_yes) ) )
  
  responses_posterior_predictive
}

predictions_en <- 
posterior_predictive_check(question_responses_n_en, fit_en_ranef)

predictions_tr <-
posterior_predictive_check(question_responses_n_tr, fit_tr_ranef)


predictions_df <-
bind_rows(
  predictions_en %>% cbind(exp = "English (Swets et al., 2008)"),
  predictions_tr %>% cbind(exp = "Turkish")
)
predictions_df$attachment %<>% as.character()
predictions_df$attachment %<>% dplyr::recode("N2" = "N2 attachment", "N1" = "N1 attachment")
predictions_df$attachment %<>% as.factor()

saveRDS(predictions_df, file = path_posterior_predictive)

