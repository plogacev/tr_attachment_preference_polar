
##################
### Fit models ###
##################

fit_tr_fixef <- brm(formula_fixef,
                    prior = priors, data = question_responses_n_tr, 
                    stanvars = model_functions,
                    save_pars = save_pars(all = TRUE),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = sprintf("../workspace/models/%s_fit_fixef_tr", model_id)
                    )

fit_tr_ranef <- brm(formula_ranef,
                    prior = priors, data = question_responses_n_tr, 
                    stanvars = model_functions,
                    save_pars = save_pars(all = TRUE),
                    iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = sprintf("../workspace/models/%s_fit_ranef_tr", model_id)
                    )

fit_en_fixef <- brm(formula_fixef,
                    prior = priors, data = question_responses_n_en, 
                    stanvars = model_functions,
                    save_pars = save_pars(all = TRUE),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = sprintf("../workspace/models/%s_fit_fixef_en", model_id)
                    )

fit_en_ranef <- brm(formula_ranef,
                    prior = priors, data = question_responses_n_en, 
                    stanvars = model_functions,
                    save_pars = save_pars(all = TRUE),
                    iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 1234, init_r = .1,
                    file = sprintf("../workspace/models/%s_fit_ranef_en", model_id)
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



