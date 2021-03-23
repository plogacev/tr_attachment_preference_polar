
library(plyr)
library(dplyr)
library(magrittr)
library(brms)
library(ggplot2)
library(scales)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue(4)

se_cousineau <- function(df, n_conditions, subject, DV, group, n_multiplier, is_proportion = NULL)
{
  stopifnot(!"avgDV" %in% colnames(df))
  subject_var <- substitute(subject) %>% deparse() %>% gsub("\"", "", .)
  DV <- substitute(DV) %>% deparse() %>% gsub("\"", "", .)
  stopifnot( subject_var %in% colnames(df) && DV %in% colnames(df) )
  
  subj_means <- df %>% group_by(.dots = subject_var) %>% 
    dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T), 
                     .groups = "drop")
  GM <- mean(subj_means$avgDV)
  df %<>% group_by(.dots = subject_var) %>% 
    dplyr::mutate(nDV = !!as.name(DV) - mean(!!as.name(DV), na.rm = T) + GM) %>%
    ungroup()
  
  if (is.null(is_proportion)) {
    dv <- df[[DV]]
    dv_unique <- unique(dv)
    if ( is.logical(dv) || (length(dv_unique) == 2 && all(dv_unique %in% c(0,1))) ) {
      is_proportion <- TRUE
    } else {
      is_proportion <- FALSE
    }
  }
  
  var_correction_factor <- n_conditions/(n_conditions-1)
  df %>% group_by(.dots = group) %>%
    dplyr::summarize(M = mean(nDV, na.rm = T),
                     Var = ifelse(is_proportion, M*(1-M), var(nDV, na.rm = T)) * var_correction_factor,
                     N = sum(!is.na(nDV)),
                     SE = sqrt(Var/ (N*n_multiplier) ), 
                     .groups = "drop" )
}





####################
### raw averages ###
####################

(load("../workspace/data/question_responses_tr_en.rda"))

perc_cis_en <-
  question_responses_n_en %>% se_cousineau(n_conditions=6, subject=subject, DV="perc_yes", group=c("attachment", "question_np"), n_multiplier = 6, is_proportion = T)

perc_cis_tr <-
  question_responses_n_tr %>% se_cousineau(n_conditions=6, subject=subject, DV="perc_yes", group=c("attachment", "question_np"), n_multiplier = 7, is_proportion = T)

perc_cis_en$experiment <- "English (Swets et al., 2008)"
perc_cis_tr$experiment <- "Turkish"
perc_cis <- bind_rows(perc_cis_en, perc_cis_tr)
perc_cis$attachment %<>% dplyr::recode("N1"="N1 attachment", "N2"="N2 attachment")
perc_cis$question_np %<>% dplyr::recode("NP1"="N1 question", "NP2"="N2 question")

p_raw <-
perc_cis %>% ggplot(aes(question_np, M, color = attachment, group = attachment)) +
  #geom_point() +
  geom_point(aes(y=M), shape = "square", size = 2) + geom_line() +
  geom_errorbar(aes(ymin=M-1.96*SE, ymax=M+1.96*SE), width = 0.1) +
  facet_wrap(~experiment) + scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + 
  theme_bw() +
  theme(legend.position="top") + scale_color_discrete(name = NULL) +
  xlab(NULL) + ylab("Percentage of 'yes' responses")

z = 1.5
ggsave(p_raw, file = "../figures/avg_perc.pdf", width = 3*z, height = 2*z)



##############################
### N1 preference averages ###
##############################

question_responses_n_en$perc_n1 <- with(question_responses_n_en, ifelse(question_np == "NP1", perc_yes, 1-perc_yes))
question_responses_n_tr$perc_n1 <- with(question_responses_n_tr, ifelse(question_np == "NP1", perc_yes, 1-perc_yes))

#perc_n1_cis_en <-
  question_responses_n_en %>% group_by(attachment) %>%
  dplyr::summarise(
    M = mean(perc_n1),
    SE = sqrt(M*(1-M)/ (length(unique(subject))*12)  )
  )

#perc_n1_cis_tr <-
  question_responses_n_tr %>% group_by(attachment) %>%
  dplyr::summarise(
    M = mean(perc_n1),
    SE = sqrt(M*(1-M)/  length(unique(subject))*14 )
  )


#########################
### Accuracy averages ###
#########################

question_responses_n_en$perc_correct <- with(question_responses_n_en, ifelse(attachment == "ambiguous", NA, ifelse(attachment == "N1 attachment", perc_n1, 1-perc_n1)) )
question_responses_n_tr$perc_correct <- with(question_responses_n_tr, ifelse(attachment == "ambiguous", NA, ifelse(attachment == "N1", perc_n1, 1-perc_n1)) )

  question_responses_n_en %>% group_by(attachment) %>% # attachment == "ambiguous"
  dplyr::summarise(
    M = mean(perc_correct),
    SE = sqrt(M*(1-M)/ (length(unique(subject))*24)  )
  )

  question_responses_n_tr %>% group_by(attachment == "ambiguous") %>% # attachment == "ambiguous"
  dplyr::summarise(
    M = mean(perc_correct),
    SE = sqrt(M*(1-M)/  length(unique(subject))*28 )
  )

# perc_n1_cis_en$experiment <- "English (Swets et al., 2008)"
# perc_n1_cis_tr$experiment <- "Turkish"
# perc_n1_cis <- bind_rows(perc_n1_cis_en, perc_n1_cis_tr)
# perc_n1_cis$attachment %<>% dplyr::recode("N1"="N1 attachment", "N2"="N2 attachment")
# 
# p_raw_n1 <-
#   perc_n1_cis %>% ggplot(aes(attachment, M, group = attachment)) +
#   #geom_point() +
#   geom_point(aes(y=M), shape = "square", size = 2) + geom_line() +
#   geom_errorbar(aes(ymin=M-1.96*SE, ymax=M+1.96*SE), width = 0.1) +
#   facet_wrap(~experiment) + scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + 
#   theme_bw() +
#   theme(legend.position="top") + scale_color_discrete(name = NULL) +
#   xlab(NULL) + ylab("Percentage of 'yes' responses")
# 
# p_raw_n1
# 
# perc_n1_cis

# z = 1.5
# ggsave(p_raw, file = "../figures/avg_perc.pdf", width = 3*z, height = 2*z)


############################################
### ahg-model posterior predictive check ###
############################################

post_pred_ahg <- readRDS("../workspace/models/ahg_posterior_predictive.rds")

post_pred_ahg$question_np %<>% dplyr::recode("NP1"="N1 question", "NP2"="N2 question")

p_ahg <-
post_pred_ahg %>% ggplot(aes(question_np, obs, color = attachment, group = attachment)) +
  #geom_point() +
  geom_point(aes(y=obs), shape = "square", size = 2, alpha = .5) + geom_line(alpha = .5) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95), width = 0.1) +
  facet_wrap(~exp) + scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + 
  theme_bw() +
  theme(legend.position="top") + scale_color_discrete(name = NULL) +
  xlab(NULL) + ylab("Percentage of 'yes' responses")

ggsave(p_ahg, file = "../figures/post_pred_ahg.pdf", width = 3*z, height = 2*z)


###############################################
### r1r2hg-model posterior predictive check ###
###############################################

post_pred_r1r2hg <- readRDS("../workspace/models/r1r2hg_posterior_predictive.rds")

post_pred_r1r2hg$question_np %<>% dplyr::recode("NP1"="N1 question", "NP2"="N2 question")

p_r1r2hg <-
post_pred_r1r2hg %>% ggplot(aes(question_np, obs, color = attachment, group = attachment)) +
  #geom_point() +
  geom_point(aes(y=obs), shape = "square", size = 2, alpha = .5) + geom_line(alpha = .5) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95), width = 0.1) +
  facet_wrap(~exp) + scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + 
  theme_bw() +
  theme(legend.position="top") + scale_color_discrete(name = NULL) +
  xlab(NULL) + ylab("Percentage of 'yes' responses")

p_r1r2hg

ggsave(p_r1r2hg, file = "../figures/post_pred_r1r2hg.pdf", width = 3*z, height = 2*z)



###########################################################
### by-subject MAP parameter estimates for r1r2hg-model ###
###########################################################

fit_en <- readRDS("../workspace/models/r1r2hg_fit_ranef_en.rds")
fit_tr <- readRDS("../workspace/models/r1r2hg_fit_ranef_tr.rds")

est_en <- fixef(fit_en) %>% .[,c("Q2.5", "Estimate", "Q97.5")] %>% plogis() %>% as.data.frame() %T>% { .$parameter <- rownames(.) }
est_tr <- fixef(fit_tr) %>% .[,c("Q2.5", "Estimate", "Q97.5")] %>% plogis() %>% as.data.frame() %T>% { .$parameter <- rownames(.) }

est_en$exp <- "EN"
est_tr$exp <- "TR"
est_en_all <- bind_rows(est_en, est_tr)
est_en_all$parameter %<>% dplyr::recode("g_Intercept"="g", "h_Intercept"="h", "r1_Intercept"="r₁", "r2_Intercept"="r₂")

p_est <-
ggplot(est_en_all, aes(exp , Estimate, color = parameter)) + 
  geom_point() + geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width = .2) + 
  facet_wrap(~parameter, nrow = 1) + theme_bw() + 
  scale_y_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1), labels = percent_format()) +
  theme(legend.position="none") + xlab(NULL) + ylab("Estimate")

z2 = 2
ggsave(p_est, file = "../figures/estimates_r1r2hg.pdf", width = 2.5*z, height = 1.5*z, device = cairo_pdf)

###########################################################
### by-subject MAP parameter estimates for r1r2hg-model ###
###########################################################

extract_by_subject_map_estimates <- function(fit)
{
  fixef <- fixef(fit) %>% .[,"Estimate"]
  ranef <- ranef(fit)
  by_subject_map <- as.data.frame(ranef$subject[,"Estimate",])
  
  by_subject_map$g_Intercept %<>% add( fixef["g_Intercept"] )
  by_subject_map$h_Intercept %<>% add( fixef["h_Intercept"] )
  by_subject_map$r1_Intercept %<>% add( fixef["r1_Intercept"] )
  by_subject_map$r2_Intercept %<>% add( fixef["r2_Intercept"] )
  by_subject_map$subject <- rownames(by_subject_map)
  
  by_subject_map %<>% tidyr::pivot_longer(-subject)
  
  by_subject_map
}

fixef_en <- fixef(fit_en)
fixef_tr <- extract_by_subject_map_estimates(fit_tr)

by_subject_map_en <- extract_by_subject_map_estimates(fit_en)
by_subject_map_tr <- extract_by_subject_map_estimates(fit_tr)

by_subject_map_en$name %<>% dplyr::recode("g_Intercept"="g", "h_Intercept"="h", "r1_Intercept"="r₁", "r2_Intercept"="r₂")
by_subject_map_tr$name %<>% dplyr::recode("g_Intercept"="g", "h_Intercept"="h", "r1_Intercept"="r₁", "r2_Intercept"="r₂")

col_hues <- c("#7CAE00", "#00BFC4", "#C77CFF")

p_est_en <-
  by_subject_map_en %>% subset(!name %in% "g") %>%
  ggplot(aes(plogis(value), fill = name )) + facet_wrap(~name, nrow = 1) + # , scales="free_y"
  geom_histogram(bins = 10, aes(y = stat(width*density)), color = "lightgrey") +
  scale_y_continuous(labels = percent_format(), breaks = c(0,.2,.4,.6), limits = c(0,.6)) +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Estimate") + ylab(NULL) + 
  scale_fill_manual(values = col_hues)
  #scale_x_continuous(limits = c(-0.1,1.1))

p_est_tr <-
  by_subject_map_tr %>% subset(!name %in% "g") %>%
  ggplot(aes( plogis(value), fill = name  )) + facet_wrap(~name, nrow = 1) + #, scales = "free_y"
  geom_histogram(bins = 10, aes(y = stat(width*density)), color = "lightgrey") +
  scale_y_continuous(labels = percent_format(), breaks = c(0,.2,.4,.6), limits = c(0,.6)) +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Estimate") + ylab(NULL) + 
  scale_fill_manual(values = col_hues) #+
  #scale_x_continuous(limits = c(0,1))


library(ggpubr)

fmt <- theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=1))

p_est_bysubj <- ggarrange(p_est_en + fmt, p_est_tr + fmt, nrow = 2)
p_est_bysubj <- p_est_bysubj + ylab("Percentage of subjects")

p_est_bysubj

z = 1.5
ggsave(p_est_bysubj, file = "../figures/estimates_r1r2hg_bysubj.pdf", width = 3*z, height = 3*z, device = cairo_pdf)


