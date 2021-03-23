
library(plyr)
library(dplyr)
library(magrittr)
library(brms)
library(ggplot2)



path_responses_tr <- "../data/question_responses.rda"
path_out_responses <- "../workspace/data/question_responses_tr_en.rda"


load(path_responses_tr)

# # compute by-item accuracy for fillers
# filler_item_accuracies <-
#   questions_fillers %>% group_by(item) %>%
#   dplyr::summarize(perc_correct = mean(response_correct, na.rm = T), 
#                    N = sum( !is.na( response_correct )) ) %>% 
#   dplyr::arrange(perc_correct)
# 
# # Filler items 1028, 1017, and 1024 have the lowest accuracy (35%-55%) - the next worst item 
# # is at ~75% accuracy. They have to be excluded.
# bad_filler_items <- c(1028, 1017, 1024)
# questions_fillers %<>% subset(!item %in% bad_filler_items)

# # merge in the filler accuracy
# filler_accuracy_by_subj <-
#   questions_fillers %>% group_by(subject) %>% 
#   dplyr::summarise( filler_fa = mean(response_yes[!correct_response_yes]), 
#                     filler_hits = mean(response_yes[correct_response_yes]),
#                     filler_accuracy = (1-filler_fa+filler_hits)/2 ) %>%
#   ungroup() %>%
#   dplyr::mutate( filler_accuracy_group = Hmisc::cut2(filler_accuracy, g = 2) )
# questions_rc %<>% left_join( filler_accuracy_by_subj )




#################################
###  prep Turkish RC data  ###
#################################

resp_averages_n <- questions_rc %>% 
  subset(stype != "control") %>%
  group_by(subject, stype, attachment, condition, question_np) %>%
  dplyr::summarise(n_yes = sum(response_yes),
                   n = n(), 
                   n_n1 = ifelse(question_np[1] == "NP1", n_yes, n-n_yes),
                   perc_yes = n_yes/n
  )

resp_averages_n %<>% left_join( data.frame(question_np = c("NP1", "NP2"), probe_n1 = c(1, 0)) )
resp_averages_n %<>% left_join( data.frame(condition = c("a", "b", "c"), condition_id = c(3, 2, 1)) )
resp_averages_n_tr <- resp_averages_n


question_responses_n_tr <- questions_rc %>% 
  subset(stype != "control") %>%
  group_by(subject, stype, attachment, condition, question_np) %>%
  dplyr::summarise(n_yes = sum(response_yes),
                   n = n(), 
                   n_n1 = ifelse(question_np[1] == "NP1", n_yes, n-n_yes),
                   perc_yes = n_yes/n
                  )

question_responses_n_tr %<>% left_join( data.frame(question_np = c("NP1", "NP2"), probe_n1 = c(1, 0)) )
question_responses_n_tr %<>% left_join( data.frame(condition = c("a", "b", "c"), condition_id = c(3, 2, 1)) )


question_responses_n_summary_tr <-
  question_responses_n_tr %>% group_by(attachment, question_np) %>% 
  dplyr::summarise( perc_yes = mean(perc_yes) )


#################################
###  prep English RC data  ###
#################################


df <- read.csv("https://raw.githubusercontent.com/plogacev/manuscript_LogacevVasishth_CogSci_SMCM/master/data_original/Data_Swets_et_al/Data_All.csv", sep=";", as.is=T)
question_responses_en <- df %>% dplyr::select(-the:-w18)

id_cols <- c("subject","item","trial","X1","response_yes","response_RT","response_correct","qtype","attachment","question_np")
colnames(question_responses_en)[1:10] <- id_cols

question_responses_en$qtype %<>% dplyr::recode("1"="RC questions","2"="superficial","3"="occasional")
question_responses_en$attachment %<>% dplyr::recode("1"="ambiguous","2"="N1 attachment","3"="N2 attachment")
question_responses_en$response_yes %<>% as.character %>% dplyr::recode("1"=1,"2"=0)
question_responses_en$question_np %<>% dplyr::recode("1"="NP1","2"="NP2")

question_responses_en %<>% subset(qtype == "RC questions")


question_responses_n_en <-
  question_responses_en %>% group_by(subject,attachment, question_np) %>% 
  dplyr::summarise( n_yes = sum(response_yes),
                    n = n(), 
                    n_n1 = ifelse(question_np[1] == "NP1", n_yes, n-n_yes),
                    perc_yes = n_yes/n
                  )
question_responses_n_en %<>% left_join( data.frame(question_np = c("NP1", "NP2"), probe_n1 = c(1, 0)) )
question_responses_n_en %<>% left_join( data.frame(attachment = c("ambiguous", "N2 attachment", "N1 attachment"), condition_id = c(3, 2, 1)) )


question_responses_n_summary_en <-
  question_responses_n_en %>% group_by(attachment, question_np) %>% 
  dplyr::summarise( perc_yes = mean(perc_yes) )


save(
question_responses_n_en,
question_responses_n_summary_en,
question_responses_n_summary_tr,
question_responses_n_tr,
file = path_out_responses
)
