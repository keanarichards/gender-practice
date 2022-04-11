# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
  }
pacman::p_load(tidyverse, here, hablar, psych, qualtRics, varhandle, magrittr)

# load data ---------------------------------------------------------------

full_raw <- read_survey(here("study5", "data", "raw.csv"))

# full_raw <- fetch_survey(surveyID =  all_surveys() %>% arrange(desc(creationDate)) %>% filter(name == "dissertation_Study1.3") %>% select(id), start_date = "2021-12-06", force_request = T)

raw <- full_raw %>% dplyr::filter(Nationality == "United States of America" , `Gender...17` == "Woman" | `Gender...17` == "Man",
  Residence == "Yes", `Phone/tablet` == "No", DistributionChannel =="anonymous") 

## reasons for exclusion: not a preview, were not using a computer to complete survey, nationality was not United states of america, indicated not woman or man for gender, did not reside in the USA


# export excluded data -------------------------------------------------

excluded <- anti_join(full_raw, raw)

## removing previews to get real excluded participants

excluded %<>% filter(DistributionChannel =="anonymous")

write.csv(excluded, here("study5", "data", "excluded.csv"), row.names = F)

## first storing the names for all columns 

col_names <-names(x = raw)

## adding in bonus payment data for exploratory analyses - probably not needed for this study. 

# total_pay <- read_csv("study4/data/total_payment_output.csv")
# 
# total_pay <- total_pay %>% tidyr::unite("pay", 	c(PR_pay, tourn_payment), remove = T, na.rm = T) 
# 
# raw <- left_join(raw, total_pay[c("combined_pay", "MTurkID")], by = "MTurkID")

# identify participants who didn't finish -----------------------------------

dropped_out <- raw %>% filter(Finished == "FALSE")
write.csv(dropped_out, here("study5", "data", "dropped_out.csv"), row.names = F)

# creating vars necessary for analysis -------------------------------------------------

## creating task_comp_check_acc variable: how many comp check ?s about the task they got wrong 

raw$task_comp_check_acc <- ifelse(raw$task_comp_check1 == "48", 0, ifelse(raw$task_comp_check2 == "2", 1,  ifelse(raw$task_comp_check2 == "66", 2, 3)))

## practice problems recoding notes: 
## asked if they would like to practice problems: Q2615 (renamed later to practice_problems_binary)
## time spent practicing in each round of practice: `1_Q1333_Page Submit` - all start with a number representing the number of the loop they are on. max is 26. so last practice problem of last loop in control is 26_Q1333_Page Submit 
## practice problems: `1_Q2597`:`1_Q2606`. all start with a number representing the number of the loop they are on. max is 26. so last practice problem of last loop is 26_Q2606
## would you like to continue practicing?: `1_Q1292`:`26_Q1292` 

## extra_practice_rounds_count: number of additional rounds practiced after completing first round of practice (aka how many times do they keep practicing after doing first one?)

raw %<>% dplyr::mutate_at(vars(contains("_Q1292")), 
                 ~as.numeric(ifelse(is.na(.)| . == "No", 0, 1))) 

## summing across cols - inspo: 
#https://stackoverflow.com/questions/29006056/efficiently-sum-across-multiple-columns-in-r
#https://stackoverflow.com/questions/34643633/mutate-data-conditionally-in-dplyr
raw %<>% rowwise() %>% mutate(extra_practice_rounds_count = sum(across(contains("_Q1292")))) 

## counting number of practice problems attempted (aka not left blank): 

raw %<>% ungroup(.) %>% mutate(practice_nonempty = rowSums(!is.na(raw %>% select(matches("_Q2597|_Q2598|_Q2599|_Q2600|_Q2601|_Q2602|_Q2603|_Q2604|_Q2605|_Q2606")))))

## checking var was created correctly: View(raw %>% filter(practice_nonempty > 0) %>% select(practice_nonempty, matches("_Q2597|_Q2598|_Q2599|_Q2600|_Q2601|_Q2602|_Q2603|_Q2604|_Q2605|_Q2606")))

## total_pract_time: total amt of time spent across all practice rounds 

raw %<>% rowwise() %>% mutate(total_pract_time = sum(across(contains("_Q1333_Page Submit")), na.rm = T)) 


## storing var for exclusion based on qualtrics fraud detecion software (using Or function because only one of condition must be met)


raw %<>% mutate(fraud = ifelse(Q_RecaptchaScore < .5 | isTRUE(Q_RelevantIDDuplicate)| Q_RelevantIDDuplicateScore >= 75| Q_RelevantIDFraudScore >= 30, 1, 0)) 

## some people have missing values in the Q_RecaptchaScore, which leads them to have missing values in fraud var.   
## after looking thru their data (View(raw %>% filter(is.na(Q_RecaptchaScore)) %>% select(Q_RecaptchaScore, `Duration (in seconds)`))), decided to keep them
## so recoding their NA so they aren't excluded



raw %<>% mutate(fraud= 
                  replace_na(fraud,0))



# removing extra columns --------------------------------------------------

raw <- raw %>% dplyr::select(c(workerId, taskscore,  `Gender...17`, `Gender_14_TEXT`, contains("comp_check"), Q2615, perc_practice_all, perc_practice_men, perc_practice_women, 
                               conf_rank, gender_perform, task_gender_prep, gender_compete_F, gender_compete_M, risk, gen_gender_prep, Age, education, Condition,  
                               extra_practice_rounds_count, practice_nonempty, fraud, Q_RecaptchaScore, Q_RelevantIDDuplicate, Q_RelevantIDDuplicateScore, Q_RelevantIDFraudScore, total_pract_time, Finished)) 

# rename columns --------------------------------------------------------

raw %<>% dplyr::rename(
  practice_problems_binary = Q2615,
  gender = `Gender...17`, gender_other = `Gender_14_TEXT`, age = Age,
  better_gender_guess =gender_perform,
  perc_task_gender_pract = task_gender_prep, perc_gender_comp_M = gender_compete_M,perc_gender_comp_F = gender_compete_F, perc_gen_gender_pract = gen_gender_prep,
  condition = Condition, task_score = taskscore
  )


# export vars and labels -----------------------------------------------------------


labels <- as.data.frame(cbind(names(raw), raw %>% sjlabelled::get_label()))

## adding descriptions for extra columns that were added

labels[labels$V1 == "task_comp_check_acc",] <- "number of comprehension check questions wrong overall"
  
labels[labels$V2 == "", "V2"] <- c("total count of extra rounds reviewing multiplying problems", "total number of practice questions that were not left blank")


write_csv(labels, file = here("study5","data" ,"vars-and-labels.csv"))


# recode vars -----------------------------------------------------------

raw$risk <- recode(
  raw$risk, "0(Not at all willing to take risks)" = "0",
  "10(Very willing to take risks)" = "10",
)


oldvalues <- sort(unique(raw$conf_rank)[2:12])
newvalues <- c(seq(from = 10, to = 90, by = 10), 100, 0)

raw$conf_rank <- newvalues[match(raw$conf_rank, oldvalues)]

oldvalues <- sort(unique(raw$perc_practice_all)[2:12])

raw$perc_practice_all <- newvalues[match(raw$perc_practice_all, oldvalues)]


oldvalues <- sort(unique(raw$perc_practice_men)[2:12])

raw$perc_practice_men <- newvalues[match(raw$perc_practice_men, oldvalues)]


oldvalues <- sort(unique(raw$perc_practice_women)[2:12])

raw$perc_practice_women <- newvalues[match(raw$perc_practice_women, oldvalues)]

raw %<>% mutate(perc_practice_all= 
  replace_na(perc_practice_all,0),
  perc_practice_men= 
    replace_na(perc_practice_men,0),
  perc_practice_women= 
    replace_na(perc_practice_women,0), 
  conf_rank = replace_na(conf_rank,0)
)



## creating perceived practice deviation variables: self-rated decile - actual decile based on number of practice problems completed. 
## pulling from bonus pay code to create deciles 

## first need perceived practice deviation relative to all other ppts

raw$perceived_pract_dev <-raw$perc_practice_all - percent_rank(raw$practice_nonempty)*100

## then relative for each gender-specific question

raw$perceived_pract_dev_F <-raw$perc_practice_women - percent_rank(raw$practice_nonempty)*100

raw$perceived_pract_dev_M <-raw$perc_practice_men - percent_rank(raw$practice_nonempty)*100

## then create percentile for women

## cut: 
# case_when(practice_nonempty==0 ~ 0,
#           practice_nonempty == 1~ 50, 
#           practice_nonempty == 10 ~60)
# 
# quantile(women$practice_nonempty, probs = seq(0, 1, by = .1))

raw$condition <- recode(raw$condition,"1" = "tournament", "0" = "piecerate")

raw %<>% mutate(practice_problems_binary_numeric = as.numeric(ifelse(practice_problems_binary == "Yes", 1, 0)), total_practice_rounds_count = practice_problems_binary_numeric+ extra_practice_rounds_count)

# export clean ------------------------------------------------------------------


write.csv(raw, here("study5", "data", "clean.csv"), row.names = F)

