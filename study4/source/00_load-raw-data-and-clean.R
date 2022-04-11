# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
  }
pacman::p_load(tidyverse, here, hablar, psych, qualtRics, varhandle, magrittr)

# load data ---------------------------------------------------------------

full_raw <- read_survey(here("study4", "data", "raw.csv"))

# full_raw <- fetch_survey(surveyID =  all_surveys() %>% arrange(desc(creationDate)) %>% filter(name == "dissertation_Study1.3") %>% select(id), start_date = "2021-12-06", force_request = T)


raw <- full_raw %>% dplyr::filter(Nationality == "United States of America" , `Gender...17` == "Woman" | `Gender...17` == "Man",
  Residence == "Yes", `Phone/tablet` == "No", DistributionChannel =="anonymous") 

## reasons for exclusion: not a preview, were not using a computer to complete survey, nationality was not United states of america, indicated not woman or man for gender, did not reside in the USA

# export excluded data -------------------------------------------------

excluded <- anti_join(full_raw, raw)

## removing previews to get real excluded participants

excluded %<>% filter(DistributionChannel =="anonymous")

write.csv(excluded, here("study4", "data", "excluded.csv"), row.names = F)

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
write.csv(dropped_out, here("study4", "data", "dropped_out.csv"), row.names = F)

# creating vars necessary for analysis -------------------------------------------------

## creating task_comp_check_acc variable: how many comp check ?s about the task they got wrong 

raw$task_comp_check_acc <- ifelse(raw$task_comp_check1 == "48", 0, ifelse(raw$task_comp_check2 == "2", 1,  ifelse(raw$task_comp_check2 == "66", 2, 3)))

## creating pay_comp_check_acc variable: how many comp check ?s about the payment scheme they got wrong

raw$pay_comp_check_acc <- 0

## if they enter $12 (correct answer), keep at current number wrong (0), otherwise, add 1 wrong 

raw$pay_comp_check_acc <- ifelse(raw$Comp_check_PR1 == "$12", raw$pay_comp_check_acc, raw$pay_comp_check_acc+ 1)

## if they enter $14 (correct answer) or they have a blank (meaning they got the first ? correct), keep at current number wrong, otherwise, add 1 wrong 

raw$pay_comp_check_acc <- ifelse(raw$comp_check_PR2 == "$14" | is.na(raw$comp_check_PR2), raw$pay_comp_check_acc, raw$pay_comp_check_acc+ 1)

### repeating above for all 3 questions

raw$pay_comp_check_acc <- ifelse(raw$Comp_check_comp1 == "$24", raw$pay_comp_check_acc, raw$pay_comp_check_acc+ 1)

raw$pay_comp_check_acc <- ifelse(raw$Comp_check_comp2 == "$28" | is.na(raw$Comp_check_comp2), raw$pay_comp_check_acc, raw$pay_comp_check_acc+ 1)


raw$pay_comp_check_acc <- ifelse(raw$Comp_check_comp3 == "$0", raw$pay_comp_check_acc, raw$pay_comp_check_acc+ 1)


raw$pay_comp_check_acc <- ifelse(raw$Comp_check_comp4 == "$0" | is.na(raw$Comp_check_comp4), raw$pay_comp_check_acc, raw$pay_comp_check_acc+ 1)  


## "Number of comprehension check questions incorrect" (combining two vars above for total number of incorrect across both types of comprehension): 

raw$n_comp_wrong <- raw$pay_comp_check_acc + raw$task_comp_check_acc

## practice problems recoding notes: 
## asked if they would like to study tables: Q1293 (for practice) or Q1294 (for control)  
## time spent on study tables: `Q1302_Page Submit`
## asked if they would like to practice problems: Q2615 (for practice) or Q2616 (for control)  
## practice problems: `1_Q2631`:`1_Q2640` (for control) or `1_Q2597`:`1_Q2606`. all start with a number representing the number of the loop they are on. max is 26. so last practice problem of last loop in control is 26_Q2640
## would you like to continue practicing?: `1_Q1292`:`26_Q1292` (for practice) or `1_Q2641`:`26_Q2641` (for control)


## study_tables_binary: yes/no study tables across conditions on first offer

raw %<>% mutate(study_tables_binary = coalesce(Q1293,Q1294)) %>% select(-c(Q1293,Q1294))

## practice_problems_binary: yes/no practice problems across conditions on first offer

raw %<>% mutate(practice_problems_binary = coalesce(Q2615,Q2616)) %>% select(-c(Q2616,Q2615))

## extra_practice_rounds_count: number of additional rounds practiced after completing first round of practice (aka how many times do they keep practicing after doing first one?)

raw %<>% dplyr::mutate_at(vars(contains("_Q1292")), 
                 ~as.numeric(ifelse(is.na(.)| . == "No" & Condition == 1, 0, 1))) 


## need to repeat for control -- making sure to add in qualifier to change value of new col if condition == 0 (eg NA might mean they weren't in that condition)

raw %<>% dplyr::mutate_at(vars(contains("_Q2641")), 
                              ~as.numeric(ifelse(is.na(.)| . == "No" & Condition == 0, 0, 1))) 

## summing across cols - inspo: 
#https://stackoverflow.com/questions/29006056/efficiently-sum-across-multiple-columns-in-r
#https://stackoverflow.com/questions/34643633/mutate-data-conditionally-in-dplyr
raw %<>% rowwise() %>% mutate(extra_practice_rounds_count = ifelse(Condition == 1,sum(across(contains("_Q1292"))),
                              ifelse(Condition == 0, sum(across(contains("_Q2641"))), NA))) 


## counting number of practice problems attempted (aka not left blank): 

raw %<>% ungroup(.) %>% mutate(practice_nonempty = rowSums(!is.na(raw %>% select(matches("_Q2597|_Q2598|_Q2599|_Q2600|_Q2601|_Q2602|_Q2603|_Q2604|_Q2605|_Q2606")))))

## checking var was created correctly: View(raw %>% filter(practice_nonempty > 0) %>% select(practice_nonempty, matches("_Q2597|_Q2598|_Q2599|_Q2600|_Q2601|_Q2602|_Q2603|_Q2604|_Q2605|_Q2606")))

## creating combined choice to compete var 

raw <- raw %>% tidyr::unite("comp_choice", 	c(choice_tourn_first2, choice_PR_first2), remove = T, na.rm = T) 

## storing var for exclusion based on qualtrics fraud detecion software (using Or function because only one of condition must be met)

raw %<>% mutate(fraud = ifelse(Q_RecaptchaScore < .5 | isTRUE(Q_RelevantIDDuplicate)| Q_RelevantIDDuplicateScore >= 75| Q_RelevantIDFraudScore >= 30, 1, 0)) 

# removing extra columns --------------------------------------------------

raw <- raw %>% dplyr::select(c(workerId, taskscore, exclude, `Gender...17`, `Gender_14_TEXT`, contains("comp_check"), n_comp_wrong, conf_rank, gender_perform, task_gender_prep, gender_compete, MC, risk, gen_gender_prep, Age, education, Condition, contains("Comp_check"), contains("choice"), extra_practice_rounds_count, study_tables_binary, practice_problems_binary, `Q1302_Page Submit`, MC, practice_nonempty)) 

raw %<>% select(-contains("_DO_"))

# rename columns --------------------------------------------------------

raw <- raw %>% dplyr::rename(
  gender = `Gender...17`, gender_other = `Gender_14_TEXT`, age = Age,
  better_gender_guess =gender_perform,
  comp_check_pr1 = Comp_check_PR1, comp_check_pr2 = comp_check_PR2, 
  comp_check_comp1 = Comp_check_comp1, comp_check_comp2 = Comp_check_comp2, comp_check_comp3 = Comp_check_comp3, comp_check_comp4 = Comp_check_comp4, perc_task_gender_pract = task_gender_prep, perc_gender_comp = gender_compete, perc_gen_gender_pract = gen_gender_prep,
  condition = Condition, task_score = taskscore, study_tables_time= `Q1302_Page Submit`
  )


# export vars and labels -----------------------------------------------------------


labels <- as.data.frame(cbind(names(raw), raw %>% sjlabelled::get_label()))
## adding descriptions for extra columns that were added

labels[labels$V2 == "",] <- c("taskscore", "filtering variable for excluding potential bots and/or duplicates","task comprehension check accuracy", "payment scheme comprehension check accuracy", "number of comprehension check questions wrong overall", "choice to compete", "optional choice (yes or no) to prepare" ,"total count of extra rounds reviewing multiplying problems")

write_csv(labels, file = here("study4","data" ,"vars-and-labels.csv"))


# recode vars -----------------------------------------------------------

raw$comp_choice <- recode(
  raw$comp_choice, "Piece-rate (where you will earn $.10 for every problem solved correctly)" = "piecerate",
  "Tournament (where you will earn $.20 for every problem solved correctly only if you win)" = "tournament",
)


raw$risk <- recode(
  raw$risk, "0(Not at all willing to take risks)" = "0",
  "10(Very willing to take risks)" = "10",
)


raw$condition <- recode(raw$condition,"1" = "pract", "0" = "control")

oldvalues <- sort(unique(raw$conf_rank)[2:12])
newvalues <- c(seq(from = 10, to = 90, by = 10), 100, 0)

raw$conf_rank <- newvalues[match(raw$conf_rank, oldvalues)]

raw <- raw %>% retype()

raw %<>% mutate(log_study_tables_time = log(study_tables_time)) 

raw %<>% mutate(practice_problems_binary_numeric = as.numeric(ifelse(practice_problems_binary == "Yes", 1, 0)), total_practice_rounds_count = practice_problems_binary_numeric+ extra_practice_rounds_count)

# export clean ------------------------------------------------------------------


write.csv(raw, here("study4", "data", "clean.csv"), row.names = F)

