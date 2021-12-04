# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
  }
pacman::p_load(tidyverse, here, hablar, psych, qualtRics)

# load data ---------------------------------------------------------------

full_raw <- read_survey(here("study4", "data", "raw.csv"))

## add this back in when running study: ,DistributionChannel =="anonymous"

raw <- full_raw %>% dplyr::filter(Nationality == "United States of America" , `Gender...24` == "Woman" | `Gender...24` == "Man",
  Residence == "United States of America" , `Phone/tablet` == "No") 

## reasons for exclusion: not a preview, were not using a computer to complete survey, nationality was not United states of america, indicated not woman or man for gender, did not reside in the USA, or just did not finish the survey

# export excluded data -------------------------------------------------

excluded <- anti_join(full_raw, raw)

## removing previews to get real excluded participants

excluded %>% filter(DistributionChannel !="anonymous")

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

## creating "Number of optional preparation rounds chosen" var. converting to numeric - where anything but 1 is considered 0, then summing across all matching cols - only for ppts in practice cond

raw %<>% dplyr::mutate_at(vars(contains("_Q2566")), 
                 ~as.numeric(ifelse(is.na(.)| . == "No" & Condition == 1, 0, 1))) 


## need to repeat for control -- making sure to add in qualifier to change value of new col if condition == 0 (eg NA might mean they weren't in that condition)

raw %<>% dplyr::mutate_at(vars(contains("_Q1244")), 
                              ~as.numeric(ifelse(is.na(.)| . == "No" & Condition == 0, 0, 1))) 


## summing across cols - inspo: 
#https://stackoverflow.com/questions/29006056/efficiently-sum-across-multiple-columns-in-r
#https://stackoverflow.com/questions/34643633/mutate-data-conditionally-in-dplyr
raw %<>% mutate(pract_count = ifelse(
  test = (Condition == 1), yes = sum(across(contains("_Q2566"),no = 
                                              ifelse(test = (Condition == 0), yes = sum(across(contains("_Q1244"), no = NA))))))) 
## creating combined choice to compete var 

raw <- raw %>% tidyr::unite("comp_choice", 	c(choice_tourn_first2, choice_PR_first2), remove = T, na.rm = T) 

## creating "choice to prepare" variable. If extra_prep_count equals 0, then they did not choose to prepare, otherwise, encode as "yes" 

raw$pract_choice <- ifelse(raw$pract_count == 0, "No","Yes")

## storing var for exclusion (using Or function because only one of condition must be met)

raw %<>% mutate(exclude = ifelse(Q_RecaptchaScore < .5 | isTRUE(Q_RelevantIDDuplicate)| Q_RelevantIDDuplicateScore >= 75| Q_RelevantIDFraudScore >= 30, 1, 0)) 
# removing extra columns --------------------------------------------------

raw <- raw %>% dplyr::select(c(taskscore, exclude, `Gender...24`, `Gender_7_TEXT`, contains("comp_check"), n_comp_wrong, conf_rank, gender_perform, task_gender_prep, gender_compete, MC, risk, gen_gender_prep, Age, education, Condition, contains("Comp_check"), contains("choice"), pract_count)) 


# rename columns --------------------------------------------------------

raw <- raw %>% dplyr::rename(
  gender = `Gender...24`, gender_other = `Gender_7_TEXT`, age = Age,
  better_gender_guess =gender_perform,
  comp_check_pr1 = Comp_check_PR1, comp_check_pr2 = comp_check_PR2, 
  comp_check_comp1 = Comp_check_comp1, comp_check_comp2 = Comp_check_comp2, comp_check_comp3 = Comp_check_comp3, comp_check_comp4 = Comp_check_comp4, perc_task_gender_pract = task_gender_prep, perc_gender_comp = gender_compete, perc_gen_gender_pract = gen_gender_prep,
  condition = Condition, task_score = taskscore
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

raw$risk <- recode(raw$risk,"0(Not at all willing to take risks)" = "0", "10(Very willing to take risks)" = "10")

raw$condition <- recode(raw$condition,"1" = "pract", "0" = "control")

oldvalues <- sort(unique(raw$conf_rank))
newvalues <- c(seq(from = 10, to = 90, by = 10), 100, 0)

raw$conf_rank <- newvalues[match(raw$conf_rank, oldvalues)]

raw <- raw %>% retype()

# export clean ------------------------------------------------------------------


write.csv(raw, here("study4", "data", "clean.csv"), row.names = F)
