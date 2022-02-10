# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "hablar", "psych", "here")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# load data ---------------------------------------------------------------

full_raw <- read_csv(here("study2", "data", "raw.csv"))

raw <- full_raw %>% slice(-2) %>%  dplyr::filter(Nationality == "American" , Gender...19 != "Other (please specify):",
  Residence == "United States" , `Phone/tablet` == "No",DistributionChannel =="anonymous") 


## reasons for exclusion: people failed comprehension check, were not using a computer to complete survey, nationality was not american, indicated "other" for gender, did not reside in the US), or just did not finish the survey


# export excluded data -------------------------------------------------

excluded <- anti_join(full_raw, raw)

## removing previews to get real excluded participants

excluded %>% filter(DistributionChannel !="anonymous")

write.csv(excluded, here("study2", "data", "excluded.csv"), row.names = F)

## first storing the names for all columns 

col_names <-names(x = raw)

## adding in bonus payment data for exploratory analyses

total_pay <- read_csv("study2/data/total_payment_output.csv")

total_pay <- total_pay %>% tidyr::unite("pay", 	c(PR_pay, tourn_payment), remove = T, na.rm = T) 

raw <- left_join(raw, total_pay[c("combined_pay", "MTurkID")], by = "MTurkID")

# identify participants who didn't finish -----------------------------------

dropped_out <- raw %>% filter(Finished == "FALSE")
write.csv(dropped_out, here("study2", "data", "dropped_out.csv"), row.names = F)


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


## creating "Number of optional preparation rounds chosen" var. If  they rejected the opportunity at the first offer, recorded as 0, otherwise add 1. Repeat for 5 opportunities:

raw$extra_prep_count <-ifelse(raw$extra_prep1 == "No", 0, ifelse(raw$`extra-prep2` == "No", 1, ifelse(raw$extraprep3 == "No", 2, ifelse(raw$extraprep4 == "No", 3, ifelse(raw$extraprep5 == "No", 4, 5)))))

## creating "choice to prepare" variable. If extra_prep_count equals 0, then they did not choose to prepare, otherwise, encode as "yes" 

raw$pract_choice <- ifelse(raw$extra_prep_count == 0, "No","Yes")


## creating practice problems attempted variable (any practice problems not left blank): 

raw %<>% ungroup(.) %>% mutate(practice_nonempty = rowSums(!is.na(raw %>% select(Q1734:Q1893, Q1896:Q2055, Q2058:Q2217, Q2220:Q2379, Q2382:Q2541))))

## checking var was created correctly: View(raw %>% filter(practice_nonempty > 0) %>% select(practice_nonempty, Q1734:Q1893, Q1896:Q2055, Q2058:Q2217, Q2220:Q2379, Q2382:Q2541))


## creating total time spent on task var: 
## imputing mean of timing  within each participant across all control tasks for 5a page submit (control) (error in Qualtrics so was not stored): 


# raw <-raw %>% retype()
# 
# mean_timing_ctrl5a <- rowMeans(raw[grep("^timing_ctrl[1_4]a", names(raw))], na.rm = TRUE)
# raw$`timing_ctrl5a_Page Submit` <- as.numeric(ifelse(is.na(raw$`timing_ctrl5b_Page Submit`) == FALSE, mean_timing_ctrl5a, raw$`timing_ctrl5a_Page Submit`))

raw <-raw %>% retype()
# 
# total_timing_ctrl <-rowSums(raw[grep("^timing_ctrl[0-9]", names(raw))], na.rm = TRUE)
# 
# total_timing_prep <-rowSums(raw[grep("^prep_timing[0-9]", names(raw))], na.rm = TRUE)
# 
# 
# raw$total_time <- ifelse(is.na(raw$`timing_ctrl1a_Page Submit`) == FALSE, total_timing_ctrl,total_timing_prep)

## creating total time spent on answering questions (for measuring performance):

# qs_timing_ctrl <-rowSums(raw[grep("^timing_ctrl[0-9]a", names(raw))], na.rm = TRUE)
# 
# qs_timing_prep <-rowSums(raw[grep("^prep_timing[0-9]a|^prep-timing1[0-9]a|^prep-timing[0-9]a|prep_timing11_Page Submit", names(raw))], na.rm = TRUE)


# raw$time_qs <- ifelse(is.na(raw$`timing_ctrl1a_Page Submit`), qs_timing_prep,qs_timing_ctrl)
# 
# raw$total_time <-ifelse(raw$total_time == 0, NA, raw$total_time)
# raw$time_qs <- ifelse(raw$time_qs == 0, NA, raw$time_qs)


## creating performance on the fixed preparation rounds (units = ?s completed/second):

# raw$perf_fixed_rounds <- (raw$score_before_task/raw$time_qs)
# 
# ## creating performance on the extra preparation rounds - multiplying count of extra prep rounds by 2 minutes, since that was the fixed amount of time they were given to complete the problems: 
# 
# raw$extra_prep_count_time <- ifelse(raw$extra_prep_count > 0, raw$extra_prep_count*120, NA)

# raw <- raw %>% mutate(perf_extra_prep = unlimited_prep_score/extra_prep_count_time)

## creating combined comp choice var

raw <- raw %>% tidyr::unite("comp_choice", 	c(choice_tourn_first2, choice_PR_first2), remove = T, na.rm = T) 


## composite field-specific ability beliefs:


raw <- raw %>% mutate_at(vars(fatigue_1:fab_6, interest), list(~recode(.,"Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4, "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)))

## recoding reverse FAB items (ie. 3, 4)


raw$fab_3 = 8 - raw$fab_3 

raw$fab_4 = 8 - raw$fab_4


items <-c("fab_1", "fab_2", "fab_3", "fab_4", "fab_5", "fab_6")
scaleKey <- rep(1, 6)
results_fab <- scoreItems(keys = scaleKey, items = raw[items], impute = "none")


## composite fatigue:

items <-c("fatigue_1", "fatigue_2")
scaleKey <- rep(1, 2)
results_fati <- scoreItems(keys = scaleKey, items = raw[items], impute = "none")

raw$fati <- as.vector(results_fati$scores)

raw$fab <- as.vector(results_fab$scores)


# removing extra columns --------------------------------------------------

raw <- raw %>% dplyr::select(-c(contains("Click"), StartDate:UserLanguage,MTurkID, `Phone/tablet`, starts_with("del"), contains("ctrl_task"), matches("^prep_[0-9]|^prep[0-9]"), starts_with("Q"), 
 Gender_3_TEXT, SC0, FL_203_DO, FL_80_DO, FL_258_DO, preparationround1_DO,
 prepround2_DO, prepround3_DO, prepround4_DO, prepround5_DO, Multiplicationtask_DO, `Confidence/gender_DO`,
 `Demographics,risk,genderperceptionsofpractice,interest,fatigue,fab_DO`, extra_prep1:extraprep5))


## storing names final dataset

original_names <-intersect(names(raw), names(full_raw))

loc <- match(original_names, names(raw))

# rename columns --------------------------------------------------------

raw <- raw %>% dplyr::rename(
  gender = Gender...19, residence = Residence, residence_other = Residence_3_TEXT, 
  nationality = Nationality,  nationality_other = Nationality_2_TEXT,income = ownincome, age = Age,
  better_gender_guess =gender_perform,
  race_ethnicity = `Race/ethnicity`, race_ethnicity_other = `Race/ethnicity_7_TEXT`,
  comp_check_pr1 = Comp_check_PR1, comp_check_pr1_do = Comp_check_PR1_DO, comp_check_pr2 = comp_check_PR2, 
  comp_check_pr2_do = comp_check_PR2_DO, comp_check_comp1 = Comp_check_comp1, comp_check_comp1_do = Comp_check_comp1_DO,
  comp_check_comp2 = Comp_check_comp2, comp_check_comp2_do = Comp_check_comp2_DO,
  comp_check_comp3 = Comp_check_comp3, comp_check_comp3_do = Comp_check_comp3_DO,
  comp_check_comp4 = Comp_check_comp4, comp_check_comp4_do = Comp_check_comp4_DO,
  perc_task_gender_pract = task_gender_prep, perc_gender_comp = gender_compete, perc_gen_gender_pract = gen_gender_prep,
  condition = Condition, task_score = taskscore, fab_do = fab_DO, comp_choice_do = CompetitionChoice_DO, bonus_task = combined_pay
)

names(raw)[match(paste0("timing_ctrl", seq(1:5), "a_Page Submit"),names(raw))] <-  paste0("timing_ctrl", seq(1:5), "q")
names(raw)[match(paste0("timing_ctrl", seq(1:5), "b_Page Submit"),names(raw))] <-  paste0("timing_ctrl", seq(1:5), "a")

names(raw)[match(paste0("prep_timing", seq(1:8), "a_Page Submit"),names(raw))] <-  paste0("timing_prep", seq(1:8), "q")
names(raw)[match(paste0("prep-timing", c(9, 10, 12), "a_Page Submit"),names(raw))] <-  paste0("timing_prep", c(9,10,12), "q")
names(raw)[match(paste0("prep_timing11_Page Submit"),names(raw))] <-  "timing_prep11q"


names(raw)[match(paste0("prep_timing", seq(1:8), "b_Page Submit"),names(raw))] <-  paste0("timing_prep", seq(1:8), "a")
names(raw)[match(paste0("prep-timing", c(9, 10, 12), "b_Page Submit"),names(raw))] <-  paste0("timing_prep", c(9,10,12), "a")
names(raw)[match(paste0("prep-tming11b_Page Submit"),names(raw))] <-  "timing_prep11a"

# export vars and labels -----------------------------------------------------------


des <-unlist(full_raw[1,original_names], use.names = F)
col_names_des <- data.frame(cbind(names(raw[loc]), des))

## adding descriptions for extra columns that were added

## find names in "original_names" that are not in "full_raw" 

# col_names_des <- col_names_des %>% dplyr::rename(col_names = V1)
# 
# col_names_des <- col_names_des %>% add_row(col_names = raw %>% dplyr::select(-all_of(loc)) %>% names(), 
#   des = c("choice to compete","bonuses earned during task","task comprehension check accuracy",
#   "payment scheme comprehension check accuracy", "number of comprehension check questions wrong overall",
#   "total count of extra rounds reviewing multiplying problems", "optional choice (yes or no) to prepare", "total time spent on either control or preparation rounds", 
#   "total time spent on the questions during either control or preparation rounds", "performance on control or preparation rounds (based on number of questions completed relative to time spent on questions)",
#   "time spent on extra preparation rounds", "performance on extra preparation rounds (based on number of questions completed relative to time spent on questions)",
#   "composite score for fatigue", "composite score for field specific ability beliefs"))

write.csv(col_names_des, here("study2", "data", "vars-and-labels.csv"), row.names = F)

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

raw$comp_choice_do <- recode(raw$comp_choice_do,"choice_PR_first2" = "pr_first", 
                             "choice_tourn_first2" = "tournament_first"
)

raw$gender <- recode(raw$gender, "Male" = "Man", "Female" = "Woman")

raw <- raw %>% retype()



# export clean ------------------------------------------------------------------


write.csv(raw, here("study2", "data", "clean.csv"), row.names = F)
