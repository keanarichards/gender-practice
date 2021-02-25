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

full_raw <- read_csv(here("study3", "data", "raw.csv"))

raw <- full_raw %>% slice(-2) %>%  dplyr::filter(DistributionChannel =="anonymous") 

## first storing the names for all columns 

col_names <-names(x = raw)

# removing extra columns --------------------------------------------------

raw <- raw %>% dplyr::select(-c(Gender, id,contains("Click"),contains("task_control_cond"),contains("task_prep_cond"), StartDate:UserLanguage, starts_with("del"), starts_with("Q"), EndLoop))

# creating vars necessary for analysis -------------------------------------------------

## creating total time spent on task var and removing unnecessary time columns: 

raw <- raw %>% retype()

total_timing_ctrl <-rowSums(raw[grep("(\\d{1,2})(_timing_control_cond_Page Submit)", names(raw))], na.rm = TRUE)

total_timing_prep <-rowSums(raw[grep("(\\d{1,2})(_timing_prep_cond_Page Submit)", names(raw))], na.rm = TRUE)

raw$total_time <- ifelse(is.na(raw$`1_timing_control_cond_Page Submit`) == FALSE, total_timing_ctrl,total_timing_prep)

raw <- raw %>% dplyr::select(-c(contains("timing")))

## recoding likert scale items:

raw <- raw %>% mutate_at(vars(control_prep_beliefs, interest), list(~recode(.,"Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3, "Neither agree nor disagree" = 4, "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)))
## storing original var names: 

original_names <-intersect(names(raw), names(full_raw))

loc <- match(original_names, names(raw))

# rename columns --------------------------------------------------------

raw <- raw %>% dplyr::rename(gender_other = gender_3_TEXT,subj_confidence = subj_confidence_1,
  better_gender_guess = perc_gen_gender_prac, 
  condition = Condition, task_score = SC0)

# export vars and labels -----------------------------------------------------------


des <-unlist(full_raw[1,original_names], use.names = F)
col_names_des <- data.frame(cbind(names(raw[loc]), des))

## adding descriptions for extra columns that were added

## find names in "original_names" that are not in "full_raw" 

col_names_des <- col_names_des %>% dplyr::rename(col_names = V1)
col_names_des <- col_names_des %>% add_row(col_names = raw %>% dplyr::select(-all_of(loc)) %>% names(), 
  des = c("total time spent on the questions during either control or preparation rounds"))
        
write.csv(col_names_des, here("study3", "data", "vars-and-labels.csv"), row.names = F)

# recode vars -----------------------------------------------------------

raw$condition <- recode(raw$condition,"1" = "pract", "0" = "control")

raw$gender <- recode(raw$gender, "Male" = "Man", "Female" = "Woman", "I identify my gender as: _______ (please specify)" = "Other")
rec <- function(x, na.rm = FALSE) (6 - x)
raw <- raw%>% mutate_at(c("rank_explanations_2", "rank_explanations_4", "rank_explanations_5", "rank_explanations_6", "rank_explanations_8"), rec)

raw <- raw %>% retype()

# export clean ------------------------------------------------------------------


write.csv(raw, here("study3", "data", "clean.csv"), row.names = F)
