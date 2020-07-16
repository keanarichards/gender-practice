# load packages -----------------------------------------------------------

## Package names
packages <- c("plyr", "tidyverse","eply", "hablar", "here")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# load data ---------------------------------------------------------------

full_raw <- read_csv(here("study1", "data", "raw.csv"))

# removing participants who don't meet inclusion criteria and row 2 from dataset----------------

raw <- full_raw %>% slice(-2) %>%  dplyr::filter(Nationality == "American" , Gender != "Other (please specify):",
  Residence == "United States" , `Phone/tablet` == "No", Progress == "100", DistributionChannel =="anonymous")


# export excluded data -------------------------------------------------

excluded <- anti_join(full_raw, raw)

## removing previews to get real excluded participants

excluded %>% filter(DistributionChannel !="anonymous")

write.csv(excluded, here("study1", "data", "excluded.csv"), row.names = F)

## removed 284 rows

# creating review count vars --------------------------------------------------

## round 1 starts at 47
## round 2 starts at 178
## round 3 starts at 374
## round 4 starts at 583
## 5 at 792
## 6 at 1001
## 7 at 1210
## 8 at 1419
## 9 at 1628
## 10 at 1837
## 11 at 2046
## 12 at 2255 to 2463

ones <- raw[seq(from = 47, to = 177, by = 13)]

twos <- raw[seq(from = 178,to = 373, by= 13)]

threes <- raw[seq(from = 374, to = 582, by = 13)]

fours <- raw[seq(from = 583,to = 791, by= 13)]

fives <- raw[seq(from = 792, to = 1000, by = 13)]

sixes <- raw[seq(from = 1001,to = 1209, by= 13)]

sevens <-raw[seq(from = 1210,to = 1418, by= 13)] 
  
eights <- raw[seq(from = 1419,to = 1627, by= 13)]

nines <- raw[seq(from = 1628,to = 1836, by= 13)]
  
tens <- raw[seq(from = 1837,to = 2045, by= 13)]

elevens <- raw[seq(from = 2046,to = 2254, by= 13)]
  
twelves <- raw[seq(from = 2255,to = 2463, by= 13)]

df.list <- list(ones, twos, threes, fours, fives, sixes, sevens, eights, nines, tens, elevens, twelves)

s <- lapply(df.list, function(x) {rowSums(x== "Yes", na.rm = T)})

s <- as.data.frame(s, col.names = paste0("pract_count", c(1:12)))

raw <- cbind(raw, s)


# removing extra columns --------------------------------------------------


raw <- raw %>% dplyr::select(-c(47:2463, Preparation1times_DO:Review12times_DO, StartDate:UserLanguage, Example,  
  MTurkID, `Phone/tablet`, Q1201:Q1212, `Q257_First Click`:Q543, SC0, Gender_1, `2_Q1382 - Topics`, FL_80_DO, FL_203_DO))


## selecting original var names to be used for export vars and labels

raw <- raw %>% tidyr::unite("comp_choice", choice_tourn_first1:choice_PR_first2, remove = T, na.rm = T) 

raw <- raw %>% tidyr::unite("comp_choice_do", FL_133_DO:FL_135_DO, remove = T, na.rm = T) 


## creating task_comp_check_accnumber variable: how many comp check ?s about the task they got wrong 

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

## add in total review count var

raw$total_review_count <-raw %>% dplyr::select(pract_count1:pract_count12) %>% rowSums(.) 

original_names <-intersect(names(raw), names(full_raw))

loc <- match(original_names, names(raw))

# rename columns --------------------------------------------------------


raw <- raw %>% dplyr::rename(
  gender = Gender, gender_other = Gender_3_TEXT, residence = Residence, residence_other = Residence_3_TEXT, 
  nationality = Nationality,  nationality_other = Nationality_2_TEXT,income = ownincome, age = Age,  
  pract_choice = prep_choice,  better_gender_guess =gender_perform,
  race_ethnicity = `Race/ethnicity`, race_ethnicity_other = `Race/ethnicity_7_TEXT`,
  comp_check_pr1 = Comp_check_PR1, comp_check_pr1_do = Comp_check_PR1_DO, comp_check_pr2 = comp_check_PR2, 
  comp_check_pr2_do = comp_check_PR2_DO, comp_check_comp1 = Comp_check_comp1, comp_check_comp1_do = Comp_check_comp1_DO,
  comp_check_comp2 = Comp_check_comp2, comp_check_comp2_do = Comp_check_comp2_DO,
  comp_check_comp3 = Comp_check_comp3, comp_check_comp3_do = Comp_check_comp3_DO,
  comp_check_comp4 = Comp_check_comp4, comp_check_comp4_do = Comp_check_comp4_DO,
  perc_task_gender_pract = gender_prep, perc_gender_comp = gender_compete, perc_gen_gender_pract = perc_gender_prep,
  condition = Condition, task_score = taskscore
)

# export vars and labels -----------------------------------------------------------


des <-unlist(full_raw[1,original_names], use.names = F)
col_names_des <- data.frame(cbind(names(raw[loc]), des))

## adding descriptions for extra columns that were added

## find names in "original_names" that are not in "full_raw" 

col_names_des <- col_names_des %>% dplyr::rename(col_names = V1)

col_names_des <- col_names_des %>% add_row(col_names = raw %>% dplyr::select(-all_of(loc)) %>% names(), 
  des = c("choice to compete", "display order for choice to compete", paste("count of practice for", seq(1:12), "times tables", "total_review_count"), 
  "task comprehension check accuracy", "payment scheme comprehension check accuracy", "number of comprehension check questions wrong overall", "total number of rounds reviewing multiplying problems"))

write.csv(col_names_des, here("study1", "data", "vars-and-labels.csv"), row.names = F)

# recode vars -----------------------------------------------------------

raw$comp_choice <- recode(raw$comp_choice, "Piece-rate (where you will earn $.10 for every problem solved correctly)" = "piecerate",
  "Tournament (where you will earn $.20 for every problem solved correctly only if you win)" = "tournament",
  "Tournament (where you will earn $.20 for every problem solved correctly  only if you win)" = "tournament")

raw$risk <- recode(raw$risk,"0(Not at all willing to take risks)" = "0", "10(Very willing to take risks)" = "10")

raw$condition <- recode(raw$condition,"1" = "pract", "0" = "control")

oldvalues <- sort(unique(raw$conf_rank))
newvalues <- c(seq(from = 10, to = 90, by = 10), 100, 0)

raw$conf_rank <- newvalues[match(raw$conf_rank, oldvalues)]

raw$comp_choice_do <- recode(raw$comp_choice_do,"CompetitionChoice--preparePRfirst" = "pr_first", 
  "CompetitionChoice--controltourfirst" = "tournament_first", "CompetitionChoice--preparetourfirst" = "tournament_first", 
  "CompetitionChoice--PRfirst" = "pr_first"
)

raw$gender <- recode(raw$gender, "Male" = "Man", "Female" = "Woman")

raw <- raw %>% retype()

# export clean ------------------------------------------------------------------
## adding in id variable

raw$id <- seq(1:nrow(raw))
write.csv(raw, here("study1" , "data", "clean.csv"), row.names = F)
