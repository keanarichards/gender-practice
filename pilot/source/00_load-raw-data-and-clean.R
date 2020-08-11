# load packages -----------------------------------------------------------


## Package names
packages <- c("here", "tidyverse", "hablar")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

full_raw <- read_csv(here("pilot", "data", "raw.csv"))

# removing participants who don't meet inclusion criteria and row 2 from dataset----------------

raw <- full_raw %>%slice(-2) %>%  dplyr::filter(Nationality == "American" , Gender != "Other:" ,
  Residence == "United States" , `Phone/tablet` == "No") %>% filter(is.na(comprehension3) | comprehension3 == "QU",
  DistributionChannel =="anonymous")

# remove duplicate IPs ----------------------------------------------------

## first sort by date of completion (starting with earliest), remove people who have matching IP address, gender, and MTurkID
raw <- raw[order(raw$StartDate),]
raw <- raw[!duplicated(raw[c("IPAddress", "MTurkID", "Gender")]),]

## because there were people who had invalid IDs, need to subset people who have invalid MTurkID & remove duplicate IP addresses for those people. MTurkID should be 12-14 characters
## need to inspect & make sure MTurk IDs are different manually 

duplicated <- raw %>% filter(duplicated(IPAddress)| duplicated(raw$IPAddress, fromLast = T))

## will scan from top to bottom, since it's already sorted by start date (ascending), will delete the second most recent one
duplicated <- duplicated[!duplicated(duplicated$IPAddress),]

raw <- anti_join(raw, duplicated)


# export excluded data -------------------------------------------------

excluded <- anti_join(full_raw, raw)

## removing previews to get real excluded participants

excluded <- excluded %>% filter(DistributionChannel !="anonymous")

write.csv(excluded, here("pilot", "data", "excluded.csv"), row.names = F)


# remove extra columns --------------------------------------------------

raw <- raw  %>% dplyr::select(
  -c(StartDate:UserLanguage, Example, round1_paid:conf4_paid, 
  bonus_pay_round1and2:bonus_paycomp_round3, FL_54_DO, FL_61_DO, `timing1_First Click`:`timing_choice_Click Count`, `Q339_First Click`:Q376, MTurkID, `Phone/tablet`)
)

## selecting original var names to be used for export vars and labels

original_names <-names(x = raw)

# rename columns --------------------------------------------------------

raw <- raw %>% dplyr::rename(
  overall_score = SC0, round_1_score = round1score, age = Age, round_2_score = round2score, 
  round_3_score = round3score, income = ownincome, min_pract = numberpractrounds,
  pract_choice = optiontopract, improve_pract =improvement, better_gender_guess =gender_guess,
  conf_men =confidence2_men, conf_women =confidence3_women, tab_key =`tab key`, 
  race_other =race_6_TEXT, nationality =Nationality, nationality_other = Nationality_2_TEXT,
  residence = Residence, gender_other =Gender_3_TEXT, gender =Gender,age =  Age, comp_choice = choice,
)


# export vars and labels -----------------------------------------------------------

col_names <- names(x = raw)
des <-unlist(full_raw[1,original_names], use.names = F)
col_names_des <- cbind(col_names, des) 

col_names_des[26, "des"] <- "Score summed across all 3 rounds"


write.csv(col_names_des, here("pilot", "data", "vars-and-labels.csv"), row.names = F)

# recode vars -----------------------------------------------------------

raw$risk <- recode(raw$risk,"0(Not at all willing to take risks)" = "0", "10(very willing to take risks)" = "10")

raw <- raw %>% retype()

raw <- raw %>% convert(num(min_pract))

raw$comp_choice <- recode(raw$comp_choice,"Piece-rate (as in Round 1)" = "piecerate", "Tournament (as in Round 2)" = "tournament")

raw$gender <- recode(raw$gender, "Male" = "Man", "Female" = "Woman")


# export clean ------------------------------------------------------------------
## adding in id variable

raw$id <- seq(1:nrow(raw))


write.csv(raw, here("pilot", "data", "clean.csv"), row.names = F)
