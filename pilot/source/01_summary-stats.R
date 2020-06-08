
# load packages -----------------------------------------------------------

## Package names
packages <- c("readr", "tidyverse")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv("pilot/data/clean.csv")



# demographics ------------------------------------------------------------

## age: average & SD & range 

(clean %>% summarize(mean = mean(age), sd = sd(age)))[1,1] 

range(clean$age)

## gender: participation rates

prop.table(table(clean$gender))*100

## race: participation rates

prop.table(table(clean$race))*100



# stats by gender ---------------------------------------------------------

## mean performance of men vs women

raw %>% group_by(gender) %>% summarize(mean_performance = mean(overall_score, na.rm = T)) 
