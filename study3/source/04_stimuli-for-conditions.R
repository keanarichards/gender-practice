
# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "here")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

set.seed(5)


## baseline numbers 

n1 <- sample(1:12, replace = T, size = 50)
n2 <- sample(1:12, replace = T, size = 50)
limited_practice_A <- n1*n2
control_A <- n1+n2

## additional numbers for multiplication unlimited practice condition 

n3 <- sample(1:12, replace = T, size = 260)
n4 <- sample(1:12, replace = T, size = 260)
unlimited_practice_A <- n3*n4

## creating character vector of multiplication questions to insert into qualtrics

limited_practice_stim <- paste0(n1, " X ", n2)
control_stim <- paste0(n1, " + ", n2)
unlimited_practice_stim <- paste0(n3, " X ", n4)

## creating full dataframe with all Qs and As 

problems_answers_baseline <- tibble (n1, n2, limited_practice_A, control_A, limited_practice_stim, control_stim)

problems_answers_optional <- tibble(n3, n4,unlimited_practice_stim)

write.csv(problems_answers_baseline, here("study3", "data", "problems_answers_baseline.csv"), row.names = FALSE)

write.csv(problems_answers_baseline, here("study3", "data", "problems_answers_optional.csv"), row.names = FALSE)
