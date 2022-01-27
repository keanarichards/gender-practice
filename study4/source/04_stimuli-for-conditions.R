
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
set.seed(5)

n3 <- sample(1:12, replace = T, size = 260)
n4 <- sample(1:12, replace = T, size = 260)
unlimited_practice_A <- n3*n4

## creating character vector of multiplication questions to insert into qualtrics

limited_practice_stim <- paste0(n1, " X ", n2)
control_stim <- paste0(n1, " + ", n2)
unlimited_practice_stim <- paste0(n3, " X ", n4)
unlimited_control_stim <- paste0(n3, " + ", n4)

## creating full dataframe with all Qs and As 

unlimited_practice_stim <- matrix(unlimited_practice_stim, nrow = 26, ncol = 10)
write.csv(unlimited_practice_stim, here("study4", "stimuli", "multiplication_problems.csv"), row.names = FALSE)

# subtraction task --------------------------------------------------------

first_num <- c(1:12, 2:13, 3:14, 4:15, 5:16, 6:17,7:18, 8:19, 9:20, 10:21, 11:22, 12:23)
second_num <- c(rep(1, 12), rep(2, 12), rep(3, 12), rep(4, 12), rep(5, 12), rep(6, 12), rep(7, 12), rep(8, 12), rep(9, 12), rep(10, 12), rep(11, 12), rep(12, 12))
unlimited_control_stim <- paste0(" ", first_num, " - ", second_num)
subtraction_A <- first_num - second_num

set.seed(5)
unlimited_control_stim <- sample(unlimited_control_stim, replace = T, size = 260)

unlimited_control_stim <- matrix(unlimited_control_stim, nrow = 26, ncol = 10)
write.csv(unlimited_control_stim, here("study4", "stimuli", "subtraction_problems.csv"), row.names = FALSE)





