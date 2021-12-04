
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
unlimited_control_stim <- paste0(n3, " + ", n4)

## creating full dataframe with all Qs and As 

unlimited_practice_stim <- matrix(unlimited_practice_stim, nrow = 26, ncol = 10)
write.csv(unlimited_practice_stim, here("study4", "stimuli", "multiplication_problems.csv"), row.names = FALSE)


unlimited_control_stim <- matrix(unlimited_control_stim, nrow = 26, ncol = 10)
write.csv(unlimited_control_stim, here("study4", "stimuli", "addition_problems.csv"), row.names = FALSE)

# creating matrices of 0s and 1 -------------------------------------------

## next step: create for loop -- going up to 30. saving each in "stimuli" folder as tableX (insert number of loop)




m <- 7
n <- 7

df <- data.frame(matrix(sample(0:1, m * n, replace = TRUE), m, n))

df <- flextable(df)
df <- delete_part(x = df, part = "header")



save_as_image(df,  path = "test.png") 
