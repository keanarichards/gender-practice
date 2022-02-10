
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

# load data ---------------------------------------------------------------

clean <- read_csv(here("study1", "data", "clean.csv"))
clean$comp_choice <- factor(clean$comp_choice)

# analyses ----------------------------------------------------------------

clean$pract_choice <- factor(clean$pract_choice)

sec_exploratory1 <- glm(pract_choice ~ gender+comp_choice , data = clean, family = binomial())

sec_exploratory2 <- glm(pract_choice ~ gender*comp_choice , data = clean, family = binomial())

sec_exploratory3 <- lm(task_score ~pract_choice + comp_choice + gender, data = clean)

sec_exploratory4 <- lm(task_score ~total_review_count + comp_choice + gender, data = clean)

sec_exploratory5 <- lm(practice_nonempty ~ gender*comp_choice, data = clean)

## gender predicting various outcomes by itself

sec_exploratory6 <- glm(comp_choice~gender, family = binomial, data = clean)
sec_exploratory7 <- lm(risk~gender, data = clean)
sec_exploratory8 <- lm(conf_rank~gender, data = clean)
sec_exploratory9 <- lm(task_score~gender, data = clean)
sec_exploratory10 <- lm(task_score~gender + conf_rank + risk, data = clean)




