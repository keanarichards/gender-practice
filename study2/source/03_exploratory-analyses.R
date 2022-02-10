
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

clean <- read_csv(here("study2", "data", "clean.csv"))


# analyses ----------------------------------------------------------------

clean$pract_choice <- factor(clean$pract_choice)

sec_exploratory1 <- glm(extra_prep_count  ~ gender+comp_choice, family="poisson", data=clean)


sec_exploratory2 <- glm(pract_choice ~ gender+comp_choice , data = clean, family = binomial())

practice_cond <- clean %>% filter(condition == "pract")

sec_exploratory3 <- glm(pract_choice ~ gender+comp_choice, data = practice_cond, family = binomial())
sec_exploratory4 <- lm(task_score ~pract_choice + comp_choice + gender, data = clean)

sec_exploratory5 <- lm(task_score ~condition, data = clean)


sec_exploratory6 <- lm(practice_nonempty ~ gender + comp_choice, data = clean)


## gender predicting various outcomes by itself

sec_exploratory7 <- glm(comp_choice~gender, family = binomial, data = clean)
sec_exploratory8 <- lm(risk~gender, data = clean)
sec_exploratory9 <- lm(conf_rank~gender, data = clean)
sec_exploratory10 <- lm(task_score~gender, data = clean)
sec_exploratory11 <- lm(task_score~gender + conf_rank + risk, data = clean)



