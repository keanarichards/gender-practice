
# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "here", "pscl")

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
clean$comp_choice <- factor(clean$comp_choice)
practice_cond <- clean %>% filter(condition == "pract")


sec_exploratory1 <- glm(extra_prep_count  ~ gender+comp_choice, family="poisson", data=clean)


sec_exploratory2 <- glm(pract_choice ~ gender+comp_choice , data = clean, family = binomial())


sec_exploratory3 <- glm(pract_choice ~ gender+comp_choice, data = practice_cond, family = binomial())
sec_exploratory4 <- lm(task_score ~pract_choice + comp_choice + gender, data = clean)

sec_exploratory5 <- lm(task_score ~condition, data = clean)


sec_exploratory6 <- lm(practice_nonempty ~ gender + comp_choice, data = clean)


## gender predicting various outcomes by itself

sec_exploratory7 <- glm(comp_choice~gender, family = binomial, data = clean)
sec_exploratory8 <- lm(risk~gender, data = clean)
sec_exploratory9 <- lm(conf_rank~gender, data = clean)
sec_exploratory10 <- lm(task_score~gender*condition + conf_rank + risk, data = clean)

sec_exploratory11 <- glm(pract_choice ~ gender*comp_choice , data = clean, family = binomial())

sec_exploratory12 <- glm(extra_prep_count  ~ gender*comp_choice, family="poisson", data=clean)
sec_exploratory13 <- glm(pract_choice ~ gender*comp_choice + conf_rank + risk + task_score, data = clean, family = binomial())

sec_exploratory14 <- glm(comp_choice~gender*condition, family = binomial, data = clean)


t1a <- table(clean$gender, clean$condition)
sec_exploratory15 <- chisq.test(t1a)

sec_exploratory16 <- glm(pract_choice ~ gender, data = clean, family = binomial())

sec_exploratory17 <- hurdle(extra_prep_count ~ gender*comp_choice, data = clean)

## does choice to prepare &/or condition predict fatigue? - NOTE: no evidnce that women feel more fatigued 

sec_exploratory18 <- lm(fati ~ pract_choice*condition + gender, data = clean)


## does practice and/or gender, regardless of condition, predict feelings of preparedness? 

sec_exploratory19 <- glm(factor(preparedness) ~ pract_choice*gender + condition, family = "binomial",data = clean)


sec_exploratory20 <- lm(fab ~ gender, data = clean)

sec_exploratory21 <- lm(interest ~ pract_choice*condition + gender, data = clean)

sec_exploratory22 <- glm(comp_choice ~ conf_rank + risk, family = "binomial", data = clean)