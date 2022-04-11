
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

sec_exploratory11 <- glm(comp_choice ~ gender*condition + task_score + risk + conf_rank,family=binomial,data = clean)
sec_exploratory12 <- lm(task_score ~ gender*condition + risk + conf_rank,data = clean)

sec_exploratory13 <- glm(total_review_count ~ gender*comp_choice , data = clean, family = "poisson")
sec_exploratory14 <- glm(pract_choice ~ gender*comp_choice+ task_score + risk + conf_rank, data = clean, family = binomial())

sec_exploratory15 <- glm(comp_choice ~ condition, data = clean, family = binomial())

t1a <- table(clean$gender, clean$condition)
sec_exploratory16 <- chisq.test(t1a)

sec_exploratory17 <- glm(pract_choice ~ gender, data = clean, family = binomial())

sec_exploratory18 <- hurdle(total_review_count ~ gender*condition, data = clean)

sec_exploratory19 <- glm(comp_choice ~ risk+conf_rank, data = clean, family = binomial())

sec_exploratory20 <- lm(task_score ~ gender*condition,data = clean)
