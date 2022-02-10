
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

clean <- read_csv(here("study4", "data", "clean.csv"))

clean$study_tables_binary <- factor(clean$study_tables_binary)
clean$practice_problems_binary <- factor(clean$practice_problems_binary)
clean$comp_choice <- factor(clean$comp_choice)

## creating filtered versions of datasets

practice <- clean %>% filter(condition == "pract")
no_fraud <- clean %>% filter(fraud == 0)
women <- clean %>% filter(gender == "Woman")
  


# analyses ----------------------------------------------------------------


## exploring other ways of measuring practice choice
 
exploratory1 <- lm(study_tables_time ~ gender+comp_choice, data=practice)


exploratory2 <- glm(study_tables_binary ~ gender+comp_choice, data = practice, family = binomial())

exploratory3 <- glm(practice_problems_binary ~ gender+comp_choice, data = practice, family = binomial())


## exploring follow-up questions 

## starting with gender diff questions

t1 <- table(clean$better_gender_guess)
exploratory4 <- chisq.test(t1, p = c(1/3, 1/3, 1/3))

t2 <- table(clean$perc_gender_comp)
exploratory5 <-chisq.test(t2, p = c(1/3, 1/3, 1/3))


t3 <- table(clean$perc_gen_gender_pract)
exploratory6 <-chisq.test(t3,p = c(1/3, 1/3, 1/3))

## make sure to filter only ppts in practice cond
t4 <- table(clean %>% filter(condition == "pract") %>% select(perc_task_gender_pract))
exploratory7 <-chisq.test(t4, p = c(1/3, 1/3, 1/3))

## filtering question to see if there is a difference in perceptions between genders (aka exclude no diff option)

t5 <- table(clean %>% filter(condition == "pract", perc_task_gender_pract != "No difference" ) %>% select(perc_task_gender_pract))
exploratory8 <-chisq.test(t5, p = c(1/2, 1/2))

## how useful do ppts find the practice? do they think multiplication practice will boost performance? 

t6 <- table(clean  %>% select(MC))
exploratory9 <-chisq.test(t6, p = c(1/3, 1/3, 1/3))

## does condition  interact with gender to predict conf

exploratory10 <- lm(conf_rank ~ condition*gender, data = clean)

## does gender predict number of practice problems attempted (aka not left blank)
exploratory11 <- lm(practice_nonempty ~ gender *comp_choice, data = practice)


## check that main results hold after excluding ppts detected by qualtrics as possibly fraudulent 

no_fraud_women <- no_fraud %>% filter(gender == "Woman")

## effect of condition still holds

exploratory12 <- glm(comp_choice ~ condition, data = no_fraud_women, family = binomial())

## gender predicting practice choice & count, respectively

## null effect of gender on practice count still holds

exploratory13 <- glm(extra_practice_rounds_count ~ gender + task_score + risk + conf_rank,family="poisson",data = no_fraud)

## null effect gender on practice problem choice holds

exploratory14 <- glm(practice_problems_binary ~ gender + task_score + risk + conf_rank,family=binomial(),data = no_fraud)

## effects from before hold 

exploratory15 <- glm(comp_choice ~ gender*condition + task_score + risk + conf_rank,family=binomial,data = no_fraud)

exploratory16 <- glm(comp_choice~gender, family = binomial, data = clean)
exploratory17 <- lm(risk~gender, data = clean)
exploratory18 <- lm(conf_rank~gender, data = clean)
exploratory19 <- lm(task_score~gender, data = clean)
exploratory20 <- lm(task_score~gender + conf_rank + risk, data = clean)
exploratory21 <- glm(comp_choice ~ condition + task_score + risk + conf_rank,family=binomial,data = women)
exploratory22 <- glm(comp_choice ~ condition,family=binomial,data = clean)
exploratory23 <- glm(comp_choice ~ condition + task_score + risk + conf_rank,family=binomial,data = clean)
exploratory24 <- glm(practice_problems_binary ~ condition,family=binomial,data = women)
exploratory25 <- glm(study_tables_binary ~ condition,family=binomial,data = women)
exploratory26 <- lm(conf_rank ~ condition,data = women)
exploratory27 <- lm(risk ~ condition,data = women)
