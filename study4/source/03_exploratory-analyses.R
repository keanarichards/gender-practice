# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here, conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
# load data ---------------------------------------------------------------

clean <- read_csv(here("study4", "data", "clean.csv"))

clean$study_tables_binary <- factor(clean$study_tables_binary)
clean$practice_problems_binary <- factor(clean$practice_problems_binary)
clean$comp_choice <- factor(clean$comp_choice)

## dropping fraud responses

clean_fraud_removed <- clean %>% filter(fraud == 0) 

## creating filtered versions of datasets

practice <- clean_fraud_removed %>% filter(condition == "pract")
no_fraud <- clean_fraud_removed %>% filter(fraud == 0)
women <- clean_fraud_removed %>% filter(gender == "Woman")
  

# analyses ----------------------------------------------------------------


## exploring other ways of measuring practice choice
 
exploratory1 <- lm(study_tables_time ~ gender*comp_choice, data=practice)


exploratory2 <- glm(study_tables_binary ~ gender*comp_choice, data = practice, family = binomial())

exploratory3 <- glm(practice_problems_binary ~ gender*comp_choice, data = practice, family = binomial())


## exploring follow-up questions 

## starting with gender diff questions

t1 <- table(clean_fraud_removed$better_gender_guess)
exploratory4 <- chisq.test(t1, p = c(1/3, 1/3, 1/3))

t2 <- table(clean_fraud_removed$perc_gender_comp)
exploratory5 <-chisq.test(t2, p = c(1/3, 1/3, 1/3))


t3 <- table(clean_fraud_removed$perc_gen_gender_pract)
exploratory6 <-chisq.test(t3,p = c(1/3, 1/3, 1/3))

## make sure to filter only ppts in practice cond
t4 <- table(practice %>% dplyr::select(perc_task_gender_pract))
exploratory7 <-chisq.test(t4, p = c(1/3, 1/3, 1/3))


## how useful do ppts find the practice? do they think multiplication practice will boost performance? 

t6 <- table(clean_fraud_removed  %>% select(MC))
exploratory9 <-chisq.test(t6, p = c(1/3, 1/3, 1/3))

## does condition  interact with gender to predict conf

exploratory10 <- lm(conf_rank ~ condition*gender, data = clean_fraud_removed)

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

exploratory16 <- glm(comp_choice~gender, family = binomial, data = clean_fraud_removed)
exploratory17 <- lm(risk~gender, data = clean_fraud_removed)
exploratory18 <- lm(conf_rank~gender, data = clean_fraud_removed)
exploratory19 <- lm(task_score~gender, data = clean_fraud_removed)
exploratory20 <- lm(task_score~gender*condition + conf_rank + risk, data = clean_fraud_removed)
exploratory21 <- glm(comp_choice ~ condition + task_score + risk + conf_rank,family=binomial,data = women)
exploratory22 <- glm(comp_choice ~ condition,family=binomial,data = clean_fraud_removed)
exploratory23 <- glm(comp_choice ~ condition*gender + task_score + risk + conf_rank,family=binomial,data = clean_fraud_removed)
exploratory24 <- glm(practice_problems_binary ~ condition,family=binomial,data = women)
exploratory25 <- glm(study_tables_binary ~ condition,family=binomial,data = women)
exploratory26 <- lm(conf_rank ~ condition,data = women)
exploratory27 <- lm(risk ~ condition,data = women)
exploratory28 <- glm(extra_practice_rounds_count ~ gender*comp_choice,family="poisson",data = practice)
exploratory29 <- glm(practice_problems_binary ~ gender*comp_choice + conf_rank + task_score + risk,family=binomial(),data = clean_fraud_removed)


# performing more targeted analysis of gender diff questions --------------

## left off at t6 above, so starting at t7

## better_gender_guess = expecting no diff is selected more than other response options, so need to check if no diff is sig higher than ppts saying men AND women as options 

## compared to men

t7 <- table(clean_fraud_removed  %>% filter(better_gender_guess != "Women" ) %>% dplyr::select(better_gender_guess))
exploratory30 <- chisq.test(t7, p = c(1/2, 1/2))


## compared to women

t8 <- table(clean_fraud_removed  %>% filter(better_gender_guess != "Men" ) %>% dplyr::select(better_gender_guess))
exploratory31 <- chisq.test(t8, p = c(1/2, 1/2))


## perc_task_gender_pract = expecting select women is selected more than other response options, so need to check if women is sig higher than ppts saying no diff AND men as options 
## make sure to filter only ppts in practice cond


## compared to no diff
t9 <- table(clean_fraud_removed  %>% filter(condition == "pract", perc_task_gender_pract != "Men" ) %>% dplyr::select(perc_task_gender_pract))
exploratory32 <- chisq.test(t9, p = c(1/2, 1/2))


## compared to men
t10 <- table(clean_fraud_removed  %>% filter(condition == "pract", perc_task_gender_pract != "No difference" ) %>% dplyr::select(perc_task_gender_pract))
exploratory33 <- chisq.test(t10, p = c(1/2, 1/2))



## perc_gen_gender_pract = expecting select women is selected more than other response options, so need to check if women is sig higher than ppts saying no diff AND men as options 

## compared to no diff
t11 <- table(clean_fraud_removed  %>% filter(perc_gen_gender_pract != "Men" ) %>% dplyr::select(perc_gen_gender_pract))
exploratory34 <- chisq.test(t11, p = c(1/2, 1/2))


## compared to men
t12 <- table(clean_fraud_removed  %>% filter(perc_gen_gender_pract != "No difference" ) %>% dplyr::select(perc_gen_gender_pract))
exploratory35 <- chisq.test(t12, p = c(1/2, 1/2))


## perc_gender_comp = expecting select men is selected more than other response options, so need to check if men is sig higher than ppts saying no diff AND women as options 

## compared to no diff
t13 <- table(clean_fraud_removed  %>% filter(perc_gender_comp != "Women" ) %>% dplyr::select(perc_gender_comp))
exploratory36 <- chisq.test(t13, p = c(1/2, 1/2))


## compared to women

t14 <- table(clean_fraud_removed  %>% filter(perc_gender_comp != "No difference" ) %>% dplyr::select(perc_gender_comp))
exploratory37 <- chisq.test(t14, p = c(1/2, 1/2))

## testing sig diff across cond

t15 <- table(clean_fraud_removed$gender, clean_fraud_removed$condition)
sec_exploratory38 <- chisq.test(t15)

sec_exploratory39 <- glm(practice_problems_binary ~ gender, data = clean_fraud_removed, family = binomial())