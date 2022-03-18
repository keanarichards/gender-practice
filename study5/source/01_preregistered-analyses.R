# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here)

# load data ---------------------------------------------------------------

clean <- read_csv(here("study5", "data", "clean.csv"))

## recoding to factors for vars used in logits

clean$gender <- factor(clean$gender)
clean$condition <- factor(clean$condition)
clean$practice_problems_binary <- factor(clean$practice_problems_binary)

## filter out potentially fraudulent responses 

clean_fraud_removed <- clean %>% filter(fraud == 0) 

## dataset with only participants who chose to practice

practice <- clean_fraud_removed %>% filter(practice_problems_binary == "Yes") 

## dataset with only women

women <- clean_fraud_removed %>% filter(gender == "Woman") 

## dataset with only men

men <- clean_fraud_removed %>% filter(gender == "Man") 

# primary hypothesis 1: gender predicting choce to practice ----------------------------------------------------

hypothesis1  <- glm(practice_problems_binary~condition*gender, family = binomial, data = clean_fraud_removed)

# primary hypothesis 2: gender predicting perceived practice dev  ----------------------------------------------------

hypothesis2 <- lm(perceived_pract_dev ~ condition*gender,data = clean_fraud_removed)

# exploratory analysis 1a-b: gender predicting accuracy of perceived practice (aka relative to 0) --------------------------------------------------

## pulling code from here: http://www.sthda.com/english/wiki/one-sample-t-test-in-r

## for women

exploratory1a <- t.test(women$perceived_pract_dev, mu = 0, alternative = "two.sided") 

## for men

exploratory1b <-t.test(men$perceived_pract_dev_F, mu = 0, alternative = "two.sided") 

# exploratory analysis 2a-b: gender predicting accuracy of perceived practice relative to specific gender (when compared to 0) --------------------------------------------------

## for women

exploratory2a <- t.test(clean_fraud_removed$perceived_pract_dev_F, mu = 0, alternative = "two.sided") 

## relative to men

exploratory2b <-t.test(clean_fraud_removed$perceived_pract_dev_M, mu = 0, alternative = "two.sided") 

# exploratory analysis 3: testing attrition --------------------------------------------------

## to ID ppts who dropped out during vs. adter learning about cond,
## this would be if Finished == F AND condition == NA, that is they were assigned to a condition, THEN dropped out

## decided against this analysis because it is such a small proportion of ppts that it wouldn't make a diff. will just note it in methods. 
## code to show this: clean %>% filter(Finished == "FALSE" & !is.na(condition)) %>% select(Finished, condition)


# exploratory analysis 4a-c: testing other measures of preparation --------------------------------------------------

## number of practice problems attempted (aka nonempty)

exploratory4a <- glm(practice_nonempty ~ condition*gender,data = clean_fraud_removed,family="poisson")

## number of extra practice rounds completed 

exploratory4b <- glm(extra_practice_rounds_count ~ condition*gender,data = clean_fraud_removed,family="poisson")

## total_pract_time across all practice rounds

exploratory4c <- lm(total_pract_time ~ condition*gender,data = clean_fraud_removed)

## although pre-registered that we would test risk/conf as possible meds of expected interaction
## between gender & cond on pract in primary hyp 1, did not find effect, so did not run that analysis 

# exploratory analysis 5a-b: possible 3 way interaction between gender, cond & risk/conf on choice to practice probs --------------------------------------------------

exploratory5a <- glm(practice_problems_binary~condition*gender*risk, family = binomial, data = clean_fraud_removed)

exploratory5b <- glm(practice_problems_binary~condition*gender*conf_rank, family = binomial, data = clean_fraud_removed)
