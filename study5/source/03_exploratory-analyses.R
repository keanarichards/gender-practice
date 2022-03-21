# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here, conflicted)

## declaring conflicts

conflict_prefer("filter", "dplyr")


# load data ---------------------------------------------------------------

clean <- read_csv(here("study5", "data", "clean.csv"))

clean$gender <- factor(clean$gender)
clean$condition <- factor(clean$condition)
clean$practice_problems_binary <- factor(clean$practice_problems_binary)

# creating filtered versions of datasets ----------------------------------

## filter out potentially fraudulent responses 

clean_fraud_removed <- clean %>% filter(fraud == 0) 

## dataset with only participants who chose to practice

practice <- clean_fraud_removed %>% filter(practice_problems_binary == "Yes") 

## dataset with only women

women <- clean_fraud_removed %>% filter(gender == "Woman") 

## dataset with only men

men <- clean_fraud_removed %>% filter(gender == "Man") 

# analyses -----------------------------------------------

## another way of testing hypothesis 2 - controlling for actual amount of practicing/decision to practice, do women tend to believe they practice more/less than others?

## for analysis with perceived practice deviation: why not just use their self-rated decile as the DV - & then control for actual practice number of problems practiced? interpretation would be: regardless of how much they practice, women tend to think they practice less than others
# because the problem with the perceived practice deviation is that there will be a lot of 0s, so not that much variation, whereas the self rated var by itself might have more variation
# repeat this with first two exploratory analyses (t tests)

sec_exploratory1 <- lm(perc_practice_all ~ condition*gender +practice_nonempty,data = clean_fraud_removed)

## looking only at ppts who chose to practice - both for deviation DV & raw relative practice var

sec_exploratory2a <- lm(perceived_pract_dev ~ condition*gender,data = practice)

sec_exploratory2b <- lm(perc_practice_all ~ condition*gender+practice_nonempty ,data = practice)

## perceptions of gender differences questions 

# exploratory analysis 3a: Do you think men or women in this study correctly solved more multiplication problems on average? --------------------------------------------------

t1 <- table(clean_fraud_removed$better_gender_guess)
sec_exploratory3a <- chisq.test(t1)

# exploratory analysis 3b: Do you think men or women in this study were more likely to choose to practice/study before completing the multiplication task? --------------------------------------------------

t2 <- table(clean_fraud_removed$perc_task_gender_pract)
sec_exploratory3b <-chisq.test(t2)

# exploratory analysis 3c: If given the opportunity to choose between the two payment schemes (Piece Rate or Tournament), do you think men in this study would choose the piece rate or the tournament payment scheme more often?  --------------------------------------------------

t3 <- table(clean_fraud_removed$perc_gender_comp_M)
sec_exploratory3c <-chisq.test(t3)


# exploratory analysis 3d: ditto above ? but for women  --------------------------------------------------

t4 <- table(clean_fraud_removed$perc_gender_comp_F)
sec_exploratory3d <-chisq.test(t4)


# exploratory analysis 3e: On most tasks, do you think men or women generally prepare (i.e., practice and/or study) more?  --------------------------------------------------

t5 <- table(clean_fraud_removed$perc_gen_gender_pract)
sec_exploratory3e <-chisq.test(t5)


## test if task scores differ between genders while controlling for other vars 

sec_exploratory4 <- lm(task_score~gender*condition+ risk+ conf_rank, data = clean_fraud_removed)

## with t t-test of how accurate people were in general.

sec_exploratory5 <- t.test(clean_fraud_removed$perceived_pract_dev, mu = 0, alternative = "two.sided") 

## looking at whether self-rated decile changes based on whether they are asked to compare themselves to men or women
## using within subject/paired t test since each ppt responds to both questions 

sec_exploratory6 <- t.test(clean_fraud_removed$perc_practice_women, clean_fraud_removed$perc_practice_men, paired = TRUE) 

## gender on risk & confidence 

sec_exploratory7 <- lm(risk~gender, data = clean_fraud_removed)
sec_exploratory8 <- lm(conf_rank~gender, data = clean_fraud_removed)


sec_exploratory9  <- glm(practice_problems_binary~risk, family = binomial, data = clean_fraud_removed)
sec_exploratory10  <- glm(practice_problems_binary~conf_rank, family = binomial, data = clean_fraud_removed)

sec_exploratory11  <- glm(practice_problems_binary~condition, family = binomial, data = clean_fraud_removed)


