# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here, conflicted, easystats, pscl, magrittr)

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

sec_exploratory12 <- lm(task_score~gender, data = clean_fraud_removed)
sec_exploratory13  <- glm(practice_problems_binary~condition*gender + task_score + risk + conf_rank, family = binomial, data = clean_fraud_removed)


# performing more targeted analysis of gender diff questions --------------

## left off at t5 above, so starting at t6

## better_gender_guess = expecting no diff is selected more than other response options, so need to check if no diff is sig higher than ppts saying men AND women as options 

## compared to men

t6 <- table(clean_fraud_removed  %>% filter(better_gender_guess != "Women correctly solved more multiplication problems than men" ) %>% dplyr::select(better_gender_guess))
sec_exploratory14 <- chisq.test(t6, p = c(1/2, 1/2))


## compared to women

t7 <- table(clean_fraud_removed  %>% filter(better_gender_guess != "Men correctly solved more multiplication problems than women" ) %>% dplyr::select(better_gender_guess))
sec_exploratory15 <- chisq.test(t7, p = c(1/2, 1/2))


## perc_task_gender_pract = expecting select women is selected more than other response options, so need to check if women is sig higher than ppts saying no diff AND men as options 

## compared to no diff
t8 <- table(clean_fraud_removed  %>% filter( perc_task_gender_pract != "Men were more likely to practice/study than women" ) %>% dplyr::select(perc_task_gender_pract))
sec_exploratory16 <- chisq.test(t8, p = c(1/2, 1/2))


## compared to men
t9 <- table(clean_fraud_removed  %>% filter( perc_task_gender_pract != "There was no difference in how likely men and women were to practice/study" ) %>% dplyr::select(perc_task_gender_pract))
sec_exploratory17 <- chisq.test(t9, p = c(1/2, 1/2))



## perc_gen_gender_pract = expecting select women is selected more than other response options, so need to check if women is sig higher than ppts saying no diff AND men as options 

## compared to no diff
t10 <- table(clean_fraud_removed  %>% filter(perc_gen_gender_pract != "Men prepare more than women" ) %>% dplyr::select(perc_gen_gender_pract))
sec_exploratory18 <- chisq.test(t10, p = c(1/2, 1/2))


## compared to men
t11 <- table(clean_fraud_removed  %>% filter(perc_gen_gender_pract != "There is no difference in how much men and women prepare" ) %>% dplyr::select(perc_gen_gender_pract))
sec_exploratory19 <- chisq.test(t11, p = c(1/2, 1/2))


## perc_gender_comp_M = expecting select tournament is selected more than other response options, so need to check if tourn is sig higher than ppts saying no diff AND PR as options 

## NOTE: this was separated into two questions so need to repeat for each

## compared to no diff
t12 <- table(clean_fraud_removed  %>% filter(perc_gender_comp_M != "Men would choose piece rate more often than tournament" ) %>% dplyr::select(perc_gender_comp_M))
sec_exploratory20 <- chisq.test(t12, p = c(1/2, 1/2))


## compared to PR

t13 <- table(clean_fraud_removed  %>% filter(perc_gender_comp_M != "Men would choose each payment scheme equally" ) %>% dplyr::select(perc_gender_comp_M))
sec_exploratory21 <- chisq.test(t13, p = c(1/2, 1/2))



## perc_gender_comp_F = expecting select PR is selected more than other response options, so need to check if PR is sig higher than ppts saying no diff AND tournament as options 


## compared to no diff
t14 <- table(clean_fraud_removed  %>% filter(perc_gender_comp_F != "Women would choose tournament more often than piece rate" ) %>% dplyr::select(perc_gender_comp_F))
sec_exploratory22 <- chisq.test(t14, p = c(1/2, 1/2))


## compared to tournament

t15 <- table(clean_fraud_removed  %>% filter(perc_gender_comp_F != "Women would choose each payment scheme equally" ) %>% dplyr::select(perc_gender_comp_F))
sec_exploratory23 <- chisq.test(t15, p = c(1/2, 1/2))


## testing sig diff in condition assn 

t16 <- table(clean_fraud_removed$gender, clean_fraud_removed$condition)
sec_exploratory24 <- chisq.test(t16)

sec_exploratory25 <- glm(practice_problems_binary ~ gender, data = clean_fraud_removed, family = binomial())



# testing effects of outliers on gender task score model  -----------------


outliers <- check_outliers(clean_fraud_removed$task_score)

as.numeric(outliers)

clean_fraud_removed <- cbind(outliers, clean_fraud_removed)
clean_fraud_removed_outlier_dropped <- clean_fraud_removed %>% filter(outliers == 0)

## https://data.library.virginia.edu/getting-started-with-hurdle-models/

sec_exploratory26 <- hurdle(total_practice_rounds_count ~ gender*condition, data = clean_fraud_removed)
## storing as summary object to be able to extract p -values & betas 

obj<- summary(sec_exploratory26)


# looking at 3 way interaction between other variables --------------------

sec_exploratory27 <- lm(total_pract_time ~ condition*gender*conf_rank, data = clean_fraud_removed)
sec_exploratory28 <- lm(total_pract_time ~ condition*gender*risk, data = clean_fraud_removed)


sec_exploratory29 <- hurdle(practice_nonempty ~ condition*gender*conf_rank, data = clean_fraud_removed)
sec_exploratory30 <- hurdle(practice_nonempty ~ condition*gender*risk, data = clean_fraud_removed)

## creating version of dataset flagging people who don't finish to run chi square test. 

clean_fraud_removed %<>% mutate(dropped_out_post_condition = ifelse(Finished == "FALSE" & !is.na(condition), "Yes", "No"))

t17 <- table(clean_fraud_removed$gender, clean_fraud_removed$dropped_out_post_condition)
sec_exploratory31 <- chisq.test(t17)


sec_exploratory32 <- t.test(practice$perceived_pract_dev, mu = 0, alternative = "two.sided") 

sec_exploratory33 <- t.test(practice$perceived_pract_dev, mu = 0, alternative = "two.sided") 

## interaction is still null when looking at full dataset

sec_exploratory34 <- glm(practice_problems_binary ~ gender*condition,family=binomial,data = clean)

sec_exploratory35 <-glm(factor(practice_problems_binary) ~ gender*perc_task_gender_pract, data = clean_fraud_removed, family = binomial())

sec_exploratory36 <-glm(factor(practice_problems_binary) ~ gender*perc_gen_gender_pract, data = clean_fraud_removed, family = binomial())

sec_exploratory37 <- lm(task_score~gender*condition, data = clean_fraud_removed)
