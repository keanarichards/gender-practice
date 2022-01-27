
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


# analyses ----------------------------------------------------------------

clean$study_tables_binary <- factor(clean$study_tables_binary)
clean$practice_problems_binary <- factor(clean$practice_problems_binary)


## exploring other ways of measuring practice choice
 
exploratory1 <- lm(study_tables_time ~ gender*comp_choice, data=clean)


exploratory2 <- glm(study_tables_binary ~ gender*comp_choice, data = clean, family = binomial())

exploratory3 <- glm(practice_problems_binary ~ gender*comp_choice, data = clean, family = binomial())


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
