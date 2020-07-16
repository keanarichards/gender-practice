
# load packages -----------------------------------------------------------

## Package names
packages <- c("here", "tidyverse", "ez")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv(here("pilot", "data", "clean.csv"))

# primary hypothesis 1 ----------------------------------------------------

## logistic regression to test hypothesis that gender predicts willingness to 
## compete in round 3 (where participants have the choice to compete)


clean$comp_choice <- factor(clean$comp_choice)


primary_hyp1 <- glm (comp_choice ~ gender, data = clean, family = binomial())


# secondary hypothesis 1 --------------------------------------------------

## logistic regressin to test whether confidence and risk aversion predict choice to compete over and above effect of gender

secondary_hyp1 <- glm (comp_choice ~ gender + conf_partner+ risk, data = clean, family = binomial())


# secondary hypothesis 2 --------------------------------------------------

clean %>% summarize(mean1 = mean(round_1_score), mean2= mean(round_2_score), mean3 = mean(round_3_score))

## repeated measures anova to test hypothesis that round number predicts performance (i.e., performance improves across rounds)


round_scores <- clean %>% pivot_longer(cols = starts_with("round"), names_to = "round",
                                       values_to = "score",  names_pattern = "round_(.)_score") %>% select(id, round, score)


Scores_across_roundsModel<-ezANOVA(data = round_scores, dv = .(score), wid = .(id),  within = .(round), type = 3, detailed = TRUE)

