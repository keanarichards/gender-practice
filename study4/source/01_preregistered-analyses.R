# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here)

# load data ---------------------------------------------------------------

clean <- read_csv(here("study4", "data", "clean.csv"))

## recoding to factors for vars used in logits

clean$gender <- factor(clean$gender)
clean$comp_choice <- factor(clean$comp_choice)
clean$condition <- factor(clean$condition)


## creating practice cond only dataset

practice <- clean %>% filter(condition == "pract")


# primary hypothesis 1 ----------------------------------------------------

prep_comp <- nrow(clean %>% filter(gender == "Woman" & condition == "pract" & comp_choice == "tournament"))
total_prep<- nrow(clean %>% filter(gender == "Woman" & condition == "pract"))

con_comp <- nrow(clean %>% filter(gender == "Woman" & condition == "control" & comp_choice == "tournament"))
total_con <- nrow(clean %>% filter(gender == "Woman" & condition == "control"))


## https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/

z.prop = function(x1,x2,n1,n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1+x2) / (n1+n2)
  denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
  z.prop.ris = numerator / denominator
  return(z.prop.ris)
}

hypothesis1_z <- z.prop(prep_comp, con_comp, total_prep, total_con)
hypothesis1_pval <- pnorm(hypothesis1_z)  


# primary hypothesis 2 ----------------------------------------------------

hypothesis2a <- glm(extra_practice_rounds_count ~ gender,family="poisson",data = practice)

hypothesis2b <- glm(extra_practice_rounds_count ~ gender + task_score,family="poisson",data = practice)

hypothesis2c <- glm(extra_practice_rounds_count ~ gender + task_score + risk + conf_rank,family="poisson",data = practice)

# primary hypothesis 3 ----------------------------------------------------

hypothesis3a <- glm(comp_choice ~ gender*condition,family=binomial,data = clean)

hypothesis3b <- glm(comp_choice ~ gender*condition + task_score,family=binomial,data = clean)

hypothesis3c <- glm(comp_choice ~ gender*condition + task_score + risk + conf_rank,family=binomial,data = clean)

