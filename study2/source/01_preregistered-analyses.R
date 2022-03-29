# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "mediation", "here")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv(here("study2", "data", "clean.csv"))

## recoding to factors for vars used in logits

clean$gender <- factor(clean$gender)
clean$comp_choice <- factor(clean$comp_choice)
clean$condition <- factor(clean$condition)

women <- clean %>% filter(gender == "Woman")


# primary hypothesis 1 ----------------------------------------------------

# prep_comp <- nrow(clean %>% filter(gender == "Woman" & condition == "pract" & comp_choice == "tournament"))
# total_prep<- nrow(clean %>% filter(gender == "Woman" & condition == "pract"))
# 
# con_comp <- nrow(clean %>% filter(gender == "Woman" & condition == "control" & comp_choice == "tournament"))
# total_con <- nrow(clean %>% filter(gender == "Woman" & condition == "control"))
# 
# 
# ## https://www.r-bloggers.com/comparison-of-two-proportions-parametric-z-test-and-non-parametric-chi-squared-methods/
# 
# z.prop = function(x1,x2,n1,n2){
#   numerator = (x1/n1) - (x2/n2)
#   p.common = (x1+x2) / (n1+n2)
#   denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
#   z.prop.ris = numerator / denominator
#   return(z.prop.ris)
# }
# 
# primary_hypothesis1_z <- z.prop(prep_comp, con_comp, total_prep, total_con)
# primary_hypothesis1_pval <- pnorm(primary_hypothesis1_z)  

hypothesis1  <- glm(comp_choice~condition, family = binomial, data = women)

# exploratory analysis 1 --------------------------------------------------

exploratory1 = glm(comp_choice ~ risk + conf_rank + task_score +gender*condition,family=binomial,data = clean)

# exploratory analysis 2 --------------------------------------------------

exploratory2 <- lm(bonus_task ~ gender + task_score + condition + gender*condition, data = clean)

# exploratory analysis 3a --------------------------------------------------

t1 <- table(clean$better_gender_guess)
exploratory3a <- chisq.test(t1)

# exploratory analysis 3b --------------------------------------------------

t2 <- table(clean$perc_gender_comp)
exploratory3b <-chisq.test(t2)

# exploratory analysis 3c --------------------------------------------------

t3 <- table(clean$perc_gen_gender_pract)
exploratory3c <-chisq.test(t3)


# exploratory analysis 3d -------------------------------------------------

t4 <- table(clean$perc_task_gender_pract)
exploratory3d <-chisq.test(t4)

# exploratory analysis 4 --------------------------------------------------

exploratory4 <- lm(task_score~ gender, data = clean)

# exploratory analysis 5 --------------------------------------------------

## did not run mediation model because condition does not predict competition choice 

exploratory5 <- glm(comp_choice ~ condition, family = binomial,data = clean)

# exploratory analysis 6 --------------------------------------------------

exploratory6 <-lm(task_score~condition, data = clean)

# exploratory analysis 7 --------------------------------------------------

# exploratory7 <-lm(total_time~condition, data = clean)

# exploratory analysis 8 --------------------------------------------------

exploratory8 <- glm(extra_prep_count ~ gender + condition + fab + gender*fab,family="poisson",data = clean)

# exploratory analysis 9 --------------------------------------------------

# exploratory9 <- lm(perf_extra_prep~interest + fati + gender + condition, data = clean)


# exploratory analysis 10 --------------------------------------------------


prep_M <- nrow(clean %>% filter(gender == "Man" & preparedness == "Yes"))

total_Mprep<- nrow(clean %>% filter(gender == "Man"))


prep_F <- nrow(clean %>% filter(gender == "Woman" & preparedness == "Yes"))

total_Fprep<- nrow(clean %>% filter(gender == "Woman"))

# exploratory10 <- z.prop(prep_M, prep_F, total_Mprep, total_Fprep)

# exploratory analysis 11 --------------------------------------------------

## IV on M
# sub <- clean %>% dplyr::select(bonus_task, gender, perf_extra_prep) %>% na.omit(.)
# 
# medModel <- lm(perf_extra_prep ~ gender,  data = sub)


## IV & M on DV


# outModel <- lm(bonus_task~ gender + perf_extra_prep, data = sub)
# 
# exploratory11 <- mediate(model.m = medModel, model.y = outModel, treat = "gender", mediator = "perf_extra_prep", data = sub)


# exploratory analysis 12 --------------------------------------------------

# 
# exploratory12 <- glm(comp_choice~ perf_fixed_rounds, family = binomial, data = clean)
# 
# exp(cbind(OR = coef(exploratory12), confint(exploratory12)))

# exploratory analysis 13 --------------------------------------------------

# exploratory13 <- lm(perf_extra_prep~comp_choice+ gender + gender*comp_choice, data = clean)




