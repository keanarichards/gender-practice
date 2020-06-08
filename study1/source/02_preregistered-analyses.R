# load packages -----------------------------------------------------------

## Package names
packages <- c("readr", "tidyverse", "ez")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv("study1/data/clean.csv")

## recoding to factors for vars used in logits

clean$gender <- factor(clean$gender)
clean$comp_choice <- factor(clean$comp_choice)
clean$condition <- factor(clean$condition)
clean$pract_choice <- factor(clean$pract_choice)

# primary hypothesis 1 ----------------------------------------------------

primary_hyp1 = glm(comp_choice ~ condition*gender,family=binomial,data = clean)
summary(primary_hyp1)

exp(cbind(OR = coef(primary_hyp1), confint(primary_hyp1)))

# primary hypothesis 2 --------------------------------------------------

primary_hyp2 = glm(pract_choice ~ gender,family=binomial,data = clean)
summary(primary_hyp2)

exp(cbind(OR = coef(primary_hyp2), confint(primary_hyp2)))


# primary hypothesis 3 --------------------------------------------------


primary_hyp3 <- glm(total_review_count ~ gender, family="poisson", data=clean)
summary(primary_hyp3)

exp(cbind(OR = coef(primary_hyp3), confint(primary_hyp3)))


# primary hypothesis 4 --------------------------------------------------

t1 <- table(clean$pract_choice, clean$gender)

chisq.test(t1)

# exploratory analysis 1 --------------------------------------------------

exploratory1 = glm(comp_choice ~ gender+ risk + conf_rank,family=binomial,data = clean)

summary(exploratory1)

# exploratory analysis 2 --------------------------------------------------

exploratory2 = glm(comp_choice ~ gender+ risk + conf_rank + condition + gender*condition,family=binomial,data = clean)

summary(exploratory2)

# exploratory analysis 3 --------------------------------------------------

exploratory3 <- lm(conf_rank~total_review_count, data = clean)
summary(exploratory3)

# exploratory analysis 4 --------------------------------------------------

exploratory4 = glm(comp_choice ~ total_review_count+gender,family=binomial,data = clean)
summary(exploratory4)


# exploratory analysis 5 --------------------------------------------------

## Calc1: I think use a calculator would help you answer the multiplication questions... 

t2 <- table(clean$calc1, clean$gender)
chisq.test(t2)

## Calc2: Did you use a calculator to complete the multiplication task (note: this will not affect your payment)?

t3 <- table(clean$calc2, clean$gender)
chisq.test(t3)


## Calc3: Do you think other participants used a calculator to complete the multiplication task?

t4 <- table(clean$calc3, clean$gender)
chisq.test(t4)


## Calc4: I think using a calculator would help others answer the multiplication questions...

t5 <- table(clean$calc4, clean$gender)
chisq.test(t5)


## Calc5: Do you think men or women might have used a calculator more often for the multiplication task?

t6 <- table(clean$calc5, clean$gender)
chisq.test(t6)


# exploratory analysis 6 --------------------------------------------------

## Calc1: I think use a calculator would help you answer the multiplication questions... 

t7 <- table(clean$calc1, clean$pract_choice)
chisq.test(t7)



## Calc2: Did you use a calculator to complete the multiplication task (note: this will not affect your payment)?
t8 <- table(clean$calc2, clean$pract_choice)
chisq.test(t8)


## Calc3: Do you think other participants used a calculator to complete the multiplication task?
t9 <- table(clean$calc3, clean$pract_choice)
chisq.test(t9)


## Calc4: I think using a calculator would help others answer the multiplication questions...

t10 <- table(clean$calc4, clean$pract_choice)
chisq.test(t10)


## Calc5: Do you think men or women might have used a calculator more often for the multiplication task?

t11 <- table(clean$calc5, clean$pract_choice)
chisq.test(t11)


# exploratory analysis 7 --------------------------------------------------

t12 <- table(clean$better_gender_guess)
chisq.test(t12)

t13 <- table(clean$perc_gen_gender_pract)
chisq.test(t13)

t14 <- table(clean$perc_task_gender_pract)
chisq.test(t14)

t15 <- table(clean$perc_gender_comp)
chisq.test(t15)
