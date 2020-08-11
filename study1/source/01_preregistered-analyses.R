# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "ez", "here")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv(here("study1", "data", "clean.csv"))

## recoding to factors for vars used in logits

clean$gender <- factor(clean$gender)
clean$comp_choice <- factor(clean$comp_choice)
clean$condition <- factor(clean$condition)
clean$pract_choice <- factor(clean$pract_choice)

## note: for all analyses, the time spent preparing data is not available, so we will replace with choice to prepare when possible
 
# primary hypothesis 1 ----------------------------------------------------

primary_hyp1 = glm(comp_choice ~ condition*gender,family=binomial,data = clean)

# primary hypothesis 2 --------------------------------------------------

primary_hyp2 = glm(pract_choice ~ gender + comp_choice,family=binomial,data = clean)

# primary hypothesis 3 --------------------------------------------------

## conducted poisson regression instead of originally planned linear regression because of nature of data (count)
primary_hyp3 <- glm(total_review_count ~ gender, family="poisson", data=clean)


# primary hypothesis 4 --------------------------------------------------

t1 <- table(clean$pract_choice, clean$gender)
primary_hyp4 = chisq.test(t1)

# exploratory analysis 1 --------------------------------------------------

exploratory1 = glm(comp_choice ~ gender+ risk + conf_rank,family=binomial,data = clean)


# exploratory analysis 2 --------------------------------------------------

exploratory2 = glm(comp_choice ~ gender+ risk + conf_rank + condition + gender*condition,family=binomial,data = clean)

# exploratory analysis 3a -------------------------------------------------
## accounting for typo on prereg doc - cannot analyze because there is no time information


# exploratory analysis 3b --------------------------------------------------

exploratory3b <- lm(conf_rank~total_review_count, data = clean)

# exploratory analysis 4 --------------------------------------------------

exploratory4 = glm(comp_choice ~ total_review_count+gender,family=binomial,data = clean)


# exploratory analysis 5 --------------------------------------------------

## Calc1: I think use a calculator would help you answer the multiplication questions... 

t2 <- table(clean$calc1, clean$gender)
exploratory5a = chisq.test(t2)

## Calc2: Did you use a calculator to complete the multiplication task (note: this will not affect your payment)?
t3 <- table(clean$calc2, clean$gender)
exploratory5b = chisq.test(t3)

## Calc3: Do you think other participants used a calculator to complete the multiplication task?

t4 <- table(clean$calc3, clean$gender)
exploratory5c = chisq.test(t4)

## Calc4: I think using a calculator would help others answer the multiplication questions...

t5 <- table(clean$calc4, clean$gender)
exploratory5d = chisq.test(t5)

## Calc5: Do you think men or women might have used a calculator more often for the multiplication task?

t6 <- table(clean$calc5, clean$gender)
exploratory5e = chisq.test(t6)

# exploratory analysis 6 --------------------------------------------------

## Calc1: I think use a calculator would help you answer the multiplication questions... 

t7 <- table(clean$calc1, clean$pract_choice)
exploratory6a= chisq.test(t7)



## Calc2: Did you use a calculator to complete the multiplication task (note: this will not affect your payment)?
t8 <- table(clean$calc2, clean$pract_choice)
exploratory6b= chisq.test(t8)


## Calc3: Do you think other participants used a calculator to complete the multiplication task?
t9 <- table(clean$calc3, clean$pract_choice)
exploratory6c= chisq.test(t9)


## Calc4: I think using a calculator would help others answer the multiplication questions...

t10 <- table(clean$calc4, clean$pract_choice)
exploratory6d= chisq.test(t10)


## Calc5: Do you think men or women might have used a calculator more often for the multiplication task?

t11 <- table(clean$calc5, clean$pract_choice)
exploratory6e= chisq.test(t11)


# exploratory analysis 7 --------------------------------------------------

t12 <- table(clean$better_gender_guess)
exploratory7a= chisq.test(t12)


t13 <- table(clean$perc_gender_comp)
exploratory7b= chisq.test(t13)

t14 <- table(clean$perc_gen_gender_pract)
exploratory7c= chisq.test(t14)

t15 <- table(clean$perc_task_gender_pract)
exploratory7c= chisq.test(t15)

