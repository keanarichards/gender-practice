# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "mediation", "here", "pmr", "broom", "stats", "XNomial")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv(here("study3", "data", "clean.csv"))

## recoding to factors for vars used in logits

clean$gender <- factor(clean$gender)
clean$condition <- factor(clean$condition)

## filtering out gender == "Other" for analyses involving gender

gender_filter <- clean %>% filter(gender != "Other") 

# primary hypothesis 1 ----------------------------------------------------

primary_hyp1 <- lm(subj_confidence ~ gender + condition + gender*condition, data = gender_filter)

# exploratory analysis 1 --------------------------------------------------

exploratory1 = lm(task_score ~ gender + condition + gender*condition,data = gender_filter)

p1 <- tidy(exploratory1)$p.value[2:4]

# exploratory analysis 2 --------------------------------------------------

exploratory2 <- lm(abs_confidence ~ gender + condition + gender*condition,data = gender_filter)

p2 <- tidy(exploratory2)$p.value[2:4]

# exploratory analysis 3 --------------------------------------------------

men <- clean %>% filter(gender == "Man")
women <- clean %>% filter(gender == "Woman")

exploratory3 <- t.test(men$interest, women$interest)
p3 <- tidy(exploratory3)$p.value

# exploratory analysis 4 --------------------------------------------------

t1 <- table(clean$better_gender_guess)
exploratory4 <- chisq.test(t1)
p4 <- tidy(exploratory4)$p.value


# exploratory analysis 5 --------------------------------------------------

t2 <- table(clean$math_gender_prep)
exploratory5 <-chisq.test(t2)
p5 <- tidy(exploratory5)$p.value

# exploratory analysis 6 --------------------------------------------------

t3 <- table(clean$gen_gender_prep)

## will need to use fisher's exact because count of people who chose men is less than 2 

obs <- as.numeric(as.matrix(t3))
p = 1/3
N = sum(t3)
E = N * p
E <- rep(E, 3)
exploratory6 <-xmulti(obs, E)
p6 <- exploratory6$pChi

# exploratory analysis 7 --------------------------------------------------

exploratory7 <- t.test(men$task_score, women$task_score)
p7 <- tidy(exploratory7)$p.value

# exploratory analysis 8 --------------------------------------------------

control <- clean %>% filter(condition == "control")
pract <- clean %>% filter(condition == "pract")

exploratory8 <- t.test(control$abs_confidence, pract$abs_confidence)
p8 <- tidy(exploratory8)$p.value


# exploratory analysis 9 --------------------------------------------------

## did not run mediation model because condition does not predict task score  

exploratory9 <- lm(task_score ~ condition, data = clean)

p9 <- tidy(exploratory9)$p.value[2]

# exploratory analysis 10 --------------------------------------------------

exploratory10 <- t.test(control$total_time, pract$total_time)
p10 <- tidy(exploratory10)$p.value

# exploratory analysis 11 --------------------------------------------------

exploratory11 <- t.test(men$control_prep_beliefs, women$control_prep_beliefs)
p11 <- tidy(exploratory11)$p.value

# exploratory analysis 12 --------------------------------------------------

## https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-13-65

rankings <- clean %>% dplyr::select(rank_explanations_2:rank_explanations_8) %>% filter(is.na(.) == FALSE)

rankings_agg <- rankagg(rankings)

de1 <- destat(rankings_agg)

## expected value of mean rank = (k+1)/2 

mean <- rep(3, 5)

## formula = 12*(nrow(clean))*sum((de1$mean.rank - mean)^2)/(k*(k+1))

chi <- 12*(nrow(clean))*sum((de1$mean.rank - mean)^2)/(5*(5+1))

## df = k (number of rankings) -1 

p12 <- dchisq(chi, 4)


## benjamini-hochberg correction:

p <- c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
p <- p.adjust(p, method = "BH")


