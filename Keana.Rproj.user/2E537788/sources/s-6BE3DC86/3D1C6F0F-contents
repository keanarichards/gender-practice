
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

clean <- read_csv(here("pilot", "data", "clean.csv"))


# analyses ----------------------------------------------------------------

clean$pract_choice <- factor(clean$pract_choice)

## does gender predict hypothetical willingness to practice? 

sec_exploratory1 <- glm(pract_choice ~ gender , data = clean, family = binomial())

tbl <- table(clean$gender, clean$comp_choice)

## redoing primary hypothesis 1 as chi-square test (is gender independent from decision to compete)

sec_exploratory2 <- chisq.test(tbl)

men <- clean %>% filter(gender == "Man")
women <- clean %>% filter(gender == "Woman")

## comparing men and women's scores

sec_exploratory3 <- t.test(x = men$overall_score,y = women$overall_score)

t1 <- table(clean$better_gender_guess)
t2 <- table(clean$gender, clean$better_gender_guess)

## is there a difference in proportion of people who thought men or women performed better? -- also is there a gender is these perceptions? 

sec_exploratory4= chisq.test(t1)
sec_exploratory5= chisq.test(t2)

t3 <- table(clean$improve_pract)

t4 <- table(clean$gender, clean$improve_pract)

## is there a difference in proportion of people who thought their score would improve if they practiced? -- also is there a gender is these perceptions? 

sec_exploratory6= chisq.test(t3)
sec_exploratory7= chisq.test(t4)

## is there a gender difference in the number of minutes participants wanted to practice?

sec_exploratory8 <- t.test(x = men$min_pract,y = women$min_pract)



