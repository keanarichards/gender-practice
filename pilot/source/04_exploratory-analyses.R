
# load packages -----------------------------------------------------------

## Package names
packages <- c("readr", "tidyverse")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv("pilot/data/clean.csv")



# analyses ----------------------------------------------------------------


## does gender predict risk?

exploratory1 <- lm(risk ~ Gender, data = practice_pilot)
summary(exploratory1)


## does gender predict willingness to practice?

clean$choice_pract <- factor(clean$choice_pract)

exploratory2 <- glm (choice_pract ~gender , data = clean, family = binomial())
summary(exploratory2)

## does gender predict number of preferred minutes to practice?

exploratory3 <- lm(min_pract ~ gender, data = clean)
summary(exploratory3)


## does gender predict confidence? 

clean$conf_partner <- factor(clean$conf_partner)
exploratory4 <- glm (conf_partner ~ gender , data = clean, family = binomial())
summary(exploratory4)

## does gender predict whether they thought they would improve?

clean$improve_pract<- factor(clean$improve_pract)
exploratory5 <- glm (improve_pract ~ gender, data = clean, family = binomial())
summary(exploratory5)
