
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

clean <- read_csv(here("study2", "data", "clean.csv"))


# analyses ----------------------------------------------------------------

clean$pract_choice <- factor(clean$pract_choice)

sec_exploratory1 <- glm(extra_prep_count  ~ gender+comp_choice, family="poisson", data=clean)


sec_exploratory2 <- glm(pract_choice ~ gender+comp_choice , data = clean, family = binomial())

practice_cond <- clean %>% filter(condition == "pract")

sec_exploratory3 <- glm(pract_choice ~ gender+comp_choice, data = practice_cond, family = binomial())
