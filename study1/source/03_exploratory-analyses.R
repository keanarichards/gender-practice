
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

clean <- read_csv(here("study1", "data", "clean.csv"))


# analyses ----------------------------------------------------------------

clean$pract_choice <- factor(clean$pract_choice)


sec_exploratory1 <- glm(pract_choice ~ gender+comp_choice , data = clean, family = binomial())


sec_exploratory2 <- glm(pract_choice ~ gender*comp_choice , data = clean, family = binomial())
