
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

## Install the stable development versions from GitHub
devtools::install_github("crsh/papaja")

library(papaja)


# load data ---------------------------------------------------------------

clean <- read_csv(here("pilot/data/clean.csv"))



# analyses ----------------------------------------------------------------

clean$pract_choice <- factor(clean$pract_choice)


exploratory1 <- glm (pract_choice ~ gender , data = clean, family = binomial())

