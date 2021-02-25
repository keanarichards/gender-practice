
# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "here", "pmr")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv(here("study3", "data", "clean.csv"))


# exploratory analysis 1 --------------------------------------------------

## testing whether there are gender diff in rankings 

## https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-13-65

men <- clean %>% filter(gender == "Man")
women <- clean %>% filter(gender == "Woman")


rankings_men <- men %>% dplyr::select(rank_explanations_2:rank_explanations_8) %>% filter(is.na(.) == FALSE)
rankings_women <- women %>% dplyr::select(rank_explanations_2:rank_explanations_8) %>% filter(is.na(.) == FALSE)

rankings_agg_men <- rankagg(rankings_men)
rankings_agg_women <- rankagg(rankings_women)

de_men <- destat(rankings_agg_men)
de_women <- destat(rankings_agg_women)

sec_exploratory1 <- chisq.test(cbind(as.vector(de_men$mar), as.vector(de_women$mar)))

