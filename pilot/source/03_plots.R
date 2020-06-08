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


# secondary hypothesis 2: performance across rounds  ----------------------

round_scores <- clean %>% pivot_longer(cols = starts_with("round"), names_to = "round",
                                       values_to = "score",  names_pattern = "round_(.)_score") %>% select(id, round, score)

Scores_across_rounds1plot <- ggplot(round_scores, aes(round, score))
Scores_across_rounds1plot + geom_boxplot() + labs(x = "Round", y = "Score")