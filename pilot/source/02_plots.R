# load packages -----------------------------------------------------------


## Package names
packages <- c("readr", "tidyverse", "jtools", "here")

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

clean <- read_csv(here::here("pilot", "data", "clean.csv"))

# primary hypothesis 1 ----------------------------------------------------

dat <- dplyr::select(clean, gender, comp_choice) %>%
  dplyr::count(gender, comp_choice) %>% filter(gender == "Woman") %>% mutate(percent = n / sum(n),
  error = sqrt((percent * (1-percent)) / n))

dat1 <- dplyr::select(clean, gender, comp_choice) %>%
  dplyr::count(gender, comp_choice) %>% filter(gender == "Man") %>% mutate(percent = n / sum(n),
  error = sqrt((percent * (1 -percent)) / n))

dat <- rbind(dat1, dat)


dat <- dat %>% filter (comp_choice == "tournament")


p <- ggplot(data = dat, aes(x = comp_choice, fill = gender)) +
  geom_bar(aes(y = percent * 100),
  position = "dodge", stat = "identity") + geom_errorbar(aes(
  ymin = (percent * 100) - (error * 100),
  ymax = (percent * 100) + (error * 100)),
  width = .05,
  position =
  position_dodge(.9)) +
  geom_text(x = 1.5, y = 100, label = "***") +
  labs(x = 'Choice to compete', y = '% Competing') + scale_fill_manual(values =
  c("springgreen3", "slateblue1")) + theme_apa()

ggsave(here("pilot", "figs", "fig00_comp-choice-by-gender-bar.png"), p, width = 7, height = 7)

# secondary hypothesis 1 --------------------------------------------------





# secondary hypothesis 2: performance across rounds  ----------------------

# round_scores <-
  #clean %>% pivot_longer(
    #cols = starts_with("round"),
    #names_to = "round",
    #values_to = "score",
    #names_pattern = "round_(.)_score") %>% select(id, round, score)

#Scores_across_rounds1plot <- ggplot(round_scores, aes(round, score))
#Scores_across_rounds1plot + geom_boxplot() + labs(x = "Round", y = "Score")



# exploratory 1 -----------------------------------------------------------

dat <- dplyr::select(clean,gender, pract_choice) %>%
  dplyr::count(gender,pract_choice) %>% filter(gender == "Woman") %>% mutate(percent = n / sum(n),
                                                                                           error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean,gender,pract_choice) %>%
  dplyr::count(gender, pract_choice) %>% filter(gender == "Man") %>% mutate(percent = n / sum(n),
                                                                                         error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat)

dat <- dat %>% filter (pract_choice == "Yes")


p <- ggplot(data = dat, aes(x = pract_choice, fill = gender))  + 
  labs(y = 'Would you take the opportunity to practice? (%)')  + scale_y_continuous(limits = c(0, 100)) + 
  geom_bar(aes(y = percent*100,
               position = "dodge", stat = "identity"),     position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                                            position=position_dodge(.9)) + theme_apa()

ggsave(here("pilot", "figs", "fig01_pract-choice-by-gender-bar.png"), p, width = 7, height = 7)

