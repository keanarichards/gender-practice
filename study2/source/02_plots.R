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

# load data ---------------------------------------------------------------

clean <- read_csv(here("study2", "data", "clean.csv"))

# primary hypothesis 1 ----------------------------------------------------

dat <- dplyr::select(clean,gender,comp_choice, condition) %>%
  dplyr::count(gender, comp_choice, condition) %>% filter(gender == "Woman") %>% mutate(percent = n / sum(n),
  error = sqrt((percent * (1-percent))/n))

dat <- dat %>% filter (comp_choice == "tournament")


p <- ggplot(data = dat, aes(x = condition)) +
  geom_bar(aes(y = percent*100),
  position = "dodge", stat = "identity", fill = "slateblue1") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Condition', y = '% Competing') + theme_apa()

ggsave(here("study2", "figs", "fig00_comp-choice-women-by-cond.png"), width = 7, height = 7)


# exploratory analysis 3a ---------------------------------------------------

dat <- dplyr::select(clean, better_gender_guess) %>%
  dplyr::count(better_gender_guess) %>%  mutate(percent = n / sum(n),
                                                error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = better_gender_guess, fill = better_gender_guess))  + 
  labs(x = 'Gender', y = 'Who peforms better? (%)')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = F)+
  geom_bar(aes(y = percent*100, position = "dodge", stat = "identity"), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                                                         position=position_dodge(.9)) + theme_apa()

ggsave(here("study2", "figs", "fig01_better-gender-guess.png"), p, width = 7, height = 7)

# exploratory analysis 3b ---------------------------------------------------

dat <- dplyr::select(clean, perc_gender_comp) %>%
  dplyr::count(perc_gender_comp) %>%  mutate(percent = n / sum(n),
                                             error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp, fill = perc_gender_comp))  + 
  labs(x = 'Gender', y = 'Who competes more? (%)')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = F)+
  geom_bar(aes(y = percent*100,
               position = "dodge", stat = "identity"),     position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                                            position=position_dodge(.9)) + theme_apa()

ggsave(here("study2", "figs", "fig02_perc-gender-comp.png"), p, width = 7, height = 7)




# exploratory analysis 3c ---------------------------------------------------

dat <- dplyr::select(clean, perc_gen_gender_pract) %>% filter(!is.na(perc_gen_gender_pract)) %>% dplyr::count(perc_gen_gender_pract) %>%  mutate(percent = n / sum(n),
                                                                                                                                                 error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = perc_gen_gender_pract))  + 
  labs(x = 'Gender', y = 'Who prepares more (in general)? (%)')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = F)+
  geom_bar(aes(y = percent*100,
               position = "dodge", stat = "identity"),     position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                                            position=position_dodge(.9)) + theme_apa()

ggsave(here("study2", "figs", "fig03_perc-gen-gender-pract.png"), p, width = 7, height = 7)

