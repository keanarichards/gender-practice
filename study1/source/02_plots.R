# load packages -----------------------------------------------------------


## Package names
packages <- c("tidyverse", "jtools", "here")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# load data ---------------------------------------------------------------

clean <- read_csv(here("study1", "data", "clean.csv"))

# primary hypothesis 1 ----------------------------------------------------

dat <- dplyr::select(clean,gender,comp_choice, condition) %>% group_by(condition, gender) %>%  dplyr::count(gender, comp_choice, condition) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(comp_choice == "tournament")


p <- ggplot(data = dat, aes(x = condition, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Condition', y = 'Percentage Competing') +
  scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + theme_apa() +
  scale_x_discrete(labels = c("Control", "Prepare")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study1", "figs", "fig00_comp-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)

# primary hypothesis 2 ----------------------------------------------------


dat <- dplyr::select(clean,gender,comp_choice, pract_choice) %>%
  dplyr::count(gender, comp_choice, pract_choice) %>% filter(gender == "Woman", pract_choice == "Yes") %>% mutate(percent = n / sum(n),
  error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean,gender,comp_choice, pract_choice) %>%
  dplyr::count(gender, comp_choice, pract_choice) %>% filter(gender == "Man", pract_choice == "Yes") %>% mutate(percent = n / sum(n),
  error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat)

p <- ggplot(data = dat, aes(x = comp_choice, fill = gender)) +
  geom_bar(aes(y = percent*100),
  position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Choice to compete', y = 'Percentage Practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Piece-rate", "Tournament"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study1", "figs", "fig01_pract-choice-by-gender-and-comp-choice-bar.png"), p, width = 7, height = 7)


## for nsf app with caption attached 
p <- p + labs (caption = "Figure 1. Proportion of participants who chose to prepare based on \n participant gender and choice to compete from first study. \n Error bars represent standard error.") + theme(
  plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14)
)

ggsave(here("nsf-application", "nsf1.png"), p, width = 7, height = 7)


# primary hypothesis 3 ----------------------------------------------------


lb <- function(x){mean(x) - sd(x)/sqrt((count(clean)))}
ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean)))} 


clean$logtotal_rev_count <- log10(clean$total_review_count +1)

## calculating mean & SEM within gender and comp choice
## first have to remove NA participants
clean <- clean %>% filter(!is.na(comp_choice))

sumld <- clean %>% 
  select(logtotal_rev_count, gender, comp_choice) %>% 
  group_by(gender, comp_choice) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))

p <- ggplot(data = sumld, aes(x = comp_choice, fill = gender)) +
  geom_bar(aes(y = mean),
  position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
  position=position_dodge(.9)) + 
  labs(x = "Competition choice", y = 'Average (log) rounds of extra practice') + theme_apa() +
  scale_fill_manual(values=c("springgreen3", "slateblue1"), labels = c("Men", "Women"))+
  scale_x_discrete(labels = c("Piece-rate", "Tournament")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study1", "figs", "fig02_total-rev-count-by-gender-comp-choice.png"), p, width = 7, height = 7)


# primary hypothesis 4 ----------------------------------------------------

dat <- dplyr::select(clean, perc_task_gender_pract) %>% drop_na(perc_task_gender_pract) %>% 
  dplyr::count(perc_task_gender_pract) %>%  mutate(percent = n / sum(n),
  error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = perc_task_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men prepare more for the task", "Women prepare more for the task"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 

ggsave(here("study1", "figs", "fig03_perc-task-gender-pract.png"), p, width = 7, height = 7)


# exploratory analysis 7a -------------------------------------------------


dat <- dplyr::select(clean, better_gender_guess) %>% drop_na(better_gender_guess) %>% 
  dplyr::count(better_gender_guess) %>%  mutate(percent = n / sum(n),
                                                   error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = better_gender_guess, fill = better_gender_guess))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men performed better on the task", "Women performed better on the task"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 


ggsave(here("study1", "figs", "fig04_better-gender-guess.png"), p, width = 7, height = 7)

# exploratory analysis 7b -------------------------------------------------

dat <- dplyr::select(clean, perc_gender_comp) %>% drop_na(perc_gender_comp) %>% 
  dplyr::count(perc_gender_comp) %>%  mutate(percent = n / sum(n),
                                                error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp, fill = perc_gender_comp))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men compete more", "Women compete more"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 


ggsave(here("study1", "figs", "fig05_perc-gender-comp.png"), p, width = 7, height = 7)


# exploratory analysis 7c -------------------------------------------------


dat <- dplyr::select(clean, perc_gen_gender_pract) %>% drop_na(perc_gen_gender_pract) %>% 
  dplyr::count(perc_gen_gender_pract) %>%  mutate(percent = n / sum(n),
                                             error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = perc_gen_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men prepare more in general", "Women prepare more in general"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 


ggsave(here("study1", "figs", "fig06_perc-gen-gender-pract.png"), p, width = 7, height = 7)

## showing main effect of gender on choice to pract

dat <- dplyr::select(clean,gender, pract_choice)  %>% na.omit(pract_choice) %>% 
  dplyr::count(gender, pract_choice) %>% filter(gender == "Woman") %>% mutate(percent = n / sum(n),
                                                                                          error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean,gender,pract_choice)%>% na.omit(pract_choice) %>% 
  dplyr::count(gender, pract_choice) %>% filter(gender == "Man") %>% mutate(percent = n / sum(n),
                                                                                        error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat) %>% filter (pract_choice == "Yes")

p <- ggplot(data = dat, aes(x = gender, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Gender', y = 'Percentage practicing') + 
  theme_apa()+theme(legend.position = "none") + guides (fill = "none") +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +scale_fill_manual(values=c("springgreen3", "slateblue1"))

ggsave(here("study1", "figs", "fig07_pract-choice-by-gender.png"), p, width = 7, height = 7)

## showing main effect of gender on extra practice rounds


lb <- function(x){mean(x) - sd(x)/sqrt((count(clean)))}
ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean)))}


sumld <- clean %>%
  dplyr::select(total_review_count, gender) %>% group_by(gender) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))

p <- ggplot(data = sumld, aes(x = gender, fill= gender)) +
  geom_bar(aes(y = mean),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
                                                                  position=position_dodge(.9)) +
  labs(x = "Gender", y = 'Average number of extra rounds practicing') + theme_apa() +
  scale_fill_manual(values=c("springgreen3", "slateblue1")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  scale_x_discrete(labels = c("Men", "Women")) + 
  theme_apa()+theme(legend.position = "none") + guides (fill = "none")

ggsave(here("study1", "figs", "fig08_total-rev-count-by-gender.png"), p, width = 7, height = 7)


