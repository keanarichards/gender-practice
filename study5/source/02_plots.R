# load packages -----------------------------------------------------------


if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here, jtools)

# load data ---------------------------------------------------------------

clean <- read_csv(here("study5", "data", "clean.csv"))

## filter out potentially fraudulent responses 

clean_fraud_removed <- clean %>% filter(fraud == 0) 

# primary hypothesis 1 ----------------------------------------------------


dat <- dplyr::select(clean,gender,condition, practice_problems_binary)  %>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, condition, practice_problems_binary) %>% filter(gender == "Woman", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
                                                                                                                                        error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean,gender,condition, practice_problems_binary)%>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, condition, practice_problems_binary) %>% filter(gender == "Man", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
                                                                                                                                      error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat)

p <- ggplot(data = dat, aes(x = condition, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Condition', y = 'Percentage Practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Prepare"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study5", "figs", "fig00_pract-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)

## showing main effect of gender on DV 

dat <- dplyr::select(clean,gender, practice_problems_binary)  %>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, practice_problems_binary) %>% filter(gender == "Woman") %>% mutate(percent = n / sum(n),
                                                                                                                                        error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean,gender,practice_problems_binary)%>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, practice_problems_binary) %>% filter(gender == "Man") %>% mutate(percent = n / sum(n),
                                                                                                                                      error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat)

p <- ggplot(data = dat, aes(x = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Gender', y = 'Percentage Practicing') + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Prepare"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study5", "figs", "fig01_pract-choice-by-gender.png"), p, width = 7, height = 7)


# primary hypothesis 2 ----------------------------------------------------

lb <- function(x){mean(x) - sd(x)/sqrt((count(clean_fraud_removed)))}
ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean_fraud_removed)))}


## calculating mean & SEM within gender 


sumld <- clean %>%
  dplyr::select(perceived_pract_dev, gender) %>% group_by(gender) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))

p <- ggplot(data = sumld, aes(x = gender)) +
  geom_bar(aes(y = mean),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
                                                                  position=position_dodge(.9)) +
  labs(x = "Competition choice", y = 'Average (log) practice count') + theme_apa() +
  scale_fill_manual(values=c("springgreen3", "slateblue1"), labels = c("Men", "Women"))+
  scale_x_discrete(labels = c("Piece-rate", "Tournament")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study5", "figs", "fig04_total-rev-count-by-gender-comp-choice.png"), p, width = 7, height = 7)


