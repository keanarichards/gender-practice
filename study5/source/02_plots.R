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


dat <- dplyr::select(clean_fraud_removed,gender,condition, practice_problems_binary)  %>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, condition, practice_problems_binary) %>% filter(gender == "Woman", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
                                                                                                                                        error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean_fraud_removed,gender,condition, practice_problems_binary)%>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, condition, practice_problems_binary) %>% filter(gender == "Man", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
                                                                                                                                      error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat)

p <- ggplot(data = dat, aes(x = condition, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Condition', y = 'Percentage practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Prepare"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study5", "figs", "fig00_pract-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)

## showing main effect of gender on DV 

dat <- dplyr::select(clean_fraud_removed,gender, practice_problems_binary)  %>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, practice_problems_binary) %>% filter(gender == "Woman") %>% mutate(percent = n / sum(n),
                                                                                                                                        error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean_fraud_removed,gender,practice_problems_binary)%>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, practice_problems_binary) %>% filter(gender == "Man") %>% mutate(percent = n / sum(n),
                                                                                                                                      error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat) %>% filter (practice_problems_binary == "Yes")

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

ggsave(here("study5", "figs", "fig01_pract-choice-by-gender.png"), p, width = 7, height = 7)


# primary hypothesis 2 ----------------------------------------------------

lb <- function(x){mean(x) - sd(x)/sqrt((count(clean_fraud_removed)))}
ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean_fraud_removed)))}


## calculating mean & SEM within gender 


sumld <- clean_fraud_removed %>%
  dplyr::select(perceived_pract_dev, gender) %>% group_by(gender) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))

p <- ggplot(data = sumld, aes(x = gender, fill= gender)) +
  geom_bar(aes(y = mean),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
                                                                  position=position_dodge(.9)) +
  labs(x = "Gender", y = 'Average perceived practice deviation') + theme_apa() +
  scale_fill_manual(values=c("springgreen3", "slateblue1")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  scale_x_discrete(labels = c("Men", "Women")) + 
  theme_apa()+theme(legend.position = "none") + guides (fill = "none")

ggsave(here("study5", "figs", "fig02_perceived-prac-dev-by-gender.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_task_gender_pract) %>% drop_na(perc_task_gender_pract) %>% 
  dplyr::count(perc_task_gender_pract) %>%  mutate(percent = n / sum(n),
                                                   error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = perc_task_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1", "orangered"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men prepare more for task", "Women prepare more for task", "No difference in preparation for task"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 

ggsave(here("study5", "figs", "fig03_perc-task-gender-pract.png"), p, width = 7, height = 7)


dat <- dplyr::select(clean_fraud_removed, perc_gen_gender_pract) %>% drop_na(perc_gen_gender_pract) %>% 
  dplyr::count(perc_gen_gender_pract) %>%  mutate(percent = n / sum(n),
                                                  error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = perc_gen_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1", "orangered"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men prepare more in general", "Women prepare more in general", "No difference in preparation in general"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 


ggsave(here("study5", "figs", "fig04_perc-gen-gender-pract.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_gender_comp_F) %>% drop_na(perc_gender_comp_F) %>% 
  dplyr::count(perc_gender_comp_F) %>%  mutate(percent = n / sum(n),
                                             error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp_F, fill = perc_gender_comp_F))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1", "orangered"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c( "Women would choose each equally","Women would choose piece rate more" , "Women would choose tournament more"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 


ggsave(here("study5", "figs", "fig05_perc-gender-comp-F.png"), p, width = 7, height = 7)



dat <- dplyr::select(clean_fraud_removed, better_gender_guess) %>% drop_na(better_gender_guess) %>% 
  dplyr::count(better_gender_guess) %>%  mutate(percent = n / sum(n),
                                                error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = better_gender_guess, fill = better_gender_guess))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1", "orangered"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men performed better on task", "Women performed better on task", "No difference in performance on task"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 


ggsave(here("study5", "figs", "fig06_better-gender-guess.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_gender_comp_M) %>% drop_na(perc_gender_comp_M) %>% 
  dplyr::count(perc_gender_comp_M) %>%  mutate(percent = n / sum(n),
                                               error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp_M, fill = perc_gender_comp_M))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1", "orangered"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c( "Men would choose each equally","Men would choose piece rate more" , "Men would choose tournament more"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") 


ggsave(here("study5", "figs", "fig07_perc-gender-comp-M.png"), p, width = 7, height = 7)