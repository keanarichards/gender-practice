# load packages -----------------------------------------------------------


if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here, jtools)

# load data ---------------------------------------------------------------

clean <- read_csv(here("study4", "data", "clean.csv"))

# primary hypothesis 1 ----------------------------------------------------

dat <- dplyr::select(clean,gender,comp_choice, condition) %>% filter(gender == "Woman") %>%
  dplyr::count(comp_choice, condition) %>%  filter(comp_choice == "tournament") %>% mutate(percent = n / sum(n),
  error = sqrt((percent * (1-percent))/n))
p <- ggplot(data = dat, aes(x = condition, fill = condition)) +
  geom_bar(aes(y = percent*100),
  position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
  position=position_dodge(.9)) +theme(legend.position = "none") + guides (fill = F)+
  labs(x = 'Condition', y = 'Percentage Competing') + theme_apa() +
  scale_x_discrete(labels = c("Control", "Prepare")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
ggsave(here("study4", "figs", "fig00_comp-choice-women-by-cond.png"), width = 7, height = 7)


# primary hypothesis 2 ----------------------------------------------------

## will use binary choice to practice for now: 


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

ggsave(here("study4", "figs", "fig01_pract-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)



# to be added in later

# lb <- function(x){mean(x) - sd(x)/sqrt((count(clean)))}
# ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean)))} 
# 
# clean$logextra_prep_count <- log10(clean$extra_practice_rounds_count +1)
# 
# ## calculating mean & SEM within gender and comp choice
# 
# 
# sumld <- clean %>% 
#   dplyr::select(logextra_prep_count, gender, comp_choice) %>% na.omit(comp_choice) %>% 
#   group_by(gender, comp_choice) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))
# 
# p <- ggplot(data = sumld, aes(x = comp_choice, fill = gender)) +
#   geom_bar(aes(y = mean),
#            position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
#                                                                   position=position_dodge(.9)) + 
#   labs(x = "Competition choice", y = 'Average (log) practice count') + theme_apa() +
#   scale_fill_manual(values=c("springgreen3", "slateblue1"), labels = c("Men", "Women"))+
#   scale_x_discrete(labels = c("Piece-rate", "Tournament")) + theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study4", "figs", "fig04_total-rev-count-by-gender-comp-choice.png"), p, width = 7, height = 7)



# primary hypothesis 3 ----------------------------------------------------

dat <- dplyr::select(clean,gender,condition, comp_choice)  %>% na.omit(comp_choice) %>% 
  dplyr::count(gender, condition, comp_choice) %>% filter(gender == "Woman", comp_choice == "tournament") %>% mutate(percent = n / sum(n),
                                                                                                                                        error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean,gender,condition, comp_choice)%>% na.omit(comp_choice) %>% 
  dplyr::count(gender, condition, comp_choice) %>% filter(gender == "Man", comp_choice == "tournament") %>% mutate(percent = n / sum(n),
                                                                                                                                      error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat)

p <- ggplot(data = dat, aes(x = condition, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Condition', y = 'Percentage Competing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Prepare"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study4", "figs", "fig02_comp-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)





# exploratory analyses 

# exploring effect of condition and competition choice on practice choice ----------------------------------------------------


dat <- dplyr::select(clean,gender,comp_choice, practice_problems_binary)  %>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, comp_choice, practice_problems_binary) %>% filter(gender == "Woman", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
                                                                                                                  error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean,gender,comp_choice, practice_problems_binary)%>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, comp_choice, practice_problems_binary) %>% filter(gender == "Man", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
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

ggsave(here("study4", "figs", "fig05_pract-choice-by-gender-and-comp-choice-bar.png"), p, width = 7, height = 7)























## FROM OLD CODE, needs to be edited - commenting out for now: 








# exploratory analysis 3a ---------------------------------------------------

# dat <- dplyr::select(clean, better_gender_guess, gender) %>%
#   dplyr::count(better_gender_guess, gender) %>% filter (gender == "Woman") %>%   mutate(percent = n / sum(n),
#                                                 error = sqrt((percent * (1-percent))/n))
# 
# dat1 <- dplyr::select(clean, better_gender_guess, gender) %>%
#   dplyr::count(better_gender_guess, gender) %>% filter (gender == "Man") %>%   mutate(percent = n / sum(n),
#                                                                                         error = sqrt((percent * (1-percent))/n))
# dat <- rbind(dat1, dat)
# 
# dat <- dat %>% filter (better_gender_guess == "Women")
# 
# 
# p <- ggplot(data = dat, aes(x = gender, fill = gender))  + 
#   labs(x = 'Participant gender', y = 'Percentage who said women performed better on task')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = F)+
#   geom_bar(aes(y = percent*100, position = "dodge", stat = "identity"), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ 
#   geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
#   position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study2", "figs", "fig01_better-gender-guess.png"), p, width = 7, height = 7)
# 
# # exploratory analysis 3b ---------------------------------------------------
# 
# dat <- dplyr::select(clean, perc_gender_comp, gender) %>%
#   dplyr::count(perc_gender_comp, gender) %>% filter(gender == "Woman") %>%  mutate(percent = n / sum(n),
#   error = sqrt((percent * (1-percent))/n))
# 
# dat1 <- dplyr::select(clean, perc_gender_comp, gender) %>%
#   dplyr::count(perc_gender_comp, gender) %>% filter(gender == "Man") %>%  mutate(percent = n / sum(n),
#   error = sqrt((percent * (1-percent))/n))
# 
# dat <- rbind(dat1, dat)
# 
# dat <- dat %>% filter (perc_gender_comp == "Men")
# 
# 
# p <- ggplot(data = dat, aes(x = gender, fill = gender))  + 
#   labs(x = 'Participant gender', y = 'Percentage who said men chose to compete more for task')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = F)+
#   geom_bar(aes(y = percent*100, position = "dodge", stat = "identity"), position = "dodge", stat = "identity") +
#   scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
#   position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study2", "figs", "fig02_perc-gender-comp.png"), p, width = 7, height = 7)
# 
# 
# # exploratory analysis 3c ---------------------------------------------------
# 
# dat <- dplyr::select(clean, perc_gen_gender_pract, gender) %>% filter(!is.na(perc_gen_gender_pract)) %>% dplyr::count(perc_gen_gender_pract, gender) %>% 
#   filter(gender == "Woman") %>%   mutate(percent = n / sum(n),
#   error = sqrt((percent * (1-percent))/n))
# 
# dat1 <- dplyr::select(clean, perc_gen_gender_pract, gender) %>% filter(!is.na(perc_gen_gender_pract)) %>% dplyr::count(perc_gen_gender_pract, gender) %>% 
#   filter(gender == "Man") %>%   mutate(percent = n / sum(n),
#   error = sqrt((percent * (1-percent))/n))
# 
# dat <- rbind(dat1, dat)
# 
# dat <- dat %>% filter (perc_gen_gender_pract == "Women")
# 
# p <- ggplot(data = dat, aes(x = gender, fill = gender))  + 
#   labs(x = 'Participant gender', y = 'Percentage who said women prepare more in general') +theme(legend.position = "none") + guides (fill = F) + scale_y_continuous(limits = c(0, 100))+ 
#   geom_bar(aes(y = percent*100), position = "dodge", stat = "identity")+scale_fill_manual(values=c("springgreen3", "slateblue1"))+ 
#   geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05, position=position_dodge(.9)) + theme_apa() +
#   scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study2", "figs", "fig03_perc-gen-gender-pract.png"), p, width = 7, height = 7)
# 
# 
# 
# # true exploratory 1 ------------------------------------------------------
# 
# lb <- function(x){mean(x) - sd(x)/sqrt((count(clean)))}
# ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean)))} 
# 
# clean$logextra_prep_count <- log10(clean$extra_prep_count +1)
# 
# ## calculating mean & SEM within gender and comp choice
# 
# 
# sumld <- clean %>% 
#   dplyr::select(logextra_prep_count, gender, comp_choice) %>% na.omit(comp_choice) %>% 
#   group_by(gender, comp_choice) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))
# 
# p <- ggplot(data = sumld, aes(x = comp_choice, fill = gender)) +
#   geom_bar(aes(y = mean),
#            position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
#                                                                   position=position_dodge(.9)) + 
#   labs(x = "Competition choice", y = 'Average (log) practice count') + theme_apa() +
#   scale_fill_manual(values=c("springgreen3", "slateblue1"), labels = c("Men", "Women"))+
#   scale_x_discrete(labels = c("Piece-rate", "Tournament")) + theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study2", "figs", "fig04_total-rev-count-by-gender-comp-choice.png"), p, width = 7, height = 7)
# 
# 
# # true exploratory 2 ------------------------------------------------------
# 
# dat <- dplyr::select(clean,gender,comp_choice, pract_choice) %>%
#   dplyr::count(gender, comp_choice, pract_choice) %>% filter(gender == "Woman", pract_choice == "Yes") %>% mutate(percent = n / sum(n),
#                                                                                                                   error = sqrt((percent * (1-percent))/n))
# 
# dat1 <- dplyr::select(clean,gender,comp_choice, pract_choice) %>%
#   dplyr::count(gender, comp_choice, pract_choice) %>% filter(gender == "Man", pract_choice == "Yes") %>% mutate(percent = n / sum(n),
#                                                                                                                 error = sqrt((percent * (1-percent))/n))
# 
# dat <- rbind(dat1, dat)
# 
# p <- ggplot(data = dat, aes(x = comp_choice, fill = gender)) +
#   geom_bar(aes(y = percent*100),
#            position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
#                                                                   position=position_dodge(.9)) + 
#   geom_text(x = 1.5, y = 100, label = "***") + 
#   labs(x = 'Choice to compete', y = 'Percentage Practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
#   theme_apa() +
#   scale_x_discrete(labels = c("Piece-rate", "Tournament"))+ theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study2", "figs", "fig05_pract-choice-by-gender-and-comp-choice-bar.png"), p, width = 7, height = 7)
# 
# 
# 
# ## for nsf app with caption attached 
# p <- p + labs (caption = "Figure 2. Proportion of participants who chose to prepare based on \n participant gender and choice to compete from second study. \n Error bars represent standard error.") + theme(
#   plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
#   axis.title.y = element_text(size = 14)
# )
# ggsave(here("nsf-application", "nsf2.png"), p, width = 7, height = 7)
# 
