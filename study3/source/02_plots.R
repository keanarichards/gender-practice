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

here <- here::here

# load data ---------------------------------------------------------------

clean <- read_csv(here("study3", "data", "clean.csv"))


## filter out other 
gender_filter <- clean %>% filter(gender != "Other")

## using summarySE function from here: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# primary hypothesis 1 ----------------------------------------------------
dat <- gender_filter %>% filter(is.na(condition) == FALSE)
dat <- summarySE(dat, "subj_confidence", c("gender", "condition"))

p <- ggplot(data = dat, aes(x = condition, y = subj_confidence, 
                                      fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = subj_confidence - ci, ymax = subj_confidence + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Condition", y = "Subjective Confidence") +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Preparation"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study3", "figs", "fig00_subj-conf-gender-by-cond.png"), p, width = 7, height = 7)

# exploratory analysis 1 --------------------------------------------------
dat <- gender_filter %>% filter(is.na(condition) == FALSE)
dat <- summarySE(dat, "task_score", c("gender", "condition"))

p <- ggplot(data = dat, aes(x = condition, y = task_score, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = task_score - ci, ymax = task_score + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Condition", y = "Task score") +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Preparation"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study3", "figs", "fig01_task_score-gender-by-cond.png"), p, width = 7, height = 7)


# exploratory analysis 2 --------------------------------------------------
dat <- gender_filter %>% filter(is.na(condition) == FALSE)
dat <- summarySE(dat, "abs_confidence", c("gender", "condition"))

p <- ggplot(data = dat, aes(x = condition, y = abs_confidence, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = abs_confidence - ci, ymax = abs_confidence + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Condition", y = "Absolute Confidence") +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Preparation"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study3", "figs", "fig02_abs-conf-gender-by-cond.png"), p, width = 7, height = 7)


# exploratory 3 -----------------------------------------------------------

dat <- summarySE(gender_filter, "interest", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = interest, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = interest - ci, ymax = interest + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Interest") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +theme(legend.position = "none") + guides (fill = F)

ggsave(here("study3", "figs", "fig03_interest-by-gender.png"), p, width = 7, height = 7)


# exploratory analysis 4 --------------------------------------------------

dat <- gender_filter %>% filter(is.na(better_gender_guess) == FALSE) %>% dplyr::select(better_gender_guess, gender) %>%
  dplyr::count(better_gender_guess, gender) %>% filter (gender == "Woman") %>%   mutate(percent = n / sum(n),
                                                error = sqrt((percent * (1-percent))/n))

dat1 <- gender_filter %>% filter(is.na(better_gender_guess) == FALSE) %>%  dplyr::select(better_gender_guess, gender) %>%
  dplyr::count(better_gender_guess, gender) %>% filter (gender == "Man") %>%   mutate(percent = n / sum(n),
                                                                                        error = sqrt((percent * (1-percent))/n))
dat <- rbind(dat1, dat)

p <- ggplot(data = dat, aes(x = better_gender_guess, fill = gender))  + 
  labs(x = 'Perceptions of gender differences in performance', y = 'Percentage')  + scale_y_continuous(limits = c(0, 100)) +
  geom_bar(aes(y = percent*100, position = "dodge", stat = "identity"), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women"))+ 
  geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
  position=position_dodge(.9)) + theme_apa() + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study3", "figs", "fig04_better-gender-guess.png"), p, width = 7, height = 7)

# exploratory analysis 5 --------------------------------------------------

dat <- gender_filter %>% filter(is.na(math_gender_prep) == FALSE) %>% dplyr::select(math_gender_prep, gender) %>%
  dplyr::count(math_gender_prep, gender) %>% filter (gender == "Woman") %>%   mutate(percent = n / sum(n),
                                                                                        error = sqrt((percent * (1-percent))/n))

dat1 <-  gender_filter %>% filter(is.na(math_gender_prep) == FALSE) %>% dplyr::select(math_gender_prep, gender) %>%
  dplyr::count(math_gender_prep, gender) %>% filter (gender == "Man") %>%   mutate(percent = n / sum(n),
                                                                                      error = sqrt((percent * (1-percent))/n))
dat <- rbind(dat1, dat)


## min is too low, so it gets cut off. Will choose only upper half of error bar
p <- ggplot(data = dat, aes(x = math_gender_prep, fill = gender))  + 
  labs(x = 'Perceptions of gender differences in preparation for math tasks', y = 'Percentage')  + scale_y_continuous(limits = c(0, 100)) +
  geom_bar(aes(y = percent*100, position = "dodge", stat = "identity"), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women"))+ 
  geom_errorbar(aes(ymin =percent*100, ymax =(percent*100)+(error*100)), width=.05,
                position=position_dodge(.9)) + theme_apa() + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study3", "figs", "fig05_perc-math-gender-pract.png"), p, width = 7, height = 7)



# exploratory analysis 6 ---------------------------------------------------


dat <-  gender_filter %>% filter(is.na(gen_gender_prep) == FALSE) %>% dplyr::select(gen_gender_prep, gender) %>%
  dplyr::count(gen_gender_prep, gender) %>% filter (gender == "Woman") %>%   mutate(percent = n / sum(n),
                                                                                        error = sqrt((percent * (1-percent))/n))

dat1 <- gender_filter %>% filter(is.na(gen_gender_prep) == FALSE) %>% dplyr::select(gen_gender_prep, gender) %>%
  dplyr::count(gen_gender_prep, gender) %>% filter (gender == "Man") %>%   mutate(percent = n / sum(n),
                                                                                      error = sqrt((percent * (1-percent))/n))
dat <- rbind(dat1, dat)

p <- ggplot(data = dat, aes(x = gen_gender_prep, fill = gender))  + 
  labs(x = 'Perceptions of gender differences in preparation for most tasks', y = 'Percentage')  + scale_y_continuous(limits = c(0, 100)) +
  geom_bar(aes(y = percent*100, position = "dodge", stat = "identity"), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women"))+ 
  geom_errorbar(aes(ymin =percent*100, ymax =(percent*100)+(error*100)), width=.05,
                position=position_dodge(.9)) + theme_apa() + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

ggsave(here("study3", "figs", "fig06_perc-gen-gender-pract.png"), p, width = 7, height = 7)

# exploratory 7 -----------------------------------------------------------

dat <- summarySE(gender_filter, "task_score", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = task_score, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = task_score - ci, ymax = task_score + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Task score") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F)

ggsave(here("study3", "figs", "fig07_task_score-by-gender.png"), p, width = 7, height = 7)

# exploratory 8 -----------------------------------------------------------

dat <- clean %>% filter(is.na(condition) == FALSE)
dat <- summarySE(dat, "abs_confidence", c("condition"))

p <- ggplot(data = dat, aes(x = condition, y = abs_confidence, 
                            fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = abs_confidence - ci, ymax = abs_confidence + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Condition", y = "Absolute Confidence") + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Preparation"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F)

ggsave(here("study3", "figs", "fig08_abs-conf-by-condition.png"), p, width = 7, height = 7)

# exploratory 9 -----------------------------------------------------------
dat <- clean %>% filter(is.na(condition) == FALSE)
dat <- summarySE(dat, "task_score", c("condition"))

p <- ggplot(data = dat, aes(x = condition, y = task_score, 
                            fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = task_score - ci, ymax = task_score + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Condition", y = "Task score") +
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Preparation"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F)

ggsave(here("study3", "figs", "fig09_task-score-by-condition.png"), p, width = 7, height = 7)

# exploratory 10 -----------------------------------------------------------
dat <- clean %>% filter(is.na(condition) == FALSE)
dat <- summarySE(dat, "total_time", c("condition"))

p <- ggplot(data = dat, aes(x = condition, y = total_time, 
                            fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = total_time - ci, ymax = total_time + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Condition", y = "Total time")  + 
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Preparation"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F)

ggsave(here("study3", "figs", "fig10_total-time-by-condition.png"), p, width = 7, height = 7)

# exploratory 11 -----------------------------------------------------------

dat <- summarySE(gender_filter, "control_prep_beliefs", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = control_prep_beliefs, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = control_prep_beliefs - ci, ymax = control_prep_beliefs + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Mean rating for beliefs about benefits of preparing") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F)

ggsave(here("study3", "figs", "fig11_control-prep-beliefs-by-gender.png"), p, width = 7, height = 7)


# exploratory 12 ----------------------------------------------------------

## explanation 2: Women are more likely to think that practicing will improve their performance on the multiplication task compared to men

dat <- summarySE(gender_filter, "rank_explanations_2", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = rank_explanations_2, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = rank_explanations_2 - ci, ymax = rank_explanations_2 + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Mean ranking") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F) + ylim(0,5)

ggsave(here("study3", "figs", "fig12_gender_rank_explanations_2.png"), p, width = 7, height = 7)


## explanation 4: Women enjoy practicing for the multiplication problems more than men

dat <- summarySE(gender_filter, "rank_explanations_4", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = rank_explanations_4, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = rank_explanations_4 - ci, ymax = rank_explanations_4 + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Mean ranking") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F) + ylim(0,5)

ggsave(here("study3", "figs", "fig13_gender_rank_explanations_4.png"), p, width = 7, height = 7)


## explanation 5: Women are less confident that they will perform well on the multiplication task compared to men

dat <- summarySE(gender_filter, "rank_explanations_5", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = rank_explanations_5, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = rank_explanations_5 - ci, ymax = rank_explanations_5 + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Mean ranking") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F) + ylim(0,5)

ggsave(here("study3", "figs", "fig14_gender_rank_explanations_5.png"), p, width = 7, height = 7)


## explanation 6: Women enjoy the process of mastering their multiplication tables more than men

dat <- summarySE(gender_filter, "rank_explanations_6", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = rank_explanations_6, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = rank_explanations_6 - ci, ymax = rank_explanations_6 + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Mean ranking") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F) + ylim(0,5)

ggsave(here("study3", "figs", "fig15_gender_rank_explanations_6.png"), p, width = 7, height = 7)


## explanation 8: Women have more free time to devote to practicing for the multiplication task than men


dat <- summarySE(gender_filter, "rank_explanations_8", c("gender"))

p <- ggplot(data = dat, aes(x = gender, y = rank_explanations_8, 
                            fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = rank_explanations_8 - ci, ymax = rank_explanations_8 + ci), width = .05,
                position = position_dodge(width = .9))+ labs(x = "Gender", y = "Mean ranking") +scale_fill_manual(values=c("springgreen3", "slateblue1")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+theme(legend.position = "none") + guides (fill = F)+ ylim(0,5)

ggsave(here("study3", "figs", "fig16_gender_rank_explanations_8.png"), p, width = 7, height = 7)



