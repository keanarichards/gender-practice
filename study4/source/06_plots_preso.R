# load packages -----------------------------------------------------------


if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here, jtools, webshot, sjPlot)

# load data ---------------------------------------------------------------

clean <- read_csv(here("study4", "data", "clean.csv"))

clean_fraud_removed <- clean %>% filter(fraud == 0) 

practice <- clean_fraud_removed %>% filter(condition == "pract") 

# primary hypothesis 1 ----------------------------------------------------

# dat <- dplyr::select(clean_fraud_removed,gender,comp_choice, condition) %>% filter(gender == "Woman") %>%
#   dplyr::count(comp_choice, condition) %>%  filter(comp_choice == "tournament") %>% mutate(percent = n / sum(n),
#   error = sqrt((percent * (1-percent))/n))
# p <- ggplot(data = dat, aes(x = condition, fill = condition)) +
#   geom_bar(aes(y = percent*100),
#   position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
#   position=position_dodge(.9)) +theme(legend.position = "none") + guides (fill = F)+
#   labs(x = 'Condition', y = '% Competing') + theme_apa() +
#   scale_x_discrete(labels = c("Control", "Prepare")) + theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# ggsave(here("study4", "figs_preso", "fig00_comp-choice-women-by-cond.png"), width = 7, height = 7)


# primary hypothesis 2 ----------------------------------------------------

## will use binary choice to practice for now: 

# 
# dat <- dplyr::select(clean_fraud_removed,gender,condition, practice_problems_binary) %>% filter(condition == "pract") %>% na.omit(practice_problems_binary) %>% 
#   dplyr::count(gender, condition, practice_problems_binary) %>% filter(gender == "Woman", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
#                                                                                                                                           error = sqrt((percent * (1-percent))/n))
# 
# dat1 <- dplyr::select(clean_fraud_removed,gender,condition, practice_problems_binary)%>% filter(condition == "pract")%>% na.omit(practice_problems_binary) %>% 
#   dplyr::count(gender, condition, practice_problems_binary) %>% filter(gender == "Man", practice_problems_binary == "Yes") %>% mutate(percent = n / sum(n),
#                                                                                                                                         error = sqrt((percent * (1-percent))/n))
# 
# dat <- rbind(dat1, dat)
# 
# p <- ggplot(data = dat, aes(x = condition, fill = gender)) +
#   geom_bar(aes(y = percent*100),
#            position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
#                                                                   position=position_dodge(.9)) + 
#   geom_text(x = 1.5, y = 100, label = "***") + 
#   labs(x = 'Condition', y = '% practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
#   theme_apa() +
#   scale_x_discrete(labels = c("Control", "Prepare"))+ theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study4", "figs_preso", "fig01_pract-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)
# 


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
# ggsave(here("study4", "figs_preso", "fig04_total-rev-count-by-gender-comp-choice.png"), p, width = 7, height = 7)



# primary hypothesis 3 ----------------------------------------------------

dat <- dplyr::select(clean_fraud_removed,gender,comp_choice, condition) %>% group_by(condition, gender) %>%  dplyr::count(gender, comp_choice, condition) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(comp_choice == "tournament")

p <- ggplot(data = dat, aes(x = condition, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) +
  geom_text(x = 1.5, y = 100, label = "***") +
  labs(x = 'Condition', y = '% Competing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) +
  theme_apa() +
  scale_x_discrete(labels = c("Control", "Prepare"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+ theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )



ggsave(here("study4", "figs_preso", "fig02_comp-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)



# exploratory analyses 

# exploring effect of condition and competition choice on practice choice ----------------------------------------------------


dat <- dplyr::select(practice,gender,comp_choice, practice_problems_binary) %>% drop_na(gender, comp_choice, practice_problems_binary) %>% 
  group_by(gender, comp_choice) %>% dplyr::count(gender, comp_choice, practice_problems_binary)%>% mutate(percent = n / sum(n),error = sqrt((percent * (1-percent))/n)) %>% filter(practice_problems_binary == "Yes")

p <- ggplot(data = dat, aes(x = comp_choice, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Choice to compete', y = '% practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Piece-rate", "Tournament"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+ theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )


ggsave(here("study4", "figs_preso", "fig05_pract-choice-by-gender-and-comp-choice-bar.png"), p, width = 7, height = 7)

## showing main effect of gender on choice to pract

dat <- dplyr::select(clean_fraud_removed,gender, practice_problems_binary, condition) %>% filter(condition == "pract") %>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, practice_problems_binary) %>% filter(gender == "Woman") %>% mutate(percent = n / sum(n),
                                                                              error = sqrt((percent * (1-percent))/n))

dat1 <- dplyr::select(clean_fraud_removed,gender,practice_problems_binary, condition) %>% filter(condition == "pract")%>% na.omit(practice_problems_binary) %>% 
  dplyr::count(gender, practice_problems_binary) %>% filter(gender == "Man") %>% mutate(percent = n / sum(n),
                                                                            error = sqrt((percent * (1-percent))/n))

dat <- rbind(dat1, dat) %>% filter (practice_problems_binary == "Yes")

p <- ggplot(data = dat, aes(x = gender, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Gender', y = '% practicing') + 
  theme_apa()+theme(legend.position = "none") + guides (fill = "none") +
  scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )



ggsave(here("study4", "figs_preso", "fig06_pract-choice-by-gender.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_task_gender_pract) %>% drop_na(perc_task_gender_pract) %>% 
  dplyr::count(perc_task_gender_pract) %>%  mutate(percent = n / sum(n),
                                                   error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = perc_task_gender_pract))  + 
  labs(x = 'Option selected', y = '% of participants')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "No difference", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )


ggsave(here("study4", "figs_preso", "fig07_perc-task-gender-pract.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_gen_gender_pract) %>% drop_na(perc_gen_gender_pract) %>% 
  dplyr::count(perc_gen_gender_pract) %>%  mutate(percent = n / sum(n),
                                                  error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = perc_gen_gender_pract))  + 
  labs(x = 'Option selected', y = '% of participants')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "No difference", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )




ggsave(here("study4", "figs_preso", "fig08_perc-gen-gender-pract.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_gender_comp) %>% drop_na(perc_gender_comp) %>% 
  dplyr::count(perc_gender_comp) %>%  mutate(percent = n / sum(n),
                                             error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp, fill = perc_gender_comp))  + 
  labs(x = 'Option selected', y = '% of participants')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "No difference", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )



ggsave(here("study4", "figs_preso", "fig09_perc-gender-comp.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, better_gender_guess) %>% drop_na(better_gender_guess) %>% 
  dplyr::count(better_gender_guess) %>%  mutate(percent = n / sum(n),
                                                error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = better_gender_guess, fill = better_gender_guess))  + 
  labs(x = 'Option selected', y = '% of participants')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "No difference", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )



ggsave(here("study4", "figs_preso", "fig10_better-gender-guess.png"), p, width = 7, height = 7)

# creating demographic tables ---------------------------------------------

## should recode the people who select more than one option to multi-racial per narratives study

fishers <- function(data, variable, by, ...) {
  result <- list()
  result$p <-  stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = T)$p.value
  result$test <- "Fisher's exact test"
  result
}

clean_fraud_removed %>%
  dplyr::select(
    condition,
    age,
    gender, 
    education
  ) %>%
  mutate(
    education = factor(
      education,
      levels = c(
        "No formal education",
        "Less than high school",
        "High school graduate (diploma)",
        "High school graduate (GED)",
        "Some college (1-4 years, no degree)",
        "Associate's degree (including occupational or academic degrees)",
        "Bachelor's degree (BA, BS, etc)",
        "Master's degree (MA, MS, MENG, MSW, etc)",
        "Professional school degree (MD, DDC, JD, etc)",
        "Doctorate degree (PhD, EdD, etc)"
      )
    )
  ) %>% 
  tbl_summary(
    by = condition,
    label = list(
      gender ~ "Gender",
      age ~ "Age",
      education ~ "Education"
    ), 
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  bold_labels() %>%
  add_p(test = list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fishers")) %>%  modify_footnote(
    update = everything() ~ NA) %>% 
  modify_header(stat_1 = "Control condition, N = {n}", stat_2 = "Preparation condition, N = {n}") %>% 
  as_gt() %>%  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("***Demographics Across Conditions***")) %>%
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study4","figs_preso", "demographics-table-conds-study4.png"))


# plots showing how perceptions & gender predict pract choice -------------

dat <- dplyr::select(practice,gender,practice_problems_binary, perc_task_gender_pract)  %>%  group_by(perc_task_gender_pract, gender) %>%  drop_na(.) %>% dplyr::count(gender, perc_task_gender_pract, practice_problems_binary) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(practice_problems_binary == "Yes")

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Option selected', y = '% practicing') +
  scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + theme_apa() +
  scale_x_discrete(labels = c("Men","No difference",  "Women"), guide = guide_axis(n.dodge = 4)) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) + theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )

ggsave(here("study4", "figs_preso", "pract-choice-by-gender-and-perc-task-prep-bar-study4.png"), p, width = 7, height = 7)


# plots showing how perceptions & gender predict pract choice -------------

dat <- dplyr::select(practice,gender,practice_problems_binary, perc_gen_gender_pract) %>% group_by(perc_gen_gender_pract, gender) %>%  drop_na(.) %>% dplyr::count(gender, perc_gen_gender_pract, practice_problems_binary) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(practice_problems_binary == "Yes")

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Option selected', y = '% practicing') +
  scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + theme_apa() +
  scale_x_discrete(labels = c("Men","No difference", "Women"), guide = guide_axis(n.dodge = 4)) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) + theme(
    plot.caption = element_text(hjust = 0, size =30), axis.text.x = element_text(size = 18), legend.text = element_text(size =18), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )

ggsave(here("study4", "figs_preso", "pract-choice-by-gender-and-perc-gen-prep-bar-study4.png"), p, width = 7, height = 7)

# creating panel plots ----------------------------------------------------

img1 <- magick::image_read(here::here("study4", "figs_preso", "fig05_pract-choice-by-gender-and-comp-choice-bar.png"))
img2 <- magick::image_read(here::here("study4", "figs_preso", "fig06_pract-choice-by-gender.png"))

panel_study4 <- magick::image_append(c(img1, img2))

image_write(panel_study4, path = here::here("study4", "figs_preso", "panel_study4.png"), format = "png")


# models  -----------------------------------------------------------------
source(here("study4", "source", "01_preregistered-analyses.R"))
source(here("study4", "source", "03_exploratory-analyses.R"))

## practice choice as DV: 


tab_model(sec_exploratory39, exploratory3, exploratory29, file = here("study4", "figs_preso","tab_pract-choice-study4.html"),  dv.labels = c("(1)", "(2)", "(3)")) 
webshot(here("study4", "figs_preso","tab_pract-choice-study4.html"), here("study4", "figs_preso","tab_pract-choice-study4.png"), selector = "table", zoom= 2)

## Task scores as DV: 

tab_model(exploratory19, exploratory46, exploratory20, file = here("study4", "figs_preso","tab_task-scores-study4.html"), dv.labels = c("(1)", "(2)", "(3)"))
webshot(here("study4", "figs_preso","tab_task-scores-study4.html"), here("study4", "figs_preso","tab_task-scores-study4.png"), selector = "table", zoom= 2)


## Comp choice as DV: 

tab_model(exploratory16, hypothesis3a, hypothesis3c, file = here("study4", "figs_preso","tab_comp-choice-study4.html"), dv.labels = c("(1)", "(2)", "(3)"))
webshot(here("study4", "figs_preso","tab_comp-choice-study4.html"), here("study4", "figs_preso","tab_comp-choice-study4.png"), selector = "table", zoom= 2)



# creating summary tables -------------------------------------------------

## will use gtsummary - split table by gender 

clean_fraud_removed %>% select(task_score, comp_choice, practice_problems_binary, risk, conf_rank,
                 gender) %>% tbl_summary(by = gender, missing = "no") %>%   modify_header(label = "**Variable**") %>% 
  as_gt() %>%  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("")) %>%
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study4","figs_preso", "summary-table-gender-study4.png"))

clean_fraud_removed %>% select(better_gender_guess,perc_task_gender_pract,  perc_gender_comp, perc_gen_gender_pract) %>% tbl_summary(missing= "no") %>% 
  as_gt() %>%  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("")) %>%
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study4","figs_preso", "summary-table-beliefs-study4.png"))

## table showing pre-registered analyses

## creating table with main hypotheses: 

Hypotheses <- c("Women in the unlimited preparation condition will be significantly more likely to compete compared to women in the control condition", 
                "Women will attempt more practice problems than men.",
                "The gender difference in competition choice will be smaller in the treatment condition than the control condition.")

Models <- c(report(hypothesis1), report(hypothesis2a), report(hypothesis3a)) 

Models <- str_split(Models, '\n\nStandardized', simplify = TRUE)[,1]

Models <- str_remove_all(Models, "\n\n|\n")

study4_models <- data.frame(Hypotheses, Models)

gt_tbl <- gt(study4_models) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("")) %>%
  opt_align_table_header(align = "left")
gt::gtsave(gt_tbl, file = here("study4","figs_preso", "pre-reg-study4.png"))








