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


dat <- dplyr::select(clean_fraud_removed,gender,condition, practice_problems_binary) %>% drop_na(gender, condition, practice_problems_binary) %>% 
  group_by(gender, condition) %>% dplyr::count(gender, condition, practice_problems_binary)%>% mutate(percent = n / sum(n),error = sqrt((percent * (1-percent))/n)) %>% filter(practice_problems_binary == "Yes")

p <- ggplot(data = dat, aes(x = condition, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Condition', y = 'Percentage practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Piece-rate", "Tournament"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+ theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

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
  theme(axis.line = element_line(color = 'black')) +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

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
  theme_apa()+theme(legend.position = "none") + guides (fill = "none")+ theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study5", "figs", "fig02_perceived-prac-dev-by-gender.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_task_gender_pract) %>% drop_na(perc_task_gender_pract) %>% 
  dplyr::count(perc_task_gender_pract) %>%  mutate(percent = n / sum(n),
                                                   error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = perc_task_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "No difference", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study5", "figs", "fig03_perc-task-gender-pract.png"), p, width = 7, height = 7)


dat <- dplyr::select(clean_fraud_removed, perc_gen_gender_pract) %>% drop_na(perc_gen_gender_pract) %>% 
  dplyr::count(perc_gen_gender_pract) %>%  mutate(percent = n / sum(n),
                                                  error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = perc_gen_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "No difference", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


ggsave(here("study5", "figs", "fig04_perc-gen-gender-pract.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_gender_comp_F) %>% drop_na(perc_gender_comp_F) %>% 
  dplyr::count(perc_gender_comp_F) %>%  mutate(percent = n / sum(n),
                                               error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp_F, fill = perc_gender_comp_F))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c( "No difference","Piece rate" , "Tournament"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


ggsave(here("study5", "figs", "fig05_perc-gender-comp-F.png"), p, width = 7, height = 7)



dat <- dplyr::select(clean_fraud_removed, better_gender_guess) %>% drop_na(better_gender_guess) %>% 
  dplyr::count(better_gender_guess) %>%  mutate(percent = n / sum(n),
                                                error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = better_gender_guess, fill = better_gender_guess))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "No difference", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


ggsave(here("study5", "figs", "fig06_better-gender-guess.png"), p, width = 7, height = 7)

dat <- dplyr::select(clean_fraud_removed, perc_gender_comp_M) %>% drop_na(perc_gender_comp_M) %>% 
  dplyr::count(perc_gender_comp_M) %>%  mutate(percent = n / sum(n),
                                               error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp_M, fill = perc_gender_comp_M))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "orangered", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                               position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c( "No difference","Piece rate" , "Tournament"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


ggsave(here("study5", "figs", "fig07_perc-gender-comp-M.png"), p, width = 7, height = 7)


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
  modify_header(stat_1 = "Control condition, N = {n}", stat_2 = "Competition condition, N = {n}") %>% 
  as_gt() %>%  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("***Demographics Across Conditions***")) %>%
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study5","figs", "demographics-table-conds-study5.png"))


# plots showing how perceptions & gender predict pract choice -------------

dat <- dplyr::select(clean_fraud_removed,gender,practice_problems_binary, perc_task_gender_pract)  %>%  group_by(perc_task_gender_pract, gender) %>%  drop_na(.) %>% dplyr::count(gender, perc_task_gender_pract, practice_problems_binary) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(practice_problems_binary == "Yes")

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Option selected', y = 'Percentage Practicing') +
  scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + theme_apa() +
  scale_x_discrete(labels = c("Men","No difference",  "Women")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) 
ggsave(here("study5", "figs", "pract-choice-by-gender-and-perc-task-prep-bar-study5.png"), p, width = 7, height = 7)


# plots showing how perceptions & gender predict pract choice -------------

dat <- dplyr::select(clean_fraud_removed,gender,practice_problems_binary, perc_gen_gender_pract) %>% group_by(perc_gen_gender_pract, gender) %>%  drop_na(.) %>% dplyr::count(gender, perc_gen_gender_pract, practice_problems_binary) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(practice_problems_binary == "Yes")

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Option selected', y = 'Percentage Practicing') +
  scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + theme_apa() +
  scale_x_discrete(labels = c("Men","No difference", "Women")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study5", "figs", "pract-choice-by-gender-and-perc-gen-prep-bar-study5.png"), p, width = 7, height = 7)

# creating panel plots ----------------------------------------------------

img1 <- magick::image_read(here::here("study5", "figs", "fig00_pract-choice-by-gender-and-cond-bar.png"))
img2 <- magick::image_read(here::here("study5", "figs", "fig01_pract-choice-by-gender.png"))

panel_study5 <- magick::image_append(c(img1, img2))

image_write(panel_study5, path = here::here("study5", "figs", "panel_study5.png"), format = "png")



# models  -----------------------------------------------------------------
source(here("study5", "source", "01_preregistered-analyses.R"))
source(here("study5", "source", "03_exploratory-analyses.R"))

## practice choice as DV: 


tab_model(sec_exploratory25, hypothesis1, sec_exploratory13, file = here("study5", "figs","tab_pract-choice-study5.html"),  dv.labels = c("(1)", "(2)", "(3)")) 
webshot(here("study5", "figs","tab_pract-choice-study5.html"), here("study5", "figs","tab_pract-choice-study5.png"), selector = "table", zoom= 2)

## Task scores as DV: 

tab_model(sec_exploratory12, sec_exploratory37, sec_exploratory4, file = here("study5", "figs","tab_task-scores-study5.html"), dv.labels = c("(1)", "(2)", "(3)"))
webshot(here("study5", "figs","tab_task-scores-study5.html"), here("study5", "figs","tab_task-scores-study5.png"), selector = "table", zoom= 2)

# creating summary tables -------------------------------------------------

## will use gtsummary - split table by gender 

clean_fraud_removed %>% select(task_score, practice_problems_binary, risk, conf_rank,
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
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study5","figs", "summary-table-gender-study5.png"))

clean_fraud_removed %>% select(better_gender_guess,perc_task_gender_pract,  perc_gender_comp_M, perc_gender_comp_F, perc_gen_gender_pract) %>% tbl_summary(missing= "no") %>% 
  as_gt() %>%  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("")) %>%
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study5","figs", "summary-table-beliefs-study5.png"))


## table showing pre-registered analyses

## creating table with main hypotheses: 

Hypotheses <- c("Women will choose to practice problems at a higher rate than men, especially when assigned to the competitive tournament payment scheme (i.e., we anticipate a main effect of gender on practice, and an interaction between gender and condition, such that women will practice more than men in both conditions, but the difference-in-differences between practicing rates across genders will be greater in the competition condition). The reference groups for all subsequent analyses will be the piece-rate payment scheme and men for condition and gender, respectively.", 
                "Women will be more likely to assume they practice less than others compared to men (that is, the effect of gender on perceived practice deviation will be negative), especially when assigned to the competitive tournament payment scheme (such that women in general will think that they practice less than other participants than men, but this difference will be exacerbated in the competition condition). ")

Models <- c(report(hypothesis1), report(hypothesis2)) 

Models <- str_split(Models, '\n\nStandardized', simplify = TRUE)[,1]

Models <- str_remove_all(Models, "\n\n|\n")


study5_models <- data.frame(Hypotheses, Models)

gt_tbl <- gt(study5_models) %>%
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
gt::gtsave(gt_tbl, file = here("study5","figs", "pre-reg-study5.png"))

# creating demographic tables by gender ---------------------------------------------

clean_fraud_removed %>%
  dplyr::select(
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
    by = gender,
    label = list(
      age ~ "Age",
      education ~ "Education"
    ), 
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  bold_labels() %>%
  add_p(test = list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fishers")) %>%  modify_footnote(
    update = everything() ~ NA) %>% 
  as_gt() %>%  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("***Demographics Based on Participant Gender***")) %>%
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study5","figs", "demographics-table-gender-study5.png"))













