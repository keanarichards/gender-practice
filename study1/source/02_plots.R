# load packages -----------------------------------------------------------


## Package names
packages <- c("tidyverse", "jtools", "here", "sjPlot", "webshot", "report", "gt", "gtsummary", "magick")

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
  theme(axis.line = element_line(color = 'black')) + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )+ theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study1", "figs", "fig00_comp-choice-by-gender-and-cond-bar.png"), p, width = 7, height = 7)

# primary hypothesis 2 ----------------------------------------------------


dat <- dplyr::select(clean,gender,comp_choice, pract_choice) %>% drop_na(gender, comp_choice, pract_choice) %>% 
  group_by(gender, comp_choice) %>% dplyr::count(gender, comp_choice, pract_choice)%>% mutate(percent = n / sum(n),error = sqrt((percent * (1-percent))/n)) %>% filter(pract_choice == "Yes")

p <- ggplot(data = dat, aes(x = comp_choice, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Choice to compete', y = 'Percentage Practicing') +scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + 
  theme_apa() +
  scale_x_discrete(labels = c("Piece-rate", "Tournament"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))+ theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


ggsave(here("study1", "figs", "fig01_pract-choice-by-gender-and-comp-choice-bar.png"), p, width = 7, height = 7)


## for nsf app with caption attached 
# p <- p + labs (caption = "Figure 1. Proportion of participants who chose to prepare based on \n participant gender and choice to compete from first study. \n Error bars represent standard error.") + theme(
#   plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
#   axis.title.y = element_text(size = 14)
# )
# 
# ggsave(here("nsf-application", "nsf1.png"), p, width = 7, height = 7)
# 

# primary hypothesis 3 ----------------------------------------------------

# 
# lb <- function(x){mean(x) - sd(x)/sqrt((count(clean)))}
# ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean)))} 
# 
# 
# clean$logtotal_rev_count <- log10(clean$total_review_count +1)
# 
# ## calculating mean & SEM within gender and comp choice
# ## first have to remove NA participants
# clean <- clean %>% filter(!is.na(comp_choice))
# 
# sumld <- clean %>% 
#   select(logtotal_rev_count, gender, comp_choice) %>% 
#   group_by(gender, comp_choice) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))
# 
# p <- ggplot(data = sumld, aes(x = comp_choice, fill = gender)) +
#   geom_bar(aes(y = mean),
#            position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
#                                                                   position=position_dodge(.9)) + 
#   labs(x = "Competition choice", y = 'Average (log) rounds of extra practice') + theme_apa() +
#   scale_fill_manual(values=c("springgreen3", "slateblue1"), labels = c("Men", "Women"))+
#   scale_x_discrete(labels = c("Piece-rate", "Tournament")) + theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# ggsave(here("study1", "figs", "fig02_total-rev-count-by-gender-comp-choice.png"), p, width = 7, height = 7)
# 
# 
# # primary hypothesis 4 ----------------------------------------------------

dat <- dplyr::select(clean, perc_task_gender_pract) %>% drop_na(perc_task_gender_pract) %>% 
  dplyr::count(perc_task_gender_pract) %>%  mutate(percent = n / sum(n),
                                                   error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = perc_task_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed")  + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study1", "figs", "fig03_perc-task-gender-pract.png"), p, width = 7, height = 7)


# exploratory analysis 7a -------------------------------------------------


dat <- dplyr::select(clean, better_gender_guess) %>% drop_na(better_gender_guess) %>% 
  dplyr::count(better_gender_guess) %>%  mutate(percent = n / sum(n),
                                                error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = better_gender_guess, fill = better_gender_guess))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


ggsave(here("study1", "figs", "fig04_better-gender-guess.png"), p, width = 7, height = 7)

# exploratory analysis 7b -------------------------------------------------

dat <- dplyr::select(clean, perc_gender_comp) %>% drop_na(perc_gender_comp) %>% 
  dplyr::count(perc_gender_comp) %>%  mutate(percent = n / sum(n),
                                             error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gender_comp, fill = perc_gender_comp))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


ggsave(here("study1", "figs", "fig05_perc-gender-comp.png"), p, width = 7, height = 7)


# exploratory analysis 7c -------------------------------------------------


dat <- dplyr::select(clean, perc_gen_gender_pract) %>% drop_na(perc_gen_gender_pract) %>% 
  dplyr::count(perc_gen_gender_pract) %>%  mutate(percent = n / sum(n),
                                                  error = sqrt((percent * (1-percent))/n))

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = perc_gen_gender_pract))  + 
  labs(x = 'Option selected', y = 'Percentage of participants who selected response option')  + scale_y_continuous(limits = c(0, 100))+theme(legend.position = "none") + guides (fill = "none")+
  geom_bar(aes(y = percent*100), position = "dodge", stat = "identity") +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                                                                                                  position=position_dodge(.9)) + theme_apa() + scale_x_discrete(labels = c("Men", "Women"))+ theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  geom_hline(yintercept = 50, linetype = "dashed") + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


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
  theme(axis.line = element_line(color = 'black')) +scale_fill_manual(values=c("springgreen3", "slateblue1"))+ theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study1", "figs", "fig07_pract-choice-by-gender.png"), p, width = 7, height = 7)

## showing main effect of gender on extra practice rounds

# 
# lb <- function(x){mean(x) - sd(x)/sqrt((count(clean)))}
# ub <- function(x) {mean(x) + sd(x)/sqrt((count(clean)))}
# 
# 
# sumld <- clean %>%
#   dplyr::select(total_review_count, gender) %>% group_by(gender) %>% summarise_all(list(mean = mean, lower = lb, upper = ub))
# 
# p <- ggplot(data = sumld, aes(x = gender, fill= gender)) +
#   geom_bar(aes(y = mean),
#            position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =lower$n, ymax =upper$n), width=.05,
#                                                                   position=position_dodge(.9)) +
#   labs(x = "Gender", y = 'Average number of extra rounds practicing') + theme_apa() +
#   scale_fill_manual(values=c("springgreen3", "slateblue1")) + theme(panel.border  = element_blank()) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black')) +
#   scale_x_discrete(labels = c("Men", "Women")) + 
#   theme_apa()+theme(legend.position = "none") + guides (fill = "none")
# 
# ggsave(here("study1", "figs", "fig08_total-rev-count-by-gender.png"), p, width = 7, height = 7)
# 
# 
# models  -----------------------------------------------------------------
source(here("study1", "source", "01_preregistered-analyses.R"))
source(here("study1", "source", "03_exploratory-analyses.R"))

## practice choice as DV: 


tab_model(sec_exploratory17, sec_exploratory2, sec_exploratory14, file = here("study1", "figs","tab_pract-choice-study1.html"),  dv.labels = c("(1)", "(2)", "(3)")) 
webshot(here("study1", "figs","tab_pract-choice-study1.html"), here("study1", "figs","tab_pract-choice-study1.png"), selector = "table", zoom= 2)

## Task scores as DV: 

tab_model(sec_exploratory9, sec_exploratory20, sec_exploratory12, file = here("study1", "figs","tab_task-scores-study1.html"), dv.labels = c("(1)", "(2)", "(3)"))
webshot(here("study1", "figs","tab_task-scores-study1.html"), here("study1", "figs","tab_task-scores-study1.png"), selector = "table", zoom= 2)


## Comp choice as DV: 

tab_model(sec_exploratory6, primary_hyp1, sec_exploratory11, file = here("study1", "figs","tab_comp-choice-study1.html"), dv.labels = c("(1)", "(2)", "(3)"))
webshot(here("study1", "figs","tab_comp-choice-study1.html"), here("study1", "figs","tab_comp-choice-study1.png"), selector = "table", zoom= 2)


## table showing pre-registered analyses

## creating table with main hypotheses: 

Hypotheses <- c("There will be an interaction between gender and condition, where we will observe the typical gender gap in willingness to compete in the control condition. However, women in the preparation condition will be more likely to compete than women in the control condition, and the effect will be greater than the equivalent effect in men. Therefore, we expect all coefficients for all of the predictors to be significant.", 
                "Women will be more likely to take advantage of the opportunity to prepare (collapsed across both conditions), so the coefficient for gender will be significant.",
                "Women will spend more time preparing for the multiplication task relative to men (collapsed across both conditions), so the coefficient for gender will be significant.")

Models <- c(report(primary_hyp1), report(primary_hyp2), report(primary_hyp3)) 

Models <- str_split(Models, '\n\nStandardized', simplify = TRUE)[,1]

Models <- str_remove_all(Models, "\n\n|\n")


study1_models <- data.frame(Hypotheses, Models)

gt_tbl <- gt(study1_models) %>%
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
gt::gtsave(gt_tbl, file = here("study1","figs", "pre-reg-study1.png"))



# creating summary tables -------------------------------------------------

## will use gtsummary - split table by gender 

clean %>% select(task_score, comp_choice, pract_choice, risk, conf_rank,
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
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study1","figs", "summary-table-gender-study1.png"))

clean %>% select(better_gender_guess,perc_task_gender_pract,  perc_gender_comp, perc_gen_gender_pract) %>% tbl_summary(missing= "no") %>% 
  as_gt() %>%  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      style = "solid"
    ),
    locations = cells_body(columns = everything(), rows = everything())
  ) %>%
  tab_header(title = md("")) %>%
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study1","figs", "summary-table-beliefs-study1.png"))


# creating demographic tables by condition ---------------------------------------------

## should recode the people who select more than one option to multi-racial per narratives study

clean %<>% mutate(race_ethnicity = ifelse(str_detect(race_ethnicity, ","), "Selected more than one option", race_ethnicity)) 

clean$race_ethnicity <- dplyr::recode(
  clean$race_ethnicity,
  "Other (please specify)" = "Selected other"
)

fishers <- function(data, variable, by, ...) {
  result <- list()
  result$p <-  stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = T)$p.value
  result$test <- "Fisher's exact test"
  result
}

clean %>%
  dplyr::select(
    condition,
    age,
    gender, 
    race_ethnicity,
    income, 
    education
  ) %>%
  mutate(
    education = factor(
      education,
      levels = c(
        "Less than a high school degree",
        "High School Diploma",
        "Vocational Training",
        "Some College",
        "Bachelor's degree",
        "Graduate Degree"
      )
    ),
    income = factor(
      income,
      levels = c(
        "Less than $10,000",
        "$10,000 to $20,000",
        "$20,000 to $30,000",
        "$30,000 to $40,000",
        "$40,000 to $50,000",
        "$50,000 to $60,000",
        "$60,000 to $70,000",
        "$70,000 to $80,000",
        "$80,000 to $90,000",
        "$90,000 to $100,000",
        "$100,000 to $200,00",
        "Over $200,000"
      )
    )
  ) %>% 
  tbl_summary(
    by = condition,
    label = list(
      gender ~ "Gender",
      age ~ "Age",
      race_ethnicity ~ "Race/ethnicity",
      education ~ "Education",
      income ~ "Household Income"
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
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study1","figs", "demographics-table-conds-study1.png"))

# creating panel plots ----------------------------------------------------

img1 <- magick::image_read(here::here("study1", "figs", "fig01_pract-choice-by-gender-and-comp-choice-bar.png"))
img2 <- magick::image_read(here::here("study1", "figs", "fig07_pract-choice-by-gender.png"))

panel_study1 <- magick::image_append(c(img1, img2))

image_write(panel_study1, path = here::here("study1", "figs", "panel_study1.png"), format = "png")


# plots showing how perceptions & gender predict pract choice -------------

dat <- dplyr::select(clean,gender,pract_choice, perc_task_gender_pract) %>% group_by(perc_task_gender_pract, gender) %>%  drop_na(.) %>% dplyr::count(gender, perc_task_gender_pract, pract_choice) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(pract_choice == "Yes")

p <- ggplot(data = dat, aes(x = perc_task_gender_pract, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Option selected', y = 'Percentage Practicing') +
  scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + theme_apa() +
  scale_x_discrete(labels = c("Men prepare more", "Women prepare more")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study1", "figs", "pract-choice-by-gender-and-perc-task-prep-bar-study1.png"), p, width = 7, height = 7)



# plots showing how perceptions & gender predict pract choice -------------

dat <- dplyr::select(clean,gender,pract_choice, perc_gen_gender_pract) %>% group_by(perc_gen_gender_pract, gender) %>%  drop_na(.) %>% dplyr::count(gender, perc_gen_gender_pract, pract_choice) %>% mutate(percent = n / sum(n), error = sqrt((percent * (1-percent))/n)) %>% filter(pract_choice == "Yes")

p <- ggplot(data = dat, aes(x = perc_gen_gender_pract, fill = gender)) +
  geom_bar(aes(y = percent*100),
           position = "dodge", stat = "identity") + geom_errorbar(aes(ymin =(percent*100)-(error*100), ymax =(percent*100)+(error*100)), width=.05,
                                                                  position=position_dodge(.9)) + 
  geom_text(x = 1.5, y = 100, label = "***") + 
  labs(x = 'Option selected', y = 'Percentage Practicing') +
  scale_fill_manual(values=c("springgreen3", "slateblue1"),  labels = c("Men", "Women")) + theme_apa() +
  scale_x_discrete(labels = c("Men prepare more", "Women prepare more")) + theme(panel.border  = element_blank()) +
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) + theme(
    plot.caption = element_text(hjust = 0, size =14), axis.text.x = element_text(size = 14), legend.text = element_text(size =14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

ggsave(here("study1", "figs", "pract-choice-by-gender-and-perc-gen-prep-bar-study1.png"), p, width = 7, height = 7)

# creating demographic tables by gender ---------------------------------------------

clean %>%
  dplyr::select(
    age,
    gender, 
    race_ethnicity,
    income, 
    education
  ) %>%
  mutate(
    education = factor(
      education,
      levels = c(
        "Less than a high school degree",
        "High School Diploma",
        "Vocational Training",
        "Some College",
        "Bachelor's degree",
        "Graduate Degree"
      )
    ),
    income = factor(
      income,
      levels = c(
        "Less than $10,000",
        "$10,000 to $20,000",
        "$20,000 to $30,000",
        "$30,000 to $40,000",
        "$40,000 to $50,000",
        "$50,000 to $60,000",
        "$60,000 to $70,000",
        "$70,000 to $80,000",
        "$80,000 to $90,000",
        "$90,000 to $100,000",
        "$100,000 to $200,00",
        "Over $200,000"
      )
    )
  ) %>% 
  tbl_summary(
    by = gender,
    label = list(
      age ~ "Age",
      race_ethnicity ~ "Race/ethnicity",
      education ~ "Education",
      income ~ "Household Income"
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
  opt_align_table_header(align = "left") %>% gt::gtsave(., file = here("study1","figs", "demographics-table-gender-study1.png"))

