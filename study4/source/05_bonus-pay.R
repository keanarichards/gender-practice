# load packages -----------------------------------------------------------

if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, here, hablar, psych, qualtRics, varhandle, magrittr)

# load data ---------------------------------------------------------------

clean <- read_csv(here("study4", "data", "clean.csv"))


# additional questions  ---------------------------------------------------


## main qs to be included & their answers: 
# Do you think men or women in this study correctly solved more multiplication problems on average?

woman <- clean %>% filter(gender == "Woman")
man <- clean %>% filter(gender == "Man")

t.test(woman$task_score, man$task_score)

## men tend to have higher scores on avg

# Do you think men or women in this study chose the tournament payment option more often?

summary(glm(factor(comp_choice) ~ gender,family=binomial,data = clean))

## women are significantly less likely to compete

# Do you think men or women spent more time practicing/studying before multiplication task? (only practice condition)

## no strong evidence of gender diff in this study  

# Which task do you think was more helpful in boosting participants’ scores on the paid multiplication task?

practice <- clean %>% filter(condition == "pract")
control <- clean %>% filter(condition == "control")

t.test(practice$task_score, control$task_score)

## no difference in performance across tasks 

# Your task score was ${e://Field/task_score}. If my performance is compared to that of all participants that completed the task, I think my score will be...

clean <- clean %>% 
  mutate(percent_rank = percent_rank(task_score), conf_rank_decimal = clean$conf_rank/100) %>% drop_na(conf_rank)

clean$conf_accuracy <- 0


for (i in 1:nrow(clean)) {
  if (clean$percent_rank[i] >= clean$conf_rank_decimal[i]) 
  {clean$conf_accuracy[i] <- "correct"
  
  } else if (clean$percent_rank [i] < clean$conf_rank_decimal[i]) 
  {clean$conf_accuracy[i] <- "incorrect"
  }
}


# randomly selecting bonus question ---------------------------------------

## making sure to separate random selection based on condition bc ppts in practice condition has extra q

# clean <- clean[!duplicated(clean$workerId),]
# 
# clean <-clean[sample(1:nrow(clean), replace = F), ]
# clean$bonus_assn <- 0
# 
# clean$bonus_assn <- ifelse(clean$condition == "pract", rep(1:5, length.out = nrow(clean)), rep(1:4, length.out = nrow(clean)))


total_payment <- total_payment[!duplicated(total_payment$workerId),]
set.seed(100)
total_payment <-total_payment[sample(1:nrow(total_payment), replace = F), ]
total_payment$bonus_assn <- 0

total_payment$bonus_assn <- ifelse(total_payment$condition == "pract", rep(1:5, length.out = nrow(total_payment)), rep(1:4, length.out = nrow(total_payment)))

## conf_accuracy q == 1; perc_task_gender_pract == 2; better_gender_guess == 3; perc_gender_comp == 4; MC = 5
## "correct" ==1, "No difference" == 2; "Men" == 3; "Men" == 4; "No difference" == 5
total_payment$bonus_qs <- ifelse(total_payment$conf_accuracy == "correct" & total_payment$bonus_assn == 1, .1, 0)
total_payment$bonus_qs <- ifelse(total_payment$perc_task_gender_pract == "No difference" & total_payment$bonus_assn == 2, .1, total_payment$bonus_qs)
total_payment$bonus_qs <- ifelse(total_payment$better_gender_guess == "Men" & total_payment$bonus_assn == 3, .1, total_payment$bonus_qs)
total_payment$bonus_qs <- ifelse(total_payment$perc_gender_comp == "Men" & total_payment$bonus_assn == 4, .1, total_payment$bonus_qs)
total_payment$bonus_qs <- ifelse(total_payment$MC == "No difference" & total_payment$bonus_assn == 5, .1, total_payment$bonus_qs)

## summing bonus q pay & task pay 
total_payment$total_bonus <- total_payment$bonus_qs + total_payment$combined_pay 




# main task payoffs -------------------------------------------------------

clean %<>% drop_na(task_score)

### filtered based on piece rate choice
PR_choice <- subset(clean, comp_choice == "piecerate", select = c(workerId, task_score))

## multiply by .10

PR_choice$PR_pay <- PR_choice$task_score*.10


### filtered based on choice for tournament
tourn_choice <- subset(clean, comp_choice == "tournament", select = c(workerId, task_score))

#### randomly sort rows of DF

set.seed(100)
tourn_choice_even <- tourn_choice %>% slice_sample(n = nrow(tourn_choice)-1)


set.seed(100)
tourn_choice_even <-tourn_choice_even[sample(1:nrow(tourn_choice_even), replace = F), ]

## making dataset even by randomly selecting ppt to be removed (to be inserted & randomly matched with partner later)


first_half <- nrow(tourn_choice_even)/2
second_half_start <- nrow(tourn_choice_even)/2 + 1
second_half_end <- nrow(tourn_choice_even)



### split DF in half 
tourn_choice_split1 <- tourn_choice_even[1:first_half, c("workerId", "task_score")]
tourn_choice_split2 <- tourn_choice_even[second_half_start:second_half_end, c("workerId", "task_score")]

tourn_choice_split1$ID <- c(1:first_half)
tourn_choice_split2$ID <- c(1:first_half)

combined2 <- merge (tourn_choice_split1, tourn_choice_split2, by = "ID")

### subtract round scores to determine winner 

combined2$difference_score <- combined2$task_score.x - combined2$task_score.y

###ID winners and put back in main dataset

difference_score <- combined2$difference_score
difference_score2 <- rep (difference_score)


difference_score <- append(difference_score, difference_score2)
tourn_choice_even$difference_score <- difference_score

tourn_choice_even$W_L <- "NA"


for (i in 1:first_half) {
  if (tourn_choice_even$difference_score [i]> 0) {tourn_choice_even$W_L[i] <- "win"
  
  } else if (tourn_choice_even$difference_score [i]< 0)
  {tourn_choice_even$W_L[i] <- "lose"
  
  } else if (tourn_choice_even$difference_score [i] == 0) 
  {tourn_choice_even$W_L[i] <- "tie"}
  
}


for (i in second_half_start:second_half_end) {
  if (tourn_choice_even$difference_score [i]> 0) {tourn_choice_even$W_L[i] <- "lose"
  
  } else if (tourn_choice_even$difference_score [i]< 0)
  {tourn_choice_even$W_L[i] <- "win"
  
  } else if (tourn_choice_even$difference_score [i] == 0) 
  {tourn_choice_even$W_L[i] <- "tie"}
  
}


## not needed because there aren't any ties  
# tourn_choice_even <- tourn_choice_even[-sample(1:sum(tourn_choice_even$W_L == "tie"), size = sum(tourn_choice_even$W_L == "tie")/2, replace = F),]


## randomly select partner for odd one out 
OOO <- anti_join(tourn_choice, tourn_choice_even)
set.seed(100)
## randomly selecting partner for them
tourn_choice_even %>% slice_sample(n =1)
## they win tournament: opponent ID = ATUA5M7PLS8W; score = 22, adding as row to dataset

tourn_winners <- tourn_choice_even %>% filter(W_L == "win"| W_L == "tie") %>% add_row(workerId = OOO$workerId, task_score = OOO$task_score, difference_score = 78, W_L = "win")

tourn_winners$tourn_payment <- 0

tourn_winners$tourn_payment <- tourn_winners$task_score*.20

##  Merging based on worker ID:

total_payment <- left_join(clean, PR_choice,by = "workerId")
total_payment <-left_join(total_payment, tourn_winners, by = "workerId")

## merging into one column
total_payment$combined_pay <- coalesce(total_payment$tourn_payment, total_payment$PR_pay)
## inserting $0 payment for people who lose tournaments

total_payment$combined_pay <- ifelse(is.na(total_payment$combined_pay), 0, total_payment$combined_pay)

total_payment %<>% filter(combined_pay > 0) %>% select(workerId, combined_pay)

write_csv(total_payment,here("study4", "data", 'total_payment_output.csv'), col_names = F)


## estimating total bonus costs including MTurk fees: 

sum(total_payment$combined_pay)*.20 + sum(total_payment$combined_pay)


# splitting workers into groups for cloudresearch  ------------------------

pay <- read_csv(here("study4", "data", 'total_payment_output.csv'))

pay %>% slice(1:100) %>% write_csv(.,here("study4", "data", 'first.csv'), col_names = F)
pay %>% slice(101:200) %>% write_csv(.,here("study4", "data", 'second.csv'), col_names = F)
pay %>% slice(201:300) %>% write_csv(.,here("study4", "data", 'third.csv'), col_names = F)
pay %>% slice(301:400) %>% write_csv(.,here("study4", "data", 'fourth.csv'), col_names = F)
pay %>% slice(401:500) %>% write_csv(.,here("study4", "data", 'fifth.csv'), col_names = F)
pay %>% slice(501:600) %>% write_csv(.,here("study4", "data", 'sixth.csv'), col_names = F)
pay %>% slice(601:700) %>% write_csv(.,here("study4", "data", 'seventh.csv'), col_names = F)
pay %>% slice(701:800) %>% write_csv(.,here("study4", "data", 'eighth.csv'), col_names = F)
pay %>% slice(801:900) %>% write_csv(.,here("study4", "data", 'ninth.csv'), col_names = F)
pay %>% slice(901:1000) %>% write_csv(.,here("study4", "data", 'tenth.csv'), col_names = F)
pay %>% slice(1001:1027) %>% write_csv(.,here("study4", "data", 'eleventh.csv'), col_names = F)


