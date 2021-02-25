

## performance from SONA study: 58.35 
n <- 800

avg_perf <-58.35

pay_per_question_comp<- .04

## for the task bonus, multiplying average task performance from respective previous study by the sample size for that specific condition (1/2 for piece-rate, only 1/4 for competition since only half of the half earn it), by the payment for that condition. 
## don't need to multiply/divide by time in seconds since we are using the same amount of time as previous studies.

## so here we're assuming that participants are equally likely to choose the PR as the competition payment scheme. in the end, we end up paying the same amount regardless of the proportion of people who choose PR vs. comp  

bonus_task <- (.25*n*avg_perf*pay_per_question_comp) + (.5*n*avg_perf*(pay_per_question_comp/2))
bonus_task
