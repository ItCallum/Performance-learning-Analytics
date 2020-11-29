cyber_security_4_leaving_survey_responses <- cyber_security_4_leaving_survey_responses %>% mutate(Cycle = 4)
cyber_security_5_leaving_survey_responses <- cyber_security_5_leaving_survey_responses %>% mutate(Cycle = 5)
cyber_security_6_leaving_survey_responses <- cyber_security_6_leaving_survey_responses %>% mutate(Cycle = 6)
cyber_security_7_leaving_survey_responses <- cyber_security_7_leaving_survey_responses %>% mutate(Cycle = 7)

combine_Leaving <- do.call("rbind", list((cyber_security_4_leaving_survey_responses),(cyber_security_5_leaving_survey_responses),
                      (cyber_security_6_leaving_survey_responses),(cyber_security_7_leaving_survey_responses) ))

levels(combine_Leaving$leaving_reason) <- c(levels(combine_Leaving$leaving_reason), "I do not have enough time")
levels(combine_Leaving$leaving_reason) <- c(levels(combine_Leaving$leaving_reason), "The course was not what I expected")
levels(combine_Leaving$leaving_reason) <- c(levels(combine_Leaving$leaving_reason), "The course would not help me reach my goals")

combine_Leaving$leaving_reason[combine_Leaving$leaving_reason == 'I donâ\200\231t have enough time'] <- 'I do not have enough time'
combine_Leaving$leaving_reason[combine_Leaving$leaving_reason == '	The course wasnâ\200\231t what I expected'] <- 'The course was not what I expected'
combine_Leaving$leaving_reason[combine_Leaving$leaving_reason == 'The course wonâ\200\231t help me reach my goals'] <- 'The course would not help me reach my goals'


combine_Leaving$Cycle <- factor(combine_Leaving$Cycle)

combine_Leaving_NoDups <- combine_Leaving %>% group_by(learner_id,Cycle) %>% filter(n()==1)




combine_Leaving_clean <- combine_Leaving_NoDups

combine_Leaving_clean[is.na(combine_Leaving_clean)] <- 0

combine_Leaving_clean <- combine_Leaving_clean %>% mutate(Date = as.Date(left_at))
#output))

combine_Leaving_clean$left_at <- as.POSIXct(combine_Leaving_clean$left_at,format="%Y-%m-%d %H:%M:%S") 

##combine_Leaving_date <- combine_Leaving_date %>% mutate(Time = format(ymd_hms(left_at), format = c("%H:%M:%S")))

combine_Leaving_clean$last_completed_step_at <- as.POSIXct(combine_Leaving_clean$last_completed_step_at,format="%Y-%m-%d %H:%M:%S") 

combine_Leaving_clean <- combine_Leaving_clean %>% mutate(Time = strftime(left_at, format="%H"))


combine_Leaving_clean$last_completed_week_number <- as.factor(combine_Leaving_clean$last_completed_week_number)

combine_Leaving_clean$last_completed_step_number <- as.factor(combine_Leaving_clean$last_completed_step_number)

combine_Leaving_clean$last_completed_step <- as.factor(combine_Leaving_clean$last_completed_step)

combine_Leaving_clean_v2 <- combine_Leaving_clean %>% filter(left_at > last_completed_step_at || is.na(last_completed_step_at))

time_between <- (na.omit(combine_Leaving_clean_v2)) %>% mutate(time_between_deciding_to_leave = difftime(left_at, last_completed_step_at, unit = "mins"), days_passed = floor(difftime(left_at, last_completed_step_at, unit = "days"))) 
