
## How many people didnt leave the course 
leave_V_complete <- data.frame(table(combine_Enrolments$Cycle),table(combine_Leaving_clean_v2$Cycle) ) %>% mutate(Completed = Freq - Freq.1 , Percenatge = (Freq.1 / Freq)* 100 ) %>% 
  rename(Cycle = Var1, Leavers = Freq.1,Total_reg = Freq)

drop <- c("Var1.1")
df_leave_enrolled = leave_V_complete[,!(names(leave_V_complete) %in% drop)]

How_many_unerolled <- sum(combine_Enrolments$unenrolled_at != "")

##Detected country

df_same_country <- merged_data %>% filter(country != "Unknown") %>% mutate(same_country = ifelse(detected_country == country, TRUE, FALSE) ) 

df_same_country_count <- df_same_country %>% count(same_country)

diffrent_country_results <- df_same_country %>% filter(same_country == FALSE )


## Two leaving dates.

 has_NA <- subset(merged_data, is.na(merged_data$unenrolled_at)) %>% select(left_at , unenrolled_at, Cycle, last_completed_week_number,last_completed_step)

 diffrent_leaving_times <-merged_data %>% filter(left_at != unenrolled_at) %>% select(left_at , unenrolled_at, Cycle, last_completed_week_number)
 
 
 ##IS there duplicates in the enrole data
 
 is_there_duplicates <- combine_Enrolments %>% filter(learner_id %in% combine_Leaving_clean_v2$learner_id) 
 
 duplicated_learner_id <- is_there_duplicates %>% group_by(learner_id) %>% filter(n() != 1) 
 
 dup_userid_enrollement <- duplicated_learner_id[order(duplicated_learner_id$learner_id),] 
 
 same_cycle_dupliacted_enrollement <- duplicated_learner_id  %>% group_by(learner_id,Cycle) %>% filter(n() != 1)