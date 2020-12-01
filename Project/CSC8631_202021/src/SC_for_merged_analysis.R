

Unenrolled_combined <- combine_Enrolments[!(is.na(combine_Enrolments$unenrolled_at) | combine_Enrolments$unenrolled_at==""), ]

## Gender

leave_V_complete_gender <- data.frame(table(combine_Enrolments$gender) ,table(Unenrolled_combined$gender) ,table(merged_data$gender) ) %>% 
  rename(Gender = Var1, Total_reg = Freq, unenrolled = Freq.1, Leavers = Freq.2) %>% mutate(PercenatgeLeft = (unenrolled / Total_reg)* 100 , PercenatgeComplained = (Leavers / unenrolled)* 100  )

drop <- c("Var1.1","Var1.2")
leave_V_complete_gender = leave_V_complete_gender[,!(names(leave_V_complete_gender) %in% drop)]

Reason_gender<- ggplot(merged_data, aes(leaving_reason)) + geom_bar() + facet_wrap(~ gender, ncol = 2) + 
  coord_flip()  + ggtitle("The reasons why people left per - gender")

## Age range

leave_V_complete_age_range <- data.frame(table(combine_Enrolments$age_range) ,table(Unenrolled_combined$age_range) ,table(merged_data$age_range) ) %>% 
  rename(age_range = Var1, Total_reg = Freq, unenrolled = Freq.1, Leavers = Freq.2) %>% mutate(PercenatgeLeft = (unenrolled / Total_reg)* 100 , PercenatgeComplained = (Leavers / unenrolled)* 100  )

drop <- c("Var1.1","Var1.2")
leave_V_complete_age_range = leave_V_complete_age_range[,!(names(leave_V_complete_age_range) %in% drop)]


Reason_age_range <- ggplot(merged_data, aes(leaving_reason)) + geom_bar() + facet_wrap(~ age_range, ncol = 4) + 
  coord_flip()  + ggtitle("The reasons why people left per cycle by gender")

## employment status

leave_V_complete_employment_status <- data.frame(table(combine_Enrolments$employment_status) ,table(Unenrolled_combined$employment_status) ,table(merged_data$employment_status) ) %>% 
  rename(age_range = Var1, Total_reg = Freq, unenrolled = Freq.1, Leavers = Freq.2) %>% mutate(PercenatgeLeft = (unenrolled / Total_reg)* 100 , PercenatgeComplained = (Leavers / unenrolled)* 100  )

drop <- c("Var1.1","Var1.2")
leave_V_complete_employment_status = leave_V_complete_employment_status[,!(names(leave_V_complete_employment_status) %in% drop)]


Reason_gender_employment_status <- ggplot(merged_data, aes(leaving_reason)) + geom_bar() + facet_wrap(~ employment_status, ncol = 3) + 
  coord_flip()  + ggtitle("The reasons why people left per cycle by gender")

## Time between enrolling and leaving

time_bet_enroll_leave <- merged_data %>% mutate(time_to_leave = floor(difftime( left_at, enrolled_at, unit = "days")), time_to_leave_min = difftime( left_at, enrolled_at, unit = "hours"))

summary(as.numeric(time_bet_enroll_leave$time_to_leave))

time_to_leave_min <- ggplot(time_bet_enroll_leave, aes(x = "" , y=as.numeric(time_to_leave))) +
  geom_violin() + geom_boxplot(width = 0.5) +  ggtitle("Histogram of days passed between a users last step and the time the offically left")

time_to_leave_min

Histogram_time <- ggplot(time_bet_enroll_leave, aes(x = as.numeric(time_to_leave))) +
  geom_histogram(binwidth=1, colour="black", fill="white") + ggtitle("Histogram of days passed between a users last step and the time the offically left")

Histogram_time

Reason_day_one <- ggplot(time_bet_enroll_leave %>% filter(time_to_leave == 0)
                         , aes(leaving_reason)) + geom_bar() +  
  coord_flip()  + ggtitle("The reasons why people left with a day")

Reason_day_one