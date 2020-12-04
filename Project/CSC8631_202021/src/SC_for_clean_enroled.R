## The following is the code for the cleaning and analysis of the the merged enrollment data
## used in the Clean_merged_enrolment_data and my report in the 1st cycle section where I am cleaning my data

## - How many people didn't leave the course 
## Work out how many people enrolled onto the course and how many how people gave a reason for leaving the course.
leave_V_complete <- data.frame(table(combine_Enrolments$Cycle),table(combine_Leaving_clean_v2$Cycle) ) %>% mutate(Completed = Freq - Freq.1 , Percenatge = (Freq.1 / Freq)* 100 ) %>% 
  rename(Cycle = Var1, Leavers = Freq.1,Total_reg = Freq)
drop <- c("Var1.1")
df_leave_enrolled = leave_V_complete[,!(names(leave_V_complete) %in% drop)]

##Get the number of rows that didnt have a unenrolled at time
How_many_unerolled <- sum(combine_Enrolments$unenrolled_at != "")

## - Detected country
##Used to work out if the contry said in detected_country matched with all the rows that had a value in country
df_same_country <- merged_data %>% filter(country != "Unknown") %>% mutate(same_country = ifelse(detected_country == country, TRUE, FALSE) ) 

## get a count of the trues and falses
df_same_country_count <- df_same_country %>% count(same_country)

##Get all the rows where the detected_country was not the same as the given country
diffrent_country_results <- df_same_country %>% filter(same_country == FALSE )

## - Why so many Unenrolled when compared to the users that are 



##Time line of Unerollment times compared to leaver responces
timeline_full_Unenrolled <- ggplot(Unenrolled_combined, aes(x=Date, y=Time),  yaxt = "n") +  geom_point() + geom_point(data = merged_data, colour = "red") + facet_wrap(~ Cycle, ncol = 1) + theme(legend.position="bottom") +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + ggtitle("Visual comparison of unenrolled data vs leaver responce") +  
  geom_vline(xintercept = as.Date("2017-11-13")) + annotate("text", x = as.Date("2017-11-13") - 3, y = 4, label = "4",colour = "blue") + 
  geom_vline(xintercept = as.Date("2018-02-05")) + annotate("text", x = as.Date("2018-02-05") - 3, y = 4, label = "5",colour = "blue") + 
  geom_vline(xintercept = as.Date("2018-06-11")) + annotate("text", x = as.Date("2018-06-11") - 3, y = 4, label = "6",colour = "blue") + 
  geom_vline(xintercept = as.Date("2018-09-10")) + annotate("text", x = as.Date("2018-09-10") - 3, y = 4, label = "7",colour = "blue")

## The time and dates of user enrolment
Unenrolled_combined <- Unenrolled_combined %>% mutate(enrolled_Date = as.Date(enrolled_at))

Unenrolled_combined <- Unenrolled_combined %>% mutate(enrolled_Time = strftime(enrolled_at, format="%H"))

## Time line of user enrollmenyts
Enrollment_dates <- ggplot(Unenrolled_combined, aes(x=enrolled_Date, y=enrolled_Time),  yaxt = "n") +  geom_point()  + facet_wrap(~ Cycle, ncol = 1) + theme(legend.position="bottom") +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + ggtitle("Enrollment dates") +   geom_vline(xintercept = as.Date("2017-11-13")) + annotate("text", x = as.Date("2017-11-13") - 3, y = 4, label = "4",colour = "blue") + 
  geom_vline(xintercept = as.Date("2018-02-05")) + annotate("text", x = as.Date("2018-02-05") - 3, y = 4, label = "5",colour = "blue") + 
  geom_vline(xintercept = as.Date("2018-06-11")) + annotate("text", x = as.Date("2018-06-11") - 3, y = 4, label = "6",colour = "blue") + 
  geom_vline(xintercept = as.Date("2018-09-10")) + annotate("text", x = as.Date("2018-09-10") - 3, y = 4, label = "7",colour = "blue")

## - Two leaving dates.

 has_NA <- subset(merged_data, is.na(merged_data$unenrolled_at)) %>% select(left_at , unenrolled_at, Cycle, last_completed_week_number,last_completed_step)

 
## - IS there duplicates in the enrole data
 
 ## Check to find if there is any duplicates in leaner ID
 is_there_duplicates <- combine_Enrolments %>% filter(learner_id %in% combine_Leaving_clean_v2$learner_id) 
 
 ## Get the users with the duplicates
 duplicated_learner_id <- is_there_duplicates %>% group_by(learner_id) %>% filter(n() != 1) 
 
 ##order these duplicated values
 dup_userid_enrollement <- duplicated_learner_id[order(duplicated_learner_id$learner_id),] 
 
 same_cycle_dupliacted_enrollement <- duplicated_learner_id  %>% group_by(learner_id,Cycle) %>% filter(n() != 1)