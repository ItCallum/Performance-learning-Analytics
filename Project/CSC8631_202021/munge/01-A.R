
## Add an cycle ID to say what cycle each row came from
cyber_security_4_leaving_survey_responses <- cyber_security_4_leaving_survey_responses %>% mutate(Cycle = 4)
cyber_security_5_leaving_survey_responses <- cyber_security_5_leaving_survey_responses %>% mutate(Cycle = 5)
cyber_security_6_leaving_survey_responses <- cyber_security_6_leaving_survey_responses %>% mutate(Cycle = 6)
cyber_security_7_leaving_survey_responses <- cyber_security_7_leaving_survey_responses %>% mutate(Cycle = 7)

## Combined each of the leaving response dataset to make one big data set the contains every possible leavers response
combine_Leaving <- do.call("rbind", list((cyber_security_4_leaving_survey_responses),(cyber_security_5_leaving_survey_responses),
                      (cyber_security_6_leaving_survey_responses),(cyber_security_7_leaving_survey_responses) ))

## There is a weird translation error in the users response that this code attepts to get rid of
levels(combine_Leaving$leaving_reason) <- c(levels(combine_Leaving$leaving_reason), "I do not have enough time")
levels(combine_Leaving$leaving_reason) <- c(levels(combine_Leaving$leaving_reason), "The course was not what I expected")
levels(combine_Leaving$leaving_reason) <- c(levels(combine_Leaving$leaving_reason), "The course would not help me reach my goals")

combine_Leaving$leaving_reason[combine_Leaving$leaving_reason == 'I donâ\200\231t have enough time'] <- 'I do not have enough time'
combine_Leaving$leaving_reason[combine_Leaving$leaving_reason == '	The course wasnâ\200\231t what I expected'] <- 'The course was not what I expected'
combine_Leaving$leaving_reason[combine_Leaving$leaving_reason == 'The course wonâ\200\231t help me reach my goals'] <- 'The course would not help me reach my goals'


## Convert the the cycle number to be a factor instead of a 
combine_Leaving$Cycle <- factor(combine_Leaving$Cycle)

## Remove all users that have left two or resoponces in the same cycle
combine_Leaving_NoDups <- combine_Leaving %>% group_by(learner_id,Cycle) %>% filter(n()==1)

## Make a duplicate of combine_Leaving_NoDups called combine_Leaving_clean which the dataset that I am going to do all of my cleaning functions on
combine_Leaving_clean <- combine_Leaving_NoDups

## Set all NA values to zero, This is based of the assumption that all NA results mean that the user didnt start the course.
combine_Leaving_clean[is.na(combine_Leaving_clean)] <- 0

## Change the date from a character to a actual data value and store the date as a new value called Date
combine_Leaving_clean <- combine_Leaving_clean %>% mutate(Date = as.Date(left_at))
#output))

## Change left_at to be a POSIXct time and not a character
combine_Leaving_clean$left_at <- as.POSIXct(combine_Leaving_clean$left_at,format="%Y-%m-%d %H:%M:%S") 

##combine_Leaving_date <- combine_Leaving_date %>% mutate(Time = format(ymd_hms(left_at), format = c("%H:%M:%S")))

## Change last_completed_step_at to be a POSIXct time and not a character
combine_Leaving_clean$last_completed_step_at <- as.POSIXct(combine_Leaving_clean$last_completed_step_at,format="%Y-%m-%d %H:%M:%S") 

## Get the hour time that the user left at
combine_Leaving_clean <- combine_Leaving_clean %>% mutate(Time = strftime(left_at, format="%H"))

## Change last_completed_week_number to be a factor
combine_Leaving_clean$last_completed_week_number <- as.factor(combine_Leaving_clean$last_completed_week_number)

## Change last_completed_step_number to be a factor
combine_Leaving_clean$last_completed_step_number <- as.factor(combine_Leaving_clean$last_completed_step_number)

## Change last_completed_step to be a factor
combine_Leaving_clean$last_completed_step <- as.factor(combine_Leaving_clean$last_completed_step)

## The creation of the final version of my cleaned leaving data base. I filtered out the any person who was able to leave the course before finishing a final step.
combine_Leaving_clean_v2 <- combine_Leaving_clean %>% filter(left_at > last_completed_step_at || is.na(last_completed_step_at))

## Creation of a sub database that was used for one test. It works out the time between when a user finshed there last step and when they decide to leave in both minuets and how many days passed. 
time_between <- (na.omit(combine_Leaving_clean_v2)) %>% mutate(time_between_deciding_to_leave = difftime(left_at, last_completed_step_at, unit = "mins"), days_passed = floor(difftime(left_at, last_completed_step_at, unit = "days"))) 


# - Cycle 2


## The creation of my second database for the second cycle analysis
## Like the first dataset cycle ID to say what cycle each row came from
cyber_security_4_enrolments <- cyber_security_4_enrolments  %>% mutate(Cycle = 4)
cyber_security_5_enrolments <- cyber_security_5_enrolments  %>% mutate(Cycle = 5)
cyber_security_6_enrolments <- cyber_security_6_enrolments  %>% mutate(Cycle = 6)
cyber_security_7_enrolments <- cyber_security_7_enrolments  %>% mutate(Cycle = 7)

##Combind all relevant data on user Enrolments to make one large data set.
combine_Enrolments <- do.call("rbind", list((cyber_security_4_enrolments),(cyber_security_5_enrolments),
                                         (cyber_security_6_enrolments),(cyber_security_7_enrolments) ))


## change the enrolled_at time to POSIXct
combine_Enrolments$enrolled_at <- as.POSIXct(combine_Enrolments$enrolled_at,format="%Y-%m-%d %H:%M:%S") 

##combine_Enrolments$unenrolled_at <- as.POSIXct(combine_Enrolments$unenrolled_at,format="%Y-%m-%d %H:%M:%S") 


## Merge my combine_Enrolments onto my combine_Leaving_clean_v2  dataset (so leaver response) by cycle and user ID.
## This is so we can get all the useful enrollment data realting to each users row
merged_data <- merge(combine_Leaving_clean_v2,combine_Enrolments,by=c("learner_id", "Cycle" ))

