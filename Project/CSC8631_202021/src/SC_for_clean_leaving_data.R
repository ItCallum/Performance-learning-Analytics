##The following is the code for the cleaning and analysis of the leaving data
## used in the Clean_merged_enrolment_data and my report in the 1st cycle section where I am cleaning my data


## ID variables 

## See if there duplicates in the id colunm
table_dup_ID <- table(duplicated(combine_Leaving$id,fromLast=TRUE))
## See if there duplicates in the learner_id colunm
table_dup_Learner_ID <- table(duplicated(combine_Leaving$learner_id,fromLast=TRUE))

## Get all the rows which cotian where the user ID appers more than once in the dataset
leavers_duplicated_learner_id <- combine_Leaving %>% group_by(learner_id) %>% filter(n() != 1) 

## Get all the user IDs and put them in order 
leavers_duplicated_learner_id[order(leavers_duplicated_learner_id$learner_id),] 

## Get all the rows which cotian a user ID appears more than once per cycle
dup_learner_id_and_cycle <-  leavers_duplicated_learner_id  %>% group_by(learner_id,Cycle) %>% filter(n() != 1)

## Remove all users that have left two or resoponces in the same cycle
removed_duplicate_df <- combine_Leaving %>% 
  group_by(learner_id,Cycle) %>% 
  filter(n()==1)

## how_many_people_left 

## Create a bar chart of how many people leave per cycle 
bar_chart_of_user_per_cycle <- ggplot(combine_Leaving_NoDups, aes(Cycle)) + geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=1)  + ggtitle("How many people left per cycle")

## Reason for leaving

## Work out a percentage of each reason 
reasons_precenatge_df <- as.data.frame(table(combine_Leaving_NoDups$leaving_reason)) %>% mutate(percent = (Freq/sum(Freq)) * 100 ) 

## visualize the perctage
Reason_Prectange <- ggplot(reasons_precenatge_df, aes(x="", y = percent, fill=Var1)) + 
  geom_bar(width = 1, stat = "identity") + ggtitle("The reason why people left")

## A count of each reason why people left 
Reason_Count <- ggplot(combine_Leaving_NoDups, aes(leaving_reason, fill=leaving_reason))  +  geom_bar() + coord_flip() + ggtitle("The reasons why people left") + theme(legend.position = "none" , axis.text=element_text(size=6)) + xlab("Leaving reason")

## A count of each reason why people left but by cycle
Reason_Count_cycle <- ggplot(combine_Leaving_NoDups, aes(leaving_reason , fill=leaving_reason)) + geom_bar() + facet_wrap(~ Cycle, ncol = 2) + 
  coord_flip()  + ggtitle("The reasons why people left per cycle")  + theme(legend.position = "none" , axis.text=element_text(size=6)) + xlab("Leaving reason")

## Did anyone have a last completed week less than there last completed step.

## work out how many people left last_completed_step was some how less than the week number
Anyone_comp_week_less_than_last_step <- na.omit(combine_Leaving_NoDups) %>% filter(last_completed_step < last_completed_week_number) 

## why some many NA

## A new dataframe to do some test on
combine_Leaving_NoDups_Zero <- combine_Leaving_NoDups

## Set all NAs to zero
combine_Leaving_NoDups_Zero[is.na(combine_Leaving_NoDups)] <- 0

## Get percentage of NA rows 
##Na_row_precenatage <- (204 / nrow(combine_Leaving_NoDups_Zero)) * 100 

lcw_table <- table(combine_Leaving_clean$last_completed_week_number)

##A graph for the number of leavers per week
Leavers_perweek_percycle <- ggplot(combine_Leaving_clean, aes(last_completed_week_number, fill = last_completed_week_number)) + geom_bar() + facet_wrap(~ Cycle, ncol = 2) + ggtitle("The amount of leavers last completed week") + theme(legend.position = "none") + xlab("Week") + ylab("Count of leaver responces")

lsc_table <- table(combine_Leaving_clean$last_completed_step)

## As there was alot of zeros I wanted to remove them in order to see the other data more clearly
combine_Leaving_clean_Remove_Zero <- combine_Leaving_clean %>% filter(last_completed_step_number != 0)

## Last step
no_zero_lcs_bar <- ggplot(combine_Leaving_clean_Remove_Zero, aes(last_completed_step)) + geom_bar() + ggtitle("The amount of leavers last completed step")

## Last step per cycle
no_zero_lcs_bar_cycle <-ggplot(combine_Leaving_clean_Remove_Zero, aes(last_completed_step)) + geom_bar() + facet_wrap(~ Cycle, ncol = 2) + ggtitle("The amount of leavers last completed step per cycle")

## How many people left 

Check_Na <- combine_Leaving_NoDups[rowSums(is.na(combine_Leaving_NoDups)) > 0,]

NA_Leaving_reason <- table(Check_Na$leaving_reason)

Check_Na_leaving_reason_bar <- ggplot(Check_Na, aes(leaving_reason, , fill=leaving_reason)) + geom_bar() + coord_flip() + ggtitle("The reason why people left but didnt complete a step") + theme(legend.position = "none" , axis.text=element_text(size=6)) + xlab("Leaving reason")

## The day people left

## Graphs of timelines 
timeline_Cycle_4 <- combine_Leaving_clean %>% filter(Cycle == 4) %>%  ggplot(aes(x=Date, y=Time, color=last_completed_week_number)) + geom_point() +  geom_vline(xintercept = as.Date("2017-11-13"))
timeline_Cycle_5 <- combine_Leaving_clean %>% filter(Cycle == 5) %>%  ggplot(aes(x=Date, y=Time, color=last_completed_week_number)) + geom_point() +  geom_vline(xintercept = as.Date("2018-02-05"))
timeline_Cycle_6 <- combine_Leaving_clean %>% filter(Cycle == 6) %>%  ggplot(aes(x=Date, y=Time, color=last_completed_week_number)) + geom_point() +  geom_vline(xintercept = as.Date("2018-06-11"))
timeline_Cycle_7 <- combine_Leaving_clean %>% filter(Cycle == 7) %>%  ggplot(aes(x=Date, y=Time, color=last_completed_week_number)) + geom_point() + geom_vline(xintercept = as.Date("2018-09-10"))

## main timeline graph of when people left
timeline_full <- ggplot(combine_Leaving_clean, aes(x=Date, y=Time, color=last_completed_week_number),  yaxt = "n") + geom_point()+ facet_wrap(~ Cycle, ncol = 1) + theme(legend.position="bottom")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + ggtitle("Time line of users leaving and there Last completed week") + 
  geom_vline(xintercept = as.Date("2017-11-13")) + annotate("text", x = as.Date("2017-11-13") - 3, y = 4, label = "4",colour = "red") + 
  geom_vline(xintercept = as.Date("2018-02-05")) + annotate("text", x = as.Date("2018-02-05") - 3, y = 4, label = "5",colour = "red") + 
  geom_vline(xintercept = as.Date("2018-06-11")) + annotate("text", x = as.Date("2018-06-11") - 3, y = 4, label = "6",colour = "red") + 
  geom_vline(xintercept = as.Date("2018-09-10")) + annotate("text", x = as.Date("2018-09-10") - 3, y = 4, label = "7",colour = "red")
## Whats going on with the person who left at week 3 before the course even started

before_course <- combine_Leaving_clean %>% filter(Cycle == 6 & Date < as.Date("2018-06-11") )

one_person <- combine_Leaving_clean %>% filter(learner_id == "353eaf1b-cff8-4d9c-98d4-b9a736c17e73") 

## "impossible" leavers 
## look at the users who where able to complete a step after they left.
impos_leaver <- combine_Leaving_clean %>% filter(left_at < last_completed_step_at )