options(scipen=999)

## The reason why a person left the course and there last completed step

##Create a table of all the percentage of leaving_reason in each week
percenatge_table <- as.data.frame(table(combine_Leaving_clean_v2$leaving_reason, combine_Leaving_clean_v2$last_completed_week_number)) %>% group_by(Var2) %>% mutate(percent = (Freq/sum(Freq)) * 100 ) 

## Find the correlation between the leaving reason and the last completed week number
Correlation_Between_leaving_LCWN <- chisq.test(table(combine_Leaving_clean_v2$leaving_reason, combine_Leaving_clean_v2$last_completed_week_number))

## A bar chart of the counts of each leaving_reason for each week
leaving_reason_lcwn <- ggplot(combine_Leaving_clean_v2, aes(leaving_reason), fill=leaving_reason) + geom_bar() + 
  coord_flip() + ggtitle("The reason why people left") + facet_wrap(~ last_completed_week_number)

## A bar chart of the percentage reason of each leaving_reason for each week
leaving_reason_lcwn_percenatge <- ggplot(percenatge_table, aes(x="", y = percent, fill=Var1)) + 
  geom_bar(width = 1, stat = "identity") + ggtitle("The reason why people left in each Week") + facet_wrap(~ Var2) + labs(fill = "Reason") +  theme(legend.text = element_text(size = 6))


##Time between leaving 

## A Density_Leave_time graph of how long it took a person to leave the course in minuets
Density_Leave_time <- ggplot(time_between, aes(x=as.numeric(time_between$time_between_deciding_to_leave))) + geom_density() + ggtitle("Density of time between last step and leaving in minuets")

## A histogram of how many days passed between when a person last fished a step and then left the course    
Histogram_Leave_time <- ggplot(time_between, aes(x=as.numeric(days_passed))) +
  geom_histogram(binwidth=1, colour="black", fill="white") + ggtitle("Histogram of days passed between a users last step and the time the offically left")

## A barchart of reasons left of the people who finished a step and then left within a day 
Leave_time_reason <- ggplot(time_between %>% filter(days_passed == 0), aes(leaving_reason, fill=leaving_reason)) + geom_bar() + 
  coord_flip()  + ggtitle("The reason why people left within a day") + theme(legend.position = "none" , axis.text=element_text(size=6)) + xlab("Leaving reason")

## Step 32

##Get all the people who left at setp 3.2
Step_32 <- combine_Leaving_clean_v2 %>% filter(last_completed_step == 3.2)

##Plot all the reasons for leaving at setp 3.2
Reason_Step_32 <- ggplot(Step_32, aes(leaving_reason, fill=leaving_reason)) + geom_bar() + 
  coord_flip() + ggtitle("The reason why people left at step 3.2") +  theme(legend.position = "none" , axis.text=element_text(size=6)) + xlab("Leaving reason")

##Plot all the reasons for leaving at setp 3.2 and put it by cycle
Reason_cycle_Step_32 <- ggplot(Step_32, aes(leaving_reason), fill=leaving_reason) + geom_bar() + 
  coord_flip() + ggtitle("The reason why people left at step 32") + facet_wrap(~ Cycle)

## A time line of people leaving at step 3.2
Reason_cycle_Step_32 <- ggplot(Step_32, aes(x=Date, y=Time, color=leaving_reason),  yaxt = "n") + geom_point()+ facet_wrap(~ Cycle, ncol = 1) + theme(legend.position="bottom")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + ggtitle("Time line of users leaving and there Last completed week") +  geom_vline(xintercept = as.Date("2017-11-13")) +  geom_vline(xintercept = as.Date("2018-02-05")) +  geom_vline(xintercept = as.Date("2018-06-11")) + geom_vline(xintercept = as.Date("2018-09-10"))+
  geom_vline(xintercept = as.Date("2017-11-13")) + annotate("text", x = as.Date("2017-11-13") - 3, y = 4, label = "4",colour = "red") + 
  geom_vline(xintercept = as.Date("2018-02-05")) + annotate("text", x = as.Date("2018-02-05") - 3, y = 4, label = "5",colour = "red") + 
  geom_vline(xintercept = as.Date("2018-06-11")) + annotate("text", x = as.Date("2018-06-11") - 3, y = 4, label = "6",colour = "red") + 
  geom_vline(xintercept = as.Date("2018-09-10")) + annotate("text", x = as.Date("2018-09-10") - 3, y = 4, label = "7",colour = "red")

## Filter the time between graph that will be used to see who long it took between a person completing step 3.2 and why they left
time_32_between <- time_between %>% filter(last_completed_step == 3.2)

## Plot the histogrpah of days passed between doing step 3.2 and leaving.
time_32_between_hist <- ggplot(time_between %>% filter(last_completed_step == 3.2), aes(x=as.numeric(days_passed))) +
  geom_histogram(binwidth=1, colour="black", fill="white") + ggtitle("Histogram of days passed between a users last step and the time the offically left")