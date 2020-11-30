options(scipen=999)

## The reason why a person left the course and there last completed step

percenatge_table <- as.data.frame(table(combine_Leaving_clean_v2$leaving_reason, combine_Leaving_clean_v2$last_completed_week_number)) %>% group_by(Var2) %>% mutate(percent = (Freq/sum(Freq)) * 100 ) 

Correlation_Between_leaving_LCWN <- chisq.test(table(combine_Leaving_clean_v2$leaving_reason, combine_Leaving_clean_v2$last_completed_week_number))

leaving_reason_lcwn <- ggplot(combine_Leaving_clean_v2, aes(leaving_reason), fill=leaving_reason) + geom_bar() + 
  coord_flip() + ggtitle("The reason why people left") + facet_wrap(~ last_completed_week_number)


leaving_reason_lcwn_percenatge <- ggplot(percenatge_table, aes(x="", y = percent, fill=Var1)) + 
  geom_bar(width = 1, stat = "identity") + ggtitle("The reason why people left") + facet_wrap(~ Var2)


##Time between leaving 

Density_Leave_time <- ggplot(time_between, aes(x=as.numeric(time_between$time_between_deciding_to_leave))) + geom_density() + ggtitle("Density of time between last step and leaving in minuets")


Histogram_Leave_time <- ggplot(time_between, aes(x=as.numeric(days_passed))) +
  geom_histogram(binwidth=1, colour="black", fill="white") + ggtitle("Histogram of days passed between a users last step and the time the offically left")

Leave_time_reason <- ggplot(time_between %>% filter(days_passed == 0), aes(leaving_reason)) + geom_bar() + 
  coord_flip()  + ggtitle("The reason why people left within a day")

## Step 32

Step_32 <- combine_Leaving_clean_v2 %>% filter(last_completed_step == 3.2)

Reason_Step_32 <- ggplot(Step_32, aes(leaving_reason), fill=leaving_reason) + geom_bar() + 
  coord_flip() + ggtitle("The reason why people left at step 32")

Reason_cycle_Step_32 <- ggplot(Step_32, aes(leaving_reason), fill=leaving_reason) + geom_bar() + 
  coord_flip() + ggtitle("The reason why people left at step 32") + facet_wrap(~ Cycle)

Reason_cycle_Step_32 <- ggplot(Step_32, aes(x=Date, y=Time, color=leaving_reason),  yaxt = "n") + geom_point()+ facet_wrap(~ Cycle, ncol = 1) + theme(legend.position="bottom")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + ggtitle("Time line of users leaving and there Last completed week") +  geom_vline(xintercept = as.Date("2017-11-13")) +  geom_vline(xintercept = as.Date("2018-02-05")) +  geom_vline(xintercept = as.Date("2018-06-11")) + geom_vline(xintercept = as.Date("2018-09-10"))

time_32_between <- time_between %>% filter(last_completed_step == 3.2)

time_32_between_hist <- ggplot(time_between %>% filter(last_completed_step == 3.2), aes(x=as.numeric(days_passed))) +
  geom_histogram(binwidth=1, colour="black", fill="white") + ggtitle("Histogram of days passed between a users last step and the time the offically left")