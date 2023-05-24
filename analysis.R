library(data.table)
library(dplyr)
library(plotly)
library(lubridate)
library(xlsx)

path <- '../dataset Data Analyst Tidio.csv'
data <- setDT(read.csv(path))

#### add additional columns ####

data$TicketCreatedDate <- ymd(data$TicketCreatedDate)
data$TicketCreatedMonth <- format(as.Date(data$TicketCreatedDate), "%Y-%m")
data$TicketCreatedWeekNumber <- lubridate::isoweek(ymd(data$TicketCreatedDate))
data[,TicketCreatedWeek_nchar := nchar(TicketCreatedWeekNumber)]
data[, TicketCreatedWeekNumber_new := 
       ifelse(TicketCreatedWeek_nchar == 1,
              paste0("0", data$TicketCreatedWeekNumber),
              data$TicketCreatedWeekNumber)
     ]
data$TicketCreatedYear <- year(data$TicketCreatedDate)
data$TicketCreatedWeekNumberYear <- paste("W", data$TicketCreatedWeekNumber_new, year(data$TicketCreatedDate)) 
# force weekdays in english
Sys.setlocale("LC_TIME", "C")
data$TicketCreatedWeekday <-  weekdays(data$TicketCreatedDate)
data$is_solved <- !is.na(data$FullResolutionTime)
data$filled_customer_satisfaction <-!is.na(data$CustomerSatisfaction)
data[, is_assigned_contigent_worker := ifelse(Worker_Type == 'Contingent Worker', 1, 0)]



#### calculate main KPIs ####

# average daily tickets
daily_tickets_per_employee <- 
  data %>%
  group_by(TicketCreatedDate,
           TicketCreatedMonth,
           TicketCreatedWeekday,
           EmployeeID,
           Worker_Type) %>%
  summarise(no_tickets = n())
  
avg_daily_tickets <- 
  daily_tickets_per_employee %>%
  group_by(TicketCreatedMonth) %>%
  summarise(avg_new_tickets_daily_per_employee = mean(no_tickets))

total_avg_daily_tickets <- 
  mean(daily_tickets_per_employee$no_tickets)


KPIs_monthly <- data %>% 
  group_by(TicketCreatedMonth) %>%
  summarise(no_tickets = n(), 
            no_tickets_solved = sum(is_solved),
            no_tickets_unsolved = sum(!is_solved),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            escalation_rate = sum(TicketEscalated) / n(),
            CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100,
            CSAT_response_rate = (sum(filled_customer_satisfaction) / sum(is_solved)) * 100
            )

KPIs_monthly <- left_join(KPIs_monthly,
                          avg_daily_tickets, 
                          by = "TicketCreatedMonth")



#### Trends in time ####

# monthly


plot_tickets <- 
  plot_ly(KPIs_monthly, 
        x = ~TicketCreatedMonth, 
        y = ~round(avg_new_tickets_daily_per_employee, 2),
        type = 'scatter',
        mode = 'lines',
        name = 'avg. new tickets / day / employee'
        ) 

plot_tickets %>%  
  layout(yaxis = list(title = 'Avg. new tickets/day/employee'),
         xaxis = list(title = 'Ticket Created Month'),
         #horizontal line - benchamark
         shapes = list(
           type = "line",
           x0 = 0,
           x1 = 1,
           xref = "paper",
           y0 = 8,
           y1 = 8,
           line = list(color = "grey", dash = "dot")
         )
  )

plot_res_time <- 
  plot_ly(KPIs_monthly, 
          x = ~TicketCreatedMonth , 
          y = ~round(avg_resolution_time, 2),
          type = 'scatter',
          mode = 'lines',
          name = 'Avg. resolution time (hours)') 

plot_res_time %>%
  layout(yaxis = list(title = 'Avg. resolution time (hours)'),
         xaxis = list(title = 'Ticket Created Month')) 




plot_CSAT <- 
  plot_ly(KPIs_monthly, 
        x = ~TicketCreatedMonth, 
        y = ~round(CSAT, 2),
        type = 'scatter',
        mode = 'lines',
        name = 'CSAT'
        ) 

plot_CSAT %>% 
  layout(yaxis = list(ticksuffix = "%",
                      title = 'CSAT'),
         xaxis = list(title = 'Ticket Created Month'))



# all 3 plots together
subplot(plot_res_time,
        plot_tickets,
        plot_CSAT,
        nrows = 3,
        shareX = T) %>%
  layout(yaxis3 = list(ticksuffix = "%"),
         xaxis = list(title = 'Ticket Created Month'),
         legend = list(orientation = "h",
                       x = 0.5, y = 1.15,
                       xanchor = "center",
                       yanchor = "top"
                       )
         )




#### correlations ####

cor(KPIs_monthly$avg_new_tickets_daily_per_employee, KPIs_monthly$avg_resolution_time)
cor(KPIs_monthly$avg_resolution_time, KPIs_monthly$CSAT)



#### CSAT decline between Jan and Feb ####

CSAT_drop <- data %>% 
  filter(TicketCreatedMonth %in% c("2021-01", "2021-02")) %>%
  group_by(TicketCreatedMonth) %>%
  summarise(no_tickets = n(),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100,
            CSAT_response_rate = (sum(filled_customer_satisfaction) / sum(is_solved)) * 100
  )


write.xlsx(as.data.frame(CSAT_drop),
           "CSAT_drop_jan_feb.xlsx")



# stats for tickets with filled customer satisfaction survey
CSAT_drop_filled_survey <- 
  data %>% 
  filter(TicketCreatedMonth %in% c("2021-01", "2021-02"),
         filled_customer_satisfaction == TRUE) %>%
  group_by(TicketCreatedMonth) %>%
  summarise(no_tickets = n(),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100
  )

write.xlsx(as.data.frame(CSAT_drop_filled_survey),
           "CSAT_drop_jan_feb.xlsx",
           append = T,
           sheetName = "filled surveys only")




###  by tier

tiers <- data %>% 
  group_by(AssigneeTier) %>%
  summarise(no_tickets = n())

plot_ly(tiers, labels = ~AssigneeTier, values = ~no_tickets, type = 'pie')
# Tier 1 and Tier 2 - most of the tickets


CSAT_tier_1_jan_feb <- data %>% 
  filter(TicketCreatedMonth %in% c("2021-01", "2021-02"),
         AssigneeTier == "Tier 1") %>%
  group_by(TicketCreatedMonth) %>%
  summarise(no_tickets = n(),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100,
            CSAT_response_rate = (sum(filled_customer_satisfaction) / sum(is_solved)) * 100
  )



CSAT_tier_2_jan_feb <- data %>% 
  filter(TicketCreatedMonth %in% c("2021-01", "2021-02"),
         AssigneeTier == "Tier 2") %>%
  group_by(TicketCreatedMonth) %>%
  summarise(no_tickets = n(),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100,
            CSAT_response_rate = (sum(filled_customer_satisfaction) / sum(is_solved)) * 100
  )

CSAT_tier_3_jan_feb <- data %>% 
  filter(TicketCreatedMonth %in% c("2021-01", "2021-02"),
         AssigneeTier == "Tier 3") %>%
  group_by(TicketCreatedMonth) %>%
  summarise(no_tickets = n(),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100,
            CSAT_response_rate = (sum(filled_customer_satisfaction) / sum(is_solved)) * 100
  )

CSAT_triage_jan_feb <- data %>% 
  filter(TicketCreatedMonth %in% c("2021-01", "2021-02"),
         AssigneeTier == "Triage") %>%
  group_by(TicketCreatedMonth) %>%
  summarise(no_tickets = n(),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100,
            CSAT_response_rate = (sum(filled_customer_satisfaction) / sum(is_solved)) * 100
  )


write.xlsx(as.data.frame(CSAT_tier_1_jan_feb),
           "CSAT_tiers_jan_feb.xlsx",
           sheetName = "Tier 1")

write.xlsx(as.data.frame(CSAT_tier_2_jan_feb),
           "CSAT_tiers_jan_feb.xlsx",
           sheetName = "Tier 2",
           append = T)









#### tickets by day of week ####

# weekdays - tickets intensity

daily_stats <- data %>%
  group_by(TicketCreatedDate,
           TicketCreatedMonth,
           TicketCreatedWeekday) %>%
  summarise(no_tickets = n(),
            employees = n_distinct(EmployeeID))

weekday_avg_daily_tickets <- 
  daily_stats %>% 
  group_by(TicketCreatedWeekday) %>%
  summarise(avg_new_tickets = mean(no_tickets),
            avg_employees = mean(employees))

weekday_avg_tickets_per_employee <-
  daily_tickets_per_employee %>%
  group_by(TicketCreatedWeekday) %>%
  summarise(avg_new_tickets_per_employee = mean(no_tickets))


weekday_intensity <- setDT(left_join(weekday_avg_daily_tickets,
                               weekday_avg_tickets_per_employee,
                               by = "TicketCreatedWeekday"))

weekday_intensity[, desired_no_employees := avg_new_tickets / 8]

plot_ly(weekday_intensity, 
        x = ~TicketCreatedWeekday, 
        y = ~avg_new_tickets,
        type = 'bar',
        name = 'avg. no new tickets') %>%
  add_trace(y = ~avg_employees,
            yaxis = "y2",
            type = 'scatter',
            mode = "markers",
            name = "avg no employees",
            marker = list(size = 15)) %>%
  layout(yaxis2 = list(overlaying = "y",
                       range = c(0, 120),
                       side = "right",
                       title = "Avg. no employees"),
         yaxis = list(title = 'Avg. no new tickets'),
         xaxis = list(title = 'Ticket Created Weekday',
                      categoryorder = "array",
                      categoryarray = c("Monday", "Tuesday", 
                                        "Wednesday", "Thursday",
                                        "Friday", "Saturday", "Sunday")),
         legend = list(x = 1.1, y = 1.15)
         ) 


plot_ly(weekday_intensity, 
        x = ~TicketCreatedWeekday , 
        y = ~avg_new_tickets_per_employee,
        type = 'bar') %>%
  layout(yaxis = list(title = 'Avg. no new tickets/day/employee'),
         xaxis = list(title = 'Ticket Created Weekday',
                      categoryorder = "array",
                      categoryarray = c("Monday", "Tuesday", 
                                        "Wednesday", "Thursday",
                                        "Friday", "Saturday", "Sunday"))) 


write.xlsx(weekday_intensity,
           "weekday_tickets_intensity.xlsx")







#### worker_type ####

KPIs_worker_type <- data %>% 
  group_by(Worker_Type) %>%
  summarise(no_tickets = n(), 
            percent_tickets_unsolved = sum(!is_solved)/n(),
            avg_resolution_time = mean(FullResolutionTime, na.rm = T) / 60,
            avg_first_response_time = mean(FirstReplyTime, na.rm = T) / 60,
            escalation_rate = sum(TicketEscalated) / n(),
            CSAT = sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction),
            CSAT_response_rate = sum(filled_customer_satisfaction) / sum(is_solved)
  )



worker_type_avg_daily_tickets <- 
  daily_tickets_per_employee %>%
  group_by(Worker_Type) %>%
  summarise(avg_new_tickets_daily_per_employee = mean(no_tickets))


KPIs_worker_type <- left_join(KPIs_worker_type,
                              worker_type_avg_daily_tickets, 
                          by = "Worker_Type")

write.xlsx(KPIs_worker_type,
           "KPIs_worker_type.xlsx")







####  plot by channel and month

KPIs_by_channel_month <- data %>% 
  group_by(TicketCreatedMonth, TicketChannel) %>%
  summarise(no_tickets = n(), 
            no_tickets_solved = sum(is_solved),
            no_tickets_unsolved = sum(!data$is_solved)
  )

plot_ly(KPIs_by_channel_month,
        x = ~TicketCreatedMonth,
        y = ~no_tickets,
        type = 'bar',
        name = ~TicketChannel,
        color = ~TicketChannel) %>%
  layout(yaxis = list(title = 'No. tickets'),
         xaxis = list(title = 'Month'),
         barmode = 'stack')



#### CSAT response rate ####

plot_CSAT_response_rate <- 
  plot_ly(KPIs_monthly, 
          x = ~TicketCreatedMonth, 
          y = ~round(CSAT_response_rate, 2),
          type = 'scatter',
          mode = 'lines'
  ) 

plot_CSAT_response_rate %>% 
  layout(yaxis = list(ticksuffix = "%",
                      title = 'CSAT response rate'),
         xaxis = list(title = 'Ticket Created Month'))


