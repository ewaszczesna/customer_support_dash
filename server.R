server <- function(input, output, session) {
  
  observeEvent(input$apply_button, {

    # filter data
    data_filtered <- data[data$EmployeeID %in% input$employee_id &
                          data$TicketChannel %in% input$ticket_channel &
                          data$TicketGroup %in% input$ticket_group &
                          data$Plan %in% input$plan &
                          data$AssigneeTier %in% input$asaignee_tier&
                          data$WorkerLocation %in% input$worker_location &
                          data$Worker_Type %in% input$worker_type &
                          data$TicketCreatedDate >= input$dateRange[1] &
                          data$TicketCreatedDate <= input$dateRange[2]
                          , ]
    
    # calculate daily tickets per employee
    daily_tickets_per_employee <- 
      data_filtered %>%
      group_by(TicketCreatedDate,
               TicketCreatedMonth,
               TicketCreatedWeekday,
               EmployeeID,
               Worker_Type) %>%
      summarise(no_tickets = n())
    
    
    
    # KPIs monthly
    
    KPIs_monthly <- data_filtered %>% 
      group_by(TicketCreatedMonth) %>%
      summarise(`Total tickets` = n(), 
                `Solved tickets` = sum(is_solved),
                `Unsolved tickets` = sum(!data_filtered$is_solved),
                `Avg. resolution time (hours)` = mean(FullResolutionTime, na.rm = T) / 60,
                `Avg. first response time (hours)` = mean(FirstReplyTime, na.rm = T) / 60,
                `Escalation rate` = sum(TicketEscalated) / n(),
                CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)) * 100,
                `CSAT response rate` = (sum(filled_customer_satisfaction) / sum(is_solved)) * 100
      )
    
    
    monthly_avg_daily_tickets <- 
      daily_tickets_per_employee %>%
      group_by(TicketCreatedMonth) %>%
      summarise(`Avg. tickets daily per employee` = mean(no_tickets))
    
    KPIs_monthly <- left_join(KPIs_monthly,
                              monthly_avg_daily_tickets, 
                              by = "TicketCreatedMonth")
    
    
    # KPIs total
    
    KPIs_total <- 
      data_filtered %>%
      summarise(`Unsolved tickets` = sum(!data_filtered$is_solved),
                `Avg. resolution time (hours)` = (sum(FullResolutionTime, na.rm = T) / sum(is_solved)) / 60,
                `Avg. first response time (hours)` = mean(FirstReplyTime, na.rm = T) / 60,
                `Escalation rate` = sum(TicketEscalated) / n(),
                CSAT = (sum(CustomerSatisfaction, na.rm = T) / sum(filled_customer_satisfaction)),
                `CSAT response rate` = (sum(filled_customer_satisfaction) / sum(is_solved))
      )
    
    total_avg_daily_tickets <- 
      mean(daily_tickets_per_employee$no_tickets)
    
    KPIs_total$`Avg. tickets daily per employee` <- total_avg_daily_tickets
    
    
    
    #### VALUE BOXES ####
    
    output$avg_tickets_per_employee_box <- renderValueBox({
      valueBox(
        value = formatC(KPIs_total$`Avg. tickets daily per employee`, digits = 2, format = "f"),
        subtitle = "Avg. tickets daily per employee",
        icon = icon("user", lib = "glyphicon"),
        color = "purple"
      )
    })
    
    output$avg_res_time_box <- renderValueBox({
      valueBox(
        value = formatC(KPIs_total$`Avg. resolution time (hours)`, digits = 2, format = "f"),
        subtitle = "Avg. resolution time (hours)",
        icon = icon("hourglass-half"),
        color = "aqua"
      )
    })
    
    output$avg_first_response_time_box <- renderValueBox({
      valueBox(
        value = formatC(KPIs_total$`Avg. first response time (hours)`, digits = 2, format = "f"),
        subtitle = "Avg. first response time (hours)",
        icon = icon("envelope", lib = "glyphicon"),
        color = "yellow"
      )
    })
    
    output$CSAT_box <- renderValueBox({
      valueBox(
        value = scales::percent(KPIs_total$CSAT,
                                accuracy = 0.1),
        subtitle = "CSAT",
        icon = icon("heart", lib = "glyphicon"),
        color = "light-blue"
      )
    })
    
    output$CSAT_response_rate_box <- renderValueBox({
      valueBox(
        value = scales::percent(KPIs_total$`CSAT response rate`, 
                                accuracy = 0.1),
        subtitle = "CSAT response rate",
        icon = icon("pencil", lib = "glyphicon"),
        color = "green"
      )
    })
    
    output$unsolved_tickets_box <- renderValueBox({
      valueBox(
        value = formatC(KPIs_total$`Unsolved tickets`, digits = 0, format = "f"),
        subtitle = "Unsolved tickets",
        icon = icon("pencil", lib = "glyphicon"),
        color = "red"
      )
    })
    
    
    #### TABLE ####

    output$tbl_metrics <- renderDataTable({
      
      datatable(KPIs_monthly) %>% 
        formatCurrency(columns = c("CSAT",
                                   "CSAT response rate",
                                   "Escalation rate"), 
                       currency = "%",
                       digits = 2,
                       before = F) %>%
        formatRound(columns = c("Avg. tickets daily per employee",
                                "Avg. resolution time (hours)",
                                "Avg. first response time (hours)"), digits = 2) %>%
        formatRound(columns = c("Total tickets",
                                "Solved tickets",
                                "Unsolved tickets"), digits = 0, mark = " ") 
    })
    
    output$download <- downloadHandler(
      filename = function(){"customer_support_KPIs.csv"}, 
      content = function(fname){
        write.csv(KPIs_monthly, fname)
      }
    )


    #### PLOTS ####
    

    # PLOT KPIs TRENDS
    output$plot_trends <- renderPlotly({
      
      plot_tickets <- 
        plot_ly(KPIs_monthly, 
                x = ~TicketCreatedMonth, 
                y = ~round(`Avg. tickets daily per employee`, 2),
                type = 'scatter',
                mode = 'lines',
                name = 'avg. new tickets / day / employee'
        ) 
      
      
      plot_res_time <- 
        plot_ly(KPIs_monthly, 
                x = ~TicketCreatedMonth , 
                y = ~round(`Avg. resolution time (hours)`, 2),
                type = 'scatter',
                mode = 'lines',
                name = 'Avg. resolution time (hours)') 
      
      
      plot_CSAT <- 
        plot_ly(KPIs_monthly, 
                x = ~TicketCreatedMonth, 
                y = ~round(CSAT, 2),
                type = 'scatter',
                mode = 'lines',
                name = 'CSAT'
        ) 
      
      plot_CSAT_response_rate <-
        plot_ly(KPIs_monthly, 
              x = ~TicketCreatedMonth, 
              y = ~round(`CSAT response rate`, 2),
              type = 'scatter',
              mode = 'lines',
              name = 'CSAT response_rate'
      )
      
      # all 4 plots together
      subplot(plot_res_time,
              plot_tickets,
              plot_CSAT,
              plot_CSAT_response_rate,
              nrows = 4,
              shareX = T) %>%
        layout(yaxis3 = list(ticksuffix = "%"),
               yaxis4 = list(ticksuffix = "%"),
               xaxis = list(title = 'Ticket Created Month'),
               legend = list(orientation = "h",
                             x = 0.5, y = 1.15,
                             xanchor = "center",
                             yanchor = "top"
               )
        )
    })
    


    # PLOT CHANNELS
    output$tickets_by_channel_plot <- renderPlotly({
      
      KPIs_by_channel <- data_filtered %>% 
        group_by(TicketCreatedMonth, TicketChannel) %>%
        summarise(no_tickets = n(), 
                  no_tickets_solved = sum(is_solved),
                  no_tickets_unsolved = sum(!data_filtered$is_solved)
        )
      
      plot_ly(KPIs_by_channel,
              x = ~TicketCreatedMonth,
              y = ~no_tickets,
              type = 'bar',
              name = ~TicketChannel,
              color = ~TicketChannel) %>%
        layout(yaxis = list(title = 'No. tickets'),
               xaxis = list(title = 'Month'),
               barmode = 'stack')
      
    })
    
    # PLOT TIERS
    output$tickets_by_tier <- renderPlotly({
      tiers <- data_filtered %>% 
        group_by(AssigneeTier) %>%
        summarise(no_tickets = n())
      
      plot_ly(tiers, labels = ~AssigneeTier, values = ~no_tickets, type = 'pie')
    })
    
    
    # PLOT WEEKDAY INTENSITY
    
    # calculate data
    daily_stats <- data_filtered %>%
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
    
    
    
    output$weekday_intensity <- renderPlotly({
      
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
    })
    
    
    output$weekday_capacity_utilization <- renderPlotly({
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
    })
    
    
    
    
    
    
  }, ignoreNULL = FALSE)
  

    
}
