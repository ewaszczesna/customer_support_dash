ui <- dashboardPage(
  dashboardHeader(title = "Customer Support"
                  ),
  
  dashboardSidebar(
    collapsed = FALSE,
    
    pickerInput(
      inputId = "employee_id",
      label = "Employee ID:",
      choices = sort(unique(data$EmployeeID)),
      selected = sort(unique(data$EmployeeID)),
      multiple = T,
      options = pickerOptions(size = 10,
                              actionsBox = TRUE,
                              liveSearch = T,
                              liveSearchPlaceholder = "write here to search for ID",
                              selectedTextFormat = "count")
    ),
    
    pickerInput(
      inputId = "ticket_channel",
      label = "Ticket channel:",
      choices = sort(unique(data$TicketChannel)),
      selected = sort(unique(data$TicketChannel)),
      multiple = T,
      options = pickerOptions(size = 10,
                              actionsBox = TRUE,
                              liveSearch = T,
                              liveSearchPlaceholder = "write here to search",
                              selectedTextFormat = "count")
    ),
    
    pickerInput(
      inputId = "ticket_group",
      label = "Ticket group:",
      choices = sort(unique(data$TicketGroup)),
      selected = sort(unique(data$TicketGroup)),
      multiple = T,
      options = pickerOptions(size = 10,
                              actionsBox = TRUE,
                              liveSearch = T,
                              liveSearchPlaceholder = "write here to search",
                              selectedTextFormat = "count")
    ),
    
    pickerInput(
      inputId = "plan",
      label = "Customer plan:",
      choices = sort(unique(data$Plan)),
      selected = sort(unique(data$Plan)),
      multiple = T,
      options = pickerOptions(size = 10,
                              actionsBox = TRUE,
                              liveSearch = T,
                              liveSearchPlaceholder = "write here to search",
                              selectedTextFormat = "count")
    ),
    
    pickerInput(
      inputId = "asaignee_tier",
      label = "Asaignee tier:",
      choices = sort(unique(data$AssigneeTier)),
      selected = sort(unique(data$AssigneeTier)),
      multiple = T,
      options = pickerOptions(size = 10,
                              actionsBox = TRUE,
                              liveSearch = T,
                              liveSearchPlaceholder = "write here to search",
                              selectedTextFormat = "count")
    ),
    
    pickerInput(
      inputId = "worker_location",
      label = "Worker location:",
      choices = sort(unique(data$WorkerLocation)),
      selected = sort(unique(data$WorkerLocation)),
      multiple = T,
      options = pickerOptions(size = 10,
                              actionsBox = TRUE,
                              liveSearch = T,
                              liveSearchPlaceholder = "write here to search",
                              selectedTextFormat = "count")
    ),
    
    pickerInput(
      inputId = "worker_type",
      label = "Worker type:",
      choices = sort(unique(data$Worker_Type)),
      selected = sort(unique(data$Worker_Type)),
      multiple = T,
      options = pickerOptions(size = 10,
                              actionsBox = TRUE,
                              liveSearch = T,
                              liveSearchPlaceholder = "write here to search",
                              selectedTextFormat = "count")
    ),
    
    dateRangeInput('dateRange',
                   label = 'Ticket created dates',
                   start = min(data$TicketCreatedDate), max(data$TicketCreatedDate)
    ),
    
    actionButton("apply_button",
                 label = div("Apply", icon("check")),
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
  ),
  
  
  dashboardBody(
    
    tabsetPanel(type = "tabs",
                tabPanel("Visuals",
                         fluidRow(
                           fluidRow(
                             valueBoxOutput("avg_tickets_per_employee_box") %>% withSpinner(color = "#7e96a2"),
                             valueBoxOutput("avg_res_time_box"),
                             valueBoxOutput("avg_first_response_time_box"),
                             valueBoxOutput("unsolved_tickets_box"),
                             valueBoxOutput("CSAT_box"),
                             valueBoxOutput("CSAT_response_rate_box")
                           ),
                           
                           fluidRow(
                             column(12, 
                                    box(width =12,
                                        title = "Trends",
                                        status = "primary",
                                        collapsible = T,
                                        plotlyOutput("plot_trends",
                                                     height = "600px") %>% withSpinner(color = "#7e96a2")
                                        )
                             )
                           ),
                           
                           fluidRow(h1(" ")),
                           box(title = "Weekday intensity of new tickets",
                               status = "primary", 
                               collapsible = T,
                               plotlyOutput("weekday_intensity") %>% withSpinner(color = "#7e96a2"),
                               plotlyOutput("weekday_capacity_utilization")
                           ),
                           
                           box(title = "No. tickets by channel and Tier",
                               status = "primary", 
                               collapsible = T,
                               plotlyOutput("tickets_by_channel_plot") %>% withSpinner(color = "#7e96a2"),
                               plotlyOutput("tickets_by_tier")
                           )
                           )
                         ),
                
                tabPanel("Table",
                         dataTableOutput("tbl_metrics") %>% withSpinner(color = "#7e96a2"),
                         downloadButton('download',"Download the data")
                         )
    ),
    
    
    shiny.info::display(
      shiny::span(
        "Created by Ewa Szczesna",
        tags$a(href = "https://www.linkedin.com/in/ewaszczesna/", icon("linkedin")),
        tags$a(href = "https://github.com/ewaszczesna", icon("github"))
      ),
      position = "bottom right"
    )
    
    
  )
)