#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## Load necessary libraries
lapply(c("shiny", "shinyjs", "ggplot2", "dplyr", "bslib", "bsicons", "shinydashboard", "DT"), library, character.only = TRUE) 


## Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("5-Minute Heart Rate Recovery (HRR) Test"),
  
  # Instruction block
  fluidRow(
    column(12, 
           div(
             style = "background-color: #f9f9f9; padding: 15px; margin-bottom: 15px; border: 1px solid #ccc; border-radius: 5px;",
             h4("Instructions"),
             p("Welcome to the 5' Minute Heart Rate Recovery (HRR) Test app."),
             p("Follow the steps below to complete your test:"),
             tags$ol(
               tags$li("Go to the 'Athlete Profile' tab to enter your profile details."),
               tags$li("Navigate to the '5' HRR Test' tab to input your heart rate readings."),
               tags$li("View your results under the 'Individual Results' tab."),
               tags$li("For team or crew data, go to the 'Team/Crew Results' tab.")
             ),
             p("Ensure that your maximum heart rate is accurate and within a valid range (0–250 bpm).")
           )
    )
  ),
  
  # User Profile tab panel
  tabsetPanel(
    tabPanel("Athlete Profile",
             textInput("name", "Name", placeholder = "First Name Last Name"),
             selectInput("sex", "Sex", choices = c("Male", "Female")),
             dateInput("dob", "Date of Birth (Year-Month-Day)", value = Sys.Date()),
             selectInput("row_class", "Rowing Discipline", choices = c("Classic (Openweight/Lightweight)", "Classic (Para)", "Coastal/Beach Sprints", "Indoor")),
             numericInput("HRmax", "Maximum Heart Rate (bpm)", value = 150, min = 0, max = 250),
             actionButton("submit_profile", "Submit", 
                          class = "btn-lng btn-success", 
                          style = "color: white; background-color: #00234f; border-color: black;", 
                          icon = icon("heart-pulse")),
             verbatimTextOutput("validation_message"),
             tags$br(), tags$br()
    ),
    tabPanel("5' HRR Test",
             sidebarLayout(
               sidebarPanel(
                 selectInput("athlete_name", "Select Your Name", choices = NULL),
                 dateInput("testdate", "Today's Date (Year-Month-Day)", value = Sys.Date()),
                 numericInput("five_min_watts", label = "Average Watts for 5'", value = 0, min = 0, max = 1500),
                 numericInput("five_min_hr", label = "HR at 5-minutes", value = 0, min = 0, max = 250),
                 numericInput("one_min_hrr", label = "HRR at 1-minute", value = 0, min = 0, max = 250), 
                 numericInput("two_min_hrr", label = "HRR at 2-minutes", value = 0, min = 0, max = 250),
                 fluidRow(
                   column(12, 
                          tags$div(
                            sliderInput("RPE", label = "Rate of Perceived Exertion", value = 0, min = 0, max = 10),
                            uiOutput("rpe_description"),  # Dynamic description
                            style = "text-align: center; margin-bottom: 20px; font-weight: bold;"  # Center-align the text and add spacing
                          )
                   )
                 ),
                 actionButton("submit_hrr", "Submit HRR Data", 
                              class = "btn-lng btn-success", 
                              style = "color: white; background-color: #00234f; border-color: black;", 
                              icon = icon("heart-pulse")),
                 tags$br(), tags$br()
               ),
               mainPanel(
                 h4("Trend Analysis Placeholder"),
                 plotOutput("trend_plot")  # Add a plot output or another UI element
               )
             )
    ),
    tabPanel("Individual Results",
             sidebarLayout(
               sidebarPanel(
                 selectInput("athlete_name_ind", "Filter By Athlete Name", choices = NULL),
                 selectInput("analysis_ind_sex", "Filter By Sex", choices = c("Male", "Female", "Both")),
                 selectInput("analysis_ind_range", "Filter By Date Range", choices = c("Today", "7-Day Rolling Average", "30-Day Rolling Average", "Custom")),
                 selectInput("analysis_row_discipline", "Filter By Rowing Discipline (Select All That Apply)", choices = c("Classic (Openweight/Lightweight)", "Classic (Para)", "Coastal/Beach Sprints", "Indoor"),
                             multiple = TRUE),
               ),
               mainPanel(
                 h4(""),  # Placeholder text
                 plotOutput("individual_trend_plot"),  # Add a plot output or other UI element
                 plotOutput("efficiency_trend_plot")
               )
             )
    ),
    tabPanel("Team/Crew Results",
             p("Team/Crew results will be displayed here.")
    )
  )
)

## Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store validation messages
  validation_message <- reactiveVal("")
  
  # Reactive data frame to store athlete data
  athlete_data <- reactiveVal(data.frame(
    Name = character(),
    Sex = character(),
    Date = as.Date(character()),
    Rowing_Discipline = character(),
    HRmax = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Reactive data frame to store HRR test data
  HRR_data <- reactiveVal(data.frame(
    Athlete_Name = character(),
    Today_Date = as.Date(character()),
    Watts = numeric(),
    HR_5min = numeric(),
    HRR_1min = numeric(),
    HRR_2min = numeric(),
    RPE = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Observe the Submit button for Athlete Profile
  observeEvent(input$submit_profile, {
    if (input$name == "") {
      validation_message("Error: Name cannot be empty.")
    } else {
      validation_message("")
      new_data <- data.frame(
        Name = input$name,
        Sex = input$sex,
        Date = input$dob,
        Rowing_Discipline = input$row_class,
        HRmax = input$HRmax,
        stringsAsFactors = FALSE
      )
      athlete_data(rbind(athlete_data(), new_data))
      showNotification("Profile submitted successfully!", type = "message")
    }
  })
  
  # Update dropdown choices in "5' HRR Test" tab
  observe({
    updateSelectInput(
      session,
      "athlete_name",
      choices = athlete_data()$Name  # Populate dropdown with names
    )
  })
  
  # Observe the Submit button for HRR Test
  observeEvent(input$submit_hrr, {
    new_data <- data.frame(
      Athlete_Name = input$athlete_name,  # Store selected athlete name
      Today_Date = input$testdate,
      Watts = input$five_min_watts,
      HR_5min = input$five_min_hr,
      HRR_1min = input$one_min_hrr,
      HRR_2min = input$two_min_hrr,
      RPE = input$RPE,
      stringsAsFactors = FALSE
    )
    HRR_data(rbind(HRR_data(), new_data))
    showNotification("HRR data submitted successfully!", type = "message")
  })

  # Output validation messages
  output$validation_message <- renderText({ validation_message() })
  
  
    # Reactive text based on slider value
    output$rpe_description <- renderUI({
      tags$strong(
      switch(as.character(input$RPE),
             "0" = "Rest",
             "1" = "Very, Very Easy",
             "2" = "Easy",
             "3" = "Moderate Effort",
             "4" = "Somewhat Hard",
             "5" = "Hard",
             "6" = "Hard",
             "7" = "Very Hard",
             "8" = "Very Hard",
             "9" = "Very Hard",
             "10" = "Maximal Effort")
    )
    })
    
    # Update drop down choices in Individual Results tab
    observe({
      updateSelectInput(
        session,
        "athlete_name_ind",
        choices = athlete_data()$Name  # Populate dropdown with names
      )
    })
    
    # Dynamically render date range input for "Custom" selection
    observe({
      if (input$analysis_ind_range == "Custom") {
        insertUI(
          selector = "#analysis_ind_range",  # Target the dropdown
          where = "afterEnd",  # Add the date range input after the dropdown
          ui = dateRangeInput(
            "custom_date_range",
            label = "",
            start = Sys.Date() - 7,  # Default: 7 days ago
            end = Sys.Date()        # Default: Today
          )
        )
      } else {
        removeUI(selector = "div:has(> #custom_date_range)")  # Remove the date range input
      }
    })
    
    # Define processed data
    processed_data <- reactive({
      inddata <- HRR_data()
      if(nrow(inddata) == 0) {
        # If no data available, return empty data frame immediately
        return(inddata)
      }
      
    # Calculate additional variables
    inddata$HRR_delta_1min <- inddata$HR_5min - inddata$HRR_1min                              # HRR Delta for 1-min
    inddata$HRR_delta_2min <- inddata$HR_5min - inddata$HRR_2min                              # HRR Delta for 2-min
    inddata$HRR_pct_1min <- inddata$HRR_delta_1min / inddata$HR_5min * 100                    # HRR for 1-min expressed as a percentage
    inddata$HRR_pct_2min <- inddata$HRR_delta_2min / inddata$HR_5min * 100                    # HRR for 2-min expressed as a percentage
    inddata$minHRR_pct_1min <- min(inddata$HRR_pct_1min)                                      # All-time minimum value of HRR% for 1-min
    inddata$bot_ten_HRR_pct_1min <- inddata$HRR_pct_1min * .1                                 # Bottom 10% of HRR% for 1-min
    inddata$mean_HRR_pct_1min <- mean(inddata$HRR_pct_1min)                                   # All-time mean value of HRR% for 1-min
    inddata$top_ten_HRR_pct_1min <- inddata$HRR_pct_1min * .9                                 # Top 10% of HRR% for 1-min
    inddata$mid_80_pct_1min <- inddata$top_ten_HRR_pct_1min - inddata$bot_ten_HRR_pct_1min   # Middle 80% of HRR%
    inddata$max_HRR_pct_1min <- max(inddata$HRR_pct_1min)                                     # All-time maximum value of HRR% for 1-min
    inddata$HRR_Efficiency <- inddata$Watts / inddata$HR_5min                                 # Watts per BPM 
    inddata$HRR_Efficiency_RPE <- inddata$HRR_Efficiency * inddata$RPE                        # RPE for W/bpm
    inddata$HRRTL <- 5 * inddata$RPE                                                          # Training Load for HRR Test
    
    return(inddata)
    })
    
    # Filter data based on user selection
    filtered_data <- reactive({
      req(input$athlete_name_ind) # Ensures an athlete is selected
      inddata <- processed_data()
      if(nrow(inddata) == 0) {
        # No processed data, return empty
        return(inddata)
      }
  
    # Filter for the selected athlete
    inddata <- inddata[inddata$Athlete_Name == input$athlete_name_ind, ]
    
    # If filtering by athlete name results in zero rows, return immediately
    if (nrow(inddata) == 0) {
      return(inddata)
    }
    
    # Filter based on date range selection
    if (input$analysis_ind_range == "Today") {
      inddata <- inddata[inddata$Today_Date == Sys.Date(), ]
    } else if (input$analysis_ind_range == "7-Day Rolling Average") {
      inddata <- inddata[inddata$Today_Date >= Sys.Date() - 6, ]  # Last 7 days
    } else if (input$analysis_ind_range == "30-Day Rolling Average") {
      inddata <- inddata[inddata$Today_Date >= Sys.Date() - 29, ]  # Last 30 days
    } else if (input$analysis_ind_range == "Custom" && !is.null(input$custom_date_range)) {
      inddata <- inddata[inddata$Today_Date >= input$custom_date_range[1] &
                     inddata$Today_Date <= input$custom_date_range[2], ]
    }
    
    return(inddata)
})
    
    # Render the first trend plot
    output$individual_trend_plot <- renderPlot({
      data_to_plot <- filtered_data()
      if (nrow(data_to_plot) == 0) {
        # No data to plot, so return without plotting
        return(NULL)
      }
        
    # Plot HRR data
    ggplot(data_to_plot, aes(x = Today_Date, y = HRR_pct_1min)) +
      geom_line(color = "black", size = 1) +
      geom_point(color = "#00234f", size = 3) +
        
    # Add a geom_text layer to label each point with its percentage
      geom_text(aes(label = paste0(round(HRR_pct_1min, 1), "%")), 
                vjust = -0.5,  # Adjust vertical position
                fontface = "bold",
                size = 5) +    # Increase text size as needed
        labs(
          title = "Heart Rate Recovery % (1-Minute)",
          x = "Date",
          y = "HRR % (1-Minute)"
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14, face = "bold")
        ) 
      
    })
    
    # Render the second trend plot (HRR Efficiency)
    output$efficiency_trend_plot <- renderPlot({
      req(filtered_data())  # Ensure filtered data is available
      data_to_plot <- filtered_data()  # Define data_to_plot in this render function too
      
      ggplot(data_to_plot, aes(x = Today_Date, y = HRR_Efficiency)) +
        geom_line(color = "black", size = 1) +
        geom_point(color = "#e8000d", size = 2) +
        # Add a geom_text layer to label each point with its percentage
        geom_text(aes(label = paste0(round(HRR_Efficiency, 1), "%")), 
                  vjust = -0.5,  # Adjust vertical position
                  fontface = "bold",
                  size = 5) +    # Increase text size as needed
        labs(
          title = "Watts Over 5-minute HR",
          x = "Date",
          y = "W/HR"
        ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14, face = "bold")
        )
    })
}


## Run the application
shinyApp(ui = ui, server = server)
