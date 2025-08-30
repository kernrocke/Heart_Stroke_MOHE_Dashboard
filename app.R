
## HEADER -----------------------------------------------------
##  R file METADATA
##  algorithm name          ami_stroke_dashboard / app.R
##  project:                BNR
##  analysts:               Kern Rocke
##  date first created      24-AUG-2025
##  date last modified      30-AUG-2025
##  algorithm task          Create AMI and Stroke Dashboard for Barbados CVD Registry
##  status                  Completed
##  objective               To have a dashboard for monitoring AMI and stroke registry data
##  methods                 See additional information on dashboard. 

#Required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(REDCapR)

# REDCap API configuration with fallback values
redcap_url <- Sys.getenv("REDCAP_URL", unset = "https://caribdata.org/redcap/api/")
api_token <- Sys.getenv("REDCAP_API_TOKEN", unset = "208D71C79CC544CC61E178CE4E4F918C")

# Function to fetch data from REDCap API with error handling
fetch_redcap_data <- function(url, token) {
  tryCatch({
    result <- REDCapR::redcap_read(
      redcap_uri = url,
      token = token,
      verbose = TRUE
    )$data
    return(result)
  }, error = function(e) {
    stop("Error fetching data from REDCap: ", e$message)
  })
}

# Fetch data at startup
cat("Fetching data from REDCap...\n")
data <- fetch_redcap_data(redcap_url, api_token)
cat("Data fetched successfully. Rows:", nrow(data), "\n")

# Compute unique years for AMI eligible cases
cat("Computing unique years for AMI...\n")
unique_years_ami <- sort(unique(data$edateyr[data$redcap_event_name == "heart_arm_2" & data$cstatus == 1 & !is.na(data$edateyr)]))
cat("Unique AMI years:", unique_years_ami, "\n")

# Compute unique years for Stroke eligible cases
cat("Computing unique years for Stroke...\n")
unique_years_stroke <- sort(unique(data$edateyr[data$redcap_event_name == "stroke_arm_1" & data$cstatus == 1 & !is.na(data$edateyr)]))
cat("Unique Stroke years:", unique_years_stroke, "\n")

ui <- dashboardPage(
  skin = "red", 
  dashboardHeader(
    title = "BNR CVD Data Dashboard",
    titleWidth = "100%"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("AMI", tabName = "ami", icon = icon("heart")),
      menuItem("Stroke", tabName = "stroke", icon = icon("brain"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* Change sidebar background to dark red */
    .skin-red .main-sidebar {
      background-color: #8B0000; /* Dark red */
    }
    /* Ensure sidebar menu items have readable text */
    .skin-red .main-sidebar .sidebar-menu a {
      color: #FFFFFF; /* White text for contrast */
    }
    /* Active/hover menu item styling */
    .skin-red .main-sidebar .sidebar-menu .active a,
    .skin-red .main-sidebar .sidebar-menu a:hover {
      background-color: #A52A2A; /* Slightly lighter red for active/hover */
      color: #FFFFFF;
    }
    /* Make header title span full width and center it */
    .main-header .logo {
      width: 100% !important;
      text-align: center;
      padding: 0;
      /* Bold and enlarge the title text */
      font-weight: bold;
      font-size: 48px; /* You can adjust this value */
    }
    .main-header {
      width: 100% !important;
      padding: 0;
    }
    /* Change dashboard body background to light gray */
    .content-wrapper, .right-side {
      background-color: #C9C8C7; /* Light gray background */
    }
    /* Ensure box backgrounds remain white for consistency */
    .box {
      background-color: #FFFFFF;
      color: #000000;
    }
    /* Increase font size of sidebar menu items */
    .sidebar-menu li a {
      font-size: 18px; /* Adjust the size as needed */
    }
  "))
    ),
    tabItems(
      # Home Page
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, align = "center",
                    img(src = "bnr_logo.png", style = "max-width: 40%; height: auto;")
                )
              ),
              fluidRow(
                box(width = 12, 
                    h2(strong("Barbados National Registry: Acute Myocardial Infarction & Stroke Dashboard")),
                    p("Welcome to the CVD dashboard for the Barbados National Registry, dedicated to providing a clear and comprehensive view of cardiovascular health data in Barbados. This platform serves as a vital resource for healthcare professionals, researchers, and policymakers, offering critical insights into the prevalence and outcomes of acute myocardial infarction (AMI) and stroke across the island."),
                    p("Our mission is to translate complex data into actionable information, empowering the public health community to make informed decisions that improve patient care and health outcomes. By tracking key metrics in real-time, we aim to support strategic planning and interventions for these leading causes of mortality and disability."),
                    h3("Key Insights and Data"),
                    p("This dashboard provides a detailed analysis of AMI and stroke events from 2023 to 2025, broken down by year and month. You can explore a variety of data points, including:"),
                    tags$ul(
                      tags$li(strong("Incidence Rates:"), " Monthly and annual estimates for new cases of AMI and stroke, highlighting trends and potential areas of concern."),
                      tags$li(strong("Mortality Data:"), " Comprehensive records on deaths attributable to AMI and stroke, offering a crucial perspective on the severity and impact of these conditions."),
                      tags$li(strong("Proportion Receiving Aspirin Acutely:"), " An essential metric tracking the percentage of patients who received aspirin shortly after an AMI or stroke event. This data is critical for evaluating the timeliness and effectiveness of initial patient management protocols.")
                    ),
                    p("We invite you to navigate through the different sections of the dashboard to explore the data. This information is one of the cornerstone for continuous quality improvement initiatives and the development of targeted public health campaigns in Barbados."),
                    p("The data presented here is an estimated representation of real-world outcomes and is subject to updates as more information is compiled. We are committed to maintaining the highest level of accuracy and transparency in our reporting.")
                )
              )
      ),
      # AMI Page
      tabItem(tabName = "ami",
              h2("Acute Myocardial Infarction (AMI) Data"),
              p("This section provides detailed data and insights related to AMI cases in Barbados."),
              fluidRow(
                box(width = 6,
                    plotOutput("ami_bar_chart")
                ),
                box(width = 6,
                    plotOutput("ami_deaths_bar_chart")
                )
              ),
              fluidRow(
                column(6,
                       selectInput("year_select_ami", "Select Year:",
                                   choices = c("All years" = "All", as.character(unique_years_ami)))
                ),
                column(6,
                       selectInput("sex_select_ami", "Select Sex:",
                                   choices = c("Both" = "Both", "Female" = "Female", "Male" = "Male"))
                )
              ),
              fluidRow(
                valueBoxOutput("total_mi_deaths"),
                valueBoxOutput("total_abstracted_mi_cases"),
                valueBoxOutput("case_fatality_rate_mi"),
                valueBoxOutput("total_receiving_aspirin_mi"),
                valueBoxOutput("proportion_receiving_aspirin_mi")
              ),
              fluidRow(
                box(width = 6,
                    plotOutput("ami_cases_by_month")
                ),
                box(width = 6,
                    plotOutput("ami_deaths_by_month")
                )
              ),
              fluidRow(
                box(width = 6,
                    plotOutput("ami_aspirin_by_month")
                )
              )
      ),
      # Stroke Page
      tabItem(tabName = "stroke",
              h2("Stroke Data"),
              p("This section provides detailed data and insights related to stroke cases in Barbados."),
              fluidRow(
                box(width = 6,
                    plotOutput("stroke_bar_chart")
                ),
                box(width = 6,
                    plotOutput("stroke_deaths_bar_chart")
                )
              ),
              fluidRow(
                column(6,
                       selectInput("year_select_stroke", "Select Year:",
                                   choices = c("All years" = "All", as.character(unique_years_stroke)))
                ),
                column(6,
                       selectInput("sex_select_stroke", "Select Sex:",
                                   choices = c("Both" = "Both", "Female" = "Female", "Male" = "Male"))
                )
              ),
              fluidRow(
                valueBoxOutput("total_stroke_deaths"),
                valueBoxOutput("total_abstracted_stroke_cases"),
                valueBoxOutput("case_fatality_rate_stroke"),
                valueBoxOutput("total_receiving_aspirin_stroke"),
                valueBoxOutput("proportion_receiving_aspirin_stroke")
              ),
              fluidRow(
                box(width = 6,
                    plotOutput("stroke_cases_by_month")
                ),
                box(width = 6,
                    plotOutput("stroke_deaths_by_month")
                )
              ),
              fluidRow(
                box(width = 6,
                    plotOutput("stroke_aspirin_by_month")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  # Reactive filtered data for AMI
  filtered_data_ami <- reactive({
    ami_filtered <- data[data$redcap_event_name == "heart_arm_2" & data$cstatus == 1, ]
    
    if (input$year_select_ami != "All") {
      ami_filtered <- ami_filtered[ami_filtered$edateyr == as.numeric(input$year_select_ami), ]
    }
    
    if (input$sex_select_ami != "Both") {
      sex_val <- ifelse(input$sex_select_ami == "Female", 1, 2)
      ami_filtered <- ami_filtered[ami_filtered$sex == sex_val, ]
    }
    
    ami_filtered
  })
  
  # Reactive filtered data for Stroke
  filtered_data_stroke <- reactive({
    stroke_filtered <- data[data$redcap_event_name == "stroke_arm_1" & data$cstatus == 1, ]
    
    if (input$year_select_stroke != "All") {
      stroke_filtered <- stroke_filtered[stroke_filtered$edateyr == as.numeric(input$year_select_stroke), ]
    }
    
    if (input$sex_select_stroke != "Both") {
      sex_val <- ifelse(input$sex_select_stroke == "Female", 1, 2)
      stroke_filtered <- stroke_filtered[stroke_filtered$sex == sex_val, ]
    }
    
    stroke_filtered
  })
  
  # Calculate totals for AMI value boxes
  total_mi_deaths <- reactive({
    sum(filtered_data_ami()$vstatus == 2, na.rm = TRUE)
  })
  
  total_abstracted_mi_cases <- reactive({
    nrow(filtered_data_ami())
  })
  
  case_fatality_rate_mi <- reactive({
    total_deaths <- total_mi_deaths()
    total_cases <- total_abstracted_mi_cases()
    if (total_cases > 0) {
      (total_deaths / total_cases) * 100
    } else {
      0
    }
  })
  
  total_receiving_aspirin_mi <- reactive({
    sum(filtered_data_ami()$asp___1 == 1, na.rm = TRUE)
  })
  
  proportion_receiving_aspirin_mi <- reactive({
    total_aspirin <- total_receiving_aspirin_mi()
    total_cases <- total_abstracted_mi_cases()
    if (total_cases > 0) {
      (total_aspirin / total_cases) * 100
    } else {
      0
    }
  })
  
  # Calculate totals for Stroke value boxes
  total_stroke_deaths <- reactive({
    sum(filtered_data_stroke()$vstatus == 2, na.rm = TRUE)
  })
  
  total_abstracted_stroke_cases <- reactive({
    nrow(filtered_data_stroke())
  })
  
  case_fatality_rate_stroke <- reactive({
    total_deaths <- total_stroke_deaths()
    total_cases <- total_abstracted_stroke_cases()
    if (total_cases > 0) {
      (total_deaths / total_cases) * 100
    } else {
      0
    }
  })
  
  total_receiving_aspirin_stroke <- reactive({
    sum(filtered_data_stroke()$asp___1 == 1, na.rm = TRUE)
  })
  
  proportion_receiving_aspirin_stroke <- reactive({
    total_aspirin <- total_receiving_aspirin_stroke()
    total_cases <- total_abstracted_stroke_cases()
    if (total_cases > 0) {
      (total_aspirin / total_cases) * 100
    } else {
      0
    }
  })
  
  # Server logic for AMI cases bar chart
  output$ami_bar_chart <- renderPlot({
    ami_data <- data[data$redcap_event_name == "heart_arm_2" & data$cstatus == 1, ]
    
    ami_data$year <- ami_data$edateyr
    
    ami_data <- ami_data[!is.na(ami_data$year), ]
    
    year_counts <- aggregate(recid ~ year, data = ami_data, FUN = length)
    colnames(year_counts)[2] <- "count"
    
    ggplot(year_counts, aes(x = as.factor(year), y = count)) +
      geom_bar(stat = "identity", fill = "#08589e") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Number of Eligible AMI Cases by Year",
           x = "Year",
           y = "Number of Cases") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for AMI deaths bar chart
  output$ami_deaths_bar_chart <- renderPlot({
    ami_deaths_data <- data[data$redcap_event_name == "heart_arm_2" & data$cstatus == 1 & data$vstatus == 2, ]
    
    ami_deaths_data$year <- ami_deaths_data$edateyr
    
    ami_deaths_data <- ami_deaths_data[!is.na(ami_deaths_data$year), ]
    
    deaths_counts <- aggregate(recid ~ year, data = ami_deaths_data, FUN = length)
    colnames(deaths_counts)[2] <- "count"
    
    ggplot(deaths_counts, aes(x = as.factor(year), y = count)) +
      geom_bar(stat = "identity", fill = "darkred") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Number of AMI Deaths by Year",
           x = "Year",
           y = "Number of Deaths") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for AMI cases by month bar chart
  output$ami_cases_by_month <- renderPlot({
    df <- filtered_data_ami() %>%
      group_by(edatemon) %>%
      summarise(count = n()) %>%
      filter(!is.na(edatemon))
    
    if (nrow(df) == 0) {
      return(NULL)  # No data to plot
    }
    
    df$month <- factor(df$edatemon, levels = 1:12, labels = month.name)
    
    ggplot(df, aes(x = month, y = count)) +
      geom_bar(stat = "identity", fill = "#08589e") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "AMI Cases by Month",
           x = "Month",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for AMI deaths by month bar chart
  output$ami_deaths_by_month <- renderPlot({
    df <- filtered_data_ami() %>%
      group_by(edatemon) %>%
      summarise(count = sum(vstatus == 2, na.rm = TRUE)) %>%
      filter(!is.na(edatemon))
    
    if (nrow(df) == 0) {
      return(NULL)  # No data to plot
    }
    
    df$month <- factor(df$edatemon, levels = 1:12, labels = month.name)
    
    ggplot(df, aes(x = month, y = count)) +
      geom_bar(stat = "identity", fill = "darkred") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "AMI Deaths by Month",
           x = "Month",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for AMI aspirin by month bar chart
  output$ami_aspirin_by_month <- renderPlot({
    df <- filtered_data_ami() %>%
      group_by(edatemon) %>%
      summarise(count = sum(asp___1 == 1, na.rm = TRUE)) %>%
      filter(!is.na(edatemon))
    
    if (nrow(df) == 0) {
      return(NULL)  # No data to plot
    }
    
    df$month <- factor(df$edatemon, levels = 1:12, labels = month.name)
    
    ggplot(df, aes(x = month, y = count)) +
      geom_bar(stat = "identity", fill = "#238b45") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Persons on Aspirin by Month",
           x = "Month",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for Stroke cases bar chart
  output$stroke_bar_chart <- renderPlot({
    stroke_data <- data[data$redcap_event_name == "stroke_arm_1" & data$cstatus == 1, ]
    
    stroke_data$year <- stroke_data$edateyr
    
    stroke_data <- stroke_data[!is.na(stroke_data$year), ]
    
    year_counts <- aggregate(recid ~ year, data = stroke_data, FUN = length)
    colnames(year_counts)[2] <- "count"
    
    ggplot(year_counts, aes(x = as.factor(year), y = count)) +
      geom_bar(stat = "identity", fill = "#08589e") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Number of Eligible Stroke Cases by Year",
           x = "Year",
           y = "Number of Cases") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for Stroke deaths bar chart
  output$stroke_deaths_bar_chart <- renderPlot({
    stroke_deaths_data <- data[data$redcap_event_name == "stroke_arm_1" & data$cstatus == 1 & data$vstatus == 2, ]
    
    stroke_deaths_data$year <- stroke_deaths_data$edateyr
    
    stroke_deaths_data <- stroke_deaths_data[!is.na(stroke_deaths_data$year), ]
    
    deaths_counts <- aggregate(recid ~ year, data = stroke_deaths_data, FUN = length)
    colnames(deaths_counts)[2] <- "count"
    
    ggplot(deaths_counts, aes(x = as.factor(year), y = count)) +
      geom_bar(stat = "identity", fill = "darkred") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Number of Stroke Deaths by Year",
           x = "Year",
           y = "Number of Deaths") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for Stroke cases by month bar chart
  output$stroke_cases_by_month <- renderPlot({
    df <- filtered_data_stroke() %>%
      group_by(edatemon) %>%
      summarise(count = n()) %>%
      filter(!is.na(edatemon))
    
    if (nrow(df) == 0) {
      return(NULL)  # No data to plot
    }
    
    df$month <- factor(df$edatemon, levels = 1:12, labels = month.name)
    
    ggplot(df, aes(x = month, y = count)) +
      geom_bar(stat = "identity", fill = "#08589e") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Stroke Cases by Month",
           x = "Month",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for Stroke deaths by month bar chart
  output$stroke_deaths_by_month <- renderPlot({
    df <- filtered_data_stroke() %>%
      group_by(edatemon) %>%
      summarise(count = sum(vstatus == 2, na.rm = TRUE)) %>%
      filter(!is.na(edatemon))
    
    if (nrow(df) == 0) {
      return(NULL)  # No data to plot
    }
    
    df$month <- factor(df$edatemon, levels = 1:12, labels = month.name)
    
    ggplot(df, aes(x = month, y = count)) +
      geom_bar(stat = "identity", fill = "darkred") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Stroke Deaths by Month",
           x = "Month",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Server logic for Stroke aspirin by month bar chart
  output$stroke_aspirin_by_month <- renderPlot({
    df <- filtered_data_stroke() %>%
      group_by(edatemon) %>%
      summarise(count = sum(asp___1 == 1, na.rm = TRUE)) %>%
      filter(!is.na(edatemon))
    
    if (nrow(df) == 0) {
      return(NULL)  # No data to plot
    }
    
    df$month <- factor(df$edatemon, levels = 1:12, labels = month.name)
    
    ggplot(df, aes(x = month, y = count)) +
      geom_bar(stat = "identity", fill = "#238b45") +
      geom_text(aes(label = count, y = count * 1.01), vjust = -0.5, size = 5) +
      labs(title = "Persons on Aspirin by Month",
           x = "Month",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  # Value box for total MI deaths
  output$total_mi_deaths <- renderValueBox({
    valueBox(
      value = total_mi_deaths(),
      subtitle = "Total Number of MI Deaths",
      color = "red",
      icon = icon("heart-broken")
    )
  })
  
  # Value box for total abstracted MI cases
  output$total_abstracted_mi_cases <- renderValueBox({
    valueBox(
      value = total_abstracted_mi_cases(),
      subtitle = "Total Number of Abstracted MI Cases",
      color = "blue",
      icon = icon("notes-medical")
    )
  })
  
  # Value box for case fatality rate MI
  output$case_fatality_rate_mi <- renderValueBox({
    valueBox(
      value = sprintf("%.2f%%", case_fatality_rate_mi()),
      subtitle = "Case Fatality Rate",
      color = "yellow",
      icon = icon("percentage")
    )
  })
  
  # Value box for total receiving aspirin MI
  output$total_receiving_aspirin_mi <- renderValueBox({
    valueBox(
      value = total_receiving_aspirin_mi(),
      subtitle = "Total Receiving Aspirin Acutely",
      color = "green",
      icon = icon("pills")
    )
  })
  
  # Value box for proportion receiving aspirin MI
  output$proportion_receiving_aspirin_mi <- renderValueBox({
    valueBox(
      value = sprintf("%.2f%%", proportion_receiving_aspirin_mi()),
      subtitle = "Proportion Receiving Aspirin",
      color = "purple",
      icon = icon("percentage")
    )
  })
  
  # Value box for total stroke deaths
  output$total_stroke_deaths <- renderValueBox({
    valueBox(
      value = total_stroke_deaths(),
      subtitle = "Total Number of Stroke Deaths",
      color = "red",
      icon = icon("heart-broken")
    )
  })
  
  # Value box for total abstracted stroke cases
  output$total_abstracted_stroke_cases <- renderValueBox({
    valueBox(
      value = total_abstracted_stroke_cases(),
      subtitle = "Total Number of Abstracted Stroke Cases",
      color = "blue",
      icon = icon("notes-medical")
    )
  })
  
  # Value box for case fatality rate stroke
  output$case_fatality_rate_stroke <- renderValueBox({
    valueBox(
      value = sprintf("%.2f%%", case_fatality_rate_stroke()),
      subtitle = "Case Fatality Rate",
      color = "yellow",
      icon = icon("percentage")
    )
  })
  
  # Value box for total receiving aspirin stroke
  output$total_receiving_aspirin_stroke <- renderValueBox({
    valueBox(
      value = total_receiving_aspirin_stroke(),
      subtitle = "Total Receiving Aspirin Acutely",
      color = "green",
      icon = icon("pills")
    )
  })
  
  # Value box for proportion receiving aspirin stroke
  output$proportion_receiving_aspirin_stroke <- renderValueBox({
    valueBox(
      value = sprintf("%.2f%%", proportion_receiving_aspirin_stroke()),
      subtitle = "Proportion Receiving Aspirin",
      color = "purple",
      icon = icon("percentage")
    )
  })
}

shinyApp(ui, server)