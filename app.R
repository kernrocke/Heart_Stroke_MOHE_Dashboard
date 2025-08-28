library(shiny)
library(shinydashboard)

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
                    p("Welcome to the official dashboard for the Barbados National Registry, dedicated to providing a clear and comprehensive view of cardiovascular health data. This platform serves as a vital resource for healthcare professionals, researchers, and policymakers, offering critical insights into the prevalence and outcomes of acute myocardial infarction (AMI) and stroke across the island."),
                    p("Our mission is to translate complex data into actionable information, empowering the public health community to make informed decisions that improve patient care and health outcomes. By tracking key metrics in real-time, we aim to support strategic planning and interventions for these leading causes of mortality and disability."),
                    h3("Key Insights and Data"),
                    p("This dashboard provides a detailed analysis of AMI and stroke events from 2023 to 2025, broken down by year and month. You can explore a variety of data points, including:"),
                    tags$ul(
                      tags$li(strong("Incidence Rates:"), " Monthly and annual estimates for new cases of AMI and stroke, highlighting trends and potential areas of concern."),
                      tags$li(strong("Mortality Data:"), " Comprehensive records on deaths attributable to AMI and stroke, offering a crucial perspective on the severity and impact of these conditions."),
                      tags$li(strong("Proportion Receiving Aspirin Acutely:"), " An essential metric tracking the percentage of patients who received aspirin shortly after an AMI or stroke event. This data is critical for evaluating the timeliness and effectiveness of initial patient management protocols.")
                    ),
                    p("We invite you to navigate through the different sections of the dashboard to explore the data. This information is a cornerstone for continuous quality improvement initiatives and the development of targeted public health campaigns."),
                    p("The data presented here is an estimated representation of real-world outcomes and is subject to updates as more information is compiled. We are committed to maintaining the highest level of accuracy and transparency in our reporting.")
                )
              )
      ),
      # AMI Page
      tabItem(tabName = "ami",
              h2("Acute Myocardial Infarction (AMI) Data"),
              p("This section provides detailed data and insights related to AMI cases in Barbados.")
      ),
      # Stroke Page
      tabItem(tabName = "stroke",
              h2("Stroke Data"),
              p("This section provides detailed data and insights related to stroke cases in Barbados.")
      )
    )
  )
)

server <- function(input, output) {
  # Server logic can be added here for dynamic content
}

shinyApp(ui, server)