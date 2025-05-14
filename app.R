# Load packages
library(bslib)
library(dplyr)
library(ggplot2)
library(gplots)
library(heatmaply)
library(htmlwidgets)
library(plotly)
library(readxl)
library(rmarkdown)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(shinyvalidate)
library(stringr)
library(tidyverse)

prefill <- list("Prefilled", "Blank")
operations <- list("Conventional", "Organic")
crops <- list("Cabbage", "Sweet Corn", "Tomato")
tabID <- c("start_here", "receipt_input", "cc_input", "var_input", "fix_input", "budgets")
add_s <- c("acre", "application", "dozen", "gallon", "hour", "instance", "payment", "piece", "pound", "ton", "unit", "use")

# Create sidebar ----
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  minified = F,
  # Menu options
  sidebarMenu(
    id = "tabs",
    # Start here
    menuItem("Start Here", tabName = "start_here"),
    # Receipts (income)
    menuItem("Receipts", tabName = "receipt_input"),
    # Cover Crop Costs
    menuItem("Cover Crop Costs", tabName = "cc_input"),
    # Variable Costs
    menuItem("Variable Costs", tabName = "var_input"),
    # Fixed Costs
    menuItem("Fixed Costs", tabName = "fix_input"),
    # Cover crop scenarios
    menuItem("Cover Crop Scenarios", tabName = "budgets"),
    # Report and sensitivity analysis
    menuItem("Report", tabName = "sensitivity_analysis")
  )
)

# Create header ----
header <- dashboardHeader(
  title = "Enterprise Budget",
  leftUi = tagList(
    dropdownButton(
      label = "Help",
      icon = icon("question-circle"),
      status = "primary",
      circle = T,
      size = "sm",
      tooltip = T,
      width = "50%",
      helpText(
        textOutput("help")
      )
    )
  )
)

# Create body ----
body <- dashboardBody(
  includeCSS("www/text.css"),
  tabItems(
    # Start here tab
    tabItem(
      tabName = "start_here",
      fluidRow(
        box(
          title = "Start Here",
          width = 12,
          status = "primary",
          fluidRow(
            align = "center",
            column(
              width = 12,
              HTML('<center><img src="header_image.jpg" width = "100%" ></center>')
            )
          ),
          headerPanel(""),
          htmlOutput("welcome"),
          htmlOutput("acre_directions"),
          headerPanel(""),
          fluidRow(
            align = "center",
            selectInput("prefillType", "Would you like prices and quantities to be prefilled?", prefill, width = "30%"),
            headerPanel("")
          ),
          headerPanel(""),
          fluidRow(
            align = "center",
            selectInput("operationType", "What method do you use for your agricultural practice?", operations, width = "30%"),
            headerPanel("")
          ),
          headerPanel(""),
          fluidRow(
            align = "center",
            selectInput("cropType", "Which crop would you like to model?", crops, width = "30%"),
            headerPanel("")
          ),
          headerPanel(""),
          fluidRow(
            column(
              width = 6,
              align = "center",
              selectInput("measure", "Which measurement do you use?",
                          choices = c("acre", "1000 sq.ft", "enterprise"))
            ),
            useShinyjs(),
            htmlOutput("acre_input")
          ),
          headerPanel(""),
          column(
            width = 12,
            align = "center",
            actionBttn("bed_feet_calculator", "Bed Feet to Square Feet Calculator")
          ),
          headerPanel(""),
          column(
            width = 12,
            fluidRow(
              align = "center",
              actionBttn("begin", "Begin", block = T),
              headerPanel("")
            ),
            fluidRow(
              align = "center",
              HTML('<center><img src="banner.png" width = "100%" ></center>')
            )
          )
        )
      )
    ),
    # Receipt tab
    tabItem(
      tabName = "receipt_input",
      fluidRow(
        box(
          title = "Receipts",
          status = "primary",
          width = 12,
          htmlOutput("receipt_directions"),
          headerPanel(""),
          uiOutput("receipt_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionBttn("prevPage", "Previous Page")
            ),
            column(
              width = 6,
              align = "center",
              actionBttn("nextPage", "Next Page")
            )
          )
        )
      )
    ),
    # Cover crop costs tab
    tabItem(
      tabName = "cc_input",
      fluidRow(
        box(
          title = "Cover Crop Costs",
          status = "primary",
          width = 12,
          htmlOutput("cc_disclaimer"),
          htmlOutput("cc_directions"),
          headerPanel(""),
          uiOutput("fc_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionButton("add_fc_her_row_button", "Add No Cover Crop Herbicide Row")
            ),
            column(
              width = 6,
              align = "center",
              actionButton("remove_fc_her_row_button", "Remove No Cover Crop Herbicide Row"),
            )
          ),
          headerPanel(""),
          uiOutput("ic_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionButton("add_ic_her_row_button", "Add Interseeding Cover Crop Herbicide Row")
            ),
            column(
              width = 6,
              align = "center",
              actionButton("remove_ic_her_row_button", "Remove Interseeding Cover Crop Herbicide Row"),
            )
          ),
          headerPanel(""),
          uiOutput("lc_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionButton("add_lc_her_row_button", "Add Fall Cover Crop Herbicide Row")
            ),
            column(
              width = 6,
              align = "center",
              actionButton("remove_lc_her_row_button", "Remove Fall Cover Crop Herbicide Row"),
            )
          ),
          headerPanel(""),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionBttn("prevPage", "Previous Page")
            ),
            column(
              width = 6,
              align = "center",
              actionBttn("nextPage", "Next Page")
            )
          )
        )
      )
    ),
    # Variable costs tab
    tabItem(
      tabName = "var_input",
      fluidRow(
        box(
          title = "Variable Costs",
          status = "primary",
          width = 12,
          htmlOutput("var_directions"),
          headerPanel(""),
          uiOutput("fer_input"),
          fluidRow(
            
            column(
              width = 6,
              align = "center",
              actionButton("add_fer_row_button", "Add Fertilizer Row"),
            ),
            column(
              width = 6,
              align = "center",
              actionButton("remove_fer_row_button", "Remove Fertilizer Row")
            )
          ),
          # uiOutput("her_input"),
          # fluidRow(
          #   column(
          #     width = 6,
          #     align = "center",
          #     actionButton("remove_her_row_button", "Remove Herbicide Row")
          #   ),
          #   column(
          #     width = 6,
          #     align = "center",
          #     actionButton("add_her_row_button", "Add Herbicide Row"),
          #   )
          # ),
          uiOutput("ins_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionButton("add_ins_row_button", "Add Insecticide Row"),
            ),
            column(
              width = 6,
              align = "center",
              actionButton("remove_ins_row_button", "Remove Insecticide Row")
            )
          ),
          uiOutput("fun_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionButton("add_fun_row_button", "Add Fungicide Row"),
            ),
            column(
              width = 6,
              align = "center",
              actionButton("remove_fun_row_button", "Remove Fungicide Row")
            )
          ),
          uiOutput("var_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionBttn("prevPage", "Previous Page")
            ),
            column(
              width = 6,
              align = "center",
              actionBttn("nextPage", "Next Page")
            )
          )
        )
      )
    ),
    # Fixed costs tab
    tabItem(
      tabName = "fix_input",
      fluidRow(
        box(
          title = "Fixed Costs Borne By This Enterprise",
          status = "primary",
          width = 12,
          htmlOutput("fix_directions"),
          htmlOutput("fix_directions2"),
          headerPanel(""),
          fluidRow(
              align = "center",
              numericInputIcon("percent_of_operation", "Percent of total fixed costs attributed to this crop", value = 100, icon = list(NULL, "%"), width = "30%")
          ),
          uiOutput("fix_input"),
          fluidRow(
            column(
              width = 6,
              align = "center",
              actionBttn("prevPage", "Previous Page")
            ),
            column(
              width = 6,
              align = "center",
              actionBttn("nextPage", "Next Page")
            )
          )
        )
      )
    ),
    # Budgets
    tabItem(
      tabName = "budgets",
      navset_pill(
        nav_panel(
          "No Cover Crop",
          uiOutput("fallow_benefits")
        ),
        nav_panel(
          "Interseeding",
          uiOutput("interseeding_benefits")
        ),
        nav_panel(
          "Late Season",
          uiOutput("lateSeason_benefits")
        ),
        nav_panel(
          "Direct Comparison",
          uiOutput("direct_comparison")
        )
      )
    ),
    # Sensitivity Analysis
    tabItem(
      tabName = "sensitivity_analysis",
      fluidRow(
        box(
          width = 12,
          status = 'primary',
          title = "Sensitivity Analysis",
          fluidRow(
            column(
              width = 12,
              align = 'center',
              htmlOutput("sensitivity_directions"),
              headerPanel(""),
              selectInput("inputData", "Would you like to have the net returns be with or without cover crop costs?", 
                          choices = c("With no cover crop costs", 
                                      "With interseeding cover crop costs", 
                                      "With late season cover crop costs"), 
                          width = '50%')
            )
          ),
          fluidRow(
            column(
              width = 12,
              align = 'center',
              useShinyjs(),
              downloadBttn("report", "Download Report",
                           style = "bordered"),
              tags$script(HTML("
                function animateLoading() {
                  var loadingText = ['Downloading report.', 'Downloading report..', 'Downloading report...'];
                  var index = 0;
                  return setInterval(function() {
                    $('#loadingText').text(loadingText[index]);
                    index = (index + 1) % loadingText.length;
                  }, 500);
                }
              "))
            )
          ),
          uiOutput("sensitivity_analysis")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Create an input validator
  iv <- InputValidator$new()
  
  # Add validation rules
  iv$add_rule("numeric_inputs", function(value) {
    if (is.na(value) || value == "" || is.null(value)) {
      "Please enter a valid number"
    }
  })
  
  # Enable the validator
  iv$enable()
  
  # Read Excel ----
  # Create read_excel_data function
  read_excel_data <- function(file_name) {
    # Read excel file with error handling
    data <- tryCatch(
      read_excel(file_name),
      error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error reading the Excel file:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }
    )
    if (!is.null(data)) {
      data <- data %>%
        mutate(across(where(is.numeric), as.numeric))
    }
    return(data)
  }
  
  initial_values <- reactiveValues(receipts = NULL)
  
  # Receipts
  receipt_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/receipts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$receipts <- data
    return(data)
  })
  
  # Variable costs
  var_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/varCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$var_data <- data
    return(data)
  })
  
  updated_var_data <- reactiveVal(NULL)
  
  observe({
    updated_var_data(var_data())
  })
  
  # Fertilizer costs
  fer_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/ferCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$fer_data <- data
    return(data)
  })
  
  updated_fer_data <- reactiveVal(NULL)
  
  observe({
    updated_fer_data(fer_data())
  })
  
  # Herbicide costs
  her_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/herCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$her_data <- data
    return(data)
  })
  
  updated_her_data <- reactiveVal(NULL)
  
  observe({
    updated_her_data(her_data())
  })
  
  # Insecticide costs
  ins_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/insCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$ins_data <- data
    return(data)
  })
  
  updated_ins_data <- reactiveVal(NULL)
  
  observe({
    updated_ins_data(ins_data())
  })
  
  # Fungicide costs
  fun_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/funCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$fun_data <- data
    return(data)
  })
  
  updated_fun_data <- reactiveVal(NULL)
  
  observe({
    updated_fun_data(fun_data())
  })
  
  # Fallow cover crop costs
  fc_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/fcCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$fc_data <- data
    return(data)
  })
  
  updated_fc_data <- reactiveVal(NULL)
  
  observe({
    updated_fc_data(fc_data())
  })
  
  # Interseeding cover crop costs
  ic_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/icCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$ic_data <- data
    return(data)
  })
  
  updated_ic_data <- reactiveVal(NULL)
  
  observe({
    updated_ic_data(ic_data())
  })
  
  # Late season cover crop costs
  lc_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/lcCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$lc_data <- data
    return(data)
  })
  
  updated_lc_data <- reactiveVal(NULL)
  
  observe({
    updated_lc_data(lc_data())
  })
  
  # Fixed costs
  fix_data <- reactive({
    file_name <- paste0(input$prefillType, "/", input$operationType, "/fixCosts_", input$cropType, ".xlsx")
    data <- read_excel_data(file_name)
    initial_values$fix_data <- data
    return(data)
  })
  
  # Benefits
  ## Fallow
  fallow_benefits_data <- reactive({
    file_name <- paste0("fallow_benefits.txt")
    data <- readLines(file_name)
    string <- HTML(paste(data, sep = "\n"))
    return(string)
  })
  
  ## Interseeding
  interseeding_benefits_data <- reactive({
    file_name <- paste0("interseeding_benefits.txt")
    data <- readLines(file_name)
    string <- HTML(paste(data, sep = "/n"))
    return(string)
  })
  
  ## Late Season
  lateSeason_benefits_data <- reactive({
    file_name <- paste0("lateSeason_benefits.txt")
    data <- readLines(file_name)
    string <- HTML(paste(data, sep = "/n"))
    return(string)
  })
  
  # Null error handling
  if (is.null(receipt_data)) return()
  if (is.null(var_data)) return()
  if (is.null(fix_data)) return()
  
  # Start here input ----
  output$start_here <- renderUI({
    column(
      width = 12,
      fluidRow(
        align = "center",
        HTML('<center><img src="header_image2.jpg" width = "100%" ></center>')
      ),
      fluidRow(
        headerPanel(""),
        renderText(
          paste0(
            "Welcome to the cover crop in mixed vegetable systems enterprise budget tool developed by the University of Maine. 
            The purpose of this tool is to support farmer cover cropping and vegetable crop production decision making by comparing financial scenarios. 
            This calculator can be used to assess current/past budgets under various cover cropping strategies, OR to project scenarios of future crop management approaches.
            Our dashboard comprises two components: User Input and Scenario Modeling. 
            Please select your crop of interest. "
          )
        ),
        headerPanel(""),
        headerPanel("")
      ),
      fluidRow(
        align = "center",
        selectInput("operationType", "What method do you use for your agricultural practice?", operations, width = "30%"),
        headerPanel("")
      ),
      headerPanel(""),
      headerPanel(""),
      fluidRow(
        align = "center",
        selectInput("cropType", "Which crop would you like to model?", crops, width = "30%"),
        headerPanel("")
      )
    )
  })

  
  # Directions ----
  ## Receipts
  output$receipt_directions <- renderUI({
    raw_text <- paste0(
      "1. This is the receipt page. Please enter the quantity of crop sold per ",
      measure(),
      " and the price per unit sold.\n",
      "2. When you have entered these values, press the 'Next Page' button at the bottom.\n",
      "Note: Default quantities and prices are included merely as examples. ",
      "It is imperative to enter values that apply to your own operation for this tool to be useful. ",
      "If you are selling into multiple outlets within these subgroups, multiply the expected prices of each outlet by the expected proportion going to each outlet, ",
      "then add these together to get expected average price per unit.\n",
      "Example:\n",
      "&nbsp;&nbsp;&nbsp;&nbsp;- 3000 units sold at $4 per unit and \n",
      "&nbsp;&nbsp;&nbsp;&nbsp;- 7000 units sold at $3 per unit.\n",
      "<p class='equation'>($4 &times; 0.3) + ($3 &times; 0.7) = $1.20 + $2.10 = $3.30 expected average price per unit.</p>"
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Cover crop costs
  output$cc_disclaimer <- renderUI({
    raw_text <- paste0(
      "This is the cover crop variable costs page. 
      On this tab there are 3 different cover cropping scenarios represented. 
      THESE ARE THREE DIFFERENT STRATEGIES, AND THEREFORE THESE COSTS REPRESENT THREE
      DIFFERENT HYPOTHETICAL REALITIES. PLEASE FILL OUT THE COSTS AS IF YOU WERE USING
      EACH RESPECTIVE STRATEGY INDEPENDENTLY OF THE OTHER TWO.
      To gain data from only one of these scenarios, only fill out the data for that one. 
      However, this tool is designed to allow you to compare the costs of these different 
      scenarios by entering data for each on this tab."
    )
    
    formatted_text <- paste0(
      "<p class='shiny-text-output'>",
      paste(raw_text, collapse = "</p><p class='shiny-text-output'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  output$cc_directions <- renderUI({
    raw_text <- paste0(
      "1. Please enter the quantity of input used per ",
      measure(),
      " and the price per unit purchased.\n",
      "2. When you have entered these values, press the 'Next Page' button at the bottom.\n",
      "3. There are some costs that will be missing from the general variable costs page, next, that are accounted for on this page.\n",
      "Note: Default quantities and prices are included merely as examples. ",
      "It is imperative to enter values that apply to your own operation for this tool to be useful."
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Variable costs
  output$var_directions <- renderUI({
    raw_text <- paste0(
      "1. This is the variable costs page. Please enter the quantity of input used per ",
      measure(),
      " and the price per unit purchased.\n",
      "2. When you have entered these values, press the 'Next Page' button at the bottom.\n",
      "Note: Default quantities and prices are included merely as examples. ",
      "It is imperative to enter values that apply to your own operation for this tool to be useful."
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Fixed costs
  output$fix_directions <- renderUI({
    raw_text <- paste0(
      "1. This is the fixed costs page. Please enter the quantity of input used and the price per unit purchased.\n",
      "2. When you have entered these values, press the 'Next Page' button at the bottom.\n",
      "Note: Default quantities and prices are included merely as examples. ",
      "It is imperative to enter values that apply to your own operation for this tool to be useful.\n"
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  output$fix_directions2 <- renderUI({
    raw_text <- paste0(
      "To calculate the percent of total fixed costs attributed to this crop, divide 1 by the total number of crops you are growing."
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='bold-indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Acres
  output$welcome <- renderUI({
    raw_text <- paste0("Welcome to the cover crop in mixed vegetable systems enterprise budget tool developed by the University of Maine. The purpose of this tool is to support farmer cover cropping and vegetable crop production decision making by comparing financial scenarios. This calculator can be used to assess current/past budgets under various cover cropping strategies, OR to project scenarios of future crop management approaches.
                       Our dashboard comprises two components: User Input and Scenario Modeling.")
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='shiny-text-output'>",
      paste(paragraphs, collapse = "</p><p class='shiny-text-output'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  output$acre_directions <- renderUI({
    raw_text <- paste0(
      "1. Please indicate whether you would like prefilled default prices and quantities or blank cells.\n",
      "2. Please indicate whether your practice is conventional or organic.\n",
      "3. Please select your crop of interest.\n",
      "4. Please indicate your preferred enterprise area measurement (area planted) and enter the number of units in your operation.\n",
      "5. When you have entered these values, press the 'Begin' button.\n",
      "Note: Default quantity is included merely as an example. ",
      "It is imperative to enter a value that applies to your own operation for this tool to be useful. Numeric input boxes can have typed inputs in addition to arrow scrolling.\n
      This tool is only used on your own machine, and thus will your data will not be saved or used elsewhere.
      Questions? Please contact Assistant Professor Rachel Schattman, Ph.D.\nEmail: rachel.schattman@maine.edu"
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Fallow enterprise budget
  output$fallow_directions <- renderUI({
    raw_text <- paste0(
      "1. The following table is an enterprise budget using your input values. In addition to those in the standard table, this table includes any fallow cover cropping-related costs. 
      2. As before, all quantities are multiplied by prices to get a total cost per ", 
      measure(), ".\n
      3. Then, variable costs are multiplied by total ", measure(), 
      " per operation to get the total variable costs.\n
      4. Fixed costs do not vary with operation size.\n
      5. Fallow cover cropping benefits are explored qualitatively to the right, as they are difficult to quantify."
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Interseeding enterprise budget
  output$interseeding_directions <- renderUI({
    raw_text <- paste0(
      "1. The following table is an enterprise budget using your input values. In addition to those in the standard table, this table includes any interseeding cover cropping-related costs.\n 
      2. As before, all quantities are multiplied by prices to get a total cost per ", 
      measure(), ".\n
      3. Then, variable costs are multiplied by total ", measure(), 
      " per operation to get the total variable costs.\n 
      4. Fixed costs do not vary with operation size.\n
      5. Interseeding cover cropping benefits are explored qualitatively to the right, as they are difficult to quantify."
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Late Season enterprise budget
  output$lateSeason_directions <- renderUI({
    raw_text <- paste0(
      "1. The following table is an enterprise budget using your input values. In addition to those in the standard table, this table includes any late season cover cropping-related costs.\n
      2. As before, all quantities are multiplied by prices to get a total cost per ", 
      measure(), ".\n
      3. Then, variable costs are multiplied by total ", measure(), 
      " per operation to get the total variable costs.\n
      4. Fixed costs do not vary with operation size.\n
      5. Late season cover cropping benefits are explored qualitatively to the right, as they are difficult to quantify."
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Sensitivity Analysis
  output$sensitivity_directions <- renderUI({
    raw_text <- paste0(
      "1. Sensitivity analysis examines how a particular variable of interest changes as key inputs and outputs change.\n
      2. In this case, our value of interest is the net returns of your operation."
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Breakeven analysis
  output$breakeven_directions <- renderUI({
    raw_text <- paste0(
      "1. You can think of the breakeven point for your enterprise as any value holding all other values constant.\n
      2. The most common is output price or harvest quantity that would result in the enterprise breaking even, all else equal. "
    )
    
    paragraphs <- unlist(strsplit(raw_text, "\n"))
    formatted_text <- paste0(
      "<p class='indented-text'>",
      paste(paragraphs, collapse = "</p><p class='indented-text'>"),
      "</p>"
    )
    
    HTML(formatted_text)
  })
  
  ## Help ----
  helpReactive <- reactive({
    current_tab <- input$tabs

    help_text <- switch(current_tab,
                        "start_here" = paste0("Please select the crop you would like to work with and hit the 'Begin' button."),
                        "acre_input" = paste0("This page will create a multiplier for your per-acre values. 
                                              The 'Next Page' button may require a double-click."),
                        "receipt_input" = paste0("These inputs are on a PER ", 
                                                 toupper(measure()), 
                                                 " basis. If there are inputs that you do not use, please change the quantity and/or price to 0. 
                                                 The 'Next Page' button may require a double-click."),
                        "cc_input" = paste0("These inputs are for three SEPARATE styles of cover cropping. 
                                            Thus they will be implemented separately, so quantities and prices will not duplicate across 
                                            strategies. Fall cover cropping refers to cover cropping done post-cash crop harvest. 
                                            If there are inputs that you do not use, 
                                             please change the quantity and/or price to 0. 
                                             The 'Next Page' button may require a double-click."),
                        "var_input" = paste0("These inputs are on a PER ", 
                                             toupper(measure()), 
                                             " basis. If there are inputs that you do not use, 
                                             please change the quantity and/or price to 0. 
                                             The 'Next Page' button may require a double-click."),
                        "fix_input" = paste0("These inputs are on a PER ", 
                                             toupper(measure()), 
                                             " basis. If there are inputs that you do not use, 
                                             please change the quantity and/or price to 0. 
                                             The 'Next Page' button may require a double-click."),
                        "fallow" = paste0("Note additional costs. If you would like to change inputs, use the sidebar to go to the desired category 
                                          and change the input, then return to this page. 
                                                 If some rows display text instead of totals, revisit the 'Size' tab on the sidebar then return to this page."),
                        "late_season" = paste0("Note additional costs. If you would like to change inputs, use the sidebar to go to the desired category 
                                          and change the input, then return to this page. 
                                                 If some rows display text instead of totals, revisit the 'Size' tab on the sidebar then return to this page."),
                        "interseeding" = paste0("Note additional costs. If you would like to change inputs, use the sidebar to go to the desired category 
                                          and change the input, then return to this page. 
                                                 If some rows display text instead of totals, revisit the 'Size' tab on the sidebar then return to this page."),
                        "sensitivity_analysis" = paste0("If unable to view plots due to errors, please be sure to visit the 'Start Here' tab."),
                        "Error: No help text found.")
    
    return(help_text)
  })
  
  output$help <- renderText(
    helpReactive()
  )
  
  # Button Behavior ----
  ## Bed Feet Calculator
  observeEvent(input$bed_feet_calculator, {
    shinyalert(
      html = TRUE,
      title = "Bed Feet Calculator",
      text = tagList(
        numericInput("calc_width", "Width (feet):", value = 0, min = 0),
        numericInput("calc_length", "Length (feet):", value = 0, min = 0),
        numericInput("calc_quantity", "Quantity:", value = 1, min = 1),
        hr(),
        h4("Total 1,000 sq.ft.:"),
        verbatimTextOutput("calc_total_sqft")
      ),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE
    )
  })
  
  # Calculate and display the total
  output$calc_total_sqft <- renderText({
    req(input$calc_width, input$calc_length, input$calc_quantity)
    total <- (input$calc_width * input$calc_length * input$calc_quantity) / 1000
    round(total, 2)
  })
  
  ## Begin
  observeEvent(input$begin, {
    updateTabItems(
      session, "tabs", "receipt_input"
    )
  })
  
  reminder_counter <- reactiveVal(0)
  
  ## Next page
  observeEvent(input$nextPage, {
    current_tab <- input$tabs
    #print(current_tab)
    current_index <- which(tabID == current_tab)
    next_index <- current_index + 1
    
    if (next_index > length(tabID)) {
      next_index <- 1 # Wrap around to the first tab
    }
    
    # next_tab <- tabID[next_index]
    # updateTabItems(session, "tabs", next_tab)
    
    
    ## Change values reminder message
    # tabID <- c("start_here", "receipt_input", "cc_input", "var_input", "fix_input", "budgets")
    if (current_tab == "receipt_input") {
      receipt_data <- receipt_data()
      
      changed <- sapply(1:nrow(receipt_data), function(i) {
        input[[paste0("q", "r", i)]] != initial_values$receipts$Quantity[i] ||
        input[[paste0("p", "r", i)]] != initial_values$receipts$Price[i]
      })
      
      #print(changed)
      
      if (all(!changed) & reminder_counter() == 0) {
        #print("Error: Please change at least one value.")
        reminder_counter(1)
        #print(reminder_counter())
        showModal(
          modalDialog(
            title = "Reminder",
            footer = modalButton("Dismiss"),
            h4("Remember that for this tool to be most helpful, user values should replace example defaults."),
            easyClose = T
          )
        )
      } else {
        #print("Changes detected.")
        next_tab <- tabID[next_index]
        updateTabItems(session, "tabs", next_tab)
        reminder_counter(0)
        #print(reminder_counter())
      }
    } else if (current_tab == "cc_input") {
      data_list <- list(fc_data = fc_data(), ic_data = ic_data(), lc_data = lc_data())
      prefix_list <- list(fc_data = "fc", ic_data = "ic", lc_data = "lc")
      
      changed_list <- lapply(names(data_list), function(name) {
        data <- data_list[[name]]
        prefix <- prefix_list[[name]]
        sapply(1:nrow(data), function(i) {
          input[[paste0("q", prefix, i)]] != initial_values[[name]]$Quantity[i] ||
            input[[paste0("p", prefix, i)]] != initial_values[[name]]$Price[i]
        })
      })
      
      # Process the changes by removing the first entry and handling NAs
      changed_list <- lapply(changed_list, function(changed) {
        changed <- changed[-1]  # Drop the first entry
        changed <- ifelse(is.na(changed), FALSE, changed)  # Replace NA with FALSE
        changed
      })
      
      # Combine the results into a single logical vector
      any_changed <- any(unlist(changed_list))
      
      # Print the result
      #print(any_changed)
      
      if (!any_changed & reminder_counter() == 0) {
        #print("Error: Please change at least one value.")
        reminder_counter(1)
        #print(reminder_counter())
        showModal(
          modalDialog(
            title = "Reminder",
            footer = modalButton("Dismiss"),
            h4("Remember that for this tool to be most helpful, user values should replace example defaults."),
            easyClose = T
          )
        )
      } else {
        #print("Changes detected.")
        next_tab <- tabID[next_index]
        updateTabItems(session, "tabs", next_tab)
        reminder_counter(0)
        #print(reminder_counter())
      }
    } else if (current_tab == "var_input") {
      data_list <- list(var_data = var_data(), fer_data = fer_data(), her_data = her_data(), ins_data = ins_data(), fun_data = fun_data())
      prefix_list <- list(var_data = "v", fer_data = "fer", her_data = "her", ins_data = "ins", fun_data = "fun")
      
      changed_list <- lapply(names(data_list), function(name) {
        data <- data_list[[name]]
        prefix <- prefix_list[[name]]
        sapply(1:nrow(data), function(i) {
          input[[paste0("q", prefix, i)]] != initial_values[[name]]$Quantity[i] ||
            input[[paste0("p", prefix, i)]] != initial_values[[name]]$Price[i]
        })
      })
      
      # Process the changes by removing the first entry and handling NAs
      changed_list <- lapply(changed_list, function(changed) {
        changed <- changed[-1]  # Drop the first entry
        changed <- ifelse(is.na(changed), FALSE, changed)  # Replace NA with FALSE
        changed
      })
      
      # Combine the results into a single logical vector
      any_changed <- any(unlist(changed_list))
      
      # Print the result
      #print(any_changed)
      
      if (!any_changed & reminder_counter() == 0) {
        #print("Error: Please change at least one value.")
        reminder_counter(1)
        #print(reminder_counter())
        showModal(
          modalDialog(
            title = "Reminder",
            footer = modalButton("Dismiss"),
            h4("Remember that for this tool to be most helpful, user values should replace example defaults."),
            easyClose = T
          )
        )
      } else {
        #print("Changes detected.")
        next_tab <- tabID[next_index]
        updateTabItems(session, "tabs", next_tab)
        reminder_counter(0)
        #print(reminder_counter())
      }
    } else if (current_tab == "fix_input") {
      fix_data <- fix_data()
      
      changed <- sapply(1:nrow(fix_data), function(i) {
        input[[paste0("q", "f", i)]] != initial_values$fix_data$Quantity[i] ||
          input[[paste0("p", "f", i)]] != initial_values$fix_data$Price[i]
      })
      
      #print(changed)
      
      if (all(!changed) & reminder_counter() == 0) {
        #print("Error: Please change at least one value.")
        reminder_counter(1)
        #print(reminder_counter())
        showModal(
          modalDialog(
            title = "Reminder",
            footer = modalButton("Dismiss"),
            h4("Remember that for this tool to be most helpful, user values should replace example defaults."),
            easyClose = T
          )
        )
      } else {
        #print("Changes detected.")
        next_tab <- tabID[next_index]
        updateTabItems(session, "tabs", next_tab)
        reminder_counter(0)
        #print(reminder_counter())
      }
    }
  })
  
  ## Previous page
  observeEvent(input$prevPage, {
    current_tab <- input$tabs
    current_index <- which(tabID == current_tab)
    prev_index <- current_index - 1
    
    if (prev_index < 0) {
      next_index <- length(tabID) # Wrap around to the end tab
    }
    
    prev_tab <- tabID[prev_index]
    updateTabItems(session, "tabs", prev_tab)
  })

  # Input field function definition ----
  ## Generate operation size ui
  measure <- reactive({
    input$measure
  })
  
  output$acre_input <- renderUI({
    column(
      width = 6,
      align = "center",
      numericInput("acres", 
                   paste0("Number of ", measure(),
                          if (!is.null(measure()) && measure() %in% add_s) {"s"},
                          " in ", tolower(input$cropType),
                          " enterprise:"),
                   value = 1, 
                   min = 0,
                   step = 0.5),
      bsPopover("acres", title = "Tip",
                "Numeric Input boxes like this one can be typed in as well as adjusted incrementally with the arrows. Enterprise can only have a value of 1.")
    )
  })
  
  observe({
    # This will trigger whenever measure() changes AND when acres input exists
    req(measure(), input$acres)
    
    if (measure() == "enterprise") {
      shinyjs::disable("acres")
      updateNumericInput(session, "acres", value = 1, min = 1, max = 1)
    } else {
      shinyjs::enable("acres")
      updateNumericInput(session, "acres", min = 0, max = Inf)
    }
  })
  
  ## Function that creates ui based on the specified dataframe and prefix
  create_ui <- function(data, prefix) {
    lapply(1:nrow(data), function(i) {
      if (data[i, "Unit"] != "HEADER") {
        fluidRow(
          column(
            width = 6,
            if (!is.na(data[i, "qInfo"])) {numericInputIcon(
              paste0("q", prefix, i),
              paste0(ifelse(data[i, "Unit"] == "NA", "", str_to_title(data[i, "Unit"])), 
                     if (data[i, "Unit"] %in% add_s) {"s"}, 
                     if (data[i, "Unit"] != "NA") {" of "}, 
                     data[i, "Item"],
                     " per ", measure(), ":"),
              # paste0("Quantity of ", data[i, "Item"], " in ", data[i, "Unit"], "s :"), ## OLD METHOD
              value = ifelse(is.na(data[i, "Quantity"]), 0, data[i, "Quantity"]),
              step = 0.5,
              icon = list(NULL, popify(icon("info-circle", style = "color: red;"),
                                       title = paste0(data[i, "Item"]),
                                       content = paste0(data[i, "qInfo"]),
                                       trigger = "hover",
                                       # trigger = "click"
              ))
            )}
            else {numericInput(
              paste0("q", prefix, i),
              paste0(ifelse(data[i, "Unit"] == "NA", "", str_to_title(data[i, "Unit"])), 
                     if (data[i, "Unit"] %in% add_s) {"s"}, 
                     if (data[i, "Unit"] != "NA") {" of "}, 
                     data[i, "Item"],
                     " per ", measure(), ":"),
              # paste0("Quantity of ", data[i, "Item"], " in ", data[i, "Unit"], "s :"), ## OLD METHOD
              value = ifelse(is.na(data[i, "Quantity"]), 0, data[i, "Quantity"]),
              step = 0.5
            )}
          ),
          column(
            width = 6,
            numericInputIcon(
              paste0("p", prefix, i),
              paste0("Price ", 
                     if (data[i, "Unit"] == "NA") {""}
                     else {paste0( " per ", data[i, "Unit"])},
                     " ",
                     if (data[i, "Unit"] != "dozen") {"of "},
                     data[i, "Item"], ":"),
              # paste0(data[i, "Item"], " price per ", data[i, "Unit"], ":"), ## OLD METHOD
              value = ifelse(is.na(data[i, "Price"]), 0, data[i, "Price"]),
              icon = list(icon("dollar-sign"), 
                          if (!is.na(data[i, "pInfo"])) {popify(icon("info", style = "color: red;"),
                                                                title = paste0(data[i, "Item"]),
                                                                content = paste0(data[i, "pInfo"]),
                                                                trigger = "hover"
                                                                # trigger = "click"
                          )}
                          else {NULL}
                          ), 
              step = 0.01
            )
          )
        )
      }
      else {
        fluidRow(
          headerPanel(paste0(data[i, "Item"]))
        )
      }
    })
  }
  
  create_fix_ui <- function(data, prefix) {
    lapply(1:nrow(data), function(i) {
      fluidRow(
        column(
          width = 6,
          if (!is.na(data[i, "qInfo"])) {numericInputIcon(
            paste0("q", prefix, i),
            if (data[i, "Unit"] == "payment") {paste0(data[i, "Item"], " ", data[i, "Unit"], 
                                                      if (data[i, "Unit"] %in% add_s) {"s"})}
            else {paste0(str_to_title(data[i, "Unit"]), 
                         if (data[i, "Unit"] %in% add_s) {"s"}, 
                         " of ", data[i, "Item"])},
            # paste0("Quantity of ", data[i, "Item"], " in ", data[i, "Unit"], "s :"), ## OLD METHOD
            value = ifelse(is.na(data[i, "Quantity"]), 0, data[i, "Quantity"]),
            icon = list(NULL, popify(icon("info", style = "color: red;"),
                                     title = paste0(data[i, "Item"]),
                                     content = paste0(data[i, "qInfo"]),
                                     trigger = "hover",
                                     # trigger = "click"
            ))
          )}
          else {numericInput(
            paste0("q", prefix, i),
            if (data[i, "Unit"] == "payment") {paste0(data[i, "Item"], " ", data[i, "Unit"], 
                                                      if (data[i, "Unit"] %in% add_s) {"s"})}
            else {paste0(str_to_title(data[i, "Unit"]), 
                         if (data[i, "Unit"] %in% add_s) {"s"}, 
                         " of ", data[i, "Item"])},
            # paste0("Quantity of ", data[i, "Item"], " in ", data[i, "Unit"], "s :"), ## OLD METHOD
            value = ifelse(is.na(data[i, "Quantity"]), 0, data[i, "Quantity"])
          )}
        ),
        column(
          width = 6,
          numericInputIcon(
            paste0("p", prefix, i),
            paste0("Price per ", data[i, "Unit"], " of ", data[i, "Item"], ":"),
            # paste0(data[i, "Item"], " price per ", data[i, "Unit"], ":"), ## OLD METHOD
            value = ifelse(is.na(data[i, "Price"]), 0, data[i, "Price"]),
            icon = list(icon("dollar-sign"),
                        if (!is.na(data[i, "pInfo"])) {popify(icon("info", style = "color: red;"),
                                                              title = paste0(data[i, "Item"]),
                                                              content = paste0(data[i, "pInfo"]),
                                                              trigger = "hover",
                                                              # trigger = "click"
                        )}
                        else {NULL}), 
            step = 0.01
          )
        )
      )
    })
  }
  
  create_ui_with_values <- function(data, prefix, values) {
    lapply(1:nrow(data), function(i) {
      if (data[i, "Unit"] != "HEADER") {
        fluidRow(
          column(
            width = 6,
            if (!is.na(data[i, "qInfo"])) {
              numericInputIcon(
                paste0("q", prefix, i),
                paste0(
                  ifelse(data[i, "Unit"] == "NA", "", str_to_title(data[i, "Unit"])),
                  if (data[i, "Unit"] %in% add_s) {"s"},
                  if (data[i, "Unit"] != "NA") {" of "}, data[i, "Item"], " per ",
                  measure(), ":"
                ),
                value = ifelse(is.null(values[[paste0("q", prefix, i)]]), 
                               ifelse(is.na(data[i, "Quantity"]), 0, data[i, "Quantity"]),
                               values[[paste0("q", prefix, i)]]),
                step = 0.5,
                icon = list(NULL, popify(
                  icon("info", style = "color: red;"),
                  title = paste0(data[i, "Item"]),
                  content = paste0(data[i, "qInfo"]),
                  trigger = "hover"
                ))
              )
            } else {
              numericInput(
                paste0("q", prefix, i),
                paste0(
                  ifelse(data[i, "Unit"] == "NA", "", str_to_title(data[i, "Unit"])),
                  if (data[i, "Unit"] %in% add_s) {"s"},
                  if (data[i, "Unit"] != "NA") {" of "}, data[i, "Item"], " per ",
                  measure(), ":"
                ),
                value = ifelse(is.null(values[[paste0("q", prefix, i)]]), 
                               ifelse(is.na(data[i, "Quantity"]), 0, data[i, "Quantity"]),
                               values[[paste0("q", prefix, i)]]),
                step = 0.5
              )
            }
          ),
          column(
            width = 6,
            numericInputIcon(
              paste0("p", prefix, i),
              paste0(
                "Price ",
                if (data[i, "Unit"] == "NA") {""}
                else {paste0(" per ", data[i, "Unit"])}, " of ",
                data[i, "Item"], ":"
              ),
              value = ifelse(is.null(values[[paste0("p", prefix, i)]]), 
                             ifelse(is.na(data[i, "Price"]), 0, data[i, "Price"]),
                             values[[paste0("p", prefix, i)]]),
              icon = list(
                icon("dollar-sign"), 
                if (!is.na(data[i, "pInfo"])) {
                  popify(
                    icon("info", style = "color: red;"),
                    title = paste0(data[i, "Item"]),
                    content = paste0(data[i, "pInfo"]),
                    trigger = "hover"
                  )
                } else {NULL}
              ),
              step = 0.01
            )
          )
        )
      } else {
        fluidRow(
          headerPanel(paste0(data[i, "Item"]))
        )
      }
    })
  }
  
  
  # Receipts
  output$receipt_input <- renderUI({
    receipt_data <- receipt_data()
    create_ui(receipt_data, "r")
  })
  
  # Variable costs
  output$var_input <- renderUI({
    var_data <- var_data()
    create_ui(var_data, "v")
  })
  
  # Fertilizer costs
  ferValues <- reactiveValues()
  
  output$fer_input <- renderUI({
    fer_data <- updated_fer_data()
    create_ui_with_values(fer_data, "fer", ferValues)
  })
  
  # Add Fertilizer Row Button
  ferCounter <- reactiveVal(1)
  
  observeEvent(input$add_fer_row_button, {
    original_data <- updated_fer_data()
    
    new_value <- ferCounter() + 1
    ferCounter(new_value)
    
    new_row <- data.frame(
      Item = paste0("Fertilizer ", ferCounter()),
      Quantity = 0,
      Unit = "pound",
      Price = 0,
      Total = NA,
      qInfo = NA,
      pInfo = NA
    )
    
    #print(new_row)
    
    insert_index <- nrow(original_data)
    #print(insert_index)
    
    updated_data <- rbind(
      original_data[0:(insert_index[1]), ],
      new_row
    )
    
    # #print(updated_data, n=nrow(updated_data))
    
    updated_fer_data(updated_data)
    # #print(updated_var_data(), n=nrow(updated_var_data()))
    
    for (i in 1:nrow(updated_data)) {
      ferValues[[paste0("qfer", i)]] <- input[[paste0("qfer", i)]]
      ferValues[[paste0("pfer", i)]] <- input[[paste0("pfer", i)]]
    }
  }) 
  
  # Remove Fertilizer Row Button
  observeEvent(input$remove_fer_row_button, {
    
    if (ferCounter() != 1) {
      original_data <- updated_fer_data()
      
      new_value <- ferCounter() - 1
      ferCounter(new_value)
      
      insert_index <- nrow(original_data)
      
      updated_data <- rbind(
        original_data[0:((insert_index[1]) - 1), ]
      )
      
      updated_fer_data(updated_data)
      
      for (i in 1:nrow(updated_data)) {
        ferValues[[paste0("qfer", i)]] <- input[[paste0("qfer", i)]]
        ferValues[[paste0("pfer", i)]] <- input[[paste0("pfer", i)]]
      }
    } else {
      showModal(
        modalDialog(
        title = "Too few rows!",
        footer = modalButton("Dismiss"),
        h4("You cannot have less than one row for this category. If you do not want it included in the total, simply enter zeroes for the values."),
        easyClose = T
        )
      )
    }
  })
  
  # Herbicide costs
  herValues <- reactiveValues()
  
  output$her_input <- renderUI({
    her_data <- updated_her_data()
    create_ui_with_values(her_data, "her", herValues)
  })
  
  # Add Row Button
  herCounter <- reactiveVal(1)
  
  observeEvent(input$add_her_row_button, {
    original_data <- updated_her_data()
    
    new_value <- herCounter() + 1
    herCounter(new_value)
    
    new_row <- data.frame(
      Item = paste0("Herbicide ", herCounter()),
      Quantity = 0,
      Unit = "pound",
      Price = 0,
      Total = NA,
      qInfo = NA,
      pInfo = NA
    )
    
    #print(new_row)
    
    insert_index <- nrow(original_data)
    #print(insert_index)
    
    updated_data <- rbind(
      original_data[0:(insert_index[1]), ],
      new_row
    )
    
    # #print(updated_data, n=nrow(updated_data))
    
    updated_her_data(updated_data)
    # #print(updated_var_data(), n=nrow(updated_var_data()))
    
    for (i in 1:nrow(updated_data)) {
      herValues[[paste0("qher", i)]] <- input[[paste0("qher", i)]]
      herValues[[paste0("pher", i)]] <- input[[paste0("pher", i)]]
    }
  }) 
  
  # Remove Herbicide Row Button
  observeEvent(input$remove_her_row_button, {
    
    if (herCounter() != 1) {
      original_data <- updated_her_data()
      
      new_value <- herCounter() - 1
      herCounter(new_value)
      
      insert_index <- nrow(original_data)
      
      updated_data <- rbind(
        original_data[0:((insert_index[1]) - 1), ]
      )
      
      updated_her_data(updated_data)
      
      for (i in 1:nrow(updated_data)) {
        herValues[[paste0("qher", i)]] <- input[[paste0("qher", i)]]
        herValues[[paste0("pher", i)]] <- input[[paste0("pher", i)]]
      }
    } else {
      showModal(
        modalDialog(
          title = "Too few rows!",
          footer = modalButton("Dismiss"),
          h4("You cannot have less than one row for this category. If you do not want it included in the total, simply enter zeroes for the values."),
          easyClose = T
        )
      )
    }
  })
  
  # Insecticide costs
  insValues <- reactiveValues()
  
  output$ins_input <- renderUI({
    ins_data <- updated_ins_data()
    create_ui_with_values(ins_data, "ins", insValues)
  })
  
  # Add Row Button
  insCounter <- reactiveVal(1)
  
  observeEvent(input$add_ins_row_button, {
    original_data <- updated_ins_data()
    
    new_value <- insCounter() + 1
    insCounter(new_value)
    
    new_row <- data.frame(
      Item = paste0("Insecticide ", insCounter()),
      Quantity = 0,
      Unit = "pound",
      Price = 0,
      Total = NA,
      qInfo = NA,
      pInfo = NA
    )
    
    #print(new_row)
    
    insert_index <- nrow(original_data)
    #print(insert_index)
    
    updated_data <- rbind(
      original_data[0:(insert_index[1]), ],
      new_row
    )
    
    # #print(updated_data, n=nrow(updated_data))
    
    updated_ins_data(updated_data)
    # #print(updated_var_data(), n=nrow(updated_var_data()))
    
    for (i in 1:nrow(updated_data)) {
      insValues[[paste0("qins", i)]] <- input[[paste0("qins", i)]]
      insValues[[paste0("pins", i)]] <- input[[paste0("pins", i)]]
    }
  }) 
  
  # Remove Insecticide Row Button
  observeEvent(input$remove_ins_row_button, {
    
    if (insCounter() != 1) {
      original_data <- updated_ins_data()
      
      new_value <- insCounter() - 1
      insCounter(new_value)
      
      insert_index <- nrow(original_data)
      
      updated_data <- rbind(
        original_data[0:((insert_index[1]) - 1), ]
      )
      
      updated_ins_data(updated_data)
      
      for (i in 1:nrow(updated_data)) {
        insValues[[paste0("qins", i)]] <- input[[paste0("qins", i)]]
        insValues[[paste0("pins", i)]] <- input[[paste0("pins", i)]]
      }
    } else {
      showModal(
        modalDialog(
          title = "Too few rows!",
          footer = modalButton("Dismiss"),
          h4("You cannot have less than one row for this category. If you do not want it included in the total, simply enter zeroes for the values."),
          easyClose = T
        )
      )
    }
  })
  
  # Fungicide costs
  funValues <- reactiveValues()
  
  output$fun_input <- renderUI({
    fun_data <- updated_fun_data()
    create_ui_with_values(fun_data, "fun", funValues)
  })
  
  # Add Row Button
  funCounter <- reactiveVal(1)
  
  observeEvent(input$add_fun_row_button, {
    original_data <- updated_fun_data()
    
    new_value <- funCounter() + 1
    funCounter(new_value)
    
    new_row <- data.frame(
      Item = paste0("Fungicide ", funCounter()),
      Quantity = 0,
      Unit = "pound",
      Price = 0,
      Total = NA,
      qInfo = NA,
      pInfo = NA
    )
    
    #print(new_row)
    
    insert_index <- nrow(original_data)
    #print(insert_index)
    
    updated_data <- rbind(
      original_data[0:(insert_index[1]), ],
      new_row
    )
    
    # #print(updated_data, n=nrow(updated_data))
    
    updated_fun_data(updated_data)
    # #print(updated_var_data(), n=nrow(updated_var_data()))
    
    for (i in 1:nrow(updated_data)) {
      funValues[[paste0("qfun", i)]] <- input[[paste0("qfun", i)]]
      funValues[[paste0("pfun", i)]] <- input[[paste0("pfun", i)]]
    }
  }) 
  
  # Remove Fungicide Row Button
  observeEvent(input$remove_fun_row_button, {
    
    if (funCounter() != 1) {
      original_data <- updated_fun_data()
      
      new_value <- funCounter() - 1
      funCounter(new_value)
      
      insert_index <- nrow(original_data)
      
      updated_data <- rbind(
        original_data[0:((insert_index[1]) - 1), ]
      )
      
      updated_fun_data(updated_data)
      
      for (i in 1:nrow(updated_data)) {
        funValues[[paste0("qfun", i)]] <- input[[paste0("qfun", i)]]
        funValues[[paste0("pfun", i)]] <- input[[paste0("pfun", i)]]
      }
    } else {
      showModal(
        modalDialog(
          title = "Too few rows!",
          footer = modalButton("Dismiss"),
          h4("You cannot have less than one row for this category. If you do not want it included in the total, simply enter zeroes for the values."),
          easyClose = T
        )
      )
    }
  })
  
  # Fallow cover crop costs
  # output$fc_input <- renderUI({
  #   fc_data <- fc_data()
  #   create_ui(fc_data, "fc")
  # })
  
  #####
  
  # Fallow Herbicide costs
  fcHerValues <- reactiveValues()
  
  output$fc_input <- renderUI({
    fc_data <- updated_fc_data()
    create_ui_with_values(fc_data, "fc", fcHerValues)
  })
  
  # Add Row Button
  fcHerCounter <- reactiveVal(1)
  
  observeEvent(input$add_fc_her_row_button, {
    original_data <- updated_fc_data()
    
    new_value <- fcHerCounter() + 1
    fcHerCounter(new_value)
    
    new_row <- data.frame(
      Item = paste0("No Cover Crop Herbicide ", fcHerCounter()),
      Quantity = 0,
      Unit = "pound",
      Price = 0,
      Total = NA,
      qInfo = NA,
      pInfo = NA
    )
    
    #print(new_row)
    
    insert_index <- nrow(original_data)
    #print(insert_index)
    
    updated_data <- rbind(
      original_data[0:(insert_index[1]), ],
      new_row
    )
    
    # #print(updated_data, n=nrow(updated_data))
    
    updated_fc_data(updated_data)
    # #print(updated_var_data(), n=nrow(updated_var_data()))
    
    for (i in 1:nrow(updated_data)) {
      fcHerValues[[paste0("qfc", i)]] <- input[[paste0("qfc", i)]]
      fcHerValues[[paste0("pfc", i)]] <- input[[paste0("pfc", i)]]
    }
  }) 
  
  # Remove Herbicide Row Button
  observeEvent(input$remove_fc_her_row_button, {
    
    if (fcHerCounter() != 1) {
      original_data <- updated_fc_data()
      
      new_value <- fcHerCounter() - 1
      fcHerCounter(new_value)
      
      insert_index <- nrow(original_data)
      
      updated_data <- rbind(
        original_data[0:((insert_index[1]) - 1), ]
      )
      
      updated_fc_data(updated_data)
      
      for (i in 1:nrow(updated_data)) {
        fcHerValues[[paste0("qfc", i)]] <- input[[paste0("qfc", i)]]
        fcHerValues[[paste0("pfc", i)]] <- input[[paste0("pfc", i)]]
      }
    } else {
      showModal(
        modalDialog(
          title = "Too few rows!",
          footer = modalButton("Dismiss"),
          h4("You cannot have less than one row for this category. If you do not want it included in the total, simply enter zeroes for the values."),
          easyClose = T
        )
      )
    }
  })
  
  #####
  
  # Interseeding cover crop costs
  # output$ic_input <- renderUI({
  #   ic_data <- ic_data()
  #   create_ui(ic_data, "ic")
  # })
  
  # Interseeding Herbicide costs
  icHerValues <- reactiveValues()
  
  output$ic_input <- renderUI({
    ic_data <- updated_ic_data()
    create_ui_with_values(ic_data, "ic", icHerValues)
  })
  
  # Add Row Button
  icHerCounter <- reactiveVal(1)
  
  observeEvent(input$add_ic_her_row_button, {
    original_data <- updated_ic_data()
    
    new_value <- icHerCounter() + 1
    icHerCounter(new_value)
    
    new_row <- data.frame(
      Item = paste0("Interseeding Cover Crop Herbicide ", icHerCounter()),
      Quantity = 0,
      Unit = "pound",
      Price = 0,
      Total = NA,
      qInfo = NA,
      pInfo = NA
    )
    
    #print(new_row)
    
    insert_index <- nrow(original_data)
    #print(insert_index)
    
    updated_data <- rbind(
      original_data[0:(insert_index[1]), ],
      new_row
    )
    
    # #print(updated_data, n=nrow(updated_data))
    
    updated_ic_data(updated_data)
    # #print(updated_var_data(), n=nrow(updated_var_data()))
    
    for (i in 1:nrow(updated_data)) {
      icHerValues[[paste0("qic", i)]] <- input[[paste0("qic", i)]]
      icHerValues[[paste0("pic", i)]] <- input[[paste0("pic", i)]]
    }
  }) 
  
  # Remove Herbicide Row Button
  observeEvent(input$remove_ic_her_row_button, {
    
    if (icHerCounter() != 1) {
      original_data <- updated_ic_data()
      
      new_value <- icHerCounter() - 1
      icHerCounter(new_value)
      
      insert_index <- nrow(original_data)
      
      updated_data <- rbind(
        original_data[0:((insert_index[1]) - 1), ]
      )
      
      updated_ic_data(updated_data)
      
      for (i in 1:nrow(updated_data)) {
        icHerValues[[paste0("qic", i)]] <- input[[paste0("qic", i)]]
        icHerValues[[paste0("pic", i)]] <- input[[paste0("pic", i)]]
      }
    } else {
      showModal(
        modalDialog(
          title = "Too few rows!",
          footer = modalButton("Dismiss"),
          h4("You cannot have less than one row for this category. If you do not want it included in the total, simply enter zeroes for the values."),
          easyClose = T
        )
      )
    }
  })
  
  #####
  
  # Late season cover crop costs
  # output$lc_input <- renderUI({
  #   lc_data <- lc_data()
  #   create_ui(lc_data, "lc")
  # })
  
  # Late Season Herbicide costs
  lcHerValues <- reactiveValues()
  
  output$lc_input <- renderUI({
    lc_data <- updated_lc_data()
    create_ui_with_values(lc_data, "lc", lcHerValues)
  })
  
  # Add Row Button
  lcHerCounter <- reactiveVal(1)
  
  observeEvent(input$add_lc_her_row_button, {
    original_data <- updated_lc_data()
    
    new_value <- lcHerCounter() + 1
    lcHerCounter(new_value)
    
    new_row <- data.frame(
      Item = paste0("Fall Cover Crop Herbicide ", lcHerCounter()),
      Quantity = 0,
      Unit = "pound",
      Price = 0,
      Total = NA,
      qInfo = NA,
      pInfo = NA
    )
    
    #print(new_row)
    
    insert_index <- nrow(original_data)
    #print(insert_index)
    
    updated_data <- rbind(
      original_data[0:(insert_index[1]), ],
      new_row
    )
    
    # #print(updated_data, n=nrow(updated_data))
    
    updated_lc_data(updated_data)
    # #print(updated_var_data(), n=nrow(updated_var_data()))
    
    for (i in 1:nrow(updated_data)) {
      lcHerValues[[paste0("qlc", i)]] <- input[[paste0("qlc", i)]]
      lcHerValues[[paste0("plc", i)]] <- input[[paste0("plc", i)]]
    }
  }) 
  
  # Remove Herbicide Row Button
  observeEvent(input$remove_lc_her_row_button, {
    
    if (lcHerCounter() != 1) {
      original_data <- updated_lc_data()
      
      new_value <- lcHerCounter() - 1
      lcHerCounter(new_value)
      
      insert_index <- nrow(original_data)
      
      updated_data <- rbind(
        original_data[0:((insert_index[1]) - 1), ]
      )
      
      updated_lc_data(updated_data)
      
      for (i in 1:nrow(updated_data)) {
        lcHerValues[[paste0("qlc", i)]] <- input[[paste0("qlc", i)]]
        lcHerValues[[paste0("plc", i)]] <- input[[paste0("plc", i)]]
      }
    } else {
      showModal(
        modalDialog(
          title = "Too few rows!",
          footer = modalButton("Dismiss"),
          h4("You cannot have less than one row for this category. If you do not want it included in the total, simply enter zeroes for the values."),
          easyClose = T
        )
      )
    }
  })
  
  # Fixed costs
  output$fix_input <- renderUI({
    fix_data <- fix_data()
    create_fix_ui(fix_data, "f")
  })
  
  # Reactive dataframe ----
  reactive_data <- reactive({
    # Create new dataframe from excel dataframe
    rdata <- receipt_data()
    rdata <- rdata %>% 
      # filter(Unit != "HEADER") %>% 
      select(Item, Quantity, Unit, Price, Total)
    vdata <- updated_var_data()
    vdata <- vdata %>% 
      # filter(Unit != "HEADER") %>% 
      select(Item, Quantity, Unit, Price, Total)
    ferdata <- updated_fer_data()
    ferdata <- ferdata %>%
      select(Item, Quantity, Unit, Price, Total)
    herdata <- updated_her_data()
    herdata <- herdata %>%
      select(Item, Quantity, Unit, Price, Total)
    insdata <- updated_ins_data()
    insdata <- insdata %>%
      select(Item, Quantity, Unit, Price, Total)
    fundata <- updated_fun_data()
    fundata <- fundata %>%
      select(Item, Quantity, Unit, Price, Total)
    fcdata <- updated_fc_data()
    fcdata <- fcdata %>% 
      # filter(Unit != "HEADER") %>% 
      select(Item, Quantity, Unit, Price, Total)
    icdata <- updated_ic_data()
    icdata <- icdata %>% 
      # filter(Unit != "HEADER") %>% 
      select(Item, Quantity, Unit, Price, Total)
    lcdata <- updated_lc_data()
    lcdata <- lcdata %>% 
      # filter(Unit != "HEADER") %>% 
      select(Item, Quantity, Unit, Price, Total)
    fdata <- fix_data()
    fdata <- fdata %>% 
      # filter(Unit != "HEADER") %>% 
      select(Item, Quantity, Unit, Price, Total)
    # Update Quantity and Price with user inputs dynamically
    ## Receipts
    for (i in 1:nrow(rdata)) { # For every row in rdata
      q_input <- input[[paste0("qr", i)]] # Replace quantity with entered value
      p_input <- input[[paste0("pr", i)]] # Replace price with entered value
      if (!is.null(q_input) && !is.null(p_input)) { # If they are null
        rdata[i, "Quantity"] <- q_input # Replace null value in df
        rdata[i, "Price"] <- p_input # Replace null value in df
      }
    }
    # Recalculate Total column
    rdata$Total <- rdata$Price * rdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    rtotal <- colSums(rdata[, "Total"], na.rm = TRUE)
    rtotal <- rtotal * input$acres
    # Combine title row, dataframe, and total row
    rtitle_row <- c("Receipts", "", "", "", "")
    acre_row <- c("Total Area", input$acres, paste0(measure(), if (!is.null(measure()) && measure() %in% add_s) {"(s)"}), "", "")
    rtotal_row <- c("Receipt Total", "", "", "", rtotal) # Define total_row
    rdf <- rbind(rtitle_row, rdata, acre_row, rtotal_row)
    
    ## Fertilizer costs
    for (i in 0:nrow(ferdata)) {
      q_input <- input[[paste0("qfer", i)]]
      p_input <- input[[paste0("pfer", i)]]
      
      # More robust input checking
      is_valid_q_input <- !is.null(q_input) && 
        q_input != "" && 
        !is.na(q_input) && 
        tryCatch({
          as.numeric(q_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      is_valid_p_input <- !is.null(p_input) && 
        p_input != "" && 
        !is.na(p_input) && 
        tryCatch({
          as.numeric(p_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      # Only update if both inputs are valid
      if (is_valid_q_input && is_valid_p_input) {
        ferdata[i, "Quantity"] <- as.numeric(q_input)
        ferdata[i, "Price"] <- as.numeric(p_input)
      }
    }
    
    #print(ferdata, n=nrow(ferdata))
    
    # Recalculate Total column
    ferdata$Total <- ferdata$Price * ferdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    fertotal <- colSums(ferdata[, "Total"], na.rm = TRUE)
    
    ## Herbicide costs
    for (i in 0:nrow(herdata)) {
      q_input <- input[[paste0("qher", i)]]
      p_input <- input[[paste0("pher", i)]]
      
      # More robust input checking
      is_valid_q_input <- !is.null(q_input) && 
        q_input != "" && 
        !is.na(q_input) && 
        tryCatch({
          as.numeric(q_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      is_valid_p_input <- !is.null(p_input) && 
        p_input != "" && 
        !is.na(p_input) && 
        tryCatch({
          as.numeric(p_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      # Only update if both inputs are valid
      if (is_valid_q_input && is_valid_p_input) {
        herdata[i, "Quantity"] <- as.numeric(q_input)
        herdata[i, "Price"] <- as.numeric(p_input)
      }
    }
    
    #print(herdata, n=nrow(herdata))
    
    # Recalculate Total column
    herdata$Total <- herdata$Price * herdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    hertotal <- colSums(herdata[, "Total"], na.rm = TRUE)
    
    ## Insecticide costs
    for (i in 0:nrow(insdata)) {
      q_input <- input[[paste0("qins", i)]]
      p_input <- input[[paste0("pins", i)]]
      
      # More robust input checking
      is_valid_q_input <- !is.null(q_input) && 
        q_input != "" && 
        !is.na(q_input) && 
        tryCatch({
          as.numeric(q_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      is_valid_p_input <- !is.null(p_input) && 
        p_input != "" && 
        !is.na(p_input) && 
        tryCatch({
          as.numeric(p_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      # Only update if both inputs are valid
      if (is_valid_q_input && is_valid_p_input) {
        insdata[i, "Quantity"] <- as.numeric(q_input)
        insdata[i, "Price"] <- as.numeric(p_input)
      }
    }
    
    #print(insdata, n=nrow(insdata))
    
    # Recalculate Total column
    insdata$Total <- insdata$Price * insdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    instotal <- colSums(insdata[, "Total"], na.rm = TRUE)
    
    ## Fungicide costs
    for (i in 0:nrow(fundata)) {
      q_input <- input[[paste0("qfun", i)]]
      p_input <- input[[paste0("pfun", i)]]
      
      # More robust input checking
      is_valid_q_input <- !is.null(q_input) && 
        q_input != "" && 
        !is.na(q_input) && 
        tryCatch({
          as.numeric(q_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      is_valid_p_input <- !is.null(p_input) && 
        p_input != "" && 
        !is.na(p_input) && 
        tryCatch({
          as.numeric(p_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      # Only update if both inputs are valid
      if (is_valid_q_input && is_valid_p_input) {
        fundata[i, "Quantity"] <- as.numeric(q_input)
        fundata[i, "Price"] <- as.numeric(p_input)
      }
    }
    
    #print(fundata, n=nrow(fundata))
    
    # Recalculate Total column
    fundata$Total <- fundata$Price * fundata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    funtotal <- colSums(fundata[, "Total"], na.rm = TRUE)
    
    ## Variable costs
    for (i in 0:nrow(vdata)) {
      q_input <- input[[paste0("qv", i)]]
      p_input <- input[[paste0("pv", i)]]
      
      # More robust input checking
      is_valid_q_input <- !is.null(q_input) && 
        q_input != "" && 
        !is.na(q_input) && 
        tryCatch({
          as.numeric(q_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      is_valid_p_input <- !is.null(p_input) && 
        p_input != "" && 
        !is.na(p_input) && 
        tryCatch({
          as.numeric(p_input)
          TRUE
        }, warning = function(w) FALSE, error = function(e) FALSE)
      
      # Only update if both inputs are valid
      if (is_valid_q_input && is_valid_p_input) {
        vdata[i, "Quantity"] <- as.numeric(q_input)
        vdata[i, "Price"] <- as.numeric(p_input)
      }
    }
    
    #print(vdata, n=nrow(vdata))
    
    # Recalculate Total column with robust handling
    vdata$Total <- ifelse(is.na(vdata$Price) | is.na(vdata$Quantity) | 
                            vdata$Price == "" | vdata$Quantity == "", 
                          0, 
                          as.numeric(vdata$Price) * as.numeric(vdata$Quantity))
    
    # Calculate the Total row with additional safeguards
    vtotal <- tryCatch({
      # Sum only numeric values, ignoring empty or NA rows
      total_sum <- sum(as.numeric(vdata$Total[!is.na(vdata$Total) & vdata$Total != ""]), na.rm = TRUE)
      
      # Add other totals with error handling
      total_sum + 
        sum(as.numeric(fertotal), na.rm = TRUE) +
        sum(as.numeric(instotal), na.rm = TRUE) +
        sum(as.numeric(funtotal), na.rm = TRUE)
    }, error = function(e) {
      warning("Error calculating variable costs total: ", e$message)
      0  # Return 0 if calculation fails
    }) * input$acres
    
    # Combine dataframe and total row with additional checks
    vtitle_row <- c("Variable Costs", "", "", "", "")
    vtotal_row <- c("Total Variable Costs", "", "", "", vtotal)
    
    # Use rbind with check for existence of each component
    vdf <- do.call(rbind, list(
      vtitle_row, 
      if(exists("ferdata")) ferdata else NULL,
      if(exists("insdata")) insdata else NULL,
      if(exists("fundata")) fundata else NULL,
      vdata, 
      acre_row, 
      vtotal_row
    ))
    
    #print(vdf, n=nrow(vdf))

    ## Fallow cover crop costs
    for (i in 1:nrow(fcdata)) { # For every row in vdata
      q_input <- input[[paste0("qfc", i)]] # Replace quantity with entered value
      p_input <- input[[paste0("pfc", i)]] # Replace price with entered value
      if (!is.null(q_input) && !is.null(p_input)) { # If they are null
        fcdata[i, "Quantity"] <- q_input # Replace null value in df
        fcdata[i, "Price"] <- p_input # Replace null value in df
      }
    }
    # Recalculate Total column
    fcdata$Total <- fcdata$Price * fcdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    fctotal <- colSums(fcdata[, "Total"], na.rm = TRUE)
    fctotal <- fctotal * input$acres
    # Combine dataframe and total row
    fctitle_row <- c("Variable Costs Influenced by Cover Crops", "", "", "", "")
    fctotal_row <- c("Total Cover Crop Related Variable Costs", "", "", "", fctotal) # Define total_row
    fcdf <- rbind(fctitle_row, fcdata, acre_row, fctotal_row)

    ## Interseeding cover crop costs
    for (i in 1:nrow(icdata)) { # For every row in vdata
      q_input <- input[[paste0("qic", i)]] # Replace quantity with entered value
      p_input <- input[[paste0("pic", i)]] # Replace price with entered value
      if (!is.null(q_input) && !is.null(p_input)) { # If they are null
        icdata[i, "Quantity"] <- q_input # Replace null value in df
        icdata[i, "Price"] <- p_input # Replace null value in df
      }
    }
    # Recalculate Total column
    icdata$Total <- icdata$Price * icdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    ictotal <- colSums(icdata[, "Total"], na.rm = TRUE)
    ictotal <- ictotal * input$acres
    # Combine dataframe and total row
    ictitle_row <- c("Variable Costs Influenced by Cover Crops", "", "", "", "")
    ictotal_row <- c("Total Cover Crop Related Variable Costs", "", "", "", ictotal) # Define total_row
    icdf <- rbind(ictitle_row, icdata, acre_row, ictotal_row)
    
    ## Late Season cover crop costs
    for (i in 1:nrow(lcdata)) { # For every row in vdata
      q_input <- input[[paste0("qlc", i)]] # Replace quantity with entered value
      p_input <- input[[paste0("plc", i)]] # Replace price with entered value
      if (!is.null(q_input) && !is.null(p_input)) { # If they are null
        lcdata[i, "Quantity"] <- q_input # Replace null value in df
        lcdata[i, "Price"] <- p_input # Replace null value in df
      }
    }
    # Recalculate Total column
    lcdata$Total <- lcdata$Price * lcdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    lctotal <- colSums(lcdata[, "Total"], na.rm = TRUE)
    lctotal <- lctotal * input$acres
    # Combine dataframe and total row
    lctitle_row <- c("Variable Costs Influenced by Cover Crops", "", "", "", "")
    lctotal_row <- c("Total Cover Crop Related Variable Costs", "", "", "", lctotal) # Define total_row
    lcdf <- rbind(lctitle_row, lcdata, acre_row, lctotal_row)

    ## Fixed costs
    for (i in 1:nrow(fdata)) { # For every row in fdata
      q_input <- input[[paste0("qf", i)]] # Replace quantity with entered value
      p_input <- input[[paste0("pf", i)]] # Replace price with entered value
      if (!is.null(q_input) && !is.null(p_input)) { # If they are null
        fdata[i, "Quantity"] <- q_input # Replace null value in df
        fdata[i, "Price"] <- p_input # Replace null value in df
      }
    }
    # Recalculate Total column
    fdata$Total <- fdata$Price * fdata$Quantity
    # Calculate the Total row as the sum of all totals of the rows above
    ftotal <- colSums(fdata[, "Total"], na.rm = TRUE)
    # Multiply percentage borne by operation
    ftotal <- ftotal["Total"] * (input$percent_of_operation / 100)
    # Combine dataframe and total row
    ftitle_row <- c("Fixed Costs", "", "", "", "")
    ftotal_row <- c("Total Fixed Costs", "", "", "", ftotal) # Define total_row
    fdf <- rbind(ftitle_row, fdata, ftotal_row)
    
    # Calculate total costs
    totalCosts <- (vtotal + ftotal)
    totalCosts_row <- c("Total Costs", "", "", "", totalCosts)
    totalCostsFC <- (vtotal + fctotal + ftotal)
    totalCostsFC_row <- c("Total Costs", "", "", "", totalCostsFC)
    totalCostsIC <- (vtotal + ictotal + ftotal)
    totalCostsIC_row <- c("Total Costs", "", "", "", totalCostsIC)
    totalCostsLC <- (vtotal + lctotal + ftotal)
    totalCostsLC_row <- c("Total Costs", "", "", "", totalCostsLC)
    
    # Net returns
    nrtitle_row <- c("Returns", "", "", "", "")
    # Calculate net returns over variable costs
    netReturnsVar <- c("Net Returns Over Variable Costs", "", "", "", (rtotal - vtotal))
    netReturnsVarFC <- c("Net Returns Over Variable Costs", "", "", "", (rtotal - vtotal - fctotal))
    netReturnsVarIC <- c("Net Returns Over Variable Costs", "", "", "", (rtotal - vtotal - ictotal))
    netReturnsVarLC <- c("Net Returns Over Variable Costs", "", "", "", (rtotal - vtotal - lctotal))
    # Calculate net returns
    netReturns <- c("Net Returns", "", "", "", (rtotal - totalCosts))
    netReturnsFC <- c("Net Returns", "", "", "", (rtotal - totalCostsFC))
    netReturnsIC <- c("Net Returns", "", "", "", (rtotal - totalCostsIC))
    netReturnsLC <- c("Net Returns", "", "", "", (rtotal - totalCostsLC))
    
    # Combine all 
    enterprise <- rbind(rdf, vdf, fdf, totalCosts_row, nrtitle_row, netReturnsVar, netReturns)
    enterprise <- enterprise %>%
      mutate(Unit = case_when(Unit %in% add_s ~ paste0(Unit, {"s"}),
                              TRUE ~ Unit))
    
    fallowEnterprise <- rbind(rdf, vdf, fcdf, fdf, totalCostsFC_row, nrtitle_row, netReturnsVarFC, netReturnsFC)
    fallowEnterprise <- fallowEnterprise %>%
      mutate(Unit = case_when(Unit %in% add_s ~ paste0(Unit, {"s"}),
                              TRUE ~ Unit))
    #print(fallowEnterprise)
    
    interseedingEnterprise <- rbind(rdf, vdf, icdf, fdf, totalCostsIC_row, nrtitle_row, netReturnsVarIC, netReturnsIC)
    interseedingEnterprise <- interseedingEnterprise %>%
      mutate(Unit = case_when(Unit %in% add_s ~ paste0(Unit, {"s"}),
                              TRUE ~ Unit))
    
    lateSeasonEnterprise <- rbind(rdf, vdf, lcdf, fdf, totalCostsLC_row, nrtitle_row, netReturnsVarLC, netReturnsLC)
    lateSeasonEnterprise <- lateSeasonEnterprise %>%
      mutate(Unit = case_when(Unit %in% add_s ~ paste0(Unit, {"s"}),
                              TRUE ~ Unit))
    
    directComparisonFallow <- rbind(fctotal_row, netReturnsVarFC, netReturnsFC)
    directComparisonFallow <- as.data.frame(directComparisonFallow) %>%
      select(, c(1,5))
    directComparisonInterseeding <- rbind(ictotal_row, netReturnsVarIC, netReturnsIC)
    directComparisonInterseeding <- as.data.frame(directComparisonInterseeding) %>%
      select(, 5)
    directComparisonLateSeason <- rbind(lctotal_row, netReturnsVarLC, netReturnsLC)
    directComparisonLateSeason <- as.data.frame(directComparisonLateSeason) %>%
      select(, 5)
    directComparison <- cbind(directComparisonFallow, directComparisonInterseeding, directComparisonLateSeason)
    colnames(directComparison) <- c("Variable", "No Cover Crop", "Interseeding Cover Crop", "Fall Cover Crop")
    #print(directComparisonFallow)
    #print(directComparisonInterseeding)
    #print(directComparisonLateSeason)
    #print(directComparison)
    
    return(list(without_cc = enterprise, with_fc = fallowEnterprise, with_ic = interseedingEnterprise, with_lc = lateSeasonEnterprise, direct_comparison = directComparison))
  })
  
  # Tables ----
  # Construct regular table from dataframe
  output$df_table <- renderTable({
    data <- reactive_data()
    tableData <- data$without_cc
    
    tableData <- tableData %>%
      mutate(Quantity = case_when(Unit == "HEADER" ~ "",
                                  TRUE ~ Quantity), 
             Price = case_when(Unit == "HEADER" ~ "", 
                               Price != "" ~ paste0("$ ", format(Price, nsmall = 2)),
                               Price == "" ~ Price),
             Total = case_when(Unit == "HEADER" ~ "", 
                               Total != "" ~ paste0("$ ", format(Total, nsmall = 2)),
                               Total == "" ~ Total),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total != "", paste0("<b>", ., "</b>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total == "", paste0("<b><u>", ., "</b></u>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "HEADER" & Price == "" & Total == "", paste0("<b><i>", ., "</b></i>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "HEADER" & Price == "" & Total == "", paste0("<b><i>", ., "</b></i>"), .)),
             Unit = case_when(Unit == "<b><i>HEADER</b></i>" ~ "<span style ='visibility: hidden;'>hidden value</span>",
                              TRUE ~ as.character(Unit)))
    
    ##print(tableData, n=nrow(tableData))
    
    tableData
  }, sanitize.text.function = function(x) x)
  
  # Construct cover crop tables
  ## Fallow
  output$fallow_table <- renderTable({
    data <- reactive_data()
    tableData <- data$with_fc
    
    tableData <- tableData %>%
      mutate(Quantity = case_when(Unit == "HEADER" ~ "",
                                  TRUE ~ Quantity), 
             Price = case_when(Unit == "HEADER" ~ "", 
                               Price != "" ~ paste0("$ ", format(Price, nsmall = 2)),
                               Price == "" ~ Price),
             Total = case_when(Unit == "HEADER" ~ "", 
                               Total != "" ~ paste0("$ ", format(Total, nsmall = 2)),
                               Total == "" ~ Total),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total != "", paste0("<b>", ., "</b>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total == "", paste0("<b><u>", ., "</b></u>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "HEADER" & Price == "" & Total == "", paste0("<b><i>", ., "</b></i>"), .)),
             Unit = case_when(Unit == "<b><i>HEADER</b></i>" ~ "<span style ='visibility: hidden;'>hidden value</span>",
                              TRUE ~ as.character(Unit)))
    
    tableData
  }, sanitize.text.function = function(x) x)
  
  ## Interseeding
  output$interseeding_table <- renderTable({
    data <- reactive_data()
    tableData <- data$with_ic
    
    tableData <- tableData %>%
      mutate(Quantity = case_when(Unit == "HEADER" ~ "",
                                  TRUE ~ Quantity), 
             Price = case_when(Unit == "HEADER" ~ "", 
                               Price != "" ~ paste0("$ ", format(Price, nsmall = 2)),
                               Price == "" ~ Price),
             Total = case_when(Unit == "HEADER" ~ "", 
                               Total != "" ~ paste0("$ ", format(Total, nsmall = 2)),
                               Total == "" ~ Total),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total != "", paste0("<b>", ., "</b>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total == "", paste0("<b><u>", ., "</b></u>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "HEADER" & Price == "" & Total == "", paste0("<b><i>", ., "</b></i>"), .)),
             Unit = case_when(Unit == "<b><i>HEADER</b></i>" ~ "<span style ='visibility: hidden;'>hidden value</span>",
                              TRUE ~ as.character(Unit)))
    
    tableData
  }, sanitize.text.function = function(x) x)
  
  ## Late Season
  output$lateSeason_table <- renderTable({
    data <- reactive_data()
    tableData <- data$with_lc
    
    tableData <- tableData %>%
      mutate(Quantity = case_when(Unit == "HEADER" ~ "",
                                  TRUE ~ Quantity), 
             Price = case_when(Unit == "HEADER" ~ "", 
                               Price != "" ~ paste0("$ ", format(Price, nsmall = 2)),
                               Price == "" ~ Price),
             Total = case_when(Unit == "HEADER" ~ "", 
                               Total != "" ~ paste0("$ ", format(Total, nsmall = 2)),
                               Total == "" ~ Total),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total != "", paste0("<b>", ., "</b>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "" & Price == "" & Total == "", paste0("<b><u>", ., "</b></u>"), .)),
             across(everything(), ~ ifelse(Quantity == "" & Unit == "HEADER" & Price == "" & Total == "", paste0("<b><i>", ., "</b></i>"), .)),
             Unit = case_when(Unit == "<b><i>HEADER</b></i>" ~ "<span style ='visibility: hidden;'>hidden value</span>",
                              TRUE ~ as.character(Unit)))
    
    tableData
  }, sanitize.text.function = function(x) x)
  
  ## Direct Comparison
  output$directComparison_table <- renderTable({
    data <- reactive_data()
    tableData <- data$direct_comparison
    
    tableData <- tableData %>%
      mutate(across(everything(), ~ ifelse(is.numeric(.) | !is.na(as.numeric(.)), 
                                           paste0("$ ", format(as.numeric(.), nsmall = 2)), 
                                           .)))
    
    tableData
  }, sanitize.text.function = function(x) x)
  
  direct_comparison_report <- reactive({
    data <- reactive_data()
    tableData <- data$direct_comparison
    
    tableData <- tableData %>%
      mutate(across(everything(), ~ ifelse(is.numeric(.) | !is.na(as.numeric(.)), 
                                           paste0("$ ", format(as.numeric(.), nsmall = 2)), 
                                           .))) 
    rownames(tableData) <- c(tableData$Variable[1], tableData$Variable[2], tableData$Variable[3])
    
    tableData <- tableData %>%
      select(`No Cover Crop`, `Interseeding Cover Crop`, `Fall Cover Crop`)
    tableData
  })
  
  output$directComparison_plot1 <- renderPlot({
    data <- reactive_data()
    plotData <- data$direct_comparison
    
    plotData <- plotData %>%
      pivot_longer(cols = -Variable, names_to = "CoverCrop", values_to = "Value")
    
    plotData <- plotData %>%
      mutate(Value = as.numeric(as.character(Value)))
    
    #print(plotData)
    
    # Calculate the clean maximum value
    max_value <- ceiling(max(plotData$Value[plotData$Variable %in% c("Net Returns", "Net Returns Over Variable Costs")]) / 1000) * 1000
    min_value <- floor(min(plotData$Value[plotData$Variable %in% c("Net Returns", "Net Returns Over Variable Costs")]) / 1000) * 1000
    
    stackedPlot <- ggplot(plotData %>% filter(Variable %in% c("Net Returns", "Net Returns Over Variable Costs")), 
                          aes(x = CoverCrop, y = Value, fill = Variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Net Returns", y = "Value", x = "") +
      theme(panel.background = element_blank(),
            axis.line = element_line(color = "black", linewidth = 1),
            axis.text = element_text(size = 16),
            title = element_text(size = 20),
            text = element_text(family = "Helvetica", face = "bold"),
            plot.title = element_text(hjust=0.5),
            legend.position = "top",
            legend.text = element_text(size = 16),
            legend.title = element_blank()) +
      scale_fill_manual(values = c("Net Returns" = "green", 
                                   "Net Returns Over Variable Costs" = "blue")) +
      scale_y_continuous(limits = c(min(min_value, 0), max_value),
                         labels = scales::dollar_format())
    
    stackedPlot
  })
  
  output$directComparison_plot2 <- renderPlot({
    data <- reactive_data()
    plotData <- data$direct_comparison
    
    plotData <- plotData %>%
      pivot_longer(cols = -Variable, names_to = "CoverCrop", values_to = "Value")
    
    plotData <- plotData %>%
      mutate(Value = as.numeric(as.character(Value)))
    
    #print(plotData)

    max_cost_value <- ceiling(max(plotData$Value[plotData$Variable == "Total Cover Crop Related Variable Costs"]) / 100) * 100
    
    ggplot(plotData %>% filter(Variable == "Total Cover Crop Related Variable Costs"), 
           aes(x = CoverCrop, y = Value)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(title = "Total Cover Crop Related Variable Costs", y = "Costs", x = "") +
      theme(panel.background = element_blank(),
            axis.line = element_line(color = "black", linewidth = 1),
            axis.text = element_text(size = 16),
            title = element_text(size = 20),
            text = element_text(family = "Helvetica", face = "bold"),
            plot.title = element_text(hjust=0.5)) +
      scale_y_continuous(limits = c(0, max_cost_value),
                         labels = scales::dollar_format())
    
  })
  
  
  # Cover crop benefits ----
  # Fallow benefits referenced with fallow enterprise budget
  # Create object
  output$fallowBenefits <- renderUI({
    fallow_benefits_data()
  })
  
  # Late season benefits referenced with late season enterprise budget
  # Create object
  output$lateSeasonBenefits <- renderUI({
    lateSeason_benefits_data()
  })
  
  # Interseeding benefits referenced with interseeding enterprise budget
  # Create object
  output$interseedingBenefits <- renderUI({
    interseeding_benefits_data()
  })

  # Cover crop user interface ----
  ## Fallow
  output$fallow_benefits <- renderUI({
    fluidRow(
      box(
        width = 12,
        status = 'primary',
        # title = "No Cover Cropping",
        headerBorder = F,
        htmlOutput("fallow_directions"),
        headerPanel(""),
        column(
          width = 8,
          align = "center",
          h1("Enterprise Budget Table"),
          div(
            class = "table-container",
            tableOutput("fallow_table")
          )
        ),
        column(
          width = 4,
          h1("Benefits"),
          headerPanel(""),
          HTML('<center><img src="noCoverCrop.jpg" width = "100%" ></center>'),
          headerPanel(""),
          htmlOutput("fallowBenefits")
        )
      )
    )
  })
  
  ## Late season
  output$lateSeason_benefits <- renderUI({
    fluidRow(
      box(
        width = 12,
        status = 'primary',
        # title = "Fall Cover Cropping",
        headerBorder = F,
        htmlOutput("lateSeason_directions"),
        headerPanel(""),
        column(
          width = 8,
          align = "center",
          h1("Enterprise Budget Table"),
          div(
            class = "table-container",
            tableOutput("lateSeason_table")
          )
        ),
        column(
          width = 4,
          h1("Benefits"),
          headerPanel(""),
          HTML('<center><img src="lateSeason.jpeg" width = "100%" ></center>'),
          headerPanel(""),
          htmlOutput("lateSeasonBenefits")
        )
      )
    )
  })
  
  ## Interseeding
  output$interseeding_benefits <- renderUI({
    fluidRow(
      box(
        width = 12,
        status = 'primary',
        # title = "Interseeding Cover Cropping",
        headerBorder = F,
        htmlOutput("interseeding_directions"),
        headerPanel(""),
        column(
          width = 8,
          align = "center",
          h1("Enterprise Budget Table"),
          div(
            class = "table-container",
            tableOutput("interseeding_table")
          )
        ),
        column(
          width = 4,
          h1("Benefits"),
          headerPanel(""),
          HTML('<center><img src="interseeding.jpg" width = "100%" ></center>'),
          headerPanel(""),
          htmlOutput("interseedingBenefits")
        )
      )
    )
  })
  
  ## Direct Comparison
  output$direct_comparison <- renderUI({
    fluidRow(
      box(
        width = 12,
        status = 'primary',
        headerBorder = F,
        fluidRow(
          column(
            width = 12,
            align = 'center',
            h1("Direct Comparison of Cover Crop Strategies"),
            div(
              class = "table-container",
              tableOutput("directComparison_table")
            )
          )
        ),
        headerPanel(""),
        headerPanel(""),
        fluidRow(
          column(
            width = 6, 
            align = 'center',
            div(
              class = "plot-scrollable",
              plotOutput("directComparison_plot1", width = "600px", height = "400px")
            )
          ),
          column(
            width = 6,
            align = 'center',
            div(
              class = "plot-scrollable",
              plotOutput("directComparison_plot2", width = "600px", height = "400px")
            )
          )
        )
      )
    )
  })
  
  # Sensitivity Analysis ----
  ## Define data
  sensitivityData <- reactive({
    data <- reactive_data()
    workingData <- data$without_cc
    
    if (!is.null(input$inputData) && input$inputData == "With no cover crop costs") {
      workingData <- data$with_fc
    } else
      if (!is.null(input$inputData) && input$inputData == "With interseeding cover crop costs") {
        workingData <- data$with_ic
      } else
        if (!is.null(input$inputData) && input$inputData == "With late season cover crop costs") {
          workingData <- data$with_lc
        }
    
    sensitivityData <- workingData %>%
      filter(!Quantity == "" & !Price == "")
    
    # Print the filtered data
    #print("Filtered Sensitivity Data:")
    #print(sensitivityData)
    
    return(sensitivityData)
  })
  
  itemData <- reactive({
    workingData <- sensitivityData()
    workingData <- workingData %>%
      filter(!Quantity == "" & !Price == "",
             !Unit == "HEADER")
    
    itemData <- workingData$Item
    
    # Print the item data
    #print("Item Data:")
    #print(itemData)

    return(itemData)
  })
  
  receipt_list <- reactive({
    workingData <- receipt_data()
    
    # rdata <- receipt_data()
    # rdata <- rdata %>% 
    #   # filter(Unit != "HEADER") %>% 
    #   select(Item, Quantity, Unit, Price, Total)
    
    workingData <- workingData %>%
      filter(!Quantity == "" & !Price == "")
    
    receipt_list <- workingData$Item
    
    #print("Receipt List:")
    #print(receipt_list)
    
    return(receipt_list)
  })
  
  ## Breakeven ----
  breakevenReactive <- reactive({
    data <- reactive_data()
    rdata <- receipt_data()
    
    workingData <- data$without_cc
    #print(workingData)
    
    if (!is.null(input$inputData) && input$inputData == "With no cover crop costs") {
      workingData <- data$with_fc
    } else
      if (!is.null(input$inputData) && input$inputData == "With interseeding cover crop costs") {
        workingData <- data$with_ic
      } else
        if (!is.null(input$inputData) && input$inputData == "With late season cover crop costs") {
          workingData <- data$with_lc
        }
    
    bevar <- input$bevar
    #print(bevar)
    beporq <- input$beporq
    #print(beporq)
    
    bevar_value <- as.numeric(workingData[workingData$Item == bevar, beporq])
    #print(bevar_value)
    bevar_unit <- workingData[workingData$Item == bevar, "Unit"]
    #print(bevar_unit)
    alt_col <- ifelse(beporq == "Price", "Quantity", "Price")
    #print(alt_col)
    bevar_alt <- as.numeric(workingData[workingData$Item == bevar, alt_col])
    #print(bevar_alt)
    bevar_total <- as.numeric(workingData[workingData$Item == bevar, "Total"])
    #print(bevar_total)
    
    quantity <- as.numeric(workingData[workingData$Item == bevar, "Quantity"])
    #print(quantity)
    price <- as.numeric(workingData[workingData$Item == bevar, "Price"])
    #print(price)
    
    yvar <- as.numeric(workingData[nrow(workingData), "Total"])
    #print(yvar)
    
    costs <- as.numeric(workingData[workingData$Item == "Total Costs", "Total"])
    #print(costs)
    fix_costs <- as.numeric(workingData[workingData$Item == "Total Fixed Costs", "Total"])
    #print(fix_costs)
    var_costs <- as.numeric(costs - fix_costs)
    #print(var_costs)
    vcpu <- (var_costs / quantity)
    #print("vcpu:")
    #print(vcpu)
    
    beq <- (fix_costs / (price - vcpu))
    #print(beq)
    beq <- round(beq, 1)
    #print("beq rounded:")
    #print(beq)
    
    bep <- ((fix_costs / quantity) + vcpu)
    #print(bep)
    bep <- round(bep, 2)
    #print(bep)
    
    breakevenReactive <- paste0(
      "The ", bevar, " breakeven quantity is ", beq, " ", bevar_unit, " per ", measure(), " at the given price of $", price, " per ", bevar_unit, "."
    )
    
    if (beporq == "Price") {
      breakevenReactive <- paste0(
        "The ", bevar, " breakeven price per ", str_sub(bevar_unit, end = -2), " is $", bep, " at the given quantity of ", quantity, " ", bevar_unit, " per ", measure(), "."
      )
    }
    
    if (fix_costs == 0) {
      breakevenReactive <- paste0(
        "For breakeven to work, fixed costs must be greater than zero. Please correct these values and return to this page."
      )
    }
    
    if (price == 0 | quantity == 0) {
      breakevenReactive <- paste0(
        "Please select an sales outlet source that has non-zero price and quantity."
      )
    }
    
    return(breakevenReactive)
  })
  
  output$breakeven_text <- renderText({
    breakevenReactive()
  })
  
  ## Line plot ----
  linePlotData <- reactive({
    data <- reactive_data()
    
    rdata <- receipt_data()
    
    # Ensure the reactive data is not NULL
    if (is.null(data)) {
      validate(need(FALSE, "Reactive data is NULL"))
      return(NULL)
    }
    
    workingData <- data$without_cc
    
    if (!is.null(input$inputData) && input$inputData == "With no cover crop costs") {
      workingData <- data$with_fc
    } else
      if (!is.null(input$inputData) && input$inputData == "With interseeding cover crop costs") {
        workingData <- data$with_ic
      } else
        if (!is.null(input$inputData) && input$inputData == "With late season cover crop costs") {
          workingData <- data$with_lc
        }
    
    #print("Working Data:")
    #print(workingData, n=nrow(workingData))
    
    xvar <- input$xvar
    porq <- input$porq
    
    # Debugging prints for inputs
    #print(paste("input$xvar:", xvar))
    #print(paste("input$porq:", porq))
    
    # Check if porq and xvar are valid
    if (is.null(porq)) {
      validate(need(FALSE, "Input 'porq' is NULL"))
      return(NULL)
    }
    
    if (is.null(xvar)) {
      validate(need(FALSE, "Input 'xvar' is NULL"))
      return(NULL)
    }
    
    # Extract the values and convert them to numeric
    xvar_value <- as.numeric(workingData[workingData$Item == xvar, porq])
    alt_col <- ifelse(porq == "Price", "Quantity", "Price")
    xvar_alt <- as.numeric(workingData[workingData$Item == xvar, alt_col])
    xvar_total <- as.numeric(workingData[workingData$Item == xvar, "Total"])
    
    # Print statements for debugging
    #print(paste("xvar:", xvar))
    #print(paste("xvar_value:", xvar_value))
    #print(paste("xvar_alt:", xvar_alt))
    #print(paste("xvar_total:", xvar_total))
    
    # Ensure there are no NA values
    if (is.na(xvar_value) || is.na(xvar_alt) || is.na(xvar_total)) {
      validate(need(FALSE, "One of the numeric values is NA"))
      return(NULL)
    }
    
    yvar <- as.numeric(workingData[nrow(workingData), "Total"])
    #print(paste("yvar:", yvar))
    
    # Ensure yvar is not NA
    if (is.na(yvar)) {
      validate(need(FALSE, "yvar is NA"))
      return(NULL)
    }
    
    # Create the dataframe with numeric calculations
    # Create the initial y values based on your formula
    y_values <- c(
      (yvar + xvar_total - ((xvar_value * 0.8) * xvar_alt)),
      (yvar + xvar_total - ((xvar_value * 0.9) * xvar_alt)),
      (yvar + xvar_total - ((xvar_value * 1.0) * xvar_alt)),
      (yvar + xvar_total - ((xvar_value * 1.1) * xvar_alt)),
      (yvar + xvar_total - ((xvar_value * 1.2) * xvar_alt))
    )
    
    # Check if xvar is "Cabbage" and flip the signs if true
    if (any(xvar == rdata[,"Item"])) {
      y_values <- c(
        (yvar - xvar_total + ((xvar_value * 0.8) * xvar_alt)),
        (yvar - xvar_total + ((xvar_value * 0.9) * xvar_alt)),
        (yvar - xvar_total + ((xvar_value * 1.0) * xvar_alt)),
        (yvar - xvar_total + ((xvar_value * 1.1) * xvar_alt)),
        (yvar - xvar_total + ((xvar_value * 1.2) * xvar_alt))
      )
    }
    
    # Create the data frame with the updated y values
    plotData <- data.frame(
      x = c(xvar_value * 0.8, xvar_value * 0.9, xvar_value, xvar_value * 1.1, xvar_value * 1.2),
      y = y_values
    )
    
    #print("Plot Data:")
    #print(plotData)
    
    return(plotData)
  })
  
  linePlot <- reactive({
    data <- linePlotData()
    porq <- input$porq
    #print("Line Plot Data:")
    #print(data)
    linePlot <- ggplot(data = data, aes(x = x, y = y)) +
      geom_line() +
      ggtitle("Sensitivity Analysis") +
      scale_x_continuous(name = paste0(input$xvar, " ", input$porq),
                         breaks = seq(data[1,1], data[5,1], by=(data[5,1] - data[1,1])/4)) +
      scale_y_continuous(name = "Net Returns",
                         breaks = seq(data[1,2], data[5,2], by=(data[5,2] - data[1,2])/4),
                         labels = scales::dollar_format()) +
      theme(panel.background = element_blank(),
            axis.line = element_line(color = "black", linewidth = 1),
            axis.text = element_text(size = 14),
            title = element_text(size = 20),
            text = element_text(family = "Helvetica", face = "bold"),
            plot.title = element_text(hjust=0.5))
    
    if(porq=="Price") {
      linePlot <- linePlot + 
        scale_x_continuous(name = paste0(input$xvar, " ", input$porq), 
                           breaks = seq(data[1,1], data[5,1], by=(data[5,1] - data[1,1])/4),
                           labels = scales::dollar_format())
    }
    
    return(linePlot)
  })
  
  # Output
  output$linePlot <- renderPlot({
    linePlot()
  })
  

  
  ## Heatmap ----
  tplotlyData <- reactive({
    data <- reactive_data()
    
    rdata <- receipt_data()
    #print("Receipt Data:")
    #print(rdata)
    
    # Ensure the reactive data is not NULL
    if (is.null(data)) {
      validate(need(FALSE, "Reactive data is NULL"))
      return(NULL)
    }
    
    workingData <- data$without_cc
    
    if (!is.null(input$inputData) && input$inputData == "With no cover crop costs") {
      workingData <- data$with_fc
    } else
      if (!is.null(input$inputData) && input$inputData == "With interseeding cover crop costs") {
        workingData <- data$with_ic
      } else
        if (!is.null(input$inputData) && input$inputData == "With late season cover crop costs") {
          workingData <- data$with_lc
        }
    
    #print("Working Data:")
    #print(workingData, n=nrow(workingData))
    
    hvar <- input$hvar
    hporq <- input$hporq
    vvar <- input$vvar
    vporq <- input$vporq
    
    # Debugging prints for inputs
    #print(paste("input$hvar:", hvar))
    #print(paste("input$hporq:", hporq))
    #print(paste("input$vvar:", vvar))
    #print(paste("input$vporq:", vporq))
    
    # Check if hporq, hvar, vporq and vvar are valid
    if (is.null(hporq)) {
      validate(need(FALSE, "Input 'hporq' is NULL"))
      return(NULL)
    }
    
    if (is.null(hvar)) {
      validate(need(FALSE, "Input 'hvar' is NULL"))
      return(NULL)
    }
    if (is.null(vporq)) {
      validate(need(FALSE, "Input 'vporq' is NULL"))
      return(NULL)
    }
    
    if (is.null(vvar)) {
      validate(need(FALSE, "Input 'vvar' is NULL"))
      return(NULL)
    }
    
    # Extract the values and convert them to numeric
    hvar_value <- as.numeric(workingData[workingData$Item == hvar, hporq])
    halt_col <- ifelse(hporq == "Price", "Quantity", "Price")
    hvar_alt <- as.numeric(workingData[workingData$Item == hvar, halt_col])
    hvar_total <- as.numeric(workingData[workingData$Item == hvar, "Total"])
    
    vvar_value <- as.numeric(workingData[workingData$Item == vvar, vporq])
    valt_col <- ifelse(vporq == "Price", "Quantity", "Price")
    vvar_alt <- as.numeric(workingData[workingData$Item == vvar, valt_col])
    vvar_total <- as.numeric(workingData[workingData$Item == vvar, "Total"])
    
    # Print statements for debugging
    #print(paste("hvar:", hvar))
    #print(paste("hvar_value:", hvar_value))
    #print(paste("hvar_alt:", hvar_alt))
    #print(paste("hvar_total:", hvar_total))
    
    #print(paste("vvar:", vvar))
    #print(paste("vvar_value:", vvar_value))
    #print(paste("vvar_alt:", vvar_alt))
    #print(paste("vvar_total:", vvar_total))
    
    # Ensure there are no NA values
    if (is.na(hvar_value) || is.na(hvar_alt) || is.na(hvar_total)) {
      validate(need(FALSE, "One of the numeric values is NA (horizontal)"))
      return(NULL)
    }
    
    if (is.na(vvar_value) || is.na(vvar_alt) || is.na(vvar_total)) {
      validate(need(FALSE, "One of the numeric values is NA (vertical)"))
      return(NULL)
    }
    
    yvar <- as.numeric(workingData[nrow(workingData), "Total"])
    #print(paste("yvar:", yvar))
    
    # Ensure yvar is not NA
    if (is.na(yvar)) {
      validate(need(FALSE, "yvar is NA"))
      return(NULL)
    }
    
    # Create the dataframe with numeric calculations
    receipt_list <- rdata %>%
      filter(!Quantity == "" & !Price == "") %>%
      select(Item)
    
    # workingData <- workingData %>%
    #   filter(!Quantity == "" & !Price == "")
    # 
    # receipt_list <- workingData$Item
    # 
    # #print("Receipt List:")
    # #print(receipt_list)
    # 
    # return(receipt_list)
    # 
    #print("Receipt list:")
    #print(receipt_list)
    #print(paste("hvar in receipt_list:", hvar %in% receipt_list, "vvar in receipt_list:", vvar %in% receipt_list))
    #print(paste("hvar:", hvar, "vvar:", vvar))
    
    plotlyData <- data.frame(matrix(ncol = 5, nrow = 5))
    
    for (row in 1:5) {
      for (col in 1:5) {
        vfactor <- 1.2 - 0.1 * (col-1)
        hfactor <- 0.8 + 0.1 * (row-1)
        plotlyData[row, col] <- yvar + hvar_total - ((hvar_value * hfactor) * hvar_alt) + vvar_total - ((vvar_value * vfactor) * vvar_alt)
        if (any(hvar == receipt_list) & !any(vvar == receipt_list)) {
          plotlyData[row, col] <- yvar - hvar_total + ((hvar_value * hfactor) * hvar_alt) + vvar_total - ((vvar_value * vfactor) * vvar_alt)
        } 
        if (any(vvar == receipt_list) & !any(hvar == receipt_list)) {
          plotlyData[row, col] <- yvar + hvar_total - ((hvar_value * hfactor) * hvar_alt) - vvar_total + ((vvar_value * vfactor) * vvar_alt)
        }
        if (hvar == vvar && any(hvar == receipt_list)) {
          plotlyData[row, col] <- yvar - hvar_total + ((hvar_value * hfactor) * hvar_alt) - vvar_total + ((vvar_value * vfactor) * vvar_alt)
        }
        if (hvar == vvar && hporq != vporq) {
          plotlyData[row, col] <- yvar + hvar_total - ((hvar_value * hfactor) * (vvar_value * vfactor))
          if (any(hvar == receipt_list)) {
            plotlyData[row, col] <- yvar - hvar_total + ((hvar_value * hfactor) * (vvar_value * vfactor))
          }
        }
      }
    }
    
    #print(plotlyData)
    
    tplotlyData <- t(plotlyData)
    #print(tplotlyData)
    
    str(tplotlyData)
    
    colnames(tplotlyData) <- paste0(hvar_value * seq(from=0.8, to=1.2, by=0.1))
    rownames(tplotlyData) <- paste0(vvar_value * seq(from=1.2, to=0.8, by=-0.1))
    
    return(tplotlyData)
  })
  
  heatmapReact <- reactive({
    heatmapReact <- heatmaply(tplotlyData(), Rowv = FALSE, Colv = FALSE,
                              main = "Net Returns Heatmap",
                              xlab = paste0(input$hvar, " ", input$hporq), ylab = paste0(input$vvar, " ", input$vporq),
                              draw_cellnote = T, cellnote_textposition = "middle center")
    return(heatmapReact)
  })
  
  output$heatmaply <- renderPlotly({
    heatmapReact()
  })
  
  
  ## User Interface ----
  output$sensitivity_analysis <- renderUI({
    fluidRow(
      column(
        width = 12,
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        box(
          title = "Breakeven Analysis",
          width = 12,
          fluidRow(
            column(
              width = 12,
              align = 'center',
              htmlOutput("breakeven_directions"),
              fluidRow(
                column(
                  width = 6,
                  selectInput("bevar", "With respect to what sales outlet would you like to see the breakeven point?", choices = receipt_list())
                ),
                column(
                  width = 6,
                  selectInput("beporq", "With respect to what value would you like to see the breakeven point?", choices = c("Price", "Quantity"))
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              textOutput("breakeven_text")
            )
          )
        ),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        box(
          title = "How do net returns change when an input or output changes?",
          width = 12,
          fluidRow(
            column(
              width = 6,
              selectInput("xvar", "What variable would you like to change?", choices = itemData())
            ),
            column(
              width = 6,
              selectInput("porq", "Price or quantity?", choices = c("Price", "Quantity"))
            )
          ),
          headerPanel(""),
          fluidRow(
            column(
              width = 12,
              plotOutput("linePlot")
            )
          )
        ),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        headerPanel(""),
        box(
          title = "How do net returns change when multiple inputs or outputs change?",
          width = 12,
          fluidRow(
            column(
              width = 6,
              selectInput("hvar", "What variable would you like to have on the horizontal axis?", choices = itemData())
            ),
            column(
              width = 6,
              selectInput("hporq", "Price or quantity?", choices = c("Price", "Quantity"))
            )
          ),
          fluidRow(
            column(
              width = 6,
              selectInput("vvar", "What variable would you like to have on the vertical axis?", choices = itemData())
            ),
            column(
              width = 6,
              selectInput("vporq", "Price or quantity?", choices = c("Price", "Quantity"), selected = "Quantity")
            )
          ),
          headerPanel(""),
          fluidRow(
            column(
              width = 12,
              plotlyOutput("heatmaply")
            )
          )
        )
      )
    )
  })
  
  # Report ----
  fc <- reactive({
    data <- reactive_data()
    fc <- data$with_fc
    
    return(fc)
  })
  
  ic <- reactive({
    data <- reactive_data()
    ic <- data$with_ic
    
    return(ic)
  })
  
  lc <- reactive({
    data <- reactive_data()
    lc <- data$with_lc
    
    return(lc)
  })
  
  ncc <- reactive({
    data <- reactive_data()
    ncc <- data$without_cc
    
    return(ncc)
  })
  
  output$report <- downloadHandler(
    filename = "report.html",
    # filename = "report.pdf", # FOR PDF REPORT
      content = function(file) {
        showModal(modalDialog(div(id = "loadingText", "Downloading report."), footer = NULL))
        runjs("window.loadingInterval = animateLoading();")
        
        on.exit({
          removeModal()
          runjs("clearInterval(window.loadingInterval);")
        })
        tempReport <- file.path(tempdir(), "report_html.Rmd")
        # tempReport <- file.path(tempdir(), "report.Rmd") # FOR PDF REPORT
        tempBanner <- file.path(tempdir(), "banner.png")
        # tempHeatmap <- file.path(tempdir(), "heatmap.png")
        file.copy("report_html.Rmd", tempReport, overwrite = T)
        # file.copy("report.Rmd", tempReport, overwrite = T) # FOR PDF REPORT
        file.copy("banner.png", tempBanner, overwrite = T)
        
        # tempHtml <- tempfile(fileext = ".html")
        # saveWidget(as_widget(heatmapReact()), tempHtml)
        # 
        # plotly::export(heatmapReact(), tempHeatmap)
        
        # Define objects used in report
        params <- list(
          fc = fc(),
          ic = ic(),
          lc = lc(),
          ncc = ncc(),
          fallowBenefits = fallow_benefits_data(),
          lateSeasonBenefits = lateSeason_benefits_data(),
          interseedingBenefits = interseeding_benefits_data(),
          porq = input$porq,
          xvar = input$xvar,
          inputData = input$inputData,
          # heatmap_path = tempHeatmap,
          heatmap = heatmapReact(),
          breakeven_text = breakevenReactive(),
          direct_comparison = direct_comparison_report()
        )
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
    }
  )

}

# ShinyApp() ----
# Construct and run app using defined objects. 
shinyApp(
  ui =
    dashboardPage(
      header,
      sidebar,
      body
    ),
  server = server
)
