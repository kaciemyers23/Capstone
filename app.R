# Capstone Interactive Dashboard Woohoo!
# Author: Kacie Myers

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(randomForest)

# ----LOAD DATA----
cognition_raw <- read.csv("data/cognition_raw.csv")
companionship_raw <- read.csv("data/companionship_raw.csv")
cognition <- read.csv("data/cognition_clean.csv")
companionship <- read.csv("data/companionship_clean.csv")

# ----QUICK CLEANING----
cognition <- cognition %>%
  mutate(AI_usage_freq = 
           factor(AI_usage_freq, 
                  levels = c("Never", "Rarely", "Occasionally", "Sometimes", "Often", "Very often", "Everyday"),
                  ordered = TRUE),
         hours_online = 
           factor(hours_online, 
                  levels = c("Less than 2 hours", "2–4 hours", "5–6 hours", "7–8 hours", "9–10 hours", "11–12 hours", "More than 12 hours"),
                  ordered = TRUE),
         age = 
           factor(age, 
                  levels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 65", "Over 65"),
                  ordered = TRUE),
         work_status = 
           factor(work_status, 
                  levels = c("Student", "Not currently working", "Employed part-time", "Employed full time", "Self-employed / freelance", "Other"),
                  ordered = TRUE),
         education = 
           factor(education, 
                  levels = c("Less than high school", "High school", "Some university/college", "Bachelor’s degree", "Postgraduate degree", "Other"),
                  ordered = TRUE))


companionship <- companionship %>%
  mutate(AI_usage_freq = 
           factor(AI_usage_freq, 
                  levels = c("Never", "Rarely", "Occasionally", "Sometimes", "Often", "Very often", "Everyday"),
                  ordered = TRUE),
         hours_online = 
           factor(hours_online, 
                  levels = c("Less than 2 hours", "2–4 hours", "5–6 hours", "7–8 hours", "9–10 hours", "11–12 hours", "More than 12 hours"),
                  ordered = TRUE),
         attachment_encounters_freq = 
           factor(attachment_encounters_freq, 
                  levels = c("Never", "Rarely", "Occasionally", "Sometimes", "Often", "Very often", "Everyday"),
                  ordered = TRUE),
         AI_replace_human_freq =
           factor(AI_replace_human_freq, 
                  levels = c("Never", "Rarely", "Occasionally", "Sometimes", "Often", "Very often", "Everyday"),
                  ordered = TRUE),
         digital_effect_mind =
           factor(digital_effect_mind, 
                  levels = c("Not at all", "Slightly", "Moderately", "Significantly", "Extremely"),
                  ordered = TRUE),
         age = 
           factor(age, 
                  levels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55 - 65", "Over 65"),
                  ordered = TRUE),
         work_status = 
           factor(work_status, 
                  levels = c("Student", "Not currently working", "Employed part-time", "Employed full time", "Self-employed / freelance", "Other"),
                  ordered = TRUE),
         education = 
           factor(education, 
                  levels = c("Less than high school", "High school", "Some university/college", "Bachelor’s degree", "Postgraduate degree", "Other"),
                  ordered = TRUE))


ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "AI & Human Cognition Dashboard",
                  tags$li(
                    class = "dropdown",
                    tags$a(
                      href = "user_manual.pdf",
                      target = "_blank",
                      style = "padding: 15px;",
                      icon("file-pdf"),
                      span(" User Manual")
                    )
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Cognition Dataset", tabName = "cognition", icon = icon("brain")),
      menuItem("Companionship Dataset", tabName = "companionship", icon = icon("users")),
      menuItem("Factor Analysis", tabName = "fa", icon = icon("layer-group")),
      menuItem("Linear Regression", tabName = "lm", icon = icon("chart-line")),
      menuItem("CART & Random Forest", tabName = "cart", icon = icon("tree"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      
      # ----OVERVIEW----
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Project Description", width = 12,
                    p("This dashboard explores how AI engagement impacts decision-making and companionship.")),
                box(title = "Data & Methodology", width = 6,
                    tags$ul(
                      tags$li("Survey-based datasets on cognition and companionship"),
                      tags$li("Exploratory data analysis and visualization"),
                      tags$li("Factor analysis and dimensionality reduction"),
                      tags$li("Linear regression for relationship modeling"),
                      tags$li("Machine learning models (CART & Random Forest)")
                    )),
                box(title = "How to Use This Dashboard", width = 6,
                    tags$ul(
                      tags$li("Navigate through tabs using the sidebar"),
                      tags$li("Select variables to explore relationships in the datasets"),
                      tags$li("Use the Factor Analysis tab to identify underlying patterns"),
                      tags$li("Review model outputs in the regression and machine learning selections"),
                      tags$li("Interpret results to understand how AI usage relates to cognition and behavior")
                    )),
                box(title = "Research Focus", width = 12,
                    p("This project is guided by key research questions:"),
                    tags$ul(
                      tags$li("How does AI usage influence human decision-making processes?"),
                      tags$li("What role does AI play in perceived companionship and social connection?"),
                      tags$li("Can statistical and machine learning models predict AI usage based on cognitive and relational behavior?")
                    ))
                )
              ),
      
      # ----COGNITION DATASET----
      tabItem(tabName = "cognition",
              fluidRow(
                box(title = "AI Usage and Cognitive Dependence", width = 12,
                    p("Select variables below to explore relationships & patterns in the cognition dataset")),
                box(title = "Filters", width = 6,
                    selectInput("cognition_vars", "Select Variable(s)", choices = NULL, multiple = TRUE)),
                    
                box(width = 12, plotlyOutput("cognition_plot"))
                
              ),
              fluidRow(
                box(title = "Cognition Dataset", width = 12, 
                    selectInput("cognition_data_choice", "Choose View",
                                choices = c("Raw Dataset", "Data Dictionary"))
              ),
              box(width = 12,
                  DTOutput("cognition_table"))
      
        )
    ),
      
      # ----COMPANIONSHIP DATASET----
      tabItem(tabName = "companionship",
              fluidRow(
                box(title = "AI Usage and Social Companionship", width = 12,
                    p("Select variables below to explore relationships & patterns in the companionship dataset")),
                box(title = "Filters", width = 6,
                    selectInput("companionship_vars", "Select Variable(s)", choices = NULL, multiple = TRUE)),    
                
                box(width = 12, plotlyOutput("companionship_plot"))
              ),
              fluidRow(
                box(title = "Companionship Dataset", width = 12, 
                    selectInput("companionship_data_choice", "Choose View",
                                choices = c("Raw Dataset", "Data Dictionary"))
                ),
                box(width = 12,
                    DTOutput("companionship_table"))
              )
      ),
      
      # ----FACTOR ANALYSIS----
      tabItem(tabName = "fa",
              fluidRow(
                box(title = "Dataset Selection", width = 4,
                    selectInput("fa_dataset", "Choose Dataset",
                                choices = c("Companionship", "Cognition")))
              ),
              fluidRow(
                box(title = "Factor Analysis Overview", width = 12,
                    p("This section displays the results of a factor analysis to reduce the dimensionality of the data."))
              ),
              fluidRow(
                box(title = "Scree Plot", width = 6,
                    plotOutput("scree_plot")),
                box(title = "Factor Interpretation", width = 6,
                    htmlOutput("fa_description")),
                box(title = "Factor Loadings", width = 12,
                    div(
                      style = "height: 400px; width: 100%; overflow: auto",
                      DTOutput("fa_loadings")
                    )
                ),
                box(title = "Factor Analysis Plot", width = 12, height = 700,
                    plotOutput("fa_plot"))
                ),
    
      ),
      
      # ----LINEAR REGRESSION----
      tabItem(tabName = "lm",
              fluidRow(
                box(title = "Dataset Selection", width = 4,
                    selectInput("lm_dataset", "Choose Dataset",
                                choices = c("Companionship", "Cognition"))
                    ),
                box(width = 8,
                    h3("Linear Regression Model Results")),
                box(title = "Model Equation", width = 12, 
                    textOutput("lm_equation"))
              ),
              fluidRow(
                box(title = "Linear Model Summary", width = 12,
                    div(
                      style = "height: 400px; width: 100%; overflow: auto",
                      DTOutput("lm_summary"))
                    )
              ),
              fluidRow(
                valueBoxOutput("r2_box"),
                valueBoxOutput("adj_r2_box"),
                valueBoxOutput("rmse_box")
              )
      ),
      
      # ----CART/RANDOM FOREST----
      tabItem(tabName = "cart",
              fluidRow(
                box(title = "Dataset Selection", width = 4,
                    selectInput("cart_dataset", "Choose Dataset",
                                choices = c("Companionship", "Cognition"))
                ),
                box(width = 8,
                    h3("CART and Random Forest Model Results"))
              ),
              fluidRow(
                box(title = "Cart Tree Visualization", width = 12,
                    plotOutput("cart_plot"))
              ),
              fluidRow(
                box(title = "CART model", width = 12,
                    div(
                      style = "height: 400px; width: 100%; overflow: auto",
                      verbatimTextOutput("cart_output")
                    ))
                ),
                
              fluidRow(
                box(title = "Random Forest Confusion Matrix", width = 12,
                      DTOutput("rf_output")
                ),
                box(title = "Random Forest Variable Importance", width = 12,
                    plotOutput("rf_importance")))
                
              
              
      )
    )
  )
)


# ----SERVER----

server <- function(input, output, session) {
  
  get_data <- function(choice) {
    if (choice == "Cognition") return(cognition)
    else return(companionship)
  }
  
  output$cognition_table <- renderDT({
    req(input$cognition_data_choice)
    if (input$cognition_data_choice == "Raw Dataset") 
    {
      datatable(cognition_raw, options = list(scrollX = TRUE)) 
    }
    else 
    {
      datatable(read_csv("data/cognition_data_dict.csv"), options = list(scrollX = TRUE))
    }
  })   
  
  
  output$companionship_table <- renderDT({
    req(input$companionship_data_choice)
    if (input$companionship_data_choice == "Raw Dataset") 
      {
      datatable(companionship_raw, options = list(scrollX = TRUE)) 
      }
    else 
      {
      datatable(read_csv("data/companionship_data_dict.csv"), options = list(scrollX = TRUE))
      }
  })         
                
  # ----UPDATE SELECT INPUTS----
  
  # cognition
  cognition_numeric_vars <- reactive({
    df <- cognition
    names(df)[sapply(df, is.numeric)]
  })
  
  observe({
    df <- cognition
    all_vars <- names(df)
    num_vars <- cognition_numeric_vars()
    selected <- input$cognition_vars %||% character(0)
    choices <- all_vars
    
    if(length(selected) == 1)
    {
      choices <- union(selected, num_vars)
    }
    updateSelectInput(session, "cognition_vars",
                      choices = choices, 
                      selected = selected)
  })
  
  
  
  # companionship
  companionship_numeric_vars <- reactive({
    df <- companionship
    names(df)[sapply(df, is.numeric)]
  })
  
  observe({
    df <- companionship
    all_vars <- names(df)
    num_vars <- companionship_numeric_vars()
    selected <- input$companionship_vars %||% character(0)
    choices <- all_vars
    if(length(selected) == 1)
    {
      choices <- union(selected, num_vars)
    }

    updateSelectInput(session, "companionship_vars",
                      choices = choices,
                      selected = selected)
    
  })
  
  # ----COGNITION PLOTS----
  output$cognition_plot <- renderPlotly({
    req(input$cognition_vars)
    df <- cognition
    cognition_vars <- input$cognition_vars
    if (length(cognition_vars) == 1) 
    {
      plot_ly(df,
              x = ~get(cognition_vars[1]),
              type = "histogram") %>%
        layout(xaxis = list(title = cognition_vars[1]),
               yaxis = list(title = "Count"))
    } else if (length(cognition_vars) == 2)
    {
      plot_ly(df,
              x = ~jitter(get(cognition_vars[1])),
              y = ~jitter(get(cognition_vars[2])),
              type = "scatter",
              mode = "markers") %>%
        layout(xaxis = list(title = cognition_vars[1]),
               yaxis = list(title = cognition_vars[2]))
    } else if (length(cognition_vars) == 3)
    {
      plot_ly(df,
              x = ~jitter(get(cognition_vars[1])),
              y = ~jitter(get(cognition_vars[2])),
              split = ~.data[[cognition_vars[3]]],
              type = "scatter",
              mode = "markers") %>%
        layout(xaxis = list(title = cognition_vars[1]),
               yaxis = list(title = cognition_vars[2]),
               legend = list(title = list(text = cognition_vars[3])))
    } else
    {
      plotly::plotly_empty() %>% layout(title = "Please select no more than 3 variables")
    }
    
  })
  
  
  # ----COMPANIONSHIP PLOTS----
  output$companionship_plot <- renderPlotly({
    req(input$companionship_vars)
    df <- companionship
    
    companionship_vars <- input$companionship_vars
    if (length(companionship_vars) == 1) 
    {
      plot_ly(df,
              x = ~get(companionship_vars[1]),
              type = "histogram") %>%
        layout(xaxis = list(title = companionship_vars[1]),
               yaxis = list(title = "Count"))
    } else if (length(companionship_vars) == 2)
    {
      plot_ly(df,
              x = ~jitter(get(companionship_vars[1])),
              y = ~jitter(get(companionship_vars[2])),
              type = "scatter",
              mode = "markers") %>%
        layout(xaxis = list(title = companionship_vars[1]),
               yaxis = list(title = companionship_vars[2]))
    } else if (length(companionship_vars) == 3)
    {
      plot_ly(df,
              x = ~jitter(get(companionship_vars[1])),
              y = ~jitter(get(companionship_vars[2])),
              split = ~.data[[companionship_vars[3]]],
              type = "scatter",
              mode = "markers") %>%
        layout(xaxis = list(title = companionship_vars[1]),
               yaxis = list(title = companionship_vars[2]),
               legend = list(title = list(text = companionship_vars[3])))
    } else
    {
      plotly::plotly_empty() %>% layout(title = "Please select 1, 2, or 3 variables")
    }
    
  })
  
  
  
  # ----FACTOR ANALYSIS----
  fa_model <- reactive({
    if (input$fa_dataset == "Companionship") 
    {
      readRDS("data/models/fa_companionship.rds")
    } else
    {
      readRDS("data/models/fa_cognition.rds")
    }
  })
  
  # Scree plot
  output$scree_plot <- renderPlot({
    df <- if (input$fa_dataset == "Companionship") companionship else cognition
    num_df <- df[, sapply(df, is.numeric)]
    num_df <- num_df[, sapply(num_df, function(x) sd(x, na.rm = TRUE) > 0)]
    num_df <- na.omit(num_df)
    cor_mat <- cor(num_df, use = "pairwise.complete.obs")
    cor_mat[is.na(cor_mat)] <- 0
    ev <- eigen(cor_mat)$values
    
    plot(ev, type = "b", pch = 19,
         xlab = "Factor Number", ylab = "Eigenvalue",
         main = "Scree Plot")
    abline(h = 1, lty = 2)
  })
  
  # Loadings
  output$fa_loadings <- renderDT({
    loadings_df <- as.data.frame(fa_model()$loadings[,]) %>%
      round(3) %>%
      tibble::rownames_to_column("Variable")
    
    datatable(
      loadings_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # Factor Interpretation
  output$fa_description <- renderUI({
    
    if (input$fa_dataset == "Cognition") {
      tagList(
        p(strong("Factor 1:"), " AI Decision Reliance"),
        p(strong("Factor 2: "), " AI Influence Awareness & Conflict"),
        p(strong("Factor 3: "), " Personal Wellbeing & Life Clarity")
      )
    } else {
      tagList(
        p(strong("Factor 1:"), " AI Emotional Connection & Companionship"),
        p(strong("Factor 2:"), " Personal Wellbeing & Values Alignment"),
        p(strong("Factor 3:"), " AI Trust & Concerns"),
        p(strong("Factor 4:"), " AI Familiarity & Comfortability"),
        p(strong("Factor 5:"), " Perceived AI Value Alignment")
      )
    }
  })
  
  # Factor Plot
  output$fa_plot <- renderPlot({
    load <- fa_model()$loadings[,]
    plot(load[,1], load[,2], 
         xlab = "Factor 1", ylab = "Factor 2",
         pch = 19)
    
    text(load[,1], load[,2], 
         labels = rownames(load), 
         pos = 3, cex = 1)
  }, height = 600)
  
  
  # ----LINEAR REGRESSION----
  lm_model <- reactive({
    if (input$lm_dataset == "Companionship")
    {
      readRDS("data/models/lm_companionship_step.rds")
    } else
    {
      readRDS("data/models/lm_cognition_step.rds")
    }
  })
  
  output$lm_equation <- renderText({
    model <- lm_model()
    formula <- formula(model)
    paste(deparse(formula))
  })
  output$lm_summary <- renderDT({
    df <- summary(lm_model())$coefficients %>%
                    as.data.frame() %>%
                    tibble::rownames_to_column("Variable")
    df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 3) else x)
    datatable(df, options = list(pageLength = 33))
  })
  
  output$r2_box <- renderValueBox({
    r2 <- summary(lm_model())$r.squared
    
    valueBox(
      value = round(r2, 3),
      subtitle = "R Squared",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$adj_r2_box <- renderValueBox({
    adj_r2 <- summary(lm_model())$adj.r.squared
    
    valueBox(
      value = round(adj_r2, 3),
      subtitle = "Adjusted R Squared",
      icon = icon("chart-line"),
      color = "aqua"
    )
  })
  
  output$rmse_box <- renderValueBox({
    model <- lm_model()
    rmse <- sqrt(mean(residuals(model)^2))
    
    valueBox(
      value = round(rmse, 3),
      subtitle = "Root Mean Squared Error",
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  
  # ----CART/RANDOM FOREST----
  
  cart_model <- reactive({
    if (input$cart_dataset == "Companionship") 
    {
      readRDS("data/models/cart_companionship.rds")
    } else
    {
      readRDS("data/models/cart_cognition.rds")
    }
  })
  
  rf_model <- reactive({
    if (input$cart_dataset == "Companionship")
    {
      readRDS("data/models/rf_companionship.rds")
    } else
    {
      readRDS("data/models/rf_cognition.rds")
    }
  })
  
  output$cart_output <- renderPrint({
    print(cart_model())
  })
  
  output$rf_output <- renderDT({
    rf <- rf_model()
    df <- as.data.frame(rf$confusion)
    df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 3) else x)
    datatable(df, options = list(scrollX = TRUE))
  })
  
  output$rf_importance <- renderPlot({
    rf <- rf_model()
    varImpPlot(rf)
  })
  

  
  output$cart_plot <- renderPlot({
    req(cart_model())
    
    rpart.plot::rpart.plot(
      cart_model(),
      type = 2,
      extra = 104,
      fallen.leaves = TRUE,
      cex = 0.7
    )
  })
  
  
}

shinyApp(ui, server)
