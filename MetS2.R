library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(pROC)

# ---------------------------
# Load data + model (FAST)
# ---------------------------
DATA_PATH  <- "NHANES_combined_all_cycles.rds"
MODEL_PATH <- "mets_glm_model.rds"
TAB_PATH   <- "mets_model_comparison.rds"

df_all  <- readRDS(DATA_PATH)
fit_glm <- readRDS(MODEL_PATH)

model_table <- NULL
if (file.exists(TAB_PATH)) model_table <- readRDS(TAB_PATH)

# ---------------------------
# UI
# ---------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Metabolic Risk App (Grocery Haul Mode)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Risk Calculator", tabName = "calc", icon = icon("calculator")),
      menuItem("Model Performance", tabName = "perf", icon = icon("chart-line")),
      menuItem("Dataset Summary", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "calc",
        fluidRow(
          box(
            width = 5, title = "Household + Grocery Items", status = "primary",
            
            selectInput("sex", "Sex", choices = c("Male" = 1, "Female" = 2), selected = 1),
            sliderInput("age", "Age", min = 12, max = 80, value = 43),
            sliderInput("waist", "Waist (cm)", min = 50, max = 160, value = 95),
            
            tags$hr(),
            
            numericInput("days", "Days this grocery haul will last", value = 7, min = 1, max = 60, step = 1),
            numericInput("people", "Number of people consuming", value = 2, min = 1, max = 20, step = 1),
            
            tags$hr(),
            
            h4("Add grocery items"),
            textInput("item_name", "Item name", value = "Oats"),
            numericInput("item_servings", "Servings in haul (total)", value = 10, min = 0, step = 1),
            numericInput("item_kcal", "Calories per serving", value = 150, min = 0, step = 10),
            numericInput("item_prot", "Protein (g) per serving", value = 5, min = 0, step = 1),
            numericInput("item_fiber", "Fiber (g) per serving", value = 4, min = 0, step = 1),
            
            fluidRow(
              column(6, actionButton("add_item", "Add item", icon = icon("plus"), class = "btn-success")),
              column(6, actionButton("clear_items", "Clear all", icon = icon("trash"), class = "btn-danger"))
            ),
            
            helpText("Enter items as totals for the whole grocery haul. The app sums them, then converts to per-person-per-day using days × people.")
          ),
          
          box(
            width = 7, title = "Totals → Daily Intake → Predicted MetS Risk", status = "primary",
            h4("Grocery haul item list"),
            tableOutput("items_table"),
            tags$hr(),
            verbatimTextOutput("totals_text"),
            tags$hr(),
            verbatimTextOutput("ppd_text"),
            tags$hr(),
            h3(textOutput("risk_text")),
            plotOutput("risk_plot", height = 250),
            helpText("This is a population risk estimate from NHANES-based models; not a medical diagnosis.")
          )
        )
      ),
      
      tabItem(
        tabName = "perf",
        fluidRow(
          box(width = 6, title = "ROC (GLM)", status = "info",
              plotOutput("roc_plot", height = 300)
          ),
          box(width = 6, title = "Calibration (Deciles)", status = "info",
              plotOutput("cal_plot", height = 300)
          )
        ),
        fluidRow(
          box(width = 12, title = "Model Comparison (if available)", status = "warning",
              tableOutput("model_table_out")
          )
        )
      ),
      
      tabItem(
        tabName = "data",
        fluidRow(
          box(width = 12, title = "Basic Dataset Summary", status = "primary",
              verbatimTextOutput("data_summary")
          )
        )
      )
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session){
  
  # Store grocery items in a reactive table
  items <- reactiveVal(
    tibble(
      item = character(),
      servings = numeric(),
      kcal_per_serv = numeric(),
      prot_per_serv = numeric(),
      fiber_per_serv = numeric()
    )
  )
  
  observeEvent(input$add_item, {
    df <- items()
    
    new_row <- tibble(
      item = ifelse(nchar(trimws(input$item_name)) == 0, "Item", trimws(input$item_name)),
      servings = as.numeric(input$item_servings),
      kcal_per_serv = as.numeric(input$item_kcal),
      prot_per_serv = as.numeric(input$item_prot),
      fiber_per_serv = as.numeric(input$item_fiber)
    )
    
    # Basic guardrails
    new_row <- new_row %>%
      mutate(
        servings = ifelse(is.na(servings) | servings < 0, 0, servings),
        kcal_per_serv = ifelse(is.na(kcal_per_serv) | kcal_per_serv < 0, 0, kcal_per_serv),
        prot_per_serv = ifelse(is.na(prot_per_serv) | prot_per_serv < 0, 0, prot_per_serv),
        fiber_per_serv = ifelse(is.na(fiber_per_serv) | fiber_per_serv < 0, 0, fiber_per_serv)
      )
    
    items(bind_rows(df, new_row))
  })
  
  observeEvent(input$clear_items, {
    items(tibble(
      item = character(),
      servings = numeric(),
      kcal_per_serv = numeric(),
      prot_per_serv = numeric(),
      fiber_per_serv = numeric()
    ))
  })
  
  # Display item list (with per-item totals)
  output$items_table <- renderTable({
    df <- items()
    if (nrow(df) == 0) {
      return(data.frame(Message = "No items added yet. Add items to compute haul totals."))
    }
    
    df %>%
      mutate(
        total_kcal  = servings * kcal_per_serv,
        total_prot  = servings * prot_per_serv,
        total_fiber = servings * fiber_per_serv
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # Compute haul totals
  haul_totals <- reactive({
    df <- items()
    if (nrow(df) == 0) {
      return(list(kcal = 0, prot = 0, fiber = 0))
    }
    
    df2 <- df %>%
      mutate(
        total_kcal  = servings * kcal_per_serv,
        total_prot  = servings * prot_per_serv,
        total_fiber = servings * fiber_per_serv
      )
    
    list(
      kcal  = sum(df2$total_kcal,  na.rm = TRUE),
      prot  = sum(df2$total_prot,  na.rm = TRUE),
      fiber = sum(df2$total_fiber, na.rm = TRUE)
    )
  })
  
  output$totals_text <- renderPrint({
    t <- haul_totals()
    cat("Haul totals (sum of all items):\n")
    cat("Total Calories:", round(t$kcal, 1), "\n")
    cat("Total Protein (g):", round(t$prot, 1), "\n")
    cat("Total Fiber (g):", round(t$fiber, 1), "\n")
  })
  
  # Convert totals -> per-person-per-day (NHANES-style daily intake)
  nhanes_daily <- reactive({
    t <- haul_totals()
    
    days   <- max(1, as.numeric(input$days))
    people <- max(1, as.numeric(input$people))
    denom  <- days * people
    
    kcal_ppd  <- t$kcal  / denom
    prot_ppd  <- t$prot  / denom
    fiber_ppd <- t$fiber / denom
    
    # Avoid divide by 0 kcal
    kcal_ppd_safe <- ifelse(kcal_ppd <= 0, NA, kcal_ppd)
    
    fiber_per_1000kcal   <- fiber_ppd / (kcal_ppd_safe / 1000)
    protein_per_1000kcal <- prot_ppd  / (kcal_ppd_safe / 1000)
    
    data.frame(
      RIAGENDR = as.numeric(input$sex),
      RIDAGEYR = as.numeric(input$age),
      BMXWAIST = as.numeric(input$waist),
      
      DR1TKCAL = as.numeric(kcal_ppd),
      DR1TPROT = as.numeric(prot_ppd),
      DR1TFIBE = as.numeric(fiber_ppd),
      
      fiber_per_1000kcal = as.numeric(fiber_per_1000kcal),
      protein_per_1000kcal = as.numeric(protein_per_1000kcal)
    )
  })
  
  output$ppd_text <- renderPrint({
    nd <- nhanes_daily()
    cat("Converted to NHANES-style daily intake (per person per day):\n")
    cat("Calories:", round(nd$DR1TKCAL, 1), "\n")
    cat("Protein (g):", round(nd$DR1TPROT, 1), "\n")
    cat("Fiber (g):", round(nd$DR1TFIBE, 1), "\n")
    cat("\nDiet density features:\n")
    cat("Fiber per 1000 kcal:", round(nd$fiber_per_1000kcal, 2), "\n")
    cat("Protein per 1000 kcal:", round(nd$protein_per_1000kcal, 2), "\n")
  })
  
  # Predict risk
  pred <- reactive({
    nd <- nhanes_daily()
    
    # If kcal is 0, ratios become NA; handle safely
    # Option 1: return NA risk
    if (any(is.na(nd$fiber_per_1000kcal)) || any(is.na(nd$protein_per_1000kcal))) {
      return(NA_real_)
    }
    
    p <- predict(fit_glm, newdata = nd, type = "response")
    as.numeric(p)
  })
  
  output$risk_text <- renderText({
    p <- pred()
    if (is.na(p)) return("Predicted Risk = NA (add items and ensure calories > 0)")
    paste0("Predicted Risk = ", round(p * 100, 1), "%")
  })
  
  output$risk_plot <- renderPlot({
    p <- pred()
    if (is.na(p)) {
      plot.new()
      text(0.5, 0.5, "Add grocery items and ensure calories > 0 to compute risk.")
      return()
    }
    
    ggplot(data.frame(risk = p), aes(x = "", y = risk)) +
      geom_col(width = 0.4) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "", y = "Probability", title = "Predicted MetS Probability") +
      theme_minimal()
  })
  
  # ---------- Performance plots ----------
  # Build a quick holdout from df_all once
  set.seed(123)
  model_df <- df_all %>%
    select(
      MetS, RIAGENDR, RIDAGEYR, BMXWAIST,
      DR1TKCAL, DR1TPROT, DR1TFIBE,
      fiber_per_1000kcal, protein_per_1000kcal
    ) %>%
    tidyr::drop_na()
  
  idx <- sample(seq_len(nrow(model_df)), size = 0.8 * nrow(model_df))
  test_df <- model_df[-idx, ]
  test_pred <- predict(fit_glm, newdata = test_df, type = "response")
  
  output$roc_plot <- renderPlot({
    roc_obj <- pROC::roc(test_df$MetS, test_pred, quiet = TRUE)
    plot(roc_obj, main = paste0("ROC Curve (AUC=", round(pROC::auc(roc_obj), 3), ")"))
  })
  
  output$cal_plot <- renderPlot({
    calib <- test_df %>%
      mutate(pred = test_pred, bin = dplyr::ntile(pred, 10)) %>%
      group_by(bin) %>%
      summarise(mean_pred = mean(pred), obs_rate = mean(MetS), .groups = "drop")
    
    ggplot(calib, aes(x = mean_pred, y = obs_rate)) +
      geom_point() +
      geom_line() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Calibration Plot", x = "Mean Predicted Risk", y = "Observed Rate") +
      theme_minimal()
  })
  
  output$model_table_out <- renderTable({
    if (is.null(model_table)) return(data.frame(Message = "No comparison table saved yet."))
    model_table
  })
  
  output$data_summary <- renderPrint({
    cat("Rows:", nrow(df_all), "\n")
    cat("Columns:", ncol(df_all), "\n\n")
    cat("MetS prevalence:", mean(df_all$MetS, na.rm = TRUE), "\n\n")
    if ("cycle" %in% names(df_all)) {
      print(df_all %>% count(cycle))
    } else {
      cat("No 'cycle' column found.\n")
    }
  })
}

shinyApp(ui, server)