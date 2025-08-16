library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)

source("css.R")

# UI ---------------------------------------------------------------------------

ui <- page_navbar(
  theme = theme,
  title = tags$div(
    class = "app-name",
    tags$div(
      class = "name-title",
      "Sample Size Planner for Clinical Trials"
    ),
    tags$div(
      class = "name-subtitle",
      "Goodbye guesswork, hello guidelines"
    )
  ),
  fillable = TRUE,
  header = tagList(
    shinyjs::useShinyjs(),
    
    navset_card_tab(
      id = "module",
      
      # --- Bioequivalence -----
      
      nav_panel(
        title = "Bioequivalence",
        value = "be",
        layout_columns(
          col_widths = c(3, 9),
          class = "cols-tight mt-3",
          
          card(
            class = "sidebar-card",
            card_header("Settings"),

            uiOutput(outputId = "ui_mode_specific"),
            uiOutput(outputId = "ui_design"),
            uiOutput(outputId = "ui_cv"),
            uiOutput(outputId = "ui_limits"),
            uiOutput(outputId = "ui_theta0"),

            sliderInput(
              inputId  = "power",
              label    = "Power",
              min      = 0.80,
              max      = 0.95,
              value    = 0.80,
              step     = 0.05,
              ticks    = TRUE
            ),
            uiOutput("ui_dropout"),
            uiOutput(outputId = "ui_side_dynamic"),
            uiOutput(outputId = "ui_alpha"),
            actionButton(
              inputId = "calc_be",
              label   = "Calculate"
            )
          ),
          
          navset_card_tab(
            id = "be_mode",
            nav_panel(
              title = "Bioequivalence",
              value = "be",
              h3("Total N"),
              textOutput(outputId = "be_single_n"),
              h4("By endpoint"),
              tableOutput(outputId = "be_single_by_endp"),
              h4("Allocation"),
              tableOutput(outputId = "be_single_alloc"),
              h4("Details"),
              verbatimTextOutput(outputId = "be_single_details"),
              h4("Graphs"),
              plotOutput("be_single_plot_n_cv", height = 240),
              plotOutput("be_single_plot_power_n", height = 240),
              layout_columns(
                col_widths = c(6, 6),
                actionButton(
                  inputId = "be_single_export_pdf",
                  label = "Export PDF"
                ),
                actionButton(
                  inputId = "be_single_export_docx",
                  label = "Export Word"
                )
              )
            ),
            nav_panel(
              title = "Adaptive (Potvin B)",
              value = "be_adapt",
              h3("Add-on N"),
              textOutput(outputId = "be_adapt_addon"),
              h4("Total N"),
              textOutput(outputId = "be_adapt_total"),
              h4("Details"),
              verbatimTextOutput(outputId = "be_adapt_details")
            ),
            nav_panel(
              title = "Non-inferiority",
              value = "ni",
              h3("Total N"),
              textOutput(outputId = "be_two_n_total"),
              h4("Stage breakdown"),
              tableOutput(outputId = "be_two_stages"),
              h4("Details"),
              verbatimTextOutput(outputId = "be_two_details")
            ),
            nav_panel(
              title = "Non-superiority",
              value = "ns",
              h3("N by design"),
              tableOutput(outputId = "be_cmp_table"),
              h4("Chart"),
              plotOutput("be_cmp_plot", height = 260)
            )
          )
        )
      ),
      
      # --- Phase I -----
      
      nav_panel(
        title = "Phase I",
        value = "p1",
        layout_columns(
          col_widths = c(3, 9),
          class = "cols-tight mt-3",
          
          card(
            class = "sidebar-card",
            card_header("Settings"),
            numericInput(
              inputId = "p1_theta0",
              label = "Effect / Ratio",
              value = 0.95,
              min = 0.8,
              max = 1.25,
              step = 0.001
            ),
            radioButtons(
              inputId = "p1_power",
              label = "Power",
              choices = c(
                "80%" = 0.80,
                "85%" = 0.85,
                "90%" = 0.90,
                "95%" = 0.95
              ),
              selected = 0.80,
              inline = TRUE
            ),
            numericInput(
              inputId = "p1_cv",
              label = "CV",
              value = 0.30,
              min = 0,
              max = 1,
              step = 0.01
            ),
            numericInput(
              inputId = "p1_drop",
              label = "Dropout %",
              value = 10,
              min = 0,
              max = 50,
              step = 1
            ),
            actionButton(
              inputId = "p1_calc",
              label = "Calculate"
            )
          ),
          
          navset_card_tab(
            id = "p1_results",
            nav_panel(
              title = "Results",
              h3("Result"),
              textOutput(outputId = "p1_n"),
              h4("Details"),
              verbatimTextOutput(outputId = "p1_details")
            )
          )
        )
      ),
      
      # --- Phase II -----
      
      nav_panel(
        title = "Phase II",
        value = "p2",
        layout_columns(
          col_widths = c(3, 9),
          class = "cols-tight mt-3",
          
          card(
            class = "sidebar-card",
            card_header("Settings"),
            numericInput(
              inputId = "p2_theta0",
              label = "Effect / Ratio",
              value = 0.95,
              min = 0.8,
              max = 1.25,
              step = 0.001
            ),
            radioButtons(
              inputId = "p2_power",
              label = "Power",
              choices = c(
                "80%" = 0.80,
                "85%" = 0.85,
                "90%" = 0.90,
                "95%" = 0.95
              ),
              selected = 0.80,
              inline = TRUE
            ),
            numericInput(
              inputId = "p2_cv",
              label = "CV",
              value = 0.30,
              min = 0,
              max = 1,
              step = 0.01
            ),
            numericInput(
              inputId = "p2_drop",
              label = "Dropout %",
              value = 10,
              min = 0,
              max = 50,
              step = 1
            ),
            actionButton(
              inputId = "p2_calc",
              label = "Calculate"
            )
          ),
          
          navset_card_tab(
            id = "p2_results",
            nav_panel(
              title = "Results",
              h3("Result"),
              textOutput(outputId = "p2_n"),
              h4("Details"),
              verbatimTextOutput(outputId = "p2_details")
            )
          )
        )
      ),
      
      # --- About -----
      
      nav_panel(
        title = "About",
        value = "about",
        layout_columns(
          col_widths = c(12),
          class = "cols-tight mt-3",
          div(
            h3("About"),
            p("Sample size planning toolkit."),
            p("UI skeleton; calculations are not implemented yet.")
          )
        )
      )
    )
  ),
  footer = div(
    class = "app-footer",
    "Version 1.0 | © 2025 Antonina Dolgorukova"
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  design_labels <- c(
    parallel = "parallel",
    "2x2"    = "2×2",
    "2x2x3"  = "2×2×3",
    "2x3x3"  = "2×3×3",
    "2x2x4"  = "2×2×4"
  )
  labelize <- function(keys) setNames(keys, design_labels[keys])
  
  mode <- reactive(input$be_mode %||% "be")
  
  # Endpoint selection only for NI/NS per spec; hidden for BE
  output$ui_mode_specific <- renderUI({
    if (identical(mode(), "be")) {
      tagList(
        radioButtons(
          inputId = "ntid",
          label   = "Narrow Therapeutic Index Drug?",
          choices = c("No" = "no", "Yes" = "yes"),
          selected = input$ntid %||% "no",
          inline   = TRUE
        )
      )
    } else if (identical(mode(), "ni") || identical(mode(), "ns")) {
      tagList(
        radioButtons(
          inputId = "endp",
          label   = "Endpoint",
          choices = c("AUC", "Cmax"),
          selected = input$endp %||% "AUC",
          inline  = TRUE
        )
      )
    } else {
      NULL
    }
  })

  output$ui_design <- renderUI({
    choices <- switch(
      mode(),
      "be" = if (identical(input$ntid, "yes"))
        c("parallel", "2x2", "2x2x4")
      else
        c("parallel", "2x2", "2x2x3", "2x3x3", "2x2x4"),
      "ni" = c("parallel", "2x2"),
      "ns" = c("parallel", "2x2"),
      "be_adapt" = c("2x2"),
      c("parallel", "2x2") # fallback
    )
    radioButtons(
      inputId = "design",
      label   = "Design",
      choices = labelize(choices)
    )
  })

  output$ui_cv <- renderUI({
    is_parallel <- identical(input$design, "parallel")
    is_repl     <- isTRUE(input$design  %in% c("2x2x3", "2x3x3", "2x2x4"))
    
    label_prefix     <- if (is_parallel) "CVinter" else "CVintra"
    repl_cmax_prefix <- if (is_repl) "CVwR" else label_prefix
    
    if (identical(mode(), "ni") || identical(mode(), "ns")) {
      endp <- input$endp %||% "AUC"
      numericInput(
        inputId = "cv_single",
        label   = paste0(label_prefix, " (", endp, ")"),
        value   = input$cv_single %||% 0.30,
        min     = 0, max = 1, step = 0.01, width = "100%"
      )
    } else {
      layout_columns(
        col_widths = c(6, 6),
        numericInput(
          inputId = "cv_auc",
          label   = paste0(label_prefix, " (AUC)"),
          value   = input$cv_auc %||% 0.30,
          min     = 0, max = 1, step = 0.01, width = "100%"
        ),
        numericInput(
          inputId = "cv_cmax",
          label   = paste0(repl_cmax_prefix, " (Cmax)"),
          value   = input$cv_cmax %||% 0.30,
          min     = 0, max = 1, step = 0.01, width = "100%"
        )
      )
    }
  })

  output$ui_limits <- renderUI({
    if (identical(mode(), "be")) {
      if (identical(input$ntid, "yes")) {
        if (identical(input$design, "2x2") ||
            identical(input$design, "parallel")) {
          radioButtons("limits", "Bioequivalence acceptance limits",
                       choices = c("90–111.11%"),
                       selected = "90–111.11%")
        } else {
          radioButtons("limits", "Bioequivalence acceptance limits",
                       choices = c("AUC: 90–111.11%, Cmax: FDA RSABE for NTID"),
                       selected = "AUC: 90–111.11%, Cmax: FDA RSABE for NTID")
        }
      } else {
        if (identical(input$design, "2x2") ||
            identical(input$design, "parallel")) {
          radioButtons("limits", "Bioequivalence acceptance limits",
                       choices = c("80–125%"),
                       selected = "80–125%")
        } else {
          radioButtons("limits", "Bioequivalence acceptance limits",
                       choices = c("AUC: 80–125%, Cmax: scaled by CV"),
                       selected = "AUC: 80–125%, Cmax: scaled by CV")
        }
      }
    } else if (identical(mode(), "ni")) {
      radioButtons("margin", "Margin",
                   choices = c("80%"), selected = "80%")
    } else if (identical(mode(), "ns")) {
      radioButtons("margin", "Margin",
                   choices = c("125%"), selected = "125%")
    } else NULL
  })

  output$ui_theta0 <- renderUI({
    ntidr <- identical(input$ntid, "yes") && identical(input$design, "2x2x4")
    lab_be <- if (ntidr) "AUC T/R (θ₀) (using 0.975 for Cmax)" else "T/R (θ₀)"
    switch(
      mode(),
      ns = numericInput("theta0", "T/R (θ₀)", 1.05, 1.05, 1.05, 0.01),
      ni = numericInput("theta0", "T/R (θ₀)", 0.95, 0.95, 0.95, 0.01),
      numericInput("theta0", lab_be, 0.95, 0.90, 0.95, 0.01)
    )
  })
  
  output$ui_dropout <- renderUI({
    if (mode() %in% c("be", "ni", "ns")) {
      numericInput(
        inputId = "drop",
        label   = "Dropout, %",
        value   = 10,
        min     = 0,
        max     = 100,
        step    = 1
      )
    } else {
      NULL
    }
  })

  output$ui_alpha <- renderUI({
    if (identical(mode(), "be")) {
      radioButtons("alpha", "Alpha",
                   choices = c("0.05"), selected = "0.05", inline = TRUE)
    } else if (identical(mode(), "ni") || identical(mode(), "ns")) {
      radioButtons("alpha", "Alpha",
                   choices = c("0.025"), selected = "0.025", inline = TRUE)
    } else if (identical(mode(), "be_adapt")) {
      radioButtons("alpha", "Alpha",
                   choices = c("0.0294"), selected = "0.0294", inline = TRUE)
    } else NULL
  })
}

shinyApp(ui, server)
