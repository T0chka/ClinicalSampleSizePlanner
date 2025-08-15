library(shiny)
library(bslib)
library(shinyjs)

source("css.R")

ui <- page_navbar(
  theme = theme,
  title = tags$div(
    class = "app-name",
    tags$div(class = "name-title",
             "Sample Size Planner for Clinical Trials"),
    tags$div(class = "name-subtitle",
             "Goodbye guesswork, hello guidelines")
  ),
  fillable = TRUE,
  header = tagList(
    shinyjs::useShinyjs(),
    
    navset_card_tab(
      id = "module",
      
      nav_panel(
        title = "БЭ",
        value = "be",
        layout_columns(
          col_widths = c(3, 9),
          class = "cols-tight mt-3",
          
          card(
            class = "sidebar-card",
            card_header("Settings"),

            radioButtons(
              inputId = "mode",
              label   = "Mode",
              choices = c(
                "BE" = "be",
                "NI" = "ni",
                "NS" = "ns"
              ),
              selected = "be",
              inline   = TRUE
            ),

            uiOutput(outputId = "ui_endp"),
            uiOutput(outputId = "ui_ntid"),
            uiOutput(outputId = "ui_design"),
            uiOutput(outputId = "ui_cv"),
            uiOutput(outputId = "ui_limits"),
            uiOutput(outputId = "ui_theta0"),

            radioButtons(
              inputId  = "power",
              label    = "Power",
              choices  = c(
                "80%" = 0.80,
                "85%" = 0.85,
                "90%" = 0.90,
                "95%" = 0.95
              ),
              selected = 0.80,
              inline   = TRUE
            ),
            numericInput(
              inputId = "drop",
              label   = "Dropout, %",
              value   = 10,
              min     = 0,
              max     = 100,
              step    = 1
            ),
            uiOutput(outputId = "ui_alpha"),
            actionButton(
              inputId = "calc_be",
              label   = "Calculate"
            )
          ),
          
          navset_card_tab(
            id = "be_mode",
            
            nav_panel(
              title = "Single-stage",
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
              title = "Two-stage",
              h3("Total N"),
              textOutput(outputId = "be_two_n_total"),
              h4("Stage breakdown"),
              tableOutput(outputId = "be_two_stages"),
              h4("Details"),
              verbatimTextOutput(outputId = "be_two_details")
            ),
            
            nav_panel(
              title = "Compare",
              h3("N by design"),
              tableOutput(outputId = "be_cmp_table"),
              h4("Chart"),
              plotOutput("be_cmp_plot", height = 260)
            )
          )
        )
      ),
      
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

server <- function(input, output, session) {
  # Helpers
  design_labels <- c(
    parallel = "parallel",
    "2x2"    = "2×2",
    "2x2x3"  = "2×2×3",
    "2x3x3"  = "2×3×3",
    "2x2x4"  = "2×2×4"
  )
  labelize <- function(keys) setNames(keys, design_labels[keys])

  # UI: dynamic controls
  # Endpoint selection only for NI/NS per spec; hidden for BE
  output$ui_endp <- renderUI({
    if (identical(input$mode, "ni") || identical(input$mode, "ns")) {
      radioButtons(
        inputId = "endp",
        label   = "Endpoint",
        choices = c("AUC", "Cmax"),
        selected = if (isTruthy(input$endp)) input$endp else "AUC",
        inline  = TRUE
      )
    } else {
      NULL
    }
  })

  output$ui_ntid <- renderUI({
    if (!identical(input$mode, "be")) return(NULL)
    radioButtons(
      inputId = "ntid",
      label   = "NTID",
      choices = c("No" = "no", "Yes" = "yes"),
      selected = input$ntid %||% "no",
      inline   = TRUE
    )
  })

  output$ui_design <- renderUI({
    if (identical(input$mode, "be")) {
      choices <- if (!identical(input$ntid, "yes"))
        c("parallel", "2x2", "2x2x3", "2x3x3", "2x2x4")
      else
        c("parallel", "2x2", "2x2x4")
    } else {
      choices <- c("parallel", "2x2")
    }
    # Генерируем радио со строго заданными значениями; без selected,
    # чтобы Shiny сам выбрал первый доступный и не мог появиться NA.
    radioButtons(
      inputId = "design",
      label   = "Design",
      choices = labelize(choices)
    )
  })

  output$ui_cv <- renderUI({
    # NI/NS: single CV by chosen endpoint
    if (identical(input$mode, "ni") || identical(input$mode, "ns")) {
      endp <- if (isTruthy(input$endp)) input$endp else "AUC"
      is_parallel <- identical(input$design, "parallel")
      label_prefix <- if (is_parallel) "CVinter" else "CVintra"
      numericInput(
        inputId = "cv_single",
        label   = paste0(label_prefix, " (", endp, ")"),
        value   = input$cv_single %||% 0.30,
        min     = 0,
        max     = 1,
        step    = 0.01
      )
    } else {
      # BE
      is_parallel <- identical(input$design, "parallel")
      is_scaleds  <- identical(input$design, "2x2x3") ||
                     identical(input$design, "2x3x3") ||
                     identical(input$design, "2x2x4")

      auc_label <- if (is_parallel) "CVinter (AUC)" else "CVintra (AUC)"
      cmax_label <- if (is_parallel) "CVinter (Cmax)" else "CVintra (Cmax)"
      if (is_scaleds) cmax_label <- "CVwR (Cmax)"

      tagList(
        numericInput(
          inputId = "cv_auc",
          label   = auc_label,
          value   = input$cv_auc %||% 0.30,
          min     = 0,
          max     = 1,
          step    = 0.01
        ),
        numericInput(
          inputId = "cv_cmax",
          label   = cmax_label,
          value   = input$cv_cmax %||% 0.30,
          min     = 0,
          max     = 1,
          step    = 0.01
        )
      )
    }
  })

  output$ui_limits <- renderUI({
    if (identical(input$mode, "be")) {
      # One radio control; text varies for replicative designs per spec
      if (identical(input$ntid, "yes")) {
        if (identical(input$design, "2x2") || identical(input$design, "parallel")) {
          radioButtons("limits", "BE limits", choices = c("90–111.11%"), selected = "90–111.11%")
        } else {
          radioButtons("limits", "BE limits", choices = c("AUC: 90–111.11%, Cmax: FDA RSABE for NTID"), selected = "AUC: 90–111.11%, Cmax: FDA RSABE for NTID")
        }
      } else {
        if (identical(input$design, "2x2") || identical(input$design, "parallel")) {
          radioButtons("limits", "BE limits", choices = c("80–125%"), selected = "80–125%")
        } else {
          radioButtons("limits", "BE limits", choices = c("AUC: 80–125%, Cmax: scaled by CV"), selected = "AUC: 80–125%, Cmax: scaled by CV")
        }
      }
    } else {
      if (identical(input$mode, "ni")) {
        radioButtons("margin", "Margin", choices = c("80%"), selected = "80%")
      } else if (identical(input$mode, "ns")) {
        radioButtons("margin", "Margin", choices = c("125%"), selected = "125%")
      } else NULL
    }
  })

  output$ui_theta0 <- renderUI({
    if (identical(input$mode, "ns")) {
      numericInput("theta0", "T/R (θ₀)", value = 1.05, min = 1.05, max = 1.05, step = 0.05)
    } else if (identical(input$mode, "ni")) {
      numericInput("theta0", "T/R (θ₀)", value = 0.95, min = 0.95, max = 0.95, step = 0.05)
    } else {
      # BE: one control; for NTID + non-2x2/parallel add explanatory help text
      help <- NULL
      if (identical(input$ntid, "yes") && !(identical(input$design, "2x2") || identical(input$design, "parallel"))) {
        help <- helpText("AUC uses 0.95 (0.90–0.95); Cmax narrows per FDA RSABE for NTID")
      }
      tagList(
        numericInput("theta0", "T/R (θ₀)", value = 0.95, min = 0.90, max = 0.95, step = 0.05),
        help
      )
    }
  })

  output$ui_alpha <- renderUI({
    if (identical(input$mode, "be")) {
      radioButtons("alpha", "Alpha", choices = c("0.05"),  selected = "0.05",  inline = TRUE)
    } else if (identical(input$mode, "ni") || identical(input$mode, "ns")) {
      radioButtons("alpha", "Alpha", choices = c("0.025"), selected = "0.025", inline = TRUE)
    } else NULL
  })
}

shinyApp(ui, server)
