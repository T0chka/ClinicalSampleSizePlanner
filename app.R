library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(shinyWidgets)
library(PowerTOST)

source("css.R")
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
            
            div(
              id = "be_form",
              uiOutput(outputId = "ui_mode_specific"),
              uiOutput(outputId = "ui_design"),
              
              div(
                id = "cv_block",
                conditionalPanel(
                  condition = "input.be_mode == 'ni' || input.be_mode == 'ns'",
                  numericInput(
                    inputId = "cv_single",
                    label   = "placeholder",
                    value   = 0.30, min = 0, max = 1, step = 0.01, width = "100%"
                  )
                ),
                conditionalPanel(
                  condition = "!(input.be_mode == 'ni' || input.be_mode == 'ns')",
                  layout_columns(
                    col_widths = c(6, 6),
                    numericInput(
                      inputId = "cv_auc",
                      label   = "placeholder",
                      value   = 0.30, min = 0, max = 1, step = 0.01, width = "100%"
                    ),
                    numericInput(
                      inputId = "cv_cmax",
                      label   = "placeholder",
                      value   = 0.30, min = 0, max = 1, step = 0.01, width = "100%"
                    )
                  )
                )
              ),
              
              uiOutput(outputId = "ui_theta0"),
              uiOutput("ui_power"),
              uiOutput("ui_dropout"),
              uiOutput(outputId = "ui_side_dynamic"),
              layout_columns(
                col_widths = c(6, 6),
                actionButton(
                  inputId = "calc_btn",
                  label   = "Calculate",
                  class   = "btn btn-primary w-100"
                ),
                actionButton(
                  inputId = "reset_btn",
                  label   = "Reset",
                  class   = "btn btn-secondary w-100"
                )
              )
            )
          ),
          
          navset_card_tab(
            id = "be_mode",
            nav_panel(
              title = "Equivalence",
              value = "be",
              uiOutput("results_be")
            ),
            nav_panel(
              title = "Adaptive (Potvin B)",
              value = "be_adapt",
              uiOutput("results_be_adapt")
            ),
            nav_panel(
              title = "Non-inferiority",
              value = "ni",
              uiOutput("results_ni")
            ),
            nav_panel(
              title = "Non-superiority",
              value = "ns",
              uiOutput("results_ns")
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
            
            div(
              id = "p1_form",
              p("Coming Soon", style = "text-align: center; margin: 2rem; color: #6c757d;")
            )
          ),
          
          navset_card_tab(
            id = "p1_mode",
            nav_panel(
              title = "Results",
              value = "p1_res",
              p("Coming Soon", style = "text-align: center; margin: 2rem; color: #6c757d;")
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
            
            div(
              id = "p2_form",
              p("Coming Soon", style = "text-align: center; margin: 2rem; color: #6c757d;")
            )
          ),
          
          navset_card_tab(
            id = "p2_mode",
            nav_panel(
              title = "Results",
              value = "p2_res",
              p("Coming Soon", style = "text-align: center; margin: 2rem; color: #6c757d;")
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
            p("Sample size planning toolkit.")
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
  labelize <- function(keys) {
    design_labels <- c(
      parallel = "parallel",
      "2x2"    = "2×2",
      "2x2x3"  = "2×2×3",
      "2x3x3"  = "2×3×3",
      "2x2x4"  = "2×2×4"
    )
    setNames(keys, design_labels[keys])
  }
  is_replicative <- function(d) isTRUE(d %in% c("2x2x3", "2x3x3", "2x2x4"))
  mode           <- reactive(input$be_mode %||% "be")
  
  # --- Settings renderUI -----
  
  # Endpoint selection only for NI/NS per spec; hidden for BE
  output$ui_mode_specific <- renderUI({
    switch(
      mode(),
      "be" = radioButtons(
        inputId  = "ntid",
        label    = "Narrow Therapeutic Index Drug?",
        choices  = c("No" = "no", "Yes" = "yes"),
        selected = "no",
        inline   = TRUE
      ),
      "be_adapt" = numericInput(
        inputId = "n1",
        label   = "N at first stage",
        value   = 12, min = 12, max = 100, step = 1,
        width = "100%"
      ),
      "ni" = radioButtons(
        inputId  = "endp",
        label    = "Endpoint",
        choices  = c("AUC", "Cmax"),
        selected = "AUC",
        inline   = TRUE
      ),
      "ns" = radioButtons(
        inputId  = "endp",
        label    = "Endpoint",
        choices  = c("AUC", "Cmax"),
        selected = "AUC",
        inline   = TRUE
      ),
      NULL
    )
  })

  output$ui_design <- renderUI({
    ntid     <- identical(input$ntid, "yes")
    adaptive <- identical(mode(), "be_adapt")
    
    choices <- switch(
      mode(),
      "be" = 
        if (ntid)  c("parallel", "2x2", "2x2x4")
        else       c("parallel", "2x2", "2x2x3", "2x3x3", "2x2x4"),
      "ni" =       c("parallel", "2x2"),
      "ns" =       c("parallel", "2x2"),
      "be_adapt" = c("2x2"),
      c("parallel", "2x2") # fallback
    )
    radioButtons(
      inputId  = "design",
      label    = "Design",
      choices  = labelize(choices),
      selected = if (adaptive) "2x2" else "parallel"
    )
  })

  observe({
    is_parallel <- identical(input$design, "parallel")
    is_repl     <- is_replicative(input$design)
    lab_prefix  <- if (is_parallel) "CVinter" else "CVintra"
    cmax_prefix <- if (is_repl) "CVwR" else lab_prefix
    
    if (mode() %in% c("ni","ns")) {
      endp <- input$endp %||% "AUC"
      updateNumericInput(
        session, "cv_single", label = paste0(lab_prefix, " (", endp, ")")
      )
    } else {
      updateNumericInput(
        session, "cv_auc", label = paste0(lab_prefix, " (AUC)")
      )
      updateNumericInput(
        session, "cv_cmax", label = paste0(cmax_prefix, " (Cmax)")
      )
    }
  })
  
  # output$ui_cv <- renderUI({
  #   single_cv   <- identical(mode(), "ni") || identical(mode(), "ns")
  #   is_parallel <- identical(input$design, "parallel")
  #   is_repl     <- is_replicative(input$design)
  #   label_prefix     <- if (is_parallel) "CVinter" else "CVintra"
  #   repl_cmax_prefix <- if (is_repl)     "CVwR"    else label_prefix
  #   
  #   if (single_cv) {
  #     endp <- input$endp %||% "AUC"
  #     numericInput(
  #       inputId = "cv_single",
  #       label   = paste(label_prefix, "(", endp, ")"),
  #       value   = isolate(input$cv_single) %||% 0.30,
  #       min = 0, max = 1, step = 0.01, width = "100%"
  #     )
  #   } else {
  #     layout_columns(
  #       col_widths = c(6, 6),
  #       numericInput(
  #         inputId = "cv_auc",
  #         label   = paste(label_prefix, "(AUC)"),
  #         value   = isolate(input$cv_auc) %||% 0.30,
  #         min = 0, max = 1, step = 0.01, width = "100%"
  #       ),
  #       numericInput(
  #         inputId = "cv_cmax",
  #         label   = paste(repl_cmax_prefix, "(Cmax)"),
  #         value   = isolate(input$cv_cmax) %||% 0.30,
  #         min = 0, max = 1, step = 0.01, width = "100%"
  #       )
  #     )
  #   }
  # })

  output$ui_theta0 <- renderUI({
    ntidr  <- identical(input$ntid, "yes") && identical(input$design, "2x2x4")
    lab_be <- if (ntidr) "AUC T/R (θ₀) (using 0.975 for Cmax)" else "T/R (θ₀)"
    switch(
      mode(),
      ns = numericInput("theta0", "T/R (θ₀)", 1.05, 1.05, 1.05, 0.01),
      ni = numericInput("theta0", "T/R (θ₀)", 0.95, 0.95, 0.95, 0.01),
      numericInput(     "theta0", lab_be,     0.95, 0.90, 0.95, 0.01)
    )
  })
  
  output$ui_power <- renderUI({
    val <- if (identical(mode(), "be_adapt")) 0.90 else 0.80
    sliderInput(
      inputId = "power",
      label   = "Power",
      value   = val, min = 0.80, max = 0.95, step = 0.05,
      ticks   = TRUE
    )
  })
  
  output$ui_dropout <- renderUI({
    if (mode() %in% c("be", "ni", "ns")) {
      numericInput(
        inputId = "drop",
        label   = "Dropout, %",
        value   = 10, min = 0, max = 100, step = 1,
        width   = "100%"
      )
    } else {
      NULL
    }
  })
  
  # --- Calculate / Reset button -----

  res_val <- reactiveVal(NULL)
  
  observeEvent(input$calc_btn, {
    res_val(safe_compute(compute_result))
  })
  
  observeEvent(input$reset_btn, {
    res_val(NULL)
    shinyjs::reset("be_form")
  })
  
  # change tabs BE/p1/p2/about
  observeEvent(input$module, {
    res_val(NULL)
    shinyjs::reset("be_form")
  }, ignoreInit = TRUE)
  
  
  # change tabs on BE: equivalence/adaptive/NI/NS
  observeEvent(input$be_mode, {
    res_val(NULL)
    shinyjs::reset("be_form")
  }, ignoreInit = TRUE)

  # --- Calculations -----------------------------------------------------------
  
  # TOST-version specific expraction
  extract_n <- function(res) {
    n <- suppressWarnings(as.integer(res[["Sample size"]]))
    if (!is.na(n)) return(n)
    n <- suppressWarnings(as.integer(res[["n"]]))
    if (!is.na(n)) return(n)
    NA_integer_
  }
  safe_compute <- function(fun) {
    tryCatch(fun(), error = function(e) list(error = conditionMessage(e)))
  }

  run_TOST <- function(alpha, CV, design, power, theta0, theta1) {
    txt <- paste(capture.output({
      r <- PowerTOST::sampleN.TOST(
        alpha = alpha, CV = CV, design = design, targetpower = power,
        theta0   = theta0,
        theta1   = theta1,
        theta2   = 1 / theta1,
        logscale = TRUE,
        method   = "exact",
        robust   = FALSE,
        imax     = 100,
        details  = TRUE,
        print    = TRUE
      )
    }), collapse = "\n")
    list(n = extract_n(r), txt = txt, method = "TOST")
  }
  
  run_scABEL <- function(alpha, CV, design, power, theta0, theta1) {
    txt <- paste(capture.output({
      r <- PowerTOST::sampleN.scABEL(
        alpha = alpha, CV = CV, design = design, targetpower = power,
        theta0    = theta0, 
        theta1    = theta1,
        theta2    = 1 / theta1,
        regulator = "EMA",
        imax      = 100, 
        details   = TRUE, 
        print     = TRUE, 
        setseed   = TRUE
      )
    }), collapse = "\n")
    list(n = extract_n(r), txt = txt, method = "scABEL (EMA)")
  }
  
  run_NTID <- function(alpha, CV, design, power, theta0, theta1) {
    txt <- paste(capture.output({
      r <- PowerTOST::sampleN.NTID(
        alpha = alpha, CV = CV, design = design, targetpower = power, 
        theta0  = theta0,
        theta1  = theta1,
        theta2  = 1 / theta1,
        imax    = 100, 
        details = TRUE, 
        print   = TRUE, 
        setseed = TRUE
      )
    }), collapse = "\n")
    list(n = extract_n(r), txt = txt, method = "NTID (RSABE)")
  }
  
  run_noninf <- function(alpha, CV, design, power, theta0, margin) {
    txt <- paste(capture.output({
      r <- PowerTOST::sampleN.noninf(
        alpha = alpha, CV = CV, design = design, targetpower = power,
        theta0   = theta0,
        margin   = margin,
        logscale = TRUE,
        robust   = FALSE,
        imax     = 100,
        details  = TRUE,
        print    = TRUE
      )
    }), collapse = "\n")
    meth <- if (isTRUE(margin < 1)) "Non-inferiority" else "Non-superiority"
    list(n = extract_n(r), txt = txt, method = meth)
  }
  
  compute_result <- function() {
    c_mode    <- mode()
    is_ntid   <- identical(input$ntid, "yes")
    design    <- input$design
    power     <- input$power
    cv_auc    <- input$cv_auc
    cv_cmax   <- input$cv_cmax
    cv_single <- input$cv_single
    theta0    <- input$theta0
    
    margin  <- if (identical(c_mode, "ni")) 0.80 else 1.25
    theta1  <- if (is_ntid) 0.90 else 0.80
    alpha  <- switch(
      c_mode,
      "be_adapt" = 0.0294,
      "ni"       = 0.025,
      "ns"       = 0.025,
      0.05
    )
    
    if (identical(c_mode, "ni") || identical(c_mode, "ns")) {
      res <- run_noninf(alpha, CV = cv_single, design, power, theta0, margin)
      methods <- data.frame(
        Endpoint = input$endp,
        Method   = res$method,
        CV       = cv_single,
        N        = res$n
      )
      out <- list(
        n_total = res$n,
        methods = methods,
        details = res$txt,
        design_used = design
      )
      return(out)
    } 
    
    # always TOST for AUC
    auc  <- run_TOST(alpha, CV = cv_auc, design, power, theta0, theta1)
    
    # different methods for Cmax
    is_repl <- is_replicative(design)
    cmax_fn <- if (!is_repl) run_TOST else if (is_ntid) run_NTID else run_scABEL
    cmax_args <- list(
      alpha  = alpha,
      CV     = cv_cmax,
      design = design,
      power  = power,
      theta0 = if (is_repl && is_ntid) 0.975 else theta0,
      theta1 = if (!is_repl) theta1 else 0.80
    )
    cmax <- do.call(cmax_fn, cmax_args)
    
    n_total <- max(auc$n, cmax$n, na.rm = TRUE)
    methods <- data.frame(
      Endpoint = c("AUC", "Cmax"),
      Method   = c(auc$method, cmax$method),
      CV       = c(cv_auc, cv_cmax),
      N        = c(auc$n, cmax$n)
    )
    out <- list(
      n_total = n_total,
      methods = methods,
      details = paste(auc$txt, cmax$txt, sep = "\n\n"),
      design_used = design
    )
    if (identical(c_mode, "be_adapt")) out$addon <- max(0L, n_total - (input$n1))
    return(out)
  }
  
  # --- Bind outputs -----------------------------------------------------------
  
  seq_map <- list(
    parallel = c("T", "R"),
    "2x2"    = c("TR", "RT"),
    "2x2x3"  = c("TRR", "RTR"),
    "2x3x3"  = c("TRR", "RTR", "RRT"),
    "2x2x4"  = c("TRTR", "RTRT")
  )
  
  alloc_table <- function(n_total, design) {
    s <- seq_map[[design]] %||% c("T", "R")
    k <- length(s)
    base <- rep.int(n_total %/% k, k)
    rem  <- n_total - sum(base)
    if (rem > 0) base[seq_len(rem)] <- base[seq_len(rem)] + 1L
    nm <- if (identical(design, "parallel")) "Arm" else "Sequence"
    df <- data.frame(name = s, n = base, check.names = FALSE)
    names(df)[1] <- nm
    df
  }
  
  table_tag <- function(df) {
    hdr <- tags$tr(lapply(names(df), tags$th))
    rows <- apply(df, 1, function(x) tags$tr(lapply(as.list(x), tags$td)))
    tags$table(class = "table table-sm", tags$thead(hdr), tags$tbody(rows))
  }
  
  make_results_ui <- function(res, design) {
    alloc_df <- alloc_table(res$n_total, design)
    cap <- if (identical(design, "parallel"))
      "Allocation by arm" else "Allocation by sequence"
    
    right_boxes <- tagList(
      bslib::value_box(
        title = "Total N",
        value = tags$span(res$n_total),
        showcase = bsicons::bs_icon("calculator")
      ),
      if (!is.null(res$addon)) bslib::value_box(
        title = "Add-on N",
        value = tags$span(res$addon),
        showcase = bsicons::bs_icon("plus")
      )
    )
    
    left_side <- tagList(
      tags$h4(cap),
      table_tag(alloc_df)
    )
    
    tagList(
      layout_columns(col_widths = c(8, 4), left_side, right_boxes),
      tags$h4("By endpoint"),
      table_tag(res$methods),
      tags$h4("Details"),
      tags$pre(res$details)
    )
  }
  
  render_results_for <- function(tab) renderUI({
    r <- res_val()
    if (is.null(r))
      return(p("Select settings and press Calculate",
               style = "text-align:center; margin:2rem;"))
    if (!is.null(r$error)) return(tags$pre(r$error))
    make_results_ui(r, r$design_used)
  })
  
  output$results_be       <- render_results_for("be")
  output$results_be_adapt <- render_results_for("be_adapt")
  output$results_ni       <- render_results_for("ni")
  output$results_ns       <- render_results_for("ns")
}

shinyApp(ui, server)
