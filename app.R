library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(shinyWidgets)
library(PowerTOST)

source("css.R")
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- UI -----------------------------------------------------------------------

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
      "Goodbye guesswork, hello regulator-friendly designs"
    )
  ),
  fillable = TRUE,
  header = tagList(
    shinyjs::useShinyjs(),
    tags$script(HTML(copy_js)),
    navset_card_tab(
      id = "main_navigation",

      # --- Bioequivalence -----------------------------------------------------

      nav_panel(
        title = "Bioequivalence",
        value = "be",
        layout_columns(
          col_widths = c(3, 9),
          class = "layout-columns mt-3",

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
                    value = 0.30, min = 0, max = 1, step = 0.01, width = "100%"
                  )
                ),
                conditionalPanel(
                  condition = "!(input.be_mode == 'ni' || input.be_mode == 'ns')",
                  layout_columns(
                    col_widths = c(6, 6),
                    numericInput(
                      inputId = "cv_auc",
                      label   = "placeholder",
                      value = 0.30, min = 0, max = 1, step = 0.01, width = "100%"
                    ),
                    numericInput(
                      inputId = "cv_cmax",
                      label   = "placeholder",
                      value = 0.30, min = 0, max = 1, step = 0.01, width = "100%"
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

          tagList(
            div(
              class = "summary-wrap",
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
            ),
            div(
              class = "mt-0",
              bslib::accordion(
                id = "accordion_output",
                bslib::accordion_panel(
                  "Details (PowerTOST log)",
                  uiOutput("results_details")
                )
              )
            )
          )
        )
      ),

      # --- Phase I ------------------------------------------------------------

      nav_panel(
        title = "Phase I",
        value = "p1",
        layout_columns(
          col_widths = c(3, 9),
          class = "layout-columns mt-3",

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
          class = "layout-columns mt-3",

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
      "2x3x3"  = "2×3×3",
      "2x2x3"  = "2×2×3",
      "2x2x4"  = "2×2×4"
    )
    setNames(keys, design_labels[keys])
  }
  is_replicative <- function(d) isTRUE(d %in% c("2x2x3", "2x3x3", "2x2x4"))
  mode           <- reactive(input$be_mode %||% "be")

  # --- Settings renderUI ------------------------------------------------------

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
    is_be    <- identical(mode(), "be")
    is_adapt <- identical(mode(), "be_adapt")
    
    select   <- if (is_adapt) "2x2" else "parallel"
    
    if (is_be) {
      basic <- c("parallel", "2x2")
      repl  <- if (ntid) c("2x2x4") else c("2x3x3", "2x2x3", "2x2x4")
      
      rb <- radioButtons(
        inputId = "design", label = "Design",
        choices = setNames(character(0), character(0)),
        selected = select
      )
      
      rb |>
        tagAppendChildren(
          div(
            style = "display:flex; gap:1.5rem;",
            div(
              style = "display:flex; flex-direction:column; width:40%;",
              lapply(
                basic,
                function(x) tags$label(
                  class = "radio",
                  tags$input(
                    type = "radio", name = "design", value = x,
                    checked = if (x == select) TRUE else NULL
                  ),
                  labelize(x)[[1]]
                )
              )
            ),
            div(
              style = "display:flex; flex-direction:column; width:60%;",
              lapply(
                repl,
                function(x) tags$label(
                  class = "radio",
                  tags$input(
                    type = "radio", name = "design", value = x
                  ),
                  labelize(x)[[1]]
                )
              )
            )
          )
        )
    } else {
      choices <- if (is_adapt) "2x2" else c("parallel", "2x2")

      radioButtons(
        inputId  = "design",
        label    = "Design",
        choices  = labelize(choices),
        selected = select
      )
    }
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

  output$ui_theta0 <- renderUI({
    ntidr  <- identical(input$ntid, "yes") && identical(input$design, "2x2x4")
    lab_be <- if (ntidr) "AUC T/R (θ₀) (using 0.975 for Cmax)" else "T/R (θ₀)"
    switch(
      mode(),
      ns = numericInput("theta0", "T/R (θ₀)", 1.05, 1.05, 1.05, 0.01, width = "100%"),
      ni = numericInput("theta0", "T/R (θ₀)", 0.95, 0.95, 0.95, 0.01, width = "100%"),
      numericInput(     "theta0", lab_be,     0.95, 0.90, 0.95, 0.01, width = "100%")
    )
  })

  output$ui_power <- renderUI({
    val <- if (identical(mode(), "be_adapt")) 0.90 else 0.80
    sliderInput(
      inputId = "power",
      label   = "Power",
      value   = val, min = 0.80, max = 0.95, step = 0.05,
      ticks   = TRUE, width = "100%" 
    )
  })

  output$ui_dropout <- renderUI({
    if (mode() %in% c("be", "ni", "ns")) {
      numericInput(
        inputId = "drop",
        label   = "Dropout, %",
        value   = 0, min = 0, max = 100, step = 1,
        width   = "100%"
      )
    } else {
      NULL
    }
  })

  # --- Calculate / Reset button -----------------------------------------------

  calc_results <- reactiveVal(NULL)

  observeEvent(input$calc_btn, {
    calc_results(safe_compute(compute_result))
  })

  observeEvent(input$reset_btn, {
    calc_results(NULL)
    shinyjs::reset("be_form")
  })

  # change tabs BE/p1/p2/about
  observeEvent(input$module, {
    calc_results(NULL)
  }, ignoreInit = TRUE)


  # change tabs on BE: equivalence/adaptive/NI/NS
  observeEvent(input$be_mode, {
    calc_results(NULL)
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

  add_be_notes <- function(notes, is_repl, is_ntid, cv_cmax, use_scABEL) {
    if (!is_repl && cv_cmax > 0.30 && !is_ntid) {
      notes <- c(
        notes,
        paste0(
          "For highly variable Cmax (CV > 30%), a replicate design is recommended ",
          "to allow scaling of BE limits ",
          "<a href='https://www.ema.europa.eu/en/investigation-bioequivalence-",
          "scientific-guideline' target='_blank'>(EMA)</a>."
        )
      )
    }

    if (use_scABEL) {
      notes <- c(
        notes,
        paste0(
          "Cmax sample size is based on scABEL (scaled limits). ",
          "If this endpoint determines the final sample size, note that ",
          "<a href='https://www.ema.europa.eu/en/investigation-bioequivalence-",
          "scientific-guideline' target='_blank'>EMA</a> also requires the point ",
          "estimate (GMR) to lie within [0.80; 1.25]."
        )
      )
    }

    if (is_ntid && is_repl) {
      notes <- c(
        notes,
        paste0(
          "Cmax sample size is based on RSABE with fixed parameters. ",
          "If this endpoint determines the final sample size, note that ",
          "<a href='https://www.fda.gov/regulatory-information/search-fda-guidance-",
          "documents' target='_blank'>FDA</a> also requires passing unscaled ABE ",
          "(80.00–125.00%) and that the upper limit of the 90% confidence interval ",
          "for \u03c3wT/\u03c3wR \u2264 2.5."
        )
      )
    }

    notes
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
    dropout   <- input$drop

    margin  <- if (identical(c_mode, "ni")) 0.80 else 1.25
    theta1  <- if (is_ntid) 0.90 else 0.80
    alpha  <- switch(
      c_mode,
      "be_adapt" = 0.0294,
      "ni"       = 0.025,
      "ns"       = 0.025,
      0.05
    )
    notes <- character(0)

    if (identical(c_mode, "ni") || identical(c_mode, "ns")) {
      endp <- input$endp
      res  <- run_noninf(alpha, CV = cv_single, design, power, theta0, margin)
      n_total <- res$n
      methods <- data.frame(
        Endpoint = endp,
        Method   = res$method,
        CV       = cv_single,
        N        = res$n
      )
      details <- setNames(list(res$txt), endp)
    } else {
      # always TOST for AUC
      auc <- run_TOST(alpha, CV = cv_auc, design, power, theta0, theta1)

      # different methods for Cmax
      is_repl <- is_replicative(design)

      use_scABEL <- is_repl && !is_ntid && (cv_cmax > 0.30)
      use_NTID   <- is_repl && is_ntid

      cmax_fn <- if (use_NTID) {
        run_NTID
      } else if (use_scABEL) {
        run_scABEL
      } else {
        run_TOST
      }

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
      details <- list(auc = auc$txt, cmax = cmax$txt)
      notes <- add_be_notes(notes, is_repl, is_ntid, cv_cmax, use_scABEL)
    }

    out <- list(
      n_total     = n_total,
      methods     = methods,
      details     = details,
      design_used = design,
      mode_used   = c_mode,
      power_used  = power,
      theta0_used = theta0,
      notes       = notes
    )

    if (identical(c_mode, "be_adapt")) {
      out$addon <- max(0L, out$n_total - (input$n1))
    } else {
      if (!is.null(dropout) && dropout > 0) {
        out$n_total <- ceiling(out$n_total / (1 - dropout/100))
        out$drop_used   = dropout
      }
    }

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

  build_mini_alloc_table <- function(df) {
    hdr <- tags$tr(lapply(names(df), tags$th))
    rows <- lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(seq_along(df), function(j) {
        cls <- if (is.numeric(df[[j]])) "text-end" else NULL
        tags$td(class = cls, df[i, j, drop = TRUE])
      }))
    })
    tags$table(class = "table table-sm", tags$thead(hdr), tags$tbody(rows))
  }

  build_endpoint_card <- function(ep, cv, meth, n){
    tags$div(
      class = "endpoint-card",
      tags$div(
        class = "endpoint-line",
        sprintf("%s (CV %.2f)", ep, cv)
      ),
      tags$div(
        class = "endpoint-line",
        sprintf("%s: N =  %s", meth, n)
      )
    )
  }

  build_endpoint_comparison <- function(res, n1 = NULL){
    m <- res$methods
    core <- if (nrow(m) >= 2){
      op <- if (m$N[1] < m$N[2]) "<" else if (m$N[1] > m$N[2]) ">" else "="
      tags$div(
        class = "compare-center",
        build_endpoint_card(m$Endpoint[1], m$CV[1], m$Method[1], m$N[1]),
        tags$div(class = "operator-badge", op),
        build_endpoint_card(m$Endpoint[2], m$CV[2], m$Method[2], m$N[2])
      )
    } else {
      tags$div(
        class = "compare-center",
        build_endpoint_card(m$Endpoint[1], m$CV[1], m$Method[1], m$N[1])
      )
    }

    if (identical(res$mode_used, "be_adapt") && !is.null(n1)) {
      form <- tags$div(
        class = "formula-row",
        tags$span(class="formula-chip", paste0("Stage 1: ", n1)),
        tags$span(class="operator-badge", "\u2794"),
        tags$span(class="formula-chip", paste0("+ Additional: ", res$addon)),
        tags$span(class="operator-badge", "="),
        tags$span(class="formula-chip", paste0("Total: ", res$n_total))
      )
      tags$div(core, form)
    } else {
      core
    }
  }

  make_results_ui <- function(res, design) {
    method_info <- build_endpoint_comparison(res, n1 = input$n1)

    params_stack <- tags$div(
      class = "methods-grid",
      tags$div(
        class = "parameters-stack",
        tags$span(class = "metric", paste("Design:", design)),
        tags$span(class = "metric", paste0("θ₀: ", res$theta0_used)),
        tags$span(
          class = "metric",
          paste0("Power: ", round((res$power_used) * 100), "%")
        ),
        if (!is.null(res$drop_used))
          tags$span(class = "metric", paste0("Dropout: ", res$drop_used, "%"))
      ),
      tags$div(class = "compare-cell", method_info)
    )

    calculator_box <- {
      cap      <- if (identical(design, "parallel")) "Arm" else "Sequence"
      is_adapt <- identical(res$mode_used, "be_adapt")
      lbl      <- if (is_adapt) "To recruit" else "Total N"
      val      <- if (is_adapt) res$addon    else res$n_total
      alloc_for_box <- alloc_table(val, design)

      bslib::value_box(
        title = NULL,
        showcase = bsicons::bs_icon("calculator"),
        value = tags$div(
          class = "valuebox-grid",
          tags$div(
            class = "valuebox-left",
            tags$div(class = "sample-size-label", lbl),
            tags$div(class = "sample-size-value", val)
          ),
          tags$div(
            class = "mini-alloc-table",
            build_mini_alloc_table(setNames(alloc_for_box, c(cap, "n")))
          )
        )
      )
    }
    layout_columns(col_widths = c(8, 4), params_stack, calculator_box)
  }

  render_results_for <- function(tab) {
    renderUI({
      r <- calc_results()
      if (is.null(r))
        return(p(
          "Select settings and press Calculate",
          style = "text-align:center; margin:2rem;"
        ))
      if (!is.null(r$error)) return(tags$pre(r$error))
      ui <- make_results_ui(r, r$design_used)

      notes <- if (length(r$notes) > 0) {
        div(
          class = "results-notes",
          tags$strong("Note:"),
          lapply(r$notes, function(x) HTML(x))
        )
      }
      tagList(ui, notes)
    })
  }

  output$results_be       <- render_results_for("be")
  output$results_be_adapt <- render_results_for("be_adapt")
  output$results_ni       <- render_results_for("ni")
  output$results_ns       <- render_results_for("ns")

  output$results_details <- renderUI({
    r <- calc_results()
    if (is.null(r)) return(p(
      "Select settings and press Calculate",
      style = "text-align:center; margin:2rem;"
    ))

    if (length(r$details) == 2) {
      div(
        class = "details-columns",
        div(
          tags$strong("AUC"),
          div(
            class = "codebox",
            actionLink(
              "copy_auc", NULL, class = "copy-btn",
              icon = icon("copy"), title = "Copy",
              onclick = "copyResultsToClipboard('log_auc','copy_auc'); return false;"
            ),
            tags$pre(id = "log_auc", class = "pre-scroll", r$details$auc)
          )
        ),
        div(
          tags$strong("Cmax"),
          div(
            class = "codebox",
            actionLink(
              "copy_cmax", NULL, class = "copy-btn",
              icon = icon("copy"), title = "Copy",
              onclick = "copyResultsToClipboard('log_cmax','copy_cmax'); return false;"
            ),
            tags$pre(id = "log_cmax", class = "pre-scroll", r$details$cmax)
          )
        )
      )
    } else {
      div(
        tags$strong(names(r$details)[1]),
        div(
          class = "codebox",
          actionLink(
            "copy_single", NULL, class = "copy-btn",
            icon = icon("copy"), title = "Copy",
            onclick = "copyResultsToClipboard('log_single','copy_single'); return false;"
          ),
          tags$pre(id = "log_single", class = "pre-scroll", r$details[[1]])
        )
      )
    }
  })
}

shinyApp(ui, server)
