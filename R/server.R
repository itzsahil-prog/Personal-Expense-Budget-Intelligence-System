# =============================================================================
# R/server.R
# Shiny server function for the Personal Expense & Budget Intelligence System.
#
# Architecture
# ------------
# One SQLite connection is opened per session (onSession start) and closed
# when the session ends (session$onSessionEnded).
#
# Reactive graph
# --------------
#   rv$data_version          — integer counter bumped after every write
#                              operation. All read-reactives invalidate on it.
#   all_transactions()       — full table, re-fetched on data_version change
#   filtered_transactions()  — Transactions-tab filtered slice
#   an_filtered_transactions() — Analytics-tab filtered slice
#
# All heavy computation (summaries, charts, forecasts, anomaly detection) is
# done inside reactive() or eventReactive() — never inside observe() or
# render*().
#
# Dependencies (must be sourced before this file in app.R):
#   validation.R, database.R, data_processing.R, analytics.R,
#   budget.R, anomaly_detection.R, prediction.R, reporting.R, ui_helpers.R
# =============================================================================

server <- function(input, output, session) {

  CURRENCY <- "\u20b9"

  # ===========================================================================
  # DATABASE CONNECTION — one per session
  # ===========================================================================
  con <- tryCatch(
    {
      c <- connect_database()
      initialize_database(c)
      c
    },
    error = function(e) {
      # Log the technical detail; surface a clean message in the UI
      message(sprintf("[ERROR] Database startup failed: %s", conditionMessage(e)))
      NULL
    }
  )

  # Guard every DB-dependent output when the connection could not be opened
  .db_ok <- function() {
    if (is.null(con) || !DBI::dbIsValid(con)) {
      showNotification(
        "Database connection is unavailable. Please restart the application.",
        type     = "error",
        duration = NULL
      )
      return(FALSE)
    }
    TRUE
  }

  session$onSessionEnded(function() {
    disconnect_database(con)
  })

  # ===========================================================================
  # GLOBAL REACTIVE STATE
  # ===========================================================================
  rv <- reactiveValues(
    data_version     = 0L,   # bump to invalidate all_transactions()
    edit_txn_id      = NULL, # transaction_id currently being edited
    delete_txn_id    = NULL, # transaction_id staged for deletion
    import_result    = NULL, # result list from import_csv() / import_excel()
    forecast_result  = NULL, # result list from run_forecast()
    anomaly_result   = NULL, # result list from get_anomaly_summary()
    budget_month     = format(Sys.Date(), "%Y-%m")
  )

  # ===========================================================================
  # CORE DATA REACTIVES
  # ===========================================================================

  # Full transaction table — invalidated by any write.
  all_transactions <- reactive({
    rv$data_version  # take dependency
    if (is.null(con) || !DBI::dbIsValid(con)) {
      return(data.frame(
        transaction_id = character(0), date = character(0),
        category = character(0), description = character(0),
        amount = numeric(0), payment_method = character(0),
        stringsAsFactors = FALSE
      ))
    }
    tryCatch(
      get_transactions(con),
      error = function(e) {
        message(sprintf("[ERROR] get_transactions failed: %s", conditionMessage(e)))
        data.frame(
          transaction_id = character(0), date = character(0),
          category = character(0), description = character(0),
          amount = numeric(0), payment_method = character(0),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  # Transactions-tab filtered slice.
  # Rebuilt when filters are applied or data changes.
  txn_filter_trigger <- reactiveVal(0L)

  filtered_transactions <- reactive({
    txn_filter_trigger()
    rv$data_version

    date_from <- isolate(input$txn_date_from)
    date_to   <- isolate(input$txn_date_to)
    cats      <- isolate(input$txn_categories)
    pm        <- isolate(input$txn_payment)
    srch      <- isolate(input$txn_search)

    get_transactions(
      con,
      date_from       = if (!is.null(date_from)) format(date_from) else NULL,
      date_to         = if (!is.null(date_to))   format(date_to)   else NULL,
      categories      = if (!is.null(cats) && cats != "All") cats else NULL,
      payment_methods = if (!is.null(pm)   && pm   != "All") pm   else NULL,
      search_text     = if (!is.null(srch) && nzchar(trimws(srch))) srch else NULL
    )
  })

  # Analytics-tab filtered slice.
  an_filter_trigger <- reactiveVal(0L)

  an_filtered_transactions <- reactive({
    an_filter_trigger()
    rv$data_version

    date_from <- isolate(input$an_date_from)
    date_to   <- isolate(input$an_date_to)
    cats      <- isolate(input$an_categories)
    pm        <- isolate(input$an_payment)

    get_transactions(
      con,
      date_from       = if (!is.null(date_from)) format(date_from) else NULL,
      date_to         = if (!is.null(date_to))   format(date_to)   else NULL,
      categories      = if (!is.null(cats) && cats != "All") cats else NULL,
      payment_methods = if (!is.null(pm)   && pm   != "All") pm   else NULL
    )
  })

  # ===========================================================================
  # FILTER BUTTON OBSERVERS (Transactions + Analytics)
  # ===========================================================================

  observeEvent(input$txn_apply,      { txn_filter_trigger(txn_filter_trigger() + 1L) })
  observeEvent(input$txn_search_btn, { txn_filter_trigger(txn_filter_trigger() + 1L) })

  observeEvent(input$txn_reset, {
    updateDateInput(session,   "txn_date_from", value = Sys.Date() - 365)
    updateDateInput(session,   "txn_date_to",   value = Sys.Date())
    updateSelectInput(session, "txn_categories", selected = "All")
    updateSelectInput(session, "txn_payment",    selected = "All")
    updateTextInput(session,   "txn_search",     value = "")
    txn_filter_trigger(txn_filter_trigger() + 1L)
  })

  observeEvent(input$an_apply, { an_filter_trigger(an_filter_trigger() + 1L) })

  observeEvent(input$an_reset, {
    updateDateInput(session,   "an_date_from", value = Sys.Date() - 365)
    updateDateInput(session,   "an_date_to",   value = Sys.Date())
    updateSelectInput(session, "an_categories", selected = "All")
    updateSelectInput(session, "an_payment",    selected = "All")
    an_filter_trigger(an_filter_trigger() + 1L)
  })

  # ===========================================================================
  # SECTION 1 — DASHBOARD
  # ===========================================================================

  dash_kpis <- reactive({
    compute_kpi_stats(all_transactions(), currency_symbol = CURRENCY)
  })

  dash_budget_status <- reactive({
    rv$data_version
    month_str  <- format(Sys.Date(), "%Y-%m")
    budget_df  <- get_budgets(con, month = month_str)
    calculate_budget_status(
      budget_df       = budget_df,
      transactions_df = all_transactions(),
      month           = month_str
    )
  })

  # KPI cards
  output$dash_kpi_row <- renderUI({
    k <- dash_kpis()
    fmt <- function(x) format_currency(x, CURRENCY)
    kpi_row(
      kpi_card("kpi_total",   "Total Spending",
               fmt(k$total_spending),          "wallet",      "primary"),
      kpi_card("kpi_month",   paste("This Month"),
               fmt(k$current_month_spending),  "calendar-alt","violet"),
      kpi_card("kpi_today",   "Today",
               fmt(k$today_spending),          "clock",       "emerald"),
      kpi_card("kpi_avg",     "Avg Daily",
               fmt(k$avg_daily_spending),      "chart-line",  "amber"),
      kpi_card("kpi_top_cat", "Top Category",
               k$highest_category,             "tag",         "rose",
               subtitle = fmt(k$highest_category_amount))
    )
  })

  # Dashboard hero stat values
  output$dash_hero_total <- renderUI({
    k <- dash_kpis()
    tags$div(class = "dash-hero-stat-value",
             format_currency(k$total_spending, CURRENCY))
  })
  output$dash_hero_month <- renderUI({
    k <- dash_kpis()
    tags$div(class = "dash-hero-stat-value",
             format_currency(k$current_month_spending, CURRENCY))
  })
  output$dash_hero_txn_count <- renderUI({
    k <- dash_kpis()
    tags$div(class = "dash-hero-stat-value",
             formatC(k$transaction_count, format = "d", big.mark = ","))
  })

  # Recent transactions widget (last 8)
  output$dash_recent_transactions <- renderUI({
    df <- all_transactions()
    if (is.null(df) || nrow(df) == 0L) {
      return(empty_state_ui(
        "No transactions yet",
        sub_message = "Add your first expense to start tracking your spending.",
        icon        = "receipt"
      ))
    }
    # Take the 8 most recent
    recent <- head(df[order(df$date, decreasing = TRUE), ], 8L)

    cat_colours <- CATEGORY_COLOURS

    items <- lapply(seq_len(nrow(recent)), function(i) {
      row    <- recent[i, , drop = FALSE]
      col    <- cat_colours[[row$category]]
      if (is.null(col)) col <- "#94A3B8"
      tags$li(
        class = "recent-txn-item",
        tags$div(
          class = "recent-txn-cat-icon",
          style = sprintf("background:%s22; color:%s;", col, col),
          tags$i(class = paste0("fa fa-", CATEGORY_ICONS[[row$category]] %||% "tag"),
                 `aria-hidden` = "true")
        ),
        tags$div(
          class = "recent-txn-body",
          tags$div(class = "recent-txn-desc",
                   if (nzchar(trimws(row$description))) row$description
                   else row$category),
          tags$div(class = "recent-txn-meta",
                   format(as.Date(row$date), "%d %b %Y"),
                   " \u00b7 ", row$category)
        ),
        tags$div(class = "recent-txn-amount negative",
                 format_currency(row$amount, CURRENCY))
      )
    })
    tags$ul(class = "recent-txn-list", tagList(items))
  })

  output$dash_budget_month_label <- renderUI({
    tags$span(class = "text-muted",
              format(Sys.Date(), "%B %Y"))
  })

  output$dash_budget_bars <- renderUI({
    bs <- dash_budget_status()
    if (is.null(bs) || nrow(bs) == 0L) {
      return(empty_state_ui(
        "No budgets set for this month.",
        sub_message = "Go to the Budgets tab to add category budgets.",
        icon        = "bullseye"
      ))
    }
    tagList(lapply(seq_len(nrow(bs)), function(i) {
      row <- bs[i, , drop = FALSE]
      budget_progress_bar(
        category        = row$category,
        used            = row$actual_spending,
        budget          = row$budget,
        currency_symbol = CURRENCY
      )
    }))
  })

  # Dashboard charts — last 30 days daily
  output$dash_plot_daily <- renderPlotly({
    df <- all_transactions()
    if (is.null(df) || nrow(df) == 0L) {
      return(plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = "No data") %>%
        layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    df <- df[as.Date(df$date) >= Sys.Date() - 30L, , drop = FALSE]
    daily <- compute_daily_summary(df)
    if (nrow(daily) == 0L) {
      return(plot_ly() %>% add_annotations(text = "No data in the last 30 days",
        showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))
    }
    plot_ly(daily, x = ~date, y = ~total_amount,
            type = "bar", marker = list(color = "#3498DB"),
            hovertemplate = paste0("<b>%{x}</b><br>",
                                   CURRENCY, " %{y:,.0f}<extra></extra>")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = paste0(CURRENCY)),
             margin = list(t = 10, b = 40),
             paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  # Dashboard chart — last 12 months monthly
  output$dash_plot_monthly <- renderPlotly({
    df <- all_transactions()
    monthly <- compute_monthly_summary(df)
    if (nrow(monthly) == 0L) {
      return(plot_ly() %>% add_annotations(text = "No data",
        showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))
    }
    monthly <- tail(monthly, 12)
    plot_ly(monthly, x = ~year_month, y = ~total_amount,
            type  = "scatter", mode = "lines+markers",
            line  = list(color = "#9B59B6", width = 2),
            marker = list(color = "#9B59B6"),
            hovertemplate = paste0("<b>%{x}</b><br>",
                                   CURRENCY, " %{y:,.0f}<extra></extra>")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = paste0(CURRENCY)),
             margin = list(t = 10, b = 40),
             paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  # Dashboard chart — category donut
  output$dash_plot_category <- renderPlotly({
    cat_df <- compute_category_summary(all_transactions())
    if (nrow(cat_df) == 0L) {
      return(plot_ly() %>% add_annotations(text = "No data",
        showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))
    }
    plot_ly(cat_df, labels = ~category, values = ~total_amount,
            type         = "pie",
            marker       = list(colors = cat_df$color_hex),
            textposition = "inside",
            textinfo     = "percent",
            hovertemplate = paste0(
              "<b>%{label}</b><br>", CURRENCY,
              " %{value:,.0f} (%{percent})<extra></extra>")) %>%
      layout(showlegend = TRUE,
             legend     = list(orientation = "v", x = 1, y = 0.5),
             margin     = list(t = 10, b = 10),
             paper_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  # Dashboard chart — payment bar
  output$dash_plot_payment <- renderPlotly({
    pay_df <- compute_payment_summary(all_transactions())
    if (nrow(pay_df) == 0L) {
      return(plot_ly() %>% add_annotations(text = "No data",
        showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))
    }
    plot_ly(pay_df, x = ~payment_method, y = ~total_amount,
            type   = "bar",
            marker = list(color = c("#3498DB","#2ECC71","#F39C12","#9B59B6")),
            hovertemplate = paste0("<b>%{x}</b><br>",
                                   CURRENCY, " %{y:,.0f}<extra></extra>")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = paste0(CURRENCY)),
             margin = list(t = 10, b = 40),
             paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  # ===========================================================================
  # SECTION 2 — TRANSACTIONS TABLE
  # ===========================================================================

  output$txn_count_badge <- renderUI({
    n <- nrow(filtered_transactions())
    tags$span(
      class = "badge badge-secondary",
      style = "line-height:2; padding: 4px 10px; font-size:13px;",
      sprintf("%d transaction%s", n, if (n == 1L) "" else "s")
    )
  })

  output$txn_table <- DT::renderDataTable({
    df <- filtered_transactions()
    if (nrow(df) == 0L) {
      return(DT::datatable(
        data.frame(Message = "No transactions match the current filters."),
        rownames = FALSE, options = list(dom = "t"),
        class = "display"
      ))
    }

    display_df <- df %>%
      dplyr::mutate(
        Date        = format(as.Date(date), "%d %b %Y"),
        Category    = category,
        Description = description,
        Amount      = sprintf("%s\u00a0%.2f", CURRENCY, as.numeric(amount)),
        Payment     = payment_method,
        Actions     = paste0(
          '<button class="dt-action-btn edit" ',
          'onclick="Shiny.setInputValue(\'txn_edit_id\',\'', transaction_id,
          '\', {priority:\'event\'})">',
          '<i class="fa fa-edit"></i> Edit</button> ',
          '<button class="dt-action-btn delete" ',
          'onclick="Shiny.setInputValue(\'txn_delete_id\',\'', transaction_id,
          '\', {priority:\'event\'})">',
          '<i class="fa fa-trash"></i> Delete</button>'
        )
      ) %>%
      dplyr::select(Date, Category, Description, Amount, Payment, Actions)

    DT::datatable(
      display_df,
      rownames  = FALSE,
      escape    = FALSE,
      class     = "display stripe hover compact",
      selection = "none",
      options   = list(
        pageLength = 15,
        dom        = "lfrtip",
        scrollX    = TRUE,
        columnDefs = list(
          list(orderable = FALSE, targets = 5),
          list(width = "35%",    targets = 2),
          list(className = "dt-right", targets = 3)
        ),
        language = list(
          search     = "Filter:",
          lengthMenu = "Show _MENU_ entries"
        )
      )
    )
  }, server = TRUE)

  # Edit button clicked in DT
  observeEvent(input$txn_edit_id, {
    req(input$txn_edit_id)
    row <- get_transaction_by_id(con, input$txn_edit_id)
    req(nrow(row) > 0L)
    rv$edit_txn_id <- input$txn_edit_id
    showModal(edit_transaction_modal(ns = identity, txn_row = row))
  })

  # Save edit
  observeEvent(input$btn_save_edit, {
    req(rv$edit_txn_id)

    result <- tryCatch(
      update_transaction(
        con,
        transaction_id = rv$edit_txn_id,
        date           = format(input$txn_date),
        category       = input$txn_category,
        description    = input$txn_description,
        amount         = input$txn_amount,
        payment_method = input$txn_payment
      ),
      error = function(e) conditionMessage(e)
    )

    if (isTRUE(result)) {
      removeModal()
      rv$data_version <- rv$data_version + 1L
      showNotification("Transaction updated.", type = "message", duration = 3)
    } else {
      output$edit_validation_msg <- renderUI({
        alert_box(as.character(result), type = "danger", dismissible = FALSE)
      })
    }
  })

  # Delete button clicked in DT
  observeEvent(input$txn_delete_id, {
    req(input$txn_delete_id)
    row <- get_transaction_by_id(con, input$txn_delete_id)
    req(nrow(row) > 0L)
    rv$delete_txn_id <- input$txn_delete_id
    showModal(delete_confirm_modal(ns = identity, txn_row = row,
                                   currency_symbol = CURRENCY))
  })

  # Confirm delete
  observeEvent(input$btn_confirm_delete, {
    req(rv$delete_txn_id)
    result <- tryCatch(
      delete_transaction(con, rv$delete_txn_id),
      error = function(e) conditionMessage(e)
    )
    removeModal()
    if (isTRUE(result)) {
      rv$data_version  <- rv$data_version + 1L
      rv$delete_txn_id <- NULL
      showNotification("Transaction deleted.", type = "warning", duration = 3)
    } else {
      showNotification(
        paste("Delete failed:", as.character(result)),
        type = "error", duration = 5
      )
    }
  })

  # Download filtered transactions
  output$txn_download_csv <- downloadHandler(
    filename = function() download_filename_transactions("csv"),
    content  = function(file) {
      path <- export_transactions_csv(filtered_transactions(),
                                       currency_symbol = CURRENCY)
      if (is.null(path)) {
        writeLines("No data to export.", file)
      } else {
        file.copy(path, file)
      }
    }
  )

  output$txn_download_excel <- downloadHandler(
    filename = function() download_filename_transactions("xlsx"),
    content  = function(file) {
      path <- export_transactions_excel(filtered_transactions())
      if (is.null(path)) {
        writexl::write_xlsx(
          list(Transactions = data.frame(Message = "No data")), path = file
        )
      } else {
        file.copy(path, file)
      }
    }
  )

  # ===========================================================================
  # SECTION 3 — ADD TRANSACTION
  # ===========================================================================

  # Clear the success / error messages when inputs change
  observe({
    input$add_date; input$add_category
    input$add_description; input$add_amount; input$add_payment
    output$add_validation_msg <- renderUI(NULL)
    output$add_success_msg    <- renderUI(NULL)
  })

  observeEvent(input$add_save_btn, {
    result <- tryCatch(
      insert_transaction(
        con,
        date           = format(input$add_date),
        category       = input$add_category,
        description    = input$add_description,
        amount         = input$add_amount,
        payment_method = input$add_payment
      ),
      error = function(e) conditionMessage(e)
    )

    if (is.character(result) && grepl("^txn_", result)) {
      # Success — result is the new transaction_id
      rv$data_version <- rv$data_version + 1L

      # Reset form
      updateDateInput(session,    "add_date",        value = Sys.Date())
      updateSelectInput(session,  "add_category",    selected = "Food")
      updateTextInput(session,    "add_description", value = "")
      updateNumericInput(session, "add_amount",      value = NA)
      updateSelectInput(session,  "add_payment",     selected = "UPI")

      output$add_validation_msg <- renderUI(NULL)
      output$add_success_msg    <- renderUI({
        alert_box(
          sprintf("Transaction saved (ID: %s).", result),
          type = "success"
        )
      })
    } else {
      output$add_success_msg    <- renderUI(NULL)
      output$add_validation_msg <- renderUI({
        alert_box(as.character(result), type = "danger", dismissible = FALSE)
      })
    }
  })

  # ===========================================================================
  # SECTION 4 — BUDGETS
  # ===========================================================================

  # Reactive: load budget rows for the selected month
  budget_data <- eventReactive(
    list(input$budget_load_btn, rv$data_version),
    {
      req(input$budget_month)
      month_str <- trimws(input$budget_month)
      if (!grepl("^\\d{4}-\\d{2}$", month_str)) return(NULL)
      rv$budget_month <- month_str
      get_budgets(con, month = month_str)
    },
    ignoreNULL = FALSE
  )

  # Render one numericInput per category, pre-filled from DB
  output$budget_form_ui <- renderUI({
    month_str  <- trimws(input$budget_month %||% format(Sys.Date(), "%Y-%m"))
    budget_df  <- budget_data()
    month_txns <- tryCatch(
      all_transactions() %>%
        dplyr::filter(format(as.Date(date), "%Y-%m") == month_str),
      error = function(e) data.frame()
    )

    existing <- if (!is.null(budget_df) && nrow(budget_df) > 0L) {
      stats::setNames(budget_df$budget_amount, budget_df$category)
    } else {
      stats::setNames(rep(0, length(VALID_CATEGORIES)), VALID_CATEGORIES)
    }

    actual_spend <- if (nrow(month_txns) > 0L) {
      agg <- month_txns %>%
        dplyr::group_by(category) %>%
        dplyr::summarise(spent = sum(amount, na.rm = TRUE), .groups = "drop")
      stats::setNames(agg$spent, agg$category)
    } else {
      stats::setNames(rep(0, length(VALID_CATEGORIES)), VALID_CATEGORIES)
    }

    tagList(
      section_header(
        paste("Budgets for", month_str),
        subtitle = "Enter 0 for no limit.",
        right_ui = actionButton("budget_save_all_btn",
                                tagList(tags$i(class = "fa fa-save"),
                                        " Save All Budgets"),
                                class = "btn btn-primary btn-sm")
      ),
      fluidRow(
        lapply(VALID_CATEGORIES, function(cat) {
          bv  <- existing[[cat]] %||% 0
          sp  <- actual_spend[[cat]] %||% 0
          pct <- if (!is.na(bv) && bv > 0) round(sp / bv * 100, 1) else NA
          clr <- if (!is.na(pct) && pct >= 100) "danger" else
                 if (!is.na(pct) && pct >= 80)  "warning" else "default"

          column(3,
            tags$div(
              class = "form-group",
              style = "margin-bottom:12px;",
              tags$label(
                style = "font-weight:600;",
                tags$i(class = sprintf("fa fa-%s", CATEGORY_ICONS[[cat]]),
                       style = sprintf("color:%s; margin-right:4px;",
                                       CATEGORY_COLOURS[[cat]])),
                cat
              ),
              numericInput(
                inputId = paste0("budget_amt_", gsub("/", "_", cat)),
                label   = NULL,
                value   = bv,
                min     = 0,
                step    = 100,
                width   = "100%"
              ),
              tags$small(
                class = paste0("text-", clr),
                sprintf("Spent: %s %.0f%s", CURRENCY, sp,
                        if (!is.na(pct)) sprintf(" (%.0f%%)", pct) else "")
              )
            )
          )
        })
      )
    )
  })

  observeEvent(input$budget_save_all_btn, {
    month_str <- trimws(input$budget_month %||% format(Sys.Date(), "%Y-%m"))
    if (!grepl("^\\d{4}-\\d{2}$", month_str)) {
      output$budget_save_status <- renderUI({
        alert_box("Month must be in YYYY-MM format.", "danger", FALSE)
      })
      return()
    }

    errors <- character(0)
    for (cat in VALID_CATEGORIES) {
      input_id <- paste0("budget_amt_", gsub("/", "_", cat))
      val      <- input[[input_id]]
      if (is.null(val) || is.na(val)) next
      result <- tryCatch(
        insert_budget(con, month = month_str, category = cat,
                      budget_amount = val),
        error = function(e) conditionMessage(e)
      )
      if (is.character(result)) errors <- c(errors, sprintf("%s: %s", cat, result))
    }

    rv$data_version <- rv$data_version + 1L

    output$budget_save_status <- renderUI({
      if (length(errors) == 0L) {
        alert_box(sprintf("Budgets for %s saved.", month_str), "success")
      } else {
        alert_box(paste("Some budgets could not be saved:",
                        paste(errors, collapse = "; ")), "danger")
      }
    })
  })

  # ===========================================================================
  # SECTION 5 — ANALYTICS
  # ===========================================================================

  an_monthly <- reactive({
    compute_monthly_summary(an_filtered_transactions())
  })
  an_category <- reactive({
    compute_category_summary(an_filtered_transactions())
  })
  an_payment <- reactive({
    compute_payment_summary(an_filtered_transactions())
  })
  an_daily <- reactive({
    compute_daily_summary(an_filtered_transactions())
  })

  # Monthly trend chart
  output$an_plot_monthly <- renderPlotly({
    d <- an_monthly()
    if (nrow(d) == 0L) {
      return(plot_ly() %>%
        add_annotations(text = "No data for this period.", showarrow = FALSE,
                        xref = "paper", yref = "paper", x = 0.5, y = 0.5))
    }
    plot_ly(d, x = ~year_month, y = ~total_amount,
            type = "scatter", mode = "lines+markers",
            line   = list(color = "#3498DB", width = 2),
            marker = list(color = "#3498DB", size = 7),
            hovertemplate = paste0("<b>%{x}</b><br>Total: ", CURRENCY,
                                   " %{y:,.0f}<extra></extra>")) %>%
      layout(title  = list(text = "Monthly Spending Trend"),
             xaxis  = list(title = "Month"),
             yaxis  = list(title = paste("Amount (", CURRENCY, ")")),
             paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"),
             displaylogo = FALSE)
  })

  output$an_table_monthly <- DT::renderDataTable({
    d <- an_monthly()
    if (nrow(d) == 0L) return(NULL)
    d %>%
      dplyr::transmute(
        Month          = year_month,
        `Total`        = sprintf("%s\u00a0%.2f", CURRENCY, total_amount),
        `Transactions` = transaction_count,
        `Avg Daily`    = sprintf("%s\u00a0%.2f", CURRENCY, avg_daily_amount)
      ) %>%
      DT::datatable(rownames = FALSE, class = "compact stripe",
                    options = list(dom = "tip", pageLength = 12))
  })

  # Category breakdown
  output$an_plot_category_bar <- renderPlotly({
    d <- an_category()
    if (nrow(d) == 0L) return(plot_ly() %>% add_annotations(
      text = "No data.", showarrow = FALSE, xref = "paper", yref = "paper",
      x = 0.5, y = 0.5))
    plot_ly(d %>% dplyr::arrange(total_amount),
            x = ~total_amount, y = ~category,
            type        = "bar", orientation = "h",
            marker      = list(color = ~color_hex),
            hovertemplate = paste0("<b>%{y}</b><br>", CURRENCY,
                                   " %{x:,.0f}<extra></extra>")) %>%
      layout(xaxis = list(title = paste("Amount (", CURRENCY, ")")),
             yaxis = list(title = ""),
             margin = list(l = 100),
             paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  output$an_plot_category_pie <- renderPlotly({
    d <- an_category()
    if (nrow(d) == 0L) return(plot_ly() %>% add_annotations(
      text = "No data.", showarrow = FALSE, xref = "paper", yref = "paper",
      x = 0.5, y = 0.5))
    plot_ly(d, labels = ~category, values = ~total_amount,
            type         = "pie",
            marker       = list(colors = d$color_hex),
            textposition = "inside", textinfo = "percent+label",
            hovertemplate = paste0("<b>%{label}</b><br>", CURRENCY,
                                   " %{value:,.0f} (%{percent})<extra></extra>")) %>%
      layout(showlegend = FALSE, margin = list(t = 10, b = 10),
             paper_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  output$an_table_category <- DT::renderDataTable({
    d <- an_category()
    if (nrow(d) == 0L) return(NULL)
    d %>%
      dplyr::transmute(
        Category       = category,
        Total          = sprintf("%s\u00a0%.2f", CURRENCY, total_amount),
        `Transactions` = transaction_count,
        `% of Total`   = sprintf("%.1f%%", pct_of_total)
      ) %>%
      DT::datatable(rownames = FALSE, class = "compact stripe",
                    options = list(dom = "tip", pageLength = 12))
  })

  # Payment breakdown
  output$an_plot_payment_bar <- renderPlotly({
    d <- an_payment()
    if (nrow(d) == 0L) return(plot_ly() %>% add_annotations(
      text = "No data.", showarrow = FALSE, xref = "paper", yref = "paper",
      x = 0.5, y = 0.5))
    pm_colours <- c(UPI = "#3498DB", Cash = "#2ECC71",
                    Card = "#F39C12", "Bank Transfer" = "#9B59B6")
    plot_ly(d, x = ~payment_method, y = ~total_amount,
            type   = "bar",
            marker = list(color = pm_colours[d$payment_method]),
            hovertemplate = paste0("<b>%{x}</b><br>", CURRENCY,
                                   " %{y:,.0f}<extra></extra>")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = paste(CURRENCY)),
             paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  output$an_plot_payment_pie <- renderPlotly({
    d <- an_payment()
    if (nrow(d) == 0L) return(plot_ly() %>% add_annotations(
      text = "No data.", showarrow = FALSE, xref = "paper", yref = "paper",
      x = 0.5, y = 0.5))
    pm_colours <- c(UPI = "#3498DB", Cash = "#2ECC71",
                    Card = "#F39C12", "Bank Transfer" = "#9B59B6")
    plot_ly(d, labels = ~payment_method, values = ~total_amount,
            type         = "pie",
            marker       = list(colors = pm_colours[d$payment_method]),
            textposition = "inside", textinfo = "percent+label",
            hovertemplate = paste0("<b>%{label}</b><br>", CURRENCY,
                                   " %{value:,.0f}<extra></extra>")) %>%
      layout(showlegend = FALSE, margin = list(t = 10, b = 10),
             paper_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = FALSE)
  })

  output$an_table_payment <- DT::renderDataTable({
    d <- an_payment()
    if (nrow(d) == 0L) return(NULL)
    d %>%
      dplyr::transmute(
        `Payment Method` = payment_method,
        Total            = sprintf("%s\u00a0%.2f", CURRENCY, total_amount),
        `Transactions`   = transaction_count,
        `% of Total`     = sprintf("%.1f%%", pct_of_total)
      ) %>%
      DT::datatable(rownames = FALSE, class = "compact stripe",
                    options = list(dom = "tip", pageLength = 12))
  })

  # Daily pattern chart
  output$an_plot_daily <- renderPlotly({
    d <- an_daily()
    if (nrow(d) == 0L) return(plot_ly() %>% add_annotations(
      text = "No data for this period.", showarrow = FALSE,
      xref = "paper", yref = "paper", x = 0.5, y = 0.5))
    plot_ly(d, x = ~date, y = ~total_amount,
            type = "scatter", mode = "lines",
            fill = "tozeroy", fillcolor = "rgba(52,152,219,0.15)",
            line = list(color = "#3498DB", width = 1.5),
            hovertemplate = paste0("<b>%{x}</b><br>", CURRENCY,
                                   " %{y:,.0f}<extra></extra>")) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = paste("Amount (", CURRENCY, ")")),
             paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA") %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"),
             displaylogo = FALSE)
  })

  # ===========================================================================
  # SECTION 6 — FORECAST
  # ===========================================================================

  observeEvent(input$fc_run_btn, {
    df <- all_transactions()
    if (is.null(df) || nrow(df) == 0L) {
      rv$forecast_result <- list(
        status  = "insufficient_data",
        message = "No transactions found. Add transactions before running a forecast."
      )
      return()
    }

    rv$forecast_result <- tryCatch(
      run_forecast(
        transactions_df = df,
        horizon_days    = as.integer(input$fc_horizon),
        method          = input$fc_method
      ),
      error = function(e) list(status = "error", message = conditionMessage(e))
    )
  })

  output$fc_status_msg <- renderUI({
    r <- rv$forecast_result
    if (is.null(r)) {
      return(alert_box(
        paste0("Click <strong>Run Forecast</strong> to generate projections. ",
               "Requires at least ", FORECAST_MIN_DAYS, " days of history."),
        type = "info", dismissible = FALSE
      ))
    }
    if (r$status == "insufficient_data") {
      return(alert_box(r$message, type = "warning", dismissible = FALSE))
    }
    if (r$status == "error") {
      return(alert_box(paste("Forecast error:", r$message),
                       type = "danger", dismissible = FALSE))
    }
    NULL
  })

  output$fc_chart <- renderPlotly({
    r <- rv$forecast_result
    if (is.null(r) || r$status != "ok") {
      return(plot_ly() %>%
        add_annotations(
          text      = if (is.null(r)) "Run a forecast to see projections."
                      else r$message,
          showarrow = FALSE, xref = "paper", yref = "paper",
          x = 0.5, y = 0.5,
          font = list(size = 14, color = "#95A5A6")
        ) %>%
        layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
               paper_bgcolor = "#FAFAFA", plot_bgcolor = "#FAFAFA")
      )
    }
    plot_forecast(r, currency_symbol = CURRENCY)
  })

  output$fc_summary_card <- renderUI({
    r <- rv$forecast_result
    if (is.null(r) || r$status != "ok") return(NULL)
    forecast_summary_card(
      summary_stats   = r$summary_stats,
      model_label     = r$model_label,
      model_details   = r$model_details,
      horizon_days    = input$fc_horizon,
      currency_symbol = CURRENCY
    )
  })

  # ===========================================================================
  # SECTION 7 — ANOMALY DETECTION
  # ===========================================================================

  observeEvent(input$anom_run_btn, {
    df <- all_transactions()
    if (is.null(df) || nrow(df) == 0L) {
      rv$anomaly_result <- list(
        status         = "insufficient_data",
        message        = "No transactions found.",
        flagged_count  = 0L,
        total_checked  = 0L,
        flagged_df     = data.frame(),
        category_stats = data.frame()
      )
      return()
    }
    rv$anomaly_result <- tryCatch(
      get_anomaly_summary(
        data           = df,
        iqr_multiplier = input$anom_multiplier,
        min_obs        = as.integer(input$anom_min_obs)
      ),
      error = function(e) list(
        status        = "error",
        status_message = conditionMessage(e),
        flagged_count  = 0L,
        total_checked  = 0L,
        flagged_df     = data.frame(),
        category_stats = data.frame()
      )
    )
  })

  output$anom_summary_ui <- renderUI({
    r <- rv$anomaly_result
    if (is.null(r)) {
      return(alert_box(
        paste0("Click <strong>Detect</strong> to scan for unusually large ",
               "transactions using the IQR method."),
        type = "info", dismissible = FALSE
      ))
    }
    if (r$status == "insufficient_data") {
      return(alert_box(r$message %||% r$status_message, "warning", FALSE))
    }
    if (r$status == "error") {
      return(alert_box(paste("Detection error:", r$status_message),
                       "danger", FALSE))
    }
    if (r$flagged_count == 0L) {
      return(alert_box(r$status_message, "success", FALSE))
    }
    # Build summary card inline — does not use legacy run_anomaly_detection() format
    cnt <- r$flagged_count
    tot <- r$total_checked
    tags$div(
      class = "anomaly-summary-card",
      tags$div(
        class = "anomaly-summary-header",
        tags$span(class = "anomaly-badge-large",
          tags$i(class = "fa fa-exclamation-triangle"),
          sprintf(" %d Anomal%s Detected", cnt, if (cnt == 1L) "y" else "ies")
        ),
        tags$span(class = "anomaly-method-label",
          sprintf("IQR method  |  k = %.1f  |  Checked %d transaction%s",
                  input$anom_multiplier, tot, if (tot == 1L) "" else "s")
        )
      ),
      tags$div(
        class = "anomaly-summary-stats",
        tags$div(class = "anomaly-stat",
          tags$span(class = "anomaly-stat-label", "Total Flagged Amount"),
          tags$span(class = "anomaly-stat-value",
            format_currency(r$flagged_amount_total, CURRENCY))
        ),
        tags$div(class = "anomaly-stat",
          tags$span(class = "anomaly-stat-label", "Categories Affected"),
          tags$span(class = "anomaly-stat-value",
            paste(r$categories_flagged, collapse = ", "))
        ),
        if (length(r$categories_skipped) > 0L) {
          tags$div(class = "anomaly-stat",
            tags$span(class = "anomaly-stat-label", "Used Global Fallback"),
            tags$span(class = "anomaly-stat-value",
              paste(r$categories_skipped, collapse = ", "))
          )
        }
      ),
      tags$p(class = "anomaly-disclaimer",
        tags$i(class = "fa fa-info-circle"),
        " Flagged transactions are statistically unusual relative to category
          spending patterns. They are not automatically classified as errors."
      )
    )
  })

  output$anom_stats_heading <- renderUI({
    r <- rv$anomaly_result
    if (is.null(r) || r$flagged_count == 0L) return(NULL)
    section_header(
      sprintf("Flagged Transactions (%d)", r$flagged_count),
      subtitle = "Sorted by excess amount — most unusual first."
    )
  })

  output$anom_table <- DT::renderDataTable({
    r <- rv$anomaly_result
    if (is.null(r) || is.null(r$flagged_df) || nrow(r$flagged_df) == 0L) {
      return(NULL)
    }
    df <- r$flagged_df %>%
      dplyr::mutate(
        Date        = format(as.Date(date), "%d %b %Y"),
        Category    = category,
        Description = description,
        Amount      = sprintf("%s\u00a0%.2f", CURRENCY, amount),
        `Upper Bound` = sprintf("%s\u00a0%.2f", CURRENCY, upper_bound),
        Excess      = sprintf("+%s\u00a0%.2f", CURRENCY, excess_amount),
        Scope       = dplyr::if_else(scope == "category",
                                     "Per-category", "Global fallback"),
        Reason      = reason
      ) %>%
      dplyr::select(Date, Category, Description, Amount,
                    `Upper Bound`, Excess, Scope, Reason)

    DT::datatable(
      df, rownames = FALSE, escape = FALSE,
      class   = "display stripe compact",
      options = list(
        pageLength = 10, scrollX = TRUE,
        dom        = "lfrtip",
        columnDefs = list(
          list(width = "30%", targets = 7),
          list(className = "dt-right", targets = c(3, 4, 5))
        )
      )
    ) %>%
      DT::formatStyle("Amount", color = "#C0392B", fontWeight = "bold") %>%
      DT::formatStyle("Excess", color = "#922B21", fontWeight = "600")
  })

  output$anom_category_stats_heading <- renderUI({
    r <- rv$anomaly_result
    if (is.null(r) || is.null(r$category_stats) ||
        nrow(r$category_stats) == 0L) return(NULL)
    tags$div(style = "margin-top:24px;",
      section_header("IQR Thresholds by Category",
                     subtitle = paste0(
                       "The upper bound used for each category. ",
                       "Categories with fewer than ",
                       input$anom_min_obs,
                       " observations used the global distribution."
                     ))
    )
  })

  output$anom_cat_stats_table <- DT::renderDataTable({
    r <- rv$anomaly_result
    if (is.null(r) || is.null(r$category_stats) ||
        nrow(r$category_stats) == 0L) return(NULL)

    r$category_stats %>%
      dplyr::transmute(
        Category      = category,
        `Observations` = n_obs,
        Q1             = sprintf("%s\u00a0%.2f", CURRENCY, Q1),
        Q3             = sprintf("%s\u00a0%.2f", CURRENCY, Q3),
        IQR            = sprintf("%s\u00a0%.2f", CURRENCY, IQR),
        `Upper Bound`  = sprintf("%s\u00a0%.2f", CURRENCY, upper_bound),
        `Distribution` = dplyr::if_else(scope == "category",
                                        "Per-category", "Global fallback")
      ) %>%
      DT::datatable(
        rownames = FALSE, class = "compact stripe",
        options  = list(dom = "t", pageLength = 15,
                        columnDefs = list(
                          list(className = "dt-right", targets = c(2, 3, 4, 5))
                        ))
      )
  })

  output$anom_download <- downloadHandler(
    filename = function() download_filename_anomalies(),
    content  = function(file) {
      r <- rv$anomaly_result
      if (is.null(r) || is.null(r$flagged_df) || nrow(r$flagged_df) == 0L) {
        writeLines("No anomalies detected.", file)
        return()
      }
      path <- export_anomaly_csv(r$flagged_df,
                                  iqr_multiplier  = input$anom_multiplier,
                                  currency_symbol = CURRENCY)
      if (!is.null(path)) file.copy(path, file)
    }
  )

  # ===========================================================================
  # SECTION 8 — IMPORT / EXPORT
  # ===========================================================================

  # File upload — runs automatically when a file is selected
  observeEvent(input$import_file, {
    req(input$import_file)
    fpath <- input$import_file$datapath
    fname <- input$import_file$name
    ext   <- tolower(tools::file_ext(fname))

    rv$import_result <- if (ext == "csv") {
      import_csv(fpath)
    } else if (ext %in% c("xlsx", "xls")) {
      import_excel(fpath)
    } else {
      list(
        status          = "error",
        summary_message = sprintf("Unsupported file type '.%s'. Upload CSV or Excel.", ext),
        clean_count     = 0L,
        discarded_count = 0L,
        total_rows      = 0L,
        has_clean       = FALSE,
        has_discarded   = FALSE,
        is_partial      = FALSE,
        detail_messages = character(0),
        clean_df        = data.frame(),
        discarded_df    = data.frame()
      )
    }
  })

  # Validation feedback UI
  output$import_validation_ui <- renderUI({
    r <- rv$import_result
    if (is.null(r)) return(NULL)

    tag_list <- tagList()

    # Summary alert
    alert_type <- switch(r$status,
      ok      = "success",
      partial = "warning",
      error   = "danger",
      "info"
    )
    tag_list <- tagList(
      tag_list,
      alert_box(r$summary_message, type = alert_type, dismissible = FALSE)
    )

    # Row-level error table (shown when discarded rows exist)
    if (!is.null(r$discarded_df) && is.data.frame(r$discarded_df) &&
        nrow(r$discarded_df) > 0L && "validation_errors" %in% names(r$discarded_df)) {

      err_df <- data.frame(
        Row    = which(
          !is.na(r$discarded_df$validation_errors) &
          nzchar(r$discarded_df$validation_errors)
        ),
        Errors = r$discarded_df$validation_errors[
          !is.na(r$discarded_df$validation_errors) &
          nzchar(r$discarded_df$validation_errors)
        ],
        stringsAsFactors = FALSE
      )
      if (nrow(err_df) > 0L) {
        tag_list <- tagList(
          tag_list,
          tags$div(
            class = "import-error-wrapper",
            style = "margin-top:8px;",
            DT::renderDataTable(
              DT::datatable(err_df, rownames = FALSE,
                            class   = "compact",
                            options = list(dom = "tip", pageLength = 5,
                                           scrollX = TRUE))
            )
          )
        )
      }
    }

    tag_list
  })

  # Import action button — shown only when clean rows are available
  output$import_action_ui <- renderUI({
    r <- rv$import_result
    if (is.null(r) || !r$has_clean) return(NULL)

    tagList(
      tags$hr(),
      tags$p(
        style = "font-weight:600;",
        sprintf("Ready to import %d valid row%s.",
                r$clean_count,
                if (r$clean_count == 1L) "" else "s")
      ),
      if (r$is_partial) {
        tags$p(
          class = "text-warning",
          tags$i(class = "fa fa-exclamation-triangle"),
          sprintf(
            " %d row%s will be skipped due to validation errors.",
            r$discarded_count,
            if (r$discarded_count == 1L) "" else "s"
          )
        )
      },
      actionButton("import_confirm_btn",
                   tagList(tags$i(class = "fa fa-upload"),
                           sprintf(" Import %d Transaction%s",
                                   r$clean_count,
                                   if (r$clean_count == 1L) "" else "s")),
                   class = "btn btn-success btn-block",
                   width = "100%")
    )
  })

  # Confirm import — write clean rows to DB
  observeEvent(input$import_confirm_btn, {
    r <- rv$import_result
    req(!is.null(r), r$has_clean, nrow(r$clean_df) > 0L)

    n_inserted <- tryCatch(
      db_bulk_insert_transactions(con, r$clean_df),
      error = function(e) conditionMessage(e)
    )

    if (is.numeric(n_inserted)) {
      rv$data_version  <- rv$data_version + 1L
      rv$import_result <- NULL
      showNotification(
        sprintf("%d transaction%s imported successfully.",
                n_inserted,
                if (n_inserted == 1L) "" else "s"),
        type = "message", duration = 4
      )
      reset("import_file")   # clear the file input (shinyjs)
    } else {
      showNotification(
        paste("Import failed:", as.character(n_inserted)),
        type = "error", duration = 6
      )
    }
  })

  # Export: filtered transactions CSV
  output$export_csv <- downloadHandler(
    filename = function() download_filename_transactions("csv"),
    content  = function(file) {
      path <- export_transactions_csv(
        filtered_transactions(),
        currency_symbol = CURRENCY,
        date_range      = sprintf("%s to %s",
                                  format(input$txn_date_from),
                                  format(input$txn_date_to))
      )
      if (is.null(path)) writeLines("No data to export.", file)
      else file.copy(path, file)
    }
  )

  # Export: filtered transactions Excel
  output$export_excel <- downloadHandler(
    filename = function() download_filename_transactions("xlsx"),
    content  = function(file) {
      path <- export_transactions_excel(filtered_transactions())
      if (is.null(path)) {
        writexl::write_xlsx(
          list(Transactions = data.frame(Message = "No data.")),
          path = file
        )
      } else file.copy(path, file)
    }
  )

  # Export: monthly report Excel
  output$export_monthly_report <- downloadHandler(
    filename = function() {
      download_filename_monthly_report(
        trimws(input$report_month %||% format(Sys.Date(), "%Y-%m"))
      )
    },
    content = function(file) {
      month_str <- trimws(input$report_month %||% format(Sys.Date(), "%Y-%m"))

      budget_df <- tryCatch(
        get_budgets(con, month = month_str),
        error = function(e) NULL
      )

      fc_result <- rv$forecast_result

      report_data <- tryCatch(
        generate_report_data(
          transactions_df = all_transactions(),
          budget_df       = budget_df,
          month           = month_str,
          forecast_result = fc_result,
          currency_symbol = CURRENCY
        ),
        error = function(e) NULL
      )

      if (is.null(report_data)) {
        writexl::write_xlsx(
          list(Error = data.frame(
            Message = "Report could not be generated. Check the month format (YYYY-MM)."
          )),
          path = file
        )
        return()
      }

      path <- tryCatch(
        generate_monthly_report(report_data = report_data),
        error = function(e) {
          message(sprintf("[ERROR] generate_monthly_report: %s", conditionMessage(e)))
          NULL
        }
      )

      if (is.null(path)) {
        writexl::write_xlsx(
          list(Error = data.frame(
            Message = paste("Report generation failed. Please try again.",
                            "If the problem persists, check available disk space.")
          )),
          path = file
        )
        return()
      }

      file.copy(path, file)
    }
  )

}
