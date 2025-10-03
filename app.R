# app.R
# Shiny EMA Semi-Random Scheduler (REDCap-ready)
# - Time pickers (shinyTime) for per-prompt windows
# - Windows + Table in the same tab with shared horizontal scroll and aligned column widths
# - Vertical time-window cards (Start above End)
# - Day column shows 1..N (dates still used internally)
# - Static Info tab; REDCap-ready CSV export; optional hard min interval; weekend highlighting

# ---- Packages ----
library(shiny)
library(DT)
library(ggplot2)
library(shinyTime)

# ---- Helpers ----
fmt_time <- function(x) format(x, "%Y-%m-%d %H:%M", usetz = FALSE)

suggest_window <- function(j) {
  switch(
    as.character(j),
    "1" = list(8, 0, 10, 0),
    "2" = list(12, 0, 14, 0),
    "3" = list(16, 0, 18, 0),
    "4" = list(19, 0, 21, 0),
    list(9, 0, 17, 0)
  )
}

fmt_ix <- function(x, max_x) {
  if (max_x < 10) as.character(x) else sprintf("%02d", x)
}

build_names <- function(n_days, prompts_per_day, template) {
  out <- character(n_days * prompts_per_day)
  k <- 1
  for (i in seq_len(n_days)) {
    day_str <- fmt_ix(i, n_days)
    for (j in seq_len(prompts_per_day)) {
      beep_str <- fmt_ix(j, prompts_per_day)
      nm <- gsub("\\{day\\}", day_str, template)
      nm <- gsub("\\{beep\\}", beep_str, nm)
      out[k] <- nm
      k <- k + 1
    }
  }
  out
}

build_export_strings <- function(edit_df) {
  times_only <- edit_df[,
    setdiff(names(edit_df), c("Day", ".wknd")),
    drop = FALSE
  ]
  vals <- as.vector(t(as.matrix(times_only)))
  sub("^(\\s*)(\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}).*$", "\\2", vals)
}

# ---- UI ----
ui <- fluidPage(
  # Global CSS (shared widths + non-wrapping windows row + shared scroller)
  tags$head(
    tags$style(HTML(
      "
      .app-logo {
      position: fixed;     /* pin to viewport */
      top: 10px;
      right: 12px;
      height: 84px;        /* adjust to taste */
      z-index: 1000;       /* above app content */
      filter: drop-shadow(0 2px 6px rgba(0,0,0,.25));
      pointer-events: none; /* don't block clicks underneath */
    }
    @media (max-width: 768px) {
      .app-logo { display: none; }  /* optional: hide on phones */
    }
      
      :root{
        --day-col-width: 64px;   /* width of left Day column and its spacer */
        --col-width: 140px;      /* width of each prompt column and its card */
      }

      /* Make entire tab content horizontally scrollable */
      .tab-content { overflow-x: auto; }

      /* Shared scroller wraps the windows row + the table so they pan together */
      .shared-scroll { width: 100%; overflow-x: auto; }

      /* Windows row: single line, no wrap, aligned with table columns */
      .windows-row {
        display: flex;
        flex-wrap: nowrap;
        gap: 8px;
        align-items: stretch;
        padding-bottom: 8px;
      }
      .windows-spacer {
        flex: 0 0 var(--day-col-width);
        min-width: var(--day-col-width);
      }
      .prompt-card {
        flex: 0 0 var(--col-width);
        min-width: var(--col-width);
      }
      .prompt-card .time-field { width: 100%; min-width: 0; }

      /* DT column widths to match the cards */
      table.dataTable { width: auto !important; }
      table.dataTable th, table.dataTable td { white-space: nowrap; }
    "
    ))
  ),

  tags$img(
    src = "https://github.com/nopsybot/ema-semirandom-scheduler/raw/main/hexlogo.png",
    class = "app-logo"
  ),

  titlePanel("EMA Semi-Random Prompt Scheduler (REDCap-ready)"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      dateInput("start_date", "Start date", value = Sys.Date()),
      numericInput(
        "n_days",
        "Number of days",
        value = 14,
        min = 1,
        max = 365,
        step = 1
      ),
      numericInput(
        "prompts_per_day",
        "Prompts per day",
        value = 4,
        min = 1,
        max = 12,
        step = 1
      ),
      textInput("record_id", "REDCap record ID (participant)", value = "1"),
      textInput(
        "event_choice",
        "REDCap event name",
        value = "enrollment_arm_1"
      ),
      uiOutput("event_notice"),
      textInput(
        "name_tpl",
        "Column name template",
        value = "beep_ic_{day}_{beep}_dtime",
        placeholder = "Use {day} and {beep}; e.g., ema_{day}_{beep}"
      ),
      helpText("{day}/{beep} pad to 2 digits only if their max ≥ 10 (01..14)."),
      textInput("tz", "Time zone (Olson name)", value = "Europe/Zurich"),
      numericInput("seed", "Random seed (optional)", value = NA),
      checkboxInput(
        "enforce_gap",
        "Enforce minimum interval between sequential prompts (hard)",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.enforce_gap",
        numericInput(
          "gap_minutes",
          "Minimum interval (minutes)",
          value = 30,
          min = 1,
          max = 720,
          step = 1
        )
      ),
      actionButton("regen", "Regenerate times", class = "btn-primary"),
      tags$hr(),
      downloadButton("dl_csv", "Export CSV for REDCap"),
      helpText(
        "Export = single row: record_id + names from template (day-major order)."
      )
    ),
    mainPanel(
      width = 8,
      uiOutput("dst_notice"),
      uiOutput("gap_stats"),
      tags$hr(),
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Table",
          h4("Per-prompt windows + schedule (horizontally scrollable)"),
          # Shared scroller holds the windows row and the table so they align & pan together
          div(
            class = "shared-scroll",
            # Windows row: spacer (for Day column) + one vertical card per prompt
            uiOutput("windows_row"),
            # Table (no inner scrolling; grows to content width)
            DTOutput("schedule_dt")
          )
        ),
        tabPanel(
          "Plot",
          div(
            style = "max-width: 720px;",
            numericInput(
              "tile_width",
              "Tile width (minutes)",
              value = 20,
              min = 5,
              max = 120,
              step = 5
            )
          ),
          plotOutput("schedule_plot", height = "420px")
        ),
        tabPanel(
          "Info",
          h4("About this app"),
          div(
            style = "border:1px solid #eee; padding:10px; background:#fafafa; margin-top:8px;",
            p(
              "This EMA Semi-Random Scheduler app helps researchers generate semi-randomized prompt schedules,"
            ),
            p(
              "review them in table or plot form, and export a REDCap-ready CSV."
            ),
            p(
              "The current version uses fixed daytime windows with per-prompt randomization and optional minimum intervals."
            )
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Time-window inputs (vertical cards) + left spacer to align with the table's Day column
  output$windows_row <- renderUI({
    p <- req(input$prompts_per_day)
    p <- max(1, min(12, as.integer(p)))

    cards <- lapply(seq_len(p), function(j) {
      d <- suggest_window(j)
      start_val <- sprintf("%02d:%02d", d[[1]], d[[2]])
      end_val <- sprintf("%02d:%02d", d[[3]], d[[4]])

      tags$div(
        class = "prompt-card",
        wellPanel(
          tags$strong(sprintf("Prompt %d", j)),
          div(
            class = "time-field",
            shinyTime::timeInput(
              inputId = paste0("tstart_", j),
              label = "Start",
              value = strptime(start_val, "%H:%M"),
              seconds = FALSE
            )
          ),
          div(
            class = "time-field",
            shinyTime::timeInput(
              inputId = paste0("tend_", j),
              label = "End",
              value = strptime(end_val, "%H:%M"),
              seconds = FALSE
            )
          )
        )
      )
    })

    # Left spacer aligns with Day column width; then one card per prompt
    tags$div(
      class = "windows-row",
      tags$div(class = "windows-spacer"),
      cards
    )
  })

  # DST transitions
  dst_transitions <- reactive({
    req(input$start_date, input$n_days, input$tz)
    n_days <- max(1, min(365, as.integer(input$n_days)))
    tz <- input$tz

    start <- as.POSIXct(
      sprintf("%s 00:00:00", as.character(as.Date(input$start_date))),
      tz = tz
    )
    end <- as.POSIXct(
      sprintf("%s 00:00:00", as.character(as.Date(input$start_date) + n_days)),
      tz = tz
    )

    hrs <- seq(from = start, to = end, by = "hour")
    if (length(hrs) < 2) {
      return(NULL)
    }

    isdst <- as.POSIXlt(hrs, tz = tz)$isdst
    changes <- which(diff(isdst) != 0)
    if (length(changes) == 0) {
      return(NULL)
    }

    hrs[changes + 1]
  })

  output$dst_notice <- renderUI({
    tt <- dst_transitions()
    if (is.null(tt)) {
      return(NULL)
    }
    tags$div(
      class = "alert alert-warning",
      style = "margin-top:10px;",
      tags$strong(
        "Heads up: Summer/Winter time change occurs during this schedule."
      ),
      tags$ul(lapply(tt, function(ti) {
        tags$li(sprintf(
          "Transition near %s (local time)",
          format(ti, "%Y-%m-%d %H:%M")
        ))
      })),
      tags$p(
        "Consider communicating this to participants and verifying device time settings."
      )
    )
  })

  # Generator with optional hard min interval
  schedule_mat <- reactive({
    req(input$start_date, input$n_days, input$prompts_per_day, input$tz)
    if (!is.na(input$seed)) {
      set.seed(as.integer(input$seed))
    }

    n_days <- max(1, min(365, as.integer(input$n_days)))
    p <- max(1, min(12, as.integer(input$prompts_per_day)))
    tz <- input$tz

    days <- seq.Date(as.Date(input$start_date), by = "day", length.out = n_days)

    windows <- lapply(seq_len(p), function(j) {
      d <- suggest_window(j)
      tstart <- input[[paste0("tstart_", j)]]
      tend <- input[[paste0("tend_", j)]]
      if (is.null(tstart)) {
        tstart <- strptime(sprintf("%02d:%02d", d[[1]], d[[2]]), "%H:%M")
      }
      if (is.null(tend)) {
        tend <- strptime(sprintf("%02d:%02d", d[[3]], d[[4]]), "%H:%M")
      }
      list(tstart = tstart, tend = tend)
    })

    mat <- matrix(NA_real_, nrow = n_days, ncol = p)

    gap_sec <- if (isTRUE(input$enforce_gap)) {
      max(
        0,
        as.integer(ifelse(is.null(input$gap_minutes), 30, input$gap_minutes)) *
          60
      )
    } else {
      0
    }
    redraws <- integer(n_days)
    infeas <- logical(n_days)

    for (i in seq_len(n_days)) {
      date_chr <- as.character(days[i])

      day_windows <- lapply(seq_len(p), function(j) {
        w <- windows[[j]]
        st_h <- as.integer(format(w$tstart, "%H"))
        st_m <- as.integer(format(w$tstart, "%M"))
        en_h <- as.integer(format(w$tend, "%H"))
        en_m <- as.integer(format(w$tend, "%M"))
        st <- as.POSIXct(
          sprintf("%s %02d:%02d:00", date_chr, st_h, st_m),
          tz = tz
        )
        en <- as.POSIXct(
          sprintf("%s %02d:%02d:00", date_chr, en_h, en_m),
          tz = tz
        )
        if (en <= st) {
          en <- st + 5 * 60
        }
        list(start = st, end = en)
      })

      # Greedy feasibility check
      feas <- TRUE
      if (gap_sec > 0) {
        cur <- day_windows[[1]]$start
        if (cur > day_windows[[1]]$end) {
          feas <- FALSE
        }
        for (j in 2:p) {
          cur <- max(cur + gap_sec, day_windows[[j]]$start)
          if (cur > day_windows[[j]]$end) {
            feas <- FALSE
            break
          }
        }
      }

      if (gap_sec == 0) {
        for (j in seq_len(p)) {
          w <- day_windows[[j]]
          delta <- as.numeric(difftime(w$end, w$start, units = "secs"))
          mat[i, j] <- as.numeric(w$start + runif(1, 0, delta))
        }
        redraws[i] <- 0L
        infeas[i] <- FALSE
      } else if (!feas) {
        infeas[i] <- TRUE
        redraws[i] <- NA_integer_
      } else {
        # Rejection sampling
        max_iter <- 5000L
        tries <- 0L
        ok <- FALSE
        while (tries < max_iter && !ok) {
          tries <- tries + 1L
          tvec <- numeric(p)
          ok <- TRUE
          for (j in seq_len(p)) {
            w <- day_windows[[j]]
            delta <- as.numeric(difftime(w$end, w$start, units = "secs"))
            tvec[j] <- as.numeric(w$start + runif(1, 0, delta))
            if (j > 1 && (tvec[j] < (tvec[j - 1] + gap_sec))) {
              ok <- FALSE
              break
            }
          }
        }
        if (!ok) {
          infeas[i] <- TRUE
          redraws[i] <- tries - 1L
        } else {
          for (j in seq_len(p)) {
            mat[i, j] <- tvec[j]
          }
          redraws[i] <- tries - 1L
          infeas[i] <- FALSE
        }
      }
    }

    dimnames(mat) <- list(
      format(days, "%Y-%m-%d"),
      paste0("Prompt ", seq_len(p))
    )
    attr(mat, "tz") <- tz
    attr(mat, "gap_redraws") <- redraws
    attr(mat, "gap_infeasible") <- infeas
    mat
  })

  # Manual regenerate trigger
  observeEvent(input$regen, {
    schedule_mat()
  })

  # Editable table state
  edits <- reactiveVal(NULL)

  observeEvent(
    schedule_mat(),
    {
      mat <- schedule_mat()
      tz <- attr(mat, "tz")
      hm <- as.data.frame(
        apply(mat, 2, function(col) {
          format(as.POSIXct(col, origin = "1970-01-01", tz = tz), "%H:%M")
        }),
        stringsAsFactors = FALSE
      )
      # show Day as 1..N (compact), keep dates for export/highlighting
      day_dates <- rownames(mat)
      day_index <- seq_len(nrow(hm))
      df_full <- as.data.frame(
        lapply(seq_len(ncol(hm)), function(j) paste(day_dates, hm[[j]])), # keep full datetime strings
        stringsAsFactors = FALSE
      )
      names(df_full) <- colnames(hm)
      wknd <- weekdays(as.Date(day_dates)) %in% c("Saturday", "Sunday")
      edits(cbind(Day = day_index, df_full, .wknd = wknd))
    },
    ignoreInit = FALSE
  )

  output$schedule_dt <- renderDT({
    df <- req(edits())
    p <- ncol(df) - 2L # number of prompt columns
    cols_to_style <- names(df)[2:(1 + p)]

    datatable(
      df,
      rownames = FALSE,
      editable = list(
        target = "cell",
        disable = list(columns = c(0, ncol(df) - 1))
      ),
      options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        autoWidth = TRUE,
        # Column widths: Day + each prompt column
        columnDefs = list(
          list(width = "var(--day-col-width)", targets = 0),
          list(width = "var(--col-width)", targets = seq.int(1, p)),
          list(visible = FALSE, targets = ncol(df) - 1) # hide .wknd
        )
      )
    ) |>
      formatStyle(
        columns = cols_to_style,
        valueColumns = ".wknd",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#FFF3CD", NA))
      )
  })

  # Gap stats
  output$gap_stats <- renderUI({
    mat <- schedule_mat()
    if (!isTRUE(input$enforce_gap)) {
      return(NULL)
    }
    redraws <- attr(mat, "gap_redraws")
    infeas <- attr(mat, "gap_infeasible")
    parts <- list()
    if (!is.null(redraws)) {
      total <- sum(redraws[!is.na(redraws)], na.rm = TRUE)
      maxr <- if (length(redraws) && any(!is.na(redraws))) {
        max(redraws[!is.na(redraws)], na.rm = TRUE)
      } else {
        0
      }
      parts[[length(parts) + 1]] <- tags$p(sprintf(
        "Total redraws required: %d (max per day: %d)",
        total,
        maxr
      ))
    }
    if (!is.null(infeas) && any(infeas, na.rm = TRUE)) {
      days <- rownames(mat)[which(infeas)]
      parts[[length(parts) + 1]] <- tags$div(
        tags$strong(
          "Some days are infeasible given the chosen windows and interval."
        ),
        tags$ul(lapply(days, function(d) tags$li(paste("Day", d)))),
        tags$p("Widen the windows or reduce the minimum interval.")
      )
      cls <- "alert alert-danger"
    } else {
      cls <- "alert alert-info"
    }
    do.call(tags$div, c(list(class = cls, style = "margin-top:10px;"), parts))
  })

  # Validate datetime edits
  observeEvent(input$schedule_dt_cell_edit, {
    info <- input$schedule_dt_cell_edit
    df <- req(edits())
    if (info$col == 0 || info$col == (ncol(df) - 1)) {
      return()
    }

    val <- trimws(info$value)
    valid <- grepl("^(\\d{4}-\\d{2}-\\d{2}) (?:[01]\\d|2[0-3]):[0-5]\\d$", val)
    if (!valid) {
      showNotification(
        "Invalid datetime. Use YYYY-MM-DD HH:MM.",
        type = "error"
      )
      replaceData(
        dataTableProxy("schedule_dt"),
        df,
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    df[info$row, info$col + 1] <- val
    edits(df)
    replaceData(
      dataTableProxy("schedule_dt"),
      df,
      resetPaging = FALSE,
      rownames = FALSE
    )
  })

  # Plot
  plot_df <- reactive({
    df <- req(edits())
    if (ncol(df) <= 2) {
      return(NULL)
    }
    day_nums <- df$Day # 1..N shown
    wknd <- df$.wknd # weekend flag (hidden)
    time_cols <- df[, setdiff(names(df), c("Day", ".wknd")), drop = FALSE]

    vals <- as.vector(t(as.matrix(time_cols)))
    if (!length(vals)) {
      return(NULL)
    }
    dt <- strptime(vals, "%Y-%m-%d %H:%M")
    n_days <- nrow(df)
    p <- ncol(time_cols)
    day_idx <- rep(seq_len(n_days), each = p)

    data.frame(
      day = as.character(day_nums[day_idx]),
      is_weekend = wknd[day_idx],
      prompt = rep(seq_len(p), times = n_days),
      tod_min = as.integer(format(dt, "%H")) *
        60 +
        as.integer(format(dt, "%M")),
      stringsAsFactors = FALSE
    ) |>
      subset(!is.na(tod_min))
  })

  output$schedule_plot <- renderPlot({
    d <- req(plot_df())
    w_min <- req(input$tile_width)
    if (!is.numeric(w_min) || w_min <= 0) {
      w_min <- 20
    }
    breaks <- seq(0, 24 * 60, by = 120)
    labels <- sprintf("%02d:00", breaks %/% 60)
    ggplot(
      d,
      aes(
        x = tod_min,
        y = factor(day, levels = unique(d$day)),
        fill = is_weekend
      )
    ) +
      geom_tile(width = w_min, height = 0.9) +
      scale_fill_manual(
        values = c(`TRUE` = "#FFF3CD", `FALSE` = "#DDDDDD"),
        labels = c("Weekday", "Weekend")
      ) +
      scale_x_continuous(
        "Time of day",
        breaks = breaks,
        labels = labels,
        limits = c(0, 24 * 60)
      ) +
      labs(y = "Day", title = "EMA prompt times (semi-random)", fill = "") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
  })

  # Event name validation
  is_valid_event <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      return(FALSE)
    }
    grepl("^[a-z][a-z0-9_]*_arm_[1-9][0-9]*$", x)
  }
  output$event_notice <- renderUI({
    ev <- input$event_choice
    if (is.null(ev) || is_valid_event(ev)) {
      return(NULL)
    }
    tags$div(
      class = "alert alert-warning",
      style = "margin-top:8px;",
      "Invalid REDCap event name. Use lowercase letters, digits, and underscores, ending with _arm_N (e.g., clinic_arm_1)."
    )
  })

  # Export
  output$dl_csv <- downloadHandler(
    filename = function() {
      rid <- ifelse(nchar(input$record_id) > 0, input$record_id, "record")
      paste0("ema_schedule_", rid, ".csv")
    },
    content = function(file) {
      df_edit <- req(edits())
      mat <- schedule_mat()
      infeas <- attr(mat, "gap_infeasible")
      if (
        isTRUE(input$enforce_gap) &&
          !is.null(infeas) &&
          any(infeas, na.rm = TRUE)
      ) {
        stop(
          "Export blocked: some days are infeasible with the chosen windows and interval. Adjust settings and regenerate."
        )
      }
      n_days <- nrow(df_edit)
      p <- ncol(df_edit) - 2
      tpl <- ifelse(
        nchar(input$name_tpl) > 0,
        input$name_tpl,
        "dt_{day}_{beep}"
      )
      col_names <- build_names(n_days, p, tpl)
      flat <- build_export_strings(df_edit)
      cols <- setNames(as.list(flat), col_names)
      df <- data.frame(
        `record_id` = input$record_id,
        `redcap_event_name` = input$event_choice,
        as.data.frame(cols),
        check.names = FALSE
      )
      if (!is_valid_event(input$event_choice)) {
        stop(
          "Export blocked: invalid REDCap event name. It must match ^[a-z][a-z0-9_]*_arm_[1-9][0-9]*$."
        )
      }
      write.csv(df, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
