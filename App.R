# app.R
# CSV -> LaTeX wheelchart -> (optional) compile with MiKTeX to PDF
# CSV columns:
#   category, date, end_date, duration_weeks, name, color
#
# date / end_date can be either:
#   - ISO week number (e.g. 12 or "12" or "12.0")
#   - date string (e.g. "2025-03-01") if Date mode allows it
#
# Spans: use end_date or duration_weeks. For funding, spans are rendered as stacked bands.

library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ISOweek)
library(tibble)

`%||%` <- function(a, b) if (!is.null(a) && length(a) && !is.na(a) && a != "") a else b

default_iso_year <- lubridate::isoyear(Sys.Date())
default_max_week <- lubridate::isoweek(lubridate::ymd(sprintf("%d-12-28", default_iso_year)))

ui <- fluidPage(
  titlePanel("Year Wheel LaTeX Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv", "Upload events CSV", accept = c(".csv")),
      
      selectInput("csv_delim", "CSV delimiter",
                  choices = c("Auto"="auto", "Comma (,)"=",", "Semicolon (;, csv2)"=";"),
                  selected = "auto"),
      
      selectInput(
        "date_mode", "CSV date fields interpret as:",
        choices = c("Auto (weeks or dates)" = "auto",
                    "Weeks only" = "weeks",
                    "Dates only" = "dates"),
        selected = "auto"
      ),
      
      checkboxInput("auto_wrap", "Auto-wrap labels (inserts \\\\ line breaks)", value = FALSE),
      numericInput("wrap_width", "Wrap width (characters)", value = 10, min = 5, max = 40, step = 1),
      
      numericInput("year", "ISO Year", value = default_iso_year, min = 1900, max = 2100, step = 1),
      numericInput("w_start", "Start week", value = 1, min = 1, max = 53),
      numericInput("w_end", "End week", value = default_max_week, min = 1, max = 53),
      textInput("center_year", "Center year label", value = as.character(default_iso_year)),
      selectizeInput("highlight_weeks", "Highlight week numbers (bright red):",
                     choices = as.character(1:53), multiple = TRUE),
      
      tags$hr(),
      actionButton("copy_btn", "Copy LaTeX", icon = icon("copy")),
      downloadButton("download_tex", "Download .tex"),
      tags$br(), tags$br(),
      actionButton("compile_btn", "Compile (MiKTeX)", icon = icon("cogs")),
      downloadButton("download_pdf", "Download PDF")
    ),
    mainPanel(
      tags$h3("Generated LaTeX"),
      tags$small("Copy or download, or compile to PDF if MiKTeX is available on PATH."),
      tags$hr(),
      verbatimTextOutput("latex", placeholder = TRUE),
      tags$hr(),
      tags$h4("CSV diagnostics"),
      verbatimTextOutput("diag", placeholder = TRUE),
      tags$hr(),
      tags$h4("Build Log"),
      verbatimTextOutput("build_log", placeholder = TRUE)
    )
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text);
      } else {
        const ta = document.createElement('textarea');
        ta.value = text;
        document.body.appendChild(ta);
        ta.select();
        document.execCommand('copy');
        document.body.removeChild(ta);
      }
    });
  "))
)

server <- function(input, output, session) {
  
  # ---------- Helpers ----------
  
  max_iso_week <- function(year) {
    isoweek(ymd(sprintf("%d-12-28", year)))
  }
  
  # Escape LaTeX specials but keep user-provided '\\' for line breaks
  latex_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "<<<BSLASH>>>", x, fixed = TRUE)
    x <- gsub("([&%$#_])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    x <- gsub("<<<BSLASH>>>", "\\\\", x, fixed = TRUE)
    x
  }
  
  wrap_commas <- function(xs, per = 20) {
    if (length(xs) == 0) return("")
    parts <- split(xs, ceiling(seq_along(xs) / per))
    paste(vapply(parts, function(v) paste(v, collapse = ","), character(1)),
          collapse = ",\n")
  }
  
  style_line <- function(slices, color) {
    slices <- sort(unique(as.integer(slices)))
    if (length(slices) == 0) return(NULL)
    paste0("  slices style{", paste(slices, collapse = ","), "}={", color, ",draw=lightgray}")
  }
  
  iso_week_dates <- function(year, weeks) {
    monday <- ISOweek2date(sprintf("%d-W%02d-1", year, weeks))
    sunday <- monday + days(6)
    paste0(day(monday), "/", day(sunday))
  }
  
  is_numeric_like <- function(s) {
    !is.na(s) && nzchar(s) && grepl("^\\d+(?:\\.0+)?$", s)
  }
  
  parse_int_or_na <- function(x) {
    s <- trimws(as.character(x))
    if (is.na(s) || s == "") return(NA_integer_)
    if (!is_numeric_like(s)) return(NA_integer_)
    as.integer(as.numeric(s))
  }
  
  parse_week_or_date_one <- function(x, selected_year, mode = c("auto", "weeks", "dates")) {
    mode <- match.arg(mode)
    s <- trimws(as.character(x))
    if (is.na(s) || s == "") return(list(week = NA_integer_, issue = NULL, kind = NULL))
    
    if (mode == "weeks") {
      if (is_numeric_like(s)) {
        wk <- as.integer(as.numeric(s))
        return(list(week = as.integer(wk), issue = NULL, kind = "week"))
      }
      return(list(week = NA_integer_, issue = "Expected ISO week number (Weeks only).", kind = "invalid"))
    }
    
    # dates or auto:
    if (mode == "auto" && is_numeric_like(s)) {
      wk <- as.integer(as.numeric(s))
      return(list(week = wk, issue = NULL, kind = "week"))
    }
    
    # date parsing
    d <- suppressWarnings(ymd(s))
    if (is.na(d)) d <- suppressWarnings(dmy(s))  # optional fallback
    if (is.na(d)) {
      if (mode == "dates") {
        return(list(week = NA_integer_, issue = "Expected a date (Dates only).", kind = "invalid"))
      } else {
        return(list(week = NA_integer_, issue = "Unparseable (not a week number or supported date).", kind = "invalid"))
      }
    }
    
    iso_y <- isoyear(d)
    if (iso_y != selected_year) {
      return(list(week = NA_integer_, issue = sprintf("Date is ISO-year %d (selected %d).", iso_y, selected_year), kind = "date"))
    }
    list(week = as.integer(isoweek(d)), issue = NULL, kind = "date")
  }
  
  format_label <- function(name, auto_wrap, width, allow_wrap = TRUE) {
    s <- as.character(name)
    
    if (!isTRUE(allow_wrap)) return(s)
    if (!isTRUE(auto_wrap))  return(s)
    
    # Keep rows that already contain LaTeX line breaks (\\) unchanged
    has_break <- grepl("\\\\\\\\", s)
    
    out <- s
    idx <- !has_break & !is.na(out) & nzchar(out)
    
    if (any(idx)) {
      wrapped <- stringr::str_wrap(out[idx], width = as.integer(width))
      wrapped <- gsub("\n", "\\\\", wrapped, fixed = TRUE)
      out[idx] <- wrapped
    }
    
    out
  }
  
  detect_delim <- function(path) {
    line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    n_comma <- stringr::str_count(line, ",")
    n_semi  <- stringr::str_count(line, ";")
    if (n_semi > n_comma) ";" else ","
  }
  
  get_delim <- function(path, choice) {
    if (is.null(choice) || choice == "" || choice == "auto") detect_delim(path) else choice
  }
  
  get_locale_for_delim <- function(delim) {
    # csv2 commonly uses decimal comma
    if (identical(delim, ";")) readr::locale(decimal_mark = ",") else readr::locale(decimal_mark = ".")
  }
  
  # ---------- Year-dependent UI constraints ----------
  
  maxw_year <- reactive({
    req(input$year)
    max_iso_week(as.integer(input$year))
  })
  
  observeEvent(list(input$year), {
    mw <- maxw_year()
    
    # Clamp current values
    ws <- min(max(1L, as.integer(input$w_start)), mw)
    we <- min(max(1L, as.integer(input$w_end)), mw)
    if (we < ws) we <- ws
    
    updateNumericInput(session, "w_start", min = 1, max = mw, value = ws)
    updateNumericInput(session, "w_end", min = 1, max = mw, value = we)
    updateSelectizeInput(session, "highlight_weeks", choices = as.character(1:mw), server = TRUE)
  }, ignoreInit = TRUE)
  
  # ---------- CSV ----------
  
  events_in <- reactive({
    req(input$year)
    year <- as.integer(input$year)
    mw <- maxw_year()
    mode <- input$date_mode %||% "auto"
    
    if (is.null(input$csv)) {
      return(tibble(
        row_id = integer(),
        category = character(),
        name = character(),
        color = character(),
        date_raw = character(),
        end_raw = character(),
        duration_weeks = integer(),
        week = integer(),
        end_week = integer(),
        issue = character()
      ))
    }
    
    path <- input$csv$datapath
    delim <- get_delim(path, input$csv_delim)
    loc <- get_locale_for_delim(delim)
    
    raw <- readr::read_delim(
      file = path,
      delim = delim,
      locale = loc,
      col_types = readr::cols(.default = readr::col_character()),
      trim_ws = TRUE,
      show_col_types = FALSE
    ) %>%
      mutate(
        row_id = row_number(),
        category = tolower(trimws(category)),
        name = as.character(name),
        color = ifelse(is.na(color) | trimws(color) == "", NA_character_, trimws(color)),
        date_raw = date,
        end_raw = end_date,
        duration_weeks = vapply(duration_weeks, parse_int_or_na, integer(1))
      )
    
    # Parse start weeks
    start_parsed <- lapply(raw$date_raw, parse_week_or_date_one, selected_year = year, mode = mode)
    start_week <- vapply(start_parsed, `[[`, integer(1), "week")
    start_issue <- vapply(start_parsed, function(z) if (is.null(z$issue)) "" else z$issue, character(1))
    missing_start <- is.na(start_week) & (is.na(raw$date_raw) | trimws(raw$date_raw) == "")
    start_issue[missing_start] <- "Missing start date/week."
    
    # Parse end weeks
    end_parsed <- lapply(raw$end_raw, parse_week_or_date_one, selected_year = year, mode = mode)
    end_week <- vapply(end_parsed, `[[`, integer(1), "week")
    end_issue <- vapply(end_parsed, function(z) if (is.null(z$issue)) "" else z$issue, character(1))
    
    # Derive end_week from duration_weeks if missing
    end_week_derived <- end_week
    needs_dur <- is.na(end_week_derived) & !is.na(start_week) & !is.na(raw$duration_weeks)
    end_week_derived[needs_dur] <- start_week[needs_dur] + pmax(1L, raw$duration_weeks[needs_dur]) - 1L
    
    # Issues: out of range or end before start
    range_issue <- rep("", nrow(raw))
    bad_start <- !is.na(start_week) & (start_week < 1 | start_week > mw)
    range_issue[bad_start] <- paste0(range_issue[bad_start], "Start week out of range. ")
    
    bad_end <- !is.na(end_week_derived) & (end_week_derived < 1 | end_week_derived > mw)
    range_issue[bad_end] <- paste0(range_issue[bad_end], "End week out of range. ")
    
    bad_order <- !is.na(start_week) & !is.na(end_week_derived) & end_week_derived < start_week
    range_issue[bad_order] <- paste0(range_issue[bad_order], "End before start. ")
    
    # Combine issues
    issue <- trimws(paste(start_issue, end_issue, range_issue))
    
    raw %>%
      transmute(
        row_id,
        category,
        name,
        color,
        date_raw,
        end_raw,
        duration_weeks,
        week = start_week,
        end_week = end_week_derived,
        issue
      )
  })
  
  diagnostics <- reactive({
    ev <- events_in()
    if (nrow(ev) == 0) return("No CSV loaded.")
    problems <- ev %>%
      filter(!is.na(issue) & issue != "") %>%
      mutate(msg = sprintf("Row %d (%s): %s [date=%s end_date=%s duration=%s]",
                           row_id, category, issue,
                           ifelse(is.na(date_raw), "", date_raw),
                           ifelse(is.na(end_raw), "", end_raw),
                           ifelse(is.na(duration_weeks), "", duration_weeks)))
    ok_n <- sum(is.na(ev$issue) | ev$issue == "")
    out <- c(
      sprintf("Parsed rows: %d OK, %d with issues.", ok_n, nrow(problems))
    )
    if (nrow(problems)) out <- c(out, problems$msg)
    paste(out, collapse = "\n")
  })
  
  output$diag <- renderText(diagnostics())
  
  prev_year <- reactiveVal(NULL)
  
  observeEvent(input$year, {
    new_year <- as.integer(input$year)
    new_max  <- max_iso_week(new_year)
    
    old_year <- prev_year()
    old_max  <- if (!is.null(old_year)) max_iso_week(as.integer(old_year)) else new_max
    
    ws <- as.integer(input$w_start)
    we <- as.integer(input$w_end)
    
    # If user was at the *final week* of the previous year, keep them at the final week of the new year
    if (!is.null(old_year) && !is.na(we) && we == old_max) {
      we <- new_max
    } else {
      we <- min(we, new_max)
    }
    
    ws <- min(max(1L, ws), new_max)
    if (we < ws) we <- ws
    
    # Keep valid highlights only
    hl <- suppressWarnings(as.integer(input$highlight_weeks))
    hl <- hl[!is.na(hl) & hl >= 1 & hl <= new_max]
    
    updateNumericInput(session, "w_start", min = 1, max = new_max, value = ws)
    updateNumericInput(session, "w_end",   min = 1, max = new_max, value = we)
    updateSelectizeInput(session, "highlight_weeks",
                         choices = as.character(1:new_max),
                         selected = as.character(hl),
                         server = TRUE)
    
    # Optional: keep center year synced only if it previously matched the old year (or was empty)
    if (is.null(old_year) || input$center_year == "" || input$center_year == as.character(old_year)) {
      updateTextInput(session, "center_year", value = as.character(new_year))
    }
    
    prev_year(new_year)
  }, ignoreInit = FALSE)
  
  # ---------- LaTeX builder ----------
  
  latex_code <- reactive({
    ev <- events_in()
    
    year <- as.integer(input$year)
    mw <- maxw_year()
    w0 <- as.integer(input$w_start)
    w1 <- as.integer(input$w_end)
    
    validate(need(w0 >= 1 && w1 >= w0, "Week range must be valid."))
    validate(need(w1 <= mw, sprintf("ISO year %d has %d weeks; end week must be <= %d.", year, mw, mw)))
    
    weeks <- w0:w1
    n_weeks <- length(weeks)
    
    wk_to_slice <- function(wk) as.integer(wk) - w0 + 1L
    
    weeks_txt <- wrap_commas(as.character(weeks), per = 20)
    
    hl <- sort(unique(as.integer(input$highlight_weeks)))
    hl <- hl[!is.na(hl) & hl >= w0 & hl <= w1]
    hl_slices <- wk_to_slice(hl)
    hl_line <- if (length(hl_slices)) {
      paste0("  arc data style{", paste(hl_slices, collapse = ","), "}={text color=brightred}")
    } else NULL
    
    dates_labels <- iso_week_dates(year, weeks)
    dates_txt <- wrap_commas(dates_labels, per = 14)
    
    months_txt <- paste(month.name, collapse = ",")
    
    ring_def <- list(
      activity   = list(r_in = 2.5, r_out = 4.3, title_rin = 4.4, title_rout = 4.8,
                        default_fill = "lightgreen", default_event_color = "darkgreen", title = "Activities"),
      funding    = list(r_in = 4.9, r_out = 6.7, title_rin = 6.8, title_rout = 7.1,
                        default_fill = "lightyellow", default_event_color = "darkyellow", title = "Funding"),
      conference = list(r_in = 7.2, r_out = 9.0, title_rin = 9.1, title_rout = 9.5,
                        default_fill = "lightblue", default_event_color = "darkblue", title = "Conferences")
    )
    
    gen_ring_block <- function(cat_key) {
      cfg <- ring_def[[cat_key]]
      
      ev_cat <- ev %>%
        filter(category == cat_key) %>%
        filter(is.na(issue) | issue == "") %>%
        mutate(
          start_wk = as.integer(week),
          end_wk_raw = as.integer(end_week),
          is_span = !is.na(end_wk_raw) & end_wk_raw > start_wk
        )
      
      # --- Point events ---
      pts <- ev_cat %>%
        filter(!is_span) %>%
        mutate(
          fill = ifelse(is.na(color), cfg$default_event_color, color),
          label_raw = format_label(name, input$auto_wrap, input$wrap_width, allow_wrap = FALSE),
          label = latex_escape(label_raw),
        ) %>%
        filter(!is.na(start_wk) & start_wk >= w0 & start_wk <= w1) %>%
        mutate(
          slice = wk_to_slice(start_wk)
        )
      
      # Winner color per slice: last row_id wins
      slice_fill <- pts %>%
        arrange(slice, row_id) %>%
        group_by(slice) %>%
        summarise(fill = dplyr::last(fill), .groups = "drop")
      
      styles <- slice_fill %>%
        group_by(fill) %>%
        summarise(slices = list(sort(unique(slice))), .groups = "drop")
      
      style_lines <- if (nrow(styles)) {
        paste(vapply(seq_len(nrow(styles)), function(i) {
          style_line(styles$slices[[i]], styles$fill[[i]])
        }, character(1)), collapse = ",\n")
      } else ""
      
      labels <- pts %>%
        arrange(slice, row_id) %>%
        group_by(slice) %>%
        summarise(
          txt = paste(label[!duplicated(label)], collapse = "\\\\"),
          .groups = "drop"
        ) %>%
        arrange(slice)
      
      label_lines <- if (nrow(labels)) {
        paste0("  wheel data{", labels$slice, "}=", labels$txt, collapse = ",\n")
      } else ""
      
      ring_main <- paste0(
        "% ", toupper(cat_key), "\n",
        "\\wheelchart[\n",
        "  radius={", cfg$r_in, "}{", cfg$r_out, "},\n",
        "  slices style={", cfg$default_fill, ",draw=lightgray}",
        if (nzchar(style_lines)) paste0(",\n", style_lines) else "",
        if (nzchar(label_lines)) paste0(",\n", label_lines) else "",
        ",\n  total count=", n_weeks, "\n",
        "]{ }\n\n"
      )
      
      ring_title <- paste0(
        "\\wheelchart[\n",
        "  arc data=", cfg$title, ",\n",
        "  arc data style={text color=gray},\n",
        "  radius={", cfg$title_rin, "}{", cfg$title_rout, "},\n",
        "  slices style={fill=none},\n",
        "  total count=6\n",
        "]{ }\n\n"
      )
      
      # --- Spans: funding only (stacked sub-bands) ---
      span_blocks <- ""
      if (cat_key == "funding") {
        spans <- ev_cat %>%
          filter(is_span) %>%
          mutate(
            fill = ifelse(is.na(color), cfg$default_event_color, color),
            label_raw = format_label(name, input$auto_wrap, input$wrap_width),
            label = latex_escape(label_raw),
            
            start_wk_clip = pmax(w0, start_wk),
            end_wk_clip   = pmin(w1, end_wk_raw)
          ) %>%
          filter(!is.na(start_wk_clip), !is.na(end_wk_clip), end_wk_clip >= start_wk_clip) %>%
          arrange(row_id) %>%
          mutate(
            start_slice = wk_to_slice(start_wk_clip),
            end_slice   = wk_to_slice(end_wk_clip)
          )
        
        if (nrow(spans) > 0) {
          band_count <- 3
          total_thick <- cfg$r_out - cfg$r_in
          band_thick <- total_thick / band_count
          
          for (i in seq_len(nrow(spans))) {
            band_idx <- (i - 1) %% band_count
            rin <- cfg$r_in + band_idx * band_thick
            rout <- rin + band_thick
            span_blocks <- paste0(
              span_blocks,
              "\\wheelchart[\n",
              "  arc data=\\WCvarA,\n",
              "  domain/.expanded=\\wcdomain{", spans$start_slice[i], "/", n_weeks, "}{", spans$end_slice[i], "/", n_weeks, "},\n",
              "  radius={", rin, "}{", rout, "},\n",
              "  slices style=", spans$fill[i], "\n",
              "]{", spans$label[i], "}\n\n"
            )
          }
        }
      }
      
      paste0(ring_main, span_blocks, ring_title)
    }
    
    header <- paste0(
      "\\documentclass[border=6pt]{standalone}\n",
      "\\usepackage[utf8]{inputenc}\n",
      "\\usepackage{wheelchart}\n",
      "\\usepackage{anyfontsize}\n",
      "\\usepackage{xfp}\n",
      "\\usetikzlibrary{decorations.text}\n\n",
      "\\begin{document}\n",
      "\\begin{tikzpicture}\n",
      "\\sffamily\n\n",
      "% colors\n",
      "\\definecolor{lightgreen}{RGB}{215 238 237}\n",
      "\\definecolor{darkgreen}{RGB}{84 184 180}\n",
      "\\definecolor{lightblue}{RGB}{215 229 240}\n",
      "\\definecolor{midblue}{RGB}{152 188 217}\n",
      "\\definecolor{darkblue}{RGB}{88 147 193}\n",
      "\\definecolor{darkpurple}{RGB}{152 121 206}\n",
      "\\definecolor{midpurple}{RGB}{191 163 236}\n",
      "\\definecolor{lightyellow}{RGB}{254 249 238}\n",
      "\\definecolor{darkyellow}{RGB}{252 239 205}\n",
      "\\definecolor{lightred}{RGB}{240 220 221}\n",
      "\\definecolor{darkred}{RGB}{195 109 114}\n",
      "\\definecolor{brightred}{RGB}{220,53,69}\n",
      "\\definecolor{white}{RGB}{255 255 255}\n",
      "\\definecolor{labelgray}{RGB}{102,102,102}\n\n",
      "\\def\\MonthGap{2}\n",
      "\\pgfmathsetmacro{\\TotalDeg}{360 - \\MonthGap}\n",
      "\\pgfmathsetmacro{\\StartAng}{450 - \\MonthGap/2}\n",
      "\\pgfmathsetmacro{\\EndAng}{90  + \\MonthGap/2}\n\n",
      "\\newcommand{\\wcang}[1]{\\fpeval{\\StartAng - \\TotalDeg*(#1)}}\n",
      "\\newcommand{\\wcdomain}[2]{\\wcang{#1}:\\wcang{#2}}\n\n",
      "\\pgfkeys{\n",
      "  /wheelchart,\n",
      "  arc data dir={\\WCmidangle>180?-1:1},\n",
      "  arc data expand=f,\n",
      "  arc data pos=0.5,\n",
      "  arc data sep=0,\n",
      "  data=,\n",
      "  domain=\\StartAng:\\EndAng,\n",
      "  plot={{#1}:{#2+0*(450-#1)}},\n",
      "  value=1,\n",
      "  wheel data pos=0.5,\n",
      "  wheel data style={align=center,rotate=\\WCmidangle+(\\WCmidangle<90?0:(\\WCmidangle<270?180:0))}\n",
      "}\n\n",
      "% INNER CIRCLE\n",
      "\\draw[lightgray, line width=0.6pt, line cap=round]\n",
      "  (0,0) ++(\\StartAng:2.4)\n",
      "  arc[start angle=\\StartAng, end angle=\\EndAng, radius=2.4];\n\n"
    )
    
    block_activity   <- gen_ring_block("activity")
    block_funding    <- gen_ring_block("funding")
    block_conference <- gen_ring_block("conference")
    
    outer_circle <- paste0(
      "\\draw[lightgray, line width=0.6pt, line cap=round]\n",
      "  (0,0) ++(\\StartAng:9.6)\n",
      "  arc[start angle=\\StartAng, end angle=\\EndAng, radius=9.6];\n\n"
    )
    
    dates_ring <- paste0(
      "\\wheelchart[\n",
      "  arc data=\\WCvarA{--}\\WCvarB,\n",
      "  radius={9.7}{10.1},\n",
      "  slices style={fill=none}\n",
      "]{", dates_txt, "}\n\n"
    )
    
    weeks_ring <- paste0(
      "\\wheelchart[\n",
      "  arc data=\\WCvarA,\n",
      "  radius={10.1}{10.5},\n",
      "  slices style={fill=none}",
      if (!is.null(hl_line)) paste0(",\n", hl_line) else "",
      "\n]{", weeks_txt, "}\n\n"
    )
    
    months_ring <- paste0(
      "\\wheelchart[\n",
      "  arc data=\\WCvarA,\n",
      "  radius={10.6}{11.1},\n",
      "  gap polar=-4,\n",
      "  gap radius=0.02,\n",
      "  middle={\\fontsize{48}{60}\\selectfont", latex_escape(input$center_year), "},\n",
      "  slices arrow={1}{0},\n",
      "  slices style={fill=none,draw=lightgray}\n",
      "]{", months_txt, "}\n"
    )
    
    footer <- "\n\\end{tikzpicture}\n\\end{document}\n"
    
    paste0(header, block_activity, block_funding, block_conference,
           outer_circle, dates_ring, weeks_ring, months_ring, footer)
  })
  
  output$latex <- renderText(latex_code())
  
  # ---------- Copy / Download .tex ----------
  
  observeEvent(input$copy_btn, {
    session$sendCustomMessage("copyToClipboard", latex_code())
  })
  
  output$download_tex <- downloadHandler(
    filename = function() sprintf("year_wheel_%d_w%02d-%02d.tex", input$year, input$w_start, input$w_end),
    content = function(file) writeLines(latex_code(), file, useBytes = TRUE)
  )
  
  # ---------- Compile with MiKTeX ----------
  
  build <- reactiveValues(log = "", last_pdf = NULL)
  
  compile_tex_to_pdf <- function(tex_string, jobname) {
    wd <- tempdir()
    tex_path <- file.path(wd, paste0(jobname, ".tex"))
    writeLines(tex_string, tex_path, useBytes = TRUE)
    
    owd <- setwd(wd)
    on.exit(setwd(owd), add = TRUE)
    
    latexmk <- Sys.which("latexmk")
    pdflatex <- Sys.which("pdflatex")
    
    log <- character()
    
    if (nzchar(latexmk)) {
      log <- c(log, paste0("> latexmk -pdf ", jobname, ".tex"))
      out <- tryCatch(
        system2(latexmk, c("-pdf", "-interaction=nonstopmode", "-halt-on-error", "-quiet", paste0(jobname, ".tex")),
                stdout = TRUE, stderr = TRUE),
        error = function(e) paste("latexmk error:", conditionMessage(e))
      )
      log <- c(log, out)
    } else if (nzchar(pdflatex)) {
      for (pass in 1:2) {
        log <- c(log, paste0("> pdflatex (pass ", pass, ") ", jobname, ".tex"))
        out <- tryCatch(
          system2(pdflatex, c("-interaction=nonstopmode", "-halt-on-error", paste0(jobname, ".tex")),
                  stdout = TRUE, stderr = TRUE),
          error = function(e) paste("pdflatex error:", conditionMessage(e))
        )
        log <- c(log, out)
      }
    } else {
      stop("No LaTeX engine found in PATH. Make sure MiKTeX bin is on PATH (e.g., C:\\\\Program Files\\\\MiKTeX\\\\miktex\\\\bin\\\\x64).")
    }
    
    pdf_path <- file.path(wd, paste0(jobname, ".pdf"))
    if (!file.exists(pdf_path)) {
      stop(paste("Compilation failed. See log.\n", paste(log, collapse = "\n")))
    }
    list(pdf = pdf_path, log = paste(log, collapse = "\n"))
  }
  
  observeEvent(input$compile_btn, {
    job <- sprintf("year_wheel_%d_w%02d-%02d", input$year, input$w_start, input$w_end)
    res <- tryCatch(
      compile_tex_to_pdf(latex_code(), job),
      error = function(e) list(pdf = NULL, log = conditionMessage(e))
    )
    build$last_pdf <- res$pdf
    build$log <- res$log
  })
  
  output$build_log <- renderText(build$log)
  
  output$download_pdf <- downloadHandler(
    filename = function() sprintf("year_wheel_%d_w%02d-%02d.pdf", input$year, input$w_start, input$w_end),
    content = function(file) {
      job <- sprintf("year_wheel_%d_w%02d-%02d", input$year, input$w_start, input$w_end)
      res <- compile_tex_to_pdf(latex_code(), job)
      file.copy(res$pdf, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)
