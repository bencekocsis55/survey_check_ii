# ============================================================================
# count_grid.R  —  Custom "number of staff per proficiency level" grid widget
# ----------------------------------------------------------------------------
# The dominant Module C question type. For a given question (e.g. C1) the
# respondent sees a table:
#
#     Competency item            | Entry | Proficiency | Mastery
#     ---------------------------+-------+-------------+--------
#     Reviewing literature ...   | [ n ] |    [ n ]    |  [ n ]
#     Drafting quant questions   | [ n ] |    [ n ]    |  [ n ]
#
# Each cell is a numericInput (0..B3). Constraint: each ROW must sum to <= B3
# (you can't have more staff at some level than exist on the team). A live
# per-row running total + an inline warning enforce this.
#
# Storage: the whole grid is serialized to a single JSON string and stored as
# one response column named after the question id (e.g. "C1"), via
# sd_question_custom(). scoring.R parses that JSON back into per-item counts.
# One column per grid keeps the DB schema stable regardless of skip logic.
#
# Public entry point:  register_count_grid(qid, b3_reactive, show_row = NULL)
#   - call once per grid, inside the Shiny server()
#   - qid           : question id present in QBANK (e.g. "C1")
#   - b3_reactive   : a reactive returning the current B3 (# key staff), or NULL
#   - visible_rows  : optional reactive -> character vector of item codes to show
#                     (implements per-row "Display if..." gating). NULL = all rows.
# In survey.qmd render it with:  sd_output(qid, type = "question")
# ============================================================================

# --- serialize the current grid inputs into a named list ------------------
.grid_collect <- function(input, qid, item_codes) {
  out <- list()
  for (code in item_codes) {
    for (lv in names(PROF_LEVELS)) {
      key <- paste0(qid, "__", code, "_", lv)     # input id for one cell
      v <- input[[key]]
      out[[paste0(code, "_", lv)]] <- if (is.null(v) || is.na(v)) 0L else as.integer(v)
    }
  }
  out
}

# --- build the HTML table of numeric inputs -------------------------------
.grid_ui <- function(qid, items, b3, visible_codes, getval = NULL) {
  ns_cell <- function(code, lv) paste0(qid, "__", code, "_", lv)
  maxv <- if (is.null(b3) || is.na(b3) || b3 < 1) 99 else b3

  header <- shiny::tags$tr(
    shiny::tags$th(style = "text-align:left; width:52%;", ""),
    lapply(unname(PROF_LEVELS), function(h) shiny::tags$th(style = "text-align:center;", h)),
    shiny::tags$th(style = "text-align:center; width:8%;", "Row total")
  )

  rows <- lapply(items, function(it) {
    if (!it$item %in% visible_codes) return(NULL)         # per-row skip logic
    label <- pipe_q(it$label, getval)                     # [q] -> quant/qual
    cells <- lapply(names(PROF_LEVELS), function(lv) {
      shiny::tags$td(
        style = "text-align:center;",
        shiny::numericInput(ns_cell(it$item, lv), label = NULL,
                            value = NA, min = 0, max = maxv, step = 1, width = "80px")
      )
    })
    total_out <- shiny::tags$td(
      style = "text-align:center; font-weight:600;",
      shiny::textOutput(paste0(qid, "__", it$item, "_rowtotal"), inline = TRUE)
    )
    shiny::tags$tr(shiny::tags$td(shiny::HTML(label)), cells, total_out)
  })

  shiny::tagList(
    shiny::div(
      class = "count-grid-help",
      style = "font-size:0.9em; color:#555; margin-bottom:6px;",
      sprintf("Enter the number of key staff at each level. Each row may total at most %s (the number of key staff).",
              if (is.null(b3) || is.na(b3)) "the team size" else b3)
    ),
    shiny::tags$table(
      class = "table table-sm count-grid-table",
      style = "border-collapse:collapse;",
      shiny::tags$thead(header),
      shiny::tags$tbody(rows)
    ),
    shiny::div(id = paste0(qid, "__warn"),
               style = "color:#c0392b; font-weight:600; margin-top:4px;",
               shiny::textOutput(paste0(qid, "__warnmsg")))
  )
}

# --- register one grid inside server() ------------------------------------
register_count_grid <- function(input, output, session, qid,
                                 b3_reactive = NULL, visible_rows = NULL,
                                 getval = NULL) {
  q <- QBANK[[qid]]
  if (is.null(q)) stop(sprintf("count_grid: unknown qid '%s'", qid))
  item_codes <- vapply(q$items, `[[`, character(1), "item")

  # Reactive: which rows are visible (default all).
  vis <- if (is.null(visible_rows)) shiny::reactive(item_codes) else visible_rows

  # Reactive: current B3 cap.
  b3 <- if (is.null(b3_reactive)) shiny::reactive(NA_integer_) else b3_reactive

  # Render the grid UI, rebuilding when B3 or visible rows change.
  output[[paste0(qid, "_grid_ui")]] <- shiny::renderUI({
    .grid_ui(qid, q$items, b3(), vis(), getval)
  })

  # Per-row running totals + over-cap detection.
  for (it in q$items) {
    local({
      code <- it$item
      output[[paste0(qid, "__", code, "_rowtotal")]] <- shiny::renderText({
        s <- sum(vapply(names(PROF_LEVELS), function(lv) {
          v <- input[[paste0(qid, "__", code, "_", lv)]]
          if (is.null(v) || is.na(v)) 0L else as.integer(v)
        }, integer(1)))
        as.character(s)
      })
    })
  }

  # Aggregate warning if any visible row exceeds B3.
  output[[paste0(qid, "__warnmsg")]] <- shiny::renderText({
    cap <- b3()
    if (is.null(cap) || is.na(cap) || cap < 1) return("")
    bad <- character(0)
    for (code in vis()) {
      s <- sum(vapply(names(PROF_LEVELS), function(lv) {
        v <- input[[paste0(qid, "__", code, "_", lv)]]
        if (is.null(v) || is.na(v)) 0L else as.integer(v)
      }, integer(1)))
      if (s > cap) bad <- c(bad, code)
    }
    if (length(bad))
      sprintf("Some rows total more than %d key staff (%s). Please revise.",
              cap, paste(bad, collapse = ", "))
    else ""
  })

  # Serialize the whole grid to JSON and expose it as the stored value.
  grid_value <- shiny::reactive({
    collected <- .grid_collect(input, qid, vis())
    jsonlite::toJSON(collected, auto_unbox = TRUE)
  })

  # Register the hidden storage question. The visible output is our grid UI.
  sd_question_custom(
    id     = qid,
    label  = paste0("<strong>", pipe_q(q$title, getval), "</strong><br>",
                    "<span style='font-weight:400;'>", pipe_q(q$prompt, getval), "</span>"),
    output = shiny::uiOutput(paste0(qid, "_grid_ui")),
    value  = grid_value
  )

  invisible(TRUE)
}
