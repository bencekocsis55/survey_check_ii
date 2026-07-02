# ============================================================================
# app.R — Grantee Research Competencies Self-Assessment (surveydown)
# Research Competencies Self-Assessment — Modules A, B, C.
# ----------------------------------------------------------------------------
# Run from RStudio:  open this file, click "Run App"  (or shiny::runApp())
# In `mode: preview` (set in survey.qmd) responses save LOCALLY — no AWS / no
# Postgres needed. To collect real data later: run sd_db_config() once, then
# set `mode: database` in survey.qmd's YAML.
# ============================================================================

library(surveydown)
library(shiny)

# Project logic (grids, piping, skip predicates, question bank) --------------
source("R/question_bank.R", local = TRUE)  # QBANK, QBANK_MATRIX, PROF_LEVELS
source("R/piping.R",        local = TRUE)  # pipe_q()
source("R/skip_logic.R",    local = TRUE)  # has_b1/has_b2/yn, module_c_row_visibility
source("R/count_grid.R",    local = TRUE)  # register_count_grid()

# Database (preview mode -> local storage) -----------------------------------
db <- sd_db_connect()

# UI --------------------------------------------------------------------------
ui <- sd_ui()

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  # --- reactive drivers -----------------------------------------------------
  b3_reactive <- reactive({
    v <- sd_value("B3")
    n <- suppressWarnings(as.integer(v))
    if (length(n) != 1 || is.na(n) || n < 1) NA_integer_ else n
  })

  # --- answer accessor (correct scope) --------------------------------------
  # Reads answers straight from `input` so row-visibility + piping don't depend
  # on sd_value()'s all_data scope (which sourced helpers can't see). surveydown
  # stores a multi-select as a character vector on input$<id>.
  getval <- function(id) {
    v <- input[[id]]
    if (is.null(v)) character(0) else as.character(v)
  }

  # --- Module C count grids -------------------------------------------------
  # Each grid is registered once; rows react to B1/B2/B3 via visible_rows.
  for (qid in names(QBANK)) {
    local({
      this <- qid
      register_count_grid(
        input, output, session,
        qid          = this,
        b3_reactive  = b3_reactive,
        visible_rows = module_c_row_visibility(this, getval),
        getval       = getval
      )
    })
  }

  # --- Module C matrix (yes/no per row) questions: C8, C14 ------------------
  # Rendered as native surveydown matrix; rows gate on B1 (handled by hiding
  # the whole matrix if no rows apply — per-row hiding within a matrix is a
  # known surveydown limitation).
  render_matrix <- function(mid) {
    m <- QBANK_MATRIX[[mid]]
    observe({
      rows <- Filter(function(r) .eval_row_cond(r$show_if, getval), m$rows)
      req(length(rows) > 0)
      sd_question(
        type  = "matrix",
        id    = mid,
        label = paste0("<strong>", m$title, "</strong>"),
        row   = stats::setNames(
                  vapply(rows, `[[`, character(1), "item"),
                  vapply(rows, function(r) pipe_q(r$label, getval), character(1))),
        option = c("Yes" = "1", "No" = "0")
      )
    })
  }
  render_matrix("C8")
  render_matrix("C14")

  # --- Conditional DISPLAY (show a question only if condition holds) --------
  # IMPORTANT: sd_show_if evaluates each condition in THIS server environment,
  # but if a condition calls a sourced helper (has_b1/yn), the sd_value() inside
  # that helper runs in the helper's frame where `all_data` is not visible and
  # silently returns NULL -> the whole rule set becomes a no-op. So the show_if
  # conditions read `input` DIRECTLY via these locally-defined closures (input
  # is in scope here, and stays in scope because they're defined in the server).
  inc  <- function(id, code) as.character(code) %in% as.character(input[[id]])   # multi-select "includes"
  is1  <- function(id) { v <- input[[id]]; !is.null(v) && as.character(v) == "1" } # yes/no == 1
  # C8 matrix rows are stored as separate inputs "C8_C8a", "C8_C8b", "C8_C8c".
  c8   <- function(row) is1(paste0("C8_", row))

  sd_show_if(
    # C3–C7 require C2 = Yes ("experience designing studies")
    is1("C2")                    ~ "C3",
    is1("C2") && inc("B1", "1")  ~ "C4",   # qualitative design (qual in scope)
    is1("C2") && inc("B1", "2")  ~ "C5",   # quantitative design (quant in scope)
    is1("C2") && inc("B1", "3")  ~ "C6",   # review design (review in scope)
    is1("C2")                    ~ "C7",

    # Sampling considerations (C8 rows -> downstream grids)
    c8("C8a") || c8("C8b")       ~ "C9",   # quant sampling considerations
    c8("C8b")                    ~ "C12",  # other quant sampling considerations
    c8("C8c")                    ~ "C13"   # qualitative sampling considerations
  )

  # --- Conditional SKIP (jump pages) ----------------------------------------
  # Same scoping rule as show_if: read `input` directly (inline closures), not
  # sourced helpers. Skip rules are evaluated on EVERY page advance, so each
  # must be guarded with ans() on its gate — otherwise a "No/absent" condition
  # is TRUE before the gate exists and the survey skips from page 1.
  ans <- function(id) { v <- input[[id]]; !is.null(v) && any(nzchar(as.character(v))) }

  sd_skip_if(
    # C2 = No -> skip the study-design grids straight to sampling
    (ans("C2") && !is1("C2"))                                       ~ "c_sampling",
    # No quant sampling experience but qual only -> skip quant grids to prob page
    (ans("C8_C8a") && ans("C8_C8b") && ans("C8_C8c") &&
       !c8("C8a") && !c8("C8b") && c8("C8c"))                       ~ "c_sampling_prob"
  )

  # --- run --------------------------------------------------------------------
  sd_server(db = db)
}

shiny::shinyApp(ui = ui, server = server)
