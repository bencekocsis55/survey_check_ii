# ============================================================================
# app.R — Research Competencies Self-Assessment (surveydown)
# Modules A, B, C, D.
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
source("R/routing.R",       local = TRUE)  # rt_* token/invite routing (Task A)

# Public base URL of THIS deployed app — used to build tokenized invite links.
# Placeholder until the Connect Cloud Content URL is confirmed; find/replace here.
APP_BASE_URL <- Sys.getenv("APP_BASE_URL",
                           "https://YOUR-CONNECT-CLOUD-APP-URL")

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

  # --- Respondent routing (Task A) ------------------------------------------
  # Resolve ?rid=<token> to a respondent, tag responses with their identity, and
  # expose whether this session is the org LEADER (who gets the nominate step).
  # Degrades gracefully: in preview mode (db is NULL) routing is inert and the
  # survey runs standalone.
  rt_con <- if (is.list(db) && !is.null(db$db)) db$db else NULL
  respondent <- reactive({
    pars <- tryCatch(sd_get_url_pars(), error = function(e) NULL)
    tok  <- if (!is.null(pars) && !is.null(pars$rid)) as.character(pars$rid) else ""
    if (is.null(rt_con) || !nzchar(tok)) return(NULL)
    tryCatch(rt_lookup(rt_con, tok), error = function(e) NULL)
  })
  is_leader <- reactive({ r <- respondent(); !is.null(r) && identical(r$role, "leader") })

  # Tag every response row with the respondent's identity (if known).
  observe({
    r <- respondent()
    if (is.null(r)) return()
    sd_store_value(r$token, "rid")
    sd_store_value(r$email, "respondent_email")
    sd_store_value(r$org %||% "", "org")
    if (!is.null(rt_con)) tryCatch(rt_set_status(rt_con, r$token, "started"),
                                   error = function(e) NULL)
  })

  # Leader-only nominate step: on submit, create nominee tokens + invites.
  observeEvent(input$rt_nominate_btn, {
    if (!is_leader() || is.null(rt_con)) return()
    emails <- strsplit(input$rt_nominate_emails %||% "", "[,;\\s]+")[[1]]
    emails <- emails[nzchar(emails)]
    if (!length(emails)) return()
    r <- respondent()
    df <- tryCatch(
      rt_nominate(rt_con, r$token, emails, base_url = APP_BASE_URL,
                  send_mode = "simulate"),
      error = function(e) NULL)
    output$rt_nominate_result <- renderUI({
      if (is.null(df)) return(shiny::span("Could not create invites."))
      shiny::tagList(
        shiny::p(sprintf("Created %d invite(s):", nrow(df))),
        shiny::tags$ul(lapply(seq_len(nrow(df)), function(i)
          shiny::tags$li(shiny::strong(df$email[i]), ": ",
                         shiny::tags$code(df$link[i])))))
    })
  })

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
  # Module D yes/no matrices.
  render_matrix("D1")
  render_matrix("D2")
  render_matrix("D4")
  render_matrix("D6")

  # --- Conditional DISPLAY (show a question only if condition holds) --------
  # IMPORTANT: sd_show_if evaluates each condition in THIS server environment,
  # but if a condition calls a sourced helper (has_b1/yn), the sd_value() inside
  # that helper runs in the helper's frame where `all_data` is not visible and
  # silently returns NULL -> the whole rule set becomes a no-op. So the show_if
  # conditions read `input` DIRECTLY via these locally-defined closures (input
  # is in scope here, and stays in scope because they're defined in the server).
  inc  <- function(id, code) as.character(code) %in% as.character(input[[id]])   # multi-select "includes"
  is1  <- function(id) { v <- input[[id]]; !is.null(v) && as.character(v) == "1" } # yes/no == 1
  # Matrix rows are stored as separate inputs "<mid>_<row>", e.g. "C8_C8a".
  mrow <- function(mid, row) is1(paste0(mid, "_", row))
  c8   <- function(row) mrow("C8", row)   # back-compat alias for Module C

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
    # (Module D D1 row visibility is handled inside render_matrix via the
    #  per-row show_if in QBANK_MATRIX, same as C8/C14 — not here.)
  )

  # --- Conditional SKIP (jump pages) ----------------------------------------
  # Same scoping rule as show_if: read `input` directly (inline closures), not
  # sourced helpers. Skip rules are evaluated on EVERY page advance, so each
  # must be guarded with ans() on its gate — otherwise a "No/absent" condition
  # is TRUE before the gate exists and the survey skips from page 1.
  ans <- function(id) { v <- input[[id]]; !is.null(v) && any(nzchar(as.character(v))) }

  sd_skip_if(
    # Non-leaders (incl. anyone with no/unknown token) skip the nominate page.
    # Fires when advancing from a_internal; the nominate page sits between
    # a_internal and b_proposed, so non-leaders jump straight to b_proposed.
    (!is_leader())                                                  ~ "b_proposed",

    # C2 = No -> skip the study-design grids straight to sampling
    (ans("C2") && !is1("C2"))                                       ~ "c_sampling",
    # No quant sampling experience but qual only -> skip quant grids to prob page
    (ans("C8_C8a") && ans("C8_C8b") && ans("C8_C8c") &&
       !c8("C8a") && !c8("C8b") && c8("C8c"))                       ~ "c_sampling_prob",

    # --- Module D: each D1 sub-section page is skipped if its D1 row = No.
    # Guarded by ans() on the row so the rule can't fire before D1 is answered.
    (ans("D1_D1a") && !mrow("D1", "D1a"))                           ~ "d_surveys",
    (ans("D1_D1b") && !mrow("D1", "D1b"))                           ~ "d_protocols",
    (ans("D1_D1c") && !mrow("D1", "D1c"))                           ~ "end"
  )

  # --- run --------------------------------------------------------------------
  sd_server(db = db)
}

shiny::shinyApp(ui = ui, server = server)
