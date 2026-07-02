# ============================================================================
# skip_logic.R  —  Module C conditional display + row visibility helpers
# ----------------------------------------------------------------------------
# surveydown gives us two mechanisms:
#   * sd_show_if(cond ~ "qid")   — show/hide a whole question on a page
#   * sd_skip_if(cond ~ "page")  — jump to another page
# and per-row visibility inside the count grids is handled by passing a
# `visible_rows` reactive to register_count_grid().
#
# IMPORTANT (how surveydown evaluates conditions): sd_show_if/sd_skip_if capture
# the LHS of each formula UNEVALUATED and re-evaluate it reactively in the
# server's environment (see evaluate_condition() in the surveydown namespace).
# It discovers reactive dependencies by walking the expression for sd_value()/
# input$ references, recursing into any function it finds defined in that env.
# => The predicate helpers below (has_b1, has_b2, yn, ...) are safe to use
#    INSIDE the formulas passed to sd_show_if()/sd_skip_if() in app.R, because
#    they call sd_value() internally and are defined in the server scope.
#
# The conditions are transcribed from spec/enabling_conditions.csv (the client's
# "Enabling conditions" table) plus the inline "Programmer: If ..." skip notes.
# B1/B2 are multi-select, so "includes" semantics use has_b1()/has_b2().
#
# Exports:
#   has_b1/has_b2/b1_only/yn      -> predicates for inline conditions in app.R
#   module_c_row_visibility(qid)  -> reactive giving visible item codes for a grid
# ============================================================================

# NOTE: DO NOT reintroduce sd_value()-based predicate helpers here.
# We tried has_b1()/has_b2()/yn()/answered() calling sd_value() and they SILENTLY
# returned NULL/FALSE inside sd_show_if()/sd_skip_if(), turning the whole rule set
# into a no-op (sd_value() needs `all_data` in the caller's lexical scope, which a
# sourced helper does not have). The Module-C show_if/skip_if rules therefore live
# INLINE in app.R and read `input` directly through locally-defined closures
# (inc/is1/c8/ans). See app.R :: server() for the authoritative rule set.
# The only survey-logic exported from this file is the per-row visibility below,
# which reads answers through a `getval` accessor the server supplies.

# ---------------------------------------------------------------------------
# Per-row visibility for a grid: evaluate each item's `show_if` string against
# the current answers. Returns a reactive -> character vector of visible codes.
#
# IMPORTANT (scoping): sd_value() requires `all_data` to be in the caller's
# lexical scope, which only holds for code whose environment chain reaches the
# sd_server() body. These helpers are sourced into app.R and so DON'T see
# all_data — calling sd_value() here silently returns NULL and hides every
# conditional row. So visibility reads answers through a `getval(id)` accessor
# that the server supplies (it closes over `input`, where reads always work).
#
# Row show_if grammar (kept deliberately small):
#   "B1 includes N"  -> N in getval("B1")
#   "B2 includes N"  -> N in getval("B2")
#   "CX == 1"        -> getval("CX") == "1"
#   "A && B"         -> conjunction of the above
#   NA               -> always shown
# `getval(id)` must return a character vector of the selected codes (empty/NULL
# if unanswered). See app.R :: mk_getval().
# ---------------------------------------------------------------------------
.eval_row_cond <- function(cond, getval) {
  if (is.na(cond) || !nzchar(cond)) return(TRUE)
  incl <- function(id, code) as.character(code) %in% as.character(getval(id))
  eq1  <- function(id) { v <- getval(id); length(v) == 1 && as.character(v) == "1" }
  parts <- trimws(strsplit(cond, "&&", fixed = TRUE)[[1]])
  all(vapply(parts, function(p) {
    if (grepl("^B1 includes ", p)) return(incl("B1", sub("^B1 includes ", "", p)))
    if (grepl("^B2 includes ", p)) return(incl("B2", sub("^B2 includes ", "", p)))
    m <- regmatches(p, regexec("^([A-Z0-9]+)\\s*==\\s*1$", p))[[1]]
    if (length(m) == 2) return(eq1(m[2]))
    FALSE   # unknown clause -> hide (fail closed)
  }, logical(1)))
}

# getval: an accessor (id -> character vector of selected codes). Supplied by
# the server so reads go through `input` (correct scope).
module_c_row_visibility <- function(qid, getval) {
  shiny::reactive({
    q <- QBANK[[qid]]
    codes <- vapply(q$items, `[[`, character(1), "item")
    conds <- vapply(q$items, function(it) it$show_if %||% NA_character_, character(1))
    keep  <- vapply(conds, function(cond) .eval_row_cond(cond, getval), logical(1))
    codes[keep]
  })
}

# null-coalescing helper (surveydown depends on rlang which exports %||%, but
# define locally to avoid an explicit import).
`%||%` <- function(a, b) if (is.null(a)) b else a
