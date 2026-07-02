# ============================================================================
# routing.R  —  Multi-respondent invite / tokenized-link routing (Task A)
# ----------------------------------------------------------------------------
# Flow:
#   1. An org "leader" is seeded with a token; they open  <app-url>?rid=<token>.
#   2. The leader nominates colleagues (emails). Each nominee gets a token + an
#      invite row in the `respondents` table.
#   3. Each nominee opens <app-url>?rid=<their-token> and completes the survey;
#      their responses are tagged with their rid + email (via sd_store_value()).
#   4. When all of an org's respondents are `complete`, the team report runs
#      over sd_get_data() filtered to that org.
#
# SEND_MODE = "simulate" (this build): NO real email is sent. Instead each
# invite's rendered email + tokenized link is written to outputs/invites/ and to
# the `respondents` table, so links can be inspected and clicked by hand. A
# future "smtp" mode can send the same rendered body via Gmail SMTP.
#
# Design choices:
#   - Tokens are deterministic-per-(email,salt) so the test harness is
#     reproducible WITHOUT Math.random()/Sys.time() (which the workflow env and
#     resume both dislike); in production swap in a random UUID.
#   - The respondents table is separate from the surveydown responses table, so
#     none of the tested survey logic changes.
# ============================================================================

RESP_TABLE <- "respondents"

# --- token generation (deterministic, reproducible) ------------------------
# A short hex token derived from email + org + a salt. Not secret-grade, but
# fine for a routing test; production should use a random UUID + store a hash.
rt_token <- function(email, org = "", salt = "mnk43") {
  raw <- paste(tolower(trimws(email)), tolower(trimws(org)), salt, sep = "|")
  substr(digest::digest(raw, algo = "sha256"), 1, 16)
}

# --- ensure the respondents table exists -----------------------------------
rt_ensure_table <- function(con) {
  DBI::dbExecute(con, sprintf('
    create table if not exists "%s" (
      token         text primary key,
      email         text not null,
      org           text,
      role          text not null default \'nominee\',   -- leader | nominee
      nominated_by  text,                                -- token of the leader
      status        text not null default \'invited\',   -- invited | started | complete
      created_at    timestamptz default now()
    )', RESP_TABLE))
  invisible(TRUE)
}

# --- create / upsert one respondent ----------------------------------------
rt_add_respondent <- function(con, email, org, role = "nominee",
                              nominated_by = NA_character_, salt = "mnk43") {
  tok <- rt_token(email, org, salt)
  DBI::dbExecute(con, sprintf('
    insert into "%s" (token, email, org, role, nominated_by, status)
    values ($1,$2,$3,$4,$5,\'invited\')
    on conflict (token) do update set email=excluded.email, org=excluded.org,
      role=excluded.role, nominated_by=excluded.nominated_by', RESP_TABLE),
    params = list(tok, tolower(trimws(email)), org, role,
                  if (is.na(nominated_by)) NA else nominated_by))
  tok
}

# --- look up a respondent by token -----------------------------------------
rt_lookup <- function(con, token) {
  if (is.null(token) || !nzchar(token)) return(NULL)
  df <- DBI::dbGetQuery(con, sprintf('select * from "%s" where token = $1', RESP_TABLE),
                        params = list(token))
  if (nrow(df) == 0) NULL else as.list(df[1, ])
}

# --- mark status -----------------------------------------------------------
rt_set_status <- function(con, token, status) {
  DBI::dbExecute(con, sprintf('update "%s" set status=$1 where token=$2', RESP_TABLE),
                 params = list(status, token))
  invisible(TRUE)
}

# --- render the invite email body (same text for simulate + real send) -----
rt_render_email <- function(email, token, base_url, org, from_leader = NA) {
  link <- paste0(base_url, "?rid=", token)
  who  <- if (!is.na(from_leader)) sprintf("Your colleague (%s)", from_leader) else "The research team"
  body <- paste0(
    "Subject: You've been invited to a Research Competencies Self-Assessment\n\n",
    "Hello,\n\n",
    who, " has invited you to complete a short research competencies self-assessment",
    if (nzchar(org)) paste0(" on behalf of ", org, ".") else ".", "\n\n",
    "Please open your personal survey link (do not share it):\n\n",
    "    ", link, "\n\n",
    "Your responses are saved as you go and combine into a team-level report.\n\n",
    "Thank you.\n")
  list(email = email, token = token, link = link, body = body)
}

# --- SIMULATE sending: write each invite to outputs/invites/ ---------------
# Returns a data.frame of (email, token, link) for inspection.
rt_simulate_send <- function(invites, out_dir = NULL) {
  if (is.null(out_dir)) out_dir <- file.path("..", "outputs", "invites")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (iv in invites) {
    fn <- file.path(out_dir, paste0("invite_", gsub("[^A-Za-z0-9]", "_", iv$email), ".txt"))
    writeLines(iv$body, fn)
  }
  do.call(rbind, lapply(invites, function(iv)
    data.frame(email = iv$email, token = iv$token, link = iv$link,
               stringsAsFactors = FALSE)))
}

# --- high-level: leader nominates a set of colleague emails ----------------
# Creates nominee rows + tokens, renders + "sends" (simulate) their invites.
rt_nominate <- function(con, leader_token, colleague_emails, base_url,
                        salt = "mnk43", send_mode = "simulate", out_dir = NULL) {
  leader <- rt_lookup(con, leader_token)
  if (is.null(leader)) stop("rt_nominate: unknown leader token")
  org <- leader$org %||% ""
  invites <- lapply(colleague_emails, function(em) {
    tok <- rt_add_respondent(con, em, org, role = "nominee",
                             nominated_by = leader_token, salt = salt)
    rt_render_email(em, tok, base_url, org, from_leader = leader$email)
  })
  if (identical(send_mode, "simulate")) {
    rt_simulate_send(invites, out_dir)
  } else {
    stop("send_mode '", send_mode, "' not implemented in this build (simulate only)")
  }
}

# small null-coalesce
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
