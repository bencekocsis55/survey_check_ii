# Research Competencies Self-Assessment — Survey

A reproducible web survey ([surveydown](https://surveydown.org) / R + Shiny) for a
staff **research competencies self-assessment**. Multiple team members complete routed
modules; responses persist to a PostgreSQL backend for downstream team-level reporting.

This repository contains the **deployable survey app** (Modules A/B/C). It is configured
to deploy to [Posit Connect Cloud](https://connect.posit.cloud).

## Layout
```
app.R              launch + server (custom grids, show_if / skip_if logic)
survey.qmd         pages + questions (Modules A / B / C)
R/
  question_bank.R  single source of truth for Module C items
  count_grid.R     custom numeric-count grid widget (row-sum <= B3)
  piping.R         [q] -> quantitative / qualitative text piping from B1
  skip_logic.R     B1/B2/gate predicates + per-row visibility
manifest.json      pinned R dependencies (for Connect Cloud)
```

## Data backend (environment variables)

The app persists responses to PostgreSQL. Credentials are read from environment
variables — **never committed**. Set these in the hosting platform's dashboard:

| Variable | Meaning |
|---|---|
| `SD_HOST` | Postgres host |
| `SD_PORT` | Port |
| `SD_DBNAME` | Database name |
| `SD_USER` | User |
| `SD_PASSWORD` | Password |
| `SD_TABLE` | Response table name |

For **local development without a database**, set `mode: preview` in `survey.qmd`'s YAML
(responses save locally); set `mode: database` to persist to Postgres.

## Deploy (Posit Connect Cloud)

1. Connect this repo at https://connect.posit.cloud (Publish → Shiny).
2. Set the six `SD_*` environment variables above.
3. Deploy — Connect Cloud installs dependencies from `manifest.json` and launches `app.R`.
