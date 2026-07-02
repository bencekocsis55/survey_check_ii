# ============================================================================
# question_bank.R  —  Single source of truth for Module C content
# ----------------------------------------------------------------------------
# Every count-grid question is defined as a list of competency items. Each item
# has a stable `item` code (e.g. "C1a") used to name the stored columns, and a
# `label` shown to the respondent. Some items carry a per-row `show_if`
# condition (a string, evaluated against B1/B2/other answers) mirroring the
# "Display if..." column in the programming instructions / enabling_conditions.
#
# The [q] token in a label is piped to "quantitative"/"qualitative" at render
# time based on B1 (see piping.R :: pipe_q()).
#
# This bank drives BOTH the survey UI (count_grid.R) and the report scoring
# (report/scoring.R reads the same item codes), so the two never drift.
# ============================================================================

# Proficiency levels — column order is fixed and used everywhere downstream.
PROF_LEVELS <- c(entry = "Entry / Novice",
                 prof  = "Proficiency / Skilled",
                 mast  = "Mastery / Expert")

# Helper to build one competency item.
.item <- function(code, label, show_if = NA_character_) {
  list(item = code, label = label, show_if = show_if)
}

# ---------------------------------------------------------------------------
# Module C — count grids. Keyed by question id. `range` is always 1..B3.
# `items` is the ordered list of rows.
# ---------------------------------------------------------------------------
QBANK <- list(

  C1 = list(
    domain = "formulating_rq",
    title  = "C1. Drafting questions",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("C1a", "Reviewing literature to understand background on the topic or problem"),
      .item("C1b", "Drafting [q] questions to understand a program's feasibility or acceptability", "B2 includes 1"),
      .item("C1c", "Drafting [q] questions to understand a program's process or performance",        "B2 includes 2"),
      .item("C1d", "Drafting questions to validate a measurement tool",                              "B2 includes 3"),
      .item("C1e", "Drafting [q] questions for an impact / causal inference study",                  "B2 includes 4"),
      .item("C1f", "Drafting [q] questions for implementation research",                             "B2 includes 5")
    )
  ),

  C3 = list(
    domain = "study_design",
    title  = "C3. Measurement design",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("C3a", "Selecting and developing indicators to accurately measure outcomes"),
      .item("C3b", "Identifying data sources")
    )
  ),

  C4 = list(
    domain = "study_design",
    title  = "C4. Qualitative design",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("C4a", "Designing case studies"),
      .item("C4b", "Designing ethnographies"),
      .item("C4c", "Designing qualitative studies using grounded theory"),
      .item("C4d", "Designing qualitative studies using participatory action research"),
      .item("C4e", "Other qualitative approach, please specify ________")
    )
  ),

  C5 = list(
    domain = "study_design",
    title  = "C5. Quantitative design",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("C5a", "Designing descriptive research"),
      .item("C5b", "Designing correlational research"),
      .item("C5c", "Designing observational comparative or pre-post research"),
      .item("C5d", "Designing quasi-experimental research"),
      .item("C5e", "Designing experimental research")
    )
  ),

  C6 = list(
    domain = "study_design",
    title  = "C6. Review design",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("C6a", "Designing narrative reviews"),
      .item("C6b", "Designing scoping reviews"),
      .item("C6c", "Designing systematic reviews"),
      .item("C6d", "Designing meta-analyses"),
      .item("C6e", "Other review design, please specify ____________")
    )
  ),

  C7 = list(
    domain = "study_design",
    title  = "C7. Other design",
    prompt = "Please list the number of key staff at each proficiency level for competencies.",
    items = list(
      .item("C7a", "Understanding the difference between causal, descriptive, and exploratory designs", "B1 includes 2"),
      .item("C7b", "Recognizing the strengths, limitations, and threats to validity in quantitative designs", "B1 includes 2"),
      .item("C7c", "Recognizing the strengths, limitations, and threats to validity in qualitative designs",  "B1 includes 1"),
      .item("C7d", "Recognizing the strengths, limitations, and threats to validity in review designs",       "B1 includes 3"),
      .item("C7e", "Ability to select appropriate research designs based on the type of questions being asked", "C5 == 1"),
      .item("C7f", "Designing mixed methods studies", "B1 includes 2 && B1 includes 3")
    )
  ),

  C9 = list(
    domain = "sampling_design",
    title  = "C9. Quantitative sampling considerations",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("C9a", "Determining a sample frame"),
      .item("C9b", "Considering and planning for site and participant selection"),
      .item("C9c", "Planning and making considerations for spillover and contamination"),
      .item("C9d", "Handling sample unit nonresponse and, or replacement"),
      .item("C9e", "Determining sampling needs to achieve representativeness of target population")
    )
  ),

  C10 = list(
    domain = "sampling_design",
    title  = "C10. Probability sampling",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("C10a", "Simple random sampling"),
      .item("C10b", "Stratified random sampling"),
      .item("C10c", "Clustered sampling"),
      .item("C10d", "Multi-stage sampling"),
      .item("C10e", "Other probability sampling method, please specify ________________")
    )
  ),

  C11 = list(
    domain = "sampling_design",
    title  = "C11. Non-probability sampling",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("C11a", "Purposive sampling"),
      .item("C11b", "Convenience sampling"),
      .item("C11c", "Criterion sampling"),
      .item("C11d", "Other non-probability sampling method, please specify _____")
    )
  ),

  C12 = list(
    domain = "sampling_design",
    title  = "C12. Other quantitative sampling considerations",
    prompt = "Please indicate how many key staff are at each proficiency level for the following competencies.",
    items = list(
      .item("C12a", "Margin of error (degree of uncertainty)"),
      .item("C12b", "Determining a desired confidence level"),
      .item("C12c", "Estimating variance"),
      .item("C12d", "Intraclass Correlation Coefficient"),
      .item("C12e", "Sample sizes when statistical tests will be conducted (vs. descriptive analysis)"),
      .item("C12f", "Using survey weights")
    )
  ),

  C13 = list(
    domain = "sampling_design",
    title  = "C13. Qualitative sampling considerations",
    prompt = "Please indicate how many key staff are at each proficiency level for the following competencies.",
    items = list(
      .item("C13a", "Conducting site visits or observations to determine eligibility"),
      .item("C13b", "Determining sample size for saturation"),
      .item("C13c", "Determining sample size for focus groups"),
      .item("C13d", "Determining sample size for interviews"),
      .item("C13e", "Determining sampling needs to achieve representativeness of target population, including subgroups")
    )
  )
)

# Matrix (yes/no per row) questions in Module C — used by C8 and C14.
QBANK_MATRIX <- list(
  C8 = list(
    domain = "sampling_design",
    title  = "C8. Do key staff have experience determining the following sampling needs?",
    rows = list(
      .item("C8a", "Conducting sample size determination for simple quantitative study designs (descriptive, correlational, two-group comparisons)", "B1 includes 2"),
      .item("C8b", "Conducting sample size determination for complex quantitative study designs (experimental, quasi-experimental, multivariate, clustered, longitudinal, SEM, etc.)", "B1 includes 2"),
      .item("C8c", "Conducting sample size determination for qualitative study designs", "B1 includes 1")
    )
  ),
  C14 = list(
    domain = "ethics",
    title  = "C14. IRB approval: has at least one key staff member led submission and received IRB approval in the following research study designs?",
    rows = list(
      .item("C14a", "Quantitative study", "B1 includes 2"),
      .item("C14b", "Qualitative study",  "B1 includes 1"),
      .item("C14c", "Mixed methods",      "B1 includes 1 && B1 includes 2")
    )
  )
)
