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

# A grid's `variant`:
#   "prof3"  (default) -> three columns Entry / Proficiency / Mastery
#   "count1"           -> a single "Number of key staff" count column (Modules G/H)
# Set via QBANK[[qid]]$variant; count_grid.R reads it. Absent = "prof3".

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
  ),

  # -------------------------------------------------------------------------
  # Module D — Develop Data Collection Tools.
  # Nested structure: the D1 matrix (D1a/b/c yes/no) gates three sub-sections,
  # each a yes/no matrix + a count grid:
  #   D1a=1 -> D2 (yn) + D3 (grid)   secondary-data tools
  #   D1b=1 -> D4 (yn) + D5 (grid)   surveys
  #   D1c=1 -> D6 (yn) + D7 (grid)   interview/focus-group protocols
  # Count grids live here; the yes/no matrices (D1,D2,D4,D6) live in QBANK_MATRIX.
  # Gating conditions per spec/enabling_conditions.csv.
  # -------------------------------------------------------------------------
  D3 = list(
    domain = "data_tools",
    title  = "D3. Tools to collect secondary data",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("D3a", "Assessing administrative-data quality frameworks"),
      .item("D3b", "Designing data collection tools and workflows from templates, layouts, and practices that provide machine-readable administrative datasets"),
      .item("D3c", "Pilot testing and refining the tools"),
      .item("D3d", "Conducting systematic and accurate data extraction")
    )
  ),

  D5 = list(
    domain = "data_tools",
    title  = "D5. Surveys",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("D5a", "Designing a clear, valid and reliable questionnaire or instrument"),
      .item("D5b", "Designing multi-instrument studies")
    )
  ),

  D7 = list(
    domain = "data_tools",
    title  = "D7. Interview and focus group discussion protocols",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("D7a", "Aligning content and construct in protocol to purpose"),
      .item("D7b", "Appropriate instrument structure and question design")
    )
  ),

  # -------------------------------------------------------------------------
  # Module E — Fieldwork. Single yes/no gates E1 (outsourced? =1 skips module),
  # E8 (field notes) live in survey.qmd. E5 is a select-all in survey.qmd.
  # E2 (surveys/interviews/focus groups selector) is a matrix; E3/E4 matrices;
  # E6/E7/E9 count grids. Gating per spec/enabling_conditions.csv.
  # -------------------------------------------------------------------------
  E6 = list(
    domain = "fieldwork",
    title  = "E6. Recruitment and training",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("E6a", "Recruiting sites and participants"),
      .item("E6b", "Train field staff")
    )
  ),

  E7 = list(
    domain = "fieldwork",
    title  = "E7. Surveys",
    prompt = "Please list the number of key staff at each proficiency level for administering surveys.",
    items = list(
      .item("E7a", "Administering surveys")
    )
  ),

  E9 = list(
    domain = "fieldwork",
    title  = "E9. Interviews and focus groups",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("E9a", "Conducting interviews",                              "E2b == 1"),
      .item("E9b", "Conducting focus groups, managing group dynamics",   "E2c == 1")
    )
  ),

  # -------------------------------------------------------------------------
  # Module F — Data Analysis. Prof-3 count grids. (Yes/no matrices, single
  # yes/no gates, and select-alls live in QBANK_MATRIX / survey.qmd.)
  # Quant grids gate on B1 includes 2; qual on B1 includes 1; review on B1
  # includes 3 (page-level skips in app.R per spec/enabling_conditions.csv).
  # -------------------------------------------------------------------------
  F12 = list(
    domain = "analysis", title = "F12. Measurement validity and reliability",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F12a", "Assessing the internal consistency such as estimating Cronbach's alpha"),
      .item("F12b", "Testing test–retest reliability to assess stability of responses over time"),
      .item("F12c", "Checking inter rater reliability (IRR)"),
      .item("F12d", "Exploratory or confirmatory factor analysis"),
      .item("F12e", "Using psychometrics to assess scales of multiple items"),
      .item("F12f", "Checking criterion validity")
    )
  ),
  F15 = list(
    domain = "analysis", title = "F15. Quantitative study designs",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("F15a", "Conducting descriptive analyses",                          "F14a == 1"),
      .item("F15b", "Conducting correlational analyses",                        "F14b == 1"),
      .item("F15c", "Conducting observational comparative or pre–post analyses", "F14c == 1"),
      .item("F15d", "Conducting quasi-experimental analyses",                   "F14d == 1"),
      .item("F15e", "Conducting experimental analyses",                         "F14e == 1")
    )
  ),
  F17 = list(
    domain = "analysis", title = "F17. Analytic approaches",
    prompt = "Please list the number of key staff at each proficiency level for conducting these analyses.",
    items = list(
      .item("F17a", "Simple, univariate linear regression or ordinary least squares (OLS)"),
      .item("F17b", "Multiple linear regression"),
      .item("F17c", "Logistic regression"),
      .item("F17d", "Clustered / nested designs"),
      .item("F17e", "Fixed-effects models"),
      .item("F17f", "Random-effects models"),
      .item("F17g", "Hierarchical or multilevel models"),
      .item("F17h", "Assessing multicollinearity and model fit in regression"),
      .item("F17i", "Other approach, please specify __________")
    )
  ),
  F18 = list(
    domain = "analysis", title = "F18. Analytic approaches (continued)",
    prompt = "Please list the number of key staff at each proficiency level for conducting these analyses.",
    items = list(
      .item("F18a", "Difference-in-differences (DiD)"),
      .item("F18b", "Regression discontinuity design (RDD)"),
      .item("F18c", "Interrupted time series (ITS)"),
      .item("F18d", "Propensity-score methods"),
      .item("F18e", "Structural equation modeling (SEM)"),
      .item("F18f", "Factor analysis"),
      .item("F18g", "Growth-curve / longitudinal modeling"),
      .item("F18h", "Machine learning methods in research"),
      .item("F18i", "Bayesian modeling"),
      .item("F18j", "Other approach, please specify __________")
    )
  ),
  F23 = list(
    domain = "analysis", title = "F23. Quantitative AI assistance",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F23a", "Using AI tools for data cleaning or preprocessing (e.g., automated outlier detection, natural language classification)"),
      .item("F23b", "Using AI tools for statistical or predictive modeling (e.g., regression with machine learning algorithms, decision trees, random forests)"),
      .item("F23c", "Validating AI-generated outputs (checking accuracy, bias, and error metrics)")
    )
  ),
  F29 = list(
    domain = "analysis", title = "F29. Qualitative analysis",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F29a", "Developing initial codes and codebooks—that is, generating open codes from data and defining code descriptions for team use.",                       "F28a == 1"),
      .item("F29b", "Applying thematic coding by assigning segments of text to themes or categories and ensuring inter-coder agreement.",                                  "F28b == 1"),
      .item("F29c", "Refining coding frameworks based on review to date to create structured sets of themes (inductive / deductive or grounded-theory based).",           "F28c == 1"),
      .item("F29d", "Developing explanatory narratives or models.",                                                                                                       "F28d == 1"),
      .item("F29e", "Triangulating qualitative findings with other data sources, like cross-checking themes against quantitative or document-based data.",               "F28e == 1")
    )
  ),
  F30 = list(
    domain = "analysis", title = "F30. Qualitative AI-assistance",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F30a", "Using AI tools for auto-coding or topic modeling by applying machine-learning methods (natural language processing, topic extraction, clustering) to suggest themes or patterns."),
      .item("F30b", "Using AI tools for summarization or synthesis (generating automated summaries of interviews or reports to support interpretation)."),
      .item("F30c", "Validating AI-generated qualitative outputs (reviewing accuracy, representativeness, and potential bias of automated themes or summaries).")
    )
  ),
  F32 = list(
    domain = "analysis", title = "F32. Narrative reviews",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F32a", "Grouping studies by topic, time, or theoretical lens"),
      .item("F32b", "Identifying major themes and trends")
    )
  ),
  F33 = list(
    domain = "analysis", title = "F33. Scoping reviews",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F33a", "Counting and tabulating frequencies (e.g., how many used RCTs vs. surveys, etc.)"),
      .item("F33b", "Summarizing patterns narratively and visually (tables, maps, bubble charts, etc.)")
    )
  ),
  F34 = list(
    domain = "analysis", title = "F34. Systematic reviews",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F34a", "Conducting study quality appraisal (e.g., risk of bias)"),
      .item("F34b", "Grouping studies by design, context, or findings"),
      .item("F34c", "Synthesizing results narratively (qualitative) or descriptively (quantitative)")
    )
  ),
  F35 = list(
    domain = "analysis", title = "F35. Meta-analyses",
    prompt = "Please list the number of key staff at each proficiency level for the following competencies.",
    items = list(
      .item("F35a", "Converting findings into standardized effect sizes (Cohen's d, OR, r, etc.)"),
      .item("F35b", "Weighting studies by sample size or precision"),
      .item("F35c", "Calculating pooled effects (fixed or random effects models)"),
      .item("F35d", "Assessing heterogeneity (I², Q test)"),
      .item("F35e", "Testing for bias (funnel plots, Egger's test)")
    )
  ),
  F41 = list(
    domain = "analysis", title = "F41. Quantitative computational software",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("F41a", "R",       "F39 includes 1"),
      .item("F41b", "SAS",     "F39 includes 2"),
      .item("F41c", "Python",  "F39 includes 3"),
      .item("F41d", "Stata",   "F39 includes 4"),
      .item("F41e", "SPSS",    "F39 includes 5"),
      .item("F41f", "SQL",     "F39 includes 6"),
      .item("F41g", "Power BI","F39 includes 7"),
      .item("F41h", "STAN",    "F39 includes 8"),
      .item("F41i", "Other, please specify __________", "F39 includes 9")
    )
  ),
  F42 = list(
    domain = "analysis", title = "F42. Qualitative coding tools",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("F42a", "Coding on paper",                                          "F40 includes 1"),
      .item("F42b", "Free computer-analysis software (e.g., Microsoft suite)",  "F40 includes 2"),
      .item("F42c", "Paid computer-analysis software (e.g., NVivo)",            "F40 includes 3"),
      .item("F42d", "Other, please specify __________",                        "F40 includes 4")
    )
  ),

  # -------------------------------------------------------------------------
  # Module G — Interpretation. G2-G6 & G13-G15 use the single-count variant
  # ("Number of key staff"); G7-G11 & G16-G18 are prof-3 grids.
  # -------------------------------------------------------------------------
  G2 = list(
    domain = "interpretation", variant = "count1", title = "G2. Interpreting descriptive analyses",
    prompt = "Please list the number of key staff that have the following competencies.",
    items = list(
      .item("G2a", "Identifies spurious trends (seasonality, outliers, sampling bias) vs. plausible."),
      .item("G2b", "Interprets measures of central tendency and variation correctly (mean vs. median, range, standard deviation)."),
      .item("G2c", "Recognizes limitations of descriptive data (no causation).")
    )
  ),
  G3 = list(
    domain = "interpretation", variant = "count1", title = "G3. Interpreting statistical testing analyses",
    prompt = "Please list the number of key staff that have the following competencies.",
    items = list(
      .item("G3a", "Can explain what statistical significance and p-values mean (and what they don't)."),
      .item("G3b", "Interprets both statistical and practical (effect size) significance."),
      .item("G3c", "Understands direction, strength, and uncertainty of relationships."),
      .item("G3d", "Recognizes false positives or negatives and multiple-testing risks."),
      .item("G3e", "Can translate test results into plain language.")
    )
  ),
  G4 = list(
    domain = "interpretation", variant = "count1", title = "G4. Interpreting regression analyses",
    prompt = "Please list the number of key staff that have the following competencies.",
    items = list(
      .item("G4a", "Interprets coefficients as change in outcome given change in predictor (for linear) or odds ratios (for logistic)."),
      .item("G4b", "Distinguishes between correlation and causation."),
      .item("G4c", "Can explain what controls do and how they affect interpretation."),
      .item("G4d", "Understands direction, magnitude, and statistical uncertainty (standard errors, confidence intervals)."),
      .item("G4e", "Interprets R² and model fit appropriately."),
      .item("G4f", "Identifies omitted variable bias or model mis-specification."),
      .item("G4g", "Relates findings back to hypotheses and context.")
    )
  ),
  G5 = list(
    domain = "interpretation", variant = "count1", title = "G5. Interpreting quasi-experimental or experimental models",
    prompt = "Please list the number of key staff that are proficient in interpreting the following models.",
    items = list(
      .item("G5a", "Randomized control trials"),
      .item("G5b", "Difference-in-differences (DiD)"),
      .item("G5c", "Regression discontinuity design (RDD)"),
      .item("G5d", "Interrupted time series (ITS)"),
      .item("G5e", "Propensity-score methods"),
      .item("G5f", "Factor analysis"),
      .item("G5g", "Machine learning methods in research"),
      .item("G5h", "Bayesian modeling")
    )
  ),
  G6 = list(
    domain = "interpretation", variant = "count1", title = "G6. Interpreting robustness checks",
    prompt = "Please list the number of key staff that have the following competencies.",
    items = list(
      .item("G6a", "Understands that robustness checks are run to test if findings hold under alternative specifications."),
      .item("G6b", "Can explain robustness checks in clear, non-technical language."),
      .item("G6c", "Interprets whether results remain stable across subsamples, controls, or methods."),
      .item("G6d", "Recognizes when changes in results indicate fragility or model dependence.")
    )
  ),
  G7 = list(
    domain = "interpretation", title = "G7. Summarizing quantitative results",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G7a", "Summarizing quantitative results"))
  ),
  G8 = list(
    domain = "interpretation", title = "G8. Contextualizing results",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G8a", "Contextualizing results"))
  ),
  G9 = list(
    domain = "interpretation", title = "G9. Interpreting limitations and uncertainty",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G9a", "Interpreting limitations and uncertainty"))
  ),
  G10 = list(
    domain = "interpretation", title = "G10. Drawing conclusions",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G10a", "Drawing conclusions"))
  ),
  G11 = list(
    domain = "interpretation", title = "G11. Explaining implications and linking to decisions",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G11a", "Explaining implications and linking to decisions"))
  ),
  G13 = list(
    domain = "interpretation", variant = "count1", title = "G13. Interpreting meaning and relationships among themes",
    prompt = "Please list the number of key staff that have the following competencies.",
    items = list(
      .item("G13a", "Identifies relationships or hierarchies among themes (causal links, processes, influences)."),
      .item("G13b", "Distinguishes between descriptive findings and explanatory ones.")
    )
  ),
  G14 = list(
    domain = "interpretation", variant = "count1", title = "G14. Contextualizing findings",
    prompt = "Please list the number of key staff that have the following competencies.",
    items = list(
      .item("G14a", "Considers how social, cultural, institutional, or programmatic context explains differences in perspectives across groups or sites and emergent findings."),
      .item("G14b", "Recognizes nuances, contradictions, and power dynamics in narratives.")
    )
  ),
  G15 = list(
    domain = "interpretation", variant = "count1", title = "G15. Divergent or negative cases",
    prompt = "Please list the number of key staff that have the following competencies.",
    items = list(
      .item("G15a", "Identifies and examines cases that contradict or deviate from dominant themes."),
      .item("G15b", "Uses divergent cases to refine interpretations or strengthen credibility.")
    )
  ),
  G16 = list(
    domain = "interpretation", title = "G16. Making claims grounded in systematic patterns",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G16a", "Making claims that are clearly grounded in systematic patterns from the data"))
  ),
  G17 = list(
    domain = "interpretation", title = "G17. Reflecting limitations and ethical considerations",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G17a", "Reflecting limitations and ethical considerations"))
  ),
  G18 = list(
    domain = "interpretation", title = "G18. Explaining implications and linking to decisions or study recommendations",
    prompt = "Please list the number of key staff at each proficiency level in the following competency.",
    items = list(.item("G18a", "Explaining implications and linking to decisions or study recommendations"))
  ),

  # -------------------------------------------------------------------------
  # Module H — Communicating Findings. H1 & H7 single-count; H3 prof-3.
  # H2 (yn matrix), H4/H5 (dashboard + upload), H6 (yn matrix) elsewhere.
  # -------------------------------------------------------------------------
  H1 = list(
    domain = "communication", variant = "count1", title = "H1. Drafting deliverables",
    prompt = "Please list the number of key staff that have led the drafting of the following deliverables.",
    items = list(
      .item("H1a", "Evaluation reports"),
      .item("H1b", "Peer-reviewed journal articles"),
      .item("H1c", "Other papers (e.g. working paper, white paper, etc.)"),
      .item("H1d", "Policy briefs or memos"),
      .item("H1e", "Practice guides"),
      .item("H1f", "Op-eds"),
      .item("H1g", "Alternative modes of sharing findings to a wider range of stakeholders such as posters, press releases, videos, blog posts, infographics, etc.")
    )
  ),
  H3 = list(
    domain = "communication", title = "H3. Data visualization",
    prompt = "Please list the number of key staff at each proficiency level in the following competencies.",
    items = list(
      .item("H3a", "Basic quantitative displays (e.g., histograms, scatter plots, bar graphs etc.)"),
      .item("H3b", "Advanced statistical visuals (e.g., regression coefficient plots, forest plots, residual or diagnostic plots, box plots etc.)"),
      .item("H3c", "Categorical or hierarchical data (e.g., heatmaps, tree maps, etc.)"),
      .item("H3d", "Spatial or geographic data (e.g., visualize location-based patterns, flow maps, animated spatial dashboard, etc.)"),
      .item("H3e", "Qualitative visuals (e.g., conceptual or causal-loop diagrams, process or flow charts, word clouds, etc.)")
    )
  ),
  H7 = list(
    domain = "communication", variant = "count1", title = "H7. Communicating findings",
    prompt = "Please list the number of key staff that have presented, facilitated, or drafted within the following contexts.",
    items = list(
      .item("H7a", "Conferences (e.g., CIES, AERA, etc.)"),
      .item("H7b", "Stakeholder workshops, community feedback meetings, participatory reflection meetings, etc."),
      .item("H7c", "Sessions with policymakers or congressional committees"),
      .item("H7d", "Technical working groups"),
      .item("H7e", "Webinars"),
      .item("H7f", "Podcasts")
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
  ),

  # --- Module D yes/no matrices ---------------------------------------------
  # D1 is the sub-section selector; D1a always shown, D1b if B1 in {qual,quant},
  # D1c if qual (B1 includes 1). D2/D4/D6 gate on D1a/b/c respectively (page-skip
  # handled in app.R). Rows store as D1_D1a etc. (matrix id + row item).
  D1 = list(
    domain = "data_tools",
    title  = "D1. Please indicate whether any of the key staff have experience drafting the following quantitative and qualitative data collection tools. Select \"Yes\" if at least one key staff member has independently drafted the tool; otherwise select \"No\".",
    rows = list(
      .item("D1a", "Tools to collect secondary data"),
      .item("D1b", "Surveys",                             "B1 includes 1 || B1 includes 2"),
      .item("D1c", "Interview and focus group protocols", "B1 includes 1")
    )
  ),
  D2 = list(
    domain = "data_tools",
    title  = "D2. Tools to collect secondary data: Please indicate whether any of the key staff have experience with the following competencies. Select \"Yes\" if at least one team member routinely performs each task as part of standard tool development practice; otherwise select \"No\".",
    rows = list(
      .item("D2a", "Identifying and defining required administrative data elements and sources"),
      .item("D2b", "Drafting data-access requirements such as Data Use Agreements")
    )
  ),
  D4 = list(
    domain = "data_tools",
    title  = "D4. Surveys: Please indicate whether any of the key staff have experience with the following competencies. Select \"Yes\" if at least one team member routinely performs each task as part of standard tool development practice; otherwise select \"No\".",
    rows = list(
      .item("D4a", "Implementing skip logic, validation rules, and programming for data collection software or paper-based tools"),
      .item("D4b", "Pre-testing / piloting and revising surveys before full data collection"),
      .item("D4c", "Ensuring survey language, context and cultural appropriateness, including translation/back-translation when needed"),
      .item("D4d", "Ensuring survey has a clear informed consent clause")
    )
  ),
  D6 = list(
    domain = "data_tools",
    title  = "D6. Interview and focus group discussion protocols: Please indicate whether any of the key staff have experience with the following competencies. Select \"Yes\" if at least one team member routinely performs each task as part of standard tool development practice; otherwise select \"No\".",
    rows = list(
      .item("D6a", "Ethical and participant-oriented procedures, including informed consent"),
      .item("D6b", "Contextual and linguistic adaptation"),
      .item("D6c", "Pilot testing and refinement of protocols")
    )
  ),

  # --- Module E yes/no matrices ---------------------------------------------
  # E2 selector (surveys/interviews/focus groups) gates E3-E9. E3/E4 are yes/no
  # matrices. E2b/E2c share condition B1=2 (per instrument). Rows store E2_E2a etc.
  E2 = list(
    domain = "fieldwork",
    title  = "E2. Please indicate whether any of the key staff have experience conducting or overseeing quantitative or qualitative fieldwork. Select \"Yes\" if at least one key staff member has independently conducted fieldwork; otherwise select \"No\".",
    rows = list(
      .item("E2a", "Surveys",      "B1 includes 1 || B1 includes 2"),
      .item("E2b", "Interviews",   "B1 includes 2"),
      .item("E2c", "Focus groups", "B1 includes 2")
    )
  ),
  E3 = list(
    domain = "fieldwork",
    title  = "E3. Managing fieldwork: Please indicate whether any of the key staff have experience managing fieldwork. Select \"Yes\" if at least one key staff member has independently conducted a step in the management process; otherwise select \"No\".",
    rows = list(
      .item("E3a", "Develop a fieldwork protocol"),
      .item("E3b", "In person logistics and operations planning"),
      .item("E3c", "Virtual logistics and operations planning"),
      .item("E3d", "Secure data transfer and storage"),
      .item("E3e", "Finalizing recordings, transcription and translation", "E2b == 1 || E2c == 1")
    )
  ),
  E4 = list(
    domain = "fieldwork",
    title  = "E4. Participant identification: Please indicate whether any of the key staff have experience identifying participants in the following ways. Select \"Yes\" if at least one key staff member has independently conducted a step in the management process; otherwise select \"No\".",
    rows = list(
      .item("E4a", "Using sample lists"),
      .item("E4b", "Snowball or chain referral"),
      .item("E4c", "Managing replacements")
    )
  )
)
