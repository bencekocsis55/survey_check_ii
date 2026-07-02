# ============================================================================
# piping.R  —  Text substitution driven by B1 (research methods)
# ----------------------------------------------------------------------------
# Many Module C labels contain a "[q]" (originally "[quantitative or
# qualitative]") token. The programming instructions say:
#   If B1 = 1 (qualitative only)  -> "qualitative"
#   If B1 = 2 (quantitative only) -> "quantitative"
#   If B1 = 1 & 2                 -> both ("quantitative or qualitative")
#
# NOTE: B1 codes are 1=Qualitative, 2=Quantitative, 3=Review (from the
# instrument). Piping only concerns qual/quant.
#
# pipe_q() is evaluated at UI-build time. It reads B1 through a `getval`
# accessor supplied by the server (reads from `input`) — NOT sd_value(), which
# a sourced helper can't scope to `all_data`. When no accessor is supplied
# (e.g. static preview render before the server runs) it falls back to the
# neutral "quantitative or qualitative".
# ============================================================================

.b1_to_qual_quant <- function(b1) {
  b1 <- as.character(b1)
  has_qual  <- any(b1 == "1")
  has_quant <- any(b1 == "2")
  if (has_quant && has_qual) return("quantitative or qualitative")
  if (has_quant)             return("quantitative")
  if (has_qual)              return("qualitative")
  "quantitative or qualitative"   # neutral default before B1 is answered
}

# Replace the [q] token in a label using the current B1 answer.
# getval: accessor (id -> character vector). If NULL, use the neutral default.
pipe_q <- function(text, getval = NULL) {
  if (!grepl("\\[q\\]", text)) return(text)
  b1 <- if (is.null(getval)) character(0) else getval("B1")
  gsub("\\[q\\]", .b1_to_qual_quant(b1), text)
}
