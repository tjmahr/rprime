#' @export
patterns <- list(
  bracket       = "\\*{3} (.*) (Start|End) \\*{3}",
  bracket_start = "\\*{3} (.*) Start \\*{3}",
  bracket_end   = "\\*{3} (.*) End \\*{3}",
  header_start  = "^\\*{3} Header Start \\*{3}$",
  header_end    = "^\\*{3} Header End \\*{3}$",
  footer_start  = "^\\*{3} LogFrame Start \\*{3}$",
  footer_end    = "^\\*{3} LogFrame End \\*{3}$"
)

rprime_cols <- list(
  level_index = "Eprime.LevelIndex",
  level = "Eprime.Level",
  basename = "Eprime.Basename",
  frame = "Eprime.FrameNumber",
  procedure = "Procedure",
  running = "Running"
)

#' @export
str_which <- function(string, pattern) {
  which(str_detect(string, pattern))
}

new_row <- function(key, value) sprintf("%s: %s", key, value)
is_bracket <- function(xs) str_detect(xs, patterns$bracket)
is_header <- function(xs) any(str_detect(xs, patterns$header_start))

length_zero <- function(x) length(x) == 0
length_one <- function(x) length(x) == 1
