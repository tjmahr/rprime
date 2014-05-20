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

is_bracket <- function(xs) str_detect(xs, patterns$bracket)
