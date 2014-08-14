
#' Extract all text chunks between Eprime LogFrame boundaries.
#' @export
extract_chunks <- function(eprime_log) {
  parsed <- parse_chunks(eprime_log)
  fixed_header <- update_header(parsed)
  numbered <- number_chunks(fixed_header)

  basename <- attr(eprime_log, "basename")
  inject_basename(numbered, basename)
}

#' Add "Running", "Procedure" lines to the header lines
#' @keywords internal
update_header <- function(chunked) {
  if (has_header(chunked)) {
    header_position <- Position(is_header, chunked)
    header <- chunked[[header_position]]
    row_run <- new_row("Running", "Header")
    row_prc <- new_row("Procedure", "Header")
    header <- inject_row(header, c(row_run, row_prc))
    chunked[[header_position]] <- header
  }
  chunked
}

# Add "Eprime.FrameNumber" lines to every frame
number_chunks <- function(chunked) {
  rows <- new_row(rprime_cols$frame, seq_along(chunked))
  Map(inject_row, chunked, rows)
}

# Add "Eprime.Basename" lines to every frame
inject_basename <- function(chunked, basename) {
  rows <- new_row(rprime_cols$basename, basename)
  Map(inject_row, chunked, rows)
}

# Insert a line in the second-to-last position in an Eprime frame
inject_row <- function(xs, ys) {
  c(but_last(xs), ys, last(xs))
}
but_last <- function(...) head(..., n = -1)
last <- function(...) tail(..., n = 1)



#' Find all the texts between Eprime LogFrame boundaries. Store in a list.
#' @export
parse_chunks <- function(eprime_log) {
  # Find all the texts between LogFrame boundaries
  starts <- str_which(eprime_log, patterns$bracket_start)
  ends <- str_which(eprime_log, patterns$bracket_end)
  ranges <- make_ranges(starts, ends, eprime_log)

  pull_lines <- function(lines) eprime_log[lines]
  frames <- lapply(ranges, pull_lines)
  attr(frames, "basename") <- attr(eprime_log, "basename")
  frames
}


#' @keywords internal
make_ranges <- function(starts, ends, eprime_log) {
  # There should be the same number of starts and ends
  min_chunks <- min(length(starts), length(ends))
  old_starts <- starts
  starts <- starts[seq_len(min_chunks)]
  ends <- ends[seq_len(min_chunks)]

  # Warn if there is an incomplete frame (more old_starts than ends)
  bad_lines <- setdiff(old_starts, starts)
  if (!length_zero(bad_lines)) {
    last_bad_line <- max(bad_lines)

    # Give a preview of the incomplete chunk in the warning
    last_preview_line <- min(last_bad_line + 10, length(eprime_log))
    sample_range <- seq(last_bad_line, last_preview_line)
    warning_header <- paste0("Incomplete Log Frame found on line ", bad_lines)
    lines <- paste0(c(warning_header, eprime_log[sample_range]), collapse = "\n")
    warning(lines)
  }

  # Each start should come before its corresponding end
  well_ordered_pairs <- starts < ends
  starts <- starts[well_ordered_pairs]
  ends <- ends[well_ordered_pairs]

  Map(seq, starts, ends)
}

