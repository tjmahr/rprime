
#' Extract all text chunks between Eprime LogFrame boundaries.
#' @export
extract_chunks <- function(eprime_log) {
  basename <- attr(eprime_log, "basename")
  parsed <- parse_chunks(eprime_log)
  fixed_header <- update_header(parsed)
  numbered <- number_chunks(fixed_header)
  inject_basename(numbered, basename)
}

# Add "Running", "Procedure" and "Eprime.Basename" lines to the header lines
update_header <- function(chunked) {
  has_header <- any(sapply(chunked, is_header))
  if (has_header) {
    header_position <- Position(is_header, chunked)
    header <-  chunked[[header_position]]

    basename_row <- new_row(rprime_cols$basename, attr(chunked, "basename"))
    run_proc_rows <- new_row(c("Running", "Procedure"), "Header")

    header <- inject_row(header, c(basename_row, run_proc_rows))
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
inject_row <- function(frame_lines, row) {
  c(frame_lines[-length(frame_lines)], row, frame_lines[length(frame_lines)])
}




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

make_ranges <- function(starts, ends, eprime_log) {
  # There should be the same number of starts and ends
  min_chunks <- min(length(starts), length(ends))
  old_starts <- starts
  starts <- starts[seq_len(min_chunks)]
  ends <- ends[seq_len(min_chunks)]

  # Warn if there is an incomplete frame
  bad_line <- setdiff(old_starts, starts)
  if (!length_zero(bad_line)) {
    last_bad_line <- max(bad_line)
    lines_after <- length(eprime_log) - last_bad_line

    # Give a preview of the incomplete chunk in the warning
    if (lines_after < 10) {
      sample_range <- seq(last_bad_line, length(eprime_log))
    } else {
      sample_range <- seq(last_bad_line, last_bad_line + 10)
    }

    warning_header <- paste0("Incomplete Log Frame found on line ", bad_line)
    lines <- paste0(c(warning_header, eprime_log[sample_range]), collapse = "\n")
    warning(lines)
  }

  # Each start should come before its corresponding end
  well_ordered_pairs <- starts < ends
  starts <- starts[well_ordered_pairs]
  ends <- ends[well_ordered_pairs]

  mapply(seq, starts, ends)
}

