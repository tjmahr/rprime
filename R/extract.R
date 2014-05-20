extract_frames <- function(eprime_log) {
  # Find all the texts between LogFrame boundaries
  starts <- str_detect_which(eprime_log, patterns$bracket_start)
  ends <- str_detect_which(eprime_log, patterns$bracket_end)
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
  bad_line <- max(setdiff(old_starts, starts))
  if (!length_zero(bad_line)) {
    lines_after <- length(eprime_log) - bad_line

    # Give a preview of the incomplete chunk in the warning
    if (lines_after < 10) {
      sample_range <- seq(bad_line, length(eprime_log))
    } else {
      sample_range <- seq(bad_line, bad_line + 10)
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

str_detect_which <- function(string, pattern) which(str_detect(string, pattern))
length_zero <- function(x) length(x) == 0
length_one <- function(x) length(x) == 1
