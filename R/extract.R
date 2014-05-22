#' @export
clean_up_list <- function(eprime_list) {
  level_name <- eprime_list[["Running"]]
  if (!is.null(level_name)) {
    # Remove the level name from the names in the list
    eprime_list <- set_which_name(eprime_list, level_name, rprime_cols$level_index)
    eprime_list[[rprime_cols$level_index]] <- paste0(level_name, "_", eprime_list[[rprime_cols$level_index]])
    names(eprime_list) <- str_replace(names(eprime_list), paste0(level_name, "\\."), "")
  }
  eprime_list
}

which_name <- function(list, name) {
  which(names(list) == name)
}

set_which_name <- function(list, name, new_name) {
  index <- which_name(list, name)
  names(list)[index] <- new_name
  list
}


# Convert a vector of colon-separated values into a list of named elements.
#
#   > example <- c("Item: 1", "OtherItem: Stuff", "Last Item: Whatever")
#   > Listify(example)
#   $Item
#   [1] "1"
#
#   $OtherItem
#   [1] "Stuff"
#
#   $`Last Item`
#   [1] "Whatever"
#

#' @export
listify <- function(colon_sep_values) {
  # Infer level of nesting by counting tabs
  tab_count <- str_count(colon_sep_values[1], "\\t")
  level <- tab_count + 1
  colon_sep_values <- c(colon_sep_values, paste0(rprime_cols$level, ": ", level))

  # Clean up the lines and listify
  colon_sep_values <- str_trim(colon_sep_values)
  colon_sep_values <- Filter(Negate(is_bracket), colon_sep_values)

  splits <- str_split_fixed(colon_sep_values, pattern = ": ", 2)
  structure(as.list(splits[, 2]), names = splits[, 1])
}





#' @export
extract_frames <- function(eprime_log) {
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

#' @export
str_which <- function(string, pattern) {
  which(str_detect(string, pattern))
}

length_zero <- function(x) length(x) == 0
length_one <- function(x) length(x) == 1
