#' @export
clean_up_list <- function(eprime_list) {
  level_name <- eprime_list[["Running"]]
  if (!is.null(level_name)) {
    # Remove the level name from the names in the list
    eprime_list <- set_which_name(eprime_list, level_name, rprime_cols$level_index)
    eprime_list[[rprime_cols$level_index]] <-
      paste0(level_name, "_", eprime_list[[rprime_cols$level_index]])
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
EprimeFrame <- function(key_value_list = list(), ...) {
  dots <- merge_lists(key_value_list, list(...))
  if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) {
    dots <- dots[[1]]
  }

  eprime_metadata <- unlist(rprime_cols, use.names = FALSE)
  defaults <- structure(as.list(rep(NA, length(eprime_metadata))),
                        names = eprime_metadata)

  structure(merge_lists(defaults, dots), class = c("EprimeFrame", "list"))
}


merge_lists <- function(x, y) {
  x[names(y)] <- y
  x
}

#' @export
print.EprimeFrame <- function(...) str(...)



#
# EprimeFrame(key_value_list)
# EprimeFrame(New = "New Attribute")
# EprimeFrame(key_value_list, Running = "New Attribute")
#
#



# colon_sep_values <- chunked[[1]]






#' @export
make_one_eprime_frame <- function(colon_sep_values) {
  frame_list <- merge_lists(listify(colon_sep_values),
                            count_tabs(colon_sep_values))
  EprimeFrame(frame_list)
}


#' @export
listify <- function(colon_sep_values) {
  # Clean up the lines
  colon_sep_values <- str_trim(colon_sep_values)
  colon_sep_values <- Filter(Negate(is_bracket), colon_sep_values)

  splits <- str_split_fixed(colon_sep_values, pattern = ": ", 2)
  structure(as.list(splits[, 2]), names = splits[, 1])
}

count_tabs <- function(colon_sep_values) {
  # Infer level of nesting by counting tabs
  tab_count <- str_count(colon_sep_values[1], "\\t")
  level <- tab_count + 1
  structure(list(level), names = rprime_cols$level)
}


#' @export
extract_chunks <- function(eprime_log) {
  #require(functional)
  #extract <- Compose(parse_chunks, update_header, number_chunks)
  #extract(eprime_log)
  parsed <- parse_chunks(eprime_log)
  fixed_header <- update_header(parsed)
  number_chunks(fixed_header)
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

