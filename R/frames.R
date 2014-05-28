


#' Make an Eprime Frame
#' @export
EprimeFrame <- function(key_value_list = list(), ...) {
  dots <- merge_lists(key_value_list, list(...))
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

#' @export
print.FrameList <- function(...) str(...)


#' @export
as_eprime_frame <- function(xs) {
  class(xs) <- unique(c(class(xs), "EprimeFrame", "list"))
  xs
}

#' @export
as_frame_list <- function(xss) {
  class(xss) <- unique(c(class(xss), "FrameList", "list"))
  xss
}




#' Convert chunks of text into Eprime Frames
#' @export
make_eprime_frames <- function(...) UseMethod("make_eprime_frames")

#' @export
make_eprime_frames.character <- function(colon_sep_values, tidy = TRUE) {
  frame_list <- merge_lists(listify(colon_sep_values),
                            count_tabs(colon_sep_values))
  frame <- EprimeFrame(frame_list)
  if (tidy) tidy_frames(frame) else frame
}

#' @export
make_eprime_frames.list <- function(chunk_list, tidy = TRUE) {
  as_frame_list(lapply(chunk_list, make_eprime_frames, tidy))
}



#' Clean up `Running`-related attributes
tidy_frames <- function(...) UseMethod("tidy_frames")

tidy_frames.EprimeFrame <- function(eprime_frame) {
  level <- eprime_frame[[rprime_cols$level]]
  level_label <- eprime_frame[["Running"]]
  if (!is.na(level_label)) {
    # A fresh frame has the structure:
    #   Eprime.Level: [Level]
    #   Running: [Key]
    #   [Key]: [Value]
    #   [Key].Cycle: [Cycle]
    #   [Key].Sample: [Sample]
    # Store [Key]_[Value] as "Eprime.LeveName"
    level_index <- eprime_frame[[level_label]]
    new_list <- list()
    new_list[rprime_cols$level_name] <- paste0(level_label, "_", level_index)

    # Remove "[Key]: [Value]" item and then remove "[Key]." from names
    eprime_frame[level_label] <- NULL
    names(eprime_frame) <- str_replace(names(eprime_frame), paste0(level_label, "\\."), "")
    eprime_frame <- merge_lists(eprime_frame, new_list)
  }
  as_eprime_frame(eprime_frame)
}

tidy_frames.FrameList <- function(frame_list) {
  as_frame_list(lapply(frame_list, tidy_frames))
}




#' Convert a vector of colon-separated values into a list of named elements
#'
#' @details White-space on both sides of the input strings is omitted. Eprime
#'   bracketing lines like \code{"*** LogFrame Start ***"} are filtered out.
#'   Lines without a colon \code{":"} are filtered out as well.
#'
#' @param colon_sep_values a character vector with lines of the form \code{"key:
#'   value"}
#' @return a named list of the values in the colon-separated lines. \code{"key:
#'   value"} yields \code{list(key = "value")}
#' @examples
#' lines <- c("\t*** LogFrame Start ***",
#'            "\tProcedure: PracticeProc",
#'            "\tStimulus1: toothbrush",
#'            "\t*** LogFrame End ***")
#'
#' str(listify(lines))
#' # List of 2
#' # $ Procedure: chr "PracticeProc"
#' # $ Stimulus1: chr "toothbrush"
#'
#' lines2 <- c("Item: 1",
#'             "  WhiteSpaceItem: Stuff\t",
#'             "AnotherItem: Two Colons: Yes",
#'             "",
#'             "Last Item: Whatever")
#'
#' str(listify(lines2))
#' # List of 4
#' # $ Item          : chr "1"
#' # $ WhiteSpaceItem: chr "Stuff"
#' # $ AnotherItem   : chr "Two Colons: Yes"
#' # $ Last Item     : chr "Whatever"
listify <- function(colon_sep_values) {
  colon_sep_values <- str_trim(colon_sep_values)
  colon_sep_values <- Filter(Negate(is_bracket), colon_sep_values)
  colon_sep_values <- Filter(is_row, colon_sep_values)

  splits <- str_split_fixed(colon_sep_values, pattern = ": ", 2)
  structure(as.list(splits[, 2]), names = splits[, 1])
}

#' Infer level of nesting by counting tabs
count_tabs <- function(colon_sep_values) {
  tab_count <- str_count(colon_sep_values[1], "\\t")
  level <- tab_count + 1
  structure(list(level), names = rprime_cols$level)
}
