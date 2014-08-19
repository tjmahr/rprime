
#' Create an EprimeFrame object
#'
#' This constructor function converts a list object into an \code{EprimeFrame}
#' object, which is just a list with some special metadata values. Just use
#' \link{make_eprime_frames} instead.
#'
#' @param key_value_list a list of named elements
#' @param ... additional arguments are treated as \code{key = value} pairs.
#'   These arguments are merged into \code{key_value_list}.
#' @return a list with the class \code{EprimeFrame} and with special
#'   \code{Eprime.} metadata, \code{Running} and \code{Procedure} values, all
#'   set to NA by default.
#' @export
#' @examples
#' key_value_list <- list(
#'   Procedure = "FamTask",
#'   item1 = "bear",
#'   item2 = "chair",
#'   CorrectResponse = "bear",
#'   Familiarization.Cycle = "1",
#'   Familiarization.Sample = "1",
#'   Running = "Familiarization",
#'   Correct = "True")
#'
#' EprimeFrame(key_value_list)
#' # List of 12
#' # $ Eprime.LevelName      : logi NA
#' # $ Eprime.Level          : logi NA
#' # $ Eprime.Basename       : logi NA
#' # $ Eprime.FrameNumber    : logi NA
#' # $ Procedure             : chr "FamTask"
#' # $ Running               : chr "Familiarization"
#' # $ item1                 : chr "bear"
#' # $ item2                 : chr "chair"
#' # $ CorrectResponse       : chr "bear"
#' # $ Familiarization.Cycle : chr "1"
#' # $ Familiarization.Sample: chr "1"
#' # $ Correct               : chr "True"
#' # - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#'
#' # Additional parameters are treated as a list and
#' # overwrite values in the list
#' EprimeFrame(key_value_list, Procedure = "Demo", Running = "Practice")
#' # List of 12
#' # $ Eprime.LevelName      : logi NA
#' # $ Eprime.Level          : logi NA
#' # $ Eprime.Basename       : logi NA
#' # $ Eprime.FrameNumber    : logi NA
#' # $ Procedure             : chr "Demo"
#' # $ Running               : chr "Practice"
#' # $ item1                 : chr "bear"
#' # $ item2                 : chr "chair"
#' # $ CorrectResponse       : chr "bear"
#' # $ Familiarization.Cycle : chr "1"
#' # $ Familiarization.Sample: chr "1"
#' # $ Correct               : chr "True"
#' # - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
EprimeFrame <- function(key_value_list = list(), ...) {
  dots <- merge_lists(key_value_list, list(...))
  as.EprimeFrame(dots)
}

#' @export
print.EprimeFrame <- function(...) str(...)

#' @export
print.FrameList <- function(...) str(...)


#' @export
as.EprimeFrame <- function(xs) {
  assert_that(inherits(xs, "list"))
  eprime_metadata <- unlist(rprime_cols, use.names = FALSE)
  defaults <- structure(as.list(rep(NA, length(eprime_metadata))),
                        names = eprime_metadata)
  classes <- unique(c(class(xs), "EprimeFrame", "list"))
  structure(merge_lists(defaults, xs), class = classes)
}

#' @export
as.FrameList <- function(xss) {
  assert_that(inherits(xss, "list"), is_list_of_lists(xss))
  class(xss) <- unique(c(class(xss), "FrameList", "list"))
  xss
}



#' Extract chunks of text and convert into Eprime Frames
#'
#' This is a shortcut for \code{make_eprime_frames(extract_chunks(eprime_log))}.
#' @param eprime_log a character vector containing the lines of text from Eprime
#'   txt file
#' @return a FrameList object (a list of EprimeFrame objects)
#' @export
extract_frames <- function(eprime_log) {
  make_eprime_frames(extract_chunks(eprime_log))
}

#' Convert log-frames into EprimeFrames
#'
#' In other words, convert character vectors of implicit key-value pairs,
#' \code{c("key: value", ...)}, into lists of explicit key-value pairs,
#' \code{list(key = value, ...)}.
#'
#' @details During the conversion, if \code{Running: x}, then the
#'   \code{x.Sample} and \code{x.Cycle} lines are simplified into \code{Sample}
#'   and \code{Cycle} lines. The \code{x: value} line is recoded as
#'   \code{Eprime.LevelName: x_value}. The purpose of this tidying is to force
#'   the same set of key names (eventually, column names) onto frames with
#'   different values for "Running".
#'
#' @param x a character vector with lines of the form \code{"key: value"}, or a
#'   list of vectors of colon-separated text
#' @return when passed a vector of \code{"key: value"} lines, a single
#'   EprimeFrame object is returned. When passed a list of such vectors, a
#'   FrameList object (a list of EprimeFrames) is returned.
#' @export
#' @examples
#' lines <- c("\t*** LogFrame Start ***",
#'            "\tProcedure: FamTask",
#'            "\titem1: bear",
#'            "\titem2: chair",
#'            "\tCorrectResponse: bear",
#'            "\tImageSide: Left",
#'            "\tDuration: 885",
#'            "\tFamiliarization: 1",
#'            "\tFamInforcer: 1",
#'            "\tReinforcerImage: Bicycle1",
#'            "\tFamiliarization.Cycle: 1",
#'            "\tFamiliarization.Sample: 1",
#'            "\tRunning: Familiarization",
#'            "\tFamTarget.RESP: ",
#'            "\tCorrect: True",
#'            "\t*** LogFrame End ***")
#'
#' make_eprime_frames(lines)
#' # List of 16
#' # $ Eprime.LevelName  : chr "Familiarization_1"
#' # $ Eprime.Level      : num 2
#' # $ Eprime.Basename   : logi NA
#' # $ Eprime.FrameNumber: logi NA
#' # $ Procedure         : chr "FamTask"
#' # $ Running           : chr "Familiarization"
#' # $ item1             : chr "bear"
#' # $ item2             : chr "chair"
#' # $ CorrectResponse   : chr "bear"
#' # $ ImageSide         : chr "Left"
#' # $ Duration          : chr "885"
#' # $ FamInforcer       : chr "1"
#' # $ ReinforcerImage   : chr "Bicycle1"
#' # $ Cycle             : chr "1"
#' # $ Sample            : chr "1"
#' # $ Correct           : chr "True"
#' # - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
make_eprime_frames <- function(x) {
  UseMethod("make_eprime_frames")
}

#' @export
make_eprime_frames.list <- function(x) {
  as.FrameList(lapply(x, make_eprime_frames))
}

#' @export
make_eprime_frames.character <- function(x) {
  frame_list <- merge_lists(listify(x), count_tabs(x))
  frame <- EprimeFrame(frame_list)
  tidy_frames(frame)
}






# Clean up `Running`-related attributes
tidy_frames <- function(x) UseMethod("tidy_frames")

tidy_frames.EprimeFrame <- function(x) {
  eprime_frame <- x
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
    names(eprime_frame) <- str_replace(names(eprime_frame),
                                       paste0(level_label, "\\."), "")
    eprime_frame <- merge_lists(eprime_frame, new_list)
  }
  as.EprimeFrame(eprime_frame)
}

#' @keywords internal
tidy_frames.FrameList <- function(x) {
  as.FrameList(lapply(x, tidy_frames))
}




#' Convert a vector of colon-separated text lines into a list of named elements
#'
#' @details Some minor cleaning of the input is performed:
#'   \itemize{
#'     \item Lines without a colon-space separator \code{": "} are filtered out.
#'     \item Lines with more 400+ characters are filtered out. (This fix is
#'           directed at \code{Clock.Information:} lines.)}
#'     \item Once the strings are split at the separator, white-space on the
#'           left and right sides of each half-string is omitted.
#' @param colon_sep_xs a character vector with lines of the form \code{"key:
#'   value"}
#' @return a named list of the values in the colon-separated lines. \code{"key:
#'   value"} yields \code{list(key = "value")}
#' @export
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
listify <- function(colon_sep_xs) {
  colon_sep_xs <- Filter(is_row, colon_sep_xs)
  colon_sep_xs <- Filter(function(x) str_length(x) < 400, colon_sep_xs)
  splits <- str_split_fixed(colon_sep_xs, pattern = ": ", 2)
  # Trim after splitting so "X: " lines are correctly parsed
  splits <- apply(splits, 2, str_trim)
  structure(as.list(splits[, 2]), names = splits[, 1])
}


#' Infer level of nesting (for a log-frame) by counting tabs
#'
#' The number of tabs before the "key: value" information in a log-frame tells
#' where the frame is nested in the experiment's structure.
#' @keywords internal
count_tabs <- function(colon_sep_xs) {
  # Add one because there is no level 0
  level <- str_count(first(colon_sep_xs), "\\t") + 1
  structure(list(level), names = rprime_cols$level)
}
