
#' Create an EprimeFrame object
#'
#' Converts a list into an EprimeFrame object, which is just a list with special
#' metadata values.
#'
#' @details Just use \link{make_eprime_frames} instead
#'
#' @param key_value_list a list of named elements
#' @param ... additional \code{key = value} pairs to merge into
#'   \code{key_value_list}
#' @return a list with the class EprimeFrame and with special \code{Eprime.}
#'   metadata, \code{Running} and \code{Procedure} values, all set to NA by
#'   default.
#' @export
#' @examples
#' EprimeFrame(list())
#' # List of 6
#' #  $ Eprime.LevelName  : logi NA
#' #  $ Eprime.Level      : logi NA
#' #  $ Eprime.Basename   : logi NA
#' #  $ Eprime.FrameNumber: logi NA
#' #  $ Procedure         : logi NA
#' #  $ Running           : logi NA
#' #  - attr(*, "class")= chr [1:2] "list" "EprimeFrame"
#'
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
make_eprime_frames <- function(x, tidy = TRUE) {
  UseMethod("make_eprime_frames")
}

#' @export
make_eprime_frames.list <- function(x) {
  chunk_list <- x
  as.FrameList(lapply(chunk_list, make_eprime_frames))
}

#' @export
make_eprime_frames.character <- function(x) {
  colon_sep_values <- x
  frame_list <- merge_lists(listify(colon_sep_values),
                            count_tabs(colon_sep_values))
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
  frame_list <- x
  as.FrameList(lapply(frame_list, tidy_frames))
}




#' Convert a vector of colon-separated text lines into a list of named elements
#'
#' @details Some minoring cleaning of the input is performed:
#'   \itemize{
#'     \item White-space on both sides of the input strings is omitted.
#'     \item Eprime bracketing lines like \code{"*** LogFrame Start ***"}
#'           are filtered out.
#'     \item Lines without a colon character \code{":"} are filtered out.}
#'
#' @param colon_sep_values a character vector with lines of the form \code{"key:
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
#' @keywords internal
count_tabs <- function(colon_sep_values) {
  tab_count <- str_count(colon_sep_values[1], "\\t")
  level <- tab_count + 1
  structure(list(level), names = rprime_cols$level)
}
