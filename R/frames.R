
#' Make an EprimeFrame object
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
#'   metadata, \code{Running} and \code{Procedure} values, set to NA by default.
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
  as_eprime_frame(dots)
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
  eprime_metadata <- unlist(rprime_cols, use.names = FALSE)
  defaults <- structure(as.list(rep(NA, length(eprime_metadata))),
                        names = eprime_metadata)
  classes <- unique(c(class(xs), "EprimeFrame", "list"))
  structure(merge_lists(defaults, xs), class = classes)
}

#' @export
as_frame_list <- function(xss) {
  class(xss) <- unique(c(class(xss), "FrameList", "list"))
  xss
}




#' Convert chunks of text into Eprime Frames
#'
#' @param colon_sep_values a character vector with lines of the form \code{"key:
#'   value"}
#' @param chunk_list a list of chunks of colon-separated text
#' @param tidy whether the Eprime Frames should be cleaned up
#' @return when passed a single chunk, a single EprimeFrame object is returned.
#'   When passed a list of chunks, a FrameList object (a list of EprimeFrames)
#'   is returned.
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
#'
#' # Not tidied: The value of "Running" still appears in "Familiarization.Cycle"
#' # and "Familiarization.Sample" and none ofthe "Eprime." metadata attributes
#' # have been updated.
#' make_eprime_frames(lines, tidy = FALSE)
#' # List of 17
#' # $ Eprime.LevelName      : logi NA
#' # $ Eprime.Level          : num 2
#' # $ Eprime.Basename       : logi NA
#' # $ Eprime.FrameNumber    : logi NA
#' # $ Procedure             : chr "FamTask"
#' # $ Running               : chr "Familiarization"
#' # $ item1                 : chr "bear"
#' # $ item2                 : chr "chair"
#' # $ CorrectResponse       : chr "bear"
#' # $ ImageSide             : chr "Left"
#' # $ Duration              : chr "885"
#' # $ Familiarization       : chr "1"
#' # $ FamInforcer           : chr "1"
#' # $ ReinforcerImage       : chr "Bicycle1"
#' # $ Familiarization.Cycle : chr "1"
#' # $ Familiarization.Sample: chr "1"
#' # $ Correct               : chr "True"
#' # - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
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
#' @keywords internal
tidy_frames <- function(...) UseMethod("tidy_frames")

#' @keywords internal
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
    names(eprime_frame) <- str_replace(names(eprime_frame),
                                       paste0(level_label, "\\."), "")
    eprime_frame <- merge_lists(eprime_frame, new_list)
  }
  as_eprime_frame(eprime_frame)
}

#' @keywords internal
tidy_frames.FrameList <- function(frame_list) {
  as_frame_list(lapply(frame_list, tidy_frames))
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
