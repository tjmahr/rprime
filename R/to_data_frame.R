#' Convert `EprimeFrame` objects into dataframes
#'
#' @details Individual [EprimeFrame()] objects are converted to a dataframe
#'   using `as.data.frame()`. (Strings are not converted to factors.)
#'
#'   The individual dataframes are then [rbind()]-ed together, with
#'   missing columns being filled with `NA`.
#'
#' @param x an [EprimeFrame()] object, or a [FrameList()] object (a list of
#'   [EprimeFrame()] objects)
#' @return all of the [EprimeFrame()] objects combined into a single dataframe.
#' @export
#' @seealso [plyr::rbind.fill()]
#' @md
to_data_frame <- function(x) {
  UseMethod("to_data_frame")
}

#' @export
to_data_frame.default <- function(x) {
  data_frames <- lapply(x, as.data.frame.list, stringsAsFactors = FALSE)
  rbind.fill(data_frames)
}

#' @export
to_data_frame.FrameList <- function(x) {
  data_frames <- lapply(x, to_data_frame)
  rbind.fill(data_frames)
}

#' @export
to_data_frame.EprimeFrame <- function(x) {
  as.data.frame.list(x, stringsAsFactors = FALSE)
}
