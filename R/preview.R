#' Preview the levels in a parsed Eprime file
#'
#' @details \code{preview_levels} prints out the unique combinations of
#' Eprime.Level number, Procedure, and Running in the frame list.
#' \code{preview_frames} prints out example frame from each of the unique
#' levels. \code{preview_eprime} does both.
#'
#' @param frame_list a FrameList (a list of EprimeFrames)
#' @return Nothing. Preview text is printed to the console.
#' @export
preview_eprime <- function(frame_list) {
  preview_levels(frame_list)
  cat("\n")
  preview_frames(frame_list)
  invisible(NULL)
}

#' @rdname preview_eprime
#' @export
preview_levels <- function(frame_list) {
  prep <- preview_prep(frame_list)
  cat("Unique Levels: \n")
  print(prep$unique_rows, row.names = FALSE)
  invisible(NULL)
}

#' @rdname preview_eprime
#' @export
preview_frames <- function(frame_list) {
  prep <- preview_prep(frame_list)

  for(chunk_num in seq_along(prep$unique_frames)) {
    curr_row <- prep$unique_rows[chunk_num, ]
    curr_chunk <- prep$unique_frames[[chunk_num]]

    cat("\n")
    print(curr_row, row.names = FALSE)
    str(curr_chunk)
  }

  invisible(NULL)
}

preview_prep <- function(frame_list) {
  main_cols <- pick_apply(c("Eprime.Level", "Running", "Procedure"), frame_list)

  full_table <- to_data_frame(main_cols)
  unique_rows <- unique(full_table)
  unique_frames <- as_frame_list(frame_list[as.numeric(row.names(unique_rows))])

  list(unique_rows = unique_rows,
       unique_frames = unique_frames)
}




#' Convert Eprime Frames into data-frames
#'
#' @details Individual Eprime Frames are converted to a data-frame using
#'   \code{as.data.frame}. Strings are not converted to factors.
#'
#'   Each of the individual data-frames are then \code{rbind}ed together, with
#'   missing columns being filled with NA.
#'
#' @param eprime_frame an EprimeFrame object
#' @param frame_list a FrameList object (a list of EprimeFrames)
#' @return all of the EprimeFrames combined into a single data frame.
#' @importFrom plyr rbind.fill
#' @export
#' @seealso plyr::rbind.fill
to_data_frame <- function(...) UseMethod("to_data_frame")

to_data_frame.default <- function(frame_list) {
  require("plyr")
  data_frames <- lapply(frame_list, as.data.frame, stringsAsFactors = FALSE)
  rbind.fill(data_frames)
}

#' @export
to_data_frame.EprimeFrame <- function(eprime_frame) {
  as.data.frame(eprime_frame, stringsAsFactors = FALSE)
}

#' @export
to_data_frame.FrameList <- function(frame_list) {
  require("plyr")
  data_frames <- lapply(frame_list, to_data_frame)
  rbind.fill(data_frames)
}

