#' Preview the levels in a parsed Eprime file
#' @export
preview_eprime <- function(eprime_lists) {
  preview_levels(eprime_lists)
  cat("\n")
  preview_chunks(eprime_lists)
  invisible(NULL)
}

#' @rdname preview_eprime
#' @export
preview_levels <- function(eprime_lists) {
  prep <- preview_prep(eprime_lists)
  cat("Unique Levels: \n")
  print(prep$unique_rows, row.names = FALSE)
  invisible(NULL)
}

#' @rdname preview_eprime
#' @export
preview_chunks <- function(eprime_lists) {
  prep <- preview_prep(eprime_lists)

  for(chunk_num in seq_along(prep$unique_chunks)) {
    curr_row <- prep$unique_rows[chunk_num, ]
    curr_chunk <- prep$unique_chunks[[chunk_num]]

    cat("\n")
    print(curr_row, row.names = FALSE)
    str(curr_chunk)
  }

  invisible(NULL)
}

preview_prep <- function(eprime_lists) {
  main_cols <- pick_apply(c("Eprime.Level", "Running", "Procedure"), eprime_lists)
  full_table <- frame_lists(main_cols)
  unique_rows <- unique(full_table)
  unique_chunks <- eprime_lists[as.numeric(row.names(unique_rows))]

  list(unique_rows = unique_rows,
       unique_chunks = unique_chunks)
}



#' @export
#' @importFrom plyr rbind.fill
to_data_frame <- function(frame_list) {
  data_frames <- lapply(frame_list, as.data.frame, stringsAsFactors = FALSE)
  plyr::rbind.fill(data_frames)
}
