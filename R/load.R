#' Load a text file outputted by Eprime
#'
#' @param file_name Either the full or relative path to the .txt file that
#'   is to be parsed.
#' @return Each line of the file is stored and returned in a character vector.
#'
#' @details In the past, we have had issues with the encoding of these text
#' files, so we include some exception-handling measures. The procedure is to
#' first try to load the file with UCS-2 Little Endian encoding. If a warning is
#' encountered, the warning is muffled and the file is loaded again, this time
#' without specifying the encoding beforehand. If a warning is encountered on
#' this second attempt, it is printed to the console.
load_eprime <- function(file_name) {
  message(paste0("Loading data in ", file_name))

  # Initialize an empty warning object.
  warned <- NULL

  # Define the procedure to handle warnings: Store them and muffle them.
  handle_warning <- function(w) {
    warned <<- c(warned, w)
    invokeRestart("muffleWarning")
  }

  # Read in a file connection using the warning handler.
  con <- file(file_name, open = "rt" , encoding = "UCS-2LE")
  withCallingHandlers(eprime_log <- readLines(file_name), warning = handle_warning)
  close(con)

  readLines(file_name)

  # If a warning is caught, try connection again with no encoding specified.
  if (length(warned) > 0) {
    con <- file(file_name, open = 'rt')
    withCallingHandlers(eprime_log <- readLines(con), warning = handle_warning)
    close(con)
  }
  eprime_log
}


# library(stringi)

read_eprime <- function(file_name) {
  require(stringi)
  require(tools)
  basename <- file_path_sans_ext(basename(file_name))
  eprime_log <- stri_read_lines(file_name)
  attr(eprime_log, "basename") <- basename
  eprime_log
}



