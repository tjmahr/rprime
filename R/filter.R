#' Make a filtering predicate
make_filter <- function(key, values) {
  function(eprime_lists) {
    # Convert NULLs to NA
    plucked <- pluck_apply(key, eprime_lists)
    plucked[sapply(plucked, is.null)] <- NA
    is.element(unlist(plucked), values)
  }
}



#' @export
filter_in <- function(eprime_lists, key, values) {
  has_key_value <- make_filter(key, values)
  eprime_lists[has_key_value(eprime_lists)]
}

#' @export
filter_out <- function(eprime_lists, key, values) {
  lacks_key_value <- Negate(make_filter(key, values))
  eprime_lists[lacks_key_value(eprime_lists)]
}

#' @export
keep_levels <- function(eprime_lists, level_numbers) {
  filter_in(eprime_lists, rprime_cols$level, as.character(level_numbers))
}

#' @export
drop_levels <- function(eprime_lists, level_numbers) {
  filter_out(eprime_lists, rprime_cols$level, as.character(level_numbers))
}
