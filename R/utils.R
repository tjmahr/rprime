first <- function(...) head(..., n = 1)
but_last <- function(...) head(..., n = -1)
last <- function(...) tail(..., n = 1)

length_zero <- function(x) length(x) == 0
length_one <- function(x) length(x) == 1

is_list_of <- function(xs, classes) {
  assert_that(is.list(xs))
  all(vapply(xs, function(x) inherits(x, classes), logical(1)))
}

merge_lists <- function(x, y) {
  x[names(y)] <- y
  x
}

#' Higher-order functions for dealing with lists
#'
#' These functions were inspired by underscore.js.
#'
#' @name list_functions
#' @param key the name of the value to pluck from a list
#' @param keys a character vector of names used to simplify the list
#' @param xss a list of lists
#' @return \code{pluck} returns an unnamed value and \code{pluck_apply} returns
#'   a list of unnamed values. \code{pick} and \code{omit} return a simplified
#'   version of the original list. \code{pick_apply} and \code{omit_apply}
#'   return a list of simplified lists.
#'
#' @details \itemize{ \item \code{pluck}: Pluck a named value from a list \item
#' \code{pick}: Simplify a list by picking out whitelisted names \item
#' \code{omit}: Simplify a list by omitting blacklisted names. }
#'
#' The simple versions of \code{pluck}, \code{pick}, and \code{omit} are curried
#' functions, meaning that they return a function which can be applied to a
#' list. See the syntax in the usage section.
NULL


#' @rdname list_functions
#' @usage pluck(key)(xs)
#' @export
pluck <- function(key) {
  function(xs) xs[[key]]
}

#' @rdname list_functions
#' @export
pluck_apply <- function(key, xss) {
  lapply(xss, pluck(key))
}


#' @rdname list_functions
#' @export
pick <- function(keys) {
  function(xs) {
    classes <- class(xs)
    xs <- xs[is.element(names(xs), keys)]
    class(xs) <- classes
    xs
  }
}

#' @rdname list_functions
#' @export
pick_apply <- function(keys, xss) {
  classes <- class(xss)
  xss <- lapply(xss, pick(keys))
  class(xss) <- classes
  xss
}

#' @rdname list_functions
#' @export
omit <- function(keys) {
  function(xs) {
    classes <- class(xs)
    xs <- xs[!is.element(names(xs), keys)]
    class(xs) <- classes
    xs
  }
}

#' @rdname list_functions
#' @export
omit_apply <- function(keys, xss) {
  classes <- class(xss)
  xss <- lapply(xss, omit(keys))
  class(xss) <- classes
  xss
}



