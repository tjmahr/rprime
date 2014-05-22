# pluck
# _.pluck(list, propertyName)
# A convenient version of what is perhaps the most common use-case for map: extracting a list of property values.
# var stooges = [{name: 'moe', age: 40}, {name: 'larry', age: 50}, {name: 'curly', age: 60}];
# _.pluck(stooges, 'name');
# => ["moe", "larry", "curly"]

# pick
# _.pick(object, *keys)
# Return a copy of the object, filtered to only have values for the whitelisted keys (or array of valid keys).
#
# _.pick({name: 'moe', age: 50, userid: 'moe1'}, 'name', 'age');
# => {name: 'moe', age: 50}

# omit
# _.omit(object, *keys)
# Return a copy of the object, filtered to omit the blacklisted keys (or array of keys).
#
# _.omit({name: 'moe', age: 50, userid: 'moe1'}, 'userid');
# => {name: 'moe', age: 50}





#' Higher-order functions for dealing with lists
#'
#' These functions were inspired by underscore.js.
#'
#' @name list_functions
#' @param key the name of the value to pluck from a list
#' @param keys a character vector of names used to simplify the list
#' @param xs a list
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
#' @usage pick(keys)(xs)
#' @export
pick <- function(keys) {
  function(xs) xs[is.element(names(xs), keys)]
}

#' @rdname list_functions
#' @export
pick_apply <- function(keys, xss) {
  lapply(xss, pick(keys))
}

#' @rdname list_functions
#' @usage omit(keys)(xs)
#' @export
omit <- function(keys) {
  function(xs) xs[!is.element(names(xs), keys)]
}

#' @rdname list_functions
#' @export
omit_apply <- function(keys, xss) {
  lapply(xss, pick(keys))
}
