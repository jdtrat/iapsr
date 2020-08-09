#' Mutate at specific rows based on a specific condition.
#'
#' This function was developed by Davis Vaughan and can be found at his GitHub
#' gist. https://gist.github.com/DavisVaughan/24cbc404c09e75d3bf23467d15a7d42d.
#'
#' I am using it in this package as it seems more logical to me than to mutate by column and
#' case_when
#'
#' @param .data The data being supplioed.
#' @param .predicate The condition to be met
#' @param ... Any number of columns/values to impute.
#'
#' @return The mutated data frame.
#'
mutate_rows <- function(.data, .predicate, ...) {
  .predicate <- rlang::enquo(.predicate)
  .predicate_lgl <- rlang::eval_tidy(.predicate, .data)
  .data[.predicate_lgl, ] <- dplyr::mutate(.data[.predicate_lgl, ], ...)
  .data
}

