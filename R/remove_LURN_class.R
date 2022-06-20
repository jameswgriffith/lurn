#' Removes the LURN_SI_10 class from an object
#'
#' @description This function removes the "LURN_SI_10" class
#' from your object if you do not want to use LURN-specific
#' plot(), summary(), autoplot() and other methods.
#'
#' @param x An object - usually a dataframe that has
#' been created by score_lurn_si_10.
#'
#' @seealso score_lurn_si_10()
#'
#' @return x with the LURN_SI_10 class removed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' some_results <- score_lurn_si_10(input = lurn_si_10_test_data,
#' output_LURN_class = TRUE)
#'
#' class(some_results)
#'
#' some_results <- remove_LURN_class(some_results)
#'
#' class(some_results)
#'
#' }
remove_LURN_class <- function(x) {
  class(x) <- class(x)[class(x) != "LURN_SI_10"]
  return(x)
}
