add_LURN_SI_10_class <- function(x) {
  if (!is.data.frame(x)) {
    stop("The object supplied to add_LURN_SI_10_class is not a dataframe.\n",
         "Please try again.")
  }

  if (!"LURN_SI_10" %in% class(x)) {
    class(x) <- append(class(x), "LURN_SI_10", 0)
  }

  return(x)

}
