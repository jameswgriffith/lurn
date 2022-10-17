recode_oor_in_col_to_na <- function(items, item_ranges) {

  if (ncol(items) != length(item_ranges)) {
    stop("The number of columns in items must be the ",
    "same length as item_ranges.\n\n",
    "Please fix your code.")
  }

  for (i in seq_along(colnames(items))) {

    item <- items[[i]]

    item[!item %in% item_ranges[[i]]] <- NA

    items[[i]] <- item

  }

  return(items)

}
