check_items_for_miscoding <- function(items,
                                      which_items = c(
                                        "1. LURN SI-10 (not including bother)",
                                        "2. LURN SI-10 (all items)",
                                        "3. LURN SI-29")) {

  which_items <- match.arg(which_items)

  # Initialise empty vector for counting possible miscoded items
  possibly_miscoded_items <- vector(mode = "character")

  item_ranges <- switch(which_items,
    "1. LURN SI-10 (not including bother)" = lurn_si_10_item_ranges(),
    "2. LURN SI-10 (all items)" =
      lurn_si_10_item_ranges(include_bother_item = FALSE),
    "3. LURN SI-29" = lurn_si_29_item_ranges())

  item_names <- names(items)

  for (i in seq_along(item_names)) {
    item_range <- item_ranges[[i]]
    col_to_chk <- items[[item_names[i]]]
    item_name <- item_names[i]

    if (is_item_possibly_miscoded(col_to_chk, item_range)) {
      possibly_miscoded_items <- append(possibly_miscoded_items, item_name)
    }
  }

  if (length(possibly_miscoded_items) > 0) {
    message("\nThere may be one or more mis-coded variables in your data.\n",
            "All item responses should be coded starting at 0\n",
            "(e.g., 0, 1, 2, ...)\n\n",
            "Variables that are possibly mis-coded are as follows:\n",
            paste(possibly_miscoded_items, collapse = " "),
            "\n\nPlease check your data very carefully.\n\n",
            "We recommend that you check your database and codebook ",
            "against the official verion ",
            "of the questionnaire at:\n",
            "https://nih-lurn.org/Resources/Questionnaires\n\n")
  }

  invisible(NULL)

}
