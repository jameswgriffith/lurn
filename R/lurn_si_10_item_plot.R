#' Uses the \href{https://ggplot2.tidyverse.org/}{ggplot2} and
#' \href{https://patchwork.data-imaginist.com/}{patchwork} packages to create a
#' set of stacked bar plots for for the LURN SI-10, which shows the proportion
#' of item responses for each item (All 10 symptom items, plus the bother item).
#'
#' @description Any responses that are out-of-range or character data will
#' be shown in red, to signal potential issues with data quality.
#'
#' @param input A dataframe containing LURN SI-10 items. Other columns may
#' also be present. The items of the SI-10 must use the recommended names:
#' SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names;
#' the variable names must be in uppercase.
#'
#' @param title We encourage you to use a descriptive title for your plot.
#' This parameter is NULL by default, which will not include a title.
#'
#' @section Item response coding: Items 1-8 are coded with 0-4;
#' Items 9, 10 are coded with 0-3;
#' the bother question is coded with 0-3.
#' This coding must be respected in order for the plot to be produced properly.
#'
#' @return A patchwork object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lurn_si_10_item_plot(lurn_si_10_test_data)
#'
#' # Save the plot
#' si10_item_plot <- lurn_si_10_item_plot(lurn_si_10_test_data)
#'
#' }
lurn_si_10_item_plot <- function(input, title = NULL) {

  x <- input

  # Check for errors in the input
  error_check_si_10_item_plot(x, title)

  x[["id"]] <- rownames(x)

  lurn_si_10_names <- lurn_si_10_names(include_bother_item = TRUE)
  item_ranges <- lurn_si_10_item_ranges(include_na = TRUE)

  x <- stats::reshape(
    x,
    direction = "long",
    v.names = "Item response",
    timevar = "Item",
    times = lurn_si_10_names,
    varying = lurn_si_10_names,
    idvar = "id")

  rownames(x) <- NULL

  x[["Item response"]] <- as.character(x[["Item response"]])

  for (i in seq_along(lurn_si_10_names)) {

    item <- lurn_si_10_names[i]
    rng <- item_ranges[[i]]
    rng_nm <- c(names(rng), "Out-of-range")
    rng_nm[rng_nm == ""] <- NA

    match_j <- match(x[x$Item == item, "Item response"],
                     rng,
                     nomatch = length(rng) + 1)

    x[x$Item == item, "Item response"] <- rng_nm[match_j]

  }

  x$`Item response`[is.na(x$`Item response`)] <- "Missing (NA)"

  # Here, one creates four separate data frames, one for each item type
  x1 <- x[x$Item %in% lurn_si_10_names[1:8], , drop = FALSE]
  x2 <- x[x$Item %in% lurn_si_10_names[9], , drop = FALSE]
  x3 <- x[x$Item %in% lurn_si_10_names[10], , drop = FALSE]
  x4 <- x[x$Item %in% lurn_si_10_names[11], , drop = FALSE]

  x1$Item <- factor(x1$Item)
  x2$Item <- factor(x2$Item)
  x3$Item <- factor(x3$Item)
  x4$Item <- factor(x4$Item)

  item_levels <- item_plot_levels()

  x1$`Item response` <- factor(x1$`Item response`,
                               levels = item_levels[["SI10_Q1_Q8"]])

  x2$`Item response` <- factor(x2$`Item response`,
                               levels = item_levels[["SI10_Q9"]])

  x3$`Item response` <- factor(x3$`Item response`,
                               levels = item_levels[["SI10_Q10"]])

  x4$`Item response` <- factor(x4$`Item response`,
                               levels = item_levels[["SI10_BOTHER"]])

  # Create constituent plots for patchwork
  item_plot_Q1_Q8 <- item_plot_Q1_Q8(x1)
  item_plot_Q9 <- item_plot_Q9(x2)
  item_plot_Q10 <- item_plot_Q10(x3)
  item_plot_bother <- item_plot_bother(x4)

  p <- item_plot_Q1_Q8 + (item_plot_Q9 / item_plot_Q10 / item_plot_bother)

  p + patchwork::plot_annotation(title = title)

}
