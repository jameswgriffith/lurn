#' @importFrom rlang .data
lurn_si_10_item_plot <- function(x, title) {

  invisible(x)

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

    match_j <- match(x[x$Item == item, "Item response"], rng, nomatch = length(rng) + 1)

    x[x$Item == item, "Item response"] <- rng_nm[match_j]

  }

  x$Item <- factor(x$Item,
                   levels = lurn_si_10_names)

  item_levels <- c(
    "Every time",
    "Most of the time",
    "About half the time",
    "A few times",
    "Never",
    "11 or more times a day",
    "8-10 times a day",
    "4-7 times a day",
    "3 or fewer times a day",
    "More than 3 times",
    "2-3 times",
    "1 time",
    "none",
    "Extremely bothered",
    "Very bothered",
    "Somewhat bothered",
    "Not at all bothered",
    "Out-of-range",
    NA)

  resp_colors <-
    c("#08519c",
      "#3182bd",
      "#6baed6",
      "#bdd7e7",
      "#eff3ff",
      "#d94701",
      "#fd8d3c",
      "#fdbe85",
      "#feedde",
      "#238b45",
      "#74c476",
      "#bae4b3",
      "#edf8e9",
      "#6a51a3",
      "#9e9ac8",
      "#cbc9e2",
      "#f2f0f7",
      "#FF0000",
      "grey50")

  item_labels <- c(
    SI10_Q1 = "Q1: Urgency",
    SI10_Q2 = "Q2: UUI",
    SI10_Q3 = "Q3: SUI: Laugh/Sneeze/Cough",
    SI10_Q4 = "Q4: SUI: Straining",
    SI10_Q5 = "Q5: Pain with filling",
    SI10_Q6 = "Q6: Hesitancy",
    SI10_Q7 = "Q7: Weak stream",
    SI10_Q8 = "Q8: Post-void dribbling",
    SI10_Q9 = "Q9: Daytime frequency",
    SI10_Q10 = "Q10: Nightime frequency",
    SI10_BOTHER = "Bother")

  ggplot2::ggplot(x, ggplot2::aes(x = .data$Item,
                                  fill = .data$`Item response`)) +
    ggplot2::geom_bar(position = "fill",
                      colour = "black",
                      show.legend = TRUE) +
    ggplot2::scale_y_continuous(
      name = "Proportion or item responses",
      expand = c(0.005, 0.005),
      limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = resp_colors,
                               breaks = item_levels) +
    ggplot2::scale_x_discrete(
      "LURN SI-10 Item",
      labels = item_labels,
      expand = c(0.05, 0.05)) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 0.0,
        face = "bold",
        size = ggplot2::rel(1.1)),
      axis.line = ggplot2::element_line(size = ggplot2::rel(0.5),
                                        colour = "black",
                                        linetype = 1),
      axis.text.x = ggplot2::element_text(
        angle = -45,
        vjust = 0.5,
        hjust = 0.025,
        size = ggplot2::rel(1.2)),
      axis.text.y = ggplot2::element_text(
        size = ggplot2:: rel(1.1)),
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(1.3),
        face = "bold"),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.5)))
}
