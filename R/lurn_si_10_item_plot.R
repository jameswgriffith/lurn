#' @importFrom rlang .data
lurn_si_10_item_plot <- function(x, title) {

  invisible(x)

  x$id <- rownames(x)

  lurn_si_10_names <- lurn_si_10_names(include_bother_item = TRUE)

  x <- stats::reshape(
    x,
    direction = "long",
    v.names = "Item responses",
    timevar = "Item",
    times = lurn_si_10_names,
    varying = lurn_si_10_names,
    idvar = "id")

  rownames(x) <- NULL

  x$`Item responses` <-
    ifelse(x$`Item responses` %in% 0:4 |
             is.na(x$`Item responses`),
           x$`Item responses`,
           "Out-of-range/Character")

  x$Item <- factor(x$Item,
                   levels = lurn_si_10_names)

  x$`Item responses` <-
    factor(x$`Item responses`,
           levels = c(4, 3, 2, 1, 0, "Out-of-range/Character"))

  colours <-
    c("#0868ac",
      "#43a2ca",
      "#7bccc4",
      "#bae4bc",
      "#f0f9e8",
      "#FF0000")

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
                                       fill = .data$`Item responses`)) +
    ggplot2::geom_bar(position = "fill",
                      colour = "black") +
    ggplot2::scale_y_continuous(
      name = "Proportion or item responses",
      expand = c(0.005, 0.005),
      limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_x_discrete(
      "LURN SI-10 Item",
      labels = item_labels,
      expand = c(0.05, 0.05)) +
    ggplot2::labs(caption =
                    paste0("Q1-Q8: Never to Almost always (0-4); ",
                           "Q9: 3 or fewer times to 11 or more times (0-3)\n",
                           "Q10: None to more than 3 times (0-3); ",
                           "Bother: Not at all to Extremely Bothered (0-3)")) +
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
