item_plot_Q9 <- function(x) {

  resp_colors <- item_plot_resp_colors("SI10_Q9")
  item_levels <- item_plot_levels("SI10_Q9")

  item_labels_9 <- c(SI10_Q9 = "Q9: Daytime frequency")

  ggplot2::ggplot(x, ggplot2::aes(.data$Item,
                                  fill = .data$`Item response`)) +
    ggplot2::geom_bar(position = "fill",
                      width = .075,
                      colour = "black",
                      show.legend = TRUE) +
    ggplot2::scale_y_continuous(
      name = "Proportion of item responses",
      expand = c(0.005, 0.005),
      limits = c(0, 1)) +
    ggplot2::scale_fill_manual(values = resp_colors,
                               breaks = item_levels,
                               drop = FALSE) +
    ggplot2::scale_x_discrete(name = NULL,
                              labels = item_labels_9,
                              expand = c(0.05, 0.05),
                              limits = rev,
                              drop = FALSE) +
    ggplot2::ggtitle("LURN SI-10: Item 9 (Daytime frequency)") +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 0.0,
        face = "bold"),
      axis.line = ggplot2::element_line(linewidth = 0.5,
                                        colour = "black",
                                        linetype = 1),
      axis.text.x = ggplot2::element_text(
        angle = -45,
        vjust = 0.5,
        hjust = 0.025),
      axis.title = ggplot2::element_text(
        face = "bold")) +
    ggplot2::coord_flip()
}
