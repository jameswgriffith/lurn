item_plot_Q1_Q8 <- function(x) {

  resp_colors <- item_plot_resp_colors("SI10_Q1_Q8")
  item_levels <- item_plot_levels("SI10_Q1_Q8")

  item_labels_1_8 <- c(
    SI10_Q1 = "Q1: Urgency",
    SI10_Q2 = "Q2: UUI",
    SI10_Q3 = "Q3: SUI: Laugh/Sneeze/Cough",
    SI10_Q4 = "Q4: SUI: Straining",
    SI10_Q5 = "Q5: Pain with filling",
    SI10_Q6 = "Q6: Hesitancy",
    SI10_Q7 = "Q7: Weak stream",
    SI10_Q8 = "Q8: Post-void dribbling")

  ggplot2::ggplot(x, ggplot2::aes(.data$Item, fill = .data$`Item response`)) +
    ggplot2::geom_bar(position = "fill",
                      width = 0.9,
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
                              labels = item_labels_1_8,
                              expand = c(0.05, 0.05),
                              limits = rev,
                              drop = FALSE) +
    ggplot2::ggtitle("LURN SI-10: Items 1-8") +
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
