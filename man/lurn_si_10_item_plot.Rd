% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lurn_si_10_item_plot.R
\name{lurn_si_10_item_plot}
\alias{lurn_si_10_item_plot}
\title{Uses the \href{https://ggplot2.tidyverse.org/}{ggplot2} and
\href{https://patchwork.data-imaginist.com/}{patchwork} packages to create a
set of stacked bar plots for for the LURN SI-10, which shows the proportion
of item responses for each item (All 10 symptom items, plus the bother item).}
\usage{
lurn_si_10_item_plot(input, title = NULL)
}
\arguments{
\item{input}{A dataframe containing LURN SI-10 items. Other columns may
also be present. The items of the SI-10 must use the recommended names:
SI10_Q1-SI10_Q10, and SI10_BOTHER. Case matters for the variable names;
the variable names must be in uppercase.}

\item{title}{We encourage you to use a descriptive title for your plot.
This parameter is NULL by default, which will not include a title.}
}
\value{
A patchwork object.
}
\description{
Any responses that are out-of-range or character data will
be shown in red, to signal potential issues with data quality.
}
\section{Item response coding}{
 Items 1-8 are coded with 0-4;
Items 9, 10 are coded with 0-3;
the bother question is coded with 0-3.
This coding must be respected in order for the plot to be produced properly.
}

\examples{
\dontrun{
lurn_si_10_item_plot(lurn_si_10_test_data)

# Save the plot
si10_item_plot <- lurn_si_10_item_plot(lurn_si_10_test_data)

}
}
