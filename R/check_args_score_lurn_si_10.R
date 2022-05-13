check_args_score_lurn_si_10 <- function(
    input,
    transfer_vars) {

  si_10_names <- lurn_si_10_names(include_bother_item = FALSE)

  returned_vars <- c("lurn_si_10_score",
                     "lurn_si_10_count_valid",
                     "lurn_si_10_note")

  if (ncol(input) < 10) {
    stop("Your input has too few columns for the LURN SI-10.\n",
         "Please re-check your input and try again.",
         call. = FALSE)
  }

  if (!all(si_10_names %in% names(input))) {

    stop(c("\n\nThe names of the LURN SI-10 were not found in the input.\n",
           "The ten scored items of the LURN SI-10 should be labelled\n\n",
           paste(lurn_si_10_names(include_bother_item = FALSE), collapse = " "),
           "\n\nPlease try again."),
         call. = FALSE)
  }

  if (!all(transfer_vars %in% names(input))) {
    stop(paste0("\n\nWe can only return the scoring results of the SI-10 and ",
                "variables found in the input.\n",
                "Please try again, choosing only variable names found in input."),
         call. = FALSE)
  }

  if (any(returned_vars %in% names(input))) {
    stop(paste0("Varible names resulting from SI-10 scoring are already ",
                "found in the names of input.\n",
                "Please modify your input and try again."),
         call. = FALSE)
  }
}
