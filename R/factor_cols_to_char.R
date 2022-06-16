factor_cols_to_char <- function(x) {
  as.data.frame(
    apply(x, 2, function(col) {
      if (is.factor(col)) {
        as.character(col)
        } else {
          col
        }
    }))
}
