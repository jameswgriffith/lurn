# Count non-NA values row-by-row ------------------------------------------

count_not_na <- function(items) {
  apply(items,
        1,
        function(x) {
          sum(!is.na(x))
        })
}
