# Convert out of range to NA ----------------------------------------------
out_of_rng_to_na <- function(items, valid_range) {

  apply(items,
        c(1, 2),
        function(response) {
          if(response %in% valid_range) {
            recoded <- response
          } else {
            recoded <- NA
          }
          return(recoded)
        })
}

