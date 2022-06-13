.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The ", pkgname,
    " package is still in alpha testing. Use with caution.")
}
