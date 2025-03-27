#' Source part of an R script
#' 
#' This function runs all lines of an R script from a start line through an end line.
#' https://stackoverflow.com/questions/12214963/source-only-part-of-a-file
#' 
#' @param file R script to be sourced
#' @param start first line to execute
#' @param end last line to execute
#' 
#' @export
source_partial <- function(file, start, end, ...) {
    file.lines <- scan(
      file = file,
      what = character(),
      skip = start - 1,
      nlines = end - start + 1,
      sep = "\n",
      quiet = TRUE
    )
    file.lines.collapsed <- paste(file.lines, collapse = "\n")
    source(textConnection(file.lines.collapsed), ...)
}
