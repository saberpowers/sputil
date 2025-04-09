#' Write LaTex table to file
#' 
#' This function is necessary because for some reason xtable::print.xtable includes an end-line
#' character at the end of its tables, which causes a blank row to be appended to the bottom of the
#' table. The most important feature of this function is that 
#' 
#' @param table a dataframe to be converted to a L
#' @param file a file name (presumably ending in .tex) to which to write the LaTeX table
#' @param include.rownames If `TRUE` the rows names are printed. Default value is `FALSE`.
#' @param ... additional arugments to `print.xtable`
#' 
#' @export
write_latex_table <- function(table, file, include.rownames = FALSE, ...) {
  text <- table |>
    xtable::xtable() |>
    print(
      hline.after = NULL,
      include.rownames = include.rownames,
      include.colnames = FALSE,
      only.contents = TRUE,
      sanitize.text.function = identity,
      ...
    ) |>
    capture.output()
  text <- text[-length(text)]
  text[length(text)] <- text[length(text)] |>
    stringr::str_replace(" $", "") |>
    stringr::str_replace("\\\\\\\\$", "")
  write(text, file = file)
}
