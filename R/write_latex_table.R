#' Write LaTex table to file
#' 
#' This function is necessary because for some reason xtable::print.xtable includes an end-line
#' character at the end of its tables, which causes a blank row to be appended to the bottom of the
#' table. The most important feature of this function is that 
#' 
#' @param table a dataframe to be converted to a L
#' @param file a file name (presumably ending in .tex) to which to write the LaTeX table
#' 
#' @export
write_latex_table <- function(table, file) {
  text <- table |>
    xtable::xtable() |>
    print(
      hline.after = NULL,
      include.rownames = FALSE,
      include.colnames = FALSE,
      only.contents = TRUE,
      sanitize.text.function = identity
    ) |>
    capture.output()
  text <- text[-length(text)]
  text[length(text)] <- text[length(text)] |>
    stringr::str_replace(" $", "") |>
    stringr::str_replace("\\\\\\\\$", "")
  write(text, file = file)
}
