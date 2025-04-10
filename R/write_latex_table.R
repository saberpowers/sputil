#' Write LaTex table to file
#' 
#' This function is necessary because for some reason xtable::print.xtable includes an end-line
#' character at the end of its tables, which causes a blank row to be appended to the bottom of the
#' table. The most important feature of this function is that 
#' 
#' @param table a dataframe to be converted to a L
#' @param file a file name (presumably ending in .tex) to which to write the LaTeX table
#' @param buffer_row row number before which to insert blank row
#' @param include.rownames If `TRUE` the rows names are printed. Default value is `FALSE`.
#' @param ... additional arugments to `print.xtable`
#' 
#' @export
write_latex_table <- function(table, file, buffer_row = NULL, include.rownames = FALSE, ...) {

  if (!is.null(buffer_row)) {
    table_top <- dplyr::slice(table, 1:(buffer_row - 1))
    table_bot <- dplyr::slice(table, buffer_row:dplyr::n())
    buffer <- table |>
      dplyr::slice(1) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), function(x) {NA})) |>
      tibble::remove_rownames()
    table <- dplyr::bind_rows(table_top, buffer, table_bot)
  }

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
