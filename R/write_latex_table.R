#' Write LaTex table to file
#' 
#' This function is necessary because for some reason xtable::print.xtable includes an end-line
#' character at the end of its tables, which causes a blank row to be appended to the bottom of the
#' table. The most important feature of this function is that 
#' 
#' @param table a dataframe to be converted to a L
#' @param file a file name (presumably ending in .tex) to which to write the LaTeX table
#' @param prefix_rows optional vector of rows to include before the contents of the table,
#'   e.g. super-headers, one character string per row. End-of-line character (double backslash)
#'   will be appended to the end of each row.
#' @param colnames optional vector of print-friendly column names to overwrite `colnames(table)`
#' @param buffer_row row number before which to insert blank row
#' @param align latex string of column aligments (defaults to all c)
#' @param digits vector of integers, passed to `xtable::xtable`
#' @param hline.after a vector indicating the rows after which a horizontal line should appear.
#'   Default value is 0.
#' @param include.rownames If `TRUE` the row names are printed. Default value is `FALSE`.
#' @param include.colnames If `TRUE` the col names are printed. Default value is `FALSE`.
#' @param only.contents This argument must be `FALSE`, and the function will err if it is not.
#'   This is included as an argument only so that the function will stop gracefully if you try to
#'   set this argument to `TRUE`.
#' @param sanitize.test.function A function taking a character vector and returning a sanitized one.
#'   Default value is `identity`.
#' @param ... additional arugments to `print.xtable`
#' @export
write_latex_table <- function(table,
                              file,
                              colnames = NULL,
                              prefix_rows = NULL,
                              buffer_row = NULL,
                              align = NULL,
                              digits = NULL,
                              hline.after = 0,
                              include.rownames = FALSE,
                              include.colnames = TRUE,
                              only.contents = FALSE,
                              sanitize.text.function = identity,
                              ...) {


  if (is.null(colnames)) {
    colnames <- colnames(table)
  }
  if (length(colnames) != ncol(table)) {
    stop("length(colnames) does not match ncol(table)")
  }
  if (include.rownames) {
    colnames <- c("", colnames)
  }

  ncol <- ncol(table) + include.rownames
  if (is.null(align)) {
    align <- paste(rep("c", times = ncol), collapse = "")
  }

  if (only.contents) {
    stop("`only.contents` must be false")
  }

  if (!is.null(buffer_row)) {
    table_top <- dplyr::slice(table, 1:(buffer_row - 1))
    table_bot <- dplyr::slice(table, buffer_row:dplyr::n())
    buffer <- table |>
      dplyr::slice(1) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), function(x) {NA})) |>
      tibble::remove_rownames()
    table <- dplyr::bind_rows(table_top, buffer, table_bot)
  }

  colnames_row <- colnames |>
    paste(collapse = " & ") |>
    paste("\\\\")
  if (!include.colnames) {
    colnames_row <- NULL
  }

  text <- table |>
    xtable::xtable(digits = digits) |>
    print(
      hline.after = hline.after,
      include.rownames = include.rownames,
      include.colnames = FALSE,
      only.contents = TRUE,
      sanitize.text.function = sanitize.text.function,
      ...
    ) |>
    capture.output()

  c(
    paste0("\\begin{tabular}{", align, "}"),
    glue::glue("{prefix_rows}\\\\"),
    colnames_row,
    text[-length(text)],    # final line is always blank
    "\\end{tabular}"
  ) |>
    write(file = file)
}
