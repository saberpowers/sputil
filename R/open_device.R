#' Open a plotting device
#'
#' This function opens a PDF or PNG plotting device.
#' 
#' @param file figure will be written to {file}. Must end in .pdf or .png.
#' @param height height of output figure
#' @param width width of output figure
#' @param ppi_png pixel density (ignored if file ends in .pdf)
#' 
#' @export
open_device <- function(file,
                        height = 7,
                        width = 7,
                        ppi_png = 300) {

  file_type <- substring(file, nchar(file) - 3, nchar(file))
  if (!file_type %in% c(".pdf", ".png")) {
    stop("file must end in .pdf or .png")
  }

  if (file_type == ".pdf") {
    pdf(filename, height = height, width = width)

  } else if (file_type == ".png") {
    png(filename, height = ppi_png * height, width = ppi_png * width, res = ppi_png)
  }

  invisible()
}
