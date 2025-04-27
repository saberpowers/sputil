#' Remove all axes from a ggplot figure
#' 
#' Why isn't this a one-liner in ggplot2?
#' 
#' @export
remove_axes <- function(remove_x = TRUE, remove_y = TRUE) {

  if (remove_x) {
    remove_x_axis <- ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  } else {
    remove_x_axis <- ggplot2::theme()
  }

  if (remove_y) {
    remove_y_axis <- ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  } else {
    remove_y_axis <- ggplot2::theme()
  }

  remove_x_axis + remove_y_axis
}
