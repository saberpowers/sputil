#' Remove all axes from a ggplot figure
#' 
#' Why isn't this a one-liner in ggplot2?
#' 
#' @export
remove_axes <- function() {
  ggplot2::theme(
    axis.line.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
}
