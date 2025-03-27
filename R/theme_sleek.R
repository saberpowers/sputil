#' Sleek theme for ggplot2
#' 
#' This function creates a classic theme with options dark (solarized dark) or light (for papers).
#' 
#' @param mode "dark" or "light"
#' 
#' @export
theme_sleek <- function(mode = c("dark", "light")) {

  mode <- match.arg(mode)

  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = color("axis", mode)),
      axis.ticks = ggplot2::element_line(color = color("axis", mode)),
      axis.text = ggplot2::element_text(color = color("axis", mode)),
      strip.text = ggplot2::element_text(color = color("axis", mode)),
      text = ggplot2::element_text(color = color("axis", mode)),
      legend.background = ggplot2::element_rect(fill = NA),
      panel.background = ggplot2::element_rect(fill = color("bg", mode)),
      plot.background = ggplot2::element_rect(fill = color("bg", mode), color = color("bg", mode)),
      strip.background = ggplot2::element_rect(fill = color("bg", mode), color = color("bg", mode))
    )
}
