#' Retrieve a Named Color Based on UI Mode
#'
#' Returns a hexadecimal or named color string depending on the specified color category
#' and display mode (light or dark). Useful for theming plots, tables, or UI elements.
#'
#' @param color Character. A color name to retrieve. Options include:
#'   \code{"fg"}, \code{"bg"}, \code{"base"}, \code{"base2"},
#'   \code{"red"}, \code{"orange"}, \code{"yellow"}, \code{"green"}, \code{"cyan"}, \code{"blue"},
#'   \code{"violet"}, \code{"magenta"}, \code{"gray"}.
#'
#' @param mode Character. UI mode for which to retrieve the color. Either \code{"dark"} or \code{"light"}.
#'
#' @return A character string representing a color in hexadecimal or named format.
#'
#' @details
#' This function is inspired by the Solarized color palette and returns different shades
#' depending on whether the requested mode is "dark" or "light". It can be used to maintain
#' visual consistency in applications that support theming.
#'
#' If an unrecognized color is passed, it defaults to a gray fallback.
#'
#' @examples
#' color("fg", "dark")       # returns "#93a1a1"
#' color("red", "light")     # returns "darkred"
#' color("cyan", "dark")     # returns "#2aa198"
#'
#' @export
color <- function(color = c(
                    "fg", "bg", "base", "base2",
                    "red", "orange", "yellow", "green", "cyan", "blue",
                    "violet", "magenta", "gray"
                  ),
                  mode = c("dark", "light")) {
  color <- match.arg(color)
  mode <- match.arg(mode)
  switch(
    EXPR = color,
    fg = switch(mode, dark = "#93a1a1", light = "#002B37"),
    bg = switch(mode, dark = "#002B37", light = "white"),
    base = switch(mode, dark = "#073642", light = "#eee8d5"),
    base2 = switch(mode, dark = "#eee8d5", light = "#073642"),
    red = switch(mode, dark = "#dc322f", light = "#e6352fff"),
    orange = switch(mode, dark = "#cb4b16", light = "darkorange"),
    yellow = switch(mode, dark = "#b58900", light = "#f9b90aff"),
    green = switch(mode, dark = "#859900", light = "#34a74bff"),
    cyan = switch(mode, dark = "#2aa198", light = "darkcyan"),
    blue = switch(mode, dark = "#268bd2", light = "#3d79f3ff"),
    violet = switch(mode, dark = "#6c71c4", light = "darkviolet"),
    magenta = switch(mode, dark = "#d33682", light = "darkmagenta"),
    gray = switch(mode, dark = "#93a1a1", light = "#a9a9a9")
  )
}
