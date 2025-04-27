#' Pick a color
#' 
#' This function picks an appropriate version of a color based on model (dark/light)
#' 
#' @param color "fg", "bg", "base", "base2", "red", "orange", "yellow", "green" or "blue"
#' @param mode "dark" or "light"
#' 
#' @return a character string representing a color
#' 
#' @export
color <- function(color = c(
                    "fg", "bg", "base", "base2",
                    "red", "orange", "yellow", "green", "cyan", "blue", "violet", "magenta"
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
    red = switch(mode, dark = "#dc322f", light = "darkred"),
    orange = switch(mode, dark = "#cb4b16", light = "darkorange"),
    yellow = switch(mode, dark = "#b58900", light = "darkgoldenrod"),
    green = switch(mode, dark = "#859900", light = "darkgreen"),
    cyan = switch(mode, dark = "#2aa198", light = "darkcyan"),
    blue = switch(mode, dark = "#268bd2", light = "dodgerblue"),
    violet = switch(mode, dark = "#6c71c4", light = "darkviolet"),
    magenta = switch(mode, dark = "#d33682", light = "darkmagenta")
  )
}
