#' Draws a biplot given a mixOmics plot object.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @import tidyverse
#' @importFrom ggthemes "scale_color_few"
#' @importFrom magrittr "%>%"
#' @export
ggbiplot <- function(model, col = NULL, ...) {
  plot_comp <- ggcompplot(model)
  plot_var <- ggvarplot(model)

  # rescale components to -1 to 1
  df <- plot_comp$data

}
