#' Draws a biplot given a mixOmics plot object.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @import tidyverse
#' @importFrom ggthemes "scale_color_few"
#' @importFrom magrittr "%>%"
#' @export
ggbiplot <- function(model, col = NULL, topn = 10, ...) {
  # produce both plots
  plot_comp <- ggcompplot(model, col)
  plot_var <- ggvarplot(model, topn)
  # rescale components to -1 to 1
  plot_comp$data <- plot_comp$data %>% dplyr::mutate_if(is.numeric, .rescale)
  plot_comp +
    geom_vline(xintercept = 0, alpha = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_path(data = .circlefun(diameter = 1), aes(x, y), alpha = 0.2) +
    geom_path(data = .circlefun(diameter = 2), aes(x, y), alpha = 0.2) +
    geom_text_repel(data = plot_var$data, aes(x, y, label = names), size = 2.5) +
    geom_segment(data = plot_var$data, aes(x = 0, y = 0, xend = x, yend = y),
                 alpha = 0.2,
                 arrow = arrow(type = 'open', length = unit(0.25,"cm")))
}

# helper - rescale
.rescale <- function(x) {
  y <- sign(x)
  x <- abs(x)
  x <- (x - min(x))/(max(x) - min(x))
  x * y
}
