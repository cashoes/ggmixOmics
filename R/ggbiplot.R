#' Draws a biplot given a mixOmics plot object.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @import tidyverse
#' @importFrom ggthemes "scale_color_few"
#' @importFrom magrittr "%>%"
#' @export
ggbiplot <- function(model, topn = NULL) UseMethod('ggbiplot')

#' @rdname ggbiplot
#' @export
ggbiplot.pca <- function(model, col = NULL, topn = 10, ...) {
  .combine(ggcompplot(model, col), ggvarplot(model, topn))
}

#' @rdname ggbiplot
#' @export
ggbiplot.DA <- function(model, topn = 10, ...) {
  .combine(ggcompplot(model), ggvarplot(model, topn))
}

#' @rdname ggbiplot
#' @export
ggbiplot.sgccda <- function(model, topn = 10, ...) {
  purrr::map2(ggcompplot(model), ggvarplot(model, topn), .combine)
}

# helper - rescale
.rescale <- function(x) {
  y <- sign(x)
  x <- abs(x)
  x <- (x - min(x))/(max(x) - min(x))
  x * y
}

# helper - combine plots
.combine <- function(compplot, varplot) {
  # rescale components to -1 to 1
  compplot$data <- compplot$data %>% dplyr::mutate_if(is.numeric, .rescale)
  compplot +
    geom_vline(xintercept = 0, alpha = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_path(data = .circlefun(diameter = 1), aes(x, y), alpha = 0.2) +
    geom_path(data = .circlefun(diameter = 2), aes(x, y), alpha = 0.2) +
    geom_text_repel(data = varplot$data, aes(x, y, label = names), size = 2.5) +
    geom_segment(data = varplot$data, aes(x = 0, y = 0, xend = x, yend = y),
                 alpha = 0.2,
                 arrow = arrow(type = 'open', length = unit(0.25,"cm")))
}
