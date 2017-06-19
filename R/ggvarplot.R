#' Draws variate plots given a mixOmics plot object.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @param topn How many features to visualize the loading vectors of. Ordered by absolute corrlation with the components.
#' @import tidyverse
#' @importFrom ggthemes "scale_color_few"
#' @importFrom magrittr "%>%"
#' @export
ggvarplot <- function(model, topn = NULL) UseMethod('ggvarplot')

#' @rdname ggvarplot
#' @export
ggvarplot.pca <- function(model, topn = NULL) {
  mixOmics::plotVar(model, plot = F)
}

#' @rdname ggvarplot
#' @export
ggvarplot.DA <- function(model, topn = NULL) {
  mixOmics::plotVar(model, plot = F)
}

#' @rdname ggvarplot
#' @export
ggvarplot.sgccda <- function(model, topn = NULL) {
  mixOmics::plotVar(model, plot = F)
}
