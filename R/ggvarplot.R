#' Draws variate plots given a mixOmics plot object.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @param topn How many features to visualize the loading vectors of. Ordered by absolute corrlation with the components.
#' @import tidyverse
#' @import mixOmics
#' @importFrom ggrepel "geom_text_repel"
#' @importFrom ggthemes "scale_color_few"
#' @importFrom magrittr "%>%"
#' @export
ggvarplot <- function(model, topn = NULL) UseMethod('ggvarplot')

#' @rdname ggvarplot
#' @export
ggvarplot.pca <- function(model, topn = NULL) {
  vars <- mixOmics::plotVar(model, plot = F)
  if(!is.null(topn)) {
    order <- vars %>%
      dplyr::select(x, y) %>%
      dplyr::mutate_all(abs) %>%
      rowSums() %>%
      order(decreasing = T)
    vars <- vars %>% dplyr::slice(order[1:topn])
  }
  .varplot(vars, model$explained_variance)
}

#' @rdname ggvarplot
#' @export
ggvarplot.DA <- function(model, topn = NULL) {
  vars <- mixOmics::plotVar(model, plot = F)
  if(!is.null(topn)) {
    order <- vars %>%
      dplyr::select(x, y) %>%
      dplyr::mutate_all(abs) %>%
      rowSums() %>%
      order(decreasing = T)
    vars <- vars %>% dplyr::slice(order[1:topn])
  }
  .varplot(vars, model$explained_variance$X)
}

#' @rdname ggvarplot
#' @export
ggvarplot.sgccda <- function(model, topn = NULL) {
  vars <- mixOmics::plotVar(model, plot = F)
  vars <- split(vars, vars$Block)
  purrr::pmap(list(vars = vars, labs = head(model$explained_variance, -1)), function(vars, labs) {
    if(!is.null(topn)) {
      order <- vars %>%
        dplyr::select(x, y) %>%
        dplyr::mutate_all(abs) %>%
        rowSums() %>%
        order(decreasing = T)
      vars <- vars %>% dplyr::slice(order[1:topn])
    }
    .varplot(vars, labs)
  })
}

# helper - unit circle
.circlefun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# helper - the plot function
.varplot <- function(df, labs) {
  ggplot(df) +
    geom_vline(xintercept = 0, alpha = 0.2) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_path(data = .circlefun(diameter = 1), aes(x, y), alpha = 0.2) +
    geom_path(data = .circlefun(diameter = 2), aes(x, y), alpha = 0.2) +
    geom_text_repel(aes(x, y, label = names), size = 2.5) +
    geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
                 alpha = 0.2,
                 arrow = arrow(type = 'open', length = unit(0.25,"cm"))) +
    labs(x = sprintf('component 1\n(%2.1f%% var. explained)', labs[1] * 100),
         y = sprintf('component 2\n(%2.1f%% var. explained)', labs[2] * 100))
}
