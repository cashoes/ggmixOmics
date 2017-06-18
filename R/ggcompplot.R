#' Draws component and variate plots given a mixOmics plot object, optionally combining them.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @param topn How many features to visualize the loading vectors of. Ordered by absolute corrlation with the components.
#' @import tidyverse
#' @import cowplot
#' @import ggthemes
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#'
#' model <- mixOmics::splsda(X, Y, keepX = c(10, 10))
#' ggcompplot(model)
ggcompplot <- function(model, topn = NULL, combined = F) UseMethod('ggcompplot')

ggcompplot.pca <- function(model, topn = NULL, combined = F) {
  data <- extract_data(model)
  data %>% purrr::map(~ mixplot(.[[1]], .[[2]]))
}

ggcompplot.spca <- function(model, topn = NULL, combined = F) {
  data <- extract_data(model)
  data %>% purrr::map(~ mixplot(.[[1]], .[[2]]))
}

ggcompplot.block.splsda <- function(model, topn = NULL, combined = F) {
  data <- extract_data(model)
  data %>% purrr::map(~ mixplot(.[[1]], .[[2]]))
}

mixplot <- function(data, labs) {
  g <- ggplot2::ggplot(data, aes(`comp 1`, `comp 2`, colour = class)) +
    ggplot2::geom_point(size = 3) +
    ggthemes::scale_color_few(name = '', labels = paste('DOL', c(0, 1, 3, 7)), na.value = 'grey') +
    ggplot2::labs(x = sprintf('component 1\n(%2.1f%% var. explained)', labs[1] * 100),
                  y = sprintf('component 2\n(%2.1f%% var. explained)', labs[2] * 100))
  if(!all(is.na(data$class)))
    return(g + ggplot2::stat_ellipse(level = 0.68, linetype = 1, show.legend = F))
  else
    g
}

