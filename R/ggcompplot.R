#' Draws component plots given a mixOmics plot object.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @import tidyverse
#' @importFrom ggthemes "scale_color_few"
#' @importFrom magrittr "%>%"
#' @export
ggcompplot <- function(model, ...) UseMethod('ggcompplot')

#' @rdname ggcompplot
#' @export
ggcompplot.pca <- function(model, col = NULL, ...) {
  df <- .extractor(model, ...)
  if(is.null(col))
    df$class <- NA
  else
    df$class <- col
  .mixplot(df, model$explained_variance)
}

#' @rdname ggcompplot
#' @export
ggcompplot.DA <- function(model, ...) {
  df <- .extractor(model, ...)
  .mixplot(df, model$explained_variance$X)
}

#' @rdname ggcompplot
#' @export
ggcompplot.sgccda <- function(model, ...) {
  purrr::map2(utils::head(.extractor(model, ...), -1),
              utils::head(model$explained_variance, -1),
              ~ .mixplot(.x, .y))
}

# helper - the plot function
.mixplot <- function(df, labs) {
  g <- ggplot2::ggplot(df, ggplot2::aes_string(x = colnames(df)[1], y = colnames(df)[2], colour = 'class')) +
    ggplot2::geom_point(size = 2) +
    ggthemes::scale_color_few(name = '', na.value = 'grey') +
    ggplot2::labs(x = sprintf('%s\n(%2.1f%% var. explained)', colnames(df)[1], labs[1] * 100),
                  y = sprintf('%s\n(%2.1f%% var. explained)', colnames(df)[2], labs[2] * 100))
  if(!all(is.na(df$class)))
    return(g + ggplot2::stat_ellipse(level = 0.68, linetype = 2, show.legend = F))
  else
    g
}

