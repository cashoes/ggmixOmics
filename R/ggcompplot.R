#' Draws component plots given a mixOmics plot object.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @param comps The components to visualize.
#' @param col Optional. A vector. Labels to colour points by.
#' @import tidyverse
#' @importFrom ggthemes "scale_color_few"
#' @importFrom magrittr "%>%"
#' @export
ggcompplot <- function(model, comps = 1:2, col = NULL) UseMethod('ggcompplot')

#' @rdname ggcompplot
#' @export
ggcompplot.pca <- function(model, comps = 1:2, col = NULL) {
  df <- data.frame(model$variates$X[ , comps])
  colnames(df) <- c(paste0('comp', comps))
  if(is.null(col))
    df$class <- NA
  else
    df$class <- col
  .compplot(df, model$explained_variance)
}

#' @rdname ggcompplot
#' @export
ggcompplot.DA <- function(model, comps = 1:2) {
  df <- data.frame(model$variates$X[ , comps], class = model$Y)
  colnames(df) <- c(paste0('comp', comps), 'class')
  .compplot(df, model$explained_variance$X)
}

#' @rdname ggcompplot
#' @export
ggcompplot.sgccda <- function(model, comps = 1:2) {
  df <- model$variates %>%
    purrr::map(~ {
      df <- data.frame(.[ , comps], class = model$Y)
      colnames(df) <- c(paste0('comp', comps), 'class')
      df
    })
  df <- utils::head(df, -1)
  labs <- utils::head(model$explained_variance, -1)
  purrr::map2(df, labs, .compplot)
}

# helper - the plot function
.compplot <- function(df, labs) {
  g <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes_string(x = colnames(df)[1], y = colnames(df)[2], colour = 'class'), size = 2) +
    ggthemes::scale_color_few(name = '', na.value = 'grey') +
    ggplot2::labs(x = sprintf('%s\n(%2.1f%% var. explained)', colnames(df)[1], labs[1] * 100),
                  y = sprintf('%s\n(%2.1f%% var. explained)', colnames(df)[2], labs[2] * 100))
  if(!all(is.na(df$class)))
    return(g + ggplot2::stat_ellipse(ggplot2::aes_string(x = colnames(df)[1], y = colnames(df)[2], colour = 'class'), level = 0.68, linetype = 2, show.legend = F))
  else
    g
}
