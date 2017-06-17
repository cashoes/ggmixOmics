#' Draws component and variate plots given a mixOmics plot object, optionally combining them.
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
ggcompplot <- function(model, topn = NULL, combined = F) {
  data <- extract_data(model)
  p <- data %>%
    purrr::map(~ mixplot(.[[1]], .[[2]]))
  p
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

#' Extracts relevant data from the model object to plot.
#'
#' @param model A mixOmics model object.
#' @import tidyverse
#' @import mixOmics
#' @importFrom magrittr "%>%"
extract_data <- function(model) UseMethod("extract_data")

# unsupervised
# non-sparse methods
extract_data.pca <- function(model, comps = 1:2, class = NA) {
  vars <- data.frame(model$variates$X[ , comps], class)
  colnames(vars) <- c(paste('comp', comps), 'class')
  labs <- model$explained_variance
  list(list(vars = vars, labs = labs, vecs = NULL))
}

# sparse methods
extract_data.spca <- function(model, comps = 1:2, class = NA) {
  vars <- data.frame(model$variates$X[ , comps], class)
  colnames(vars) <- c(paste('comp', comps), 'class')
  labs <- model$explained_variance
  vecs <- mixOmics::plotVar(model, plot = F)
  list(list(vars = vars, labs = labs, vecs = vecs))
}

# supervised
# non-sparse methods
# extract_data.plsda <- function(model, comps = 1:2, class = NA) {}

# sparse methods
# extract_data.splsda <- function(model, comps = 1:2, class = NA) {}

# multiblock methods
extract_data.block.splsda <- function(model, comps = 1:2) {
  purrr:::pmap(.l = list(vars = head(model$variates, -1),
                 labs = head(model$explained_variance, -1),
                 vecs = mixOmics::plotVar(model, plot = F) %>% split(.$Block)),
       function(vars, labs, vecs) {
         vars <- data.frame(vars[ , comps], model$Y)
         colnames(vars) <- c(paste('comp', comps), 'class')
         list(vars = vars, labs = labs, vecs = vecs)
       })
}
