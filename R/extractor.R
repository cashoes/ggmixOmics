#' Extracts relevant data from the model object to plot.
#' @author Casey Shannon
#'
#' @param model A mixOmics model object.
#' @import tidyverse
#' @import mixOmics
#' @importFrom magrittr "%>%"
extractor <- function(model, comps = 1:2) UseMethod("extractor")

# unsupervised methods
extractor.pca <- function(model, comps = 1:2, class = NA) {
  # check that model is prcomp
  stopifnot(any(class(model) %in% c('pca', 'spca')))
  # extract values for specified components and return
  df <- data.frame(model$variates$X[ , comps], class)
  colnames(df) <- c(paste('comp', comps), 'class')
  df
}

# supervised methods
extractor.DA <- function(model, comps = 1:2) {
  # check that model is supervised
  stopifnot(any(class(model) %in% c('plsda', 'splsda', 'block.splsda')))
  # extract values for specified components and return
  df <- data.frame(model$variates$X[ , comps], class = model$Y)
  colnames(df) <- c(paste('comp', comps), 'class')
  df
}

# multiblock supervised methods
extractor.sgccda <- function(model, comps = 1:2) {
  model$variates %>%
    map(~ {
      df <- data.frame(.[ , comps], class = model$Y)
      colnames(df) <- c(paste('comp', comps), 'class')
      df
    })
}
