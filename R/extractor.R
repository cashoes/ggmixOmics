#' Extracts relevant data from the model object to plot.
#' @author Casey Shannon
#' @param model A mixOmics model object.
#' @import tidyverse
#' @import mixOmics
#' @importFrom magrittr "%>%"
#' @export
.extractor <- function(model, ...) UseMethod('.extractor')

#' @export
.extractor.pca <- function(model, comps = 1:2) {
  df <- data.frame(model$variates$X[ , comps])
  colnames(df) <- c(paste0('comp', comps))
  df
}

#' @export
.extractor.DA <- function(model, comps = 1:2) {
  df <- data.frame(model$variates$X[ , comps], class = model$Y)
  colnames(df) <- c(paste0('comp', comps), 'class')
  df
}

#' @export
.extractor.sgccda <- function(model, comps = 1:2) {
  model$variates %>%
    purrr::map(~ {
      df <- data.frame(.[ , comps], class = model$Y)
      colnames(df) <- c(paste0('comp', comps), 'class')
      df
    })
}
