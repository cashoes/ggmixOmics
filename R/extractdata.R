#' Extracts relevant data from the model object to plot.
#' @author Casey Shannon
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
