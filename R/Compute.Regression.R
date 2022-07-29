#' Compute.Regression: function to compute summary coefficients when conducting phylogenetic regression on trait data
#' 
#' This function returns the beta- and p-values for the regression based on PICs
#' @param X A vector of trait values used as X for linear regression
#' @param Y A vector of trait values used as Y for linear regression
#' @param Tree A phylogenetic tree used for linear regression
#' @keywords Zoe, Cain, Regression 
#' @return XXX XXX
#' @export
#' @examples
#'
#' ################
#' # load depends #
#' ################
#' library(ape)
#' 
#' ###########
#' # Example #
#' ###########
#' BrainData <- read.table(file = '~/Desktop/BrainData.txt', sep = '\t', header = T)
#' BrainData[,2:19] <- log(x = (BrainData[,2:19] + 1), base = 10)
#' TargetTree <- read.tree(file = '~/Desktop/TargetTree.tree')
#' X <- BrainData[1,11:19]
#' Y <- BrainData[1,2:10]
#' Compute.Regression(X, Y, TargetTree)
#' 
#' 

####################################
# R function to compute regression #
####################################
Compute.Regression <- function(X, Y, Tree){
  pic.X <- pic(X, phy = Tree)
  pic.Y <- pic(Y, phy = Tree)
  beta <- summary(lm(pic.Y~pic.X-1))$coefficients[1,1]
  p <- summary(lm(pic.Y~pic.X-1))$coefficients[1,4]
  return(c(beta, p))
}