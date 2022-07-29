#' Remove.Zeros: function to remove any rows containing a zero from a dataframe
#' 
#' This function returns the given datframe without any rows that contained one or more zeros
#' @param dataframe The dataframe containing rows with zero values
#' @keywords Zoe, Cain, Zeros
#' @return XXX
#' @export
#' @examples 
#' 
#' ################
#' # load depends #
#' ################
#' library(dplyr)
#' 
#' ###########
#' # Example #
#' ###########
#' BrainData <- read.table(file = '~/Desktop/BrainData.txt', sep = '\t', header = T)
#' BrainData[,2:19] <- log(x = (BrainData[,2:19] + 1), base = 10)
#' Remove.Zeros(BrainData)
#' 
#' 

########################################
# R function to remove rows with zeros #
########################################
Remove.Zeros <- function(dataframe){
  dataframe$Zeros <- rowSums(dataframe == 0)
  dataframe <- dataframe %>% filter(Zeros == 0)
  dataframe <- subset(dataframe, select = -c(Zeros))
  return(dataframe)
}
