#' getForestColNames
#' @description returns the colnames of the data within the forest object
#'
#' @param forest Forest object; 
#' @export
#' 
getForestColNames<-function(forest){
  return(colnames(forest@data))
}