#' createBasePAR
#' @description create a modifyable par file based on another parfile.
#'
#' @param parFile Character sting naming the new par-file. 
#' @param model Character sting naming the already existing par-file. 
#' @param pattern Vector of character strings naming those variables of the par file which will be modified
#' @param replacement Vector of character strings which will replace the line, where the pattern is found.
#' @export

# Todo Should be improved and applicable for arrays...

createBasePAR<-function(parFile,base="../model/formind_parameters/",pattern=NA,replacement=NA){
  par<-readLines(parFile)
  
  #replace some lines if need be.
  if(!is.na(pattern[1])){
    for(i in 1:length(pattern)){
      par[grep(pattern[i],par)][1]<-replacement[i]
    }
  }
  
  # write base parameter set.
  writeLines(par,paste0(base,"base.par"))
  
}




