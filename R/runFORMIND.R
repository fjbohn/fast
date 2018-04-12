#' runFORMIND
#' @description runs FORMIND using a CMDfile and check run for errors.
#'
#' @param model Character string naming the modelfile  
#' @param parFile Character string naming the PARfile  
#' @param parStandardPath bool; if T parfile is in folder formind_parameters
#' @seealso \code{\link{prepareCMD}}
#' 
#' @export


runFORMIND<-function(model="formind.exe",parFile="experiment.par",parStandardPath=T)
{
  cmdfile<-"formind.cmd"
  if(parStandardPath)parFile<-paste0("formind_parameters/",parFile)
  prepareCMD(cmdfile,model,parFile)
  
  system(cmdfile)
  
  sterr<-readLines("sterr.txt")
  if(length(grep("ERROR",sterr)) > 0){
    cat(sterr[grep("ERROR",sterr)])
    stop("ERROR in FORMIND model run")
  }else{
    cat("simulation succesfull")
  }
  
  file.remove(cmdfile)
}

