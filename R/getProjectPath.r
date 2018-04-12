#' getProjectPath
#' @description function only needed for FORMIND-tutorial
#' @param FolderNAme string, naming the project folder.
#' @export

getProjectPath<-function(FolderName){
  analysisFolder = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
  path<-strsplit(analysisFolder,"\\Formind_Analysis")[[1]][1]
  path<-paste0(path,"FORMIND_Model/",FolderName)
  return(path)
}