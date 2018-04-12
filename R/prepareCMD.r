#' prepareCMD
#' @description writes the CMDfile, which allows to run a formind with a given par-file.
#'
#' @param file Character string naming the cmd.file 
#' @param formindVersion Character string indicating the FORMIND-executable relative to the location of cmd.file, which will be used
#' @param parFile Character string indicating the par-file in the formind_parameters folder which must be at the same location as the cmd-file.
#' 
#' @export



prepareCMD<-function(file,formindVersion="formind.exe",parFile="experiment.par"){
  command<-vector()
  command[1]<-"echo starttime was %TIME% > time.txt"
  command[2]<-paste0(normalizePath(formindVersion)," ",normalizePath(parFile)," 1> stout.txt 2> sterr.txt")
  command[3]<-"echo endtime was %TIME% >> time.txt"
  writeLines(command,file)
}

