#' modify par-files
#' @description modify and rename par-files
#'
#' @param baseFile Character string indicating the par-file, which will be modified
#' @param newfile Character string naming the new modified par-file
#' @param replacement List of new values.Name of entries indicate variable, which will be modified. If the replacing variable is a float put a single number into the list. If the replacing variable an array put an vecotr into the list in case of one dimension array, or a matrix in case of two dimesnional array.
#' @note  small numbers e.g. 0.0000001 will be transformed into 1exp-6. I don't now if FORMIND can read shuch numbers. better write small numbers as string "0.000001"
#' @export


modifyPAR<-function(baseFile,newfile,replacement=NA){
  par<-readLines(baseFile)
  if(is.list(replacement)){
    pattern<-names(replacement)
    # if no replacement just renaming par...
    #replace some lines if need be.
    for(i in 1:length(pattern)){
      line<-grep(pattern[i],par)[1]
      splitline<-strsplit(par[line],"\\s")[[1]]
      splitline<-splitline[which(splitline!="")]
      if(is.na(line))stop(paste("ERROR: can not found ",pattern[i],"in the file:",baseFile))
      
      if(length(splitline)==1)splitline<-strsplit(par[line],"  ")[[1]]
      if(splitline[1]=="float"||splitline[1]=="string" || splitline[1]=="int" || splitline[1]=="int "){
        par[line]<-paste(splitline[1],splitline[2],replacement[i],sep="\t")
      }else if(splitline[1]=="array"){
        dim<-par[grep("\tdimension",par[line:(line+10)])+line-1]
        splitline<-strsplit(dim,"\\s")[[1]]
        splitline<-splitline[which(splitline!="")]
        dim<-splitline[(length(splitline)-1):length(splitline)]  
        
        if(dim[1]=="dimension"){
          newLine<-""
          for( j in 1:length(replacement[[i]])){
            newLine<-paste(newLine,replacement[[i]][j],sep="\t")
          }
          oldLine<-grep("data",par[line:(line+10)])+line
          par[oldLine]<-newLine
          if(length(replacement[[i]])!=dim[2]){
            par[grep("\tdimension",par[line:(line+10)])+line-1]<-paste0("    \tdimension   ",length(replacement[[i]]))
          }
        }else{
          for(k in 1:nrow(replacement[[i]])){
            newLine<-""
            for( j in 1:ncol(replacement[[i]])){
              newLine<-paste(newLine,replacement[[i]][k,j],sep="\t")
            }
            oldLine<-grep("data",par[line:(line+10)])+line-1+k
            par[oldLine]<-newLine
          }
        }
      }
      
    }
    
  }
  # write base parameter set.
  writeLines(par,newfile)
  
}
