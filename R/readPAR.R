#' read  par-files
#' @description read par-files to R
#'
#' @param parFile Character string indicating the par-file, which will be read
#' @param parameter vector of parameter files or NA. default: NA ,reads all parameters
#' @param pftNames vector of pft Names which will be placed in the headers of parameters which differ between pfts.
#'
#' @export


readPAR<-function(parFile,parameter=NA,pftNames=NA){
  res<-list()
  
  par<-readLines(parFile)
  
  #if(is.na(parameter)){
  #  pattern<-names(replacement)
  # if no replacement just renaming par...
  #replace some lines if need be.
  for(i in 1:length(parameter)){
    # find parameter type
    line<-grep(parameter[i],par)[1]
    splitline<-strsplit(par[line],"\\s")[[1]]
    splitline<-splitline[which(splitline!="")]
    if(is.na(line))stop(paste("ERROR: can not found ",pattern[i],"in the file:",baseFile))
    
    if(length(splitline)==1)splitline<-strsplit(par[line],"  ")[[1]]
    # single value
    if(splitline[1]=="float"||splitline[1]=="string" || splitline[1]=="int" || splitline[1]=="int "){
      res[[i]]<-as.numeric(splitline[3])
      names(res)[i]<-parameter[i]
    }
    
    if(splitline[1]=="array"){
      dim<-par[grep("\tdimension",par[line:(line+10)])+line-1]
      dimLine<-grep("\tdimension",par[line:(line+10)],value=F)
      splitline<-strsplit(dim,"\\s")[[1]]
      splitline<-splitline[which(splitline!="")]
      dim<-splitline[(length(splitline)-1):length(splitline)]
    }
    # read vector
    if(dim[1]=="dimension"){
      array<-array(dim=c(1,as.numeric(dim[2])))
      
      param<-par[line+dimLine+1]
      param<-strsplit(param,"\\s")[[1]]
      param<-as.numeric(param[which(param!="")])
      
      if(!is.na(pftNames)){
        if(length(pftNames)==dim[2])colnames(array)<-pftNames
      }
      res[[i]]<-param
      names(res)[i]<-parameter[i]
    }
    # read array
    if(dim[1]!="dimension"){
      dim<-as.numeric(dim)
      array<-array(dim=c(dim))
      for( n in 1:dim[1]){
        param<-par[line+dimLine+n]
        param<-strsplit(param,"\\s")[[1]]
        param<-as.numeric(param[which(param!="")])
        array[n,]<-param  
      }
      if(!is.na(pftNames)){
        if(length(pftNames)==dim[2])colnames(array)<-pftNames
      }
      res[[i]]<-array
      names(res)[i]<-parameter[i]
    }
    
    
    
    
  }
  
  # write base parameter set.
  return(res)
}
