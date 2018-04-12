read.dia<-function(path,file=NA,skip=2){
  if(!is.na(file)){
    if(skip==2)dia <- read.table(paste0(path,file), header=T, skip=skip)
    if(skip>=2){
      dia <- read.table(paste0(path,file), header=F, skip=skip)
    colnames(dia)<-colnames(read.table(paste0(path,file), header=T, skip=2,nrows=2))
    }
  }
  return(dia)
}

