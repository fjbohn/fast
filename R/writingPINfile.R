#' writingPINfile
#' @description writes a pin file
#'
#' @param data a matrix: plotnumber,species (as number),which dbhclass as index
#' @param dbhclasses vector containing the dbh classes
#' @param filename string of the new pin-file
#' @param seedData string of the seed in every plot
#' @param no_pft number of pfts.
#' @export

writePINfile<-function(data,dbhclasses,filename,seedData=NA,no_pft=NA){
  # data= plot,species (as number),which dbhclass as index,number of trees)
  if(is.na(no_pft))no_pft<-length(table(data[,2]))
  
  dbh_line<-vector()
  for ( ii in 1:length(dbhclasses))dbh_line<-paste(dbh_line,dbhclasses[ii]," ",sep="")
  
  if(is.na(seedData)){
    seed_line<-" seeds = "
    for ( ii in 1:no_pft)seed_line<-paste(seed_line,0,sep=" ")
  }
  #head
  PIN<-vector()
  PIN[1]<-"file pinfile"
  PIN[2]<-"regionheader = \"this file is created with library FAST writingPINfile.\" "
  PIN[3]<-"dclass       ="
  PIN[4]<-dbh_line
  #body
  plots<-array(dim=c(no_pft,length(dbhclasses),25),data=0)
  for(i in 1:nrow(data)){
    plots[data[i,2],data[i,3],data[i,1]]<-data[i,4]+plots[data[i,2],data[i,3],data[i,1]]
  }
  
  for (i in 1:25){
    PIN[length(PIN)+1]<-"block plot"
    PIN[length(PIN)+1]<-paste(" name     = \" plot ",i," \" ",sep="")
    PIN[length(PIN)+1]<-paste(" position = ",20*((i-1)%%5)," ",20*floor((i-1)/5)," ", 20*((i-1)%%5)+20," ",20*floor((i-1)/5)+20,sep="")
    PIN[length(PIN)+1]<-" code     = 1"
    PIN[length(PIN)+1]<-" mel      = 0"
    PIN[length(PIN)+1]<-" n0       ="
    for ( j in 1:no_pft){
      tree_line<-vector()
      for(ii in 1:length(dbhclasses))tree_line<-paste(tree_line,plots[j,ii,i]," ",sep="") 
      PIN[length(PIN)+1]<-paste(tree_line,",",sep="")
    }
    PIN[length(PIN)+1]<-seed_line
  }  
  
  
  
  if(!is.na(filename)) writeLines(PIN,filename)
  
  if(is.na(filename))  return (PIN)
}
