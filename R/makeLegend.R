#' intern: add a shiny legend
#' @description modify and rename par-files
#'@param labels describtion of labels
#'@param farben the collors
#'@param graficType the grafic type
#'@param varNames the names
#'@param y the y

#' @export

makeLegend<-function(labels,farben,graficType,varNames,y){
  if( graficType=="paperC2"){
    par(mar=c(3,0,2,0),fg="white",col.lab="white",col.axis="white")
    plot(-1000)
    legend("center",labels,cex=1,fill=farben,bg="#00000010",box.col="#00000010",ncol=1,text.col="black")
  }
  if( graficType=="ppt"){
    par(mar=c(3,0,2,0),fg="white",col.lab="white",col.axis="white")
    plot(-1000)
    legend("center",labels,cex=0.7,fill=farben,bg="#00000010",box.col="#00000010",ncol=1,text.col="black")
  }
  
  if(graficType=="paperC1"){
    if(length(varNames)>=7){ncol=3}else{ncol=2}
    legend(x=0,y=y,labels,cex=0.6,fill=farben,bg="#00000010",box.col="#00000010",ncol=ncol)
    
  }
  if(graficType=="plotBox"){
    if(length(varNames)>=7){ncol=3}else{ncol=2}
    legend(x=0,y=y,labels,cex=0.6,fill=farben,bg="#00000010",box.col="#00000010",ncol=ncol)
    
  }
  if(graficType=="plotBoxHist"){
    if(length(varNames)>=7){ncol=3}else{ncol=2}
    legend("topright",labels,cex=0.6,fill=farben,bg="#00000010",box.col="#00000010",ncol=ncol)
    
  }

}