#' create various colour palettes
#' @description This function creates various colour pallets which suit to the different layouts
#'
#' @param n Number of colours
#' @param graficType For which grafic type should the colours be optimized ("paperC1","paperC2","plotBox","ppt")
#' @param totalExists Boolean. If TRUE black is added to the pallete.
#' 
#' @export


makeColorPalette<-function(n
                           ,graphicType="plotBox"
                           ,totalExists=F
)
{
  farben<-vector()
  if(graphicType=="paperC1" | graphicType=="paperC2"){
    if(totalExists){
      if(n-1==0)farben<-"#000000"
      if(n-1<3 & n-1>=1){
        farben<-c(c("#9ECAE1", "#3182BD")[1:(ncol(plotLinesY)-1)],"#000000")
      }
      if(n-1>=3){
        farben<-c(RColorBrewer::brewer.pal(n-1,name="Blues"),"#000000")
      }
    }else{
      farben<-pft_rainbow(n)
    } 
    
  }
  
  if(graphicType=="ppt" | graphicType=="plotBox"){
    if(totalExists){
      if(n-1==0)farben<-"#000000"
      if(n-1<3 & n-1>=1){
        farben<-c(c("#377EB8","#4DAF4A")[1:(n-1)],"#000000")
      }
      if(n-1>=3){
        farben<-c(RColorBrewer::brewer.pal(n-1,name="Set1"),"#000000")
      }
    }else{
      if(n<3){
        farben<-pft_rainbow(n)
      }else{
        farben<-RColorBrewer::brewer.pal(n,name="Set1")
      }
      
    }
  }
  
  return(farben)
}

