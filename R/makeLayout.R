#' makeLayout
#' @description male a nice layout
#'
#' @param graficType Character string; "paperC1" optimizes graphic for one collumen in an a paper. "paperC1" optimizes for a graphic straging over two collumns in a paper. "ppt" optimizes for powerpoint. "plotWindow" creates a new grahic device (window). 
#' @param innerTicks boolean; see par
#' @param las numeric; see par
#' @param file character string; filename where graphic will be saved. allowed suffixes:"png", "pdf".
#' @export
makeLayout<-function(graficType
                     ,innerTicks=T
                     ,las=1
                     ,file=NA
                     )
{
  if(graficType=="paperC1"){windowWith=3.2;windowHeight=3}
  if(graficType=="paperC2"){windowWith=6.4;windowHeight=3}
  if(graficType=="ppt"){windowWith=4.9;windowHeight=3}
  if(graficType=="plotBox"){}
  
  if(innerTicks){tickLength=0.2}else{tickLength=-0.2}
  if(las==1){margins<-c(3,5,2,1)+0.1}else{margins<-c(3,3,2,1)+0.1}
  
  if(graficType!="plotBox"){
    if(is.na(file)){windows(windowWith,windowHeight)
    }else{
      suffix<-strsplit(file, "[.]")[[1]]
      suffix<-suffix[length(suffix)]
      if(suffix=="pdf")pdf(file,width=windowWith,height=windowHeight,onefile=F)
      if(suffix=="png")png(file,width=windowWith,height=windowHeight,unit="in",res=300,pointsize=1/300)
      if(suffix!= "png" | suffix != "pdf")stop("file format is not supported")
    }
  }
  
  if(graficType=="paperC2")par(mar=margins,bty="L",ps=12,mgp=c(1.8,0.3,0),tcl=tickLength, las=las)
  if(graficType=="paperC1")par(mar=margins,bty="L",ps=12,mgp=c(1.8,0.3,0),tcl=tickLength,las=las)
  if(graficType=="ppt")par(mar=margins,bty="L",ps=12,mgp=c(1.8,0.3,0),tcl=tickLength,las=las)
  if(graficType=="plotBox")par(mar=margins,bty="L",ps=12,mgp=c(1.8,0.3,0),tcl=tickLength,las=las)
  
  if(graficType=="ppt" | graficType=="paperC2" ){nf <- layout(matrix(c(1:2),1,2,byrow = TRUE), c(0.70,0.30), TRUE)
  layout.show(nf)
  }
  return(TRUE)
}