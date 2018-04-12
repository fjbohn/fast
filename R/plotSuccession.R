#' plot formind simulation data over time
#' @description Process forest-class-objects to create time vs. variable plots.
#'
#' @param data A forest object including the data 
#' @param y Vector of character strings nameing the variables which will be plotted. \code{getFNames(object)} returns an overview of available variables. You can also write short strings to plot all variables which start with these characters. E.g. "biomass_pft" plots the biomass of all pfts, whereas "biomass" plots biomass of all pfts and the total biomass.
#' @param col Color palette to be used. If NA, an adequate collorpallet is created
#' @param graphicType Character string indicating layout type of the graphic. "ppt" optimizes for powerpoint slides; "paperC1","paperC2" optimizes for graphic stretching over one or two collumns within a paper; "plotbox" opens a window an optimizes for the screen.
#' @param lineShows Character sting indicate what is shown by the line: "mean" or "median"
# @param band not available at the moment
# @param file NA or character string. A character string would save the graphic with as png, pdf, or svg (depending on the given suffix)
#' @param firstLetterCapital Boolean. Should first Letter of labales be capitalized
#' @param squaredBraked Boolean. squared brakets (TRUE) or round brakets (FALSE) for unit information in the labels
#' @param innerTicks Boolean. 
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],1: always horizontal,2: always perpendicular to the axis, 3: always vertical.
#' @param legend. Boolean. Should the legend be plotted.
#' @param timeframe A vector of the form \code{c(startTime,endTime)}. 

#' @seealso \code{\link{Forest}},
#' @export

# TODO-> if pft more than 9 pft_1 plots pft_1 and pft_10...
# -> bands
#-> files svg, pdf, png...
#referenceData = NA

plotSuccession<-function(data
                         ,y
                         ,col=NA
                         ,graphicType="paperC2"
                         ,line_shows="mean"
                         ,band=NA #sd
                         ,file=NA
                         , firstLetterCapital=F
                         , squaredBraked=T
                         , innerTicks=T
                         , las=0
                         #, referenceData = NA
                         #, timeframe =c(1,nrow(daten))
                         ,legend =T
                         ,xpos = 0
                         ,ypos = 0
)
#TODO: bands, firstLetterCapital
{
  daten<-data
  
  farben<-col
  #make some daten organisations:
  if(is(daten)!="Forest")stop("the given object is not of type Forest. Please read describtion of forest class in FAST. You can transform data.frames and matrices into forest-objects")
  
  # select relevant data of forest object 
  colNumbers<-vector()
  if(is.character(y[1])){
    for( i in 1:length(y)){
      colNumbers<-c(colNumbers,grep(y[i],colnames(daten@data)))
    }
  }
  
  varNames<-c(colnames(daten@data)[colNumbers])
  
  if(length(varNames)>=9)stop("to many variables selected. max = 8 ")
  
  plotLinesY <- daten@data[,colNumbers] 
  if(length(colNumbers)==1)plotLinesY<-as.matrix(plotLinesY)
  plotDatenX <- daten@data[,1] ## Time 
  
  # is there an totalX
  TotalName<-grep("Total",varNames)  
  if(length(TotalName)==0){
    totalExists=F
    labels<-daten@label[colNumbers[1:length(colNumbers)]]
  }
  if(length(TotalName)==1){
    if(is.vector(plotLinesY)){
      plotLinesY<-as.matrix(plotLinesY); colnames(plotLinesY)<-colnames(daten@data)[colNumbers]
    }
    totalExists=T
    plotLinesY<-cbind(plotLinesY[,grep("Total",colnames(plotLinesY),invert=T)],plotLinesY[,grep("Total",colnames(plotLinesY))])
    
    labelColNumber<-which(1:length(colNumbers)!=grep("Total",varNames))
    labels<-c(daten@label[colNumbers[labelColNumber]],daten@label[colNumbers[grep("Total",varNames)]])
  }
  if(length(TotalName)>=2)stop("more than one \"total\"-collumn")
  
  # various simulations?
  numberSimulations<-length(which(plotDatenX==plotDatenX[length(plotDatenX)]))  
  
  #find some informations for plotting ####################
  if(numberSimulations>1){
    # find max y-value:
    
    max_y_werte<-vector()
    time_span<-max(daten@time)-min(daten@time)
    time_line<-min(daten@time):max(daten@time)
    time_dist<-daten@time[3]-daten@time[2]
    for( i in 3:ncol(dat)){
      max_y_werte[i]<-max(dat[,i,with=F])
    }
    if(!mult_seed_exists){  max_y_wert<-max(max_y_werte[3:length(max_y_werte)])
    linienwerte<-dat
    linienwerte<-as.matrix(linienwerte)
    }
    
    if(mult_seed_exists){
      linienwerte<-matrix(nrow=time_span,ncol=(ncol(dat)))
      #mittelwerte or median
      mdat<-as.matrix(dat)
      linienwerte[,1]<-time_line
      for( i in time_line){
        for( j in 3:ncol(mdat))
          if(line_shows=="mean")linienwerte[i+time_dist,j]<-mean(mdat[which(mdat[,1]==i),j])
          if(line_shows=="median")linienwerte[i+time_dist,j]<-median(mdat[which(mdat[,1]==i),j])
          
      }
      #
      max_lwerte<-rep(0,2)
      for( i in 3:ncol(mdat)){
        max_lwerte[i]<-max(linienwerte[,i])
      }
      
      if(band=="sd"){
        sdwerte<-matrix(nrow=time_span,ncol=(ncol(mdat)))
        for( i in time_line){
          for( j in 1:ncol(mdat))
            sdwerte[i+time_dist,j]<-sd(mdat[which(mdat[,1]==i),j])
        }
        max_sdwerte<-rep(0,2)
        for( i in 3:ncol(mdat)){
          max_sdwerte[i]<-max(sdwerte[,i])
        }
      }
      if(band=="SE"){
        SEwerte<-matrix(nrow=time_span,ncol=(ncol(mdat)))
        N<-length(which(mdat[,1]==1))
        for( i in time_line){
          for( j in 3:ncol(mdat))
            SEwerte[i+time_dist,j]<-sd(mdat[which(mdat[,1]==i),j])/N^0.5
        }
        max_SEwerte<-rep(0,2)
        for( i in 3:ncol(mdat)){
          max_SEwerte[i]<-max(SEwerte[,i])
        }
      }
      if(band=="range"){
        rmax_werte<-matrix(nrow=time_span,ncol=(ncol(mdat)))
        rmin_werte<-matrix(nrow=time_span,ncol=(ncol(mdat)))
        N<-length(which(mdat[,1]==1))
        for( i in time_line){
          for( j in 3:ncol(mdat))
            rmax_werte[i+time_dist,j]<-max(mdat[which(mdat[,1]==i),j])
          rmin_werte[i+time_dist,j]<-max(mdat[which(mdat[,1]==i),j])
        }
        max_rmax_werte<-rep(0,2)
        min_rmax_werte<-rep(0,2)
        for( i in 3:ncol(mdat)){
          max_rmax_werte[i]<-max(rmax_werte[,i])
          min_rmax_werte[i]<-min(rmax_werte[,i])
        }
      }
      if(band=="quant95"){
        qlowwerte<-matrix(nrow=time_span,ncol=(ncol(mdat)))
        qhighwerte<-matrix(nrow=time_span,ncol=(ncol(mdat)))
        
        for( i in time_line){
          for( j in 3:ncol(mdat)){
            qlowwerte[i+time_dist,j]<-quantile(mdat[which(mdat[,1]==i),j],probs=0.025)
            qhighwerte[i+time_dist,j]<-quantile(mdat[which(mdat[,1]==i),j],probs=0.975)
          }
        }
        max_95werte<-rep(0,2)
        for( i in 3:ncol(mdat)){
          max_95werte[i]<-max(qhighwerte[,i])
        }
        
      }
      
      #  if(band=="no")
    }
  }
  
  # farbpallette ##########################################
  if(length(farben)==1)if(is.na(farben)){
    farben<-makeColorPalette(ncol(plotLinesY)
                             ,graphicType=graphicType
                             ,totalExists=totalExists)
  }
  
  
  # create layout #################################
  makeLayout(graficType=graphicType
             ,innerTicks=innerTicks
             ,las=las
             ,file
  )
  # modfy lables ##############################
  xlab<-daten@unit[1]
  ylab<-daten@unit[colNumbers[1]]
  if(squaredBraked==F){
    xlab<-sub("\"\\]","\")",xlab)
    xlab<-gsub("\"\\[","\"(",xlab)
    ylab<-sub("\"\\]","\")",ylab)
    ylab<-sub("\"\\[","\"(",ylab)
  }
  if(firstLetterCapital){
    substr(ylab,1,1)<-capitalize(substr(ylab,1,1))
    substr(xlab,1,1)<-capitalize(substr(xlab,1,1))
  }
  # plot #################################################
  # find maxY
  if(numberSimulations==1){ 
    maxY<-max(plotLinesY)
  }else{
    #   if(!mult_seed_exists)max_value<-max_y_wert
    #   if(mult_seed_exists & band=="sd")max_value<-max(max_lwerte)+max(max_sdwerte)
    #   if(mult_seed_exists & band=="SE")max_value<-max(max_lwerte)+max(max_SEwerte)
    #   if(mult_seed_exists & band=="quant95")max_value<-max(max_95werte)
    #   if(mult_seed_exists & band=="range")max_value<-max(mdat[,c(2,collumn_pft_daten)])
  }
  
  if( graphicType=="paperC1" || graphicType=="plotBox" ){
    if(length(varNames)<=2)  maxY<-max(plotLinesY)*1.2
    if(length(varNames)>2 & length(varNames)<=4)  maxY<-max(plotLinesY)*1.3
    if(length(varNames)>4)maxY<-max(plotLinesY)*1.4
  }
  # plot
  
  plot(1,1 # TODO better programming please
       ,col="white"
       ,xlim=c(min(plotDatenX),max(plotDatenX))
       ,ylim=c(0,maxY)
       ,ylab=""
       ,xlab=eval(as.formula(xlab))
       ,type="n"
  )
  
  if(las==1){par(las=0);mtext( eval(as.formula(ylab)), side=2, line=3, at=maxY/2,adj=0.5,cex=1) # TODO getUnit
  }else{mtext(eval(as.formula(ylab)), side=2, line=1.5, at=maxY/2,adj=0.5,cex=1)}
  # bands
  if(numberSimulations>1){
    if(mult_seed_exists & band=="sd"){
      for( i in 3:ncol(dat))
        polygon(c(time_line,sort(time_line,decreasing=T)),c(linienwerte[,i]+sdwerte[,i],linienwerte[nrow(sdwerte):1,i]-sdwerte[nrow(sdwerte):1,i]),col=paste0(farben[i-2],"50"),border=NA)
    }
    if(mult_seed_exists & band=="quant95"){
      for( i in 3:ncol(dat))
        polygon(c(time_line,sort(time_line,decreasing=T)),c(qlowwerte[,i],qhighwerte[nrow(qhighwerte):1,i]),col=paste0(farben[i-2],"50"),border=NA)
    }
    
  }
  # lines
  for( i in 1:(ncol(plotLinesY))){
    points(plotDatenX,plotLinesY[,i],type="l",col=farben[i],lwd=2)
  }
  
  #legend
  if(legend)makeLegend(labels,farben,graphicType,colNumbers,maxY)
  
}

