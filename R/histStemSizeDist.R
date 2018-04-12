#' make a histogram
#' @description histogramms of the data of an forest object
#'
#' @param data A forest object including the data 
#' @param y Vector of character strings nameing the variables which will be plotted. \code{getFNames(object)} returns an overview of available variables. You can also write short strings to plot all variables which start with these characters. E.g. "biomass_pft" plots the biomass of all pfts, whereas "biomass" plots biomass of all pfts and the total biomass.
#' @param col Color palette to be used. If NA, an adequate collorpallet is created
#' @param breaks one of:
#' (i)a vector giving the breakpoints between histogram cells,
#' (ii) a function to compute the vector of breakpoints,
#' (iii) a single number giving the number of cells for the histogram,
#' (iv)a character string naming an algorithm to compute the number of cells (see ‘Details’),
#' (v) a function to compute the number of cells.
#'In the last three cases the number is a suggestion only; the breakpoints will be set to pretty values. If breaks is a function, the x vector is supplied to it as the only argument.
#' @param usedYears numeric; the last x persantage [0-1] of the time line used for analysis
#' @param firstLetterCapital Boolean. Should first Letter of labales be capitalized
#' @param squaredBraked Boolean. squared brakets (TRUE) or round brakets (FALSE) for unit information in the labels
#' @param innerTicks Boolean. 
#' @param las numeric in {0,1,2,3}; the style of axis labels. 0: always parallel to the axis [default],1: always horizontal,2: always perpendicular to the axis, 3: always vertical.
#' @param legend. Boolean. Should the legend be plotted.
#' @param timeframe A vector of the form \code{c(startTime,endTime)}. 
#' @param timeframe A vector of the form \code{c(startTime,endTime)}. 
#' @param log string specifying if axis scales should be logarithmic
#' @param legend Boolean. Should the legend be plotted.

#' @seealso \code{\link{Forest}},
#' @export

#TODO axis beschriftung stimmt noch nicht...

histStemSizeDist<-function(data
                           ,y= "NumberTreesTotal"# vector of collumn names or numbers
                           ,col=NA
                           ,breaks=0 # 0 is default
                           ,usedYears=0.1 # the last x percent)
                           ,graphicType="plotBox"
                           ,xlim=NA
                           ,ylim=NA
                           ,innerTicks=T
                           ,file=NA
                           ,las=1
                           ,log=NA
                           ,legend=T
                           
)
{
  # checks
  if(is(data)!="Forest")stop("the given object is not of type Forest. Please read describtion of forest class in FAST. You can transform data.frames and matrices into forest-objects")
  
  if(length(grep("DiameterClass",colnames(data@data)))!=1){
    stop("Error: no diameter Classes in forest object. Plear read a dia.file")
  }
  # for same type (ba vs n)
  # select data
  variables<-vector()
  if(is.character(y[1])){
    for( i in 1:length(y)){
      variables<-c(variables,grep(y[i],colnames(data@data)))
    }
  }
  varNames<-c(colnames(data@data)[variables])
  
  daten<-data@data[,c(1:2,variables)]
  
  
  # make result data
  maxTime<-max(daten[,1])
  results<-matrix(nrow=nrow(table(daten[,2])),ncol=1+length(variables),data=unique(daten[,2]),byrow=F)
  for(i in 1:nrow(results)){
    for( j in 1:length(variables)){
      results[i,j+1]<-mean(daten[which(daten[,1]>=maxTime*(1-usedYears) & daten[,2]==results[i,1]),j+2])
    }
  }
  if(!is.na(log)){
    if(log=="y")for( i in 2:(length(variables)+1)){
      results[,i]<-replace(results[,i],which(results[,i]==0),NA)
    }
  }
  
  results<-t(results)
  
  
  # collour palette
  if(length(col)==1)if(is.na(col)){
    col<-makeColorPalette(length(varNames)
                          ,graphicType=graphicType
                          ,totalExists=F)
  }
  
  if(is.na(xlim[1])){
    max<-vector()
    for( i in 2:nrow(results)){
      max[i-1]<-max(which(results[i,]!=0))
    }
    if(results[1,1]<0){
      results<-results[,2:max(max)]
    }else{
      results<-results[,2:max(max)]
    }
    xlim=c(0,max(max)*1.1+1)
  }
  makeLayout(graphicType,innerTicks,las,file)
  
  if(!is.na(log)){
    if(log=="y")barplot(results[2:(length(variables)+1),],col=col,xlim=xlim
                        ,xlab="stem diameter class [m]"
                        ,ylab=eval(as.formula(data@unit[variables[1]]))
                        ,log="y"
    )
  }
  if(is.na(log))barplot(results[2:(length(variables)+1),],col=col,xlim=xlim
                        ,xlab="stem diameter class [m]"
                        ,ylab=eval(as.formula(data@unit[variables[1]]))
  )
  axis(1,at=c(1,(seq(floor(xlim[2]/5),floor(xlim[2]/5)*5, length.out = 5)-0.1)*1.17),results[1,c(1,seq(floor(xlim[2]/5),floor(xlim[2]/5)*5,length.out = 5))],tick=F)
  
  #legend
  if(graphicType=="plotBox")graphicType<-"plotBoxHist"
  if(legend)makeLegend(varNames,col,graphicType,length(varNames),maxY)
}
