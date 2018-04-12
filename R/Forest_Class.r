#' An S4 class to represent a bank account.
#'
#' @slot balance A length-one numeric vector
setClass(Class="Forest",
         representation=representation(
           time = "numeric",
           data = "matrix",
           unit = "character",
           label = "character",
           overview = "matrix"
         ),
         validity=function(object){
           if(length(object@time)!=nrow(object@data)){
             stop ("[Forest : validation] the number of time steps does not correspond to the number of rows of the matrix")}else{}
           if(length(object@unit)!=ncol(object@data)){
             stop ("[Forest : validation] the number of units does not correspond to the number of columns of the data matrix")}else{}
           return(TRUE)
         }
)
      
setMethod("show","Forest",
          function(object){
            if(nrow(object@data)>2){
              
              print(formatC(object@overview),
                    quote=FALSE)

            }else{
              print("empty object") 
            }
          }
)

setGeneric (
   name= "getFNames",
   def=function(object){standardGeneric("getFNames")}
)
setMethod(
   f= "getFNames",
   signature= "Forest",
   definition=function(object){
       names<-colnames(object@data)
       return(names)
     }
   )

setGeneric (
  name= "setFNames",
  def=function(object,oldName,newName){standardGeneric("setFNames")}
)
setMethod(
  f= "setFNames",
  signature= "Forest",
  definition=function(object,oldName,newName){
    if(length(oldName)!=length(newName)){
      stop("length of old Names and new Names differ!")
    }else{   
      for( i in 1:length(oldName)){
        position<-which(colnames(object@data)==oldName[i])
        colnames(object@data)[position]<-newName[i]   
      }
    return(object)
    }
  }
)

Forest <- function(data,time,units,label){
   # transform data.frame to matrix
   if(is.data.frame(data)){
     names<-names(data)
     data<-as.matrix(data)
     colnames(data)<-names
   }
   # time & data
   time_collumns<-which(colnames(data)=="Time")
      
   if(length(time_collumns)>=1){
     time<-data[,time_collumns[1]]
     data<-data[,c(time_collumns[1],which(colnames(data)!="Time"))]
   }
   
   if(missing(time)){time <- 1:nrow(data)} 
   # unit
   if(missing(units)){units <- rep("unknown~symbol(\"[\")~symbol(\"?\")~symbol(\"]\") ",ncol(data))}
   #label
   if(missing(label)){
     label <- rep("unknown",ncol(data))
     for ( i in 1:length(label)){
       if(length(grep("PFT",colnames(data)[i]))==1){
         nummer<-strsplit(substr(colnames(data)[i],start=nchar(colnames(data)[i])-1,stop=nchar(colnames(data)[i])),"_")[[1]]
         label[i]<-paste0("pft",nummer[length(nummer)])
       }
       if(length(grep("Total",colnames(data)[i]))==1){
         label_i<-paste0("Total ",substr(colnames(data)[i],start=6,stop=nchar(colnames(data)[i])))
         label[i]<-transformUnits(label_i,onlyCapitals = T)
       }
     }
   }
   #overview
   overview<-matrix(nrow=10,ncol=ncol(data))
   overview[c(4:8),]<-boxplot(data,plot=F)$stats
   for( i in 1:ncol(data)){
     overview[2,i]<-min(data[,i])
     overview[1,i]<-mean(data[,i])
     overview[c(3,9),i]<-quantile(data[,i],probs=c(0.025,0.975),na.rm=T)    
     overview[10,i]<-max(data[,i])
   }
   rownames(overview)<-c("mean","min","q2.5%","whiskerLow","q25%","median","q75%","whiskerhigh","q97.5%","max")
   colnames(overview)<-colnames(data)
   
   # generate
   new(Class="Forest",
       time=time,
       data=data,
       unit=units,
       label=label,
       overview=overview
       )
}


setGeneric (
  name= "combineForests",
  def=function(object1,object2){standardGeneric("combineForests")}
)
setMethod(
  f= "combineForests",
  signature= "Forest",
  definition=function(object1,object2){
    if(ncol(object1@data)!=ncol(object2@data) | nrow(object1@data)!=nrow(object2@data))stop("size of data matrix differs between the objects")
    if(!identical(object1@unit,object2@unit))stop("unit information differs between the objects")
       if(!identical(object1@label,object2@label))stop("label information differs between the objects")
    
    data<-rbind(object1@data,object1@data)      
          
    overview<-matrix(nrow=10,ncol=ncol(data))
    overview[c(4:8),]<-boxplot(data,plot=F)$stats
    for( i in 1:ncol(data)){
      overview[2,i]<-min(data[,i])
      overview[1,i]<-mean(data[,i])
      overview[c(3,9),i]<-quantile(data[,i],probs=c(0.025,0.975))    
      overview[10,i]<-max(data[,i])
    }
    rownames(overview)<-c("mean","min","q2.5%","whiskerLow","q25%","median","q75%","whiskerhigh","q97.5%","max")
    colnames(overview)<-colnames(data)
    
    new(Class="Forest",
        time=c(object1@time,object2@time),
        data=data,
        unit=object1@unit,
        label=object1@label,
        overview=overview
    )
  }
)

