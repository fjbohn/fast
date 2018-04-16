#' fast
#' @description conducting a simulation and analyse it in one go
#'
#' @param model Character sting naming the model-file.
#' @param param Character sting naming the par-file. 
#' 
#' @param runtime Number of years to simulate.
#' @param repetitions Number of repetitions of identical model runs, which differ only in the random seed.
#' @param newSimulation Boolean; Default TRUE; if FALSE no simulation is processed but data are analysed if available.
#' 
#' @param suc.plot bool; plot succession
#' @param circle.plot bool; make circle.plot
#' @param hist.plot bool; make histogramm
#' 
#' @param graficType Character string; "paperC1" optimizes graphic for one collumen in an a paper. "paperC1" optimizes for a graphic straging over two collumns in a paper. "ppt" optimizes for powerpoint. "plotWindow" creates a new grahic device (window). 
#' 
#' @export

fast<-function(model,param,runtime=500,repetitions=1,suc.plot=T,circle.plot=F,hist.plot=F,graficType="paperC2",newSimulation=T)
{ 
  ##**********************************************  
  ## extract paths 
  ##**********************************************

  parPath<-normalizePath(parameterFile)
  ncharPar<-nchar(rev(strsplit(parameterFile,"/")[[1]])[1])
  parPath<-substr(parPath,start=1,stop=(nchar(parPath)-ncharPar))
  ncharPar<-sum(nchar(rev(strsplit(parameterFile,"/")[[1]])[2]))+1
  projectPath<-substr(parPath,start=1,stop=(nchar(parPath)-ncharPar))
  
  ##**********************************************  
  ## make experiment 
  ##**********************************************  
  
  for ( r in 1:repetitions){
    if(newSimulation){
    # create a experiment par
    modifyPAR(parameterFile,paste0(parPath,"exp",r,".par")
              , list(RandomInit = r)
    )
    runFORMIND(model,paste0(parPath,"exp",r,".par"),parStandardPath = F)
    }
  }
  ##********************************************************
  ## read result
  ##********************************************************
  
  simData<-read.formindData(paste0("exp",r)
                            ,path = paste0(projectPath,"/results/")
                            ,fileTypes=c("ba","bt","n") 
  )  
  
  plotSuccession(simData,y=c("TotalBiomass"
                             ,"BiomassPerPFT"
                            )
                 ,graphicType="paperC2"
  ) # plot grafix in the plot box within the RStudio GUI
  
  plotSuccession(simData,y=c("TotalBasalArea"
                             ,"BasalAreaPerPFT"
  )
  ,graphicType="paperC2"
  )
  
  plotSuccession(simData,y=c("TotalNumber"
                             ,"NumberPerPFT"
  )
  ,graphicType="paperC2"
  )

  return(simData)
}
