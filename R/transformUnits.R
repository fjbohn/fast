transformUnits<-function(x,onlyCapitals=F){
  einheiten<-x
  if(!onlyCapitals){
    for( i in 1:10)einheiten<-sub("\\s","~",einheiten)
    einheiten<-sub("\\[","~symbol(\"[\")~",einheiten)
    einheiten<-sub("]","~symbol(\"]\")",einheiten)
    einheiten<-sub("a1","a",einheiten)
    einheiten<-sub("yr1","yr",einheiten)
    einheiten<-sub("~a~","~yr~",einheiten)
    einheiten<-sub("T_ODM1","t[ODM]",einheiten)
    einheiten<-sub("t_ODM1","t[ODM]",einheiten)
    einheiten<-sub("number1","number",einheiten)
    einheiten<-sub("mm1","mm",einheiten)
    einheiten<-sub("m2","m^{2}",einheiten)
    einheiten<-sub("t_C1","t_C",einheiten)
    einheiten<-sub("-1","^{-1}",einheiten)
    einheiten<-sub("-2","^{-2}",einheiten)
    einheiten<-sub("m3","m^{3}",einheiten)
    
    einheiten<-sub("~per~PFT~[0-9]+~","",einheiten)
    einheiten<-sub("~per~commercial~group~[0-9]+~","",einheiten)
    einheiten<-sub("Total~","",einheiten)
  }
  einheiten<-sub("Total","total",einheiten)
  einheiten<-sub("Time","time",einheiten)
  einheiten<-sub("BasalArea","basal area",einheiten)
  einheiten<-sub("B","b",einheiten)
  einheiten<-sub("S","s",einheiten)
  einheiten<-sub("V","v",einheiten)
  einheiten<-sub("A","a",einheiten)
  einheiten<-sub("Number","number",einheiten)
  einheiten<-sub("~~","~",einheiten)
  
  return(einheiten)
}

