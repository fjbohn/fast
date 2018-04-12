pft_rainbow<-function(n,alpha=NA,lum.fix=FALSE){
  if(!lum.fix)palette<-colorspace::rainbow_hcl(n,c=100,l=(1:n)/n *70+15,start=250,end=605*(n-1)/n)
  if(lum.fix)palette<-colorspace::rainbow_hcl(n,c=85,start=250,end=605*(n-1)/n)
  if(!is.na(alpha))palette<-paste(palette,substr(rgb(alpha,1,1),2,3),sep="")
  return(palette)
}
