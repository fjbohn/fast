palette_legend <- function(col
                           , border = "white" 
                           , numbers=T
                           , header =""
                           , line = 1
){
  n <- length(col)
  par(mar=c(5,0,5,0)+0.1)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0.5,0:(n-1)/n,0.7, 1:n/n, col = col, border = border)
  if(numbers)for( i in 1:n)text(0.3,(i/n)-1/16,i)
  mtext(header,side=3,cex=0.7,line=line) 
}