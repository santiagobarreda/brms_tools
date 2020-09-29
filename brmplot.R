
## function to make summary plots for coefficients

brmplot = function (mat, ylim=NULL, xlim = NULL, horizontal = TRUE, add = FALSE, xs = NULL, col = 1, 
                    labels = "",xlab='',ylab='', pch=16, lwd=2,cex=1.5, ...){
  
  n = nrow (mat)
  if (horizontal){
    if (is.null(xs)) xs = 1:n
    if (is.null(xlim)) xlim = range (1:n)
    if (is.null(ylim)) ylim = range (mat[,3:4])
    if (is.null(labels)) labels = rownames(mat)  
    if (!add) plot (xs,mat[,1], col=col,pch=pch,cex=cex, ylim = ylim,xlim=xlim, xlab=xlab,xaxt='n',ylab = ylab,...)
    if (add) points (xs, mat[,1], col=col, pch=pch,cex=cex)
    if (labels[1] !='') axis (side=1, at = 1:n, labels = labels, las=2)
    
    if (length(col)==1) col = rep (col, length(xs))
    for (i in 1:n) segments (xs[i],mat[i,3],xs[i],mat[i,4],lwd=lwd, col=col[i])
  }
  if (!horizontal){
    if (is.null(xs)) xs = n:1
    if (is.null(xlim)) xlim = range (mat[,3:4])
    if (is.null(labels)) labels = rownames(mat)  
    if (is.null(ylim)) ylim = range (1:n)
    if (!add) plot (mat[,1],xs, pch=pch, col=col,cex=cex, ylim = ylim,xlim=xlim, ylab=ylab,yaxt='n',xlab = ylab,...)
    if (add) points (mat[,1],xs, pch=pch,cex=cex, col=col)
    if (labels[1] !='') axis (side=2, at = n:1, labels = labels, las=1)
    
    if (length(col)==1) col = rep (col, length(xs))
    for (i in n:1) segments (mat[i,3],xs[i],mat[i,4],xs[i],lwd=lwd, col=col[i])
  }
}

