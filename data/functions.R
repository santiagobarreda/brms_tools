

hypothesis2 = function (x, ...){
  output = brms::hypothesis (x, ...)
  output[[1]][,1:5]
}

  
## function to make summary plots for coefficients

brmplot = function (mat, ylim=NULL, xlim = NULL, horizontal = TRUE, add = FALSE, 
                    xs = NULL, col = 1, labels = "default",xlab='',ylab='', 
                    pch=16, lwd=2,cex=1.5, las=NA,cex.axis=1,grid=TRUE,cex.lab=1,
                    robust = FALSE, yaxs="r",xaxs="r",line=TRUE, nudge =0,...){
    
  n = nrow (mat)
  if (n > 500 & colnames(mat)[1] !="Estimate") mat = posterior_summary (mat, robust=robust)
  n = nrow (mat)
  
  if (horizontal){
    if (is.null(xs)) xs = (1:n)
    xs = xs + nudge
    if (is.null(xlim)) xlim = range (1:n)
    if (is.null(ylim)) ylim = range (mat[,3:4])
    if (labels[1]=="default") labels = rownames(mat)  
    if (is.null(labels)) labels = 1:nrow(mat)  
      
    if (!add){
      plot (0, type='n', ylim = ylim,xlim=xlim, xlab=xlab,xaxt='n',ylab = ylab,
            cex.axis=cex.axis,yaxs=yaxs,xaxs=xaxs,cex.lab=cex.lab,...)
      if (grid) grid()
      if (line) abline(h=0)
      if (line) abline(v=0)
      points (xs,mat[,1], col=col,pch=pch,cex=cex)
    }
    if (add)  points (xs, mat[,1], col=col, pch=pch,cex=cex)
    if (is.na(las))las=1
    if (labels[1]!="") axis (side=1, at = xs, labels = labels, las=las, cex.axis=cex.axis)
    
    if (length(col)==1) col = rep (col, length(xs))
    for (i in 1:n) segments (xs[i],mat[i,3],xs[i],mat[i,4],lwd=lwd, col=col[i])
  }
  if (!horizontal){
    if (is.null(xs)) xs = (n:1)
    xs = xs + nudge
    if (is.null(xlim)) xlim = range (mat[,3:4])
    if (labels[1]=="default") labels = rownames(mat)  
    if (is.null(labels)) labels = 1:nrow(mat)  
    
    if (is.null(ylim)) ylim = range (1:n)
    if (!add){
      plot (0,type='n', ylim = ylim,xlim=xlim, ylab=ylab,yaxt='n',xlab = xlab,
            cex.axis=cex.axis,yaxs=yaxs,xaxs=xaxs,cex.lab=cex.lab,...)
      if (grid) grid()
      if (line) abline(v=0)
      points (mat[,1],xs, pch=pch, col=col,cex=cex)
      
    }
    if (add) points (mat[,1],xs, pch=pch,cex=cex, col=col)
    if (is.na(las))las=2
    if (labels[1]!="") axis (side=2, at = xs, labels = labels, las=las, cex.axis=cex.axis)
    
    if (length(col)==1) col = rep (col, length(xs))
    for (i in n:1) segments (mat[i,3],xs[i],mat[i,4],xs[i],lwd=lwd, col=col[i])
  }
}


divide_factors = function (model, formula = NULL){
  
  fe = fixef (model, summary = FALSE)
  
  if (is.null(formula)) 
    formula = formula (strsplit (deparse(model$formula$formula), split=" \\+ \\(")[[1]][[1]])
  
  mod = model.matrix (formula, data = model$data)

  terms = attr(terms(formula), "term.labels")
  intercept = attr(terms(formula), "intercept")
  if (intercept) terms = c("(Intercept)",terms)
  
  num = attr (mod, "assign") + intercept

  factors = list()
  for (i in 1:max(num)){
    factors[[i]] = fe[,num==i]
    names(factors)[i] = terms[i]
  }
  return (factors)
}


add_missing = function (x){
  
  n = ncol (x)
  lab = colnames (x)[n]
  
  num = 1 + as.numeric(substr(lab,nchar(lab),nchar(lab)))
  newlab = paste0 (substr(lab,1,nchar(lab)-1), num)
  
  output = cbind (x, - rowSums (x))
  colnames (output)[n+1] = newlab
  
  return (output)
  
}


summarize = function (x){
  lapply (factors, posterior_summary)
}





plot.interaction <- function (x.factor, trace.factor, response, fun = mean,
                              type = c("l", "p", "b", "o", "c"), legend = TRUE, 
                              trace.label = deparse(substitute(trace.factor)), 
                              fixed = FALSE, xlab = deparse(substitute(x.factor)),
                              ylab = ylabel, ylim = range(cells, na.rm = TRUE), 
                              lty = nc:1, col = 1, pch = c(1L:9, 0, letters), 
                              xpd = NULL, leg.bg = par("bg"), leg.bty = "n", 
                              xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, 
                              leg.x=NULL, leg.y=NULL, xlim = NULL, ...) {
  ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
  type <- match.arg(type)
  cells <- tapply(response, list(x.factor, trace.factor), fun)
  nr <- nrow(cells)
  nc <- ncol(cells)
  xvals <- 1L:nr
  if (is.ordered(x.factor)) {
    wn <- getOption("warn")
    options(warn = -1)
    xnm <- as.numeric(levels(x.factor))
    options(warn = wn)
    if (!anyNA(xnm)) 
      xvals <- xnm
  }
  xlabs <- rownames(cells)
  ylabs <- colnames(cells)
  nch <- max(sapply(ylabs, nchar, type = "width"))
  if (is.null(xlabs)) 
    xlabs <- as.character(xvals)
  if (is.null(ylabs)) 
    ylabs <- as.character(1L:nc)
  if (is.null(xlim)) 
    xlim <- range(xvals)
  if (is.null(leg.x)) {
    leg.x <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) * 
      diff(xlim)
  }
  dev.hold()
  on.exit(dev.flush())
  matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim, 
          xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", 
          col = col, lty = lty, pch = pch)
  if (axes && xaxt != "n") {
    axisInt <- function(x, main, sub, lwd, bg, log, asp, 
                        ...) axis(1, x, ...)
    mgp. <- par("mgp")
    if (!xtick) 
      mgp.[2L] <- 0
    axisInt(1, at = xvals, labels = xlabs, tick = xtick, 
            mgp = mgp., xaxt = xaxt, ...)
  }
  if (legend) {
    yrng <- diff(ylim)
    if (is.null(leg.y))
      leg.y <- ylim[2L] - 0.1 * yrng
    if (!is.null(xpd) || {
      xpd. <- par("xpd")
      !is.na(xpd.) && !xpd. && (xpd <- TRUE)
    }) {
      op <- par(xpd = xpd)
      on.exit(par(op), add = TRUE)
    }
    # text(leg.x, ylim[2L] - 0.05 * yrng, paste("  ", 
    #                                          trace.label), adj = 0)
    if (!fixed) {
      ord <- sort.list(cells[nr, ], decreasing = TRUE)
      ylabs <- ylabs[ord]
      lty <- lty[1 + (ord - 1)%%length(lty)]
      col <- col[1 + (ord - 1)%%length(col)]
      pch <- pch[ord]
    }
    legend(leg.x, leg.y, legend = ylabs, col = col, 
           title = if (trace.label == "") NULL else trace.label,
           pch = if (type %in% c("p", "b")) 
             pch, lty = if (type %in% c("l", "b")) 
               lty, bty = leg.bty, bg = leg.bg)
  }
  invisible()
}




'%+%' = function (x, y){
  if (length(x)==1 & length(y)==1) paste (x,y,sep='')
  if (length(x)==1 & length(y)==1) paste (x,y,sep='')
  if (length(x)==1 & length(y)==1) paste (x,y,sep='')
}

ztop = function (z) 1 / (1 + exp (-z) )

ptoz = function (p){
  p[p==1] = .99  # if p=1, change to 0.99
  p[p==0] = .01  # if p=0, change to 0.01 (i.e. 1-0.99)
  log (p) - log(1-p)
}

mae = function (x) mean (abs(x))


HDI = function( x , mass = 0.95 ) {
  x = sort( x )
  points = ceiling( mass * length( x ) )
  
  steps = length( x ) - points
  width = rep( 0 , steps )
  for ( i in 1:steps ) {
    width[ i ] = x[ i + points ] - x[ i ]
  }
  HDImin = x[ which.min( width ) ]
  HDImax = x[ which.min( width ) + points ]
  HDI = as.numeric(c( HDImin , HDImax ))
  return( HDI )
}



banova = function (model, superpopulation = FALSE, collapse = TRUE){

  output = list() 
  
  if (!superpopulation){
    fixefs_finite = brms::fixef(model, summary = FALSE)  
    fixefs_finite = brms::posterior_summary (abs(fixefs_finite))
    
    if (model$family$family == "gaussian" | model$family$family == "student"){
      sigma_finite = residuals (model, summary = FALSE)
      sigma_finite = apply (sigma_finite, 1, sd, na.rm = TRUE)
      sigma_finite = brms::posterior_summary (sigma_finite)
      rownames(sigma_finite)[1] = "sigma"
      fixefs_finite = rbind(sigma_finite, fixefs_finite)
    }
    res = brms::ranef (model, summary = FALSE)
    res_summary = list()
    for (i in 1:length(res)){
      tmp = res[[i]]
      if (dim(tmp)[3]>1) tmp = apply (tmp[,,],c(1,3),sd, na.rm = TRUE)
      else tmp = apply (tmp[,,],1,sd, na.rm = TRUE)
      tmp = brms::posterior_summary (tmp)
      if (nrow (tmp)==1) rownames(tmp)[1] = "Intercept"
      res_summary[[i]] = tmp
      rownames(res_summary[[i]]) = 
        paste0 (rownames(res_summary[[i]]), ":", names(res)[i])
      
    }
    names (res_summary) = names (res)
  
    output[[1]] = fixefs_finite
    for (i in 1:length (res_summary))  output[[i+1]] = res_summary[[i]]
    names(output) = c("fixefs", names (res_summary))
  }
  if (superpopulation){
    fixefs_finite = brms::fixef(model, summary = FALSE)  
    fixefs_finite = brms::posterior_summary (abs(fixefs_finite))
    
    gaussian = (model$family$family == "gaussian" | model$family$family == "student")
    if (gaussian){
      sigma_super = brms::VarCorr(model)$residual$sd
      rownames(sigma_super)[1] = "sigma"
      fixefs_finite = rbind(sigma_super, fixefs_finite)
    }
    
    res = brms::VarCorr(model)
    res_summary = list()
    for (i in 1:(length(res)-gaussian)){
      res_summary[[i]] = res[[i]][["sd"]]
      rownames(res_summary[[i]]) = 
        paste0 (rownames(res_summary[[i]]), ":", names(res)[i])
    }
    names (res_summary) = names (res)[1:(length(res)-gaussian)]
    
    output[[1]] = fixefs_finite
    for (i in 1:length (res_summary))  output[[i+1]] = res_summary[[i]]
    names(output) = c("fixefs", names (res_summary))
  }
  if (collapse){
    label = rep (names (output), sapply (output, nrow))
    output = data.frame (do.call (rbind, output))
    output$cluster = label
    
    use = which(rownames(output)=="sigma")
    if (length(use)>0) output$cluster[use] = "sigma" 
  }
  
  return (output)
}



banovaplot = function (baov, ylim=NULL, xlim = NULL, horizontal = TRUE, 
              cols = NULL, labels = "default", xlab='',ylab='', pch=16, lwd=2,
              cex=1.5, las=NA, cex.axis=1,grid=TRUE, robust = FALSE, yaxs="r",
              xaxs="r",line=TRUE,...){

  cols = c("#EE4E62BF","#0C8275BF","#F8A61BBF","#27C0D8BF","#CA87B9BF",
           "#3A65AFBF","#822B32BF","#602D45BF","#FF6400BF","#F7B5C5BF")
  
  cols = cols[as.numeric (factor (baov$cluster))]
  
  brmplot (baov, ylim=ylim, xlim = xlim, horizontal = horizontal, col=cols, 
           labels = labels,xlab=xlab,ylab=ylab, pch=pch, lwd=lwd,cex=cex,
           las=las,cex.axis=cex.axis,grid=grid,  robust = robust, 
           yaxs=yaxs,xaxs=xaxs, line=line,...)
}


