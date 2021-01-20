
interaction.plot (h95$gender, h95$adult, h95$f0, col = 3:4, ylim = c(130,280),
                  lwd = 3, type = 'b', pch = c(16,17), cex = 1.5, main = "Data")


x1 = h95$gender
x2 = h95$adult
y = h95$f0

iplot = function (y, x1, x2,legend =...){
  
  interaction.plot (x1, x2, y,...)
  
  
  
}



iplot (h95$f0, h95$gender, h95$adult, col = 3:4, ylim = c(130,280),
                  lwd = 3, type = 'b', pch = c(16,17), cex = 1.5, main = "Data")




interaction.plot <- function (x.factor, trace.factor, response, fun = mean,
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
