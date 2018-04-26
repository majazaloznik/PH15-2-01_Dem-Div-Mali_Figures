#' FunPyramidPlotNoAxes
#'
#' @param lx vector of male values (left)
#' @param rx vector of female values (right)
#' @param labels for age groups, if not provided 1:ncats will be used (useful for psftag)
#' @param top.labels left,  and right = male,  female for example
#' @param main 
#' @param laxlab left axis labels, if not provideded, they are calculated from xlim
#' @param raxlab 
#' @param unit 
#' @param lxcol 
#' @param rxcol 
#' @param gap 
#' @param space 
#' @param ppmar 
#' @param labelcex cex for axis labels
#' @param add 
#' @param xlim both positive, e.g. c(10, 10) for left and right
#' @param ndig 
#' @param do.first 
#' @param ylab 
#'
#' @return
#' @export
#' 
#' 
FunPyramidPlotNoAxes <- function (lx, rx, labels = NA, 
                                  top.labels = c("Male", "Female"), 
                                  main = "", laxlab = NULL, raxlab = NULL, unit = "%", 
                                  lxcol, rxcol, gap = 1, space = 0.2, ppmar = c(4, 2, 4, 2), 
                                  labelcex = 1, add = FALSE, xlim, show.values = FALSE, ndig = 1, 
                                  do.first = NULL, axes=FALSE, density=NULL, angle=45, lwd = 2, border="black",
                                  ylab="", cex.main = NULL) 
{
  # check for negative values
  if (any(c(lx, rx) < 0, na.rm = TRUE)) 
    stop("Negative quantities not allowed")
  
  # get number of age groups 
  lxdim <- dim(lx)
  rxdim <- dim(rx)
  ncats <- ifelse(!is.null(lxdim), dim(lx)[1], length(lx))
  
  # if labels don't exist, make them as 1:ncats
  if (length(labels) == 1) 
    labels <- 1:ncats
  ldim <- length(dim(labels))
  
  # but if they do exist, make sure there are the right numeber
  nlabels <- ifelse(ldim, length(labels[, 1]), length(labels))
  if (nlabels != ncats) 
    stop("lx and labels must all be the same length")
  
  # get xlim (2x positive value) from lx/rx
  if (missing(xlim)) 
    xlim <- rep(ifelse(!is.null(lxdim), 
                       ceiling(max(c(rowSums(lx), 
                                     rowSums(rx)), na.rm = TRUE)),
                       ceiling(max(c(lx, rx), na.rm = TRUE))), 2)
  
  
  # labels on axes right and left
  if (!is.null(laxlab) && xlim[1] < max(laxlab)) 
    xlim[1] <- max(laxlab)
  if (!is.null(raxlab) && xlim[2] < max(raxlab)) 
    xlim[2] <- max(raxlab)
  
  oldmar <- par("mar")
  
  # start new plot
    par(mar = ppmar, cex.axis = labelcex)
    
    # plot empty plot
    plot(0, xlim = c(-(xlim[1] + gap), xlim[2] + gap), ylim = c(0,ncats + 1), 
         type = "n", axes = FALSE, xlab = "", 
         ylab = ylab, xaxs = "i", yaxs = "i", main = main, cex.main = cex.main)
    grid(nx = 10, ny = NA)
    # don't know what this is 
    # if (!is.null(do.first)) 
    #   eval(parse(text = do.first))
    
    # plot x axis, right and left
    if (is.null(laxlab)) {
      laxlab <- seq(xlim[1] - gap, 0, by = -1)
      if (axes==TRUE) axis(1, at = -xlim[1]:-gap, labels = laxlab)
    }
    else {if (axes==TRUE) axis(1, at = -(laxlab + gap), labels = laxlab)}
    if (is.null(raxlab)) {
      raxlab <- 0:(xlim[2] - gap)
      if (axes==TRUE) axis(1, at = gap:xlim[2], labels = raxlab)
    }
    else {if (axes==TRUE) axis(1, at = raxlab + gap, labels = raxlab)}
    
    

    
    # top text labels and units
    mtext(top.labels, 3, -2, at = c(-xlim[1]*2/3,  xlim[2]*2/3), 
          adj = 0.5, cex = labelcex)
    mtext(c(unit, unit), 1, 2, at = c(-xlim[1]/2, xlim[2]/2))

    # plot bars - single file
  halfwidth <- 0.5 - space/2
  if (is.null(lxdim)) {
    if (missing(lxcol)) 
      lxcol <- rainbow(ncats)
    if (missing(rxcol)) 
      rxcol <- rainbow(ncats)
    rect(-(lx + gap), 1:ncats - halfwidth, rep(-gap, ncats), 
         1:ncats + halfwidth, col = lxcol, density=density, angle=angle, lwd=lwd, border=border)
    rect(rep(gap, ncats), 1:ncats - halfwidth, (rx + gap), 
         1:ncats + halfwidth, col = rxcol, density=density, angle=angle, lwd=lwd, border=border)
    if (show.values) {
      par(xpd = TRUE)
      text(-(gap + lx), 1:ncats, round(lx, ndig), pos = 2, 
           cex = labelcex)
      text(gap + rx, 1:ncats, round(rx, ndig), pos = 4, 
           cex = labelcex)
      par(xpd = FALSE)
    }
  }
  # plot bars - multiple file
  else {
    nstack <- dim(lx)[2]
    if (missing(lxcol)) 
      lxcol <- rainbow(nstack)
    if (missing(rxcol)) 
      rxcol <- rainbow(nstack)
    lxstart <- rxstart <- rep(gap, ncats)
    for (i in 1:nstack) {
      lxcolor <- rep(lxcol[i], ncats)
      rxcolor <- rep(rxcol[i], ncats)
      densityfinal <- rep(density[i], ncats)
      rect(-(lx[, i] + lxstart), 1:ncats - halfwidth, -lxstart, 
           1:ncats + halfwidth, col = lxcolor, density=densityfinal, angle=angle, lwd=lwd, border=border)
      rect(rxstart, 1:ncats - halfwidth, rx[, i] + rxstart, 
           1:ncats + halfwidth, col = rxcolor, density=densityfinal, angle=angle, lwd=lwd, border=border)
      lxstart <- lx[, i] + lxstart
      rxstart <- rx[, i] + rxstart
    }
  }
  return(oldmar)
}

