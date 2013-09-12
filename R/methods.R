# S3 dispatch methods for Curves object

##' @S3method max Curves
max.Curves = function(x, ...){
  vapply(x, function(xy){max(xy$y, ...)}, numeric(1))
}

##' @S3method min Curves
min.Curves = function(x, ...){
  vapply(x, function(xy){min(xy$y, ...)}, numeric(1))
}

##' @S3method range Curves
range.Curves = function(x, ...){
  vapply(x, function(xy){range(xy$y, ...)}, numeric(2))
}

##' Plot Curves
##' 
##' Plots a Curves object as a grid of thumbnails.  Optional arguments allow for
##' horizontal and vertical annotation lines and grid layout.
##' 
##' @param x A groan Curves object
##' @param hlines A named list specifying arguments to \code{abline()} for drawing
##'   horizontal annotation lines. The first element should be a numeric vector
##'   of length = # of curves, and is assumed to be the value of the argument 
##'   \code{h=}.
##'   Default is NULL, no lines drawn.
##' @param vlines A named list specifying arguments to \code{abline()} for drawing
##'   vertical annotation lines. The first element should be a numeric vector
##'   of length = # of curves, and is assumed to be the value of the argument 
##'   \code{v=}.
##'   Default is NULL, no lines drawn.
##' @param lyt A named list specifying arguments to \code{layout()} that define
##'   the arrangement of curve thumbnails. Default is NULL, which generates a grid
##'   as close to a square as possible, with a bias towards a landscape (wide)
##'   layout.
##' @param ... Additional arguments passed to \code{plot()} for the underlying
##'   individual curve data.
##' 
##' @details
##'   Arguments \code{hlines} and \code{vlines} are processed via \code{do.call()}.
##' 
##' @section Warnings
##'   This function uses \code{layout()} to generate a grid of thumbnails. It is 
##'   therefore incompatible with other device subplotting mechanims such as 
##'   \link[graphics]{par(mfrow)}, \link[graphics]{par(mfcol)}, and
##'   \link[graphics]{split.screen}
##' 
##' @return
##'   The grid layout matrix so that it can be reused in other plotting methods.
##' 
##' @seealso
##'   \link[graphics]{plot.default}, \link[graphics]{lines}, \link[graphics]{points},
##'   \link[graphics]{layout},
##'   \link[base]{do.call}
##' 
##' @export
plot.Curves = function(x, 
                       hlines = NULL, vlines = NULL, 
                       lyt = NULL, ...){
  #par.init = par(no.readonly=TRUE)
  par.init = par('mar')
  on.exit({suppressWarnings(par(par.init))})#; layout(1)})
  
  par(mar=c(0,0,0,0))
  
  if (is.null(lyt)) {
    # guess the grid layout
    ncol = ceiling(sqrt(length(x)))
    nrow = ceiling(length(x)/ncol)
    lyt = list(mat=matrix(1:(ncol*nrow), ncol=ncol, nrow=nrow, byrow=T))
  }
  
  do.call(layout, lyt)
  
  vargs = list(...)
  
  # plotting constants - these cannot be overwritten
  vargs[['ann']] = FALSE
  vargs[['xaxt']] = 'n'
  vargs[['yaxt']] = 'n'
  
  # plotting defaults for aesthetics if they are not explicitly set
  if (!'pch' %in% names(vargs)) vargs[['pch']] = 16
  if (!'cex' %in% names(vargs)) vargs[['cex']] = 0.5
  
  if (!'ylim' %in% names(vargs)) {
    ylim = apply(range(x), 1, median)
    
    if ('log' %in% names(vargs) && grepl('y', vargs[['log']])) ylim = log10(ylim)
    
    ylim = ylim + c(-1, 1) * diff(ylim) * 0.25
    
    if ('log' %in% names(vargs) && grepl('y', vargs[['log']])) ylim = 10^ylim
    
    vargs[['ylim']] = ylim
  }
  
  lapply(seq_along(x), function(n){
    pargs = c(list(x=x[[n]], y=NULL), vargs)
    do.call(plot, pargs)
    if (!is.null(hlines)) do.call(abline, c(h=hlines[[1]][n], hlines[-1]))
    if (!is.null(vlines)) do.call(abline, c(v=vlines[[1]][n], vlines[-1]))
    legend('bottomright', legend=names(x)[n], bty='n')
  })
  
  invisible(lyt)
}

##' @rdname plot.Curves
##' @export
lines.Curves = function(x, lyt, ...){
  #on.exit(layout(1))
  #do.call(layout, lyt)
  
  vargs = list(...)
  
  # plotting constants - these cannot be overwritten
  vargs[['ann']] = FALSE
  vargs[['xaxt']] = 'n'
  vargs[['yaxt']] = 'n'
  
  # plotting defaults for aesthetics if they are not explicitly set
  if (!'lwd' %in% names(vargs)) vargs[['lwd']] = 2
  
  lapply(seq_along(x), function(n){
    # surprisingly, despite all the warnings in the documentation about how
    # par(mfcol, mfrow, mfg) and layout() are incompatible, the following works!
    par(mfg=which(lyt$mat == n, arr.ind=TRUE)[1,])
    
    pargs = c(list(x=x[[n]], y=NULL), vargs)
    do.call(lines, pargs)
  })
  
  invisible(NULL)
}

##' @rdname plot.Curves
##' @export
points.Curves = function(x, lyt, ...){
  #on.exit(layout(1))
  #do.call(layout, lyt)
  
  vargs = list(...)
  
  # plotting constants - these cannot be overwritten
  vargs[['ann']] = FALSE
  vargs[['xaxt']] = 'n'
  vargs[['yaxt']] = 'n'
  
  # plotting defaults for aesthetics if they are not explicitly set
  if (!'pch' %in% names(vargs)) vargs[['pch']] = 16
  if (!'cex' %in% names(vargs)) vargs[['cex']] = 0.5
  
  lapply(seq_along(x), function(n){
    # surprisingly, despite all the warnings in the documentation about how
    # par(mfcol, mfrow, mfg) and layout() are incompatible, the following works!
    par(mfg=which(lyt$mat == n, arr.ind=TRUE)[1,])
    
    pargs = c(list(x=x[[n]], y=NULL), vargs)
    do.call(points, pargs)
  })
  
  invisible(NULL)
}