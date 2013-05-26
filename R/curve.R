# library of curve processing functions

##' Compute specific growth rate
##' 
##' Computes the specific growth rate mu for a growth curve or a set of growth
##' curves in a matrix or data.frame
##' 
##' @param curve An atomic numeric vector with the y values of a growth curve or
##'   a \code{matrix} or a \code{data.frame} where each column is the y-coordinates
##'   of a growth curve.  More details apply for \code{data.frame}.
##' @param x An optional atomic vector of x-coordinate values for the curve(s)
##'   defined by \code{y}
##' @param smooth Option to specify additional smoothing to the input curve prior
##'   to calculating mu.  Default is \code{NULL} for no smoothing.  Other options
##'   are 'loess' and 'spline'.
##' 
##' @details
##' The data.frame and matix methods attempt to locate a column called "Time" or "X" to
##' use as x-coordinate values if they are not explicitly provided.  If not found, the 
##' first column is used.
##' 
##' Explicitly defined x-coordinates for NxM data.frames or matrices can either be a
##' 1-D numeric vector of length N, or a data.frame or matrix of the same size.
##' Column names are matched if possible.
##' 
##' @export
##' @seealso \link[stats]{smooth.spline}, \link[stats]{predict.smooth.spline}, 
##'   \link[groan]{smooth.adaptive.loess}, \link[groan]{smooth.adaptive.spline}
mu = function(curve, ...) {
  # This is the generic function that gets specific growth rate profile(s) from 
  # input curves(s) defined in object curve.
  # Here we use S3 method dispatching to compute mu appropriately for the sorted
  # object types that can be passed in.
  UseMethod("mu")
}

##' @rdname mu
##' @export
mu.default = function(curve, x = NULL, smooth = NULL) {
  # the default mu method
  # applies to atomic vectors of numeric values
  
  # if timepoints are optionally provided in x, x is NULL if not
  if (is.null(x)) {
    # assume equally spaced points in curve
    x = seq_along(curve)
  }
  
  y = curve
  
  # apply smoothing if requested
  if (!is.null(smooth)) {
    y = switch(smooth, 
               loess  = smooth.adaptive.loess(list(x=x,y=y)),
               spline = smooth.adaptive.spline(list(x=x,y=y)))
    
  }
  
  # using smooth.spline allows for the use of predict to compute derivatives
  ss = smooth.spline(x, y)
  dydx = predict(ss, deriv=1)$y
  
  return(dydx/y)
}

##' @rdname mu
##' @export
mu.data.frame = function(curve, x=NULL, smooth=NULL) {
  # mu method for data.frame
  # computes mu for columns
  
  x.col = NULL
  x.col.name = 'Time'
  if (is.null(x)) {
    # locate a column with the name "[Tt]ime|[Xx]"
    x.col = which(tolower(colnames(curve)) %in% c('time', 'x'))
    if (length(x.col) >= 1) {
      x.col.name = colnames(curve)[x.col[1]]
      x = curve[,  x.col[1], drop=TRUE] # pick the first matching x.col
      Y = curve[, -x.col]               # exclude all matching x.cols from Y
    } else {
      # assume equally spaced timepoints
      x = curve[, 1]
      Y = curve[,-1]
    }
  } else {
    # x values supplied, assume all columns in curves are Y
    Y = curve
    
  }
  
  if (!is.null(dim(x))) {
    if (dim(x) != dim(Y)) {
      stop('Dimensions of `x` do not match dimensions of `curves`.')
    }
    
    MU = lapply(colnames(Y), function(n){
      y = Y[[n]]
      x = x[[n]]
      
      u = mu.default(y, x, smooth)
    })
    names(MU) = colnames(Y)
    MU = as.data.frame(MU)
    
    return(MU)
    
  } else {
    # x supplied as atomic vector
    if (length(x) != nrow(Y)) {
      stop('Number of timepoints specified not equal to number of timepoints in `curves`.')
    }
    
    # use the default method for each column
    MU = lapply(Y, mu.default, x)
    
    names(MU) = colnames(Y)
    MU = as.data.frame(MU)
    MU = data.frame(TIME=x, MU)
    colnames(MU)[1] = x.col.name
    
    return(MU)
  }
}

##' @rdname mu
##' @export
mu.matrix = function(curve, x=NULL, smooth=NULL) {
  MU = mu.data.frame(as.data.frame(curve), x, smooth)
  return(as.matrix(MU))
}