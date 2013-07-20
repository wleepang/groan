##' Initialize a groan Curves object
##' 
##' Initializes a groan Curves object from a matrix or a data.frame.
##' 
##' @param x input object either a matrix or data.frame.
##' 
##' @details
##'   The first column of the \code{x} is taken to be x-values for all curves in
##'   the data.
##'   
##' @return A groan Curves object (S3) which is simply a list of xy.coords objects.
##'   Names of the list are the same as column names in the input \code{x}
##' 
##' @seealso \link[grDevices]{xy.coords}
##' 
##' @export
groan.init = function(x) {
  if ((!is.matrix(x) && !is.data.frame(x)) || dim(x)[2] < 2) {
    stop('Argument `x` must be either a matrix or data.frame with 2 or more columns')
  }
  
  if (is.matrix(x)) x = as.data.frame(x)
  
  xvals = x[,1]
  Curves = lapply(x[,-1], function(y){xy.coords(x=xvals, y=y, xlab=colnames(x)[1])})
  class(Curves) = 'Curves'
  
  return(Curves)
}

##' Apply smoothing to a groan Curves object
##' 
##' @param x A groan Curves object
##' @param method Underlying smoothing algorithm to apply.
##' @param adaptive Defines if an adaptive approach should be applied. See details.
##' @param FUN.IN List of functions to apply to x,y-coordinates prior to smoothing.
##'   Default is \code{list(x=identity, y=identity)}.
##' @param FUN.OUT List of function to apply to x,y-coordinates post smoothing.
##'   Typically, this is the inverse of the function applied in \code{FUN.IN}.
##'   Default is \code{list(x=identity, y=identity)}.
##' @param ... further arguments passed to underlying function calls.
##' 
##' @details
##'   Adaptive smoothing uses different approaches depending on the method chosen:
##'   \describe{
##'     \item{spline}{
##'       \code{smooth.spline()} is run for several iterations over a random subset of the
##'       input data (aka jack-knifing).  The final smoothed curve is computed as
##'       the average of all iterations (aka bootstrapped aggregation - or bagging).  
##'     }
##'     \item{loess, lowess}{
##'       The span interval for \code{loess} and \code{lowess}, the window over
##'       which locally weighted polynomial fits are performed, is optimized using
##'       \code{optim} to minimize the sum of squared residuals between the
##'       smoothed curve and data.
##'     }
##'   }
##'   
##'   Note, non-adaptive \code{loess} is wrapped so that its call is compatible
##'   with other functions in the set - e.g. it uses an \code{xy.coords} object
##'   as its first argument.
##' 
##' 
##' @return A groan Curves object of same size as the input where all y-values
##'   are replaced with smoothed values.  Details of the smoothing process are
##'   appended as attributes.
##' 
##' @seealso
##'   Core smoothing methods:
##'     \link[stats]{smooth.spline}, 
##'     \link[stats]{loess},
##'     \link[stats]{lowess};
##'   Adaptive variations of the former:
##'     \link[groan]{smooth.adaptive.spline}, 
##'     \link[groan]{smooth.adaptive.loess},
##'     \link[groan]{smooth.adaptive.lowess}
##'   Also of interest for adaptive loess and lowess:
##'     \link[stats]{optim}
##' 
##' @export
groan.smooth = function(x, method=c('spline', 'loess', 'lowess'), adaptive=FALSE, FUN.IN=list(x=identity, y=identity), FUN.OUT=list(x=identity, y=identity), ...) {
  if (class(x) != 'Curves') {
    stop('Required argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  fcall  = match.call()
  method = match.arg(method)
  
  if (adaptive) {
    # apply adaptive smoothing methods
    smooth.fun = switch(method, 
                        spline = smooth.adaptive.spline, 
                        loess  = smooth.adaptive.loess, 
                        lowess = smooth.adaptive.lowess)
  } else {
    # apply default smoothing
    smooth.fun = switch(method, 
                        spline = smooth.spline, 
                        loess  = loess.xy.coords, 
                        lowess = lowess)
  }
  
  x.s = lapply(names(x), function(n, ...) {
    xy.in = list(x=FUN.IN$x(x[[n]]$x), y=FUN.IN$y(x[[n]]$y))
    
    s = smooth.fun(xy.in, ...)
    
    x[[n]]$x = FUN.OUT$x(s$x)
    x[[n]]$y = FUN.OUT$y(s$y)
    
    x[[n]][['smooth']] = list(call=fcall, result=s)
    return(x[[n]])
  }, ...)
  
  names(x.s) = names(x)
  class(x.s) = class(x)
  return(x.s)
}

##' Specific growth rate (mu)
##' 
##' Calculates the specific growth rate profile for a growth curve
##' 
##' @param x A groan Curves object representing growth curves
##' 
##' @return A groan Curves object representing specific growth rate profiles
##' 
##' @export
groan.mu = function(x) {
  if (!is(x, 'Curves')) {
    stop('Input `x` must be a Curves object.  See ?groan.init')
  }
  
  u = lapply(x, function(xy){
    dydx = splinefun(xy)(xy$x, deriv=1)
    xu = xy
    xu$y = dydx/xy$y
    
    return(xu)
  })
  
  class(u) = class(x)
  return(u)
}

##' Fit model
##' 
##' @param x A groan Curves object
##' @param model Model to fit to curves either:
##'   \describe{
##'     \item{pulse}{logistic pulse, typical for specific rate curves.}
##'     \item{logi}{logisic sigmoid, typical for growth curves.}
##'   }
##' 
##' @return A groan Curves object were y-values are replaced with fitted model
##'   predictions.  Also, model fit details are appended as additional properties.
##' 
##' @seealso \link[groan]{groan.pulsefit} \link[groan]{groan.logifit}
##' @export
groan.fit = function(x, method=c('pulse', 'logi')) {
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  method = match.arg(method)
  fit.fun = switch(method, 
                   pulse = groan.pulsefit, 
                   logi  = groan.logifit)
  
  return(fit.fun(x))
}

##' Pulse model fitting
##' 
##' Fits curves (e.g. specific rate profiles) to a pulse derived from a mixture
##' of logistic sigmoids
##' 
##' @param x A groan Curves object
##' 
##' @return A groan Curves object where y-coordinates are the fitted pulse profile.
##'   In addition, results of the fit, e.g. pulse parameters and goodness-of-fit
##'   statistics.
##' 
##' @seealso \link[groan]{groan.logifit}, \link[stats]{nlminb}
##' 
##' @export
groan.pulsefit = function(x) {
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  opt.fun = function(p, xy) {
    res = xy$y - pulse(xy$x, p=p)
    sse = res %*% res
    return(sse[1])
  }
  x.f = lapply(x, function(xy){
    p0 = c(d=0, w=diff(range(xy$x))/2, h=max(xy$y), kr=1, kf=1)
    fit = nlminb(p0, objective=opt.fun, gradient=NULL, hessian=NULL, xy, lower=0)
    y.f = pulse(xy$x, p=fit$par)
    
    gof = ks.test(xy$y, y.f)
    
    xy$y = y.f
    xy[['pulsefit']] = fit
    xy[['pulsefit.gof']] = gof
    
    return(xy)
  })
  
  return(x.f)
}

##' Logistic sigmoid model fitting
##' 
##' Fits curves (e.g. growth curves) to a logisitic sigmoid.
##' 
##' @param x A groan Curves object
##' 
##' @return A groan Curves object where y-coordinates are the fitted logistic
##'   sigmoid profile.  In addition, results of the fit, e.g. sigmoid parameters
##'   and goodness-of-fit statistics.
##' 
##' @seealso \link[groan]{groan.pulsefit}, \link[stats]{nlminb}
##' 
##' @export
groan.logifit = function(x) {
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  opt.fun = function(p, xy) {
    res = xy$y - logip(xy$x, p)
    sse = res %*% res
    return(sse[1])
  }
  x.f = lapply(x, function(xy){
    p0 = c(d=0, h=max(xy$y), k=1, y0=min(xy$y))
    fit = nlminb(p0, opt.fun, gradient=NULL, hessian=NULL, xy)
    y.f = logip(xy$x, fit$par)
    
    gof = ks.test(xy$y, y.f)
    
    xy$y = y.f
    xy[['logifit']] = fit
    xy[['logifit.gof']] = gof
    
    return(xy)
  })
  
  return(x.f)
}

##' Logistic sigmoid
##' 
##' \deqn{y = \frac{1}{1 + exp(-k*x)}}
##' 
##' @param x x-coordinates
##' @param k exponential rate
##' 
##' @return The logistic sigmoid function with rate \code{k} evaluated at \code{x}
logi = function(x, k=1){1/(1 + exp(-k*x))}

##' Logistic sigmoid (parameterized)
##' 
##' \deqn{y = y0 + (h-y0)*\frac{1}{1 + exp(-k*(x-d))}}
##' 
##' @param x x-coordinates
##' @param p named vector of curve parameters:
##'   \describe{
##'     \item{y0}{y-offset (minimum asymptote)}
##'     \item{d}{delay}
##'     \item{h}{maximum asymptote}
##'     \item{k}{climb rate}
##'   }
##' 
##' @seealso \link[groan]{logi}
##' 
##' @return The logistic function specified by \code{p} evaluated at \code{x}
logip = function(x, p = c(d=0, h=1, k=1, y0=0)) {
  d = p['d']; h = p['h']; k = p['k']; y0 = p['y0']
  y = y0 + (h-y0)*logi(x-d, k)
}

##' Logistic pulse
##' 
##' @param x Vector of x-coordinates
##' @param d Delay (x value for pulse rise)
##' @param w Pulse width. The value of \code{d+w} is the (x value for pulse fall)
##' @param h Pulse height
##' @param kr Rate of rise
##' @param Kf Rate of fall
##' @param p Optional. Vector of parameters where element names match argument 
##'   names described above.  If provided, overides individual argument specifications.
##' 
##' @details Produces a pulse based on the superposition of two logistic sigmoids.
##'   \deqn{y = h*(logi(x-d, kr) - logi(x-(d+w), kf))}
##' 
##' @return A vector of y values for the pulse.
##' 
##' @seealso \link[groan]{logi}
pulse = function(x, d=0, w=diff(range(x))/2, h=1, kr=1, kf=1, p=NULL){
  if (!is.null(p)) {
    d = p['d']; w = p['w']; h = p['h']; kr = p['kr']; kf = p['kf']
  }
  y = (logi(x-d, kr) - logi(x-(w + d),kf))*h
  return(y)
}

##' Calculate number of generations
##' 
##' Calculates the number of generations in a growth curve by computing the
##' area under the specific rate profile
##' 
##' @param x A groan Curves object representing specific growth rates
##' 
##' @return An atomic numeric vector
##' 
##' @seealso \link[stats]{splinefun}, \link[stats]{integrate}
##' 
##' @export
groan.generations = function(x) {
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  G = vapply(x, function(u){
    f = splinefun(u)
    g = integrate(f, min(u$x), max(u$x))$value
    return(g)
  }, numeric(1))
  
  return(G)
}

##' Maximum specific growth rate
##' 
##' @param x A groan Curves object of specific growth rate profiles
##' @param na.rm Boolean, specifies if NA values are removed when finding max rates.
##' 
##' @details Simply a wrapper for \code{vapply}-ing \code{max} over the Curves object. 
##' 
##' @return A numeric vector of maximum specific growth rates
##' @export
groan.mumax = function(x, na.rm=T) {
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  mumax = vapply(x, function(xy){max(xy$y, na.rm=na.rm)}, numeric(1))
  return(mumax)
}

##' Estimate lag time
##' 
##' @param x A groan Curves object of specific growth rate profiles
##' @param pct.thresh Percent threshold relative to the curve maximum for detection.  Default is 5\% (0.05).
##' 
##' @details
##'   Scans the curve going forward in time.  The point at which the curve
##'   crosses the defined threshold is returned.
##' 
##' @return A numeric vector of lag times
##' @seealso \link[groan]{groan.tsat}
##' @export
groan.tlag = function(x, pct.thresh = 0.05){
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  tlag = vapply(x, function(xy){
    pct = xy$y/max(xy$y, na.rm=T)
    return(xy$x[which(pct > pct.thresh)[1]])
  }, numeric(1))
  
  return(tlag)
}

##' Estimate saturation time
##' 
##' @param x A groan Curves object of specific growth rate profiles
##' @param pct.thresh Percent threshold relative to the curve maximum for detection.  Default is 5\% (0.05).
##' 
##' @details
##'   Scans the curve going backward in time.  The point at which the curve
##'   crosses the defined threshold is returned.
##' 
##' @return A numeric vector of lag times
##' @seealso \link[groan]{groan.tlag}
##' @export
groan.tsat = function(x, pct.thresh){
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  tsat = vapply(x, function(xy){
    pct = xy$y/max(xy$y, na.rm=T)
    return(xy$x[rev(which(pct > pct.thresh))[1]])
  }, numeric(1))
  
  return(tsat)
}

##' Estimate carrying capacity
##' 
##' @param x A groan Curves object of growth curves
##' 
##' @details \code{x} is fitted to a logistic growth curve (if it has not been already).
##'   The value of the fitted curve at \code{x-coord = Inf} is returned.
##' 
##' @return A numeric vector of capacity values
##' 
##' @export
groan.capacity = function(x){
  if (!is(x, 'Curves')) {
    stop('Argument `x` must be a groan Curves object. See ?groan.init')
  }
  
  if (!'logifit' %in% names(x[[1]])) {
    x = groan.logifit(x)
  }
  
  cap = vapply(x, function(xy){
    logip(Inf, p=xy$logifit$par)
  })
  
  return(cap)
}
