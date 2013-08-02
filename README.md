groan
=====

`groan` is an R package for **GRO**wth curve **AN**alysis.  Growth curves are
typically time series measurements of optical density generated by microtiter
plate readers such as a Bioscreen-C or TECAN Sunrise.  `groan` is used to
extract key parameters of growth:

 * (maximum) specific growth rate
 * lag time
 * culture saturation level
 * saturation time

`groan` also provides a number of smoothing and model fitting options to make
the extraction of the above parameters more robust.

Installation
------------
`groan` can be installed directly from `github` using the `devtools` R package:

```R
require(devtools)
install_github('groan', 'wleepang')
```

Usage
-----

Once installed, load `groan` via:

```R
library(groan)
```

Input data to `groan` is simple delimited text (either comma or tab) - e.g.
files that are easily read by R's `read.table()` family of functions.  The data
therein should be formatted with a time value column followed by individual
culture measurement columns:

Time   | Culture1  | Culture2  | ...
------ | --------- | --------- | ---
  time |     value |     value | ...
   ... |       ... |       ... | ...

Note, time values that are imported as strings will need to be converted to
numeric values prior to analysis with `groan`.

### Examples

A simple workflow for extracting maximum growth rates would be:

```R
Y = read.csv('path/to/your/data.csv', stringsAsFactors=F)
Y = groan.init(Y)
U = groan.mu(Y)

u.max = max(U)
```

A more complicated workflow that involves adaptive data smoothing and model
fitting, with tabulation of additional growth parameters would be:

```R
Y = read.csv('path/to/your/data.csv', stringsAsFactors=F)
Y = groan.init(Y)
Y.s = groan.smooth(Y, adaptive=T, method='loess')

U = groan.mu(Y.s)
U.s = groan.smooth(U, adaptive=T, method='loess')
U.f = groan.fit(U.s, method='pulse')

stats = data.frame(mumax = max(U.f),
                   t.lag = groan.tlag(U.f),
                   gen   = groan.generations(U.f))
```

Creating graphs of the data is as easy as:

```R
plot(Y)   # plot thumbnail grid of raw growth curves
plot(U.s) # plot thumbnail grid of smoothed specific rate profiles

plot(Y[[1]]) # plot the first growth curve in the data set by itself 
```

Credits
-------
`groan` was developed using:

 * [RStudio](http://www.rstudio.com/ide)
 * [roxygen2](http://cran.r-project.org/web/packages/roxygen2/index.html)
 * [devtools](http://cran.r-project.org/web/packages/devtools/index.html)

Features of `groan` were inspired/suggested by:
 * [BioscreenUtils](https://github.com/wleepang/BioscreenUtils)
 * [GCAF](https://github.com/KarlynB23/GCAF_git) by Goodwin Gibbins, [David Reiss](http://github.com/dreiss-isb), and [Karlyn Beer](http://github.com/KarlynB23)
 * Jabus Tyerman