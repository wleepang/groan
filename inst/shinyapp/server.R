# source('install.R', local=TRUE)

library(shiny)
library(shinyIncubator)
library(groan)

# global vars?
shinyServer(function(input, output, session) {
  dataSet = reactive({
    inFile = input$dataFile
    
    if (is.null(inFile))
      return(NULL)
    
    X = read.csv(inFile$datapath, stringsAsFactors=F)
    
    # convert time
    X$Time = sapply(X$Time, function(x){
      sum(as.numeric(unlist(strsplit(x, ':')))*(1/c(1,60,3600)))
    })
    
    output$timeRangeSlider <<- renderUI({
      sliderInput('timeRange', 'Use the slider below to limit the time range of the analysis.', min=min(X$Time), max=max(X$Time), value=c(min(X$Time), max(X$Time)), ticks=T)
    })
    
    return(X)
  })
  
  Y = reactive({
    if (is.null(dataSet()))
      return(NULL)
    
    if (is.null(input$timeRange))
      return(NULL)
    
    X = dataSet()
    tr = input$timeRange
    
    ix = which(X$Time >= tr[1] & X$Time <= tr[2])
    X$Time = X$Time - X$Time[ix[1]]
    
    Y = groan.init(X[ix, ])
    return(Y)
    
  })
  
  Y.s = reactive({
    if (is.null(dataSet()))
      return(NULL)
    
    if (nchar(input$smoothYMethod) < 1) {
      return(NULL)
    }
    
    progress = Progress$new(session)
    
    message = 'Smoothing data'
    detail = NULL
    if (input$smoothYAdaptive) {
      detail = 'Adaptive methods can take a while, please be patient ...'
    }
    
    progress$set(message = message, detail=detail, value=0.2)
    Y.s = groan.smooth(Y(), method=input$smoothYMethod, adaptive=input$smoothYAdaptive)
    progress$set(message = 'Done', value = 1)
    progress$close()
    
    return(Y.s)
  })
  
  Y.out = reactive({
    if (!is.null(Y.s()))
      return(Y.s())
    
    return(Y())
  })
  
  U = reactive({
    if (is.null(dataSet()))
      return(NULL)
    
    U = groan.mu(Y())
    return(U)
  })
  
  U.s = reactive({
    if (is.null(dataSet()))
      return(NULL)
    
    if (nchar(input$smoothUMethod) < 1) {
      return(NULL)
    }
    
    progress = Progress$new(session)
    
    message = 'Smoothing data'
    detail = NULL
    if (input$smoothUAdaptive) {
      detail = 'Adaptive methods can take a while, please be patient ...'
    }
    
    progress$set(message = message, detail=detail, value=0.2)
    U.s = groan.smooth(U(), method=input$smoothUMethod, adaptive=input$smoothUAdaptive)
    progress$set(message = 'Done', value = 1)
    progress$close()
  
    return(U.s)
  })
  
  U.f = reactive({
    if (is.null(dataSet()))
      return(NULL)
    
    if (!input$fitU) {
      return(NULL)
    }
    
    U = U()
    if (!is.null(U.s())) {
      U = U.s()
    }
    
    progress = Progress$new(session)
    
    progress$set(message='Fitting rate profile', value=0.2)
    U.f = groan.fit(U(), method='pulse')
    progress$set(message='Done', value=1)
    progress$close()
    
    return(U.f)
  })
  
  U.out = reactive({
    if (!is.null(U.f())) {
      return(U.f())
    }
    
    if (!is.null(U.s())) {
      return(U.s())
    }
    
    return(U())
  })
  
  ResultSet = reactive({
    if (is.null(dataSet()))
      return(NULL)
    
    if (is.null(input$statsData) || length(input$statsData) < 1)
      return(NULL)
    
    R = list()
    
    Y = Y.out()
    U = U.out()
    
    if ('umax' %in% input$statsData) R$umax = max(U)
    if ('tlag' %in% input$statsData) R$tlag = groan.tlag(U)
    if ('tsat' %in% input$statsData) R$tsat = groan.tsat(U)
    if ('gen' %in% input$statsData)  R$gen  = groan.generations(U)
    if ('cap' %in% input$statsData)  R$cap  = groan.capacity(Y)
    
    return(as.data.frame(R))
  })
  
  observe({
    if (input$statsDataSelectAll > 0) {
      statChoices = c(`Maximum specific rate`='umax', `Lag time`='tlag', `Saturation time`='tsat', `# Generations`='gen', `Maximum capacity`='cap')
      updateSelectInput(session, 'statsData', choices=statChoices, selected=names(statChoices))  
    }
  })
  
  output$curveGrid = renderPlot({
    if (is.null(dataSet()))
      return(NULL)
    
    pargs = list()
    
    pfun = switch(input$plotData,
                   Y=function(...) {
                     if (is.null(Y())) {
                       return(NULL)
                     }
                     
                     if (input$plotLogY) pargs[['log']] = 'y'
                     if (is.null(Y.s())) {
                       do.call(plot, c(list(Y()), pargs))
                     } else {
                       pargs[['col']] = 'gray80'
                       lyt = do.call(plot, c(list(Y()), pargs))
                       lines(Y.s(), lyt=lyt, col='dodgerblue')
                     }
                   },
                   U=function(...) {
                     # ignore log transform of y-axis
                     if (is.null(U())) {
                       return(NULL)
                     }
                     
                     pargs[['log']] = ''
                     pargs[['type']] = 'p'
                     pargs[['col']] = 'gray80'
                     
                     if (is.null(U.s()) && is.null(U.f())) {
                       # plot just data points
                       lyt = do.call(plot, c(list(U()), pargs))
                     } else if (!is.null(U.s()) && is.null(U.f())) {
                       # plot smoothed curves over points
                       lyt = do.call(plot, c(list(U()), pargs))
                       lines(U.s(), lyt=lyt, col='dodgerblue')
                       
                     } else if (is.null(U.s()) && !is.null(U.f())) {
                       # plot fitted curves over points
                       lyt = do.call(plot, c(list(U()), pargs))
                       lines(U.f(), lyt=lyt, col='darkred')
                       
                     } else {
                       # plot fitted curves over smoothed points
                       lyt = do.call(plot, c(list(U()), pargs))
                       lines(U.s(), lyt=lyt, col='dodgerblue')
                       lines(U.f(), lyt=lyt, col='darkred')
                       
                     }
                     
                     lapply(seq_along(U()), function(n){
                       par(mfg=which(lyt$mat == n, arr.ind=TRUE)[1,])
                       abline(h=0, col='gray50', lty='dashed')
                     })
                   },
                   function(...){return(NULL)})
    
    pfun(pargs=pargs)
  })
  
  output$statsTable = renderTable(ResultSet())
  
  output$downloadStats = downloadHandler(
    filename = function() { paste('parameters-', Sys.Date(), '.csv', sep='') },
    content = function(file) { write.csv(ResultSet(), file, row.names=T) },
    contentType = 'text/csv'
  )
  
  
  
})