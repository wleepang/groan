library(shiny)
library(shinyIncubator)

statChoices = c(`Maximum specific rate`='umax', `Lag time`='tlag', `Saturation time`='tsat', `# Generations`='gen', `Maximum capacity`='cap')

shinyUI(pageWithSidebar(
  
  headerPanel('groany: a shiny webapp based on groan'),
  
  sidebarPanel(
    progressInit(),
    tags$h3('Description'),
    tags$p('Calculates growth curve parameters (e.g. specific growth rate, lag 
           time, etc) from high throughput growth curve data.'),
    tags$hr(),
    tags$h4('Input Data'),
    fileInput('dataFile', 'Select a raw data file (*.csv).  Make sure that the first column has a heading of "Time".', multiple=F, accept=c('text/csv')),
    tags$hr(),
    tags$h4('Analysis'),
    conditionalPanel('true',
                     tabsetPanel(
                       tabPanel('Plotting',
                                uiOutput('timeRangeSlider'),
                                selectInput('plotData', 'Show', choices=c(`Growth`='Y', `Specific Rate`='U')),
                                checkboxInput('plotLogY', 'Log Y-axis', value=F)
                       ),
                       tabPanel('Smoothing',
                                helpText('Smoothing removes noise from the data, making parameter estimation more robust.',
                                         'SPLINE smoothing fits a curve through all points and may not reduce noise.',
                                         'LOESS and LOWESS methods use locally weighted polynomial fitting and is more robust to outlier points',
                                         'Adaptive algorithms iteratively optimize smoothing parameters, but take extra time to compute.'),
                                tabsetPanel(tabPanel('Growth', 
                                                     selectInput('smoothYMethod', 'Method', choices=c(`-- none --`=NA, 'spline', 'loess', 'lowess')),
                                                     checkboxInput('smoothYAdaptive', 'Use adaptive algorithm?', value=F)
                                ),
                                            tabPanel('Rate',
                                                     selectInput('smoothUMethod', 'Method', choices=c(`-- none --`=NA, 'spline', 'loess', 'lowess')),
                                                     checkboxInput('smoothUAdaptive', 'Use adaptive algorithm?', value=F),
                                                     checkboxInput('fitU', 'Fit pulse model?', value=T)
                                            )
                                )
                       ),
                       tabPanel('Statistics',
                                selectInput('statsData', 'Select parameters to estimate:', choices=statChoices, selected=names(statChoices), multiple=T),
                                tags$br(),
                                actionButton('statsDataSelectAll', 'Select All')
                       )
                     )
    ),
    tags$hr(),
    tags$h3('Credits'),
    tags$p('Created by', tags$a(href='mailto:wleepang@gmail.com', 'W. Lee Pang'), 'using:',
           tags$ul(tags$li(tags$a('R', href='http://www.r-project.org'), 'a data analysis platform.'),
                   tags$li(tags$a('package:shiny', href='https://github.com/rstudio/shiny'), 'a package for developing web applications.'),
                   tags$li(tags$a('package:groan', href='https://github.com/wleepang/groan'), 'a package for growth curve analysis.')))
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Curves', plotOutput('curveGrid', height='800px')),
      tabPanel('Calculated Parameters', 
               downloadButton('downloadStats'), 
               tags$hr(),
               tableOutput('statsTable'))
    )
  )
))