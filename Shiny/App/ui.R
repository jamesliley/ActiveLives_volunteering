library("shiny")
library(shinyTree)
source("opts.R")

# Geometry
dt=10; dl=10 # this far from top and left
th=100 # title height
sw=350 # sidebar width
sp=450 # Plot width
xyw=180 # Size (square) of plot panel

fixedPage(
  absolutePanel(h2('ActiveLives volunteering rates viewer'),top = dt, left = dl),
  absolutePanel(top=dt + th,left=dl,width=sw,  #width=2,
         #selectInput('xcol1', 'Colour map 1 by:', choices=colourOpts,selected = colourOpts[3]),
         #selectInput('xcol2', 'Colour map 2 by:', choices=colourOpts,selected = colourOpts[1]),
         #hr(),
         h4("Colour map 1 by: "),
         tags$head(
           tags$style(
             HTML("
                 .jstree li.jstree-open > a.jstree-anchor > i.jstree-checkbox,
                 .jstree li.jstree-closed > a.jstree-anchor > i.jstree-checkbox {
                   display:none; 
                 }
                }")
           )
         ),    
         shinyTree("panel1tree", checkbox = TRUE,multiple=FALSE),
         hr(),
         h4("Colour map 2 by: "),
         shinyTree("panel2tree", checkbox = TRUE,multiple=FALSE),
         hr(),
         selectInput('ycol', 'Colour scheme:', rainbowOpts,selected=rainbowOpts[3]),
         hr(),
         h6("Draw box on map 1 to zoom"),
         actionButton("zoom", "Zoom maps"),
         actionButton("reset", "Reset maps"),
         hr(),
         h6("Double click on map to show details"),
         hr(),
         h6("Credits"),
         uiOutput("UKmap"),
         uiOutput("IMD"),
         uiOutput("IMD_variation"),
         uiOutput("ActiveLives"),
         uiOutput("NSSEC")
  ),
  absolutePanel(top=dt + th,left=dl + sw + dl,width=sp,  #width=2,
         plotOutput('distPlot1',
                    dblclick=clickOpts("xclick1"),
                    brush = brushOpts(
                      id = "plot1_brush",
                      resetOnNew = TRUE
                    )),
         
         verbatimTextOutput('clickx1')
  ),
  absolutePanel(top=dt + th,left=dl + sw + dl + sp + dl,width=sp,
         plotOutput('distPlot2',
                    dblclick=clickOpts("xclick2")),
         verbatimTextOutput('clickx2') 
  ),
  absolutePanel(top=dt + th,left=dl + sw + dl + sp - round(xyw/2) + 20,
                plotOutput('distPlot3',height=xyw,width=xyw)
  )
)