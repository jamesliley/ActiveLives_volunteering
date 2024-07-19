#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# TO-DOs
#  Add p-value (test of Spearman's rank correlations, removing outliers/LA 
#   recodes etc) to scatterplots
#  Add glossary of variable names

library(shiny)
library(shinyTree)
source("opts.R")

# Server logic for ActiveLives viewer
shinyServer(function(input, output) {
  
  observeEvent(input$zoom, {
    brush = input$plot1_brush
    if (!is.null(brush)) {
      ranges$x = c(brush$xmin, brush$xmax)
      ranges$y = c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x = range(xr)
      ranges$y = range(yr)
    }
  })
  

  observeEvent(input$reset, {
    ranges$x = range(xr)
    ranges$y = range(yr)
    la1$i=NULL
    la2$i=NULL
  })

  
  output$panel1tree <- renderTree({
    colourOptsTree1
  })
  output$panel2tree <- renderTree({
    colourOptsTree2
  })
  
  output$distPlot1 <- renderPlot({
    par(mar=c(3,1,2,1))
    xput1=processTreeInput(input$panel1tree,1)
    xdat=dat[[xput1]]
    hcols=y2h(input$ycol)
    zcol=colorRampPalette(hcols)(200)
    xd2=(xdat-min(xdat,na.rm=T))/(max(xdat,na.rm=T)-min(xdat,na.rm=T))
    qcol=zcol[1+round(xd2*(length(zcol)-1))]
    qcol[which(is.na(xd2))]="lightgreen"
    ww=which(colourOpts==xput1)
    plot(0,xlim=ranges$x,ylim=ranges$y,
         type="n",xlab="",ylab="",
         main=paste0("Map 1. ",names(colourOpts)[ww],"\n",subtitles[colourOpts[ww]]),
         xaxt="n",yaxt="n",bty="n",cex.main=0.75)
    for (i in 1:length(xpoly)) polygon(xpoly[[i]],col=qcol[i])
    if (!is.null(la1$i)) {
      polygon(xpoly[[la1$i]],border = "darkgreen",lwd=3,col=NA)
    }
    legend("bottomright",legend=c("Missing"),col="lightgreen",pch=16,pt.cex=2,
           bg=rgb(1,1,1,alpha=0.8),box.lwd=0)
    gradientLegend(range(xdat,na.rm=T),side=1,border.col="NA",
                   color=zcol,dec=1,length=0.5)
  })
  
  observeEvent(input$xclick1, {
    z = input$xclick1
    if (length(z)>0) {
      x=z$x; y=z$y
      np=length(xpoly)
      ind=NA
      for (i in 1:np) 
        if (point.in.polygon(x,y,xpoly[[i]][,1],xpoly[[i]][,2])) ind=i
      if (!is.finite(ind)) {
        la1$i=NULL
      } else {
        la1$i=ind
      }
    } else la1$i=NULL 
  })
  
  
  output$clickx1=renderText({
    if (is.null(la1$i)) out="" else out=format_dat(la1$i)
  })
  
  
  output$distPlot2 <- renderPlot({
    par(mar=c(3,1,2,1))
    xput2=processTreeInput(input$panel2tree,2)
    xdat=dat[[xput2]]
    hcols=y2h(input$ycol)
    zcol=colorRampPalette(hcols)(200)
    xd2=(xdat-min(xdat,na.rm=T))/(max(xdat,na.rm=T)-min(xdat,na.rm=T))
    qcol=zcol[1+round(xd2*(length(zcol)-1))]
    qcol[which(is.na(xd2))]="lightgreen"
    ww=which(colourOpts==xput2)
    plot(0,xlim=ranges$x,ylim=ranges$y,
         type="n",xlab="",ylab="",
         main=paste0("Map 2. ",names(colourOpts)[ww],"\n",subtitles[colourOpts[ww]]),
         xaxt="n",yaxt="n",bty="n",cex.main=0.75)
    for (i in 1:length(xpoly)) polygon(xpoly[[i]],col=qcol[i])
    if (!is.null(la2$i)) {
      polygon(xpoly[[la2$i]],border = "purple",lwd=3,col=NA)
    }
    legend("bottomright",legend=c("Missing"),col="lightgreen",pch=16,pt.cex=2,
           bg=rgb(1,1,1,alpha=0.8),box.lwd=0)
    gradientLegend(range(xdat,na.rm=T),side=1,border.col="NA",
                   color=zcol,dec=1,length=0.5)
  })
  
  
  output$distPlot3<-renderPlot({
    par(mar=c(3,3,1,1),mgp=c(1,1,0)/3)
    xput1=processTreeInput(input$panel1tree,1)
    xput2=processTreeInput(input$panel2tree,2)
    xdat1=dat[[xput1]]
    xdat2=dat[[xput2]]
    plot(xdat1,xdat2,pch=16,cex.axis=0.5,ann=FALSE,xaxt="n",yaxt="n",bty="n")
    axis(1,cex.axis=0.5,tck=-0.05)
    axis(2,cex.axis=0.5,tck=-0.05)
    w1=which(colourOpts==xput1)
    w2=which(colourOpts==xput2)
    mt1=names(colourOpts)[w1]; mt2=names(colourOpts)[w2]
    title(xlab=mt1,ylab=mt2,cex.lab=0.7,line=1.2)
    lx=lm(xdat2~xdat1,na.action="na.omit")
    abline(lx,col="red",lwd=2)
    if (!is.null(la1$i) | !is.null(la2$i)) {
      if (!is.null(la1$i)) {
        w=la1$i
        points(xdat1[w],xdat2[w],col="green",pch=16,cex=2)
      }
      if (!is.null(la2$i)) {
        w=la2$i
        points(xdat1[w],xdat2[w],col="purple",pch=16,cex=2)
      }
      #legend("bottomright",c("Map 1","Map 2"),col=c("green","purple"),pch=16)
    }
    
  })
  
  

  observeEvent(input$xclick2, {
    z = input$xclick2
    if (length(z)>0) {
      x=z$x; y=z$y
      np=length(xpoly)
      ind=NA
      for (i in 1:np) 
        if (point.in.polygon(x,y,xpoly[[i]][,1],xpoly[[i]][,2])) ind=i
      if (!is.finite(ind)) {
        la2$i=NULL
      } else {
        la2$i=ind
      }
    } else la2$i=NULL 
  })
  
  
  output$clickx2=renderText({
    if (is.null(la2$i)) out="" else out=format_dat(la2$i)
  })
  
  
  # Credits
  output$UKmap=renderUI({
    tagList("UK LA map: ",
            a("Link",
              href="https://martinjc.github.io/UK-GeoJSON/"))
  })
  output$IMD= renderUI({
    tagList("English IMD: ", 
            a("Link",
              href="https://www.gov.uk/guidance/english-indices-of-deprivation-2019-mapping-resources#indices-of-deprivation-2019-geopackage"))
  })
  output$IMD_variation= renderUI({
    tagList("IMD variation: ", 
            a("Link",
              href="https://www.ons.gov.uk/visualisations/dvc1371/#/E07000223"))
  })
  output$ActiveLives= renderUI({
    tagList("ActiveLives: ", 
            a("Link",
              href="https://www.sportengland.org/research-and-data/data/active-lives"))
  })
  output$NSSEC=renderUI({
    tagList("NS-SeC: ",
            a("Link",
              href="https://www.ons.gov.uk/datasets/TS062/editions/2021/versions/3"))
  })
})
