# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. Find out more about building applications here:
# http://shiny.rstudio.com/


# LOADING LIBRARIES
# ***********************************************
library(shiny)
library(lattice)
library(latticeExtra)
library(tidyr)
library(plyr)
source("calculateMu.R")
source("custom.themes.R")

# define some global variables like
# directory of raw data
datadir={
  if (grepl("multicultivator/ShinyMC", getwd())) gsub(".ShinyMC", "", getwd())
  else if(dir.exists("/media/transfer/multicultivator/")) "/media/transfer/multicultivator/"
  else "data/"
}


# make list of database files in data folder
datalistfiles <- list.files(datadir, pattern="measurements.csv", full.names=TRUE, 
  recursive=TRUE)
co2files <- list.files(datadir, pattern=".CO2.txt$", full.names=TRUE, 
  recursive=TRUE)


# SHINY UI
# ***********************************************
# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(
  
   # Title on NavBar Header
  "ShinyMC - interactive cultivation tools",
  
  # Use one of different shiny themes
  theme=shinytheme("cosmo"),
  #shinythemes::themeSelector(),
  
  # Sidebar
  sidebarLayout(
    
    # HERE COME ALL CONTROLS FOR THE SIDEBAR PANEL
    sidebarPanel(width=4,
   
      # SOME GENERAL PLOT OPTIONS
      selectInput("UserDataChoice",
        "Choose data file:", datalistfiles, 
        selected=tail(datalistfiles,1)),
      
      fluidRow(
        column(width=8,
          checkboxGroupInput("UserChannelCheck", 
          "Select Channels:", choices=1:8, selected=1:8, inline=TRUE)
        ),
        column(width=4,
          selectInput("UserTheme", 
            "Select theme:", choices=c("lattice grey", "lattice blue", "ggplot1", "ggplot2"), 
            selected="lattice grey")
        )
      ),
      
      fluidRow(
        column(width=4, 
          selectInput("UserPanelLayout", 
            "Panel layout:", choices=list("c(1,4)", "c(2,2)", "c(4,1)", "c(1,8)", "c(2,4)", "c(4,2)", "c(8,1)"),
            selected="c(4,2)")
        ),
        column(width=4, 
          numericInput("UserPrintWidth",
            "Plot width:", value=8)
        ),
        column(width=4, 
          numericInput("UserPrintHeight",
            "Plot height:", value=6)
        )
      ),
      
      # OD PLOT OPTIONS
      hr(),
      h4("OPTICAL DENSITY"),
      column(width=4,
        checkboxGroupInput("UserODType", 
          "Points, lines:", choices=c("p","l"), selected="l", inline=TRUE)
      ),
      column(width=4,
        selectInput("UserODCorrect", 
          "correct OD:", choices=c(TRUE, FALSE), selected=FALSE)
      ),
      column(width=4,
        selectInput("UserLogY",
          "Y axis type:", c("linear", "logarithmic"), selected="linear")
      ),
      conditionalPanel(condition="input.UserLogY=='linear'",
        sliderInput("UserYlim", 
          "Y scale range:", min=-0.2, max=10, step=0.05, value=c(-0.1, 1))
      ),
      conditionalPanel(condition="input.UserLogY=='logarithmic'",
        sliderInput("UserYlimLog", 
          "Y scale range:", min=0.01, max=10, step=0.05, value=c(0.01, 1))
      ),
      sliderInput("UserXlim", 
        "X scale range:", min=0, max=500, step=5, value=c(0, 100)),
      
      # GROWTH RATE OPTIONS
      hr(),
      h4("GROWTH RATE"),
      fluidRow(
        column(width=6,
          selectInput("UserMuType",
            "Type of cultivation:", c("batch mode", "conti - dilution", "conti - interval"), 
            selected="batch mode")
        ),
        column(width=6,
          checkboxGroupInput("UserMuPlot", 
            "Points, lines, trend, smooth:", choices=c("p","l","t","sm"), selected="p", inline=TRUE)
        )
      ),
      fluidRow(
        column(width=6, 
          conditionalPanel(condition="input.UserMuType=='conti - dilution' | input.UserMuType=='batch mode'",
            selectInput("UserMuTime",
              "t for µ estimation:", 2:10, selected=5)
          ),
          conditionalPanel(condition="input.UserMuType=='conti - interval'",
            numericInput("UserMaxInterval",
              "threshold max:", value=0.2, min=0, max=1)
          )
        ),
        column(width=6,
          conditionalPanel(condition="input.UserMuType=='conti - dilution'",
            numericInput("UserDilFactor",
              "Dil factor:", value=0.1, min=0, max=1)
          ),
          conditionalPanel(condition="input.UserMuType=='conti - interval'",
            numericInput("UserRsquared",
              "R^2 filtering:", value=0.9, min=0, max=1)
          )
        )
      ),
      conditionalPanel(condition="input.UserMuType=='conti - interval'",
        fluidRow(
          column(width=6, 
            selectInput("UserMinSelect",
              "Interval length:", c("auto", "min-max", 2:7,10,15,20), selected="auto")
          )
        )
      ),
      sliderInput("UserMUYlim", 
        "Y scale range:", min=0, max=0.5, step=0.01, value=c(-0.01, 0.1)),
      sliderInput("UserLoess", 
        "Loess smoothing:", min=0, max=1, step=0.1, value=0.4)
    ),
    
    # Show plots on extra tabs
    # Each tab has individual Download buttons
    column(width=8,
      wellPanel(
        tabsetPanel(
          tabPanel("OD", uiOutput("ODplot.ui"),
            downloadButton("UserDownloadOD", "Download svg")
          ),
          
          tabPanel("Growthrate", uiOutput("MUplot.ui"),
            downloadButton("UserDownloadMU", "Download svg"),
            downloadButton("UserDownloadMUdat", "Download table")
          ),
          
          tabPanel("Retention", uiOutput("RTplot.ui"),
            downloadButton("UserDownloadRT", "Download svg"),
            downloadButton("UserDownloadRTdat", "Download table")
          ),
          
          tabPanel("Temp", uiOutput("Tempplot.ui"),
            downloadButton("UserDownloadTemp", "Download svg")
          ),
          
          tabPanel("OD correction", uiOutput("ODcorrection.ui"),
            downloadButton("UserDownloadODcorr", "Download table")
          ),
          
          tabPanel("CO2", uiOutput("CO2plot.ui"),
          # additional user controls for CO2 file
            fluidRow(
              column(width=2, downloadButton("UserDownloadCO2", "Download svg")),
              column(width=8,
                selectInput("UserCO2Choice", width="100%",
                NULL, co2files, selected=tail(co2files, 1))),
              column(width=2, 
                actionButton("UserButtonCO2", "Refresh")
              )
            )
          )
        )
      )
    )
  )
))


# SHINY SERVER
# ***********************************************
server <- shinyServer(function(input, output) {
  
  # MAIN DATA IS LOADED
  # the reactive environment makes sure all widgets can use the data
  # without re-reading every time
  data <- reactive({
    
    # read csv tables of user selection
    data <- read.csv(input$UserDataChoice, head=TRUE, row.names=1)
    # filter by selected channels
    data <- subset(data, channel_id %in% input$UserChannelCheck)
    data
  
  })
  
  
  # To control size of the plots, we need to wrap the ODplot and Muplot
  # into additional renderUI function that can take height argument
  output$ODplot.ui <- renderUI({
    plotOutput("ODplot", height=input$UserPrintHeight*100)
  })
  output$MUplot.ui <- renderUI({
    plotOutput("MUplot", height=input$UserPrintHeight*100)
  })
  output$RTplot.ui <- renderUI({
    plotOutput("RTplot", height=input$UserPrintHeight*100)
  })
  output$Tempplot.ui <- renderUI({
    plotOutput("Tempplot", height=input$UserPrintHeight*100)
  })
  output$CO2plot.ui <- renderUI({
    plotOutput("CO2plot", height=input$UserPrintHeight*50)
  })
  output$ODcorrection.ui <- renderUI({
    tableOutput("ODcorrtable")
  })
  
  # plot data using xyplot from lattice package, 
  # that is made for multifactorial data
  output$ODplot <- renderPlot(res=120, {
    
    
    # set log or lin flag and adjust scales accordingly
    if (input$UserLogY=="linear")
      scaleoptions=list(
        alternating=FALSE, 
        x=list(limits=input$UserXlim),
        y=list(limits=input$UserYlim)
      ) else
      scaleoptions=list(
        alternating=FALSE,
        x=list(limits=input$UserXlim),
        y=list(log=10, limits=input$UserYlimLog)
      )
    

    # select theme
    if (input$UserTheme=="ggplot1") theme <- ggplot2like()
    else if (input$UserTheme=="ggplot2") theme <- custom.ggplot
    else if (input$UserTheme=="lattice grey") theme <- custom.lattice
    else if (input$UserTheme=="lattice blue") theme <- theEconomist.theme()
    
    
    # select OD correction
    if(input$UserODCorrect) 
      od_select <- 'od_corr' else
      od_select <- 'od_value'
    
    
    # actual plot is drawn
    ODplot <- xyplot(get(od_select) ~ as.numeric(batchtime_h) | factor(channel_id), 
      data(), groups=od_led, par.settings=theme, 
      layout=eval(parse(text=input$UserPanelLayout)), 
      auto.key=list(columns=2), 
      as.table=TRUE,
      scales=scaleoptions,
      xlab="time [h]", ylab="OD",
      type=input$UserODType, lwd=2,
      panel=function(x, y, ...) {
        lims <- round(input$UserXlim, -1)
        panel.abline(v=seq(lims[1], lims[2], by=10), col=grey(0.95))
        panel.grid(h=-1, v=-1, col=grey(0.95))
        panel.xyplot(unique(x), y[seq(1, length(y), 2)]/y[seq(2, length(y), 2)], 
          col=grey(0.7), cex=0.2)
        if (panel.number()==1) {
          panel.text(input$UserXlim[1], input$UserYlim[2]*0.85, cex=0.5, col=grey(0.4),
            pos=4, labels=paste0("last measurement: \n", data()[nrow(data()), "time"]))
        }
        panel.superpose(x, y, ...)
        },
      panel.groups=function(x, y, ...){
        panel.xyplot(x, y, ...)
      }
    )
    
    print(ODplot)
    
    output$UserDownloadOD <- downloadHandler(
      filename="ODplot.svg",
      content = function(file) {
        svg(file, width=input$UserPrintWidth, height=input$UserPrintHeight)
        print(ODplot)
        dev.off()
      },
      contentType="image/svg"
    )
  })
  
  output$MUplot <- renderPlot(res=120, {
    
    # filter to OD720 only
    data <- subset(data(), od_led=="720" &
      batchtime_h > input$UserXlim[[1]] & batchtime_h < input$UserXlim[[2]])
    
    # set log or lin flag and adjust scales accordingly
    scaleoptions=list(
      alternating=FALSE, 
      x=list(limits=input$UserXlim),
      y=list(limits=input$UserMUYlim)
    )
    
    # select theme
    if (input$UserTheme=="ggplot1") theme <- ggplot2like()
    else if (input$UserTheme=="ggplot2") theme <- custom.ggplot
    else if (input$UserTheme=="lattice grey") theme <- custom.lattice
    else if (input$UserTheme=="lattice blue") theme <- theEconomist.theme()
    
    
    # select OD correction
    if(input$UserODCorrect) 
      od_select <- 'od_corr' else
      od_select <- 'od_value'
    
    # call function for mu calculation
    mu <- calculate.mu(data, input, od_select)
    
    # draw dotplot of mu
    MUplot <- xyplot(value ~ batchtime_h | factor(channel_id), mu,
      layout=eval(parse(text=input$UserPanelLayout)), 
      scales=scaleoptions, as.table=TRUE, 
      key=list(text=list("growth rate", col=1, cex=0.9), 
        points=list(pch=19, col=1, cex=0.65)),
      par.settings=theme,
      xlab="time [h]", ylab="µ [1/h]",
      type=input$UserMuPlot, lwd=2,
      panel=function(x, y, ...) {
        xlims <- round(input$UserXlim, -1)
        ylims <- round(input$UserMUYlim, 2)
        panel.abline(v=seq(xlims[1], xlims[2], by=10), col=grey(0.95))
        panel.abline(h=seq(ylims[1], ylims[2], by=0.01), col=grey(0.95))
        panel.grid(h=-1, v=-1, col=grey(0.95))
        panel.xyplot(x, y, ...)
        if (any(input$UserMuPlot %in% "t")) {
          panel.ablineq(h=mean(y), fontfamily="FreeSans", pos=3, offset=1, cex=0.8,
            label=paste(
              round(mean(y), 4),"\U00B1", 
              round(sd(y), 4)
            )
          )
        }
        if (any(input$UserMuPlot %in% "sm")) {
          try(silent=TRUE,
            panel.loess(x, y, span=input$UserLoess, col="#00BA38", ...))
        }
      }
    )
    
    print(MUplot)
    
    output$UserDownloadMU <- downloadHandler(
      filename="MUplot.svg",
      content = function(file) {
        svg(file, width=input$UserPrintWidth, height=input$UserPrintHeight)
        print(MUplot)
        dev.off()
      },
      contentType="image/svg"
    )
    output$UserDownloadMUdat <- downloadHandler(
      filename="MU.csv",
      content = function(file) {
        write.csv(mu, file)
      },
      contentType="text/csv"
    )
  })
  
  output$RTplot <- renderPlot(res=120, {

    # filter by selected channels and OD720
    data <- subset(data(), od_led=="720" &
      batchtime_h > input$UserXlim[[1]] & batchtime_h < input$UserXlim[[2]])
    
    
    # select theme
    if (input$UserTheme=="ggplot1") theme <- ggplot2like()
    else if (input$UserTheme=="ggplot2") theme <- custom.ggplot
    else if (input$UserTheme=="lattice grey") theme <- custom.lattice
    else if (input$UserTheme=="lattice blue") theme <- theEconomist.theme()
    
    
    # select OD correction
    if(input$UserODCorrect) 
      od_select <- 'od_corr' else
      od_select <- 'od_value'
    
    
    # call function for mu calculation
    mu <- calculate.mu(data, input, od_select)
    mu <- subset(mu, value >0)
    mu$t_doubling <- log(2)/mu$value
    mu$t_retention <- 1/mu$value
    
    
    # set log or lin flag and adjust scales accordingly
    scaleoptions=list(
      alternating=FALSE, 
      x=list(limits=input$UserXlim),
      y=list(limits=c(0, 
        1.4*max(tapply(mu$t_retention, mu$channel_id, median))))
    )

    # draw dotplot of mu
    RTplot <- xyplot(t_doubling + t_retention ~ batchtime_h | factor(channel_id), mu,
      layout=eval(parse(text=input$UserPanelLayout)), 
      scales=scaleoptions, as.table=TRUE, 
      auto.key=list(columns=2),
      par.settings=theme,
      xlab="time [h]", ylab="t_R / t_D [h]",
      type=input$UserMuPlot, lwd=2,
      panel=function(x, y, ...) {
        xlims <- round(input$UserXlim, -1)
        ylims <- round(input$UserMUYlim, 2)
        panel.abline(v=seq(xlims[1], xlims[2], by=10), col=grey(0.95))
        panel.abline(h=seq(ylims[1], ylims[2], by=0.01), col=grey(0.95))
        panel.grid(h=-1, v=-1, col=grey(0.95))
        panel.superpose(x, y, ...)
      },
      panel.groups=function(x, y, ...) {
        panel.xyplot(x, y, ...)
        if (any(input$UserMuPlot %in% "t")) {
          panel.ablineq(h=mean(y), fontfamily="FreeSans", pos=3, offset=1, cex=0.8,
            label=paste(
              round(mean(y), 1),"\U00B1",
              round(sd(y), 1)
            )
          )
        }
        if (any(input$UserMuPlot %in% "sm")) {
          try(silent=TRUE,
            panel.loess(x, y, span=input$UserLoess, col="#00BA38", ...))
        }
      }
    )

    print(RTplot)
    
    output$UserDownloadRT <- downloadHandler(
      filename="RTplot.svg",
      content = function(file) {
        svg(file, width=input$UserPrintWidth, height=input$UserPrintHeight)
        print(RTplot)
        dev.off()
      },
      contentType="image/svg"
    )
    output$UserDownloadRTdat <- downloadHandler(
      filename="RT.csv",
      content = function(file) {
        write.csv(mu, file)
      },
      contentType="text/csv"
    )
  })
  
  output$Tempplot <- renderPlot(res=120, {


    # set log or lin flag and adjust scales accordingly
    scaleoptions=list(
      alternating=FALSE, 
      x=list(limits=input$UserXlim),
      y=list(limits=c(0, 50))
    )
    
    # select theme
    if (input$UserTheme=="ggplot1") theme <- ggplot2like()
    else if (input$UserTheme=="ggplot2") theme <- custom.ggplot
    else if (input$UserTheme=="lattice grey") theme <- custom.lattice
    else if (input$UserTheme=="lattice blue") theme <- theEconomist.theme()

    
    # plot temperature chart
    temp <- xyplot(temperature ~ as.numeric(batchtime_h) | factor(channel_id), 
      subset(data(), od_led=="720"), par.settings=theme, 
      scales=scaleoptions, as.table=TRUE,
      layout=eval(parse(text=input$UserPanelLayout)),
      xlab="time [h]", ylab="T [*C]",
      type=input$UserODType, lwd=2,
      panel=function(x, y, ...) {
        panel.grid(h=-1, v=-1, col=grey(0.95))
        panel.xyplot(x, y, ...)
        panel.text(mean(x), tail(y, 1), labels=paste0("current T = ", round(tail(y, 1), 2), " *C"),
          pos=3, offset=1, cex=0.8, col=1)
      }
    )
    
    print(temp)
    
    output$UserDownloadTemp <- downloadHandler(
      filename="Tempplot.svg",
      content = function(file) {
        svg(file, width=input$UserPrintWidth, height=input$UserPrintHeight)
        print(temp)
        dev.off()
      },
      contentType="image/svg"
    )
  })
  
  output$ODcorrtable <- renderTable(digits=4, {
    
    # OD CORRECTION
    # ***********************************************
    # Per channel and per wavelength OD correction based on
    # first n hour's measurements
    
    
    ODcorr <- sapply(1:3, function(i) {
      with(subset(data(), batchtime_h <= i), {
        # calculate median per channel and led...
        ODtable <- tapply(od_value, list(channel_id, od_led), median)
        # and subtract raw OD values from mean to obtain correction factor
        apply(ODtable, 2, function(x) mean(x)-x)
      })
    })
    ODcorr <- as.data.frame(ODcorr)
    colnames(ODcorr) <- c("Correct_1h", "Correct_2h", "Correct_3h")
    ODcorr$Wavelength <- rep(c(680, 720), each=length(unique(data()[["channel_id"]])))
    ODcorr$Channel <- rep(unique(data()[["channel_id"]]), 2)
    
    
    output$UserDownloadODcorr <- downloadHandler(
      filename="ODcorr.csv",
      content = function(file) {
        write.csv(ODcorr, file)
      },
      contentType="text/csv"
    )
    
    ODcorr
  })
  
  output$CO2plot <- renderPlot(res=120, {

    input$UserButtonCO2
    # read csv tables of user selection
    data <- read.table(input$UserCO2Choice, head=FALSE, sep=" ", fill=TRUE,
      col.names=c("co2", "co2_corr", "sensor", "date", "time"))
    # filter out lines with missing values
    data <- subset(data, apply(data, 1, function(x) !any(is.na(x)))) 
    data$batchtime_h <- strptime(with(data, paste(date, time)),
      format="%Y-%m-%d %H:%M")
    data$batchtime_h <- difftime(data$batchtime_h, data[1, "batchtime_h"], units="hours")
    

    # select theme
    if (input$UserTheme=="ggplot1") theme <- ggplot2like()
    else if (input$UserTheme=="ggplot2") theme <- custom.ggplot
    else if (input$UserTheme=="lattice grey") theme <- custom.lattice
    else if (input$UserTheme=="lattice blue") theme <- theEconomist.theme()
    
    
    # actual plot is drawn
    CO2plot <- xyplot(co2/1000 ~ as.numeric(batchtime_h), data,
      par.settings=theme,
      groups=sensor, 
      auto.key=list(cex=0.8, columns=length(unique(data$sensor))),
      xlab="time [h]", ylab="% CO2",
      type=input$UserODType, lwd=2,
      panel=function(x, y, ...) {
        panel.abline(v=seq(round(min(x)), round(max(x)), by=1), col=grey(0.95))
        panel.grid(h=-1, v=-1, col=grey(0.95))
        panel.xyplot(x, y, ...)
      }
    )
    CO2plot2 <- xyplot(co2*10 ~ as.numeric(batchtime_h), type=NA, data, ylab="ppm CO2")
    
    print(
      doubleYScale(CO2plot, CO2plot2, 
        use.style=FALSE, add.axis=TRUE, add.ylab2=TRUE)
    )
    
    output$UserDownloadCO2 <- downloadHandler(
      filename="CO2plot.svg",
      content = function(file) {
        svg(file, width=input$UserPrintWidth, height=input$UserPrintHeight)
        print(
          doubleYScale(CO2plot, CO2plot2, 
          use.style=FALSE, add.axis=TRUE, add.ylab2=TRUE)
        )
        dev.off()
      },
      contentType="image/svg"
    )
  })
})

# Run the application 
shinyApp(ui=ui, server=server)

