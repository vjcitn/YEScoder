
#' function to plot a dot with a given color balance
#' @param r numeric(1) should lie in [0,1], red proportion
#' @param g numeric(1) should lie in [0,1], green proportion
#' @param b numeric(1) should lie in [0,1], blue proportion
#' @examples
#' par(mfrow=c(2,2))
#' colordot(1,0,0)
#' @export
colordot = function(r,g,b) plot(0,0,col=grDevices::rgb(r,g,b),pch=19,cex=15)

#' demonstration app for color balance
#' @import shiny
#' @import littleDeep
#' @export
colorapp = function() {
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(  
   helpText("sliders for color balance"),
   sliderInput("red", "red", min=0, max=1, step=.05, value=1),
   sliderInput("green", "green", min=0, max=1, step=.05, value=0),
   sliderInput("blue", "blue", min=0, max=1, step=.05, value=0)
   ),
   mainPanel(
    tabsetPanel(
     tabPanel("dot", plotOutput("dot")),
     tabPanel("orange", plotOutput("orange"))
    )
   )
  )
 )
 server = function(input, output) {
  output$dot = renderPlot({
   colordot(input$red, input$green, input$blue)
   })
  output$orange = renderPlot({
   if (!exists("ciftrain1k")) data("ciftrain1k", package="littleDeep")
   litarr = ciftrain1k[4]
   arr = getArray(litarr)
   arr[1,,,1] = input$red * arr[1,,,1]
   arr[1,,,2] = input$green * arr[1,,,2]
   arr[1,,,3] = input$blue * arr[1,,,3]
   litarr@arr = arr
   littleDeep::plotOne(litarr)
   })
  }
  
 runApp(list(ui=ui, server=server))
}

#' dump the source code and render
#' @export
dumpapp = function() {
 tf = paste0(tempfile(), ".R")
 dput(colorapp, tf)
 nf = styler::style_file(tf)
 readLines(nf$file)
}
