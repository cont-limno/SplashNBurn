#### Exploring Shiny app ####
# updated: 9-22-22

library(shiny)
library(plotly)
library(shinydashboard)

setwd("C:/Users/immcc/Documents/SplashNBurn")
may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")

may_june_july$Month_factor <- factor(as.character(may_june_july$Month), levels = c('may', 'june', 'july'))
month_colors <- c('dodgerblue','tan','gold')

may_june_july$logChloro <- log(may_june_july$Chloro)
may_june_july$logTP <- log(may_june_july$TP)
may_june_july$logOP <- log(may_june_july$OP)
may_june_july$logTN <- log(may_june_july$TN)
may_june_july$logNH4N <- log(may_june_july$NH4N)
may_june_july$logNO2NO3 <- log(may_june_july$NO2NO3)
may_june_july$logANCmg <- log(may_june_july$ANCmg)
may_june_july$logANCaueq <- log(may_june_july$ANCaueq)
may_june_july$logTSS <- log(may_june_july$TSS)
may_june_july$logTSVS <- log(may_june_july$TSVS)
may_june_july$logDOC <- log(may_june_july$DOC)

may_data <- subset(may_june_july, Month=='may')
june_data <- subset(may_june_july, Month=='june')
july_data <- subset(may_june_july, Month=='july')

## working, basic version

shinyApp(
  ui <- shinyUI(fluidPage(
    sidebarLayout(sidebarPanel( h4("RAPID project")),
                  mainPanel(plotlyOutput("plot1"))
    )
  )),
  
  server <- shinyServer(
    function(input, output) {
      output$plot1 <- renderPlotly({
        p <- ggplot(data=may_june_july,aes(x=TN,y=TP,shape=Lake, color=Site))
        p <- p + geom_point(size=2) + theme_classic() +
          theme(legend.position='none')
        
        ggplotly(p, tooltip=c("colour"))
      })
    }
  ))

#shinyApp(ui, server)

### Experimental area ###
ui=fluidPage( 
  selectInput(inputId="location",label="Select desired month", 
              choices=c("May"=may_data,"June"=june_data,"July"=july_data)), 
  tableOutput("table") ) 

server=function(input, output){
  
  inputx=reactive({get(input$location)}) 
  output$table<-renderTable(inputx())
  
}
shinyApp(ui,server)


## This one also works decently well
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable=T),
  dashboardBody(
    fluidRow(
      box(width=3, selectInput("scat_x", label = h3("select x-axis"), 
                      choices = colnames(may_june_july), selected='TP'),
          selectInput("scat_y", label = h3("select y-axis"), 
                      choices = colnames(may_june_july), selected='DOC'),
          selectInput("color", label = h3("select color variable"),
                      choices = c("Conn class"="Lake", "Lake name"="Site", "Burned vs. Control"="Type"),
                      selected='Type')),
      box(plotOutput("scatter", height = 450, width=600))
    )
    
  )
)

server <- function(input, output) {
  output$scatter<- renderPlot({
    ggplot(may_data, aes_string(x=input$scat_x, y=input$scat_y, color=input$color)) + 
      geom_point(size=3)+
      theme_classic()+
      theme(axis.text.x = element_text(color='black', size=12),
            axis.text.y = element_text(color='black', size=12))
  })
}


shinyApp(ui, server)
