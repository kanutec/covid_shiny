library(dplyr)
library(shiny)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(DT)
library(gganimate)
library(gifski)
library(av)
library(shinyanimate)
library(rsconnect)
library(shinythemes)
library(tidyr)
library(curl)
library(httr)

set_config(httr::config(ssl_verifypeer=0L))

rsconnect::setAccountInfo(name='nicholas-bett', 
                          token='E2CA808E9AFE75B8D363AF03DC403B22', 
                          secret='gJhwbHf2LaPsHVwtkgYryn3D78IOm45UiFsdhbEi')

data <- read.csv("./coronavirus_kenya.csv")
data_tests <- read.csv("./coronavirus_tests.csv")
data_tests$date <- dmy(data_tests$date)
data_tests$daily.case<- as.numeric(data_tests$daily.case)
data_tests$daily.test<- as.numeric(data_tests$daily.test)
data_tests$total.confirmed.cases<- as.numeric(data_tests$total.confirmed.cases)
data_tests$total.tests<- as.numeric(data_tests$total.tests)

data_recent <- data%>%
  select(1:10, -brief.no.)
data_recent[is.na(data_recent)] <- 0
data_recent$new.deaths <- as.integer(data_recent$new.deaths)
data_recent$total.deaths <- as.integer(data_recent$total.deaths)
data_recent$new.recoveries <- as.integer(data_recent$new.recoveries)
data_recent$total.recoveries <- as.integer(data_recent$total.recoveries)


data_recent_main1 <- data_recent[,c(1,4,6,8,9)]%>%
  gather("cases", "number", -day)


ui <- fluidPage(
  titlePanel("Covid 19 Kenya"),
  theme = shinythemes::shinytheme('lumen'),
  sidebarLayout(
    sidebarPanel(
      sliderInput("day", "Day", max(data_recent_main1$day), min=1, max=max(data_recent_main1$day), step=10),
      selectInput("cases", "Type of Case", choices = c("All cases"), selected = "All cases"),
      sliderInput("date", "Date.tests", max(data_tests$date), min= min(data_tests$date), max=max(data_tests$date, step = 10)),
      downloadButton("downloaddatset", "Download Dataset")),
      mainPanel(
      tabsetPanel(tabPanel("Plot.Cases", plotOutput("plot")), 
                  tabPanel("Plot.Tests", plotOutput("plot.test")),
                  tabPanel("Table.Cases",DTOutput("table1")), 
                  tabPanel("Table.Tests",DTOutput("table2")))
)
)
)

server <- function(input, output, session){
  output$plot <- renderPlot({ggplot(data=data_recent_main1%>%
                                      filter(day<=input$day))+
      geom_line(aes(x=day, y=number, group = cases, colour=cases))+
      labs( title="Number of Confirmed, Active, Recoveries and Deaths vs Time",
            caption="(Ministry of Health GOK)",
            x="Time in days",
            y="Count")+
      theme_gray()
  })
  output$plot.test <- renderPlot({ggplot(data=data_tests%>%
                                           filter(date<=input$date))+
    geom_col(aes(x=date, y=daily.test), fill="gray")+
    geom_line(aes(x=date, y=total.confirmed.cases, colour=total.confirmed.cases))+
    ggtitle("Daily tests and total confirmed cases")
  })
    
    
  output$table1 <- renderDT({
    data_recent%>%
      filter(day<=input$day)
  })
output$table2 <- renderDT({
  data_tests%>%
    filter(date<=input$date)
}) 
}
shinyApp(ui, server)

#rsconnect::deployApp(appDir = getwd(), 
                     #appFiles = c("./coronavirus_kenya.csv","./coronavirus_tests.csv"), 
                     #account = 'nicholas-bett', 
                     #server = 'shinyapps.io')
