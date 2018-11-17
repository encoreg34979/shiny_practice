#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(stringr) #str_detect
source("getLatLng.R")
Mykey = "YOUR_KEY"
register_google(key = Mykey) 
# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("首頁",
  tabPanel("地址分析",column(3, 
                         h4("API抓取經緯度"),
                         actionButton("request", "取資料"),
                         hr(),
                         numericInput("seed", h3("種子碼"), value = 20180112),
                         numericInput("k_value", h3("k值"), value = 10),
                         actionButton("do", "執行")
  ),
  column(9,
         tabsetPanel(
           tabPanel("地址表單",tableOutput("Rout1")),
           tabPanel("地址分布",plotOutput("Rout2",height = "800px")),
           tabPanel("k-mean結果",plotOutput("Rout3",height = "800px"))
         ))),
  tabPanel("訂單分析",fluidRow(
    column(3, 
           h4("過濾"),
           sliderInput('price', '價格多少以上',
                       min=0, max=12000, value=300,
                       step=100, round=0),
           selectInput('payment', '付款方式', c("信用卡", 
                                            "ATM轉帳", 
                                            "貨到付款", 
                                            "現金", 
                                            "無",
                                            "其他"))
    ),
    column(9, 
           tableOutput('ordersTable')
    )
  )),
  tabPanel("會員分析",fluidRow(
    column(3, 
           h4("user資料"),
           textInput("phone_number", h3("手機號碼"), value = "")
    ),
    column(9, 
           tableOutput('userTable')
    )
  ))
  
   
 
))

# Define server logic required to draw a histogram
server <- function(input, output) {
   v <- reactiveValues(address = read.csv("address.csv",stringsAsFactors = FALSE,header = FALSE, fileEncoding = "UTF-8")) #shiny跟一般R不同的地方,暫存要存在reactiveValue,用法類似於list 
   orders <- read.csv("orders.csv", stringsAsFactors = FALSE)
   user <- read.csv("user.csv", stringsAsFactors = FALSE)
#----------- tabpanel 1開始
   getLatLngWithProcress = function(address, total){
     incProgress(1/total,detail = "解析地址中")
     return(getLatLng(address))
   }
   observeEvent(input$request,{
     withProgress(message = "擷取經緯度", value = 0,{
       v$addresswithLatLng <- v$address %>%
         rowwise() %>%
         mutate(LatLng = getLatLngWithProcress(V1,nrow(v$address))) %>%
         filter(LatLng != "error") %>%
         separate(LatLng,c("Lat","Lng"),sep = ",") %>%
         mutate(Lat = as.numeric(Lat), Lng = as.numeric(Lng))      
     })

   })
   output$Rout1 <- renderTable({
     if (is.null(v$addresswithLatLng))
       return(v$address)
     v$addresswithLatLng
     })
#-------------
  output$Rout2 <- renderPlot({
    if(is.null(v$addresswithLatLng))
      return()
    ggmap(get_googlemap(center=c(121.52311,25.04126), zoom=12, maptype='satellite'), extent='device') + 
      geom_point(data = v$addresswithLatLng, aes(x = Lat, y= Lng), colour = "red")
  })
#-------------
   observeEvent(input$do,{
     if(is.null(v$addresswithLatLng))
       return()
     set.seed(input$seed)
     k <- kmeans(x = v$addresswithLatLng[,c("Lat","Lng")], centers = input$k_value)
     v$addressWithKmean <- v$addresswithLatLng %>%
       ungroup() %>%
       mutate(category = k$cluster)
     print("Kmean success")
   })  
   output$Rout3 <- renderPlot({
     if(is.null(v$addressWithKmean))
       return()
     map <- get_googlemap(center=c(121.52311,25.04126), zoom=12, maptype='satellite')
     ggmap(map,extent='device') + geom_point(data = v$addressWithKmean, aes(x = Lat, y= Lng), colour = factor(v$addressWithKmean$category))  #factor 把向量取唯一後作標籤
   })
#---------------tabpanel 1結束
   output$ordersTable <- renderTable({
     orders %>%
       filter(input$price < PRICE, input$payment == PAYMENTTYPE)
   })
#--------------tabpanel 2結束
   output$userTable <- renderTable({
     if(input$phone_number=="") return(user)
     user %>%
       filter(str_detect(MOBILE, input$phone_number))
   })
   #--------------tabpanel 3結束
}

# Run the application 
shinyApp(ui = ui, server = server)

