#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(rJava)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(stringr)
library(RODBC)
library(lubridate)
library(dygraphs)
library(xts)
library(DT)
library(ggplot2)
library(ggiraph)
library(xlsx)
library(foreach)
library(doParallel)



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Cubacel RAN KPI"),
  dashboardSidebar(
    
    #textInput("idate","Date(dd/mm/yyyy):",value = format(Sys.Date()-1, format="%d-%m-%Y") ),
    dateInput("idate","Incio :",value = Sys.Date()-1, format = "yyyy-mm-dd"),
    dateInput("fdate","Final :",value = Sys.Date(), format ="yyyy-mm-dd"),

    actionButton("query","Ejecutar Query"),
    
    sidebarMenu(
      menuItem("Tecnologia", tabName = "Tecno", icon = icon("tb"),
               menuSubItem("2G", tabName = "Tec2G"),
               menuSubItem("3G", tabName = "Tec3G")
               ),
      menuItem("Provincia", tabName = "Provi", icon = icon("tb"),
               menuSubItem("2G", tabName = "Pro2G"),
               menuSubItem("3G", tabName = "Pro3G")
      ),
      menuItem("Controlador", tabName = "Cont", icon = icon("tb"),
               menuSubItem("2G", tabName = "Cont2G"),
               menuSubItem("3G", tabName = "Cont3G")
      ),
      menuItem("Celda", tabName = "cel", icon = icon("tb"),
               menuSubItem("2G", tabName = "Cel2G"),
               menuSubItem("3G", tabName = "Cel3G")
      ),
      
      downloadButton("EstadoRed","Descargar")
   
    )
  ),
  


  dashboardBody(
    tabItems(
  #####2G###### 
      ####Tecnologia####
      tabItem(tabName = "Tec2G",
              fixedRow(
              tabBox(width = 20,height = "2000px",
                       title = "KPI",id="tec2G",
                       # The id lets us use input$tabset1 on the server to find the current tab
                tabPanel(title = "KPI",
                         box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("kpi2GR")),
                         box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("kpi2GE")),
                         box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("kpi2GH")),
                         box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("kpi2GN"))
                         
                    ),
                
                tabPanel(title = "Trafico",
                         box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("traf2GR")),
                         box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("traf2GE")),
                         box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("traf2GH")),
                         box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("traf2GN"))
                         
                ),
                tabPanel(title = "Cong",
                         box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("cong2GR")),
                         box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong2GE")),
                         box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong2GH")),
                         box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong2GN"))
                         
                ),
                
                tabPanel(title = "Drop",
                         box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("drop2GR")),
                         box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("drop2GE")),
                         box(status = "info", width = 10, title = "HUAWEI", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("drop2GH")),
                         box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("drop2GN"))
                         
                ),
                tabPanel(title = "Paging",
                         box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("pag2GR")),
                         box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pag2GE")),
                         box(status = "info", width = 10, title = "HUAWEI", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pag2GH")),
                         box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pag2GN"))
                         
                ),
                tabPanel(title = "PagingRate",
                         box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("pagR2GR")),
                         box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagR2GE")),
                         box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagR2GH")),
                         box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagR2GN"))
                         
                ),
                tabPanel(title = "HandoverRate",
                         box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("hov2GR")),
                         box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hov2GE")),
                         box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hov2GH")),
                         box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hov2GN"))
                         
                )
                
                
              )
              
              
              )
      ),
      
      ####Provincia####
      tabItem(tabName = "Pro2G",
              fixedRow(
                tabBox(width = 20,height = "2000px",
                       title = "KPI",id="Pro2G",
                       # The id lets us use input$tabset1 on the server to find the current tab
                       tabPanel(title = "ACC",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("accP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("accP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("accP2GH")),
                                box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("accP2GN"))
                                
                       ),
                       
                       tabPanel(title = "RET",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("retP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("retP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("retP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("retP2GN"))
                                
                       ),
                       tabPanel(title = "SER",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("serP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("serP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("serP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("serP2GN"))
                                
                       ),
                       
                       tabPanel(title = "TraficoTCH",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("ttchP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ttchP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ttchP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ttchP2GN"))
                                
                       ),
                       tabPanel(title = "TraficoSDCCH",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("tsdcP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("tsdcP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("tsdcP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("tsdcP2GN"))
                                
                       ),
                       tabPanel(title = "CongTCH",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("ctchP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ctchP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ctchP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ctchP2GN"))
                                
                       ),
                       tabPanel(title = "CongSDCCH",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("csdcP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("csdcP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("csdcP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("csdcP2GN"))
                                
                       ),
                       tabPanel(title = "DropTCH",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("dtchP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dtchP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dtchP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dtchP2GN"))
                                
                       ),
                       tabPanel(title = "DropSDCCH",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("dsdcP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dsdcP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dsdcP2GH")),
                                box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dsdcP2GN"))
                                
                       ),
                       tabPanel(title = "Paging",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("pagP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagP2GH")),
                                box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagP2GN"))
                                
                       ),
                       tabPanel(title = "Paging_Rate",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("pagRP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRP2GH")),
                                box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRP2GN"))
                                
                       ),
                       tabPanel(title = "HandoverRate",
                                box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("hovP2GR")),
                                box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hovP2GE")),
                                box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hovP2GH")),
                                box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hovP2GN"))
                                
                       )
                       
                )
                
                
              )
      ),
      
      ####Controlador####
      tabItem(tabName = "Cont2G",
      fixedRow(
        tabBox(width = 20,height = "2000px",
               title = "KPI",id="Cont2G",
               # The id lets us use input$tabset1 on the server to find the current tab
               tabPanel(title = "ACC",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("accC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("accC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("accC2GH")),
                        box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("accC2GN"))
                        
               ),
               
               tabPanel(title = "RET",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("retC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("retC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("retC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("retC2GN"))
                        
               ),
               tabPanel(title = "SER",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("serC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("serC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("serC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("serC2GN"))
                        
               ),
               
               tabPanel(title = "TraficoTCH",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("ttchC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ttchC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ttchC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ttchC2GN"))
                        
               ),
               tabPanel(title = "TraficoSDCCH",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("tsdcC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("tsdcC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("tsdcC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("tsdcC2GN"))
                        
               ),
               tabPanel(title = "CongTCH",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("ctchC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ctchC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ctchC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ctchC2GN"))
                        
               ),
               tabPanel(title = "CongSDCCH",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("csdcC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("csdcC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("csdcC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("csdcC2GN"))
                        
               ),
               tabPanel(title = "DropTCH",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("dtchC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dtchC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dtchC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dtchC2GN"))
                        
               ),
               tabPanel(title = "DropSDCCH",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("dsdcC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dsdcC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dsdcC2GH")),
                        box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dsdcC2GN"))
                        
               ),
               tabPanel(title = "Paging",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("pagC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagC2GH")),
                        box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagC2GN"))
                        
               ),
               tabPanel(title = "Paging_Rate",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("pagRC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRC2GH")),
                        box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRC2GN"))
                        
               ),
               tabPanel(title = "HandoverRate",
                        box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("hovC2GR")),
                        box(status = "info", width = 10, title = "ERICSSON", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hovC2GE")),
                        box(status = "info", width = 10, title = "HUAWEI",  solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hovC2GH")),
                        box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("hovC2GN"))
                        
               )
               
               
        )
        
        
      )
      
  
    ),
  
  #####3G######
      ####Tecnologia####
  tabItem(tabName = "Tec3G",
  fixedRow(
    tabBox(width = 20,height = "2000px",
           title = "KPI",id="tec3G",
           # The id lets us use input$tabset1 on the server to find the current tab
           tabPanel(title = "KPI",
                    box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("kpi3GR")),
                    box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("kpi3GE")),
                    box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("kpi3GH")),
                    box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("kpi3GN"))
                    
           ),
           
           tabPanel(title = "TraficoCS",
                    box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("traf3GR")),
                    box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("traf3GE")),
                    box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("traf3GH")),
                    box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("traf3GN"))
                    
           ),
           tabPanel(title = "VolumenDatos",
                    box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("vol3GR")),
                    box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("vol3GE")),
                    box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("vol3GH")),
                    box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("vol3GN"))
                    
           ),
           tabPanel(title = "Cong_RRC",
                    box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("cong_rrc_3GR")),
                    box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong_rrc_3GE")),
                    box(status = "info", width = 10, title = "HUAWEI", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong_rrc_3GH")),
                    box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong_rrc_3GN"))
                    
           ),
           
           tabPanel(title = "Cong_RAB",
                    box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("cong_rab_3GR")),
                    box(status = "info", width = 10, title = "ERICSSON",skin = "green" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong_rab_3GE")),
                    box(status = "info", width = 10, title = "HUAWEI", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong_rab_3GH")),
                    box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("cong_rab_3GN"))
                    
           ),
           
           tabPanel(title = "Drop",
                    box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("drop3GR")),
                    box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("drop3GE")),
                    box(status = "info", width = 10, title = "HUAWEI", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("drop3GH")),
                    box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("drop3GN"))
                    
           ),
           
           tabPanel(title = "Paging%",
                    box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("pag3GR")),
                    box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pag3GE")),
                    box(status = "info", width = 10, title = "HUAWEI", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pag3GH")),
                    box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pag3GN"))
                    
           )
            
    )
    
    
  )
 
  ),
      ####Provincia####
  tabItem(tabName = "Pro3G",
          fixedRow(
            tabBox(width = 20,height = "2000px",
                   title = "KPI",id="Pro3G",
                   # The id lets us use input$tabset1 on the server to find the current tab
                   tabPanel(title = "ACC_CS",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("acc_csP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("acc_csP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_csP3GH")),
                            box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_csP3GN"))
                            
                   ),
                   
                   tabPanel(title = "ACC_PS",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("acc_psP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("acc_psP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_psP3GH")),
                            box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_psP3GN"))
                            
                   ),
                   tabPanel(title = "RET_CS",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,      dygraphOutput("ret_csP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ret_csP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ret_csP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,    dygraphOutput("ret_csP3GN"))
                            
                   ),
                   tabPanel(title = "RET_PS",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,      dygraphOutput("ret_psP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ret_psP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ret_psP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,    dygraphOutput("ret_psP3GN"))
                            
                   ),
                   tabPanel(title = "TraficoCS",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("trafP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("trafP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("trafP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("trafP3GN"))
                            
                   ),
                   tabPanel(title = "VolumenDatos",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("volP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("volP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("volP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("volP3GN"))
                            
                   ),
                   tabPanel(title = "Cong_CE_RRC",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("ceRrcP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ceRrcP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("ceRrcP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ceRrcP3GN"))
                            
                   ),
                   tabPanel(title = "Cong_PWR_RRC",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pwrRrcP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pwrRrcP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pwrRrcP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pwrRrcP3GN"))
                            
                   ),
                   tabPanel(title = "Cong_CODE_RRC",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("codeRrcP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("codeRrcP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("codeRrcP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("codeRrcP3GN"))
                            
                   ),
                   tabPanel(title = "Cong_CE_RAB",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("ceRabP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ceRabP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("ceRabP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ceRabP3GN"))
                            
                   ),
                   tabPanel(title = "Cong_PWR_RAB",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pwrRabP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pwrRabP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pwrRabP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pwrRabP3GN"))
                            
                   ),
                   tabPanel(title = "Cong_CODE_RAB",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("codeRabP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("codeRabP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("codeRabP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("codeRabP3GN"))
                            
                   ),
                   tabPanel(title = "Drop_CS",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("dropCsP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dropCsP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("dropCsP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("dropCsP3GN"))
                            
                   ),
                   tabPanel(title = "Drop_PS",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("dropPsP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dropPsP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("dropPsP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("dropPsP3GN"))
                            
                   ),
                   
                   tabPanel(title = "Pagings",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pagP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pagP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pagP3GN"))
                            
                   ),
                   
                   tabPanel(title = "PagingRate",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pagRP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pagRP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pagRP3GN"))
                            
                   ),
                   
                   tabPanel(title = "RTWP",
                            box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("rtwpP3GR")),
                            box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("rtwpP3GE")),
                            box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("rtwpP3GH")),
                            box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("rtwpP3GN"))
                            
                   )
                   
            )
            
            
          )
  ),
      ####Controlador####
  tabItem(tabName = "Cont3G",
   fixedRow(
     tabBox(width = 20,height = "2000px",
            title = "KPI",id="Cont3G",
            # The id lets us use input$tabset1 on the server to find the current tab
            tabPanel(title = "ACC_CS",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE, collapsed = FALSE,     dygraphOutput("acc_csC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("acc_csC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_csC3GH")),
                     box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_csC3GN"))
                     
            ),
            
            tabPanel(title = "ACC_PS",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("acc_psC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("acc_psC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_psC3GH")),
                     box(status = "info", width = 10, title = "NOKIA", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("acc_psC3GN"))
                     
            ),
            tabPanel(title = "RET_CS",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,      dygraphOutput("ret_csC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ret_csC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ret_csC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,    dygraphOutput("ret_csC3GN"))
                     
            ),
            tabPanel(title = "RET_PS",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,      dygraphOutput("ret_psC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON" ,solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ret_psC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ret_psC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,    dygraphOutput("ret_psC3GN"))
                     
            ),
            tabPanel(title = "TraficoCS",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("trafC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("trafC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("trafC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("trafC3GN"))
                     
            ),
            tabPanel(title = "VolumenDatos",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("volC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("volC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("volC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("volC3GN"))
                     
            ),
            tabPanel(title = "Cong_CE_RRC",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("ceRrcC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ceRrcC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("ceRrcC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ceRrcC3GN"))
                     
            ),
            tabPanel(title = "Cong_PWR_RRC",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pwrRrcC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pwrRrcC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pwrRrcC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pwrRrcC3GN"))
                     
            ),
            tabPanel(title = "Cong_CODE_RRC",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("codeRrcC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("codeRrcC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("codeRrcC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("codeRrcC3GN"))
                     
            ),
            tabPanel(title = "Cong_CE_RAB",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("ceRabC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("ceRabC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("ceRabC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("ceRabC3GN"))
                     
            ),
            tabPanel(title = "Cong_PWR_RAB",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pwrRabC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pwrRabC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pwrRabC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pwrRabC3GN"))
                     
            ),
            tabPanel(title = "Cong_CODE_RAB",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("codeRabC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("codeRabC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("codeRabC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("codeRabC3GN"))
                     
            ),
            tabPanel(title = "Drop_CS",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("dropCsC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dropCsC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("dropCsC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("dropCsC3GN"))
                     
            ),
            tabPanel(title = "Drop_PS",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("dropPsC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("dropPsC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("dropPsC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("dropPsC3GN"))
                     
            ),
            
            tabPanel(title = "Pagings",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pagC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pagC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pagC3GN"))
                     
            ),
            
            tabPanel(title = "PagingRate",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,     dygraphOutput("pagRC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("pagRC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("pagRC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("pagRC3GN"))
                     
            ),
            tabPanel(title = "RTWP",
                     box(status = "info", width = 10, title = "RED",solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,dygraphOutput("rtwpC3GR")),
                     box(status = "info", width = 10, title = "ERICSSON",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,dygraphOutput("rtwpC3GE")),
                     box(status = "info", width = 10, title = "HUAWEI",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,  dygraphOutput("rtwpC3GH")),
                     box(status = "info", width = 10, title = "NOKIA",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,   dygraphOutput("rtwpC3GN"))
                     
            )
            
     )
     
     
   )
  )

  )
)
)


  

      
  #####FUCNIONES####
sigestop <- function(){
  con <- odbcConnect('SIGESTOP',uid="ADMINRED",pwd="adm!nr3d",believeNRows= FALSE)
  
  qry <-"SELECT CELL_ID AS CELDA,PROVINCE as PROVINCIA, CELDA as SITIO from GSM_CELL_ID"
  
  datos <- sqlQuery(con,qry)
  
  datos$PROVINCIA <- as.character(datos$PROVINCIA)
  
  datos$PROVINCIA = ifelse(datos$PROVINCIA == "Habana", "La Habana", datos$PROVINCIA)
  datos$PROVINCIA = ifelse(datos$PROVINCIA == "Isla Juventud", "Isla de la Juventud", datos$PROVINCIA)
  
  odbcClose(con)
  
  return (datos)
}

createFrame <- function(dataH,dataE,dataN,group){

  switch (group,
          "day"  =  {dataH <- dataH %>% group_by(DIA)
                     dataE <- dataE %>% group_by(DIA)%>% mutate(TFNSCAN = TFNSCAN / 24,THNSCAN = THNSCAN / 24, CNSCAN = CNSCAN / 24)
                     dataN <- dataN %>% group_by(DIA)%>% mutate(RES_AV_DENOM14 = RES_AV_DENOM14 / 24,RES_AV_DENOM15 = RES_AV_DENOM15 / 24)},
          
          "cont" = {dataH <- dataH %>% group_by(DIA,BSC)
                    dataE <- dataE %>% group_by(DIA,BSC)%>% mutate(TFNSCAN = TFNSCAN / 24,THNSCAN = THNSCAN / 24, CNSCAN = CNSCAN / 24)
                    dataN <- dataN %>% group_by(DIA,BSC)%>% mutate(RES_AV_DENOM14 = RES_AV_DENOM14 / 24,RES_AV_DENOM15 = RES_AV_DENOM15 / 24)} ,
          
          "prov" = {dataH <- dataH %>% group_by(DIA,PROVINCIA)
                    dataE <- dataE %>% group_by(DIA,PROVINCIA)%>% mutate(TFNSCAN = TFNSCAN / 24,THNSCAN = THNSCAN / 24, CNSCAN = CNSCAN / 24)
                    dataN <- dataN %>% group_by(DIA,PROVINCIA)%>% mutate(RES_AV_DENOM14 = RES_AV_DENOM14 / 24,RES_AV_DENOM15 = RES_AV_DENOM15 / 24)}
         
  )
  
  dataH <- dataH %>% summarise(
    
    ACC_NUM_H = as.numeric(sum(K3000)- sum(K3001))*as.numeric(sum(K3003)-sum(CM30))*as.numeric(sum(K3013A)),
    
    ACC_DEN_H = as.numeric(sum(K3000))*as.numeric(sum(K3003)) * as.numeric(sum(K3010A)),
    
    RET_NUM_H = as.numeric( sum(K3040)+ sum(CH363) - sum(CH303)- sum(CH313)- sum(CH333)- sum(CH353)- sum(CM33)),
    
    RET_DEN_H = as.numeric(sum(K3040)+sum(CH363)- sum(CH313)-sum(CH333)- sum(CH353)),
    
    TRAFICO_TCH_H = round(as.numeric(sum(K3014)),3),
    
    TRAFICO_SDCCH_H = round(as.numeric(sum(K3004)),3),
    
    DROP_TCH_H = as.numeric(sum(CM30)),
    
    DROP_SDCCH_H = as.numeric(sum(CM33)),
    
    PAGING_H = as.numeric(sum(A338 + A339)),
    
    CONG_TCH_NUM_H =   as.numeric(sum(K3011A)),
    
    CONG_SDCCH_NUM_H =  as.numeric(sum(K3001)),
    
    CONG_TCH_DEN_H =   as.numeric(sum(K3010A)),
    
    CONG_SDCCH_DEN_H =  as.numeric(sum(K3000)),
    
    HOV_H =  as.numeric(sum(0)),
    
    PAGING_SENT_H = as.numeric(sum(A330)) + as.numeric(sum(A331))
  )
  
  dataE <- dataE %>% summarise(
    
    ACC_NUM_E = as.numeric(sum(CCALLS) - sum(CCONGS))* as.numeric(sum(CMSESTAB)- sum(CDISSS) + sum(CDISQA) + sum(CDISTA) )
    * as.numeric(sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB)),
    
    ACC_DEN_E = as.numeric(as.numeric(sum(CCALLS)) * as.numeric(sum(CMSESTAB)) * as.numeric(sum(TASSALL))),
    
    RET_NUM_E = as.numeric(sum(TFMSESTB) + sum(THMSESTB) )-as.numeric( sum(TFNDROP) + sum(THNDROP) + sum(TFNDROPSUB) + sum(THNDROPSUB)),
    
    RET_DEN_E = as.numeric(sum(TFMSESTB) + sum(THMSESTB)),
    
    TRAFICO_TCH_E = round(sum(as.numeric(TFTRALACC)/as.numeric(TFNSCAN) + as.numeric(THTRALACC)/as.numeric(THNSCAN)),2),
    
    TRAFICO_SDCCH_E = round(sum(as.numeric(CTRALACC)/as.numeric(CNSCAN)),2),
    
    DROP_TCH_E = as.numeric(sum(DROP_TCH)),
    
    DROP_SDCCH_E = as.numeric(sum(DROP_SDCCH)),
    
    PAGING_E = as.numeric(sum(PAGETOOOLD)+sum(PAGPCHCONG) ),
    
    CONG_TCH_NUM_E =    as.numeric( sum(CNRELCONG) + sum(TFNRELCONG) + sum(THNRELCONG) + sum(TFNRELCONGSUB) + sum(THNRELCONGSUB)), 
    
    CONG_SDCCH_NUM_E =  as.numeric(sum(CCONGS)),
    
    CONG_TCH_DEN_E =   as.numeric(sum(TASSALL)),
    
    CONG_SDCCH_DEN_E =  as.numeric(sum(CCALLS)),
    
    HOV_E =  as.numeric(sum(0)),
    
    PAGING_SENT_E = as.numeric(sum(PAGESRECCS+PAGESRECPS))
  )
  
  dataN <- dataN %>% summarise(
    
    ACC_NUM_N = as.numeric(sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)) * as.numeric(sum(SDCCH_DROP_CALL_AND_HO) ) ,
    
    ACC_DEN_N = as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) ) * as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ),
    
    RET_NUM_N = as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN)) + as.numeric(sum(TCH_HO_ASSIGN) ) -  as.numeric(sum(DROP_AFTER_TCH_ASSIGN)) - as.numeric(sum(TCH_HO_RELEASE)) ,
    
    RET_DEN_N = (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE))),
    
    TRAFICO_TCH_N = round(sum(as.numeric(AVE_BUSY_TCH)/as.numeric(RES_AV_DENOM14)),3),
    
    TRAFICO_SDCCH_N = round(sum(as.numeric(AVE_BUSY_SDCCH)/as.numeric(RES_AV_DENOM15)),3),
    
    DROP_TCH_N = as.numeric(sum(DROP_AFTER_TCH_ASSIGN)),
    
    DROP_SDCCH_N = as.numeric(sum(DROP_SDCCH) - sum(T3101_EXPIRED)),
    
    PAGING_N = as.numeric(sum(DELETE_PAGING_COMMAND)),
    
    CONG_TCH_NUM_N =  as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)- sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) ),
    
    CONG_SDCCH_NUM_N =  as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) ),
    
    CONG_TCH_DEN_N =  as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)),
    
    CONG_SDCCH_DEN_N =  as.numeric(sum(SDCCH_SEIZ_ATT)),
    
    HOV_N =  as.numeric(sum(0)),
    
    PAGING_SENT_N = as.numeric(sum(PAGING_MSG_SENT))
  )
  
  dataH[is.na(dataH)] <- 0
  dataE[is.na(dataE)] <- 0
  dataN[is.na(dataN)] <- 0
  
  switch (group,
          "day"  =  {datosRed <- dataH %>%  inner_join(dataE)%>% inner_join(dataN)%>%group_by(DIA)},
          
          "cont" = {datosRed <- dataH %>%  inner_join(dataE)%>% inner_join(dataN)%>%group_by(DIA,BSC)},
          
          "prov" = {datosRed <- dataH %>%  inner_join(dataE)%>% inner_join(dataN)%>%group_by(DIA,PROVINCIA)}
  )
  
  return(list("datosHua"= dataH, "datosEri"=dataE,"datosRed"= datosRed,"datosNok"=dataN ))
 
}

FinalFrame <- function(data,group){

        switch (group,
          "day" = {datosRed <- data$datosRed %>% group_by(DIA)
                   datosHua <- data$datosHua %>% group_by(DIA)
                   datosEri <- data$datosEri %>% group_by(DIA)
                   datosNok <- data$datosNok %>% group_by(DIA)
                   
                   datosT <- datosRed %>% summarise(
                     ACC = round(100* as.numeric((sum(ACC_NUM_H)+sum(ACC_NUM_E) + sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E) + sum(ACC_DEN_N))),2),
                     
                     RET = round(100 * as.numeric((sum(RET_NUM_H)+sum(RET_NUM_E) + sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E) + sum(RET_DEN_N) )),2),
                     
                     SER = round(100* as.numeric(((sum(ACC_NUM_H)+sum(ACC_NUM_E)+sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E)+sum(ACC_DEN_N)))* ((sum(RET_NUM_H)+sum(RET_NUM_E)+sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E)+sum(RET_DEN_N)))),2),
                     
                     TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_H)+sum(TRAFICO_TCH_E)+sum(TRAFICO_TCH_N)),3),
                     
                     TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_H)+sum(TRAFICO_SDCCH_E)+sum(TRAFICO_SDCCH_N)),3),
                     
                     DROP_TCH = round(as.numeric(sum(DROP_TCH_H)+sum(DROP_TCH_E)+sum(DROP_TCH_N)),3),
                     
                     DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_H)+sum(DROP_SDCCH_E)+sum(DROP_SDCCH_N)),3),
                     
                     PAGING = round(as.numeric(sum(PAGING_H)+sum(PAGING_E)+sum(PAGING_N)),3),
                     
                     CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_H)+sum(CONG_TCH_NUM_E) + sum(CONG_TCH_NUM_N))/(sum(CONG_TCH_DEN_H)+sum(CONG_TCH_DEN_E) + sum(CONG_TCH_DEN_N))),2),
                     
                     CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_H)+sum(CONG_SDCCH_NUM_E) + sum(CONG_SDCCH_NUM_N))/(sum(CONG_SDCCH_DEN_H)+sum(CONG_SDCCH_DEN_E) + sum(CONG_SDCCH_DEN_N))),2),
                     
                     HANDOVER = round(as.numeric(sum(HOV_H)+sum(HOV_E)+sum(HOV_N)),3),
                     
                     PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H)+sum(PAGING_E)+sum(PAGING_N))/ as.numeric(sum(PAGING_SENT_H)+sum(PAGING_SENT_E)+sum(PAGING_SENT_N)),3)
                   )  
                   
                   datosHua <- datosHua %>% summarise(
                     ACC = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))),2),
                     
                     RET = round(100 *as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
                     
                     SER = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))) * as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
                     
                     TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_H)),3),
                     
                     TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_H)),3),
                     
                     DROP_TCH = round(as.numeric(sum(DROP_TCH_H)),3),
                     
                     DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_H)),3),
                     
                     PAGING = round(as.numeric(sum(PAGING_H)),3),
                     
                     CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_H))/(sum(CONG_TCH_DEN_H))),2),
                     
                     CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_H))/(sum(CONG_SDCCH_DEN_H))),2),
                     
                     HANDOVER = round(as.numeric(sum(HOV_H)),3),
                     
                     PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H))/ as.numeric(sum(PAGING_SENT_H)),3)
                   ) 
                   
                   datosHua[is.na(datosHua)] <- 0
                   datosHua$RET = ifelse(datosHua$RET == "-Inf", 0, datosHua$RET)
                   
                   datosEri <- datosEri %>% summarise(
                     
                     ACC = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))),2),
                     
                     RET = round(100 *as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))),2),
                     
                     SER = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))) * as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))) ,2),
                     
                     TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_E)),3),
                     
                     TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_E)),3),
                     
                     DROP_TCH = round(as.numeric(sum(DROP_TCH_E)),3),
                     
                     DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_E)),3),
                     
                     PAGING = round(as.numeric(sum(PAGING_E)),3),
                     
                     CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_E))/(sum(CONG_TCH_DEN_E))),2),
                     
                     CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_E))/(sum(CONG_SDCCH_DEN_E))),2),
                     
                     HANDOVER = round(as.numeric(sum(HOV_E)),3),
                     
                     PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_E))/ as.numeric(sum(PAGING_SENT_E)),3)
                   )  
                   
                   datosNok <- datosNok %>% summarise(
                     
                     ACC = round(100 - 100* as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N))),2),
                     
                     RET = round(100 *as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N))),2),
                     
                     SER = round(  (100 - 100*as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N)))) * (100*as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N)))) /100 ,2),
                     
                     TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_N)),3),
                     
                     TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_N)),3),
                     
                     DROP_TCH = round(as.numeric(sum(DROP_TCH_N)),3),
                     
                     DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_N)),3),
                     
                     PAGING = round(as.numeric(sum(PAGING_N)),3),
                     
                     CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_N))/(sum(CONG_TCH_DEN_N))),2),
                     
                     CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_N))/(sum(CONG_SDCCH_DEN_N))),2),
                     
                     HANDOVER = round(as.numeric(sum(HOV_N)),3),
                     
                     PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_N))/ as.numeric(sum(PAGING_SENT_N)),3)
                   )  
                   
                   },
          
          "prov" = {
           
            datosHua <- data$datosHua %>% group_by(DIA,PROVINCIA)
            datosEri <- data$datosEri %>% group_by(DIA,PROVINCIA)
            datosNok <- data$datosNok %>% group_by(DIA,PROVINCIA)
            
            
            ##Por total
            habana_eri <- filter(datosEri, PROVINCIA == "La Habana")
            
            habana_hua <- filter(datosHua, PROVINCIA == "La Habana")
            
            ciefue_hua <- filter(datosHua, PROVINCIA == "Cienfuegos")
            
            ciefue_nok <- filter(datosNok, PROVINCIA == "Cienfuegos")
          
            cienfuegos <- ciefue_hua %>% inner_join(ciefue_nok) %>% group_by(DIA,PROVINCIA)
            habana <- habana_hua %>% inner_join(habana_eri) %>% group_by(DIA,PROVINCIA)

            habana <- habana %>% summarise(
              ACC = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_E))/(sum(ACC_DEN_H + ACC_DEN_E))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_H + RET_NUM_E))/(sum(RET_DEN_H + RET_DEN_E))),2),
              
              SER = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_E))/(sum(ACC_DEN_H + ACC_DEN_E))) * as.numeric((sum(RET_NUM_H + RET_NUM_E))/(sum(RET_DEN_H + RET_DEN_E))),2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_H + TRAFICO_TCH_E )),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_H + TRAFICO_SDCCH_E)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_H + DROP_TCH_E)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_H + DROP_SDCCH_E)),3),
              
              
              
              PAGING = round(as.numeric(sum(PAGING_H + PAGING_E)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_H + CONG_TCH_NUM_E))/(sum(CONG_TCH_DEN_H + CONG_TCH_DEN_E))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_H + CONG_SDCCH_NUM_E))/(sum(CONG_SDCCH_DEN_H + CONG_SDCCH_DEN_E))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_H + HOV_E)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H)+sum(PAGING_E))/ as.numeric(sum(PAGING_SENT_H)+sum(PAGING_SENT_E)),3)
            ) 
            habana[is.na(habana)] <- 0
            habana <- as.data.frame(habana)
            
            cienfuegos <- cienfuegos %>% summarise(
              ACC = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_N))/(sum(ACC_DEN_H + ACC_DEN_N))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_H + RET_NUM_N))/(sum(RET_DEN_H + RET_DEN_N))),2),
              
              SER = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_N))/(sum(ACC_DEN_H + ACC_DEN_N))) * as.numeric((sum(RET_NUM_H + RET_NUM_N))/(sum(RET_DEN_H + RET_DEN_N))),2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_H + TRAFICO_TCH_N )),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_H + TRAFICO_SDCCH_N)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_H + DROP_TCH_N)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_H + DROP_SDCCH_N)),3),
              
              PAGING = round(as.numeric(sum(PAGING_H + PAGING_N)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_H + CONG_TCH_NUM_N))/(sum(CONG_TCH_DEN_H + CONG_TCH_DEN_N))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_H + CONG_SDCCH_NUM_N))/(sum(CONG_SDCCH_DEN_H + CONG_SDCCH_DEN_N))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_H + HOV_N)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H)+sum(PAGING_N))/ as.numeric(sum(PAGING_SENT_H)+sum(PAGING_SENT_N)),3)
            )
            cienfuegos[is.na(cienfuegos)] <- 0
            cienfuegos <- as.data.frame(cienfuegos)
            
            ##Por Provincia
            datosHua <- datosHua %>% summarise(
              ACC = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
              
              SER = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))) * as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_H)),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_H)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_H)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_H)),3),
              
              PAGING = round(as.numeric(sum(PAGING_H)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_H))/(sum(CONG_TCH_DEN_H))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_H))/(sum(CONG_SDCCH_DEN_H))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_H)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H))/ as.numeric(sum(PAGING_SENT_H)),3)
            ) 
            
            datosHua[is.na(datosHua)] <- 0
           # datosHua = ifelse(datosHua == "-Inf", 0, datosHua)
            
            datosHua <- as.data.frame(datosHua)
            
            datosEri <- datosEri %>% summarise(
              
              ACC = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))),2),
              
              SER = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))) * as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))) ,2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_E)),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_E)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_E)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_E)),3),
              
              PAGING = round(as.numeric(sum(PAGING_E)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_E))/(sum(CONG_TCH_DEN_E))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_E))/(sum(CONG_SDCCH_DEN_E))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_E)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_E))/ as.numeric(sum(PAGING_SENT_E)),3)
            )  
            datosEri[is.na(datosEri)] <- 0
            datosEri <- as.data.frame(datosEri)
            datosNok <- datosNok %>% summarise(
              
              ACC = round(100 - 100* as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N))),2),
              
              SER = round(  (100 - 100*as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N)))) * (100*as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N)))) /100 ,2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_N)),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_N)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_N)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_N)),3),
              
              PAGING = round(as.numeric(sum(PAGING_N)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_N))/(sum(CONG_TCH_DEN_N))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_N))/(sum(CONG_SDCCH_DEN_N))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_N)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_N))/ as.numeric(sum(PAGING_SENT_N)),3)
            ) 
            datosNok[is.na(datosNok)] <- 0
            datosNok <- as.data.frame(datosNok)
       
          datosEri_T <- filter(datosEri, PROVINCIA != "La Habana", PROVINCIA != "Matanzas")
          datosHua_T <- filter(datosHua, PROVINCIA != "La Habana",PROVINCIA != "Villa Clara",PROVINCIA != "Santi Spiritus",PROVINCIA != "Cienfuegos",PROVINCIA != "Pinar del Rio")
          
          datosEri_T$DIA <- as.factor(datosEri_T$DIA)

          datosT <- rbind(datosHua_T,datosEri_T,habana,cienfuegos)
          
          },

          "cont" = {
            datosHua <- data$datosHua %>% group_by(DIA,BSC)
            datosEri <- data$datosEri %>% group_by(DIA,BSC)
            datosNok <- data$datosNok %>% group_by(DIA,BSC)
            
            datosHua <- datosHua %>% summarise(
              ACC = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
              
              SER = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))) * as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_H)),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_H)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_H)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_H)),3),
              
              PAGING = round(as.numeric(sum(PAGING_H)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_H))/(sum(CONG_TCH_DEN_H))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_H))/(sum(CONG_SDCCH_DEN_H))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_H)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H))/ as.numeric(sum(PAGING_SENT_H)),3)
            ) 
            
            datosHua[is.na(datosHua)] <- 0
            datosHua <- as.data.frame(datosHua)
            
            datosEri <- datosEri %>% summarise(
              
              ACC = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))),2),
              
              SER = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))) * as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))) ,2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_E)),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_E)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_E)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_E)),3),
              
              PAGING = round(as.numeric(sum(PAGING_E)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_E))/(sum(CONG_TCH_DEN_E))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_E))/(sum(CONG_SDCCH_DEN_E))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_E)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_E))/ as.numeric(sum(PAGING_SENT_E)),3)
            )
            
            datosEri[is.na(datosEri)] <- 0
            datosEri <- as.data.frame(datosEri)
            
            datosNok <- datosNok %>% summarise(
              
              ACC = round(100 - 100* as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N))),2),
              
              RET = round(100 *as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N))),2),
              
              SER = round(  (100 - 100*as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N)))) * (100*as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N)))) /100 ,2),
              
              TRAFICO_TCH = round(as.numeric(sum(TRAFICO_TCH_N)),3),
              
              TRAFICO_SDCCH= round(as.numeric(sum(TRAFICO_SDCCH_N)),3),
              
              DROP_TCH = round(as.numeric(sum(DROP_TCH_N)),3),
              
              DROP_SDCCH = round(as.numeric(sum(DROP_SDCCH_N)),3),
              
              PAGING = round(as.numeric(sum(PAGING_N)),3),
              
              CONG_TCH =   round(100* as.numeric((sum(CONG_TCH_NUM_N))/(sum(CONG_TCH_DEN_N))),2),
              
              CONG_SDCCH =  round(100* as.numeric((sum(CONG_SDCCH_NUM_N))/(sum(CONG_SDCCH_DEN_N))),2),
              
              HANDOVER = round(as.numeric(sum(HOV_N)),3),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_N))/ as.numeric(sum(PAGING_SENT_N)),3)
            )
            
            datosNok[is.na(datosNok)] <- 0
            datosNok <- as.data.frame(datosNok)
            
            datosT <- rbind(datosHua,datosEri,datosNok)
            
            },
          
          "Tot" = {
        
          data$datosRed$TOTAL <- "TOTAL"
          datosRed <- data$datosRed %>% group_by(TOTAL)
          
          
          datosT <- datosRed %>% summarise(
            ACC = round(100* as.numeric((sum(ACC_NUM_H)+sum(ACC_NUM_E) + sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E) + sum(ACC_DEN_N))),2),
            
            RET = round(100 * as.numeric((sum(RET_NUM_H)+sum(RET_NUM_E) + sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E) + sum(RET_DEN_N) )),2),
            
            SER = round(100* as.numeric(((sum(ACC_NUM_H)+sum(ACC_NUM_E)+sum(ACC_NUM_N))/(sum(ACC_DEN_H)+sum(ACC_DEN_E)+sum(ACC_DEN_N)))* ((sum(RET_NUM_H)+sum(RET_NUM_E)+sum(RET_NUM_N))/(sum(RET_DEN_H)+sum(RET_DEN_E)+sum(RET_DEN_N)))),2)
            
          )  
          
          datosHua <- 1
          datosEri <- 1
          datosNok <- 1
          
          },
          
          "TotP" = {
            datosHua <- data$datosHua %>% group_by(PROVINCIA)
            datosEri <- data$datosEri %>% group_by(PROVINCIA)
            datosNok <- data$datosNok %>% group_by(PROVINCIA)
          
          
          ##Por total
          habana_eri <- filter(datosEri, PROVINCIA == "La Habana")
          
          habana_hua <- filter(datosHua, PROVINCIA == "La Habana")
          
          ciefue_hua <- filter(datosHua, PROVINCIA == "Cienfuegos")
          
          ciefue_nok <- filter(datosNok, PROVINCIA == "Cienfuegos")
          
          cienfuegos <- ciefue_hua %>% inner_join(ciefue_nok) %>% group_by(PROVINCIA)
          habana <- habana_hua %>% inner_join(habana_eri) %>% group_by(PROVINCIA)
          
          habana <- habana %>% summarise(
            ACC = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_E))/(sum(ACC_DEN_H + ACC_DEN_E))),2),
            
            RET = round(100 *as.numeric((sum(RET_NUM_H + RET_NUM_E))/(sum(RET_DEN_H + RET_DEN_E))),2),
            
            SER = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_E))/(sum(ACC_DEN_H + ACC_DEN_E))) * as.numeric((sum(RET_NUM_H + RET_NUM_E))/(sum(RET_DEN_H + RET_DEN_E))),2)
            
           ) 
          habana[is.na(habana)] <- 0
          habana <- as.data.frame(habana)
          
          cienfuegos <- cienfuegos %>% summarise(
            ACC = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_N))/(sum(ACC_DEN_H + ACC_DEN_N))),2),
            
            RET = round(100 *as.numeric((sum(RET_NUM_H + RET_NUM_N))/(sum(RET_DEN_H + RET_DEN_N))),2),
            
            SER = round(100* as.numeric((sum(ACC_NUM_H + ACC_NUM_N))/(sum(ACC_DEN_H + ACC_DEN_N))) * as.numeric((sum(RET_NUM_H + RET_NUM_N))/(sum(RET_DEN_H + RET_DEN_N))),2)
            
            )
          cienfuegos[is.na(cienfuegos)] <- 0
          cienfuegos <- as.data.frame(cienfuegos)
          
          ##Por Provincia
          datosHua <- datosHua %>% summarise(
            ACC = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))),2),
            
            RET = round(100 *as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2),
            
            SER = round(100* as.numeric((sum(ACC_NUM_H))/(sum(ACC_DEN_H))) * as.numeric((sum(RET_NUM_H))/(sum(RET_DEN_H))),2)
            
            ) 
          
          datosHua[is.na(datosHua)] <- 0
          # datosHua = ifelse(datosHua == "-Inf", 0, datosHua)
          
          datosHua <- as.data.frame(datosHua)
          
          datosEri <- datosEri %>% summarise(
            
            ACC = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))),2),
            
            RET = round(100 *as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))),2),
            
            SER = round(100* as.numeric((sum(ACC_NUM_E))/(sum(ACC_DEN_E))) * as.numeric((sum(RET_NUM_E))/(sum(RET_DEN_E))) ,2)
            
            )  
          datosEri[is.na(datosEri)] <- 0
          datosEri <- as.data.frame(datosEri)
          datosNok <- datosNok %>% summarise(
            
            ACC = round(100 - 100* as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N))),2),
            
            RET = round(100 *as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N))),2),
            
            SER = round(  (100 - 100*as.numeric((sum(ACC_NUM_N))/(sum(ACC_DEN_N)))) * (100*as.numeric((sum(RET_NUM_N))/(sum(RET_DEN_N)))) /100 ,2)
            
           ) 
          datosNok[is.na(datosNok)] <- 0
          datosNok <- as.data.frame(datosNok)
          
          datosEri_T <- filter(datosEri, PROVINCIA != "La Habana", PROVINCIA != "Matanzas")
          datosHua_T <- filter(datosHua, PROVINCIA != "La Habana",PROVINCIA != "Villa Clara",PROVINCIA != "Santi Spiritus",PROVINCIA != "Cienfuegos",PROVINCIA != "Pinar del Rio")
          
          datosT <- rbind(datosHua_T,datosEri_T,habana,cienfuegos)
          }
          
         )
         return(list("datosHua"= datosHua, "datosEri"=datosEri,"datosRed"= datosT,"datosNok"=datosNok ))
       
}

createFrame3G <- function(dataH,dataE,dataN,group) {
  
  switch (group,
          "day"  =  {dataH <- dataH %>% group_by(DIA)
          dataE <- dataE %>% group_by(DIA) 
          dataN <- dataN %>% group_by(DIA)},
          
          "cont" = {dataH <- dataH %>% group_by(DIA,RNC)
          dataE <- dataE %>% group_by(DIA,RNC) %>% filter(pmSamplesBestCs12Establish > 0)
          dataN <- dataN %>% group_by(DIA,RNC)} ,
          
          "prov" = {dataH <- dataH %>% group_by(DIA,PROVINCIA)
          dataE <- dataE %>% group_by(DIA,PROVINCIA)%>% filter(pmSamplesBestCs12Establish > 0)
          dataN <- dataN %>% group_by(DIA,PROVINCIA)}
          
          )
          dataH <- dataH %>% summarise(
            
          ACC_NUM_CS_H = as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC))),
          
          ACC_DEN_CS_H = as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB))),
          
          ACC_NUM_PS_H = as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))),
          
          ACC_DEN_PS_H = as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC))),
          
          RET_NUM_CS_H = as.numeric(sum(NUM_AMR_CALL_DROP)),
          
          RET_DEN_CS_H = as.numeric(sum(DEN_AMR_CALL_DROP)),
          
          RET_NUM_PS_H = as.numeric(sum(PS_DROP_FD_NUM)),
          
          RET_DEN_PS_H = as.numeric(sum(PS_DROP_FD_DEN)),
          
          TRAFICO_CS_H = round(as.numeric(sum(TRAFICO_CS)),3),
          
          VOL_H = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA) + sum(TOTAL_VOLUMEN_HSDPA)  )/1000000000,3),
          
          CONG_CE_RRC_NUM_H = as.numeric(sum(CE_CONG_RRC)),
          
          CONG_CE_RRC_DEN_H = as.numeric(sum(RRC_CONN_SETUP)),
          
          CONG_PWR_RRC_NUM_H = as.numeric(sum(PWR_CONG_RRC)),
          
          CONG_PWR_RRC_DEN_H = as.numeric(sum(RRC_CONN_SETUP)),
          
          CONG_CODE_RRC_NUM_H = as.numeric(sum(CODE_CONG_RRC)),
          
          CONG_CODE_RRC_DEN_H = as.numeric(sum(RRC_CONN_SETUP)),
          
          CONG_CE_RAB_NUM_H = as.numeric(sum(CE_PS_CONG_RAB) + sum(CE_CS_CONG_RAB)),
          
          CONG_CE_RAB_DEN_H = as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),
          
          CONG_PWR_RAB_NUM_H = as.numeric(sum(PWR_PS_CONG_RAB) + sum(PWR_CS_CONG_RAB)),
          
          CONG_PWR_RAB_DEN_H =as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),
          
          CONG_CODE_RAB_NUM_H = as.numeric(sum(CODE_PS_CONG_RAB) + sum(CODE_CS_CONG_RAB)),
          
          CONG_CODE_RAB_DEN_H = as.numeric(sum(RAB_CS_ATT) + sum(PS_RAB_ATT)),
          
          DROP_CS_H = as.numeric(sum(CS_DROP_A)),
          
          DROP_PS_H = as.numeric(sum(PS_DROP_A)),
          
          RTWP_H = round(as.numeric(mean(RTWP)),3),
          
          PAGING_H = as.numeric(sum(PAGING_DISCARD)),
          
          PAGING_DEN_H = as.numeric(sum(PAGING_SENT))
          )
          
          dataE <- dataE %>% summarise(
            
            ACC_NUM_CS_E = as.numeric(sum(pmTotNoRrcConnectReqCsSucc))*as.numeric(sum(pmNoRabEstablishSuccessSpeech) + sum(pmNoRabEstablishSuccessCs64) + sum(pmRabEstablishEcSuccess)),
            
            ACC_DEN_CS_E = as.numeric(sum(pmTotNoRrcConnectReqCs) - sum(pmNoLoadSharingRrcConnCs)) * as.numeric(sum(pmRabEstablishEcAttempt) + sum(pmNoRabEstablishAttemptSpeech) + sum(pmNoRabEstablishAttemptCs64) - sum(pmNoDirRetryAtt)),
            
            ACC_NUM_PS_E = as.numeric(sum(pmTotNoRrcConnectReqPsSucc))*as.numeric(sum(pmNoRabEstablishSuccessPacketInteractive)),
            
            ACC_DEN_PS_E = as.numeric(sum(pmTotNoRrcConnectReqPs) - sum(pmNoLoadSharingRrcConnPs)) * as.numeric(sum(pmNoRabEstablishAttemptPacketInteractive)),
            
            RET_NUM_CS_E = as.numeric(sum(pmNoSystemRabReleaseSpeech) + sum(pmNoSystemRabReleaseCs64)),
            
            RET_DEN_CS_E = as.numeric(sum(pmNoNormalRabReleaseSpeech) + sum(pmNoNormalRabReleaseCs64) + sum(pmNoSystemRabReleaseSpeech) + sum(pmNoSystemRabReleaseCs64)),
            
            RET_NUM_PS_E = as.numeric( sum(as.numeric(pmNoSystemRabReleasePacket )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) )),
            
            RET_DEN_PS_E = as.numeric(sum(as.numeric(pmNoNormalRabReleasePacket) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(pmNoSystemRabReleasePacket) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                      + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) )),
            
            
            TRAFICO_CS_E = round(24 * as.numeric(sum(pmSumBestCs12Establish/pmSamplesBestCs12Establish)),2),
            
            VOL_E =  round(as.numeric(sum(as.numeric(pmDlTrafficVolumePsIntHs)) + sum(as.numeric(pmUlTrafficVolumePsIntEul) ) + sum(as.numeric(VOLUMEN_R99_UL) ) + sum(as.numeric(VOLUMEN_R99_DL) )  )/ (8*1000000),3),
            
            CONG_CE_RRC_NUM_E = as.numeric(sum(pmNoRrcReqDeniedAdmDlHw) + sum(pmNoRrcReqDeniedAdmUlHw)),
            
            CONG_CE_RRC_DEN_E = as.numeric(sum(pmTotNoRrcConnectReq)),
            
            CONG_PWR_RRC_NUM_E= as.numeric(sum(pmNoRrcReqDeniedAdmDlPwr)),
            
            CONG_PWR_RRC_DEN_E = as.numeric(sum(pmTotNoRrcConnectReq)),
            
            CONG_CODE_RRC_NUM_E = as.numeric(sum(pmNoRrcReqDeniedAdmDlChnlCode)),
            
            CONG_CODE_RRC_DEN_E = as.numeric(sum(pmTotNoRrcConnectReq)),
            
            CONG_CE_RAB_NUM_E = as.numeric(sum(pmNoFailedRabEstAttemptLackDlHw) + sum(pmNoFailedRabEstAttemptLackUlHw)),
            
            CONG_CE_RAB_DEN_E = as.numeric(sum(pmNoRabEstablishAttemptSpeech) + sum(pmNoRabEstablishAttemptPacketInteractive) + sum(pmNoRabEstablishAttemptPacketStream)  + sum(pmNoRabEstablishAttemptPacketStream128)),
            
            CONG_PWR_RAB_NUM_E = as.numeric(sum(pmNoFailedRabEstAttemptLackDlPwr)),
            
            CONG_PWR_RAB_DEN_E = as.numeric(sum(pmNoRabEstablishAttemptSpeech) + sum(pmNoRabEstablishAttemptPacketInteractive) + sum(pmNoRabEstablishAttemptPacketStream)  + sum(pmNoRabEstablishAttemptPacketStream128)),
            
            CONG_CODE_RAB_NUM_E = as.numeric(sum(pmNoFailedRabEstAttemptLackDlChnlCode) ),
            
            CONG_CODE_RAB_DEN_E = as.numeric(sum(pmNoRabEstablishAttemptSpeech) + sum(pmNoRabEstablishAttemptPacketInteractive) + sum(pmNoRabEstablishAttemptPacketStream)  + sum(pmNoRabEstablishAttemptPacketStream128)),
            
            DROP_CS_E =  as.numeric(sum( pmNoSystemRabReleaseSpeech + pmNoSystemRabReleaseCs64)),
            
            DROP_PS_E = as.numeric(sum(pmNoSystemRabReleasePacket)),
            
            RTWP_E =  -112 +  ((0.1)*as.numeric(sum(pmSUMUlRssi)/sum(pmSamplesUlRssi))),
            
            PAGING_E = as.numeric(sum(pmNoPagingAttemptUtranRejected)),
            
            PAGING_DEN_E =  as.numeric(sum(pmNoPagingType1AttemptCs))+ as.numeric(sum(pmNoPagingType1AttemptPs))
            
          )
          
          dataN <- dataN %>% summarise(
            
            ACC_NUM_CS_N = as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),
            
            ACC_DEN_CS_N = as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)),
            
            ACC_NUM_PS_N = as.numeric(as.numeric(sum(NUM_ACC_PS))* as.numeric(sum(MULT_NUM1))),
            
            ACC_DEN_PS_N = as.numeric(as.numeric(sum(DEN_ACC_PS))* as.numeric(sum(MULT_NUM2))),
            
            RET_NUM_CS_N = as.numeric(sum(NUM_RET_CS)),
            
            RET_DEN_CS_N = as.numeric(sum(DEN_RET_CS)),
            
            RET_NUM_PS_N = as.numeric(sum(NUM_RET_PS)) ,
            
            RET_DEN_PS_N = as.numeric(sum(DEN_RET_PS)),
            
            TRAFICO_CS_N = round(as.numeric(sum(as.numeric(AVG_RAB_HLD_TM_CS_VOICE) ))/360000,3),
            
            VOL_N = round(as.numeric(sum(VOL_DL)+sum(VOL_UL))/1000000000,3),
            
            CONG_CE_RRC_NUM_N = as.numeric(sum(RRC_CONN_STP_FAIL_BTS)),
            
            CONG_CE_RRC_DEN_N = as.numeric(sum(RRC_CONN_STP_ATT)),
            
            CONG_PWR_RRC_NUM_N = as.numeric(sum(RRC_CONN_STP_FAIL_AC_UL)+sum(RRC_CONN_STP_FAIL_AC_DL)),
            
            CONG_PWR_RRC_DEN_N = as.numeric(sum(sum(RRC_CONN_STP_ATT))),
            
            CONG_CODE_RRC_NUM_N = as.numeric(sum(RRC_CONN_STP_FAIL_AC_COD)),
            
            CONG_CODE_RRC_DEN_N = as.numeric(sum(RRC_CONN_STP_ATT)),
            
            CONG_CE_RAB_NUM_N = as.numeric(sum(RAB_STP_FAIL_CS_VOICE_BTS)),
            
            CONG_CE_RAB_DEN_N = as.numeric(sum(RAB_STP_COMP_CS_VOICE)),
            
            CONG_PWR_RAB_NUM_N = as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_UL)+sum(RAB_STP_FAIL_CS_VOICE_AC_DL) + sum(PS_SETUP_FAIL_AC_DL_NRT) + sum(PS_SETUP_FAIL_AC_UL_NRT)),
            
            CONG_PWR_RAB_DEN_N =as.numeric(sum(RAB_STP_COMP_CS_VOICE) + sum(PS_RAB_ATT)),
            
            CONG_CODE_RAB_NUM_N = as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_COD)+ sum(PS_SETUP_FAIL_AC_COD_NRT)),
            
            CONG_CODE_RAB_DEN_N = as.numeric(sum(RAB_STP_COMP_CS_VOICE)+ sum(PS_RAB_ATT)),
            
            DROP_CS_N = round(as.numeric(sum(NUM_RET_CS)),3),
            
            DROP_PS_N = round(as.numeric(sum(NUM_RET_PS)),3),
            
            RTWP_N = round(10 * log10(as.numeric(as.numeric(10^(-11)*sum(RTWP_CLASS_0)) + as.numeric(10^(-10.75)*sum(RTWP_CLASS_1)) +as.numeric(10^(-10.65)*sum(RTWP_CLASS_2)) +as.numeric(10^(-10.55)*sum(RTWP_CLASS_3)) +as.numeric(10^(-10.45)*sum(RTWP_CLASS_4))
                                                 +as.numeric(10^(-10.25)*sum(RTWP_CLASS_5)) + as.numeric(10^(-10.25)*sum(RTWP_CLASS_6)) +as.numeric(10^(-10.15)*sum(RTWP_CLASS_7)) +as.numeric(10^(-10.05)*sum(RTWP_CLASS_8)) +as.numeric(10^(-9.95)*sum(RTWP_CLASS_9)) 
                                                 +as.numeric(10^(-9.85)*sum(RTWP_CLASS_10)) + as.numeric(10^(-9.70)*sum(RTWP_CLASS_11)) +as.numeric(10^(-9.50)*sum(RTWP_CLASS_12)) +as.numeric(10^(-9.30)*sum(RTWP_CLASS_13)) +as.numeric(10^(-9.05)*sum(RTWP_CLASS_14)) 
                                                 +as.numeric(10^(-8.75)*sum(RTWP_CLASS_15)) + as.numeric(10^(-8.45)*sum(RTWP_CLASS_16)) +as.numeric(10^(-8.15)*sum(RTWP_CLASS_17)) +as.numeric(10^(-7.75)*sum(RTWP_CLASS_18)) +as.numeric(10^(-7.25)*sum(RTWP_CLASS_19))
                                                 +as.numeric(10^(-6.75)*sum(RTWP_CLASS_20)) + as.numeric(10^(-6.50)*sum(RTWP_CLASS_21)) )/
                                        as.numeric( sum(RTWP_CLASS_0) +sum(RTWP_CLASS_1)+sum(RTWP_CLASS_2)+sum(RTWP_CLASS_3)+sum(RTWP_CLASS_4)+sum(RTWP_CLASS_5)+sum(RTWP_CLASS_6)+sum(RTWP_CLASS_7)+sum(RTWP_CLASS_8)+sum(RTWP_CLASS_9)+sum(RTWP_CLASS_10)+
                                                      +sum(RTWP_CLASS_11)+sum(RTWP_CLASS_12)+sum(RTWP_CLASS_13)+sum(RTWP_CLASS_14)+sum(RTWP_CLASS_15)+sum(RTWP_CLASS_16)+sum(RTWP_CLASS_17)+sum(RTWP_CLASS_18)+sum(RTWP_CLASS_19)+sum(RTWP_CLASS_20)+sum(RTWP_CLASS_21))
            ),2),
            
            PAGING_N = round(as.numeric(sum(as.numeric(FAIL_PAG_NO_RESP_URA_PCH)) +sum(as.numeric(FAIL_PAG_NO_RESP_CELL_PCH))),3),
            
            PAGING_DEN_N = as.numeric(sum(PAGING_OCCASION_CELL_PCH) + sum(PAGING_OCCASION_URA_PCH) )
          )
          
          dataH[is.na(dataH)] <- 0
          dataE[is.na(dataE)] <- 0
          dataN[is.na(dataN)] <- 0
          
          switch (group,
                  "day"  =  {datosRed <- dataH %>%  inner_join(dataE)%>% inner_join(dataN)%>%group_by(DIA)},
                  
                  "cont" = {datosRed <- dataH %>%  inner_join(dataE)%>% inner_join(dataN)%>%group_by(DIA,RNC)},
                  
                  "prov" = {datosRed <- dataH %>%  inner_join(dataE)%>% inner_join(dataN)%>%group_by(DIA,PROVINCIA)}
          )
          
          return(list("datosHua"= dataH, "datosEri"=dataE,"datosRed"= datosRed,"datosNok"=dataN ))
  
}

FinalFrame3G <- function(data,group){
  
  switch (group,
          "day" = {datosRed <- data$datosRed %>% group_by(DIA)
          datosHua <- data$datosHua %>% group_by(DIA)
          datosEri <- data$datosEri %>% group_by(DIA)
          datosNok <- data$datosNok %>% group_by(DIA)
          
          datosT <- datosRed %>% summarise(
            ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H)+sum(ACC_NUM_CS_E) + sum(ACC_NUM_CS_N))/(sum(ACC_DEN_CS_H)+sum(ACC_DEN_CS_E) + sum(ACC_DEN_CS_N))),2),
            
            ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H)+sum(ACC_NUM_PS_E) + sum(ACC_NUM_PS_N))/(sum(ACC_DEN_PS_H)+sum(ACC_DEN_PS_E) + sum(ACC_DEN_PS_N))),2),
            
            RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H)+sum(RET_NUM_CS_E) + sum(RET_NUM_CS_N))/(sum(RET_DEN_CS_H)+sum(RET_DEN_CS_E) + sum(RET_DEN_CS_N))),2),
            
            RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H)+sum(RET_NUM_PS_E) + sum(RET_NUM_PS_N))/(sum(RET_DEN_PS_H)+sum(RET_DEN_PS_E) + sum(RET_DEN_PS_N))),2),
            
            TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_H)+sum(TRAFICO_CS_E)+sum(TRAFICO_CS_N)),3),
            
            VOL = round(as.numeric(sum(VOL_H)+sum(VOL_E)+sum(VOL_N)),3),
            
            CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_H)+sum(CONG_CE_RRC_NUM_E) + sum(CONG_CE_RRC_NUM_N))/(sum(CONG_CE_RRC_DEN_H)+sum(CONG_CE_RRC_DEN_E) + sum(CONG_CE_RRC_DEN_N))),2),
            
            CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_H)+sum(CONG_PWR_RRC_NUM_E) + sum(CONG_PWR_RRC_NUM_N))/(sum(CONG_PWR_RRC_DEN_H)+sum(CONG_PWR_RRC_DEN_E) + sum(CONG_PWR_RRC_DEN_N))),2),
            
            CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_H)+sum(CONG_CODE_RRC_NUM_E) + sum(CONG_CODE_RRC_NUM_N))/(sum(CONG_CODE_RRC_DEN_H)+sum(CONG_CODE_RRC_DEN_E) + sum(CONG_CODE_RRC_DEN_N))),2),
            
            CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_H)+sum(CONG_CE_RAB_NUM_E) + sum(CONG_CE_RAB_NUM_N))/(sum(CONG_CE_RAB_DEN_H)+sum(CONG_CE_RAB_DEN_E) + sum(CONG_CE_RAB_DEN_N))),2),
            
            CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_H)+sum(CONG_PWR_RAB_NUM_E) + sum(CONG_PWR_RAB_NUM_N))/(sum(CONG_PWR_RAB_DEN_H)+sum(CONG_PWR_RAB_DEN_E) + sum(CONG_PWR_RAB_DEN_N))),2),
            
            CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_H)+sum(CONG_CODE_RAB_NUM_E) + sum(CONG_CODE_RAB_NUM_N))/(sum(CONG_CODE_RAB_DEN_H)+sum(CONG_CODE_RAB_DEN_E) + sum(CONG_CODE_RAB_DEN_N))),2),
            
            DROP_CS =   round(as.numeric((sum(DROP_CS_H)+sum(DROP_CS_E) + sum(DROP_CS_N))),2),
            
            DROP_PS =   round(as.numeric((sum(DROP_PS_H)+sum(DROP_PS_E) + sum(DROP_PS_N))),2),
            
            PAGING =   round(as.numeric((sum(PAGING_H) + sum(PAGING_E) + sum(PAGING_N))),2),

            PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H) + sum(PAGING_E) + sum(PAGING_N))/as.numeric(sum(PAGING_DEN_H) + sum(PAGING_DEN_E) + sum(PAGING_DEN_N)),2) 
            
          )  
          
          datosHua <- datosHua %>% summarise(
            ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H))/(sum(ACC_DEN_CS_H))),2),
            
            ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H))/(sum(ACC_DEN_PS_H))),2),
            
            RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H))/(sum(RET_DEN_CS_H))),2),
            
            RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H))/(sum(RET_DEN_PS_H))),2),
            
            TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_H)),3),
            
            VOL = round(as.numeric(sum(VOL_H)),3),
            
            CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_H))/(sum(CONG_CE_RRC_DEN_H))),2),
            
            CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_H))/(sum(CONG_PWR_RRC_DEN_H))),2),
            
            CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_H))/(sum(CONG_CODE_RRC_DEN_H))),2),
            
            CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_H))/(sum(CONG_CE_RAB_DEN_H))),2),
            
            CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_H))/(sum(CONG_PWR_RAB_DEN_H))),2),
            
            CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_H))/(sum(CONG_CODE_RAB_DEN_H))),2),
            
            DROP_CS =   round(as.numeric((sum(DROP_CS_H))),2),
            
            DROP_PS =   round(as.numeric((sum(DROP_PS_H))),2),
            
            PAGING =   round(as.numeric((sum(PAGING_H))),2),
            
            PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H))/as.numeric(sum(PAGING_DEN_H)),2) 
            
          ) 
          
          datosHua[is.na(datosHua)] <- 0
          #datosHua$RET = ifelse(datosHua$RET == "-Inf", 0, datosHua$RET)
          
          datosEri <- datosEri %>% summarise(
            
            ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_E))/(sum(ACC_DEN_CS_E))),2),
            
            ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_E))/(sum(ACC_DEN_PS_E))),2),
            
            RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_E))/(sum(RET_DEN_CS_E))),2),
            
            RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_E))/(sum(RET_DEN_PS_E))),2),
            
            TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_E)),3),
            
            VOL = round(as.numeric(sum(VOL_E)),3),
            
            CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_E))/(sum(CONG_CE_RRC_DEN_E))),2),
            
            CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_E))/(sum(CONG_PWR_RRC_DEN_E))),2),
            
            CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_E))/(sum(CONG_CODE_RRC_DEN_E))),2),
            
            CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_E))/(sum(CONG_CE_RAB_DEN_E))),2),
            
            CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_E))/(sum(CONG_PWR_RAB_DEN_E))),2),
            
            CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_E))/(sum(CONG_CODE_RAB_DEN_E))),2),
            
            DROP_CS =   round(as.numeric((sum(DROP_CS_E))),2),
            
            DROP_PS =   round(as.numeric((sum(DROP_PS_E))),2),
            
            PAGING =   round(as.numeric((sum(PAGING_E))),2),
            
            PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_E))/as.numeric(sum(PAGING_DEN_E)),2) 
            
          )  
          
          datosEri[is.na(datosEri)] <- 0
          
          datosNok <- datosNok %>% summarise(
            
            ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_N))/(sum(ACC_DEN_CS_N))),2),
            
            ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_N))/(sum(ACC_DEN_PS_N))),2),
            
            RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_N))/(sum(RET_DEN_CS_N))),2),
            
            RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_N))/(sum(RET_DEN_PS_N))),2),
            
            TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_N)),3),
            
            VOL = round(as.numeric(sum(VOL_N)),3),
            
            CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_N))/(sum(CONG_CE_RRC_DEN_N))),2),
            
            CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_N))/(sum(CONG_PWR_RRC_DEN_N))),2),
            
            CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_N))/(sum(CONG_CODE_RRC_DEN_N))),2),
            
            CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_N))/(sum(CONG_CE_RAB_DEN_N))),2),
            
            CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_N))/(sum(CONG_PWR_RAB_DEN_N))),2),
            
            CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_N))/(sum(CONG_CODE_RAB_DEN_N))),2),
            
            DROP_CS =   round(as.numeric((sum(DROP_CS_N))),2),
            
            DROP_PS =   round(as.numeric((sum(DROP_PS_N))),2),
            
            PAGING =   round(as.numeric((sum(PAGING_N))),2),
            
            PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_N))/as.numeric(sum(PAGING_DEN_N)),2)
          )  
          
          datosNok[is.na(datosNok)] <- 0
          
          },
          
          "prov" = {
            
            datosHua <- data$datosHua %>% group_by(DIA,PROVINCIA)
            datosEri <- data$datosEri %>% group_by(DIA,PROVINCIA)
            datosNok <- data$datosNok %>% group_by(DIA,PROVINCIA)
            
            
            ##Por total
            habana_eri <- filter(datosEri, PROVINCIA == "La Habana")
            
            habana_hua <- filter(datosHua, PROVINCIA == "La Habana")
            
            ciefue_hua <- filter(datosHua, PROVINCIA == "Cienfuegos")
            
            ciefue_nok <- filter(datosNok, PROVINCIA == "Cienfuegos")
            
            cienfuegos <- ciefue_hua %>% inner_join(ciefue_nok) %>% group_by(DIA,PROVINCIA)
            habana <- habana_hua %>% inner_join(habana_eri) %>% group_by(DIA,PROVINCIA)
            
            habana <- habana %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H)+sum(ACC_NUM_CS_E) )/(sum(ACC_DEN_CS_H)+sum(ACC_DEN_CS_E) )),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H)+sum(ACC_NUM_PS_E) )/(sum(ACC_DEN_PS_H)+sum(ACC_DEN_PS_E) )),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H)+sum(RET_NUM_CS_E) )/(sum(RET_DEN_CS_H)+sum(RET_DEN_CS_E) )),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H)+sum(RET_NUM_PS_E) )/(sum(RET_DEN_PS_H)+sum(RET_DEN_PS_E) )),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_H)+sum(TRAFICO_CS_E)),3),
              
              VOL = round(as.numeric(sum(VOL_H)+sum(VOL_E)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_H)+sum(CONG_CE_RRC_NUM_E) )/(sum(CONG_CE_RRC_DEN_H)+sum(CONG_CE_RRC_DEN_E) )),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_H)+sum(CONG_PWR_RRC_NUM_E) )/(sum(CONG_PWR_RRC_DEN_H)+sum(CONG_PWR_RRC_DEN_E))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_H)+sum(CONG_CODE_RRC_NUM_E) )/(sum(CONG_CODE_RRC_DEN_H)+sum(CONG_CODE_RRC_DEN_E) )),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_H)+sum(CONG_CE_RAB_NUM_E))/(sum(CONG_CE_RAB_DEN_H)+sum(CONG_CE_RAB_DEN_E))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_H)+sum(CONG_PWR_RAB_NUM_E) )/(sum(CONG_PWR_RAB_DEN_H)+sum(CONG_PWR_RAB_DEN_E) )),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_H)+sum(CONG_CODE_RAB_NUM_E) )/(sum(CONG_CODE_RAB_DEN_H)+sum(CONG_CODE_RAB_DEN_E) )),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_H)+sum(DROP_CS_E) )),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_H)+sum(DROP_PS_E) )),2),
              
              PAGING =   round(as.numeric((sum(PAGING_H) + sum(PAGING_E) )),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H) + sum(PAGING_E) )/as.numeric(sum(PAGING_DEN_H) + sum(PAGING_DEN_E) ),2) ,
              
              RTWP = round(as.numeric(sum(RTWP_H) + sum(RTWP_E))/2,2) 
              
            ) 
            habana[is.na(habana)] <- 0
            habana <- as.data.frame(habana)
            
            cienfuegos <- cienfuegos %>% summarise(
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H)+sum(ACC_NUM_CS_N) )/(sum(ACC_DEN_CS_H)+sum(ACC_DEN_CS_N) )),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H)+sum(ACC_NUM_PS_N) )/(sum(ACC_DEN_PS_H)+sum(ACC_DEN_PS_N) )),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H)+sum(RET_NUM_CS_N) )/(sum(RET_DEN_CS_H)+sum(RET_DEN_CS_N) )),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H)+sum(RET_NUM_PS_N) )/(sum(RET_DEN_PS_H)+sum(RET_DEN_PS_N) )),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_H)+sum(TRAFICO_CS_N)),3),
              
              VOL = round(as.numeric(sum(VOL_H)+sum(VOL_N)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_H)+sum(CONG_CE_RRC_NUM_N) )/(sum(CONG_CE_RRC_DEN_H)+sum(CONG_CE_RRC_DEN_N) )),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_H)+sum(CONG_PWR_RRC_NUM_N) )/(sum(CONG_PWR_RRC_DEN_H)+sum(CONG_PWR_RRC_DEN_N))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_H)+sum(CONG_CODE_RRC_NUM_N) )/(sum(CONG_CODE_RRC_DEN_H)+sum(CONG_CODE_RRC_DEN_N) )),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_H)+sum(CONG_CE_RAB_NUM_N))/(sum(CONG_CE_RAB_DEN_H)+sum(CONG_CE_RAB_DEN_N))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_H)+sum(CONG_PWR_RAB_NUM_N) )/(sum(CONG_PWR_RAB_DEN_H)+sum(CONG_PWR_RAB_DEN_N) )),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_H)+sum(CONG_CODE_RAB_NUM_N) )/(sum(CONG_CODE_RAB_DEN_H)+sum(CONG_CODE_RAB_DEN_N) )),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_H)+sum(DROP_CS_N) )),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_H)+sum(DROP_PS_N) )),2),
              
              PAGING =   round(as.numeric((sum(PAGING_H) + sum(PAGING_N) )),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H) + sum(PAGING_N) )/as.numeric(sum(PAGING_DEN_H) + sum(PAGING_DEN_N) ),2) ,
              
              RTWP = round(as.numeric(sum(RTWP_H) + sum(RTWP_N))/2,2) 
            )
            cienfuegos[is.na(cienfuegos)] <- 0
            cienfuegos <- as.data.frame(cienfuegos)
            
            ##Por Provincia
            datosHua <- datosHua %>% summarise(
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H))/(sum(ACC_DEN_CS_H))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H))/(sum(ACC_DEN_PS_H))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H))/(sum(RET_DEN_CS_H))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H))/(sum(RET_DEN_PS_H))),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_H)),3),
              
              VOL = round(as.numeric(sum(VOL_H)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_H))/(sum(CONG_CE_RRC_DEN_H))),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_H))/(sum(CONG_PWR_RRC_DEN_H))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_H))/(sum(CONG_CODE_RRC_DEN_H))),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_H))/(sum(CONG_CE_RAB_DEN_H))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_H))/(sum(CONG_PWR_RAB_DEN_H))),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_H))/(sum(CONG_CODE_RAB_DEN_H))),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_H))),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_H))),2),
              
              PAGING =   round(as.numeric((sum(PAGING_H))),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H))/as.numeric(sum(PAGING_DEN_H)),2) ,
              
              RTWP = round(as.numeric(sum(RTWP_H)),2)
              
            ) 
            
            datosHua[is.na(datosHua)] <- 0
            # datosHua = ifelse(datosHua == "-Inf", 0, datosHua)
            
            datosHua <- as.data.frame(datosHua)
            
            datosEri <- datosEri %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_E))/(sum(ACC_DEN_CS_E))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_E))/(sum(ACC_DEN_PS_E))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_E))/(sum(RET_DEN_CS_E))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_E))/(sum(RET_DEN_PS_E))),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_E)),3),
              
              VOL = round(as.numeric(sum(VOL_E)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_E))/(sum(CONG_CE_RRC_DEN_E))),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_E))/(sum(CONG_PWR_RRC_DEN_E))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_E))/(sum(CONG_CODE_RRC_DEN_E))),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_E))/(sum(CONG_CE_RAB_DEN_E))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_E))/(sum(CONG_PWR_RAB_DEN_E))),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_E))/(sum(CONG_CODE_RAB_DEN_E))),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_E))),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_E))),2),
              
              PAGING =   round(as.numeric((sum(PAGING_E))),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_E))/as.numeric(sum(PAGING_DEN_E)),2),
              
              RTWP = round(as.numeric(sum(RTWP_E)),2)
            )  
            datosEri[is.na(datosEri)] <- 0
            datosEri <- as.data.frame(datosEri)
            datosNok <- datosNok %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_N))/(sum(ACC_DEN_CS_N))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_N))/(sum(ACC_DEN_PS_N))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_N))/(sum(RET_DEN_CS_N))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_N))/(sum(RET_DEN_PS_N))),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_N)),3),
              
              VOL = round(as.numeric(sum(VOL_N)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_N))/(sum(CONG_CE_RRC_DEN_N))),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_N))/(sum(CONG_PWR_RRC_DEN_N))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_N))/(sum(CONG_CODE_RRC_DEN_N))),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_N))/(sum(CONG_CE_RAB_DEN_N))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_N))/(sum(CONG_PWR_RAB_DEN_N))),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_N))/(sum(CONG_CODE_RAB_DEN_N))),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_N))),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_N))),2),
              
              PAGING =   round(as.numeric((sum(PAGING_N))),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_N))/as.numeric(sum(PAGING_DEN_N)),2),
              
              RTWP = round(as.numeric(sum(RTWP_N)),2)
            ) 
            datosNok[is.na(datosNok)] <- 0
            datosNok <- as.data.frame(datosNok)
            
            datosEri_T <- filter(datosEri, PROVINCIA != "La Habana", PROVINCIA != "Matanzas")
            datosHua_T <- filter(datosHua, PROVINCIA != "La Habana",PROVINCIA != "Villa Clara",PROVINCIA != "Santi Spiritus",PROVINCIA != "Cienfuegos",PROVINCIA != "Pinar del Rio")
            
            datosEri_T$DIA <- as.factor(datosEri_T$DIA)
            
            
            
            datosT <- rbind(datosHua_T,datosEri_T,habana,cienfuegos)
            
          },
          
          "cont" = {
            datosHua <- data$datosHua %>% group_by(DIA,RNC)
            datosEri <- data$datosEri %>% group_by(DIA,RNC)
            datosNok <- data$datosNok %>% group_by(DIA,RNC)
            
            datosHua <- datosHua %>% summarise(
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H))/(sum(ACC_DEN_CS_H))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H))/(sum(ACC_DEN_PS_H))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H))/(sum(RET_DEN_CS_H))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H))/(sum(RET_DEN_PS_H))),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_H)),3),
              
              VOL = round(as.numeric(sum(VOL_H)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_H))/(sum(CONG_CE_RRC_DEN_H))),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_H))/(sum(CONG_PWR_RRC_DEN_H))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_H))/(sum(CONG_CODE_RRC_DEN_H))),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_H))/(sum(CONG_CE_RAB_DEN_H))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_H))/(sum(CONG_PWR_RAB_DEN_H))),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_H))/(sum(CONG_CODE_RAB_DEN_H))),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_H))),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_H))),2),
              
              PAGING =   round(as.numeric((sum(PAGING_H))),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_H))/as.numeric(sum(PAGING_DEN_H)),2) ,
              
              RTWP = round(as.numeric(sum(RTWP_H)),2)
            ) 
            
            datosHua[is.na(datosHua)] <- 0
            datosHua <- as.data.frame(datosHua)
            
            datosEri <- datosEri %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_E))/(sum(ACC_DEN_CS_E))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_E))/(sum(ACC_DEN_PS_E))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_E))/(sum(RET_DEN_CS_E))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_E))/(sum(RET_DEN_PS_E))),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_E)),3),
              
              VOL = round(as.numeric(sum(VOL_E)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_E))/(sum(CONG_CE_RRC_DEN_E))),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_E))/(sum(CONG_PWR_RRC_DEN_E))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_E))/(sum(CONG_CODE_RRC_DEN_E))),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_E))/(sum(CONG_CE_RAB_DEN_E))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_E))/(sum(CONG_PWR_RAB_DEN_E))),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_E))/(sum(CONG_CODE_RAB_DEN_E))),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_E))),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_E))),2),
              
              PAGING =   round(as.numeric((sum(PAGING_E))),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_E))/as.numeric(sum(PAGING_DEN_E)),2) ,
              
              RTWP = round(as.numeric(sum(RTWP_E)),2)
            )
            
            datosEri[is.na(datosEri)] <- 0
            datosEri <- as.data.frame(datosEri)
            
            datosNok <- datosNok %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_N))/(sum(ACC_DEN_CS_N))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_N))/(sum(ACC_DEN_PS_N))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_N))/(sum(RET_DEN_CS_N))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_N))/(sum(RET_DEN_PS_N))),2),
              
              TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS_N)),3),
              
              VOL = round(as.numeric(sum(VOL_N)),3),
              
              CONG_CE_RRC =   round(100* as.numeric((sum(CONG_CE_RRC_NUM_N))/(sum(CONG_CE_RRC_DEN_N))),2),
              
              CONG_PWR_RRC =   round(100* as.numeric((sum(CONG_PWR_RRC_NUM_N))/(sum(CONG_PWR_RRC_DEN_N))),2),
              
              CONG_CODE_RRC =   round(100* as.numeric((sum(CONG_CODE_RRC_NUM_N))/(sum(CONG_CODE_RRC_DEN_N))),2),
              
              CONG_CE_RAB =   round(100* as.numeric((sum(CONG_CE_RAB_NUM_N))/(sum(CONG_CE_RAB_DEN_N))),2),
              
              CONG_PWR_RAB =   round(100* as.numeric((sum(CONG_PWR_RAB_NUM_N))/(sum(CONG_PWR_RAB_DEN_N))),2),
              
              CONG_CODE_RAB =   round(100* as.numeric((sum(CONG_CODE_RAB_NUM_N))/(sum(CONG_CODE_RAB_DEN_N))),2),
              
              DROP_CS =   round(as.numeric((sum(DROP_CS_N))),2),
              
              DROP_PS =   round(as.numeric((sum(DROP_PS_N))),2),
              
              PAGING =   round(as.numeric((sum(PAGING_N))),2),
              
              PAGING_RATE = round(100 - 100 * as.numeric(sum(PAGING_N))/as.numeric(sum(PAGING_DEN_N)),2) ,
              
              RTWP = round(as.numeric(sum(RTWP_N) ),2)
            )
            
            datosNok[is.na(datosNok)] <- 0
            datosNok <- as.data.frame(datosNok)
            
            datosT <- rbind(datosHua,datosEri,datosNok)
            
          },
          
          "Tot" = {
            
            data$datosRed$TOTAL <- "TOTAL"
            datosRed <- data$datosRed %>% group_by(TOTAL)
            
            datosT <- datosRed %>% summarise(
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H)+sum(ACC_NUM_CS_E) + sum(ACC_NUM_CS_N))/(sum(ACC_DEN_CS_H)+sum(ACC_DEN_CS_E) + sum(ACC_DEN_CS_N))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H)+sum(ACC_NUM_PS_E) + sum(ACC_NUM_PS_N))/(sum(ACC_DEN_PS_H)+sum(ACC_DEN_PS_E) + sum(ACC_DEN_PS_N))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H)+sum(RET_NUM_CS_E) + sum(RET_NUM_CS_N))/(sum(RET_DEN_CS_H)+sum(RET_DEN_CS_E) + sum(RET_DEN_CS_N))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H)+sum(RET_NUM_PS_E) + sum(RET_NUM_PS_N))/(sum(RET_DEN_PS_H)+sum(RET_DEN_PS_E) + sum(RET_DEN_PS_N))),2)
              
            )
            
            datosHua <- 1
            datosEri <- 1
            datosNok <- 1
            
          },
          
          "TotP" = {
            
            
            data1 <- data$datosHua
            
            datosHua <- data$datosHua %>% group_by(PROVINCIA)
            datosEri <- data$datosEri %>% group_by(PROVINCIA)
            datosNok <- data$datosNok %>% group_by(PROVINCIA)
            
            
            ##Por total
            habana_eri <- filter(datosEri, PROVINCIA == "La Habana")
            
            habana_hua <- filter(datosHua, PROVINCIA == "La Habana")
            
            ciefue_hua <- filter(datosHua, PROVINCIA == "Cienfuegos")
            
            ciefue_nok <- filter(datosNok, PROVINCIA == "Cienfuegos")
            
            cienfuegos <- ciefue_hua %>% inner_join(ciefue_nok) %>% group_by(PROVINCIA)
            habana <- habana_hua %>% inner_join(habana_eri) %>% group_by(PROVINCIA)
            
            habana <- habana %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H)+sum(ACC_NUM_CS_E) )/(sum(ACC_DEN_CS_H)+sum(ACC_DEN_CS_E) )),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H)+sum(ACC_NUM_PS_E) )/(sum(ACC_DEN_PS_H)+sum(ACC_DEN_PS_E) )),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H)+sum(RET_NUM_CS_E) )/(sum(RET_DEN_CS_H)+sum(RET_DEN_CS_E) )),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H)+sum(RET_NUM_PS_E) )/(sum(RET_DEN_PS_H)+sum(RET_DEN_PS_E) )),2)
              
              
              
            ) 
            habana[is.na(habana)] <- 0
            habana <- as.data.frame(habana)
            
            cienfuegos <- cienfuegos %>% summarise(
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H)+sum(ACC_NUM_CS_N) )/(sum(ACC_DEN_CS_H)+sum(ACC_DEN_CS_N) )),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H)+sum(ACC_NUM_PS_N) )/(sum(ACC_DEN_PS_H)+sum(ACC_DEN_PS_N) )),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H)+sum(RET_NUM_CS_N) )/(sum(RET_DEN_CS_H)+sum(RET_DEN_CS_N) )),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H)+sum(RET_NUM_PS_N) )/(sum(RET_DEN_PS_H)+sum(RET_DEN_PS_N) )),2)
              
              
            )
            cienfuegos[is.na(cienfuegos)] <- 0
            cienfuegos <- as.data.frame(cienfuegos)
            
            ##Por Provincia
            datosHua <- datosHua %>% summarise(
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_H))/(sum(ACC_DEN_CS_H))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_H))/(sum(ACC_DEN_PS_H))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_H))/(sum(RET_DEN_CS_H))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_H))/(sum(RET_DEN_PS_H))),2)
              
      
            ) 
            
            datosHua[is.na(datosHua)] <- 0
            # datosHua = ifelse(datosHua == "-Inf", 0, datosHua)
            
            datosHua <- as.data.frame(datosHua)
            
            datosEri <- datosEri %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_E))/(sum(ACC_DEN_CS_E))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_E))/(sum(ACC_DEN_PS_E))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_E))/(sum(RET_DEN_CS_E))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_E))/(sum(RET_DEN_PS_E))),2)
              
            )  
            datosEri[is.na(datosEri)] <- 0
            datosEri <- as.data.frame(datosEri)
            datosNok <- datosNok %>% summarise(
              
              ACC_CS = round(100* as.numeric((sum(ACC_NUM_CS_N))/(sum(ACC_DEN_CS_N))),2),
              
              ACC_PS = round(100* as.numeric((sum(ACC_NUM_PS_N))/(sum(ACC_DEN_PS_N))),2),
              
              RET_CS = round(100 - 100* as.numeric((sum(RET_NUM_CS_N))/(sum(RET_DEN_CS_N))),2),
              
              RET_PS = round(100 - 100* as.numeric((sum(RET_NUM_PS_N))/(sum(RET_DEN_PS_N))),2)
              
             
            ) 
            datosNok[is.na(datosNok)] <- 0
            datosNok <- as.data.frame(datosNok)
            
            datosEri_T <- filter(datosEri, PROVINCIA != "La Habana", PROVINCIA != "Matanzas")
            datosHua_T <- filter(datosHua, PROVINCIA != "La Habana",PROVINCIA != "Villa Clara",PROVINCIA != "Santi Spiritus",PROVINCIA != "Cienfuegos",PROVINCIA != "Pinar del Rio")
            
            
            
            datosT <- rbind(datosHua_T,datosEri_T,habana,cienfuegos)
            
          }
  )
  return(list("datosHua"= datosHua, "datosEri"=datosEri,"datosRed"= datosT,"datosNok"=datosNok ))
  
}

cell2G <- function(dataH,dataE,dataN){
  
  dataH[is.na(dataH)] <- 0
  dataE[is.na(dataE)] <- 0
  dataN[is.na(dataN)] <- 0

  datosHua <- dataH %>% group_by(CELDA) %>% summarise(
    ACC = round(100* as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                 *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                 *(sum(K3013A) / sum(K3010A))
    ),3),
    
    RET = round(100 * (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
                         as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353))),3),
    
    SER = round(100 * ((as.numeric( (1-as.numeric(sum(K3001)/sum(K3000)))
                                     *(1 - as.numeric(sum(CM30)/sum(K3003)))
                                     *(sum(K3013A) / sum(K3010A))
    ))
    *
      (as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353) - sum(CM33))/
         as.numeric(sum(K3040) + sum(CH363) - sum(CH303) - sum(CH313) - sum(CH333) - sum(CH353)))),3),
    CONG_TCH =   round(100 * as.numeric(sum(K3011A)/ sum(K3010A)),3),
    CONG_SDCCH =  round(100 * as.numeric(sum(K3001)/ sum(K3000)),3),
    TRAFF_TCH = round(as.numeric(sum(K3014)),3),
    TRAFF_SDCCH = round(as.numeric(sum(K3004)),3),
    CALL_DROP_TCH = sum(CM33),
    CALL_DROP_SDCCH = sum(CM30),
    PAGING_CS = as.numeric(mean(A338)),
    PAGING_PS = as.numeric(mean(A339)),
    TRAFF_TCH_FR = as.numeric(sum(K3014MINUSK3034)) ,
    TRAFF_TCH_HR = as.numeric(sum(K3034)),
    PAGING_RATE = round(100 - 100 * as.numeric(sum(A338 + A339))/as.numeric(sum(A330 + A331)),3),
    DISPONIBILIDAD = as.numeric(0)
  )
  
 datosHua[is.na(datosHua)] <- 0
  
 datosEri <- dataE %>% group_by(CELDA) %>% summarise(
   
   ACC = round(100 * (1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                 (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))),3),  
   RET = round(100 * (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB)))),3),
   
   SER = round( 100*  ((1-as.numeric(sum(CCONGS)/sum(CCALLS))) * (1-as.numeric((sum(CDISSS)+ sum(CDISQA)+sum(CDISTA))/sum(CMSESTAB))) *
                         (as.numeric((sum(TFCASSALL) + sum(TFCASSALLSUB) + sum(THCASSALL) + sum(THCASSALLSUB))/sum(TASSALL))))
                *
                  (1- as.numeric((sum(TFNDROP)+sum(THNDROP)+sum(TFNDROPSUB)+ sum(THNDROPSUB))/(sum(TFMSESTB)+sum(THMSESTB))))
                ,3),
   CONG_TCH = 100 * round(  as.numeric(as.numeric( sum(CNRELCONG) + sum(TFNRELCONG) + sum(THNRELCONG) + sum(TFNRELCONGSUB) + sum(THNRELCONGSUB) )/ as.numeric(sum(TASSALL))),3),
   CONG_SDCCH= 100* round( as.numeric(sum(CCONGS)) / as.numeric(sum(CCALLS)),3),
   TRAFF_TCH = round(sum(as.numeric(TFTRALACC)/as.numeric(TFNSCAN) + as.numeric(THTRALACC)/as.numeric(THNSCAN)),2),
   TRAFF_SDCCH = round(sum(as.numeric(CTRALACC)/as.numeric(CNSCAN)),2),
   CALL_DROP_TCH = sum(DROP_TCH),
   CALL_DROP_SDCCH = sum(DROP_SDCCH),
   PAGING_CS = as.numeric(mean(PAGPCHCONGCS+PAGETOOOLDCS)),
   PAGING_PS = as.numeric(mean(PAGPCHCONGPS+PAGETOOOLDPS)),
   TRAFF_TCH_FR = round(sum(as.numeric(TFTRALACC)/as.numeric(TFNSCAN)),2),
   TRAFF_TCH_HR = round(sum(as.numeric(THTRALACC)/as.numeric(THNSCAN)),2),
   PAGING_RATE = round(100 - 100 * (as.numeric(sum(PAGETOOOLD+PAGPCHCONG))/as.numeric(sum(PAGESRECCS+PAGESRECPS))),2),
   INT4_5P = round(as.numeric((sum(ITFUSIB5) + sum(ITFUSIB4))/sum(NOACCUF)),2),
   DISPONIBILIDAD = as.numeric(0)
   
  
   
 )
 
 datosEri[is.na(datosEri)] <- 0
 
  datosNok <- dataN %>% group_by(CELDA) %>% summarise(
    
    ACC = round((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                  (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  )) *
                  (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000,3),
    RET = round(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))),3),
    SER = round(as.numeric((100 - 100* as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) - sum(SUCC_TCH_SEIZ_CALL_ATTEMPT)  )/(as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT) )))*
                             (100 - 100*as.numeric(sum(SDCCH_DROP_CALL_AND_HO) )/as.numeric(sum(SDCCH_ALLOC_FOR_VOICE_CALL)  ))*
                             (100 - 100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)))/10000)
                *as.numeric(100-(100 * as.numeric(sum(DROP_AFTER_TCH_ASSIGN) )/ (as.numeric(sum(TCH_NEW_CALL_ASSIGN) ) + as.numeric(sum(TCH_RE_EST_ASSIGN) ) + as.numeric(sum(TCH_HO_ASSIGN) ) - as.numeric(sum(TCH_HO_RELEASE) ))) )/100,3),
    CONG_TCH = round(100 * as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)- sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) )/as.numeric(sum(TCH_REQUESTS_CALL_ATTEMPT)),2),
    CONG_SDCCH = round(100 * as.numeric(sum(SDCCH_BUSY_ATT)- sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) )/as.numeric(sum(SDCCH_SEIZ_ATT)),2),
    TRAFF_TCH =round(24 * sum( as.numeric(AVE_BUSY_TCH)/as.numeric(RES_AV_DENOM14)),3),
    TRAFF_SDCCH = round(24 * as.numeric(sum(AVE_BUSY_SDCCH))/as.numeric(sum(RES_AV_DENOM15) ),3),
    CALL_DROP_TCH = as.numeric(sum(DROP_AFTER_TCH_ASSIGN)),
    CALL_DROP_SDCCH = as.numeric(sum(DROP_SDCCH) - sum(T3101_EXPIRED)),
    PAGINGS = as.numeric(mean(DELETE_PAGING_COMMAND)),
    TRAFF_TCH_FR = round(as.numeric(sum(AVE_TCH_BUSY_FULL)) + (2*as.numeric(sum(AVE_BUSY_DFR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
    TRAFF_TCH_HR = round(as.numeric(sum(AVE_TCH_BUSY_HALF)) + (2*as.numeric(sum(AVE_BUSY_DHR_TCH))/as.numeric(sum(AVE_BUSY_DHR_TCH_DENOM)) /2),3),
    PAGING_RATE = round(100 - 100 * as.numeric(sum(DELETE_PAGING_COMMAND))/as.numeric(sum(PAGING_MSG_SENT)),2),
    INT4_5P = round(100 * as.numeric(as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+ as.numeric(sum(AVE_IDLE_F_TCH_5)/sum(RES_AV_DENOM8)) ) / as.numeric(  as.numeric(sum(AVE_IDLE_F_TCH_1)/(sum(RES_AV_DENOM4)) )+ 
                                                                                                                                                                      as.numeric(sum(AVE_IDLE_F_TCH_2)/(sum(RES_AV_DENOM5)) )+
                                                                                                                                                                      as.numeric(sum(AVE_IDLE_F_TCH_3)/(sum(RES_AV_DENOM6)) )+
                                                                                                                                                                      as.numeric(sum(AVE_IDLE_F_TCH_4)/(sum(RES_AV_DENOM7)) )+
                                                                                                                                                                      as.numeric(sum(AVE_IDLE_F_TCH_5)/(sum(RES_AV_DENOM8)) )),3),
    DISPONIBILIDAD = as.numeric(0)
    
    
  )
  datosNok[is.na(datosNok)] <- 0
  
  str(datosHua)
  str(datosEri)
  str(datosNok)
  
  return(list("datosHua"= datosHua, "datosEri"=datosEri,"datosNok"=datosNok ))
  
}

cell3G <- function(dataH,dataE,dataN){
  
  dataH[is.na(dataH)] <- 0
  dataE[is.na(dataE)] <- 0
  dataN[is.na(dataN)] <- 0
  
  datosHua <- dataH %>% group_by(CELDA) %>% summarise(
    ACC_CS =  round( 100  *(as.numeric(as.numeric(sum(NUM_AMR_RAB)) * as.numeric(sum(NUM_CS_RRC)))  ) / 
                       (as.numeric(as.numeric(sum(DEN_CS_RRC)) * as.numeric(sum(DEN_AMR_RAB)))) ,3),
    
    ACC_PS = round( 100  *(as.numeric(as.numeric(sum(NUM_PS_RAB)) * as.numeric(sum(NUM_PS_RRC))))  / 
                      (as.numeric(as.numeric(sum(DEN_PS_RAB)) * as.numeric(sum(DEN_PS_RRC)))),3),
    
    RET_CS = 100 - round(100 * (as.numeric(as.numeric(sum(NUM_AMR_CALL_DROP)) / as.numeric(sum(DEN_AMR_CALL_DROP)))) ,3 ),
    
    RET_PS= 100 -  100 * round((as.numeric(as.numeric(sum(PS_DROP_FD_NUM))/as.numeric(sum(PS_DROP_FD_DEN)))),3),
    
    CONG_CE_RRC = round ( as.numeric(sum(CE_CONG_RRC) ),3),
    
    CONG_PWR_RRC = round ( as.numeric(sum(PWR_CONG_RRC)),3),
    
    CONG_CODE_RRC = round (as.numeric(sum(CODE_CONG_RRC)),3),
    
    CONG_CE_RAB = round ( as.numeric(sum(CE_PS_CONG_RAB) + sum(CE_CS_CONG_RAB)),3),
    #
    CONG_PWR_RAB = round (as.numeric(sum(PWR_PS_CONG_RAB) + sum(PWR_CS_CONG_RAB)),3),
    #
    CONG_CODE_RAB = round (as.numeric(sum(CODE_PS_CONG_RAB) + sum(CODE_CS_CONG_RAB)),3),
    
    TRAFICO_CS = round(as.numeric(sum(TRAFICO_CS)),3),
    
    VOLUMEN_DATOS = round(as.numeric(sum(TOTAL_VOLUMEN_HSUPA) + sum(TOTAL_VOLUMEN_HSDPA)  )/1000000000,3),
    
    DROP_CS = as.numeric(sum(CS_DROP_A)),
    
    DROP_PS = as.numeric(sum(PS_DROP_A)),
    
    PAGING_DISCARD = round(as.numeric(sum(PAGING_DISCARD)),0),
    
    PAGING_RATE = 100 * round(as.numeric(sum(PAGING_DISCARD)/sum(PAGING_SENT)),2),
    
    MAX_TROUGHPUT_HSDPA = round(as.numeric(max(MAX_TROUGHPUT_HSDPA)),3),
    
    AVG_TROUGHPUT_HSDPA = round(as.numeric(mean(AVG_TROUGHPUT_HSDPA)),3),
    
    MAX_TROUGHPUT_HSUPA = round(as.numeric(max(MAX_TROUGHPUT_HSUPA)),3),
    
    AVG_TROUGHPUT_HSUPA = round(as.numeric(mean(AVG_TROUGHPUT_HSUPA)),3),
    
    RTWP = round(mean(as.numeric(RTWP),na.rm = TRUE),3),
    
    MAX_USER_HSDPA = round(as.numeric(sum(MAX_USER_HSDPA)),3),
    
    MAX_USER_HSUPA = round(as.numeric(sum(MAX_USER_HSUPA)),3),
    
    #AVG_USER_HSDPA = round(as.numeric(sum(AVG_USER_HSDPA)),3),
    
    #AVG_USER_HSUPA = round(as.numeric(sum(AVG_USER_HSUPA)),3),
    
    DISPONIBILIDAD = as.numeric(0)
  )
  
  datosHua[is.na(datosHua)] <- 0
  
  datosEri <- dataE %>% group_by(CELDA) %>% summarise(
    
    ACC_CS = round(100 * as.numeric(as.numeric(sum(as.numeric(pmTotNoRrcConnectReqCsSucc)  )/(sum(as.numeric(pmTotNoRrcConnectReqCs)  )-sum(as.numeric(pmNoLoadSharingRrcConnCs) )))    *    as.numeric((sum(as.numeric(pmRabEstablishEcSuccess) )+sum(as.numeric(pmNoRabEstablishSuccessSpeech) )+sum(as.numeric(pmNoRabEstablishSuccessCs64) )) 
                                                                                                                                                                                                        /(sum(as.numeric(pmRabEstablishEcAttempt) ) + sum(as.numeric(pmNoRabEstablishAttemptSpeech) ) + sum(as.numeric(pmNoRabEstablishAttemptCs64) ) - sum(as.numeric(pmNoDirRetryAtt) )))),3),
    
    ACC_PS = round(100 * as.numeric(as.numeric(sum(as.numeric(pmTotNoRrcConnectReqPsSucc) )/(sum(as.numeric(pmTotNoRrcConnectReqPs) ) - sum(as.numeric(pmNoLoadSharingRrcConnPs) ))) * 
                                      as.numeric(sum(pmNoRabEstablishSuccessPacketInteractive)/sum(pmNoRabEstablishAttemptPacketInteractive))),3),
    
    
    RET_CS = 100 -  round(100*(as.numeric(sum(as.numeric(pmNoSystemRabReleaseSpeech) ) + sum(as.numeric(pmNoSystemRabReleaseCs64) )) / 
                                 as.numeric(sum(as.numeric(pmNoNormalRabReleaseSpeech) ) + sum(as.numeric(pmNoNormalRabReleaseCs64) ) + sum(as.numeric(pmNoSystemRabReleaseSpeech) ) + sum(as.numeric(pmNoSystemRabReleaseCs64) )) ),3),
    
    RET_PS = round(100 - 100 * as.numeric(as.numeric( sum(as.numeric(pmNoSystemRabReleasePacket )- as.numeric(pmNoSystemRabReleasePacketUra) )  - sum(as.numeric(pmChSwitchAttemptFachUra) -as.numeric( pmChSwitchSuccFachUra) ) - sum(as.numeric(pmChSwitchAttemptDchUra) - as.numeric(pmChSwitchSuccDchUra) ) - sum(as.numeric(pmChSwitchAttemptHsUra) - as.numeric(pmChSwitchSuccHsUra) ))
                                          / as.numeric(sum(as.numeric(pmNoNormalRabReleasePacket) ) - sum(as.numeric(pmNoNormalRabReleasePacketUra) ) +  sum(as.numeric(pmNoSystemRabReleasePacket) ) + sum(as.numeric(pmNoSystemRabReleasePacketUra) ) 
                                                       + sum(as.numeric(pmChSwitchSuccFachUra) ) + sum(as.numeric(pmChSwitchSuccDchUra) )  + sum(as.numeric(pmChSwitchSuccHsUra) )) ),3),
    
    CONG_CE_RRC = as.numeric(sum(pmNoRrcReqDeniedAdmDlHw) + sum(pmNoRrcReqDeniedAdmUlHw)),
    
    CONG_PWR_RRC =  as.numeric(sum(pmNoRrcReqDeniedAdmDlPwr)),
    
    CONG_CODE_RRC =  as.numeric(sum(pmNoRrcReqDeniedAdmDlChnlCode)),
    
    CONG_CE_RAB =  as.numeric(sum(pmNoFailedRabEstAttemptLackDlHw) + sum(pmNoFailedRabEstAttemptLackUlHw)),
    
    #
    CONG_PWR_RAB =  as.numeric(sum(pmNoFailedRabEstAttemptLackDlPwr)),
    
    
    CONG_CODE_RAB = as.numeric(sum(pmNoFailedRabEstAttemptLackDlChnlCode)), 
    
    TRAFF_CS_ERL = round(24 * as.numeric(sum(pmSumBestCs12Establish/pmSamplesBestCs12Establish)),2),
    
    VOLUMEN_DATOS =  round(as.numeric(sum(as.numeric(pmDlTrafficVolumePsIntHs) ) + sum(as.numeric(pmUlTrafficVolumePsIntEul) ) + sum(as.numeric(VOLUMEN_R99_UL) ) + sum(as.numeric(VOLUMEN_R99_DL) )  )/ (8*1000000),3),
    
    DROP_CS = as.numeric(sum(as.numeric( pmNoSystemRabReleaseSpeech )+ as.numeric(pmNoSystemRabReleaseCs64) )),
    
    DROP_PS = as.numeric(sum(as.numeric(pmNoSystemRabReleasePacket))),
    
    PAGING_DISCARD = as.numeric(mean(pmNoPagingAttemptUtranRejected)),
    
    PAGING_RATE =  round(100 - 100 * as.numeric(sum(pmNoPagingAttemptUtranRejected))/as.numeric(sum(pmNoPagingType1AttemptCs+pmNoPagingType1AttemptPs)),2),
    
    MAX_TROUGHPUT_HSDPA = round(as.numeric(max(0)),3),
    
    AVG_TROUGHPUT_HSDPA = ((sum(as.numeric(pmDlTrafficVolumePsIntHs))/
                                           ((sum(as.numeric(pmSumPsHsAdchRabEstablish))/sum(as.numeric(pmSamplesPsHsAdchRabEstablish)))+
                                              (sum(as.numeric(pmSumPsEulRabEstablish))/sum(as.numeric(pmSamplesPsEulRabEstablish))))) /
                                          (sum(as.numeric(pmSamplesBestCs12Establish))*5)),
    
    MAX_TROUGHPUT_HSUPA = round(as.numeric(0),3),
    
    AVG_TROUGHPUT_HSUPA = round(as.numeric(0),3),
    
    RTWP = -112 +  ((0.1)*as.numeric(sum(pmSUMUlRssi)/sum(pmSamplesUlRssi))),
    
    MAX_USER_HSDPA = round(as.numeric(max(pmSumPsHsAdchRabEstablish/pmSamplesPsHsAdchRabEstablish)),3),
    
    MAX_USER_HSUPA = round(as.numeric(max(pmSumPsEulRabEstablish/pmSamplesPsEulRabEstablish)),3),
    
    AVG_USER_HSDPA = round(as.numeric(sum(pmSumPsHsAdchRabEstablish/pmSamplesPsHsAdchRabEstablish)),3),
    
    AVG_USER_HSUPA = round(as.numeric(sum(pmSumPsEulRabEstablish/pmSamplesPsEulRabEstablish)),3),
    
    DISPONIBILIDAD = 100 * round( 1 - as.numeric(sum(pmCellDowntimeAuto) + sum(pmCellDowntimeMan))/86400,3)
    
  )
  
  datosEri[is.na(datosEri)] <- 0
  
  datosNok <- dataN %>% group_by(CELDA) %>% summarise(
    
    ACC_CS = round(100* as.numeric(sum(ACC_CS_N) + sum(CELL_UPDATE_SUCC_CS_CALL)) /as.numeric(sum(ACC_CS_D) + sum(CELL_UPDATE_ATT_CS_CALL)) * as.numeric(sum(RAB_CS_VOICE)/sum(RAB_STP_VOICE)),3),
    
    ACC_PS = round(100 * as.numeric(sum(NUM_ACC_PS)/sum(DEN_ACC_PS))*as.numeric(sum(MULT_NUM1)/sum(MULT_NUM2)),3),
    
    RET_CS = round(100 -  as.numeric(100 * as.numeric(sum(NUM_RET_CS))/as.numeric(sum(DEN_RET_CS))),3),
    
    RET_PS = round(100 - as.numeric(100 * as.numeric(sum(NUM_RET_PS))/as.numeric(sum(DEN_RET_PS))),3),
    
    CONG_CE_RRC = round(100* as.numeric(sum(RRC_CONN_STP_FAIL_BTS))/as.numeric(sum(RRC_CONN_STP_ATT)),3),
    
    CONG_PWR_RRC =  round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_UL)+sum(RRC_CONN_STP_FAIL_AC_DL) )/as.numeric(sum(RRC_CONN_STP_ATT)),3),
    
    CONG_CODE_RRC =  round(100 * as.numeric(sum(RRC_CONN_STP_FAIL_AC_COD)/sum(RRC_CONN_STP_ATT)),3),
    
    CONG_CE_RAB =  round(100* as.numeric(sum(RAB_STP_FAIL_CS_VOICE_BTS))/as.numeric(sum(RAB_STP_COMP_CS_VOICE)),3),
    
    CONG_PWR_RAB =  round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_UL)+sum(RAB_STP_FAIL_CS_VOICE_AC_DL) + sum(PS_SETUP_FAIL_AC_DL_NRT) + sum(PS_SETUP_FAIL_AC_UL_NRT))/as.numeric(sum(RAB_STP_COMP_CS_VOICE) + sum(PS_RAB_ATT)) ,3),
    
    CONG_CODE_RAB = round(100 * as.numeric(sum(RAB_STP_FAIL_CS_VOICE_AC_COD)+ sum(PS_SETUP_FAIL_AC_COD_NRT)) / as.numeric(sum(RAB_STP_COMP_CS_VOICE)++ sum(PS_RAB_ATT)),3),
    
    TRAFF_CS_ERL = round(as.numeric(sum(as.numeric(as.numeric(AVG_RAB_HLD_TM_CS_VOICE) )))/360000,3),
    
    VOLUMEN_DATOS =  round(as.numeric(sum(as.numeric(VOL_DL) )+sum(as.numeric(VOL_UL) ))/1000000000,3),
    
    DROP_CS = round(as.numeric(sum(NUM_RET_CS)),3),
    
    DROP_PS = round(as.numeric(sum(NUM_RET_PS)),3),
    
    PAGING_DISCARD = as.numeric(sum(FAIL_PAG_NO_RESP_URA_PCH + FAIL_PAG_NO_RESP_CELL_PCH)),
    
    PAGING_RATE = round(100 - 100 * as.numeric(sum(FAIL_PAG_NO_RESP_URA_PCH + FAIL_PAG_NO_RESP_CELL_PCH))/as.numeric(sum(PAGING_OCCASION_CELL_PCH + PAGING_OCCASION_URA_PCH)),2),
    
    MAX_TROUGHPUT_HSDPA = round(as.numeric(0),3),
    
    AVG_TROUGHPUT_HSDPA = round(as.numeric(sum(as.numeric(HSDPA_ORIG_DATA)) *8*500) / as.numeric(sum(as.numeric(HSDPA_BUFF_WITH_DATA_PER_TTI)) ) ,3),
    
    MAX_TROUGHPUT_HSUPA = round(as.numeric(0),3),
    
    AVG_TROUGHPUT_HSUPA = round((as.numeric(sum(as.numeric(MACE_PDU_DATA_2MS_TTI) ) + sum(as.numeric(MACE_PDU_DATA_10MS_TTI) ))*8)/as.numeric(as.numeric(sum(as.numeric(MACE_PDUS_2MS_TTI)) /500) + as.numeric(sum(as.numeric(MACE_PDUS_10MS_TTI)   ) /100)),3),
    
    RTWP = round(10 * log10(as.numeric(as.numeric(10^(-11)*sum(RTWP_CLASS_0)) + as.numeric(10^(-10.75)*sum(RTWP_CLASS_1)) +as.numeric(10^(-10.65)*sum(RTWP_CLASS_2)) +as.numeric(10^(-10.55)*sum(RTWP_CLASS_3)) +as.numeric(10^(-10.45)*sum(RTWP_CLASS_4))
                                       +as.numeric(10^(-10.25)*sum(RTWP_CLASS_5)) + as.numeric(10^(-10.25)*sum(RTWP_CLASS_6)) +as.numeric(10^(-10.15)*sum(RTWP_CLASS_7)) +as.numeric(10^(-10.05)*sum(RTWP_CLASS_8)) +as.numeric(10^(-9.95)*sum(RTWP_CLASS_9)) 
                                       +as.numeric(10^(-9.85)*sum(RTWP_CLASS_10)) + as.numeric(10^(-9.70)*sum(RTWP_CLASS_11)) +as.numeric(10^(-9.50)*sum(RTWP_CLASS_12)) +as.numeric(10^(-9.30)*sum(RTWP_CLASS_13)) +as.numeric(10^(-9.05)*sum(RTWP_CLASS_14)) 
                                       +as.numeric(10^(-8.75)*sum(RTWP_CLASS_15)) + as.numeric(10^(-8.45)*sum(RTWP_CLASS_16)) +as.numeric(10^(-8.15)*sum(RTWP_CLASS_17)) +as.numeric(10^(-7.75)*sum(RTWP_CLASS_18)) +as.numeric(10^(-7.25)*sum(RTWP_CLASS_19))
                                       +as.numeric(10^(-6.75)*sum(RTWP_CLASS_20)) + as.numeric(10^(-6.50)*sum(RTWP_CLASS_21)) )/
                              as.numeric( sum(RTWP_CLASS_0) +sum(RTWP_CLASS_1)+sum(RTWP_CLASS_2)+sum(RTWP_CLASS_3)+sum(RTWP_CLASS_4)+sum(RTWP_CLASS_5)+sum(RTWP_CLASS_6)+sum(RTWP_CLASS_7)+sum(RTWP_CLASS_8)+sum(RTWP_CLASS_9)+sum(RTWP_CLASS_10)+
                                            +sum(RTWP_CLASS_11)+sum(RTWP_CLASS_12)+sum(RTWP_CLASS_13)+sum(RTWP_CLASS_14)+sum(RTWP_CLASS_15)+sum(RTWP_CLASS_16)+sum(RTWP_CLASS_17)+sum(RTWP_CLASS_18)+sum(RTWP_CLASS_19)+sum(RTWP_CLASS_20)+sum(RTWP_CLASS_21))
    ),2),
    
    MAX_USER_HSDPA = as.numeric(sum(as.numeric(MAX_HSDPA_USERS_IN_CELL) )),
    
    MAX_USER_HSUPA = as.numeric(sum(as.numeric(MAX_HSUPA_USERS_IN_CELL) )),
    
    AVG_USER_HSDPA = round(as.numeric(0),3),
    
    AVG_USER_HSUPA = round(as.numeric(0),3),
    
    DISPONIBILIDAD = round(100 * sum(AVAIL_WCELL_IN_WO_STATE)/as.numeric(sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)-sum(AVAIL_WCELL_BLOCKED_BY_USER)),2)
    )
  
  datosNok[is.na(datosNok)] <- 0
  
  return(list("datosHua"= datosHua, "datosEri"=datosEri,"datosNok"=datosNok ))
  
}

queryEricsson <- function(idate,fdate,datos,granul){
  con <- odbcConnect('dwhdb',uid = 'dcbo' , pwd = 'dcbo',believeNRows= FALSE)
   ####2G####
  qry<- "SELECT  DATEFORMAT(DATETIME_ID,'granul')  AS DIA, CELL_NAME as CELDA, BSC,
  
  --ACC---
  Sum(CLSDCCH_CCONGS) as CCONGS,
  Sum(CLSDCCH_CCALLS) as CCALLS,
  SUM(CELLCCHDR_CDISSS) as CDISSS,
  SUM(CELLCCHDR_CDISQA) as CDISQA,
  SUM(CELLCCHDR_CDISTA) as CDISTA,
  Sum(CLSDCCH_CMSESTAB) as CMSESTAB,
  SUM(CELTCHF_TFCASSALL) as TFCASSALL,
  SUM(CELTCHF_TFCASSALLSUB) as TFCASSALLSUB,
  SUM(CELTCHH_THCASSALL) as THCASSALL,
  SUM(CELTCHH_THCASSALLSUB) as THCASSALLSUB,
  SUM(CLTCH_TASSALL) as TASSALL,
  SUM(CLTCH_TCASSALL) as TCASSALL,
  SUM(CLSDCCH_CTRALACC) as  CTRALACC,
  SUM(CLSDCCH_CNSCAN)  as CNSCAN,
  SUM(CELTCHF_TFTRALACC)  as TFTRALACC, 
  SUM(CELTCHF_TFNSCAN)  as TFNSCAN, 
  SUM(CELTCHH_THTRALACC)  as THTRALACC,
  SUM(CELTCHH_THNSCAN) as THNSCAN,
  SUM(CLSDCCH_CNDROP) as DROP_SDCCH,
  SUM (CELTCHF_TFNDROP + CELTCHH_THNDROP + CELTCHF_TFNDROPSUB + CELTCHH_THNDROPSUB) as DROP_TCH,
  
  --RET---
  SUM(CELTCHF_TFNDROP) as TFNDROP,
  SUM(CELTCHH_THNDROP) as THNDROP,
  SUM(CELTCHF_TFNDROPSUB) as TFNDROPSUB,
  SUM(CELTCHH_THNDROPSUB) as THNDROPSUB,
  SUM(CELTCHF_TFMSESTB) as TFMSESTB,
  SUM(CELTCHH_THMSESTB) as THMSESTB,
  
  --ICM BAND---
  Sum(IDLEOTCHF_ITFOSIB4) as ITFOSIB4,
  Sum(IDLEOTCHF_ITFOSIB5) as ITFOSIB5,
  Sum(IDLEUTCHF_ITFUSIB4) as ITFUSIB4,
  Sum(IDLEUTCHF_ITFUSIB5) as ITFUSIB5,
  
  Sum(IDLEOTCHF_ITFOSIB1) as ITFOSIB1,
  Sum(IDLEOTCHF_ITFOSIB2) as ITFOSIB2,
  Sum(IDLEOTCHF_ITFOSIB3) as ITFOSIB3,
  Sum(IDLEUTCHF_ITFUSIB1) as ITFUSIB1,
  Sum(IDLEUTCHF_ITFUSIB2) as ITFUSIB2,
  Sum(IDLEUTCHF_ITFUSIB3) as ITFUSIB3,
  SUM(IDLEUTCHF_NOACCUF) AS NOACCUF,
  
  
  --Congestion TCH---
  SUM(CLSDCCH_CNRELCONG) as CNRELCONG,
  SUM(CELTCHF_TFNRELCONG) as TFNRELCONG,
  SUM(CELTCHH_THNRELCONG) as THNRELCONG,
  SUM(CELTCHF_TFNRELCONGSUB) as TFNRELCONGSUB,
  SUM(CELTCHH_THNRELCONGSUB) as THNRELCONGSUB,
  
  --Pagings---
  MAX(CELLPAG_PAGETOOOLD) as PAGETOOOLD,
  MAX(CELLPAG_PAGPCHCONG) as PAGPCHCONG,
  MAX(CELLPAG_PAGESRECCS) as PAGESRECCS,
  MAX(CELLPAG_PAGESRECPS) as PAGESRECPS,
  
  SUM(CELLPAG_PAGPCHCONGCS) AS PAGPCHCONGCS,
  SUM(CELLPAG_PAGPCHCONGPS) AS PAGPCHCONGPS,
  SUM(CELLPAG_PAGETOOOLDCS) AS PAGETOOOLDCS,
  SUM(CELLPAG_PAGETOOOLDPS) AS PAGETOOOLDPS,
  
  --RANDOM---
  Sum(RANDOMACC_CNROCNT) AS CNROCNT,
  Sum(RANDOMACC_RAACCFA) AS RAACCFA
  
  
  FROM dc.DC_E_BSS_CELL_CS_RAW
  
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  GROUP BY DATEFORMAT(DATETIME_ID,'granul'),BSC,CELDA
  ORDER BY DATEFORMAT(DATETIME_ID,'granul'),BSC,CELDA"
  
  qry2 <- "
  SELECT  DATEFORMAT(DATETIME_ID,'granul') AS DIA, CELL_NAME as CELDA,  BSC,
  SUM(CELLQOSEG_DLBGEGDATA) AS CELLQOSEG_DLBGEGDATA,
  SUM(CELLQOSEG_DLTHP1EGDATA) AS CELLQOSEG_DLTHP1EGDATA,
  SUM(CELLQOSEG_DLTHP2EGDATA) AS CELLQOSEG_DLTHP2EGDATA,
  SUM(CELLQOSEG_DLTHP3EGDATA) AS CELLQOSEG_DLTHP3EGDATA,
  SUM(CELLQOSEG_ULBGEGDATA) AS CELLQOSEG_ULBGEGDATA,
  SUM(CELLQOSEG_ULTHP1EGDATA) AS CELLQOSEG_ULTHP1EGDATA,
  SUM(CELLQOSEG_ULTHP2EGDATA) AS CELLQOSEG_ULTHP2EGDATA,
  SUM(CELLQOSEG_ULTHP3EGDATA) AS CELLQOSEG_ULTHP3EGDATA,
  SUM(CELLQOSG_DLBGGDATA) AS CELLQOSG_DLBGGDATA,
  SUM(CELLQOSG_DLTHP1GDATA) AS CELLQOSG_DLTHP1GDATA,
  SUM(CELLQOSG_DLTHP2GDATA) AS CELLQOSG_DLTHP2GDATA,
  SUM(CELLQOSG_DLTHP3GDATA) AS CELLQOSG_DLTHP3GDATA,
  SUM(CELLQOSG_ULBGGDATA) AS CELLQOSG_ULBGGDATA,
  SUM(CELLQOSG_ULTHP1GDATA) AS CELLQOSG_ULTHP1GDATA,
  SUM(CELLQOSG_ULTHP2GDATA) AS CELLQOSG_ULTHP2GDATA,
  SUM(CELLQOSG_ULTHP3GDATA) AS CELLQOSG_ULTHP3GDATA,
  SUM(CELLGPRS_DLTBFEST) AS CELLGPRS_DLTBFEST,
  SUM(CELLGPRS_FAILDLTBFEST) AS CELLGPRS_FAILDLTBFEST,
  SUM(CELLGPRS2_PSCHREQ) AS CELLGPRS2_PSCHREQ,
  SUM(CELLGPRS2_PREJTFI) AS CELLGPRS2_PREJTFI,
  SUM(CELLGPRS2_PREJOTH) AS CELLGPRS2_PREJOTH
  FROM dc.DC_E_BSS_CELL_PS_RAW
  WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
  GROUP BY DATEFORMAT(DATETIME_ID,'granul'),BSC,CELDA
  ORDER BY DATEFORMAT(DATETIME_ID,'granul'),BSC,CELDA"

datos2G <- sqlQuery(con, gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry))))
datos2G_1 <- sqlQuery(con, gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry2))))

dataEri2G <- datos2G %>% inner_join(datos2G_1)


   ####3G####
qry1<- "
SELECT  DATEFORMAT(DATETIME_ID,'granul') AS DIA, UtranCell as CELDA, RNC,


--PS DROP---

SUM(pmNoNormalRabReleasePacket) as pmNoNormalRabReleasePacket,

--CS DROP---
SUM(pmNoSystemRabReleaseSpeech ) as pmNoSystemRabReleaseSpeech,
SUM(pmNoSystemRabReleaseCs64) as pmNoSystemRabReleaseCs64,
SUM(pmNoNormalRabReleaseSpeech) as pmNoNormalRabReleaseSpeech,
SUM(pmNoNormalRabReleaseCs64) as pmNoNormalRabReleaseCs64,

---PS_RRC_Rate---
SUM(pmTotNoRrcConnectReqPsSucc) as pmTotNoRrcConnectReqPsSucc,
SUM(pmTotNoRrcConnectReqPs) as pmTotNoRrcConnectReqPs,
SUM(pmNoLoadSharingRrcConnPs) as pmNoLoadSharingRrcConnPs,

---PS_RAB_Rate---
SUM(pmNoRabEstablishSuccessPacketInteractive) as pmNoRabEstablishSuccessPacketInteractive,
SUM(pmNoRabEstablishAttemptPacketInteractive) as pmNoRabEstablishAttemptPacketInteractive,


---CS_RRC_Rate---
SUM(pmTotNoRrcConnectReqCsSucc) as pmTotNoRrcConnectReqCsSucc,
SUM(pmTotNoRrcConnectReqCs) as pmTotNoRrcConnectReqCs,
SUM(pmNoLoadSharingRrcConnCs) as pmNoLoadSharingRrcConnCs,

---CS_RAB_Rate---
SUM(pmNoRabEstablishSuccessSpeech) as pmNoRabEstablishSuccessSpeech,
SUM(pmNoRabEstablishSuccessCs64) as pmNoRabEstablishSuccessCs64,
SUM(pmRabEstablishEcSuccess) as pmRabEstablishEcSuccess,
SUM(pmRabEstablishEcAttempt) as pmRabEstablishEcAttempt,
SUM(pmNoRabEstablishAttemptSpeech) as pmNoRabEstablishAttemptSpeech,
SUM(pmNoRabEstablishAttemptCs64) as pmNoRabEstablishAttemptCs64,
SUM(pmNoDirRetryAtt) as pmNoDirRetryAtt,

---Congestion_PWR---
SUM(pmNoRrcReqDeniedAdmDlPwr) as pmNoRrcReqDeniedAdmDlPwr,
SUM(pmNoFailedRabEstAttemptLackDlPwr) as pmNoFailedRabEstAttemptLackDlPwr,

---Congestion_CODES---
SUM(pmNoRrcReqDeniedAdmDlChnlCode) as pmNoRrcReqDeniedAdmDlChnlCode,
SUM(pmNoFailedRabEstAttemptLackDlChnlCode) as pmNoFailedRabEstAttemptLackDlChnlCode,

---Congestion_CE---
SUM(pmNoFailedRabEstAttemptLackDlHw) as pmNoFailedRabEstAttemptLackDlHw,
SUM(pmNoFailedRabEstAttemptLackUlHw) as pmNoFailedRabEstAttemptLackUlHw,
SUM(pmNoRrcReqDeniedAdmDlHw) as pmNoRrcReqDeniedAdmDlHw,
SUM(pmNoRrcReqDeniedAdmUlHw) as pmNoRrcReqDeniedAdmUlHw,

---Total Eventos Congestion---

SUM(pmDlTrafficVolumePsIntHs) as pmDlTrafficVolumePsIntHs,
SUM(pmUlTrafficVolumePsIntEul) as pmUlTrafficVolumePsIntEul,
SUM(pmUlTrafficVolumePs384+pmUlTrafficVolumePs8+pmUlTrafficVolumePs16+pmUlTrafficVolumePs64+pmUlTrafficVolumePs128) as VOLUMEN_R99_UL,
SUM(pmDlTrafficVolumePs64+pmDlTrafficVolumePs128+pmDlTrafficVolumePs384+pmDlTrafficVolumePs8+pmDlTrafficVolumePs16) as VOLUMEN_R99_DL,

---Paging---
SUM(pmNoPagingAttemptUtranRejected) as pmNoPagingAttemptUtranRejected,
SUM(pmNoPagingType1Attempt) as pmNoPagingType1Attempt,
sum(pmNoPagingType1AttemptCs) as pmNoPagingType1AttemptCs,
sum(pmNoPagingType1AttemptPs) as pmNoPagingType1AttemptPs,



---RSSI---
SUM(pmSUMUlRssi) as pmSUMUlRssi,
SUM(pmSamplesUlRssi) as pmSamplesUlRssi,
SUM(pmSumBestCs12Establish)  as pmSumBestCs12Establish, 
SUM(pmSamplesBestCs12Establish) as pmSamplesBestCs12Establish,

---PS DROP-----
SUM(pmNoSystemRbReleaseHs) as   pmNoSystemRbReleaseHs,
SUM(pmNoSystemRbReleaseEul) as   pmNoSystemRbReleaseEul,

SUM(pmNoSystemRabReleasePacket) as pmNoSystemRabReleasePacket,
---VER
SUM(pmNoSystemRabReleasePacketUra) as pmNoSystemRabReleasePacketUra,
SUM(pmChSwitchAttemptFachUra) as pmChSwitchAttemptFachUra,
SUM(pmChSwitchSuccFachUra) as pmChSwitchSuccFachUra,
SUM(pmChSwitchAttemptDchUra) as pmChSwitchAttemptDchUra,
SUM(pmChSwitchSuccDchUra) as pmChSwitchSuccDchUra,
SUM(pmChSwitchAttemptHsUra) as pmChSwitchAttemptHsUra,
SUM(pmChSwitchSuccHsUra) as pmChSwitchSuccHsUra,
SUM(pmNoNormalRabReleasePacketUra) as pmNoNormalRabReleasePacketUra,
---Erlangs PS----
SUM(pmSumBestDchPsIntRabEstablish) as pmSumBestDchPsIntRabEstablish,
SUM(pmSumFachPsIntRabEstablish) as pmSumFachPsIntRabEstablish,
SUM(pmSumBestPsHsAdchRabEstablish) as pmSumBestPsHsAdchRabEstablish,
SUM(pmSumBestPsEulRabEstablish) as pmSumBestPsEulRabEstablish,

SUM(pmSumDlCode) AS pmSumDlCode,
SUM(pmSamplesDlCode) AS pmSamplesDlCode,

SUM(pmChSwitchAttemptUraFach) AS pmChSwitchAttemptUraFach,
SUM(pmChSwitchSuccUraFach) AS pmChSwitchSuccUraFach,
SUM(pmChSwitchAttemptUraDch) AS pmChSwitchAttemptUraDch,
SUM(pmChSwitchSuccUraDch) AS pmChSwitchSuccUraDch,
SUM(pmChSwitchAttemptUraHs) AS pmChSwitchAttemptUraHs,
SUM(pmChSwitchSuccUraHs) AS pmChSwitchSuccUraHs,
SUM(pmNoSysRelSpeechSoHo) AS pmNoSysRelSpeechSoHo,
SUM(pmNoSysRelSpeechNeighbr) AS pmNoSysRelSpeechNeighbr,
SUM(pmNoSysRelSpeechUlSynch) AS pmNoSysRelSpeechUlSynch,
SUM(pmNoOfTermSpeechCong) AS pmNoOfTermSpeechCong,

SUM(pmSumPsHsAdchRabEstablish) as pmSumPsHsAdchRabEstablish,
SUM(pmSamplesPsHsAdchRabEstablish) as pmSamplesPsHsAdchRabEstablish, 
SUM(pmSumPsEulRabEstablish) as pmSumPsEulRabEstablish,
SUM(pmSamplesPsEulRabEstablish) as pmSamplesPsEulRabEstablish,
SUM(pmCellDowntimeAuto) as pmCellDowntimeAuto,
SUM(pmCellDowntimeMan) as pmCellDowntimeMan,

SUM(pmTotNoRrcConnectReq) as pmTotNoRrcConnectReq,

SUM(pmNoRabEstablishAttemptPacketStream) as pmNoRabEstablishAttemptPacketStream,
SUM(pmNoRabEstablishAttemptPacketStream128) as pmNoRabEstablishAttemptPacketStream128

FROM dc.DC_E_RAN_UCELL_RAW


WHERE (DATE_ID >= 'idate') AND (DATE_ID < 'fdate') 
GROUP BY DATEFORMAT(DATETIME_ID,'granul'), RNC,CELDA
ORDER BY DATEFORMAT(DATETIME_ID,'granul'), RNC,CELDA"

dataEri3G <- sqlQuery(con, gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry1))))



   ####Funciones####
  
  dataEri2G$TFTRALACC = ifelse(dataEri2G$TFTRALACC == 0, 1, dataEri2G$TFTRALACC)
  dataEri2G$TFNSCAN = ifelse(dataEri2G$TFNSCAN == 0, 1 , dataEri2G$TFNSCAN)
  dataEri2G$THTRALACC = ifelse(dataEri2G$THTRALACC == 0, 1, dataEri2G$THTRALACC)
  dataEri2G$THNSCAN = ifelse(dataEri2G$THNSCAN == 0, 1 , dataEri2G$THNSCAN)
  dataEri2G$CTRALACC = ifelse(dataEri2G$CTRALACC == 0, 1 , dataEri2G$CTRALACC)
  dataEri2G$CNSCAN = ifelse(dataEri2G$CNSCAN == 0, 1 , dataEri2G$CNSCAN)


  dataEri3G$VOLUMEN_R99_UL <- as.numeric(as.character(dataEri3G$VOLUMEN_R99_UL))
  dataEri3G$VOLUMEN_R99_DL <- as.numeric(as.character(dataEri3G$VOLUMEN_R99_DL))
  dataEri3G$pmSamplesUlRssi <- as.numeric(as.character(dataEri3G$pmSamplesUlRssi))
  dataEri3G$pmSamplesBestCs12Establish <- as.numeric(as.character(dataEri3G$pmSamplesBestCs12Establish))
  dataEri3G$pmSUMUlRssi <- as.numeric(as.character(dataEri3G$pmSUMUlRssi))
  dataEri3G$pmSumBestCs12Establish <- as.numeric(as.character(dataEri3G$pmSumBestCs12Establish))

  dataEri3G$pmSumPsHsAdchRabEstablish <- as.numeric(as.character(dataEri3G$pmSumPsHsAdchRabEstablish))
  dataEri3G$pmSamplesPsHsAdchRabEstablish <- as.numeric(as.character(dataEri3G$pmSamplesPsHsAdchRabEstablish))
  dataEri3G$pmSumPsEulRabEstablish <- as.numeric(as.character(dataEri3G$pmSumPsEulRabEstablish))
  dataEri3G$pmSamplesPsEulRabEstablish <- as.numeric(as.character(dataEri3G$pmSamplesPsEulRabEstablish))

  dataEri3G[is.na(dataEri3G)] <- 0
  dataEri2G[is.na(dataEri2G)] <- 0
  #dataEri4G[is.na(dataEri4G)] <- 0

  dataEri3G$VOLUMEN_R99_UL = ifelse(dataEri3G$VOLUMEN_R99_UL == 0,1,dataEri3G$VOLUMEN_R99_UL)
  dataEri3G$VOLUMEN_R99_DL = ifelse(dataEri3G$VOLUMEN_R99_DL == 0,1,dataEri3G$VOLUMEN_R99_DL)

  dataEri3G <- dataEri3G %>% inner_join(datos)
  dataEri2G <- dataEri2G %>% inner_join(datos)

  #dataEri4G$sum_pmActiveUeDlMax <- as.numeric(as.character(dataEri4G$sum_pmActiveUeDlMax))
  #dataEri4G$max_pmActiveUeDlMax <- as.numeric(as.character(dataEri4G$max_pmActiveUeDlMax))
  #dataEri4G$sum_pmActiveUeUlMax <- as.numeric(as.character(dataEri4G$sum_pmActiveUeUlMax))
  #dataEri4G$max_pmActiveUeUlMax <- as.numeric(as.character(dataEri4G$max_pmActiveUeUlMax))

  
  #datos2G$DIA <- as.Date(datos2G$DIA)
  #datos3G$DIA <- as.Date(datos3G$DIA)
  #datos4G$DIA <- as.Date(datos4G$DIA)
  #
  #datos2G <- datos2G %>% filter(DIA >= idate,DIA < fdate) %>% inner_join(datos) 
  #datos3G <- datos3G %>% filter(DIA >= idate,DIA < fdate) %>% inner_join(datos) 
  #datos4G <- datos4G %>% filter(DIA >= idate,DIA < fdate) %>% inner_join(datos) 
  #
  dataEri3G$DIA <- as.factor(dataEri3G$DIA)
  dataEri2G$DIA <- as.factor(dataEri2G$DIA)
  #datos4G$DIA <- as.factor(datos4G$DIA)
  
  return(list("ericsson2G"= dataEri2G,"ericsson3G"=dataEri3G))
  
}
queryNokia <- function(idate,fdate,datos,granul){
   ####2G####
  qry1 <- "select to_char(period_start_time, 'granul') as DIA,
CO_NAME as CELDA,
  
  ----ACC----
  sum(TCH_REQUESTS_CALL_ATTEMPT) as TCH_REQUESTS_CALL_ATTEMPT,
  sum(SUCC_TCH_SEIZ_CALL_ATTEMPT) as SUCC_TCH_SEIZ_CALL_ATTEMPT,
  sum(SDCCH_DROP_CALL_AND_HO) as SDCCH_DROP_CALL_AND_HO,
  
  ----RET----
  sum(DROP_AFTER_TCH_ASSIGN) as DROP_AFTER_TCH_ASSIGN,
  
  ----SDCCH CONG
  sum(SDCCH_BUSY_ATT) as SDCCH_BUSY_ATT,
  sum(TCH_SEIZ_ATT_DUE_SDCCH_CON) as TCH_SEIZ_ATT_DUE_SDCCH_CON,
  sum(SDCCH_SEIZ_ATT) as SDCCH_SEIZ_ATT,
  
  sum(SDCCH_RADIO_FAIL + SDCCH_RF_OLD_HO  +SDCCH_BCSU_RESET+SDCCH_NETW_ACT+SDCCH_RADIO_FAIL+
  + SDCCH_ABIS_FAIL_CALL+SDCCH_ABIS_FAIL_OLD+SDCCH_BTS_FAIL+SDCCH_LAPD_FAIL+
  SDCCH_A_IF_FAIL_CALL+SDCCH_A_IF_FAIL_OLD
  ) as DROP_SDCCH
  
  
  from BSC_PS_TRAFFIC_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (CO_GID = bts_gid ) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME
  "
  
  qry2 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  ----ACC----
  SUM(SDCCH_ALLOC_FOR_VOICE_CALL) AS SDCCH_ALLOC_FOR_VOICE_CALL,
  SUM(RES_AV_DENOM15) AS RES_AV_DENOM15,
  SUM(AVE_BUSY_SDCCH) AS AVE_BUSY_SDCCH,
  SUM(AVE_BUSY_TCH) AS AVE_BUSY_TCH,
  SUM(AVE_BUSY_SDCCH) AS AVE_BUSY_SDCCH,
  SUM(RES_AV_DENOM14) AS RES_AV_DENOM14,
  SUM(AVE_TCH_BUSY_FULL) AS AVE_TCH_BUSY_FULL,
  SUM(AVE_BUSY_DHR_TCH) AS AVE_BUSY_DHR_TCH,
  SUM(AVE_TCH_BUSY_HALF) AS AVE_TCH_BUSY_HALF,
  SUM(AVE_BUSY_DFR_TCH) AS AVE_BUSY_DFR_TCH,
  SUM(AVE_BUSY_DHR_TCH_DENOM) AS AVE_BUSY_DHR_TCH_DENOM,
  sum(AVE_IDLE_F_TCH_1) as AVE_IDLE_F_TCH_1,
  sum(AVE_IDLE_F_TCH_2) as AVE_IDLE_F_TCH_2,
  sum(AVE_IDLE_F_TCH_3) as AVE_IDLE_F_TCH_3,
  sum(AVE_IDLE_F_TCH_4) as AVE_IDLE_F_TCH_4,
  sum(AVE_IDLE_F_TCH_5) as AVE_IDLE_F_TCH_5,
  sum(RES_AV_DENOM4) as RES_AV_DENOM4,
  sum(RES_AV_DENOM5) as RES_AV_DENOM5,
  sum(RES_AV_DENOM6) as RES_AV_DENOM6,
  sum(RES_AV_DENOM7) as RES_AV_DENOM7,
  sum(RES_AV_DENOM8) as RES_AV_DENOM8
  
  from BSC_PS_RESAVAIL_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry3 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(TCH_NEW_CALL_ASSIGN) AS TCH_NEW_CALL_ASSIGN,
  SUM(TCH_RE_EST_ASSIGN) AS TCH_RE_EST_ASSIGN,
  SUM(TCH_HO_ASSIGN) AS TCH_HO_ASSIGN,
  SUM(T3101_EXPIRED) AS T3101_EXPIRED,
  SUM(TCH_HO_RELEASE) AS TCH_HO_RELEASE
  
  from BSC_PS_SERVICE_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry4 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(CELL_TCH_TCH) AS CELL_TCH_TCH
  
  from BSC_PS_HO_TTP1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry5 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(DELETE_PAGING_COMMAND) AS DELETE_PAGING_COMMAND,
  SUM(PAGING_MSG_SENT) as PAGING_MSG_SENT
  
  from BSC_PS_RESACC_TTP2_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry6 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----RET----
  SUM(RLC_DATA_BLOCKS_UL_CS1) AS RLC_DATA_BLOCKS_UL_CS1,
  SUM(RLC_DATA_BLOCKS_UL_CS2) AS RLC_DATA_BLOCKS_UL_CS2,
  SUM(RLC_DATA_BLOCKS_DL_CS1) AS RLC_DATA_BLOCKS_DL_CS1,
  SUM(RLC_DATA_BLOCKS_DL_CS2) AS RLC_DATA_BLOCKS_DL_CS2
  
  from BSC_PS_PCU_TTP_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry7 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(UL_RLC_BLOCKS_IN_ACK_MODE + UL_RLC_BLOCKS_IN_UNACK_MODE) as  UL_RLC_BLOCK,
  SUM(DL_RLC_BLOCKS_IN_ACK_MODE + DL_RLC_BLOCKS_IN_UNACK_MODE) as  DL_RLC_BLOCK
  
  from BSC_PS_CODINGSC_CS1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (bts_gid = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  } 
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 7
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('NOKIA',uid="pmr",pwd="pmr")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    #    result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datanok2G <- outputPar[[1]]
  for (i in 2:length(outputPar)){datanok2G <- datanok2G %>% full_join(outputPar[[i]])}
  
  datanok2G <- filter(datanok2G, !is.na(CELDA))
  datanok2G$BSC <- "BSC_NOK"
  datanok2G[is.na(datanok2G)] <- 0
  
  
   ####3G####
  qry1 <- "
  select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  ----TRAFF_CS-----
  sum(AVG_RAB_HLD_TM_CS_VOICE + avg_rab_hld_tm_cs_conv ) as AVG_RAB_HLD_TM_CS_VOICE,
  
  ----ACC_CS----NUM
  SUM(moc_conv_call_atts - moc_conv_call_fails + mtc_conv_call_atts - mtc_conv_call_fails + emergency_call_atts 
  - emergency_call_fails - rrc_acc_rel_emergency - rrc_acc_rel_mo_conv - rrc_acc_rel_mt_conv 
  ) as ACC_CS_N,
  
  sum(moc_conv_call_atts + mtc_conv_call_atts + emergency_call_atts - rrc_att_rep_mo_conv - rrc_att_rep_mt_conv 
  - rrc_att_rep_emergency - rrc_acc_rel_emergency - rrc_acc_rel_mo_conv - rrc_acc_rel_mt_conv -
  rrc_conn_stp_rej_emerg_call ) as ACC_CS_D,
  
  SUM(RAB_ACC_COMP_CS_VOICE)as RAB_CS_VOICE, 
  
  sum(RAB_STP_ATT_CS_VOICE) as RAB_STP_VOICE,
  
  --ACC_PS
  ---NUM---
  sum( moc_inter_call_atts - moc_inter_call_fails + moc_backg_call_atts - moc_backg_call_fails 
  + mtc_inter_call_atts - mtc_inter_call_fails + mtc_backg_call_atts - mtc_backg_call_fails + moc_high_prior_sign_atts 
  - moc_high_prior_sign_fails + mtc_high_prior_sign_atts - mtc_high_prior_sign_fails - rrc_acc_rel_interactive - rrc_acc_rel_mo_background 
  - rrc_acc_rel_mo_high_pr_sign - rrc_acc_rel_mo_interactive - rrc_acc_rel_mt_background - rrc_acc_rel_mt_high_pr_sign ) as NUM_ACC_PS,
  
  ---DEN----
  sum (
  moc_inter_call_atts + moc_backg_call_atts + moc_high_prior_sign_atts + mtc_inter_call_atts + mtc_backg_call_atts + 
  mtc_high_prior_sign_atts - rrc_att_rep_interactive - rrc_att_rep_mo_interactive - rrc_att_rep_mo_high_pr_sign - rrc_att_rep_mo_background - 
  rrc_att_rep_mt_background - rrc_att_rep_mt_high_pr_sign - rrc_acc_rel_interactive - rrc_acc_rel_mo_background - rrc_acc_rel_mo_high_pr_sign 
  - rrc_acc_rel_mo_interactive - rrc_acc_rel_mt_background - rrc_acc_rel_mt_high_pr_sign) as DEN_ACC_PS,
  
  ---MULT_NUM----
  sum(rab_acc_comp_ps_inter + rab_acc_comp_ps_backg) as MULT_NUM1,
  sum(rab_stp_att_ps_inter + rab_stp_att_ps_backg) as MULT_NUM2,
  
  ---RET_CS
  ----NUM-----
  sum(rab_act_rel_cs_voice_p_emp + rab_act_fail_cs_voice_iu + rab_act_fail_cs_voice_radio + rab_act_fail_cs_voice_bts
  + rab_act_fail_cs_voice_iur + rab_act_fail_cs_voice_rnc + rab_act_fail_cs_voice_ue + rab_act_fail_cs_voice_trans ) as NUM_RET_CS,
  ----DEN----
  sum( rab_act_comp_cs_voice + rab_act_rel_cs_voice_srnc +  rab_act_rel_cs_voice_p_emp +  rab_act_rel_cs_voice_hho + rab_act_rel_cs_voice_isho + 
  rab_act_rel_cs_voice_ganho + rab_act_rel_cs_voice_ganho + rab_act_fail_cs_voice_iu + rab_act_fail_cs_voice_radio + rab_act_fail_cs_voice_bts +
  rab_act_fail_cs_voice_iur + rab_act_fail_cs_voice_rnc + rab_act_fail_cs_voice_ue + rab_act_fail_cs_voice_trans) as DEN_RET_CS,
  
  ----CONG_CE---------
  sum(RRC_CONN_STP_FAIL_BTS) as RRC_CONN_STP_FAIL_BTS,
  sum(RAB_STP_FAIL_CS_VOICE_BTS) as RAB_STP_FAIL_CS_VOICE_BTS,
  
  ----CONG_CODE---------
  sum(RAB_STP_FAIL_CS_VOICE_AC_COD) as RAB_STP_FAIL_CS_VOICE_AC_COD,
  sum(RRC_CONN_STP_FAIL_AC_COD) as RRC_CONN_STP_FAIL_AC_COD,
  
  
  ----CONG_PWR---------
  
  sum(RAB_STP_FAIL_CS_VOICE_AC_UL) as RAB_STP_FAIL_CS_VOICE_AC_UL,
  sum(RAB_STP_FAIL_CS_VOICE_AC_DL) as RAB_STP_FAIL_CS_VOICE_AC_DL,
  sum(RRC_CONN_STP_FAIL_AC_UL) as RRC_CONN_STP_FAIL_AC_UL,
  sum(RRC_CONN_STP_FAIL_AC_DL) as RRC_CONN_STP_FAIL_AC_DL,
  sum(RAB_STP_FAIL_PS_STREA_BTS) as RAB_STP_FAIL_PS_STREA_BTS,
  sum(RRC_CONN_STP_ATT) as RRC_CONN_STP_ATT,
  sum(RAB_STP_COMP_CS_VOICE)as RAB_STP_COMP_CS_VOICE
  
  from NOKRWW_PS_SERVLEV_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD')) 
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time,'granul'),CO_NAME
  "
  
  qry2 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  ----ACC----
  sum(CELL_UPDATE_SUCC_CS_CALL) as CELL_UPDATE_SUCC_CS_CALL,
  
  sum(CELL_UPDATE_ATT_CS_CALL) as CELL_UPDATE_ATT_CS_CALL,
  
  sum(FAIL_PAG_NO_RESP_CELL_PCH) as FAIL_PAG_NO_RESP_CELL_PCH,
  
  sum(FAIL_PAG_NO_RESP_URA_PCH) as FAIL_PAG_NO_RESP_URA_PCH,
  
  sum(PAGING_OCCASION_URA_PCH) as PAGING_OCCASION_URA_PCH,
  
  sum(PAGING_OCCASION_CELL_PCH) as PAGING_OCCASION_CELL_PCH,
  
  sum(ATT_HS_DSCH_TO_FACH) as ATT_HS_DSCH_TO_FACH,
  
  sum(ATT_FACH_TO_HS_DSCH) as ATT_FACH_TO_HS_DSCH, 
  
  sum(SUCC_HS_DSCH_TO_FACH) as SUCC_HS_DSCH_TO_FACH,
  
  sum(SUCC_FACH_TO_HS_DSCH) as SUCC_FACH_TO_HS_DSCH
  
  
  from NOKRWW_PS_RRC_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME "
  
  
  qry3 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  ---RET_PS----
  ---NUM----
  sum(ps_rel_rl_fail_hs_e_int + ps_rel_rl_fail_hs_e_bgr + ps_rel_rl_fail_hs_d_int + ps_rel_rl_fail_hs_d_bgr + ps_rel_rl_fail_d_d_int +
  ps_rel_rl_fail_d_d_bgr + ps_rel_oth_fail_hs_e_int + ps_rel_oth_fail_hs_e_bgr + ps_rel_oth_fail_hs_d_int + ps_rel_oth_fail_hs_d_bgr +
  ps_rel_oth_fail_d_d_int + ps_rel_oth_fail_d_d_bgr + ps_rel_rl_fail_hs_e_stre + ps_rel_rl_fail_hs_d_stre + ps_rel_rl_fail_d_d_stre  +
  ps_rel_oth_fail_hs_e_stre + ps_rel_oth_fail_hs_d_stre + ps_rel_oth_fail_d_d_stre ) as NUM_RET_PS,
  
  ---DEN----
  sum(ps_rel_rl_fail_hs_e_int + ps_rel_rl_fail_hs_e_bgr + ps_rel_rl_fail_hs_d_int + ps_rel_rl_fail_hs_d_bgr + ps_rel_rl_fail_d_d_int +
  ps_rel_rl_fail_d_d_bgr + ps_rel_oth_fail_hs_e_int + ps_rel_oth_fail_hs_e_bgr + ps_rel_oth_fail_hs_d_int + ps_rel_oth_fail_hs_d_bgr +
  ps_rel_oth_fail_d_d_int + ps_rel_oth_fail_d_d_bgr + ps_rel_rl_fail_hs_e_stre + ps_rel_rl_fail_hs_d_stre + ps_rel_rl_fail_d_d_stre  +
  ps_rel_oth_fail_hs_e_stre + ps_rel_oth_fail_hs_d_stre + ps_rel_oth_fail_d_d_stre + ps_rel_norm_hs_e_stre + ps_rel_norm_hs_d_stre +
  ps_rel_norm_d_d_stre + ps_rel_norm_hs_e_int + ps_rel_norm_hs_e_bgr + ps_rel_norm_hs_d_int + ps_rel_norm_hs_d_bgr + ps_rel_norm_d_d_int + ps_rel_norm_d_d_bgr ) as DEN_RET_PS,
  sum(PS_SETUP_FAIL_AC_COD_NRT) as PS_SETUP_FAIL_AC_COD_NRT,
  sum(PS_SETUP_FAIL_AC_DL_NRT) as PS_SETUP_FAIL_AC_DL_NRT,
  sum(PS_SETUP_FAIL_AC_UL_NRT) as PS_SETUP_FAIL_AC_UL_NRT,
  SUM(PS_ATT_HSDSCH_EDCH_INT + PS_ATT_HSDSCH_EDCH_BGR + PS_ATT_HSDSCH_DCH_INT + PS_ATT_HSDSCH_DCH_BGR + PS_ATT_DCH_DCH_INT + PS_ATT_DCH_DCH_BGR) as PS_RAB_ATT 
  
  
  from NOKRWW_PS_PKTCALL_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time,'granul'),CO_NAME "
  
  qry4 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(REJ_DCH_DUE_POWER_INT_DL) as REJ_DCH_DUE_POWER_INT_DL,
  SUM(REJ_DCH_DUE_POWER_BGR_DL) as REJ_DCH_DUE_POWER_BGR_DL,
  SUM(REJ_DCH_REC_DUE_PWR_INT_DL) as REJ_DCH_REC_DUE_PWR_INT_DL,
  SUM(REJ_DCH_REC_DUE_PWR_BGR_DL) as REJ_DCH_REC_DUE_PWR_BGR_DL,
  SUM(REJ_DCH_DUE_POWER_INT_UL) as REJ_DCH_DUE_POWER_INT_UL,
  SUM(REJ_DCH_DUE_POWER_BGR_UL) as REJ_DCH_DUE_POWER_BGR_UL,
  SUM(REJ_DCH_REC_DUE_PWR_INT_UL) as REJ_DCH_REC_DUE_PWR_INT_UL, 
  SUM(REJ_DCH_REC_DUE_PWR_BGR_UL) as REJ_DCH_REC_DUE_PWR_BGR_UL,
  sum(REJ_DCH_DUE_CODES_INT_DL) as REJ_DCH_DUE_CODES_INT_DL,
  sum(REJ_DCH_DUE_CODES_BGR_DL) as REJ_DCH_DUE_CODES_BGR_DL,
  SUM(dch_sel_max_hsdpa_users_int + dch_sel_max_hsdpa_users_bgr + dch_sel_max_hsdpa_users_str) as HSDPA_TO_DCH_NUM,
  SUM(allo_hs_dsch_flow_int + allo_hs_dsch_flow_bgr + allo_hs_dsch_flow_str + dch_sel_max_hsdpa_users_int + dch_sel_max_hsdpa_users_bgr + dch_sel_max_hsdpa_users_str 
  + dl_dch_sel_hsdpa_power_int + dl_dch_sel_hsdpa_power_bgr +dl_dch_sel_hsdpa_power_str + rej_hs_dsch_ret_int + rej_hs_dsch_ret_bgr + rej_hs_dsch_ret_str 
  + setup_fail_rnc_hs_dsch_int + setup_fail_rnc_hs_dsch_bgr + setup_fail_rnc_hs_dsch_str + setup_fail_ue_hs_dsch_int + setup_fail_ue_hs_dsch_bgr + setup_fail_ue_hs_dsch_str 
  + setup_fail_bts_hs_dsch_int +setup_fail_bts_hs_dsch_bgr + setup_fail_bts_hs_dsch_str + setup_fail_iub_hs_total_int + setup_fail_iub_hs_total_bgr + setup_fail_iub_hs_total_str ) as HSDPA_TO_DCH_DEN,
  SUM(ul_dch_sel_max_hsupa_usr_int + ul_dch_sel_max_hsupa_usr_bgr + ul_dch_sel_max_hsupa_usr_str) as HSUPA_TO_DCH_NUM,
  
  ---HSDPA ACC---
  SUM(allo_success_edch_int + allo_success_edch_bgr + allo_success_edch_str + edch_allo_canc_na_as_bgr + edch_allo_canc_na_as_int + edch_allo_canc_na_as_str 
  + ul_dch_sel_max_hsupa_usr_bgr + ul_dch_sel_max_hsupa_usr_int + ul_dch_sel_max_hsupa_usr_str + ul_dch_sel_bts_hw_int + ul_dch_sel_bts_hw_bgr + ul_dch_sel_bts_hw_str 
  + setup_fail_edch_bts_bgr + setup_fail_edch_bts_int + setup_fail_edch_bts_str + setup_fail_edch_other_bgr + setup_fail_edch_other_int + setup_fail_edch_other_str 
  + setup_fail_edch_trans_bgr + setup_fail_edch_trans_int + setup_fail_edch_trans_str + setup_fail_edch_ue_bgr + setup_fail_edch_ue_int + setup_fail_edch_ue_str 
  + setup_rej_edch_ac_int + setup_rej_edch_ac_bgr) as HSUPA_TO_DCH_DEN,
  
  SUM(allo_hs_dsch_flow_int + allo_hs_dsch_flow_bgr + rej_hs_dsch_ret_int + rej_hs_dsch_ret_bgr + setup_fail_rnc_hs_dsch_int + setup_fail_bts_hs_dsch_int + setup_fail_iub_hs_total_int +
  setup_fail_rnc_hs_dsch_bgr + setup_fail_bts_hs_dsch_bgr + setup_fail_iub_hs_total_bgr + setup_fail_ue_hs_dsch_int + setup_fail_ue_hs_dsch_bgr + dch_sel_max_hsdpa_users_int + dch_sel_max_hsdpa_users_bgr) as DEN_HSDPA_ACC,
  sum(allo_hs_dsch_flow_int + allo_hs_dsch_flow_bgr)  as NUM_HSDPA_ACC,
  
  ---HSUPA ACC---
  SUM(allo_success_edch_int + allo_success_edch_bgr + edch_allo_canc_na_as_bgr + edch_allo_canc_na_as_int 
  + ul_dch_sel_max_hsupa_usr_bgr + ul_dch_sel_max_hsupa_usr_int + ul_dch_sel_bts_hw_int + ul_dch_sel_bts_hw_bgr  + setup_fail_edch_bts_bgr + setup_fail_edch_bts_int + setup_fail_edch_other_bgr + setup_fail_edch_other_int 
  + setup_fail_edch_trans_bgr + setup_fail_edch_trans_int + setup_fail_edch_ue_bgr + setup_fail_edch_ue_int + setup_rej_edch_ac_int + setup_rej_edch_ac_bgr) as DEN_HSUPA_ACC,
  SUM(allo_success_edch_int + allo_success_edch_bgr) as NUM_HSUPA_ACC
  
  from NOKRWW_PS_TRAFFIC_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME "
  
  qry5 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(RB_RELEASE_DUE_DYLO_RL_POWER) as RB_RELEASE_DUE_DYLO_RL_POWER,
  SUM(RB_RELEASE_DUE_PBS_BTS) as RB_RELEASE_DUE_PBS_BTS,
  SUM(RB_RELEASE_DUE_PBS_INTERF) as RB_RELEASE_DUE_PBS_INTERF,
  SUM(RB_RELEASE_DUE_PBS_SPREAD) as RB_RELEASE_DUE_PBS_SPREAD,
  
  SUM(RB_RELEASE_DUE_PRE_EMP_BTS) as RB_RELEASE_DUE_PRE_EMP_BTS,
  SUM(RB_RELEASE_DUE_PRE_EMP_INTF)  as RB_RELEASE_DUE_PRE_EMP_INTF,
  SUM(RB_RELEASE_DUE_PRE_EMP_SPREA)  as RB_RELEASE_DUE_PRE_EMP_SPREA,
  
  SUM(HSPA_NRT_O_NRT_DUE_USR) as HSPA_NRT_O_NRT_DUE_USR,
  SUM(HSPA_NRT_O_NRT_DUE_BTS_HW) as HSPA_NRT_O_NRT_DUE_BTS_HW,
  SUM(HSPA_NRT_O_NRT_DUE_SC_SLOT) as HSPA_NRT_O_NRT_DUE_SC_SLOT,
  SUM(HSPA_NRT_O_NRT_DUE_TRANS) as HSPA_NRT_O_NRT_DUE_TRANS,
  SUM(HSPA_RT_O_NRT_DUE_SC) as HSPA_RT_O_NRT_DUE_SC,
  MAX(MAX_HSUPA_USERS_IN_CELL) as MAX_HSUPA_USERS_IN_CELL,
  MAX(MAX_HSDPA_USERS_IN_CELL) as MAX_HSDPA_USERS_IN_CELL,
  SUM(AVE_CE_USED_AMR ) as AVE_CE_USED_AMR,
  SUM(ave_ce_used_ps_bgr_8_ul +  ave_ce_used_ps_bgr_16_ul + ave_ce_used_ps_bgr_32_ul + ave_ce_used_ps_bgr_64_ul +
  ave_ce_used_ps_bgr_128_ul + ave_ce_used_ps_bgr_256_ul + ave_ce_used_ps_bgr_384_ul
  ) as AVE_CE_USED_BGR_UL,
  SUM(ave_ce_used_ps_bgr_8_dl +  ave_ce_used_ps_bgr_16_dl + ave_ce_used_ps_bgr_32_dl + ave_ce_used_ps_bgr_64_dl +
  ave_ce_used_ps_bgr_128_dl + ave_ce_used_ps_bgr_256_dl + ave_ce_used_ps_bgr_384_dl
  ) as AVE_CE_USED_BGR_DL,
  SUM(ave_ce_used_ps_int_8_ul +  ave_ce_used_ps_int_16_ul + ave_ce_used_ps_int_32_ul + ave_ce_used_ps_int_64_ul +
  ave_ce_used_ps_int_128_ul + ave_ce_used_ps_int_256_ul + ave_ce_used_ps_int_384_ul
  ) as AVE_CE_USED_INT_UL,
  SUM(ave_ce_used_ps_int_8_dl +  ave_ce_used_ps_int_16_dl + ave_ce_used_ps_int_32_dl + ave_ce_used_ps_int_64_dl +
  ave_ce_used_ps_int_128_dl + ave_ce_used_ps_int_256_dl + ave_ce_used_ps_int_384_dl
  ) as AVE_CE_USED_INT_DL,
  SUM(CE_SAMPLE_AMOUNT) as CE_SAMPLE_AMOUNT,
  
  sum(RTWP_CLASS_0 ) as RTWP_CLASS_0 ,
  sum(RTWP_CLASS_1 ) as RTWP_CLASS_1 ,
  sum(RTWP_CLASS_2 ) as RTWP_CLASS_2 ,
  sum(RTWP_CLASS_3 ) as RTWP_CLASS_3 ,
  sum(RTWP_CLASS_4 ) as RTWP_CLASS_4 ,
  sum(RTWP_CLASS_5 ) as RTWP_CLASS_5 ,
  sum(RTWP_CLASS_6 ) as RTWP_CLASS_6 ,
  sum(RTWP_CLASS_7 ) as RTWP_CLASS_7 ,
  sum(RTWP_CLASS_8 ) as RTWP_CLASS_8 ,
  sum(RTWP_CLASS_9 ) as RTWP_CLASS_9 ,
  sum(RTWP_CLASS_10) as RTWP_CLASS_10,
  sum(RTWP_CLASS_11) as RTWP_CLASS_11,
  sum(RTWP_CLASS_12) as RTWP_CLASS_12,
  sum(RTWP_CLASS_13) as RTWP_CLASS_13,
  sum(RTWP_CLASS_14) as RTWP_CLASS_14,
  sum(RTWP_CLASS_15) as RTWP_CLASS_15,
  sum(RTWP_CLASS_16) as RTWP_CLASS_16,
  sum(RTWP_CLASS_17) as RTWP_CLASS_17,
  sum(RTWP_CLASS_18) as RTWP_CLASS_18,
  sum(RTWP_CLASS_19) as RTWP_CLASS_19,
  sum(RTWP_CLASS_20) as RTWP_CLASS_20,
  sum(RTWP_CLASS_21) as RTWP_CLASS_21,
  sum(AVAIL_WCELL_IN_WO_STATE) as AVAIL_WCELL_IN_WO_STATE,
  sum(AVAIL_WCELL_EXISTS_IN_RNW_DB)as AVAIL_WCELL_EXISTS_IN_RNW_DB,
  sum(AVAIL_WCELL_BLOCKED_BY_USER)as AVAIL_WCELL_BLOCKED_BY_USER
  
  from NOKRWW_PS_CELLRES_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME "
  
  qry6 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(NRT_EDCH_UL_DATA_VOL) as NRT_EDCH_UL_DATA_VOL,
  SUM(HS_DSCH_DATA_VOL) as HS_DSCH_DATA_VOL,
  SUM(INTERA_DL_DATA + BGR_DL_DATA) as VOL_DL,
  SUM(INTERA_UL_DATA + BGR_UL_DATA) as VOL_UL
  
  from NOKRWW_PS_CELLTP_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry7 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(HSDPA_ORIG_DATA) as HSDPA_ORIG_DATA
  
  from NOKRWW_PS_CELTPW_MNC1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  
  qry8 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(HSDPA_BUFF_WITH_DATA_PER_TTI) as HSDPA_BUFF_WITH_DATA_PER_TTI,
  SUM(MACE_PDU_DATA_2MS_TTI) as MACE_PDU_DATA_2MS_TTI,
  SUM(MACE_PDU_DATA_10MS_TTI) as MACE_PDU_DATA_10MS_TTI,
  SUM(MACE_PDUS_2MS_TTI) as MACE_PDUS_2MS_TTI,
  SUM(MACE_PDUS_10MS_TTI) as MACE_PDUS_10MS_TTI
  
  from NOKRWW_PS_HSDPAW_MNC1_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  qry9 <- "select to_char(period_start_time, 'granul') as DIA,
  (CO_NAME) as CELDA,
  
  SUM(SUCC_UPDATES_ON_SHO_FOR_RT) as SUCC_UPDATES_ON_SHO_FOR_RT,
  SUM(SUCC_UPDATES_ON_SHO_FOR_NRT) as SUCC_UPDATES_ON_SHO_FOR_NRT,
  SUM(CELL_ADD_REQ_ON_SHO_FOR_RT) as  CELL_ADD_REQ_ON_SHO_FOR_RT,
  SUM(CELL_REPL_REQ_ON_SHO_FOR_RT) as CELL_REPL_REQ_ON_SHO_FOR_RT,
  SUM(CELL_DEL_REQ_ON_SHO_FOR_RT) as CELL_DEL_REQ_ON_SHO_FOR_RT,
  SUM(CELL_ADD_REQ_ON_SHO_FOR_NRT) as CELL_ADD_REQ_ON_SHO_FOR_NRT,
  SUM(CELL_REPL_REQ_ON_SHO_FOR_NRT) as CELL_REPL_REQ_ON_SHO_FOR_NRT,
  SUM(CELL_DEL_REQ_ON_SHO_FOR_NRT) as CELL_DEL_REQ_ON_SHO_FOR_NRT,
  SUM(UNSUCC_UPDATES_ON_SHO_NRT) as UNSUCC_UPDATES_ON_SHO_NRT,
  SUM(UNSUCC_UPDATES_ON_SHO_FOR_RT) as UNSUCC_UPDATES_ON_SHO_FOR_RT
  
  from NOKRWW_PS_SOFTHO_MNC_RAW,UTP_COMMON_OBJECTS
  WHERE (period_start_time >= to_date('idate', 'YYYY-MM-DD')) 
  AND (period_start_time < to_date('fdate', 'YYYY-MM-DD'))
  AND (wcel_id = CO_GID) 
  
  GROUP BY to_char(period_start_time, 'granul'),CO_NAME"
  
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  } 
  
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8,qry9)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 9
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('NOKIA',uid="pmr",pwd="pmr")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    #    result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datanok3G <- outputPar[[1]]
  for (i in 2:length(outputPar)){datanok3G <- datanok3G %>% full_join(outputPar[[i]])}
  
  datanok3G$CELDA <- substr(datanok3G$CELDA,1,7)
  datanok3G$RNC <- "RNC_NOK"
   ####Funciones####
  datanok3G[is.na(datanok3G)] <- 0
  datanok2G[is.na(datanok2G)] <- 0
  
  datanok3G$NRT_EDCH_UL_DATA_VOL <- as.numeric(as.character(datanok3G$NRT_EDCH_UL_DATA_VOL))
  datanok3G$HS_DSCH_DATA_VOL <- as.numeric(as.character(datanok3G$HS_DSCH_DATA_VOL))
  datanok2G$AVE_TCH_BUSY_HALF <- as.numeric(as.character(datanok2G$AVE_TCH_BUSY_HALF))
  datanok2G$AVE_TCH_BUSY_FULL <- as.numeric(as.character(datanok2G$AVE_TCH_BUSY_FULL))
  
  
  #datos2G <- readRDS("F:/Leo/Programacion/WorkSpace R/Shiny/DataCharge/datosNok2G.rds")
  #datos3G <- readRDS("F:/Leo/Programacion/WorkSpace R/Shiny/DataCharge/datosNok3G.rds")
  #
  #datos2G$DIA <- as.Date(datos2G$DIA)
  #datos3G$DIA <- as.Date(datos3G$DIA)
  #
  #datos2G <- datos2G %>% filter(DIA >= idate,DIA < fdate) %>% inner_join(datos) 
  #datos3G <- datos3G %>% filter(DIA >= idate,DIA < fdate) %>% inner_join(datos) 
  #
  #datos2G$DIA <- as.factor(datos2G$DIA)
  #datos3G$DIA <- as.factor(datos3G$DIA)
  
  datanok2G <- datanok2G %>% inner_join(datos)
  datanok3G <- datanok3G %>% inner_join(datos)
  
  datanok2G$DIA <- as.factor(datanok2G$DIA)
  datanok3G$DIA <- as.factor(datanok3G$DIA)
  
  return(list("nokia2G"= datanok2G,"nokia3G"=datanok3G))
}
queryHuawei <- function(idate,fdate,datos,granul){
  
  
  ####2G####
  qry1 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(K3000) AS K3000,
  SUM(K3003) AS K3003,
  SUM(K3001) AS K3001,
  SUM(K3010A) AS K3010A,
  SUM(K3011A) AS K3011A,
  SUM(K3013A) AS K3013A,
  SUM(K3040) AS K3040,
  MAX(K3045) AS K3045,
  SUM(K3014)  AS K3014,
  SUM(K3004)  AS K3004,
  SUM(K3014 - K3034) AS K3014MINUSK3034,
  SUM(K3034) as K3034
  FROM STATDBA.OSS_HUA_KPICEL_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  
  qry2 <-"SELECT  OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(CM30) AS CM30,
  SUM(CM33) AS CM33
  FROM  STATDBA.OSS_HUA_CDROP_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  
  qry3 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(CH363) AS CH363
  FROM STATDBA.OSS_HUA_IINTERRAT_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  
  qry4<-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(CH303) AS CH303
  FROM STATDBA.OSS_HUA_INTRAHCEL_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  qry5 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(CH333) AS CH333
  FROM STATDBA.OSS_HUA_OEINTERHC_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  qry6 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(CH353) AS CH353
  FROM STATDBA.OSS_HUA_OINTERRAT_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  qry7 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(CH313) AS CH313
  FROM STATDBA.OSS_HUA_OIINTERHC_V2
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  qry8 <-"SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(A330) AS A330,
  SUM(A331) AS A331,
  SUM(A337) AS A337,
  SUM(A338) AS A338,
  SUM(A339) AS A339,
  SUM(A340) AS A340
  FROM STATDBA.OSS_HUA_PAGING_ABIS
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),EXCHANGE_ID,OBJ"
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  } 
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 8
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    #    result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  
  datahua2G <- outputPar[[1]]
  for (i in 2:length(outputPar)){datahua2G <- datahua2G %>% full_join(outputPar[[i]])}
  datahua2G$CELDA<-substr(datahua2G$CELDA,7,stop=unlist(lapply(gregexpr(',',datahua2G$CELDA), `[[`, 1))-1)
  
  #datos2G <- readRDS("F:/Leo/Programacion/WorkSpace R/Shiny/DataCharge/datosHua2G.rds")
  #datos3G <- readRDS("F:/Leo/Programacion/WorkSpace R/Shiny/DataCharge/datosHua3G.rds")
  #
  #datos2G$DIA <- as.Date(datos2G$DIA)
  #datos3G$DIA <- as.Date(datos3G$DIA)
  #
  #
  #datos2G <- datos2G %>% filter(DIA >= idate,DIA < fdate) %>% inner_join(datos) 
  #datos3G <- datos3G %>% filter(DIA >= idate,DIA < fdate) %>% inner_join(datos) 
  #
  #datos2G$DIA <- as.factor(datos2G$DIA)
  #datos3G$DIA <- as.factor(datos3G$DIA)
  
  ####3G####
  qry1<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1) AS RRC_CONN_SETUP,
  SUM(C2) AS RRC_CONN_ATT,
  SUM(C3) AS RRC_CONN_FAIL,
  SUM(C4) AS SetupConnEstab,
  SUM(C5) AS PS_DROP_FD_DEN,
  SUM(C6) AS PS_DROP_FD_NUM,
  SUM(C7) AS DEN_AMR_Call_DROP,
  SUM(C8) AS DEN_AMR_RAB,
  SUM(C9) AS DEN_CS_RRC,
  SUM(C10) AS DEN_PS_CALL,
  SUM(C11) AS DEN_PS_RAB,
  SUM(C12) AS DEN_PS_RRC,
  SUM(C13) AS NUM_AMR_Call_DROP,
  SUM(C14) AS NUM_AMR_RAB,
  SUM(C15) AS NUM_CS_RRC,
  SUM(C16) AS NUM_PS_CALL,
  SUM(C17) AS NUM_PS_RAB,
  SUM(C18) AS NUM_PS_RRC
  
  FROM STATDBA.OSS_HUA_RRC_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry2<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1) AS FailConnEstb_Cong,
  SUM(C2) AS CODE_CONG_RRC,
  SUM (C3 + C6) as CE_CONG_RRC,
  SUM (C5 + C8) as PWR_CONG_RRC,
  SUM (C4 + C7) as IUB_CONG_RRC 
  FROM STATDBA.OSS_HUA_RRC_REJECT
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry3<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C3) AS Cong_PS_RAB,
  SUM(C5 + C6 + C7 + C10) as PS_RAB_ATT
  
  FROM STATDBA.OSS_HUA_PS_RAB_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD '))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry4<- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1) as CODE_PS_CONG_RAB,
  SUM(C2) as CELL_PS_CONG_RAB,
  SUM (C3 + C8 ) as CE_PS_CONG_RAB,
  SUM (C4 + C9 ) as IUB_PS_CONG_RAB,
  SUM (C5 + C10 ) as PWR_PS_CONG_RAB,
  SUM (C5) as PWR_PS_CONG_RAB_DL,
  SUM (C10) as PWR_PS_CONG_RAB_UL
  
  FROM STATDBA.OSS_HUA_PS_RAB_FAIL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry5 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C2) AS RAB_CS_ATT
  
  FROM STATDBA.OSS_HUA_CS_RAB_SETUP
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry6 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1) as CODE_CS_CONG_RAB,
  SUM(C2) as CELL_CS_CONG_RAB,
  SUM (C3 + C6 ) as CE_CS_CONG_RAB,
  SUM (C4 + C7 ) as IUB_CS_CONG_RAB,
  SUM (C5 + C8 ) as PWR_CS_CONG_RAB,
  SUM( C5) as PWR_CS_CONG_RAB_DL,
  SUM( C8) as PWR_CS_CONG_RAB_UL
  
  FROM STATDBA.OSS_HUA_CS_RAB_FAIL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry7 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C4)  AS  TRAFICO_CS
  FROM STATDBA.OSS_HUA_RADIO_BEARER
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry8 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  MAX(C3) AS MAX_USER_HSDPA,
  AVG(C3) AS AVG_USER_HSDPA,
  MAX (C4)  AS MAX_TROUGHPUT_HSDPA,
  AVG (C4)  AS AVG_TROUGHPUT_HSDPA,
  SUM (C7) AS TOTAL_VOLUMEN_HSDPA
  FROM STATDBA.OSS_HUA_HSDPA_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry9 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM (C6) AS TOTAL_VOLUMEN_HSUPA,
  MAX (C2) AS MAX_USER_HSUPA,
  AVG(C2) AS AVG_USER_HSUPA,
  MAX (C3)  AS MAX_TROUGHPUT_HSUPA,
  AVG (C3)  AS AVG_TROUGHPUT_HSUPA
  
  FROM STATDBA.OSS_HUA_HSUPA_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  
  qry10 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C1)	AS CS_DROP_A,				
  SUM(C2)	AS CS_DROP_N,				
  SUM(C3)	AS PS_DROP_A,				
  SUM(C4)	AS PS_DROP_N				
  FROM STATDBA.OSS_HUA_RAB_REL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry11 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  AVG(C11) as RTWP
  FROM STATDBA.OSS_HUA_RTWP_CELL
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry12 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  SUM(C27) as SUM_USER_DL,
  AVG(C27) as MAX_USER_DL,
  SUM(C28) as SUM_USER_UL,
  AVG(C28) as MAX_USER_UL,
  AVG(C1) as CPICH_PWR,
  SUM (C42) as CPICH_BY_RTWP,
  SUM(C10) as  LDR_PWR_DL,
  SUM(C13) as LDR_PWR_UL,
  sum(C112) as SUCC_FAST_DORM,
  sum(C105) as ATT_FAST_DORM
  
  
  FROM STATDBA.OSS_HUA_EQV_USER
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID "
  
  qry13 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  
  SUM(C8) as PAGING_DISCARD,
  SUM(C9) as PAGING_SENT
  FROM STATDBA.OSS_HUA_PAGING_3G
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  qry14 <- "SELECT   OBJ as CELDA, to_char(DATETIME, 'granul') AS Dia,EXCHANGE_ID,
  
  MAX(C1) as MAX_FACH,
  SUM(C2) as CELL_DCH,
  SUM(C3) as CELL_FACH,
  SUM(C4) as CELL_PCH
  
  FROM STATDBA.OSS_HUA_RRC_STATUS
  WHERE (DATETIME >= to_date('idate', 'YYYY-MM-DD')) 
  AND (DATETIME < to_date('fdate', 'YYYY-MM-DD'))
  GROUP BY to_char(DATETIME, 'granul'),OBJ,EXCHANGE_ID"
  
  
  paramtoqry <- function(qry,granul,fdate,idate){
    gsub("granul",granul,sub("fdate",fdate,sub("idate",idate,qry)))
  } 
  qrys <- list(qry1,qry2,qry3,qry4,qry5,qry6,qry7,qry8,qry9,qry10,qry11,qry12,qry13,qry14)
  qrys <- lapply(qrys[],paramtoqry,granul=granul,fdate=fdate,idate=idate)
  
  no_cores <- 14
  registerDoParallel(no_cores)
  
  outputPar <- foreach(i = 1:length(qrys), .packages="RODBC",.export = "qrys")%dopar%{
    #Connecting to database through RODBC
    con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
    options(dec=",")
    #Test connection
    #odbcGetInfo(ch)
    result<- sqlQuery(con, qrys[i])
    #    result<-sqlQuery(ch,paste('SELECT * FROM ',databases[i]))  
    odbcClose(con)
    result
  }
  
  stopImplicitCluster()
  
  datahua3G <- outputPar[[1]]
  for (i in 2:length(outputPar)){datahua3G <- datahua3G %>% full_join(outputPar[[i]])}
  
  ####FUNCIONES####
  datahua2G[is.na(datahua2G)] <- 0
  datahua3G[is.na(datahua3G)] <- 0
  
  datahua2G$K3014 <- as.numeric(as.character(datahua2G$K3014))
  datahua2G$K3004 <- as.numeric(as.character(datahua2G$K3004))
  datahua2G$K3014MINUSK3034 <- as.numeric(as.character(datahua2G$K3014MINUSK3034))
  datahua2G$K3034 <- as.numeric(as.character(datahua2G$K3034))
 
  datahua3G$CONG_PS_RAB <- as.numeric(as.character(datahua3G$CONG_PS_RAB))
  datahua3G$TRAFICO_CS <- as.numeric(as.character(datahua3G$TRAFICO_CS))
  datahua3G$MAX_TROUGHPUT_HSDPA <- as.numeric(as.character(datahua3G$MAX_TROUGHPUT_HSDPA))
  datahua3G$AVG_TROUGHPUT_HSDPA <- as.numeric(as.character(datahua3G$AVG_TROUGHPUT_HSDPA))
  datahua3G$TOTAL_VOLUMEN_HSDPA <- as.numeric(as.character(datahua3G$TOTAL_VOLUMEN_HSDPA))
  datahua3G$TOTAL_VOLUMEN_HSUPA <- as.numeric(as.character(datahua3G$TOTAL_VOLUMEN_HSUPA))
  datahua3G$MAX_TROUGHPUT_HSUPA <- as.numeric(as.character(datahua3G$MAX_TROUGHPUT_HSUPA))
  datahua3G$AVG_TROUGHPUT_HSUPA <- as.numeric(as.character(datahua3G$AVG_TROUGHPUT_HSUPA))
  datahua3G$SUM_USER_DL <- as.numeric(as.character(datahua3G$SUM_USER_DL))
  datahua3G$MAX_USER_DL <- as.numeric(as.character(datahua3G$MAX_USER_DL))
  datahua3G$SUM_USER_UL <- as.numeric(as.character(datahua3G$SUM_USER_UL))
  datahua3G$MAX_USER_UL <- as.numeric(as.character(datahua3G$MAX_USER_UL))
  datahua3G$RTWP <- as.numeric(as.character(datahua3G$RTWP))
  
  datahua3G$CELL_DCH <- as.numeric(as.character(datahua3G$CELL_DCH))
  datahua3G$CELL_PCH <- as.numeric(as.character(datahua3G$CELL_PCH))
  datahua3G$CELL_FACH <- as.numeric(as.character(datahua3G$CELL_FACH))
  
  datahua3G$PAGING_SENT <- as.numeric(as.character(datahua3G$PAGING_SENT))
  
  datahua2G[is.na(datahua2G)] <- 0
  datahua3G[is.na(datahua3G)] <- 0
  
  qry <- "SELECT * FROM EXCHANGE_ID"
  con <- odbcConnect('SIGESTOP',uid="felix",pwd="F3l!x2k10")
  options(dec=",")
  EXCHANGE_ID <- sqlQuery(con, qry)
  odbcClose(con)
  
  datahua3G <-datahua3G%>%inner_join(EXCHANGE_ID %>% select(ID,CODE),by =c("EXCHANGE_ID" = "CODE"))%>%mutate(RNC=ID)
  datahua2G <-datahua2G%>%inner_join(EXCHANGE_ID %>% select(ID,CODE),by =c("EXCHANGE_ID" = "CODE"))%>%mutate(BSC=ID)
  
  datahua2G <- datahua2G %>% inner_join(datos)
  datahua3G <- datahua3G %>% inner_join(datos)
  
  
  datahua2G$DIA <- as.factor(datahua2G$DIA)
  datahua3G$DIA <- as.factor(datahua3G$DIA)
  
  return(list("huawei2G"= datahua2G,"huawei3G"=datahua3G))
}

renderGraf <- function(dataP,index){
  granul <- "yyyy-mm-dd"
  
  if(granul == "yyyy-mm-dd"){
  tformat <- "%Y-%m-%d"
  }
   
  if(granul == "yyyy-mm-dd hh"){
  tformat <- "%Y-%m-%d %H"
  }
  
  cell.vcl <- levels(factor(unlist(dataP[,2]), ordered = TRUE))
  #              mydate <- strptime(levels(factor(unlist(datos[,1]$Date_h), ordered = TRUE)),format = "%Y-%m-%d %H")
  mydate <- strptime(levels(factor(unlist(dataP[,1]), ordered = TRUE)),format = tformat,tz="EST")
  mydata <- data.frame(matrix(0, nrow = length(mydate), ncol = 0))
  for (i in 1:length(cell.vcl)) {
    x <- as.numeric(dim(dataP[str_detect(unlist(dataP[,2]),cell.vcl[i]),index])[1])
    y <- as.numeric(length(mydate))
    
    data <- dataP
    if (dim(dataP[str_detect(unlist(dataP[,2]),cell.vcl[i]),index])[1] == length(mydate))
    {mydata <- cbind(mydata,dataP[str_detect(unlist(dataP[,2]),cell.vcl[i]),index])} 
    else
    {
      tempdata <- data.frame(matrix(0, nrow = length(mydate), ncol = 1))
      mydata <- cbind(mydata,tempdata)
    }
    colnames(mydata)[i] <- cell.vcl[i]
  }
  
  return(list("mydata"= mydata, "mydate"=mydate))
}
rendermygraphOK <- function (mylist,value){
  
  granul <- "yyyy-mm-dd"
  
  if(value == "P"){
    return(renderDygraph({
      d1 <- dygraph(as.xts(mylist$mydata,order.by = mylist$mydate, tz="GMT"))%>%
        dyLegend(show = "always", hideOnMouseOut = TRUE, width = 400) %>%
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2 ,hideOnMouseOut = FALSE) %>%
        dyRangeSelector(fillColor = "green", strokeColor = "black") 
        
        

      d1$x$css = "
                  .dygraph-legend > span.highlight { display: inline; }
                  .dygraph-legend > span.highlight { border: 1px solid grey; }
                            "
      
      d1
    }))
  }
  if(value == "T"){
    return(renderDygraph({
      
      g <- granul
      
      if(granul == "yyyy-mm-dd"){
        tformat <- "%Y-%m-%d"
      }
      
      if(granul == "yyyy-mm-dd hh"){
        tformat <- "%Y-%m-%d %H"
      }
      
      day <- strptime(levels(factor(unlist(mylist[,1]$DIA), ordered = TRUE)),format = tformat)
      #day<- as.Date(day$DIA)
      mylist <- mylist[,-1]
      
      d1 <- dygraph(as.xts(mylist,order.by = day,tz="GMT"))%>%
        dyLegend(show = "always", hideOnMouseOut = TRUE, width = 400) %>%
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2 ,hideOnMouseOut = FALSE) %>%
        dyRangeSelector(fillColor = "green", strokeColor = "black") 
      
      d1$x$css = "
                  .dygraph-legend > span.highlight { display: inline; }
                  .dygraph-legend > span.highlight { border: 1px solid grey; }
                  
      "
      d1
      
    }))
  }
  
}

  #####SERVER####
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #datos.2GEri <- data.frame()
  datos.Hua <- list()
  datos.Eri <- list()
  datos.Nok <- list()
  datoSigestop <- data.frame()
  
  observeEvent(input$query, {
    ###Eventos################
    gra <- "yyyy-mm-dd"
    datoSigestop <<- sigestop()
    
    withProgress(message ='Ejecutando Consulta: Ericsson',value = 0.5,{
      
      datos.Eri <<- queryEricsson(as.character(input$idate),as.character(input$fdate),datoSigestop,gra)
      #Event Ericsson 2G
    })
    
    withProgress(message ='Ejecutando Consulta: Huawei',value = 0.5,{
      #Event Huawei 2G
      datos.Hua<<- queryHuawei(as.character(input$idate),as.character(input$fdate),datoSigestop,gra)
      
    })
    
    withProgress(message ='Ejecutando Consulta: Nokia',value = 0.5,{
      
      datos.Nok<<- queryNokia(as.character(input$idate),as.character(input$fdate),datoSigestop,gra)
      #Event NOKIA 3G
    })
    
    
    
    
   
    #id <- showNotification("Filtrando datos.",duration =NULL)
    withProgress(message ='Filtrando 2G',value = 0.5,{
      ####2G
      ####DAY####
      dataTec2GD <- createFrame(datos.Hua$huawei2G,datos.Eri$ericsson2G,datos.Nok$nokia2G,"day")
      dataTec2GD_F <- FinalFrame(dataTec2GD,"day")
      
      datosRed2G <- dataTec2GD_F$datosRed %>% group_by(DIA)
      datosHua2G <- dataTec2GD_F$datosHua %>% group_by(DIA)
      datosEri2G <-dataTec2GD_F$datosEri %>% group_by(DIA)
      datosNok2G <-dataTec2GD_F$datosNok %>% group_by(DIA)
      
      output$kpi2GR <- rendermygraphOK(datosRed2G[c(1,2,3,4)],"T")
      output$kpi2GH <- rendermygraphOK(datosHua2G[c(1,2,3,4)],"T")
      output$kpi2GE <- rendermygraphOK(datosEri2G[c(1,2,3,4)],"T")
      output$kpi2GN <- rendermygraphOK(datosNok2G[c(1,2,3,4)],"T")
      
      output$traf2GR <- rendermygraphOK(datosRed2G[c(1,5,6)],"T")
      output$traf2GH <- rendermygraphOK(datosHua2G[c(1,5,6)],"T")
      output$traf2GE <- rendermygraphOK(datosEri2G[c(1,5,6)],"T")
      output$traf2GN <- rendermygraphOK(datosNok2G[c(1,5,6)],"T")
      #
      output$drop2GR <- rendermygraphOK(datosRed2G[c(1,7,8)],"T")
      output$drop2GH <- rendermygraphOK(datosHua2G[c(1,7,8)],"T")
      output$drop2GE <- rendermygraphOK(datosEri2G[c(1,7,8)],"T")
      output$drop2GN <- rendermygraphOK(datosNok2G[c(1,7,8)],"T")
      
      output$pag2GR <- rendermygraphOK(datosRed2G[c(1,9)],"T")
      output$pag2GH <- rendermygraphOK(datosHua2G[c(1,9)],"T")
      output$pag2GE <- rendermygraphOK(datosEri2G[c(1,9)],"T")
      output$pag2GN <- rendermygraphOK(datosNok2G[c(1,9)],"T")
      
      output$cong2GR <- rendermygraphOK(datosRed2G[c(1,10,11)],"T")
      output$cong2GH <- rendermygraphOK(datosHua2G[c(1,10,11)],"T")
      output$cong2GE <- rendermygraphOK(datosEri2G[c(1,10,11)],"T")
      output$cong2GN <- rendermygraphOK(datosNok2G[c(1,10,11)],"T")
      
      output$hov2GR <- rendermygraphOK(datosRed2G[c(1,12)],"T")
      output$hov2GH <- rendermygraphOK(datosHua2G[c(1,12)],"T")
      output$hov2GE <- rendermygraphOK(datosEri2G[c(1,12)],"T")
      output$hov2GN <- rendermygraphOK(datosNok2G[c(1,12)],"T")
      
      output$pagR2GR <- rendermygraphOK(datosRed2G[c(1,13)],"T")
      output$pagR2GH <- rendermygraphOK(datosHua2G[c(1,13)],"T")
      output$pagR2GE <- rendermygraphOK(datosEri2G[c(1,13)],"T")
      output$pagR2GN <- rendermygraphOK(datosNok2G[c(1,13)],"T")
      
      ####PROVINCIA####
      dataTec2GP <- createFrame(datos.Hua$huawei2G,datos.Eri$ericsson2G,datos.Nok$nokia2G,"prov")
      dataTec2GP_F <- FinalFrame(dataTec2GP,"prov")
      
      datosRed2GP <- as.data.frame(dataTec2GP_F$datosRed) %>% group_by(DIA)
      datosHua2GP <- as.data.frame(dataTec2GP_F$datosHua)%>% group_by(DIA)
      datosEri2GP <-as.data.frame(dataTec2GP_F$datosEri)%>% group_by(DIA)
      datosNok2GP <-as.data.frame(dataTec2GP_F$datosNok)%>% group_by(DIA)
      
      
      ##ACC
      output$accP2GR <- rendermygraphOK(renderGraf(datosRed2GP,3),"P")
      output$accP2GE <- rendermygraphOK(renderGraf(datosEri2GP,3),"P")
      output$accP2GH <- rendermygraphOK(renderGraf(datosHua2GP,3),"P")
      output$accP2GN <- rendermygraphOK(renderGraf(datosNok2GP,3),"P")
      ##RET
      output$retP2GR <- rendermygraphOK(renderGraf(datosRed2GP,4),"P")
      output$retP2GE <- rendermygraphOK(renderGraf(datosEri2GP,4),"P")
      output$retP2GH <- rendermygraphOK(renderGraf(datosHua2GP,4),"P")
      output$retP2GN <- rendermygraphOK(renderGraf(datosNok2GP,4),"P")
      ##SER
      output$serP2GR <- rendermygraphOK(renderGraf(datosRed2GP,5),"P")
      output$serP2GE <- rendermygraphOK(renderGraf(datosEri2GP,5),"P")
      output$serP2GH <- rendermygraphOK(renderGraf(datosHua2GP,5),"P")
      output$serP2GN <- rendermygraphOK(renderGraf(datosNok2GP,5),"P")
      ##TTCH
      output$ttchP2GR <- rendermygraphOK(renderGraf(datosRed2GP,6),"P")
      output$ttchP2GE <- rendermygraphOK(renderGraf(datosEri2GP,6),"P")
      output$ttchP2GH <- rendermygraphOK(renderGraf(datosHua2GP,6),"P")
      output$ttchP2GN <- rendermygraphOK(renderGraf(datosNok2GP,6),"P")
      ##TSDCCH
      output$tsdcP2GR <- rendermygraphOK(renderGraf(datosRed2GP,7),"P")
      output$tsdcP2GE <- rendermygraphOK(renderGraf(datosEri2GP,7),"P")
      output$tsdcP2GH <- rendermygraphOK(renderGraf(datosHua2GP,7),"P")
      output$tsdcP2GN <- rendermygraphOK(renderGraf(datosNok2GP,7),"P")
      
      ##DTCH
      output$dtchP2GR <- rendermygraphOK(renderGraf(datosRed2GP,8),"P")
      output$dtchP2GE <- rendermygraphOK(renderGraf(datosEri2GP,8),"P")
      output$dtchP2GH <- rendermygraphOK(renderGraf(datosHua2GP,8),"P")
      output$dtchP2GN <- rendermygraphOK(renderGraf(datosNok2GP,8),"P")
      ##DSDCCH
      output$dsdcP2GR <- rendermygraphOK(renderGraf(datosRed2GP,9),"P")
      output$dsdcP2GE <- rendermygraphOK(renderGraf(datosEri2GP,9),"P")
      output$dsdcP2GH <- rendermygraphOK(renderGraf(datosHua2GP,9),"P")
      output$dsdcP2GN <- rendermygraphOK(renderGraf(datosNok2GP,9),"P")
      
      ##PAGING
      output$pagP2GR <- rendermygraphOK(renderGraf(datosRed2GP,10),"P")
      output$pagP2GE <- rendermygraphOK(renderGraf(datosEri2GP,10),"P")
      output$pagP2GH <- rendermygraphOK(renderGraf(datosHua2GP,10),"P")
      output$pagP2GN <- rendermygraphOK(renderGraf(datosNok2GP,10),"P")
      
      ##CTCH
      output$ctchP2GR <- rendermygraphOK(renderGraf(datosRed2GP,11),"P")
      output$ctchP2GE <- rendermygraphOK(renderGraf(datosEri2GP,11),"P")
      output$ctchP2GH <- rendermygraphOK(renderGraf(datosHua2GP,11),"P")
      output$ctchP2GN <- rendermygraphOK(renderGraf(datosNok2GP,11),"P")
      
      ##CSDCCH
      output$csdcP2GR <- rendermygraphOK(renderGraf(datosRed2GP,12),"P")
      output$csdcP2GE <- rendermygraphOK(renderGraf(datosEri2GP,12),"P")
      output$csdcP2GH <- rendermygraphOK(renderGraf(datosHua2GP,12),"P")
      output$csdcP2GN <- rendermygraphOK(renderGraf(datosNok2GP,12),"P")
      
      
      ##HANDOVER
      output$hovP2GR <- rendermygraphOK(renderGraf(datosRed2GP,13),"P")
      output$hovP2GE <- rendermygraphOK(renderGraf(datosEri2GP,13),"P")
      output$hovP2GH <- rendermygraphOK(renderGraf(datosHua2GP,13),"P")
      output$hovP2GN <- rendermygraphOK(renderGraf(datosNok2GP,13),"P")
      
      ##PAGING_RATE
      output$pagRP2GR <- rendermygraphOK(renderGraf(datosRed2GP,14),"P")
      output$pagRP2GE <- rendermygraphOK(renderGraf(datosEri2GP,14),"P")
      output$pagRP2GH <- rendermygraphOK(renderGraf(datosHua2GP,14),"P")
      output$pagRP2GN <- rendermygraphOK(renderGraf(datosNok2GP,14),"P")
      
      ####CONTROLADOR####
      dataTec2GC <- createFrame(datos.Hua$huawei2G,datos.Eri$ericsson2G,datos.Nok$nokia2G,"cont")
      dataTec2GC_F <- FinalFrame(dataTec2GC,"cont")
      
      datosRed2GC <- as.data.frame(dataTec2GC_F$datosRed) %>% group_by(DIA)
      datosHua2GC <- as.data.frame(dataTec2GC_F$datosHua)%>% group_by(DIA)
      datosEri2GC <-as.data.frame(dataTec2GC_F$datosEri)%>% group_by(DIA)
      datosNok2GC <-as.data.frame(dataTec2GC_F$datosNok)%>% group_by(DIA)
      
      ##ACC
      output$accC2GR <- rendermygraphOK(renderGraf(datosRed2GC,3),"P")
      output$accC2GE <- rendermygraphOK(renderGraf(datosEri2GC,3),"P")
      output$accC2GH <- rendermygraphOK(renderGraf(datosHua2GC,3),"P")
      output$accC2GN <- rendermygraphOK(renderGraf(datosNok2GC,3),"P")
      ##RET
      output$retC2GR <- rendermygraphOK(renderGraf(datosRed2GC,4),"P")
      output$retC2GE <- rendermygraphOK(renderGraf(datosEri2GC,4),"P")
      output$retC2GH <- rendermygraphOK(renderGraf(datosHua2GC,4),"P")
      output$retC2GN <- rendermygraphOK(renderGraf(datosNok2GC,4),"P")
      ##SER
      output$serC2GR <- rendermygraphOK(renderGraf(datosRed2GC,5),"P")
      output$serC2GE <- rendermygraphOK(renderGraf(datosEri2GC,5),"P")
      output$serC2GH <- rendermygraphOK(renderGraf(datosHua2GC,5),"P")
      output$serC2GN <- rendermygraphOK(renderGraf(datosNok2GC,5),"P")
      ##TTCH
      output$ttchC2GR <- rendermygraphOK(renderGraf(datosRed2GC,6),"P")
      output$ttchC2GE <- rendermygraphOK(renderGraf(datosEri2GC,6),"P")
      output$ttchC2GH <- rendermygraphOK(renderGraf(datosHua2GC,6),"P")
      output$ttchC2GN <- rendermygraphOK(renderGraf(datosNok2GC,6),"P")
      ##TSDCCH
      output$tsdcC2GR <- rendermygraphOK(renderGraf(datosRed2GC,7),"P")
      output$tsdcC2GE <- rendermygraphOK(renderGraf(datosEri2GC,7),"P")
      output$tsdcC2GH <- rendermygraphOK(renderGraf(datosHua2GC,7),"P")
      output$tsdcC2GN <- rendermygraphOK(renderGraf(datosNok2GC,7),"P")
      
      ##DTCH
      output$dtchC2GR <- rendermygraphOK(renderGraf(datosRed2GC,8),"P")
      output$dtchC2GE <- rendermygraphOK(renderGraf(datosEri2GC,8),"P")
      output$dtchC2GH <- rendermygraphOK(renderGraf(datosHua2GC,8),"P")
      output$dtchC2GN <- rendermygraphOK(renderGraf(datosNok2GC,8),"P")
      ##DSDCCH
      output$dsdcC2GR <- rendermygraphOK(renderGraf(datosRed2GC,9),"P")
      output$dsdcC2GE <- rendermygraphOK(renderGraf(datosEri2GC,9),"P")
      output$dsdcC2GH <- rendermygraphOK(renderGraf(datosHua2GC,9),"P")
      output$dsdcC2GN <- rendermygraphOK(renderGraf(datosNok2GC,9),"P")
      ##PAGING
      output$pagC2GR <- rendermygraphOK(renderGraf(datosRed2GC,10),"P")
      output$pagC2GE <- rendermygraphOK(renderGraf(datosEri2GC,10),"P")
      output$pagC2GH <- rendermygraphOK(renderGraf(datosHua2GC,10),"P")
      output$pagC2GN <- rendermygraphOK(renderGraf(datosNok2GC,10),"P")
      
      ##CTCH
      output$ctchC2GR <- rendermygraphOK(renderGraf(datosRed2GC,11),"P")
      output$ctchC2GE <- rendermygraphOK(renderGraf(datosEri2GC,11),"P")
      output$ctchC2GH <- rendermygraphOK(renderGraf(datosHua2GC,11),"P")
      output$ctchC2GN <- rendermygraphOK(renderGraf(datosNok2GC,11),"P")
      ##CSDCCH
      output$csdcC2GR <- rendermygraphOK(renderGraf(datosRed2GC,12),"P")
      output$csdcC2GE <- rendermygraphOK(renderGraf(datosEri2GC,12),"P")
      output$csdcC2GH <- rendermygraphOK(renderGraf(datosHua2GC,12),"P")
      output$csdcC2GN <- rendermygraphOK(renderGraf(datosNok2GC,12),"P")
      
      ##HANDOVER
      output$hovC2GR <- rendermygraphOK(renderGraf(datosRed2GP,13),"P")
      output$hovC2GE <- rendermygraphOK(renderGraf(datosEri2GP,13),"P")
      output$hovC2GH <- rendermygraphOK(renderGraf(datosHua2GP,13),"P")
      output$hovC2GN <- rendermygraphOK(renderGraf(datosNok2GP,13),"P")
      
      ##PAGING_RATE
      output$pagRC2GR <- rendermygraphOK(renderGraf(datosRed2GP,14),"P")
      output$pagRC2GE <- rendermygraphOK(renderGraf(datosEri2GP,14),"P")
      output$pagRC2GH <- rendermygraphOK(renderGraf(datosHua2GP,14),"P")
      output$pagRC2GN <- rendermygraphOK(renderGraf(datosNok2GP,14),"P")
      
      
      
    })
    
    withProgress(message ='Filtrando Celda',value = 0.5,{
      
    #
    cell2G <- cell2G(datos.Hua$huawei2G,datos.Eri$ericsson2G,datos.Nok$nokia2G)
    #
    cell3G <- cell3G(datos.Hua$huawei3G,datos.Eri$ericsson3G,datos.Nok$nokia3G)
    })
    
    
   
    
    withProgress(message ='Filtrando 3G',value = 0.5,{
      ####3G####
      ####DAY####
      
      dataTec3GD <- createFrame3G(datos.Hua$huawei3G,datos.Eri$ericsson3G,datos.Nok$nokia3G,"day")
      dataTec3GD_F <- FinalFrame3G(dataTec3GD,"day")
      
      datosRed3G <- dataTec3GD_F$datosRed
      datosHua3G <- dataTec3GD_F$datosHua
      datosEri3G <-dataTec3GD_F$datosEri
      datosNok3G <-dataTec3GD_F$datosNok
      
      output$kpi3GR <- rendermygraphOK(datosRed3G[c(1,2,3,4,5)],"T")
      output$kpi3GH <- rendermygraphOK(datosHua3G[c(1,2,3,4,5)],"T")
      output$kpi3GE <- rendermygraphOK(datosEri3G[c(1,2,3,4,5)],"T")
      output$kpi3GN <- rendermygraphOK(datosNok3G[c(1,2,3,4,5)],"T")
      
      output$traf3GR <- rendermygraphOK(datosRed3G[c(1,6)],"T")
      output$traf3GH <- rendermygraphOK(datosHua3G[c(1,6)],"T")
      output$traf3GE <- rendermygraphOK(datosEri3G[c(1,6)],"T")
      output$traf3GN <- rendermygraphOK(datosNok3G[c(1,6)],"T")
      
      output$vol3GR <- rendermygraphOK(datosRed3G[c(1,7)],"T")
      output$vol3GH <- rendermygraphOK(datosHua3G[c(1,7)],"T")
      output$vol3GE <- rendermygraphOK(datosEri3G[c(1,7)],"T")
      output$vol3GN <- rendermygraphOK(datosNok3G[c(1,7)],"T")
      
      output$cong_rrc_3GR <- rendermygraphOK(datosRed3G[c(1,8,9,10)],"T")
      output$cong_rrc_3GH <- rendermygraphOK(datosHua3G[c(1,8,9,10)],"T")
      output$cong_rrc_3GE <- rendermygraphOK(datosEri3G[c(1,8,9,10)],"T")
      output$cong_rrc_3GN <- rendermygraphOK(datosNok3G[c(1,8,9,10)],"T")
      
      output$cong_rab_3GR <- rendermygraphOK(datosRed3G[c(1,11,12,13)],"T")
      output$cong_rab_3GH <- rendermygraphOK(datosHua3G[c(1,11,12,13)],"T")
      output$cong_rab_3GE <- rendermygraphOK(datosEri3G[c(1,11,12,13)],"T")
      output$cong_rab_3GN <- rendermygraphOK(datosNok3G[c(1,11,12,13)],"T")
      
      output$drop3GR <- rendermygraphOK(datosRed3G[c(1,14,15)],"T")
      output$drop3GH <- rendermygraphOK(datosHua3G[c(1,14,15)],"T")
      output$drop3GE <- rendermygraphOK(datosEri3G[c(1,14,15)],"T")
      output$drop3GN <- rendermygraphOK(datosNok3G[c(1,14,15)],"T")
      
      output$pag3GR <- rendermygraphOK(datosRed3G[c(1,17)],"T")
      output$pag3GH <- rendermygraphOK(datosHua3G[c(1,17)],"T")
      output$pag3GE <- rendermygraphOK(datosEri3G[c(1,17)],"T")
      output$pag3GN <- rendermygraphOK(datosNok3G[c(1,17)],"T")
      
      ####PROVINCIA####
      dataTec3GP <- createFrame3G(datos.Hua$huawei3G,datos.Eri$ericsson3G,datos.Nok$nokia3G,"prov")
      dataTec3GP_F <- FinalFrame3G(dataTec3GP,"prov")
      
      
      
      datosRed3GP <- dataTec3GP_F$datosRed%>% group_by(DIA)
      datosHua3GP <- dataTec3GP_F$datosHua%>% group_by(DIA)
      datosEri3GP <-dataTec3GP_F$datosEri%>% group_by(DIA)
      datosNok3GP <-dataTec3GP_F$datosNok%>% group_by(DIA)
      
      ##KPI
      output$acc_csP3GR <- rendermygraphOK(renderGraf(datosRed3GP,3),"P")
      output$acc_csP3GE <- rendermygraphOK(renderGraf(datosEri3GP,3),"P")
      output$acc_csP3GH <- rendermygraphOK(renderGraf(datosHua3GP,3),"P")
      output$acc_csP3GN <- rendermygraphOK(renderGraf(datosNok3GP,3),"P")
      
      output$acc_psP3GR <- rendermygraphOK(renderGraf(datosRed3GP,4),"P")
      output$acc_psP3GE <- rendermygraphOK(renderGraf(datosEri3GP,4),"P")
      output$acc_psP3GH <- rendermygraphOK(renderGraf(datosHua3GP,4),"P")
      output$acc_psP3GN <- rendermygraphOK(renderGraf(datosNok3GP,4),"P")
      
      output$ret_csP3GR <- rendermygraphOK(renderGraf(datosRed3GP,5),"P")
      output$ret_csP3GE <- rendermygraphOK(renderGraf(datosEri3GP,5),"P")
      output$ret_csP3GH <- rendermygraphOK(renderGraf(datosHua3GP,5),"P")
      output$ret_csP3GN <- rendermygraphOK(renderGraf(datosNok3GP,5),"P")
      
      output$ret_psP3GR <- rendermygraphOK(renderGraf(datosRed3GP,6),"P")
      output$ret_psP3GE <- rendermygraphOK(renderGraf(datosEri3GP,6),"P")
      output$ret_psP3GH <- rendermygraphOK(renderGraf(datosHua3GP,6),"P")
      output$ret_psP3GN <- rendermygraphOK(renderGraf(datosNok3GP,6),"P")
      
      output$trafP3GR <- rendermygraphOK(renderGraf(datosRed3GP,7),"P")
      output$trafP3GE <- rendermygraphOK(renderGraf(datosEri3GP,7),"P")
      output$trafP3GH <- rendermygraphOK(renderGraf(datosHua3GP,7),"P")
      output$trafP3GN <- rendermygraphOK(renderGraf(datosNok3GP,7),"P")
      
      output$volP3GR <- rendermygraphOK(renderGraf(datosRed3GP,8),"P")
      output$volP3GE <- rendermygraphOK(renderGraf(datosEri3GP,8),"P")
      output$volP3GH <- rendermygraphOK(renderGraf(datosHua3GP,8),"P")
      output$volP3GN <- rendermygraphOK(renderGraf(datosNok3GP,8),"P")
      
      output$ceRrcP3GR <- rendermygraphOK(renderGraf(datosRed3GP,9),"P")
      output$ceRrcP3GE <- rendermygraphOK(renderGraf(datosEri3GP,9),"P")
      output$ceRrcP3GH <- rendermygraphOK(renderGraf(datosHua3GP,9),"P")
      output$ceRrcP3GN <- rendermygraphOK(renderGraf(datosNok3GP,9),"P")
      
      output$pwrRrcP3GR <- rendermygraphOK(renderGraf(datosRed3GP,10),"P")
      output$pwrRrcP3GE <- rendermygraphOK(renderGraf(datosEri3GP,10),"P")
      output$pwrRrcP3GH <- rendermygraphOK(renderGraf(datosHua3GP,10),"P")
      output$pwrRrcP3GN <- rendermygraphOK(renderGraf(datosNok3GP,10),"P")
      
      output$codeRrcP3GR <- rendermygraphOK(renderGraf(datosRed3GP,11),"P")
      output$codeRrcP3GE <- rendermygraphOK(renderGraf(datosEri3GP,11),"P")
      output$codeRrcP3GH <- rendermygraphOK(renderGraf(datosHua3GP,11),"P")
      output$codeRrcP3GN <- rendermygraphOK(renderGraf(datosNok3GP,11),"P")
      
      output$ceRabP3GR <- rendermygraphOK(renderGraf(datosRed3GP,12),"P")
      output$ceRabP3GE <- rendermygraphOK(renderGraf(datosEri3GP,12),"P")
      output$ceRabP3GH <- rendermygraphOK(renderGraf(datosHua3GP,12),"P")
      output$ceRabP3GN <- rendermygraphOK(renderGraf(datosNok3GP,12),"P")
      
      output$pwrRabP3GR <- rendermygraphOK(renderGraf(datosRed3GP,13),"P")
      output$pwrRabP3GE <- rendermygraphOK(renderGraf(datosEri3GP,13),"P")
      output$pwrRabP3GH <- rendermygraphOK(renderGraf(datosHua3GP,13),"P")
      output$pwrRabP3GN <- rendermygraphOK(renderGraf(datosNok3GP,13),"P")
      
      output$codeRabP3GR <- rendermygraphOK(renderGraf(datosRed3GP,14),"P")
      output$codeRabP3GE <- rendermygraphOK(renderGraf(datosEri3GP,14),"P")
      output$codeRabP3GH <- rendermygraphOK(renderGraf(datosHua3GP,14),"P")
      output$codeRabP3GN <- rendermygraphOK(renderGraf(datosNok3GP,14),"P")
      
      output$dropCsP3GR <- rendermygraphOK(renderGraf(datosRed3GP,15),"P")
      output$dropCsP3GE <- rendermygraphOK(renderGraf(datosEri3GP,15),"P")
      output$dropCsP3GH <- rendermygraphOK(renderGraf(datosHua3GP,15),"P")
      output$dropCsP3GN <- rendermygraphOK(renderGraf(datosNok3GP,15),"P")
      
      output$dropPsP3GR <- rendermygraphOK(renderGraf(datosRed3GP,16),"P")
      output$dropPsP3GE <- rendermygraphOK(renderGraf(datosEri3GP,16),"P")
      output$dropPsP3GH <- rendermygraphOK(renderGraf(datosHua3GP,16),"P")
      output$dropPsP3GN <- rendermygraphOK(renderGraf(datosNok3GP,16),"P")
      
      output$pagP3GR <- rendermygraphOK(renderGraf(datosRed3GP,17),"P")
      output$pagP3GE <- rendermygraphOK(renderGraf(datosEri3GP,17),"P")
      output$pagP3GH <- rendermygraphOK(renderGraf(datosHua3GP,17),"P")
      output$pagP3GN <- rendermygraphOK(renderGraf(datosNok3GP,17),"P")
      
      output$pagRP3GR <- rendermygraphOK(renderGraf(datosRed3GP,18),"P")
      output$pagRP3GE <- rendermygraphOK(renderGraf(datosEri3GP,18),"P")
      output$pagRP3GH <- rendermygraphOK(renderGraf(datosHua3GP,18),"P")
      output$pagRP3GN <- rendermygraphOK(renderGraf(datosNok3GP,18),"P")
      
      output$rtwpP3GR <- rendermygraphOK(renderGraf(datosRed3GP,19),"P")
      output$rtwpP3GE <- rendermygraphOK(renderGraf(datosEri3GP,19),"P")
      output$rtwpP3GH <- rendermygraphOK(renderGraf(datosHua3GP,19),"P")
      output$rtwpP3GN <- rendermygraphOK(renderGraf(datosNok3GP,19),"P")
      
      ####CONTROLADOR####
      dataTec3GC <- createFrame3G(datos.Hua$huawei3G,datos.Eri$ericsson3G,datos.Nok$nokia3G,"cont")
      dataTec3GC_F <- FinalFrame3G(dataTec3GC,"cont")
      
      datosRed3GC <- dataTec3GC_F$datosRed%>% group_by(DIA)
      datosHua3GC <- dataTec3GC_F$datosHua%>% group_by(DIA)
      datosEri3GC <-dataTec3GC_F$datosEri%>% group_by(DIA)
      datosNok3GC <-dataTec3GC_F$datosNok%>% group_by(DIA)
      
      ##KPI
      output$acc_csC3GR <- rendermygraphOK(renderGraf(datosRed3GC,3),"P")
      output$acc_csC3GE <- rendermygraphOK(renderGraf(datosEri3GC,3),"P")
      output$acc_csC3GH <- rendermygraphOK(renderGraf(datosHua3GC,3),"P")
      output$acc_csC3GN <- rendermygraphOK(renderGraf(datosNok3GC,3),"P")
      
      output$acc_psC3GR <- rendermygraphOK(renderGraf(datosRed3GC,4),"P")
      output$acc_psC3GE <- rendermygraphOK(renderGraf(datosEri3GC,4),"P")
      output$acc_psC3GH <- rendermygraphOK(renderGraf(datosHua3GC,4),"P")
      output$acc_psC3GN <- rendermygraphOK(renderGraf(datosNok3GC,4),"P")
      
      output$ret_csC3GR <- rendermygraphOK(renderGraf(datosRed3GC,5),"P")
      output$ret_csC3GE <- rendermygraphOK(renderGraf(datosEri3GC,5),"P")
      output$ret_csC3GH <- rendermygraphOK(renderGraf(datosHua3GC,5),"P")
      output$ret_csC3GN <- rendermygraphOK(renderGraf(datosNok3GC,5),"P")
      
      output$ret_psC3GR <- rendermygraphOK(renderGraf(datosRed3GC,6),"P")
      output$ret_psC3GE <- rendermygraphOK(renderGraf(datosEri3GC,6),"P")
      output$ret_psC3GH <- rendermygraphOK(renderGraf(datosHua3GC,6),"P")
      output$ret_psC3GN <- rendermygraphOK(renderGraf(datosNok3GC,6),"P")
      
      output$trafC3GR <- rendermygraphOK(renderGraf(datosRed3GC,7),"P")
      output$trafC3GE <- rendermygraphOK(renderGraf(datosEri3GC,7),"P")
      output$trafC3GH <- rendermygraphOK(renderGraf(datosHua3GC,7),"P")
      output$trafC3GN <- rendermygraphOK(renderGraf(datosNok3GC,7),"P")
      
      output$volC3GR <- rendermygraphOK(renderGraf(datosRed3GC,8),"P")
      output$volC3GE <- rendermygraphOK(renderGraf(datosEri3GC,8),"P")
      output$volC3GH <- rendermygraphOK(renderGraf(datosHua3GC,8),"P")
      output$volC3GN <- rendermygraphOK(renderGraf(datosNok3GC,8),"P")
      
      output$ceRrcC3GR <- rendermygraphOK(renderGraf(datosRed3GC,9),"P")
      output$ceRrcC3GE <- rendermygraphOK(renderGraf(datosEri3GC,9),"P")
      output$ceRrcC3GH <- rendermygraphOK(renderGraf(datosHua3GC,9),"P")
      output$ceRrcC3GN <- rendermygraphOK(renderGraf(datosNok3GC,9),"P")
      
      output$pwrRrcC3GR <- rendermygraphOK(renderGraf(datosRed3GC,10),"P")
      output$pwrRrcC3GE <- rendermygraphOK(renderGraf(datosEri3GC,10),"P")
      output$pwrRrcC3GH <- rendermygraphOK(renderGraf(datosHua3GC,10),"P")
      output$pwrRrcC3GN <- rendermygraphOK(renderGraf(datosNok3GC,10),"P")
      
      output$codeRrcC3GR <- rendermygraphOK(renderGraf(datosRed3GC,11),"P")
      output$codeRrcC3GE <- rendermygraphOK(renderGraf(datosEri3GC,11),"P")
      output$codeRrcC3GH <- rendermygraphOK(renderGraf(datosHua3GC,11),"P")
      output$codeRrcC3GN <- rendermygraphOK(renderGraf(datosNok3GC,11),"P")
      
      output$ceRabC3GR <- rendermygraphOK(renderGraf(datosRed3GC,12),"P")
      output$ceRabC3GE <- rendermygraphOK(renderGraf(datosEri3GC,12),"P")
      output$ceRabC3GH <- rendermygraphOK(renderGraf(datosHua3GC,12),"P")
      output$ceRabC3GN <- rendermygraphOK(renderGraf(datosNok3GC,12),"P")
      
      output$pwrRabC3GR <- rendermygraphOK(renderGraf(datosRed3GC,13),"P")
      output$pwrRabC3GE <- rendermygraphOK(renderGraf(datosEri3GC,13),"P")
      output$pwrRabC3GH <- rendermygraphOK(renderGraf(datosHua3GC,13),"P")
      output$pwrRabC3GN <- rendermygraphOK(renderGraf(datosNok3GC,13),"P")
      
      output$codeRabC3GR <- rendermygraphOK(renderGraf(datosRed3GC,14),"P")
      output$codeRabC3GE <- rendermygraphOK(renderGraf(datosEri3GC,14),"P")
      output$codeRabC3GH <- rendermygraphOK(renderGraf(datosHua3GC,14),"P")
      output$codeRabC3GN <- rendermygraphOK(renderGraf(datosNok3GC,14),"P")
      
      output$dropCsC3GR <- rendermygraphOK(renderGraf(datosRed3GC,15),"P")
      output$dropCsC3GE <- rendermygraphOK(renderGraf(datosEri3GC,15),"P")
      output$dropCsC3GH <- rendermygraphOK(renderGraf(datosHua3GC,15),"P")
      output$dropCsC3GN <- rendermygraphOK(renderGraf(datosNok3GC,15),"P")
      
      output$dropPsC3GR <- rendermygraphOK(renderGraf(datosRed3GC,16),"P")
      output$dropPsC3GE <- rendermygraphOK(renderGraf(datosEri3GC,16),"P")
      output$dropPsC3GH <- rendermygraphOK(renderGraf(datosHua3GC,16),"P")
      output$dropPsC3GN <- rendermygraphOK(renderGraf(datosNok3GC,16),"P")
      
      output$pagC3GR <- rendermygraphOK(renderGraf(datosRed3GP,17),"P")
      output$pagC3GE <- rendermygraphOK(renderGraf(datosEri3GP,17),"P")
      output$pagC3GH <- rendermygraphOK(renderGraf(datosHua3GP,17),"P")
      output$pagC3GN <- rendermygraphOK(renderGraf(datosNok3GP,17),"P")
      
      output$pagRC3GR <- rendermygraphOK(renderGraf(datosRed3GP,18),"P")
      output$pagRC3GE <- rendermygraphOK(renderGraf(datosEri3GP,18),"P")
      output$pagRC3GH <- rendermygraphOK(renderGraf(datosHua3GP,18),"P")
      output$pagRC3GN <- rendermygraphOK(renderGraf(datosNok3GP,18),"P")
      
      output$rtwpC3GR <- rendermygraphOK(renderGraf(datosRed3GP,19),"P")
      output$rtwpC3GE <- rendermygraphOK(renderGraf(datosEri3GP,19),"P")
      output$rtwpC3GH <- rendermygraphOK(renderGraf(datosHua3GP,19),"P")
      output$rtwpC3GN <- rendermygraphOK(renderGraf(datosNok3GP,19),"P")
      
      
    })
    
    withProgress(message ='Filtrando Final',value = 0.5,{
      
      ####TOTAL#####
      
      data2GT <- FinalFrame(dataTec2GD,"Tot")
      data2GP <- FinalFrame(dataTec2GP,"TotP")
      
      data3GT <- FinalFrame3G(dataTec3GD,"Tot")
      data3GP <- FinalFrame3G(dataTec3GP,"TotP")
      
      names(data2GT$datosRed)[1]<-"PROVINCIA"
      names(data3GT$datosRed)[1]<-"PROVINCIA"
      
      red2G <- rbind(data2GP$datosRed,data2GT$datosRed)
      red3G <- rbind(data3GP$datosRed,data3GT$datosRed)
      
      
      ####EXPORTAR XLSX#### 
      output$EstadoRed <- downloadHandler(
        filename = 'RED.zip',
        
        content = function(fname) {
          tmpdir <- tempdir()
          setwd(tempdir())
          print(tempdir())
          
          fs <- c("Red_Celda.xlsx","Red_2G.xlsx","Red_3G.xlsx","Red_Total.xlsx")
          
          file.remove("Red_2G.xlsx")
          file.remove("Red_Celda.xlsx")
          file.remove("Red_3G.xlsx")
          file.remove("Red_Total.xlsx")
          
          write.xlsx2(cell2G$datosEri[1:1500,]%>% arrange(ACC),"Red_Celda.xlsx",sheetName="2G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(cell2G$datosHua[1:1500,]%>% arrange(ACC),"Red_Celda.xlsx",sheetName="2G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(cell2G$datosNok[1:1500,]%>% arrange(ACC),"Red_Celda.xlsx",sheetName="2G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(cell3G$datosEri[1:1500,]%>% arrange(ACC_CS),"Red_Celda.xlsx",sheetName="3G_Ericsson",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(cell3G$datosHua[1:1500,]%>% arrange(ACC_CS),"Red_Celda.xlsx",sheetName="3G_Huawei",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(cell3G$datosNok[1:1500,]%>% arrange(ACC_CS),"Red_Celda.xlsx",sheetName="3G_Nokia",col.names=TRUE, row.names = TRUE ,append = TRUE)
          
          write.xlsx2(dataTec2GD_F$datosRed,"Red_2G.xlsx",sheetName="2G_Diario",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(dataTec2GP_F$datosRed,"Red_2G.xlsx",sheetName="2G_Provincia",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(dataTec2GC_F$datosRed,"Red_2G.xlsx",sheetName="2G_Controlador",col.names=TRUE, row.names = TRUE ,append = TRUE)
          
          write.xlsx2(dataTec3GD_F$datosRed,"Red_3G.xlsx",sheetName="3G_Diario",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(dataTec3GP_F$datosRed,"Red_3G.xlsx",sheetName="3G_Provincia",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(dataTec3GC_F$datosRed,"Red_3G.xlsx",sheetName="3G_Controlador",col.names=TRUE, row.names = TRUE ,append = TRUE)
          
          write.xlsx2(red2G,"Red_Total.xlsx",sheetName="2G_Total",col.names=TRUE, row.names = TRUE ,append = TRUE)
          write.xlsx2(red3G,"Red_Total.xlsx",sheetName="3G_Total",col.names=TRUE, row.names = TRUE ,append = TRUE)
          
          
          print (fs)
          zip(zipfile=fname, files=fs)
        },
        
        contentType = "application/zip"
        
      )
      
      
    })
    
    #removeNotification(id)
    #showNotification("Hecho.",duration =10)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

