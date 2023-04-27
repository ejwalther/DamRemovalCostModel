#=========================================================
#        Dam Removel Cost Estimator ShinyApp
#   
#           Author: Eric J Walther
#           Last date modified: April 27, 2023
#==========================================================

#load required packages
library(Rcpp)
library(caret)
library(gbm)
library(shiny)
library(scales)
library(rsconnect)
library(tidyverse)
library(shinythemes)

#load data
filenames <- list.files("data/Final_Model_R_data_files", pattern="*.RDS", full.names=TRUE)
ldat <- lapply(filenames, readRDS)
names(ldat) <- substr(filenames, 46,60) 
listofstates <- c("Alabama","Alaska","Arizona","Arkansas","California",
                  "Colorado","Connecticut","Delaware","Florida","Georgia",
                  "Idaho","Illinois","Indiana","Iowa",
                  "Kansas","Kentucky","Louisiana","Maine","Maryland",
                  "Massachusetts","Michigan","Minnesota","Mississippi","Missouri"
                  ,"Montana","Nebraska","Nevada","New Hampshire","New Jersey",
                  "New Mexico","New York","North Carolina","North Dakota","Ohio",
                  "Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                  "South Dakota","Tennessee","Texas","Utah","Vermont",
                  "Virginia","Washington","West Virginia","Wisconsin","Wyoming")

listofregion<-c("Southeast","Northwest","Southwest","Southeast","Southwest",
                "Southwest","Northeast","Northeast","Southeast","Southeast",
                "Northwest","Midwest","Midwest","Midwest",
                "Midwest","Midwest","Southeast","Northeast","Northeast",
                "Northeast","Midwest","Midwest","Southeast","Midwest",
                "Northwest","Midwest","Southwest","Northeast","Northeast",
                "Southwest","Northeast","Southeast","Midwest","Midwest",
                "Southeast","Northwest","Northeast","Northeast","Southeast",
                "Midwest","Southeast","Southeast","Southwest","Northeast",
                "Southeast","Northwest","Northeast","Midwest","Northwest")
regionalcomp<-cbind(listofstates,listofregion)
regionalcomp <- as.data.frame(regionalcomp)

DamData <- read.csv("data/CleanedDataFinalCopy.csv")

StateAdj <- read.csv("data/USACE_StateAdjustmentFactors.csv")
PAad<-filter(StateAdj,State=="Pennsylvania")$Factor_2020
NHDdat<-read.csv("data/NHDPlusV21_LookupTable.csv")

#=====================##
#   user interface
#=====================##
ui<-navbarPage(title="Dam Removal Cost Estimator",theme = shinytheme("lumen"),
               tabPanel(title = "Model",  fluid = TRUE,
                        tags$style("#text1 {font-size:20px;}"),
                        tags$style("#text2 {font-size:20px;}"),
                        tags$style("#text3 {font-size:20px;}"),
                        tags$style("#text6 {font-size:18px;}"),
                        tags$style("#introtext {font-size:17px;}"),
                        tags$style("#intro2 {font-size:17px;}"),
                        tags$style("#text8 {color: red;font-size:18px; font-style: bold;}"),
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Input parameters"),
                            helpText("Please provide values for the following varialbes to obtain cost predictions. Detailed explanations for each variable can be found in the introduction tab to the right."),
                            htmlOutput("text1"),
                            numericInput("num",
                                         "Dam height (in meters)", min = .3, max = 235, step =.1 ,value = 11.5),
                            selectInput("varDamMaterial",
                                        label = "Dam Material",
                                        choices = c("Masonry/Concrete/Steel","Combined","Earthen/Wood/Sheet piling")),
                            sliderInput("Comp","Comp",
                                        label="Project complexity",
                                        min = 0, max = 1,step=.1, value = c(.3,.5)),
                            hr(),
                            htmlOutput("text2"),
                            numericInput("NHD",
                                         "NHD COMID*", min = 800000001, max = 921260002, step =1 ,
                                         value = NULL),
                            helpText("*NHDPlus V2.1"),
                            fluidRow(column(width=3,htmlOutput("text19")),
                                     column(width=2,tableOutput("tableFlow")),
                                     column(width=3,htmlOutput("text18")),
                                     column(width=2,tableOutput("tableDA"))
                                     ),
                            htmlOutput("text6"),
                            br(),
                            numericInput("discharge",
                                         HTML(paste0("Annual avg. discharge (in m",tags$sup("3"),"/s)")),
                                         min = 1, max = 500, step =.25 ,
                                         value=2.15),
                            numericInput("DA",
                                         HTML(paste0("Drainage area (in km",tags$sup("2"),")")), min = 1, max = 65000, step =.25 ,
                                         value =170),
                            helpText("Please enter either a NHD reach COMID value OR discharge and draiange area values. Any entered NHD COMID will override the directly entered discharge and draiange area values. If the user wishes to directly enter hydrology data, the NHD stream code cell must be blank."),
                            hr(),
                            htmlOutput("text3"),
                            selectInput("varDamRegion",
                                        label = "Select state",
                                        choices = listofstates
                            ),
                            fluidRow(column(width=2,htmlOutput("text4")),
                                     column(width=10,tableOutput("tableRegion"))),
                            imageOutput('regionmap'),
                            helpText("Note: Hawaii is outside the geographic scope of this model.")
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Introduction",
                                                 fluidRow(column(width=6,br(),htmlOutput("introtext")),
                                                          column(width = 6,imageOutput("dampic"),htmlOutput("cap1"),imageOutput("VI"),br(),br(),htmlOutput("cap2",style = "text-align:justify"))),
                                                 hr(),
                                                 htmlOutput("intro2",style = "text-align:justify"),
                                                 br(),
                                                 p("References:"),
                                                 p("1. Duda, J. J., Johnson, R. C., Jensen, B. L., Wagner, E. J., Richards, K., and Wieferich, D. J. (2023). Compilation of cost estimates for dam removal projects in the United States. Available at:",a("https://doi.org/10.5066/P9G8V371",
                                                                                                                                                                                                                                                           href="https://doi.org/10.5066/P9G8V371",
                                                                                                                                                                                                                                                           target="_blank"),"."),
                                                 p("2. Duda, J.J., Jumani, S., Wieferich, D.J., Bountry, J.A., Tullos, D., McKay, S.K., Randle, T.J., Jansen, A., Bailey, S., Jenson, B.L., Johnson, R.C., Wagner, E., Richards, K., Wenger, S., and Walther, E.J. (in review). Patterns, drivers, and a predictive model of dam removal cost in the United States. Frontiers in Ecology and the Environment")
                                        ),
                                        tabPanel("Cost prediction",
                                                 h3("Estimated Costs (in 2020 USD)"),
                                                 br(),
                                                 htmlOutput("text8"),
                                                 tableOutput("tablePreds"),
                                                 fluidRow(column(width=8,plotOutput("multicost")),
                                                          column(width=4,plotOutput("plotlegend")))
                                        )
                            ))
                        )),
               tabPanel(title = "Model description",fluid = TRUE,
                        tags$style("#text15 {font-size:17px;}"),
                        tags$style("#text16 {font-size:17px;}"),
                        tags$style("#text17 {font-size:17px;}"),
                        tags$style("#text5 {font-size:17px;}"),
                            h3("The data"),
                            htmlOutput("text16",style = "text-align:justify"),
                            br(),
                            h3("The model"),
                            fluidRow(column(width=9,
                                            htmlOutput("text5")),
                                     column(width=1," "),
                                     column(width=2,imageOutput("GBT"))),
                            h3("Model results"),
                            htmlOutput("text17",style = "text-align:justify"),
                            fluidRow(column(width=4,imageOutput("VI2")),
                                     column(width=4,imageOutput("Pred")),
                                     column(width = 4,htmlOutput("text15",style = "text-align:justify"))),
                            fluidRow(column(width=4,htmlOutput("Fig2cap")),
                                     column(width=4,htmlOutput("Fig3cap")),
                                     column(width=4,p(""))),
                            br(),
                        p("References:"),
                        p("1. Duda, J. J., Johnson, R. C., Jensen, B. L., Wagner, E. J., Richards, K., and Wieferich, D. J. (2023). Compilation of cost estimates for dam removal projects in the United States. Available at:",a("https://doi.org/10.5066/P9G8V371",
                                                                                                                                                                                                                                  href="https://doi.org/10.5066/P9G8V371",
                                                                                                                                                                                                                                  target="_blank"),"."),
                        p("2. Duda, J.J., Jumani, S., Wieferich, D.J., Bountry, J.A., Tullos, D., McKay, S.K., Randle, T.J., Jansen, A., Bailey, S., Jenson, B.L., Johnson, R.C., Wagner, E., Richards, K., Wenger, S., and Walther, E.J. (in review). Patterns, drivers, and a predictive model of dam removal cost in the United States. Frontiers in Ecology and the Environment"),
                        p("3. Friedman, J. H. (2002). Stochastic gradient boosting. Comput. Stat. Data Anal. 38, 367–378. doi:",a("10.1016/S0167-9473(01)00065-2",
                                                                                                                             href="https://doi.org/10.1016/S0167-9473(01)00065-2",
                                                                                                                             target="_blank"),".")
               ),
               tabPanel(title = "Case study: Boardman dam",fluid = TRUE,
                        tags$style("#text1.1 {font-size:20px;}"),
                        tags$style("#text2.1 {font-size:20px;}"),
                        tags$style("#text3.1 {font-size:20px;}"),
                        tags$style("#text7 {font-size:17px;}"),
                        tags$style("#text10 {font-size:17px;}"),
                        tags$style("#text11 {font-size:17px;}"),
                        tags$style("#text12 {font-size:17px;}"),
                        tags$style("#text13 {font-size:17px;}"),
                        tags$style("#text20 {font-size:17px;}"),
                        tags$style("#Boardmantext {font-size:17px;}"),
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Input parameters"),
                            helpText("Enterd in the fields below are input values for each variable used to estimate removal cost predictions for Boardman dam."),
                            htmlOutput("text1.1"),
                            numericInput("num",
                                         "Dam height (in meters)", min = .3, max = 100, step =.1 ,value = 18.0),
                            selectInput("varDamMaterial",
                                        label = "Dam Material",
                                        choices = c("Masonry/Concrete/Steel","Combined","Earthen/Wood/Sheet piling"),
                                        selected = "Combined"),
                            sliderInput("Comp","Comp",
                                        label="Project complexity",
                                        min = 0, max = 1,step=.1, value = c(.5,.5)),
                            hr(),
                            htmlOutput("text2.1"),
                            numericInput("NHD",
                                         "NHD COMID*", min = 800000001, max = 921260002, step =1 ,
                                         value = NULL),
                            helpText("*NHDPlus V2.1"),
                            fluidRow(column(width=3,htmlOutput("text19.1")),
                                     column(width=2,tableOutput("tableFlow.1")),
                                     column(width=3,htmlOutput("text18.1")),
                                     column(width=2,tableOutput("tableDA.1"))
                            ),
                            htmlOutput("text6.1"),
                            br(),
                            numericInput("discharge",
                                         HTML(paste0("Annual avg. discharge (in m",tags$sup("3"),"/s)")),
                                         min = 1, max = 500, step =.25 ,
                                         value= 5.624802),
                            numericInput("DA",
                                         HTML(paste0("Drainage area (in km",tags$sup("2"),")")),min = 1, max = 65000, step =.25,
                                         value =691.881 ),
                            helpText("Please enter either a NHD reach COMID value OR discharge and draiange area values. Any entered NHD COMID will override the directly entered discharge and draiange area values. If the user wishes to directly enter hydrology data, the NHD stream code cell must be blank."),
                            hr(),
                            htmlOutput("text3.1"),
                            selectInput("varDamRegion.1",
                                        label = "Select state",
                                        choices = listofstates,
                                        selected ="Michigan"
                            ),
                            fluidRow(column(width=2,htmlOutput("text4.1")),
                                     column(width=10,tableOutput("tableRegion.1"))),
                            imageOutput('regionmap2'),
                            helpText("Note: Hawaii is outside the geographic scope of this model.")
                          ),
                          mainPanel(
                        htmlOutput("Boardmantext"),
                        br(),
                        imageOutput("BDpicboth"),
                        p("Aerial view of Boardman dam during (left) and after (right) removal. Photo credits: USACE (left), Interlochen Public Radio (right)"),
                        br(),
                        fluidRow(column(5,htmlOutput("text20")),
                                 column(3,tableOutput("TrueCost"))),
                        fluidRow(column(5,htmlOutput("text7")),
                                 column(3,tableOutput("MedCost"))),
                        fluidRow(column(5,htmlOutput("text10")),
                                 column(3,tableOutput("L50"))),
                        fluidRow(column(5,htmlOutput("text11")),
                                 column(3,tableOutput("U50"))),
                        fluidRow(column(5,htmlOutput("text12")),
                                 column(3,tableOutput("L025"))),
                        fluidRow(column(5,htmlOutput("text13")),
                                 column(3,tableOutput("U975"))),
                        plotOutput("Boardplot"),
                        p("Figure 1. The actual removal cost (yellow diamond) and the predicted median cost of removal (blue dot) derived from the gradient boosted quantile regression model for Boardman Dam, Michigan, USA. The dark gray error bars represent the 50% prediction intervals and the light gray error bars represent the 95% predictions intervals.")
               ))),
               tabPanel(title = "Case study: Savage Rapids dam",fluid = TRUE,
                        tags$style("#text1.2 {font-size:20px;}"),
                        tags$style("#text2.2 {font-size:20px;}"),
                        tags$style("#text3.2 {font-size:20px;}"),
                        tags$style("#text7b {font-size:17px;}"),
                        tags$style("#text10b {font-size:17px;}"),
                        tags$style("#text11b {font-size:17px;}"),
                        tags$style("#text12b {font-size:17px;}"),
                        tags$style("#text13b {font-size:17px;}"),
                        tags$style("#text20b {font-size:17px;}"),
                        tags$style("#SavageRapidstext {font-size:17px;}"),
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Input parameters"),
                            helpText("Enterd in the fields below are input values for each variable used to estimate removal cost predictions for Savage Rapids dam."),
                            htmlOutput("text1.2"),
                            numericInput("num",
                                         "Dam height (in meters)", min = .3, max = 100, step =.1 ,value = 11.9),
                            selectInput("varDamMaterial",
                                        label = "Dam Material",
                                        choices = c("Masonry/Concrete/Steel","Combined","Earthen/Wood/Sheet piling"),
                                        selected = "Masonry/Concrete/Steel"),
                            sliderInput("Comp","Comp",
                                        label="Project complexity",
                                        min = 0, max = 1,step=.1, value = c(.3,.3)),
                            hr(),
                            htmlOutput("text2.2"),
                            numericInput("NHD",
                                         "NHD COMID*", min = 800000001, max = 921260002, step =1 ,
                                         value = NULL),
                            helpText("*NHDPlus V2.1"),
                            fluidRow(column(width=3,htmlOutput("text19.2")),
                                     column(width=2,tableOutput("tableFlow.2")),
                                     column(width=3,htmlOutput("text18.2")),
                                     column(width=2,tableOutput("tableDA.2"))
                            ),
                            htmlOutput("text6.2"),
                            br(),
                            numericInput("discharge",
                                         HTML(paste0("Annual avg. discharge (in m",tags$sup("3"),"/s)")),
                                         min = 1, max = 500, step =.25 ,
                                         value = 105.8467), 
                            numericInput("DA",
                                         HTML(paste0("Drainage area (in km",tags$sup("2"),")")), min = 1, max = 65000, step =.25 ,
                                         value = 6289.909 ),
                            helpText("Please enter either a NHD reach COMID value OR discharge and draiange area values. Any entered NHD COMID will override the directly entered discharge and draiange area values. If the user wishes to directly enter hydrology data, the NHD stream code cell must be blank."),
                            hr(),
                            htmlOutput("text3.2"),
                            selectInput("varDamRegion.2",
                                        label = "Select state",
                                        choices = listofstates,
                                        selected ="Oregon"
                            ),
                            fluidRow(column(width=2,htmlOutput("text4.2")),
                                     column(width=10,tableOutput("tableRegion.2"))),
                            imageOutput('regionmap3'),
                            helpText("Note: Hawaii is outside the geographic scope of this model.")
                          ),
                          mainPanel(
                            htmlOutput("SavageRapidstext"),
                            br(),
                            imageOutput("SRDpic1"),
                            p("Aerial view of Savage Rapids dam before (left) and after (right) removal. Photo credit: ODFW"),
                            br(),
                            fluidRow(column(5,htmlOutput("text20b")),
                                     column(3,tableOutput("TrueCost.1"))),
                            fluidRow(column(5,htmlOutput("text7b")),
                                     column(3,tableOutput("MedCost.1"))),
                            fluidRow(column(5,htmlOutput("text10b")),
                                     column(3,tableOutput("L50.1"))),
                            fluidRow(column(5,htmlOutput("text11b")),
                                     column(3,tableOutput("U50.1"))),
                            fluidRow(column(5,htmlOutput("text12b")),
                                     column(3,tableOutput("L025.1"))),
                            fluidRow(column(5,htmlOutput("text13b")),
                                     column(3,tableOutput("U975.1"))),
                            plotOutput("SavagePlot"),
                            p("Figure 1. The actual removal cost (yellow diamond) and the predicted median cost of removal (blue dot) derived from the gradient boosted quantile regression model for Savage Rapids Dam, Oregon, USA. The dark gray error bars represent the 50% prediction intervals and the light gray error bars represent the 95% predictions intervals.")
                          ))),
               tabPanel(title = "Intended use",fluid = TRUE,
                        tags$style("#intenteduse {font-size:17px;}"),
                        tags$style("#databaseresource {font-size:17px;}"),
                        tags$style("#interactivetools {font-size:17px;}"),
                        tags$style("#associatedpubs {font-size:17px;}"),
                        h3("Intended use"),
                        htmlOutput("intenteduse",style = "text-align:justify"),
                        br(),
                        h3("Related Resources:"),
                        htmlOutput("databaseresource",style = "text-align:justify"),
                        h3("Interactive tools:"),
                        htmlOutput("interactivetools",style = "text-align:justify"),
                        h3("Associated publications"),
                        htmlOutput("associatedpubs",style = "text-align:justify")
               ),
               tabPanel(title = "Contributors",fluid = TRUE,
                        h3("Contributor statement"),
                        p("Eric Walther: Conceptualization, Software, Visualization, Writing - Review & Editing "),
                        p("Suman Jumani: Conceptualization, Formal Analysis, Visualization, Writing - Original Draft"),
                        p("Jeffrey Duda: Conceptualization, Data Curation, Writing - Review & Editing"),
                        imageOutput("logos")
                        )
)


server <- function(input, output) {
  ##==================###
  #       panel 1a
  ##=====================
  output$text1<-renderText({"<B><u>Dam characteristics<u><B>"})
  output$text14<-renderText({"Median complexity value:"})
  output$CompValue<-renderTable({
    C1<-paste0(round(median(seq(input$Comp[1],input$Comp[2],by=.1)),1))
    C1
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$text2<-renderText({"<B><u>Hydrology<u><B>"})
  output$text18<-renderUI({HTML(paste0("Drainage area (km",tags$sup("2"),"):"))})
  output$tableDA<-renderTable({
    if(is.na(input$NHD)==FALSE){
      DA<-filter(NHDdat,ComID==input$NHD)$TotDASqKm
    }else{
      DA<-input$DA
    }
    DA
    },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$tableFlow<-renderTable({
    if(is.na(input$NHD)==FALSE){
      discharge<-filter(NHDdat,ComID==input$NHD)$QA_MA_cms
    }else{
      discharge<-input$discharge
    }
    discharge
    },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$text19<-renderUI({HTML(paste0("Discharge (m",tags$sup("3"),"/s):"))})
  output$text6<-renderText({"OR"})
  output$text3<-renderText({"<B><u>Location<u><B>"})
  output$text4<-renderText({"Region"})
  output$tableRegion<-renderTable({
    r1<-paste0(filter(regionalcomp,listofstates==input$varDamRegion) %>% 
                 select(listofregion))
    r1
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$VI<-renderImage({
    list(src = "www/VariableImportance.png",
         width = 600,
         height = 450)
  }, deleteFile = F)
  output$dampic<-renderImage({
    list(src = "www/Dam_removal.jpg",
         width = 550,
         height = 400)
  }, deleteFile = F)
  output$cap1<-renderText({"Removal of the Marmot Dam on the Sandy River in Oregon, USA. Photo credit: NOAA"})
  output$cap2<-renderText({"Model variable importance plot that depicts the relative importance of each variable in predicting dam removal costs."})
  output$intro2<-renderText({"Based on user inputs for six variables (on the left panel), median predicted costs of dam removal along with their 50% and 95% predictions intervals is generated (on the next tab). For a more detailed examination of cost drivers and their relationship with dam removal costs, refer to (1)"})
  ##=====================
  #     panel 1b
  ##=====================
  output$introtext<-renderUI({
    HTML(paste("Welcome to our Shiny app, developed to provide cost estimates for dam removal projects within the conterminous United States. Since cost considerations can influence dam removal planning, prioritization, and execution, this tool is built as a resource for the dam removal community of practice.",
               "",
               "The costs associated with dam removal can vary widely based on several factors. An analysis of a dam removal cost database (1) revealed the following common cost drivers (2) –",
               "",
               "•	Dam height: The monetary cost of dam removal predictably rises with the size of the structure",
               "•	Annual average discharge: Dams with high discharges are associated with wider and shallower reservoirs, and hence comprise larger areas to manage or restore",
               "•	Project complexity: In addition to the construction costs associated with dam removal, other complexity drivers can elevate costs. These include <b>sediment management</b> (including considerations such as sediment contamination, mechanical sediment removal, sediments stabilization, or pilot channel dredging), <b>mitigation requirements</b> to minimize the effects of dam removal (including reservoir dewatering mechanisms, replacing lost functions, construction or enhancement of river habitat features/levees/floodplains, protecting or constructing bridges/roads/fish hatcheries), and  <b>post-removal actions</b> (including reshaping reservoir or downstream topography, revegetation, eradication of invasive species, fish passage, monitoring, installation of stability berms, relocation or protection of utilities; creation of access roads; and installation of interpretive displays). This is inferred by a complexity score, ranging from 0 (no additional complexity) to 1 (highest level of complexity). An anchor point to estimate this score is the Elwha Dam, which had a complexity score of 0.92.",
               "•	Dam drainage area: Larger drainage areas are associated with greater sediment management concerns and dam size.",
               "•	Dam material: Dams composed of more durable material such as concrete, masonry and steel are more expensive to remove per unit area relative to earthen or wooden dams.",
               "•	Geographic region: Due to variations in permitting procedures, experiences with prior removals, and cost of removals, dam cost vary by geographic region within the US."
               , sep="<br/>"
    ))})
  output$text8<-renderText({
    warning<-"WARNING: Input values are outside the range of the model training dataset. Interpet results with caution."
    blankmes<-""
    ifelse(input$num>64|input$discharge>445.3|input$DA>64103,paste0(warning),paste0(blankmes))
    })
  output$text9<-renderText({""})
  output$regionmap<-renderImage({
    list(src = "www/USplot.png",
         width = "100%",
         height = 300)
  }, deleteFile = F)
  output$multicost<-renderPlot({
    if(is.na(input$NHD)==FALSE){
      DA<-filter(NHDdat,ComID==input$NHD)$TotDASqKm
      discharge<-filter(NHDdat,ComID==input$NHD)$QA_MA_cms
    }else{
      discharge<-input$discharge
      DA<-input$DA
    }
  region<-paste0(filter(regionalcomp,listofstates==input$varDamRegion) %>% 
                   select(listofregion))
  modvarNames<-names(ldat[[3]]$trainingData[-1])
  TESTDAT<-data.frame(matrix(ncol=length(modvarNames),nrow=1,dimnames = list(NULL,modvarNames)))
  TESTDAT$dam_height_m<-input$num
  TESTDAT$AvgAnnualQ.CS<-discharge
  TESTDAT$TotDA.SqKm<-DA
  TESTDAT$Tot_scale2<-NA
  TESTDAT$DamMaterialCat<-switch(input$varDamMaterial,
                                 "Masonry/Concrete/Steel" = 3,
                                 "Combined" = 2,
                                 "Earthen/Wood/Sheet piling" = 1)
  colpop<-which(substr(colnames(TESTDAT[,which(substr(colnames(TESTDAT),1,6)=="region")]),7,15)==region)
  for(i in 1:ncol(TESTDAT[,which(substr(colnames(TESTDAT),1,6)=="region")][1,])){
    TESTDAT[1,(which(substr(colnames(TESTDAT),1,6)=="region")-1)[1]+i]<-ifelse(colpop==i,1,0)
  }
  
  
    nrowsMat<-length(seq(input$Comp[1],input$Comp[2],by=.1))
    for(i in 1:(nrowsMat-1)){
      TESTDAT<-rbind(TESTDAT,TESTDAT[1,])
    }
    TESTDAT$Tot_scale2<-seq(input$Comp[1],input$Comp[2],by=.1)
    predcost<-matrix(NA,nrow = nrowsMat,ncol = 5)
    predcost.df<-data.frame()
    for(i in 1:nrowsMat){
      for(p in 1:5){
        predcost[i,p]<-((exp(predict(ldat[[p]],TESTDAT[i,]))-1)*filter(StateAdj,State==input$varDamRegion)$Factor_2020)/PAad
      }#p
      quants<-c("95%","50%","med","50%","95%")
      quants<-as.factor(quants)
      df<-data.frame(Cost = predcost[i,], Quantile = quants, site = rep("x",5),Comp=rep(TESTDAT$Tot_scale2[i],5))
      predcost.df<-rbind(predcost.df,df)
    }#i
    MulitCOstplot<-ggplot(predcost.df,aes(x= Cost/1000000, y= as.factor(Comp)))+
      geom_line(color="grey")+
      geom_point(aes(col=Quantile),size=rep(c(4,4,8,4,4),nrowsMat),alpha=rep(c(.3,.6,.8,.6,.3),nrowsMat),colour="darkblue")+
      geom_point(aes(col=Quantile),size=rep(c(4,4,8,4,4),nrowsMat),alpha=rep(c(0,0,.8,0,0),nrowsMat),colour="gold")+
      theme_classic()+
      labs(x = "Cost (in 2020 USD Millions)", y = "Project Complexity")+
      theme(axis.title = element_text(size=22),
            axis.text = element_text(size=20))+
      coord_flip()
    MulitCOstplot
  })
  output$plotlegend<-renderImage({
    list(src = "www/legendplotForShiny.png",
         width = 288,
         height = 360)
  }, deleteFile = F)
  output$tablePreds<-renderTable({
    if(is.na(input$NHD)==FALSE){
      DA<-filter(NHDdat,ComID==input$NHD)$TotDASqKm
      discharge<-filter(NHDdat,ComID==input$NHD)$QA_MA_cms
    }else{
      discharge<-input$discharge
      DA<-input$DA
    }
    region<-paste0(filter(regionalcomp,listofstates==input$varDamRegion) %>% 
                     select(listofregion))
    modvarNames<-names(ldat[[3]]$trainingData[-1])
    TESTDAT<-data.frame(matrix(ncol=length(modvarNames),nrow=1,dimnames = list(NULL,modvarNames)))
    TESTDAT$dam_height_m<-input$num
    TESTDAT$AvgAnnualQ.CS<-discharge
    TESTDAT$TotDA.SqKm<-DA
    TESTDAT$Tot_scale2<-NA
    TESTDAT$DamMaterialCat<-switch(input$varDamMaterial,
                                   "Masonry/Concrete/Steel" = 3,
                                   "Combined" = 2,
                                   "Earthen/Wood/Sheet piling" = 1)
    colpop<-which(substr(colnames(TESTDAT[,which(substr(colnames(TESTDAT),1,6)=="region")]),7,15)==region)
    for(i in 1:ncol(TESTDAT[,which(substr(colnames(TESTDAT),1,6)=="region")][1,])){
      TESTDAT[1,(which(substr(colnames(TESTDAT),1,6)=="region")-1)[1]+i]<-ifelse(colpop==i,1,0)
    }
    nrowsMat<-length(seq(input$Comp[1],input$Comp[2],by=.1))
    for(i in 1:(nrowsMat-1)){
      TESTDAT<-rbind(TESTDAT,TESTDAT[1,])
    }
    TESTDAT$Tot_scale2<-seq(input$Comp[1],input$Comp[2],by=.1)
    predcost<-matrix(NA,nrow = nrowsMat,ncol = 5)
    predcost.df<-data.frame()
    for(i in 1:nrowsMat){
      for(p in 1:5){
        predcost[i,p]<-((exp(predict(ldat[[p]],TESTDAT[i,]))-1)*filter(StateAdj,State==input$varDamRegion)$Factor_2020)/PAad
      }#p
      quants<-c("L95%","L50%","med","H50%","H95%")
      quants<-as.factor(quants)
      df<-data.frame(Cost = predcost[i,], Quantile = quants,Comp=rep(TESTDAT$Tot_scale2[i],5))
      predcost.df<-rbind(predcost.df,df)
    }#i
    df.out<-predcost.df %>% spread(Comp,Cost) %>% 
      select(as.character(TESTDAT$Tot_scale2))
    rownames(df.out)<-c("Upper 50% prediction interval:",
                        "Upper 95% prediction interval:",
                        "Lower 50% prediction interval:",
                        "Lower 95% prediction interval:",
                        "Median cost of removal:"
    )
    df.out$seq<-c(3,5,2,4,1)
    df.outF<-df.out %>% arrange(seq) %>% select(as.character(TESTDAT$Tot_scale2))
    df.outFtab<-df.outF
    for(r in 1:nrow(df.outF)){
      dollah<-NULL
      for(c in 1:ncol(df.outF)){
        dollah[c]<-paste0(dollar(round(df.outF[r,c],0)))
      }
      df.outFtab[r,]<-dollah
    }
    df.outFtab
  },colnames = TRUE,rownames = TRUE,bordered = TRUE, striped = TRUE)
  
  ##=====================
  #         panel 2
  ##=====================
  output$text16<-renderText({paste0("A recently compiled dam removal cost database (1) was used to model dam removal costs against various predictor variables. This dataset comprised cost estimates of 668 dams removed in the United States from 1965 to 2020. Apart from cost data, the database included information of dam characteristics, hydrography, geographic location, and component costs pertaining to sediment, mitigation, and post-removal drivers. Detailed information and access to the cost database can be found at ",a("https://doi.org/10.5066/P9G8V371",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     href="https://doi.org/10.5066/P9G8V371",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       target="_blank"))})
  output$text5 <- renderUI({
    HTML(paste0("We analyzed the relationships between dam removal costs and various predictor variables using stochastic gradient boosting (3) with a quantile regression, together called gradient boosted quantile regressions (GBQR). SGBs are a machine learning technique where an ensemble of shallow and weak trees are successively built such that the performance of each tree is boosted by improving on the errors or residuals of the preceding tree (Fig 1). Unlike conventional regression trees that fit a single parsimonious model, SGBs incorporate the advantages of regression trees (i.e., handling mixed data types and missing data) whilst overcoming some of their limitations (i.e., poor predictive performance, lack of optimal tree structure, and high sensitivity to small changes in the data set). At each iteration, a tree is built from a random sub-sample of the dataset which incrementally improves model prediction accuracy while preventing over-fitting the data. In our analysis, the response variable of natural log-transformed cost estimates was modelled against six predictor variables including dam height (m), average annual discharge (m",tags$sup("3"),"/s), drainage area (km",tags$sup("2"),"), project complexity, dam material durability, and region (one hot-encoded as five variables). The steps used to build the model are summarised below – "
               ,br(), "• The data were split into training (80%, n=536) and testing (20%, n=132) datasets."
               ,br(),"• GBQRs were built on the training data with a quantile distribution."
               ,br(),"• Variables included in the model were based on their relative importance and extent of representation in the database."
               ,br(),"• The final model parameters were obtained after a grid-based hyperparameter tuning."
               ,br(),"• Median cost estimates along with 50% and 95% prediction intervals were generated based on quantile values."
               ,br(),"• Model accuracy was examined based on the relationship between the actual and predicted costs for the test data computed as the coefficient of determination or R",tags$sup("2"),", mean absolute error (MAE), and root mean squared error (RMSE)",
               br(),"For further information on model parameters and results, refer to Duda et al. (2023)"))
  })
  output$GBT<-renderImage({
    list(src = "www/SGB_schematic2.png",
         width = 196,
         height = 371)
  }, deleteFile = F)
  output$text17<-renderText({"Dam height was the strongest predictor of removal costs, followed by average annual discharge, project complexity, and drainage area (Fig 2). Additionally, the variables of Southwest region, dam material, and other regional variables had a relatively lower effect. Despite the differences in the strength of their effects, all variables were important in predicting cost as they were retained in the model. This model explained 56.98% of the variance in dam removal costs for the training dataset."})
  output$Pred<-renderImage({
    list(src = "www/ActVPred.png",
         width = 472,
         height = 376)
  }, deleteFile = F)
  output$VI2<-renderImage({
    list(src = "www/VariableImportance.png",
         width = 536,
         height = 376)
  }, deleteFile = F)
  output$text15<-renderUI({HTML(paste0(br(),"The relationship between actual and predicted cost values (n=97) derived from model performance on the test dataset is shown in Figure 3. The coefficient of determination (or R",tags$sup("2"),") was 43.8%, and the overall MAE and RMSE associated with predicted costs were $1.45M and $5.09M respectively. Errors between predicted and actual costs were larger for larger sized and more expensive dam removal projects. The relationship between actual and predicted costs were more accurate for dams under 5 meters in height, particularly when considering the 50% prediction interval. Even for the most extreme outliers, actual cost values were encompassed within the 95% prediction interval (Fig 3)."))})
  output$Fig2cap<-renderText({"Figure 2. Model variable importance plot depicting the relative importance of each variable in predicting dam removal costs scaled to the most important predictor (i.e., dam height)."})
  output$Fig3cap<-renderText({"Figure 3. Actual versus predicted median dam removal costs (in natural-log scale) for the 97 dams included in the test dataset derived from GBQR model."})
  ##============================
  #   panel 4 - boardman dam       
  ##===============================
  #Side panel
  output$text1.1<-renderText({"<B><u>Dam characteristics<u><B>"})
  output$text2.1<-renderText({"<B><u>Hydrology<u><B>"})
  output$text3.1<-renderText({"<B><u>Location<u><B>"})
  output$text4.1<-renderText({"Region"})
  output$text19.1<-renderUI({HTML(paste0("Discharge (m",tags$sup("3"),"/s):"))})
  output$text18.1<-renderUI({HTML(paste0("Drainage area (km",tags$sup("2"),"):"))})
  output$text6.1<-renderText({"OR"})
  output$tableDA.1<-renderTable({
    "691.881"
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$tableFlow.1<-renderTable({
    "5.624802"
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$tableRegion.1<-renderTable({
    r1<-paste0(filter(regionalcomp,listofstates==input$varDamRegion.1) %>% 
                 select(listofregion))
    r1
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$regionmap2<-renderImage({
    list(src = "www/USplot.png",
         width = "100%",
         height = 300)
  }, deleteFile = F)
  #main panel
  output$Boardmantext<-renderUI({
    HTML(paste("The Boardman Dam was an  irrigation and hydropower dam owned and operated by a local government in the State of Michigan.
    The input parameters for Boardamn dam are shown in the left panel. The project complexity score was 0.46 due to the presence of six additional cost drivers including the presence of contaminated sediment, mechanical sediment removal, need for erosion control, building of a coffer dam for dewatering, implementation of bank stabilization measures, and the need for post-removal revegetation.
    Based on these input variables, the predicted median cost of removal (standardised to 2020 US dollars) was <b>$9,300,608</b>. In contrast, the reported cost of removal for this dam was <b>$8,786,517</b>. Although predicted costs are slightly overestimated, we see good agreement between predicted and actual costs of removal.
"
))})
  output$BDpicboth<-renderImage({
    list(src = "www/BoardmanComb.png",
         width = "100%",
         height = 400)
  }, deleteFile = F)
  output$text7<-renderText({"Predicted median cost of removal:"})
  output$text10<-renderText({"Lower 50% prediction interval:"})
  output$text11<-renderText({"Upper 50% prediction interval:"})
  output$text12<-renderText({"Lower 95% prediction interval:"})
  output$text13<-renderText({"Upper 95% prediction interval:"})
  output$text20<-renderText({"Actual cost of removal:"})
  output$TrueCost<-renderTable({
    predcostout<-paste0(dollar(8786517))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$MedCost<-renderTable({
    predcostout<-paste0(dollar(9300608))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$L50<-renderTable({
    predcostout<-paste0(dollar(3953795))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$U50<-renderTable({
    predcostout<-paste0(dollar(23345790))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$L025<-renderTable({
    predcostout<-paste0(dollar(229747))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$U975<-renderTable({
    predcostout<-paste0(dollar(75737430))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$Boardplot<-renderPlot({
    predcost.df<-data.frame(Cost=c(229747,3953795,9300608,23345790,75737429,8786517.0),
                            Quantile=as.factor(c("L95","L50","med","H50","H95","actual")))
    BoardmanPlot<-predcost.df %>% 
      spread(Quantile,Cost)
    BoardmanPlot$dam<-"x"
    BoardmanPlot2<-predcost.df[c(3,6),]
    BoardmanPlot2$dam<-c("x","x")
    BoardmanPlot2$Quantile<-factor(BoardmanPlot2$Quantile,levels = c("med","actual"),labels = c("Predicted median cost of removal","Actual cost of removal"))
    finalBoardmanplot<-ggplot()+
      geom_errorbar(data=BoardmanPlot,aes(x=dam,ymin = L95/1000000, ymax = H95/1000000),size=1,width=.1,color="lightgrey")+
      geom_linerange(data=BoardmanPlot,aes(x=dam,ymin = L50/1000000, ymax = H50/1000000),size=2,color="darkgrey")+
      geom_errorbar(data=BoardmanPlot,aes(x=dam,ymin = L50/1000000, ymax = H50/1000000),size=1,width=.1,color="darkgrey")+
      geom_point(data = BoardmanPlot2,aes(x=dam,y=Cost/1000000,fill=Quantile,colour=Quantile,shape=Quantile),size=6,alpha=.8)+
      scale_shape_manual(values = c(21,23))+
      scale_fill_manual(values = c("blue","yellow"))+
      scale_colour_manual(values = c("black","black"))+
      labs(y= "Cost (in 2020 USD Millions)")+
      scale_y_continuous(limits = c(0,80))+
      theme_classic()+
      theme(
        axis.title = element_text(size=22),
        axis.text = element_text(size=20),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        legend.text = element_text(size=20),
        legend.position = c(.6,.8)
      )+
      coord_flip()
    finalBoardmanplot
  })
  ##===========================
  #   panel 5 - Savage Rapids
  ##==============================
  #Sidepanel
  output$text1.2<-renderText({"<B><u>Dam characteristics<u><B>"})
  output$text2.2<-renderText({"<B><u>Hydrology<u><B>"})
  output$text3.2<-renderText({"<B><u>Location<u><B>"})
  output$text4.2<-renderText({"Region"})
  output$text19.2<-renderUI({HTML(paste0("Discharge (m",tags$sup("3"),"/s):"))})
  output$text18.2<-renderUI({HTML(paste0("Drainage area (km",tags$sup("2"),"):"))})
  output$text6.2<-renderText({"OR"})
  output$tableFlow.2<-renderTable({
    "105.8467"
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$tableDA.2<-renderTable({
    "6289.909"
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$tableRegion.2<-renderTable({
    r1<-paste0(filter(regionalcomp,listofstates==input$varDamRegion.2) %>% 
                 select(listofregion))
    r1
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$regionmap3<-renderImage({
    list(src = "www/USplot.png",
         width = "100%",
         height = 300)
  }, deleteFile = F)
  #main panel
  output$SavageRapidstext<-renderUI({
    HTML(paste("The Savage Rapids Dam was a concrete 11.9 meter tall water supply dam in the State of Oregon. The input parameters for the Savage Rapids dam are shown in the left panel. The project complexity score was 0.31 due to the presence of four additional cost drivers including the need for excavating a channel through the reservoir, building of a coffer dam for dewatering, setting up a pumping plant, and other mitigation measures. Based on these input variables, the predicted median cost of removal (standardised to 2020 US dollars) was <b>$11,745,129</b>. In contrast, the reported cost of removal for this dam was <b>$50,801,777</b>.",
               br(),br(),
               "This dam presents a noteworthy case study due to the large difference between its actual and predicted costs of removal. Although the actual cost of removal was encompassed within the 95% prediction interval range, the predicted median undervalued the actual cost by about 4.5 times. An examination of the component-wise breakdown of costs can help better understand the  reasons for these discrepancies. First, the complexity score represents the presence of additional actions undertaken at the time of removal (i.e. binary presence or absence of cost drivers), but does not account for the magnitude of effect of each driver. For instance, the construction of a new pumping plant accounted for 86% (or $32,873,474) of the total cost. Additionally, some costs were also associated with litigation, which is not accounted for in the cost database. Consequently, a more accurate evaluation of the project complexity score, particularly when individual actions are expected to be associated with relatively large costs, can improve model outcomes."
    ))})
  output$SRDpic1<-renderImage({
    list(src = "www/SavageRapidsDam(ODFW).png",
         width = "100%",
         height = 400)
  }, deleteFile = F)
  output$text7b<-renderText({"Predicted median cost of removal:"})
  output$text10b<-renderText({"Lower 50% prediction interval:"})
  output$text11b<-renderText({"Upper 50% prediction interval:"})
  output$text12b<-renderText({"Lower 95% prediction interval:"})
  output$text13b<-renderText({"Upper 95% prediction interval:"})
  output$text20b<-renderText({"Actual cost of removal:"})
  output$TrueCost.1<-renderTable({
    predcostout<-paste0(dollar(50801777))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$MedCost.1<-renderTable({
    predcostout<-paste0(dollar(11745129))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$L50.1<-renderTable({
    predcostout<-paste0(dollar(5039195))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$U50.1<-renderTable({
    predcostout<-paste0(dollar(46692680))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$L025.1<-renderTable({
    predcostout<-paste0(dollar(489462))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$U975.1<-renderTable({
    predcostout<-paste0(dollar(160866744))
    predcostout
  },colnames = FALSE,rownames = FALSE,bordered = TRUE, striped = TRUE)
  output$SavagePlot<-renderPlot({
    predcost.df<-data.frame(Cost=c(489462,5039195,11745129,46692680,160866744,50801777),
                            Quantile=as.factor(c("L95","L50","med","H50","H95","actual")))
    SRPlot<-predcost.df %>% 
      spread(Quantile,Cost)
    SRPlot$dam<-"x"
    SRPlot2<-predcost.df[c(3,6),]
    SRPlot2$dam<-c("x","x")
    SRPlot2$Quantile<-factor(SRPlot2$Quantile,levels = c("med","actual"),labels = c("Predicted median cost of removal","Actual cost of removal"))
    finalSRplot<-ggplot()+
      geom_errorbar(data=SRPlot,aes(x=dam,ymin = L95/1000000, ymax = H95/1000000),size=1,width=.1,color="lightgrey")+
      geom_linerange(data=SRPlot,aes(x=dam,ymin = L50/1000000, ymax = H50/1000000),size=2,color="darkgrey")+
      geom_errorbar(data=SRPlot,aes(x=dam,ymin = L50/1000000, ymax = H50/1000000),size=1,width=.1,color="darkgrey")+
      geom_point(data = SRPlot2,aes(x=dam,y=Cost/1000000,fill=Quantile,colour=Quantile,shape=Quantile),size=6,alpha=.8)+
      scale_shape_manual(values = c(21,23))+
      scale_fill_manual(values = c("blue","yellow"))+
      scale_colour_manual(values = c("black","black"))+
      labs(y= "Cost (in 2020 USD Millions)")+
      theme_classic()+
      scale_y_continuous(limits = c(0,180),breaks = seq(0,175,by=35))+
      theme(
        axis.title = element_text(size=22),
        axis.text = element_text(size=20),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        legend.text = element_text(size=20),
        legend.position = c(.6,.8)
      )+
      coord_flip()
    finalSRplot
  })
  ##===========================
  #           panel 6
  ##==============================
  output$intenteduse<-renderUI({
    HTML(paste("The cost predictions obtained from this model provide median cost estimates along with their 50% and 95% uncertainties based on the best data available to date for the United States. However, we recognize that these estimates are subject to the constraints of this dataset. Users are advised to note the following:",
               "",
               "•	The database comprises cost data of varying quality and completeness.",
               "•	The costs in the database may not account for social, litigation, permitting, and EIA/EIS costs. However, these factors could significantly increase the time and costs associated with dam removals.",
               "•	Due to fewer data points for large dam removals, uncertainties around cost estimates for larger dams may be higher.",
               "•	It may be challenging to quantify a project complexity score before on-ground assessments. Hence, users are encouraged to select a range of complexity scores to derive more accurate cost estimates.",
               "",
               "Dam removal cost estimates derived here can be used for dam removal planning, prioritization, and execution. For instance, costs can be used as input constraints in optimization tools or as decision criteria in multi-criteria decision analysis methods while evaluating a portfolio of dams for removal.",
               sep="<br/>"
    ))})
  output$databaseresource<-renderUI({HTML(paste0("•	",a("USGS cost estimates for dam removal projects in the US",
                                                        href="https://doi.org/10.5066/P9G8V371",
                                                        target="_blank"),br(),
                                                 "•	Oregon State Univesity detailed cost database [link formcoming]",br(),
  "•	",a("American Rivers Dam Removal database",
         href="https://doi.org/10.6084/m9.figshare.5234068.v2",
         target="_blank"),br(),
"•	",a("Dam Removal Information Portal",
       href="https://data.usgs.gov/drip-dashboard/",
       target="_blank"), br(),
"•	",a("SARP aquatic Barrier Inventory",
       href="https://connectivity.sarpdata.com/summary/",
       target="_blank")
))})
 output$interactivetools<-renderUI({
   HTML(paste0(
     "•	",a("Dam removal objective & metric selector tool",
                           href="https://lnaslund.shinyapps.io/MCDA/",
                           target="_blank"),br(),
     "•	",a("SARP aquatic barrier prioritization tool",
            href="https://connectivity.sarpdata.com/priority/",
            target="_blank")
   ))})
 output$associatedpubs<-renderUI({
   HTML(paste0(
     "•	Duda, J.J., Jumani, S., Wieferich, D.J., Bountry, J.A., Tullos, D., McKay, S.K., Randle, T.J., Jansen, A., Bailey, S., Jenson, B.L., Johnson, R.C., Wagner, E., Richards, K., Wenger, S., and Walther, E.J. (in review). Patterns, drivers, and a predictive model of dam removal cost in the United States. Frontiers in Ecology and the Environment",br(),
     "•	Bountry J., Duda J., Randle T., Jansen A., Jumani S., McKay K., Bailey S.  (2023). Dam Removal Cost Databases and Drivers. SEDHYD."))
 })
 
  output$logos<-renderImage({
    list(src = "www/logos.png",
         width = 850,
         height = 150)
  }, deleteFile = F)
}

shinyApp(ui, server)
