library(shiny)
library(shinyjs)
library(shinyBS)
library(openxlsx)
library(gdata)
library(DT)
library(gtools)
library(tidyverse)
library(NormalizeMets)
library(ggplot2)
library(ggsci)
library(plotly)
library(e1071)
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))
#
ui<-renderUI(
  fluidPage(
    shinyjs::useShinyjs(),
    fluidRow(
      column(5,div(
        HTML(
          "<div style='text-align:right;margin-right:0px'>
               <a href='#' target=''><img src='pseudoQC_original.jpg' width='100px'>
               </a>
               </div>"
        )
      )),
      column(7,div(
        HTML(
          "<div style='text-align:left;margin-top:30px;margin-left:-20px'>
               <a href='#' target=''><img src='pseudoQC_logoti.jpg' height='80px'>
               </a>
               </div>"
        )
      ))
    ),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js")
      )
    ),
    div(class = "busy",
        h2(strong("Calculating......")),
        img(src="rmd_loader.gif")
    ),
    tabsetPanel(
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Import Original Data"),
            h4("1. Peaks data:"),
            radioButtons(
              "fileType_Input",
              label = h4("File format:"),
              choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
              selected = 1,
              inline = TRUE
            ),
            fileInput('file1', 'Import your data：',
                      accept=c('text/csv','text/plain','.xlsx','.xls')),
            checkboxInput('header', 'Header？', TRUE),
            checkboxInput('firstcol', 'First column？', TRUE),
            conditionalPanel(condition = "input.fileType_Input==1",
                             numericInput("xlsxindex","Sheet index:",value = 1)),
            conditionalPanel(condition = "input.fileType_Input==2",
                             numericInput("xlsxindex","Sheet index:",value = 1)),
            conditionalPanel(condition = "input.fileType_Input==3",
                             radioButtons('sep', 'Separator：',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t',
                                            BlankSpace=' '),
                                          ',')),
            tags$hr(style="border-color: grey;"),
            h4("2. Samples information data:"),
            radioButtons(
              "mchuanshaodxyibanfileType_Input_fenzu",
              label = h4("File format:"),
              choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
              selected = 1,
              inline = TRUE
            ),
            fileInput('mchuanshaodxyibanfile1_fenzu', 'Import your data：',
                      accept=c('text/csv','text/plain','.xlsx','.xls')),
            checkboxInput('mchuanshaodxyibanheader_fenzu', 'Header？', TRUE),
            checkboxInput('mchuanshaodxyibanfirstcol_fenzu', 'First column？', TRUE),
            conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==1",
                             numericInput("mchuanshaodxyibanxlsxindex_fenzu","Sheet index:",value = 1)),
            conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==2",
                             numericInput("mchuanshaodxyibanxlsxindex_fenzu","Sheet index:",value = 1)),
            conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==3",
                             radioButtons('mchuanshaodxyibansep_fenzu', 'Separator：',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t',
                                            BlankSpace=' '),
                                          ','))
          ),
          mainPanel(
            width = 9,
            hr(),
            actionButton("mcsbtn_yuanshidata","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            hidden(
              div(
                id="mcsbtn_yuanshidata_hid",
                h4("1. Peaks qualitation and quantification data："),
                dataTableOutput("peaksdata"),
                h4("2. Samples information data："),
                dataTableOutput("samplesdata")
              )
            )
          )
        )
      ),
      tabPanel(
        "Imputation",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Missing Value (NA) Imputation"),
            numericInput("NAthresh","NA Rate:",value = 0.5,min = 0,max = 1,step = 0.05),
            bsTooltip("NAthresh","Those features whose Missing Value (NA) above this parameter would be removed！",
                      options = list(container = "body"))
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              #tabPanel(
              #  "NA filtered",
              #  downloadButton("NAfilterdl","Download"),
              #  dataTableOutput("NAfilterdata")
              #),
              tabPanel(
                "Imputated Data",
                downloadButton("NAimputeddl","Download"),
                dataTableOutput("NAimputedata")
              )
            )
          )
        )
      ),
      tabPanel(
        "Log.Trans.",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Log Transformation"),
            selectInput('quantidatachange', 'Log：', choices =c("Log2","Log", "Log10", "None"))
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "Box plot before",
                actionButton("mcsbtn_logbefore","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #00008B; border-color: #00008B"),
                hidden(
                  div(
                    id="mcsbtn_logbeforeid",
                    div(id="logbeforesize_div",checkboxInput("logbeforesize","Chang figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.logbeforesize==true",
                      sliderInput("logbeforesize_height","figure height：",min = 500,max = 5000,step = 100,value = 1500)
                    ),
                    downloadButton("logbeforepicdl","Download"),
                    plotOutput("logbeforeplot")
                  )
                )
              ),
              tabPanel(
                "Box plot after",
                actionButton("mcsbtn_logafter","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #00008B; border-color: #00008B"),
                hidden(
                  div(
                    id="mcsbtn_logafterid",
                    div(id="logaftersize_div",checkboxInput("logaftersize","Chang figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.logaftersize==true",
                      sliderInput("logaftersize_height","figure height：",min = 500,max = 5000,step = 100,value = 1500)
                    ),
                    downloadButton("logafterpicdl","Download"),
                    plotOutput("logafterplot")
                  )
                )
              ),
              tabPanel(
                "Coefficient of variation(CV)",
                #downloadButton("quanticvpicdl","Download"),
                plotlyOutput("quanticvplot",height = "800px")
              ),
              tabPanel(
                "Log Result data",
                downloadButton("quantiprocessdatadl","Download"),
                dataTableOutput("quantiprocessdata")
              )
            )
          )
        )
      ),
      tabPanel(
        "Original",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("No Normalization"),
            numericInput("cvthresh","CV Threshold:",value = 0.3,min = 0,max = 1,step = 0.05),
            bsTooltip("cvthresh","Those features whose Coefficient of variation (CV) above this parameter in any group would be removed！",
                      options = list(container = "body"))
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "CV filtered",
                downloadButton("CVfilterdl","Download"),
                dataTableOutput("CVfilterdata")
              ),
              tabPanel(
                "PCA plot",
                downloadButton("originalpcaplotdl","Download"),
                plotOutput("originalpcaplot",height = "1000px")
              )
            )
          )
        )
      ),
      tabPanel(
        "Linear",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Linear Regression Derivation"),
            numericInput("Linearcvthresh","Linear CV Threshold:",value = 0.05,min = 0,max = 1,step = 0.05),
            bsTooltip("Linearcvthresh","Those features whose Coefficient of variation (CV) below this parameter in all groups would be used as train data for deducing the Quality Control (QC) data！",
                      options = list(container = "body")),
            numericInput("pseudoqcnum","pseudoQC Number:",value = 10)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "Linear Derivation data",
                downloadButton("Lineardatadl","Download"),
                dataTableOutput("Lineardata")
              ),
              tabPanel(
                "Derivation Sample data",
                downloadButton("sampleqcdatadl","Download"),
                dataTableOutput("sampleqcdata")
              ),
              tabPanel(
                "CV",
                plotlyOutput("Linearcvplot",height = "800px")
              )
              #tabPanel(
              #  "PCA Plot",
              #  downloadButton("Linearpcaplotdl","Download"),
              #  plotOutput("Linearpcaplot",height = "1000px")
              #)
            )
          )
        )
      ),
      tabPanel(
        "Lasso",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Lasso Regression (Lasso) Derivation"),
            numericInput("loesscvthresh","Lasso CV Threshold:",value = 0.05,min = 0,max = 1,step = 0.05),
            bsTooltip("loesscvthresh","Those features whose Coefficient of variation (CV) below this parameter in all groups would be used as train data for deducing the Quality Control (QC) data！",
                      options = list(container = "body")),
            numericInput("pseudoqcnumloess","pseudoQC Number:",value = 10)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "Lasso Derivation data",
                downloadButton("loessdatadl","Download"),
                dataTableOutput("loessdata")
              ),
              tabPanel(
                "Derivation Sample data",
                downloadButton("sampleqcdataloessdl","Download"),
                dataTableOutput("sampleqcdataloess")
              ),
              tabPanel(
                "CV",
                plotlyOutput("loesscvplot",height = "800px")
              )
              #tabPanel(
              #  "PCA Plot",
              #  downloadButton("loesspcaplotdl","Download"),
              #  plotOutput("loesspcaplot",height = "1000px")
              #)
            )
          )
        )
      ),
      tabPanel(
        "SVR",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Support Vector Regression (SVR) Derivation"),
            numericInput("svrcvthresh","SVR CV Threshold:",value = 0.05,min = 0,max = 1,step = 0.05),
            bsTooltip("svrcvthresh","Those features whose Coefficient of variation (CV) below this parameter in all groups would be used as train data for deducing the Quality Control (QC) data！",
                      options = list(container = "body")),
            numericInput("pseudoqcnumsvr","pseudoQC Number:",value = 10)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "SVR Derivation data",
                downloadButton("SVRdatadl","Download"),
                dataTableOutput("SVRdata")
              ),
              tabPanel(
                "Derivation Sample data",
                downloadButton("sampleqcdatasvrdl","Download"),
                dataTableOutput("sampleqcdatasvr")
              ),
              tabPanel(
                "CV",
                plotlyOutput("SVRcvplot",height = "800px")
              )
              #tabPanel(
              #  "PCA Plot",
              #  downloadButton("Linearpcaplotdl","Download"),
              #  plotOutput("Linearpcaplot",height = "1000px")
              #)
            )
          )
        )
      ),
      tabPanel(
        "RFR",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Random Forest Regression (RFR) Derivation"),
            numericInput("rfrcvthresh","RFR CV Threshold:",value = 0.05,min = 0,max = 1,step = 0.05),
            bsTooltip("rfrcvthresh","Those features whose Coefficient of variation (CV) below this parameter in all groups would be used as train data for deducing the Quality Control (QC) data！",
                      options = list(container = "body")),
            numericInput("pseudoqcnumrfr","pseudoQC Number:",value = 10)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "RFR Derivation data",
                downloadButton("RFRdatadl","Download"),
                dataTableOutput("RFRdata")
              ),
              tabPanel(
                "Derivation Sample data",
                downloadButton("sampleqcdatarfrdl","Download"),
                dataTableOutput("sampleqcdatarfr")
              ),
              tabPanel(
                "CV",
                plotlyOutput("RFRcvplot",height = "800px")
              )
              #tabPanel(
              #  "PCA Plot",
              #  downloadButton("Linearpcaplotdl","Download"),
              #  plotOutput("Linearpcaplot",height = "1000px")
              #)
            )
          )
        )
      )
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=20*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #
  #show data
  peaksdataout<-reactive({
    files <- input$file1
    if (is.null(files)){
      dataread<-read.csv("CXL-pos-dingliang.csv",stringsAsFactors = F,row.names = 1,check.names = F)
    }else{
      if (input$fileType_Input == "1"){
        dataread<-read.xlsx(files$datapath,rowNames=input$firstcol,
                            colNames = input$header,sheet = input$xlsxindex)
      }
      else if(input$fileType_Input == "2"){
        if(sum(input$firstcol)==1){
          rownametf<-1
        }else{
          rownametf<-NULL
        }
        dataread<-read.xls(files$datapath,sheet = input$xlsxindex,header=input$header,
                           row.names = rownametf, sep=input$sep,stringsAsFactors = F)
      }
      else{
        if(sum(input$firstcol)==1){
          rownametf<-1
        }else{
          rownametf<-NULL
        }
        dataread<-read.csv(files$datapath,header=input$header,
                           row.names = rownametf, sep=input$sep,stringsAsFactors = F)
      }
    }
  })
  samplesdataout<-reactive({
    files <- input$mchuanshaodxyibanfile1_fenzu
    if (is.null(files)){
      dataread<-read.csv("CXL_sampleinfo.csv",header = T,stringsAsFactors = F,check.names = F)
    }else{
      if (input$mchuanshaodxyibanfileType_Input_fenzu == "1"){
        dataread<-read.xlsx(files$datapath,rowNames=input$mchuanshaodxyibanfirstcol_fenzu,
                            colNames = input$mchuanshaodxyibanheader_fenzu,sheet = input$mchuanshaodxyibanxlsxindex_fenzu)
      }
      else if(input$mchuanshaodxyibanfileType_Input_fenzu == "2"){
        if(sum(input$mchuanshaodxyibanfirstcol_fenzu)==1){
          rownametfmchuanshaodxyiban_fenzu<-1
        }else{
          rownametfmchuanshaodxyiban_fenzu<-NULL
        }
        dataread<-read.xls(files$datapath,sheet = input$mchuanshaodxyibanxlsxindex_fenzu,header=input$mchuanshaodxyibanheader_fenzu,
                           row.names = rownametfmchuanshaodxyiban_fenzu, sep=input$mchuanshaodxyibansep_fenzu,stringsAsFactors = F)
      }
      else{
        if(sum(input$mchuanshaodxyibanfirstcol_fenzu)==1){
          rownametfmchuanshaodxyiban_fenzu<-1
        }else{
          rownametfmchuanshaodxyiban_fenzu<-NULL
        }
        dataread<-read.csv(files$datapath,header=input$mchuanshaodxyibanheader_fenzu,
                           row.names = rownametfmchuanshaodxyiban_fenzu, sep=input$mchuanshaodxyibansep_fenzu,stringsAsFactors = F)
      }
    }
    zidatadf_info2<-dataread
    zidatadf_info2$class<-toupper(zidatadf_info2$class)
    zidatadf_info2
  })

  observeEvent(
    input$mcsbtn_yuanshidata,{
      shinyjs::show(id = "mcsbtn_yuanshidata_hid", anim = FALSE)

      output$peaksdata<-renderDataTable({
        datatable(peaksdataout(), options = list(pageLength = 10))
      })

      output$samplesdata<-renderDataTable({
        samplesdf<-samplesdataout()
        datatable(samplesdf, options = list(pageLength = 10))
      })
    }
  )

  imputedataout<-reactive({
    datamaxqproNA<-peaksdataout()
    zidatadf_info<-zidatadf_info3<-samplesdataout()
    zidatadf_info$class_char<-zidatadf_info3$class
    zidatadf_info$class<-factor(zidatadf_info3$class,levels = unique(zidatadf_info3$class),
                                labels = 0:(length(unique(zidatadf_info3$class))-1))
    datamaxqproNA[datamaxqproNA == 0] <- NA
    zidatadf<-t(datamaxqproNA)
    #zidatadf<-tdatamaxqproNA[zidatadf_info3$order_original,]
    imp_before <- MissingValues(zidatadf,zidatadf_info,
                                feature.cutof=input$NAthresh, sample.cutoff=0.9, method="knn")
    if(input$quantidatachange=="Log2"){
      datamaxqpro<-log2(imp_before$featuredata+1)
    }else if(input$quantidatachange=="Log"){
      datamaxqpro<-log(imp_before$featuredata+1)
    }else if(input$quantidatachange=="Log10"){
      datamaxqpro<-log10(imp_before$featuredata+1)
    }else{
      datamaxqpro<-imp_before$featuredata
    }
    imp_before$featuredata_log<-datamaxqpro
    imp_before
  })

  logboxplot_height <- reactive({
    heightx<-input$logbeforesize_height
    heightx
  })

  output$NAimputedata<-renderDataTable({
    datanaimput<-as.data.frame(t(imputedataout()$featuredata))
    datanaimput
  })
  output$NAimputeddl<-downloadHandler(
    filename = function(){paste("NA.Imputation_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(as.data.frame(t(imputedataout()$featuredata)),file = file)
    }
  )

  boxplotdata<-reactive({
    #aa<<-imputedataout()
    datamaxqprobox1<-as.data.frame(t(imputedataout()$featuredata))
    datamaxqprobox1$mz<-rownames(datamaxqprobox1)
    datamaxqprobox<-datamaxqprobox1 %>%  gather(key = sample,value = intensities,-mz)
    datamaxqprobox_merge<-merge(datamaxqprobox,imputedataout()$sampledata,by="sample")
    datamaxqprobox_merge
  })

  observeEvent(
    input$mcsbtn_logbefore,{
      shinyjs::show(id = "mcsbtn_logbeforeid", anim = FALSE)
      output$logbeforeplot<-renderPlot({
        databox<-boxplotdata()
        ggplot(databox,aes(x=sample,y=intensities,fill=class_char))+
          geom_boxplot()+theme_bw()+coord_flip()+ggtitle("Boxplot of Original data")
      },height = logboxplot_height)

      logbeforeplotout<-reactive({
        databox<-boxplotdata()
        ggplot(databox,aes(x=sample,y=intensities,fill=class_char))+
          geom_boxplot()+theme_bw()+coord_flip()+ggtitle("Boxplot of Original data")
      })

      output$logbeforepicdl<-downloadHandler(
        filename = function(){paste("Logbefore_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =logboxplot_height()/100,height = logboxplot_height()/100)
          print(logbeforeplotout())
          dev.off()
        }
      )
    }
  )

  logboxplot_afterheight <- reactive({
    heightx<-input$logaftersize_height
    heightx
  })

  boxplotdata_log<-reactive({
    datamaxqprobox1<-as.data.frame(t(imputedataout()$featuredata_log))
    datamaxqprobox1$mz<-rownames(datamaxqprobox1)
    datamaxqprobox<-datamaxqprobox1 %>%  gather(key = sample,value = intensities,-mz)
    datamaxqprobox_merge<-merge(datamaxqprobox,imputedataout()$sampledata,by="sample")
    datamaxqprobox_merge
  })

  observeEvent(
    input$mcsbtn_logafter,{
      shinyjs::show(id = "mcsbtn_logafterid", anim = FALSE)
      output$logafterplot<-renderPlot({
        databox<-boxplotdata_log()
        ggplot(databox,aes(x=sample,y=intensities,fill=class_char))+
          geom_boxplot()+theme_bw()+coord_flip()+ggtitle("Boxplot of Log data")
      },height = logboxplot_afterheight)

      logafterplotout<-reactive({
        databox<-boxplotdata_log()
        ggplot(databox,aes(x=sample,y=intensities,fill=class_char))+
          geom_boxplot()+theme_bw()+coord_flip()+ggtitle("Boxplot of Log data")
      })

      output$logafterpicdl<-downloadHandler(
        filename = function(){paste("Logafter.boxplot_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =logboxplot_afterheight()/100,height = logboxplot_afterheight()/100)
          print(logafterplotout())
          dev.off()
        }
      )
    }
  )


  output$quanticvplot<-renderPlotly({
    quantirawdatasub<-as.data.frame(t(imputedataout()$featuredata_log))
    datafenzudf<-imputedataout()$sampledata
    cvclassnames<-dimnames(table(datafenzudf$class_char))[[1]]
    chuanshao_cv_fanwei<-NULL
    for(i in cvclassnames){
      cvdfi<-quantirawdatasub[,datafenzudf$sample[which(datafenzudf$class_char==i)]]
      CVsd<-apply(cvdfi,1,sd)
      CVmean<-apply(cvdfi,1,mean)
      CV<-round(CVsd/CVmean,5)
      cvlabel<-data.frame(CV=CV)
      cvlabel$Label<-"NC"
      cvlabel$Label[cvlabel$CV<=0.05 & cvlabel$CV>=0]<-"0 - 05%"
      cvlabel$Label[cvlabel$CV<=0.1 & cvlabel$CV>0.05]<-"05% - 10%"
      cvlabel$Label[cvlabel$CV<=0.15 & cvlabel$CV>0.1]<-"10% - 15%"
      cvlabel$Label[cvlabel$CV<=0.2 & cvlabel$CV>0.15]<-"15% - 20%"
      cvlabel$Label[cvlabel$CV<=0.3 & cvlabel$CV>0.2]<-"20% - 30%"
      cvlabel$Label[cvlabel$CV>0.3]<-"above 30%"
      chuanshao_cv_fanwei<-rbind(chuanshao_cv_fanwei,cvlabel)
    }
    chuanshao_cv_fanweinames<-rep(cvclassnames,rep(dim(quantirawdatasub)[1],length(cvclassnames)))
    chuanshao_cv_fanwei_df<-cbind(chuanshao_cv_fanweinames,chuanshao_cv_fanwei)
    colnames(chuanshao_cv_fanwei_df)<-c("Groups","CV","Label")
    chuanshao_cv_fanwei_count<-chuanshao_cv_fanwei_df %>% dplyr::count(Groups, Label)
    chuanshao_cv_fanwei_count$CV_Rate<-chuanshao_cv_fanwei_count$n/dim(quantirawdatasub)[1]
    CV_x <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = cvclassnames,
      showgrid = TRUE,
      showline = TRUE,
      autorange = TRUE,
      showticklabels = TRUE,
      ticks = "outside",
      tickangle = 0
    )
    CV_y <- list(title = "Number")
    chuanshao_CV_p<-plot_ly(chuanshao_cv_fanwei_count,x = ~Groups, y = ~n, color = ~Label, textposition = 'auto',
                            type = 'bar',text=paste(round(chuanshao_cv_fanwei_count$CV_Rate*100, 2), '%')) %>%
      layout(yaxis = CV_y, xaxis = CV_x,autosize = T)
    chuanshao_CV_p
  })

  #quanticvplotdl<-reactive({})

  #output$quanticvpicdl<-downloadHandler(
  #  filename = function(){paste("CV_figure",".pdf",sep="")},
  #  content = function(file){
  #    pdf(file,colormodel="cmyk")
  #    print(quanticvplotdl())
  #    dev.off()
  #  }
  #)

  #
  quantiprocessdataout<-reactive({
    quantirawdatasub<-as.data.frame(t(imputedataout()$featuredata_log))
    datafenzudf<-imputedataout()$sampledata
    cvclassnames<-dimnames(table(datafenzudf$class_char))[[1]]
    chuanshao_cv_df<-list()
    for(i in cvclassnames){
      cvdfi<-quantirawdatasub[,datafenzudf$sample[which(datafenzudf$class_char==i)]]
      CVsd<-apply(cvdfi,1,sd)
      CVmean<-apply(cvdfi,1,mean)
      CV<-round(CVsd/CVmean,5)
      cvlabel<-list(CV)
      chuanshao_cv_df<-c(chuanshao_cv_df,cvlabel)
    }
    chuanshao_cv_df1<-as.data.frame(chuanshao_cv_df)
    colnames(chuanshao_cv_df1)<-cvclassnames
    chuanshao_cv_df2<-cbind(quantirawdatasub,chuanshao_cv_df1)
    list(chuanshao_cv_df2=chuanshao_cv_df2,quantirawdatasub=quantirawdatasub,cvvaluedf=chuanshao_cv_df1)
  })

  output$quantiprocessdata<-renderDataTable({
    datatable(quantiprocessdataout()$chuanshao_cv_df2)
  })

  output$quantiprocessdatadl<-downloadHandler(
    filename = function(){paste("LogTrans.results_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(quantiprocessdataout()$chuanshao_cv_df2,file = file)
    }
  )

  CVfilterdataout<-reactive({
    cvdata<-quantiprocessdataout()$cvvaluedf
    chuanshao_cv_index<-apply(cvdata,1,function(x){
      if(any(as.numeric(x)>input$cvthresh)){
        return(FALSE)
      }else{
        return(TRUE)
      }
    })
    cvfilterdata1<-quantiprocessdataout()$chuanshao_cv_df2[chuanshao_cv_index,]
    cvfilterdata2<-quantiprocessdataout()$quantirawdatasub[chuanshao_cv_index,]
    cvfilterdata3<-cvdata[chuanshao_cv_index,]

    list(cvfilterdata1=cvfilterdata1,cvfilterdata2=cvfilterdata2,cvfiltercv=cvfilterdata3)
  })

  output$CVfilterdata<-renderDataTable({
    datatable(CVfilterdataout()$cvfilterdata1)
  })

  output$CVfilterdl<-downloadHandler(
    filename = function(){paste("CV.filter_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(CVfilterdataout()$cvfilterdata1,file = file)
    }
  )

  output$originalpcaplot<-renderPlot({
    #bb<<-CVfilterdataout()
    originalpcadata<-t(CVfilterdataout()$cvfilterdata2)
    pca_before<-prcomp(originalpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    zidatadf_info<-imputedataout()$sampledata
    Class<-factor(zidatadf_info$class_char)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Original data")+
      theme(legend.position="bottom")
  })

  originalpcaplotout<-reactive({
    originalpcadata<-t(CVfilterdataout()$cvfilterdata2)
    pca_before<-prcomp(originalpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    zidatadf_info<-imputedataout()$sampledata
    Class<-factor(zidatadf_info$class_char)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Original data")+
      theme(legend.position="bottom")
  })

  output$originalpcaplotdl<-downloadHandler(
    filename = function(){paste("Original.PCA_figure",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width =12,height = 12)
      print(originalpcaplotout())
      dev.off()
    }
  )

  ################Linear############
  Lineardataout<-reactive({
    library(plyr)
    nqc<-input$pseudoqcnum
    cvfiltercvdf<-CVfilterdataout()$cvfiltercv
    cvyz_buyaodf<-CVfilterdataout()$cvfilterdata2
    cvyuzhi<-input$Linearcvthresh
    cvyuzhiindex<-apply(cvfiltercvdf,1,function(x){
      xx<-as.numeric(x)
      if(all(xx<=cvyuzhi)){
        return(TRUE)
      }else{
        return(FALSE)
      }
    })
    cvxcdf_noqc_small<-cvyz_buyaodf[cvyuzhiindex,]
    cvxcdf_noqc_big<-cvyz_buyaodf[!cvyuzhiindex,]
    cv_small<-cvfiltercvdf[cvyuzhiindex,]
    noqcclass<-imputedataout()$sampledata$class_char
    gpnames1<-unique(noqcclass)
    niheqcdf<-NULL
    for(i in 1:nrow(cv_small)){
      smallcv1<-which.min(cv_small[i,])
      smalldf1<-cvxcdf_noqc_small[i,noqcclass==gpnames1[smallcv1]]
      smalldf2<-as.matrix(smalldf1[1:nqc])
      niheqcdf<-rbind(niheqcdf,smalldf2)
    }
    colnames(niheqcdf)<-paste0("psQC",1:nqc)
    rownames(niheqcdf)<-rownames(cvxcdf_noqc_small)
    niheqcdf1<-cbind(niheqcdf,cvxcdf_noqc_small)
    niheqcdf_test<-NULL
    for(i in 1:nqc){
      yy<-niheqcdf[,i]
      dindex<-cbind(yy,cvxcdf_noqc_small)
      lmi<-lm(yy~.,data = dindex)
      lmipre<-predict(lmi,cvxcdf_noqc_big)
      lmipredf<-as.matrix(data.frame(xx=lmipre))
      colnames(lmipredf)<-paste0("psQC",i)
      niheqcdf_test<-cbind(niheqcdf_test,lmipredf)
    }
    niheqcdf_test1<-cbind(niheqcdf_test,cvxcdf_noqc_big)
    niheqc_alldf<-rbind(niheqcdf1,niheqcdf_test1)
    niheqc_sampledf1<-data.frame(sample=paste0("psQC",1:nqc),batch=1,class="psQC")
    niheqc_sampledf2<-samplesdataout()[,-4]
    niheqc_sampledf<-rbind(niheqc_sampledf1,niheqc_sampledf2)
    niheqc_sampledf$order<-1:nrow(niheqc_sampledf)
    list(niheqc_alldf=niheqc_alldf,niheqc_sampledf=niheqc_sampledf)
  })

  output$Lineardata<-renderDataTable({
    datatable(Lineardataout()$niheqc_alldf)
  })

  output$Lineardatadl<-downloadHandler(
    filename = function(){paste("Linear_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(Lineardataout()$niheqc_alldf,file = file)
    }
  )

  output$sampleqcdata<-renderDataTable({
    datatable(Lineardataout()$niheqc_sampledf)
  })

  output$sampleqcdatadl<-downloadHandler(
    filename = function(){paste("Sample_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(Lineardataout()$niheqc_sampledf,file = file)
    }
  )

  #Linearcvplot_data<-reactive({
  #  datamaxqprobox1<-as.data.frame(Lineardataout())
  #  datamaxqprobox1$mz<-rownames(datamaxqprobox1)
  #  datamaxqprobox<-datamaxqprobox1 %>%  gather(key = sample,value = intensities,-mz)
  #  datamaxqprobox_merge<-merge(datamaxqprobox,imputedataout()$sampledata,by="sample")
  #  datamaxqprobox_merge
  #})

  output$Linearcvplot<-renderPlotly({
    quantirawdatasub<-as.data.frame(Lineardataout()$niheqc_alldf)
    datafenzudf<-Lineardataout()$niheqc_sampledf
    cvclassnames<-dimnames(table(datafenzudf$class))[[1]]
    chuanshao_cv_fanwei<-NULL
    for(i in cvclassnames){
      cvdfi<-quantirawdatasub[,datafenzudf$sample[which(datafenzudf$class==i)]]
      CVsd<-apply(cvdfi,1,sd)
      CVmean<-apply(cvdfi,1,mean)
      CV<-round(CVsd/CVmean,5)
      cvlabel<-data.frame(CV=CV)
      cvlabel$Label<-"NC"
      cvlabel$Label[cvlabel$CV<=0.05 & cvlabel$CV>=0]<-"0 - 05%"
      cvlabel$Label[cvlabel$CV<=0.1 & cvlabel$CV>0.05]<-"05% - 10%"
      cvlabel$Label[cvlabel$CV<=0.15 & cvlabel$CV>0.1]<-"10% - 15%"
      cvlabel$Label[cvlabel$CV<=0.2 & cvlabel$CV>0.15]<-"15% - 20%"
      cvlabel$Label[cvlabel$CV<=0.3 & cvlabel$CV>0.2]<-"20% - 30%"
      cvlabel$Label[cvlabel$CV>0.3]<-"above 30%"
      chuanshao_cv_fanwei<-rbind(chuanshao_cv_fanwei,cvlabel)
    }
    chuanshao_cv_fanweinames<-rep(cvclassnames,rep(dim(quantirawdatasub)[1],length(cvclassnames)))
    chuanshao_cv_fanwei_df<-cbind(chuanshao_cv_fanweinames,chuanshao_cv_fanwei)
    colnames(chuanshao_cv_fanwei_df)<-c("Groups","CV","Label")
    chuanshao_cv_fanwei_count<-chuanshao_cv_fanwei_df %>% dplyr::count(Groups, Label)
    chuanshao_cv_fanwei_count$CV_Rate<-chuanshao_cv_fanwei_count$n/dim(quantirawdatasub)[1]
    CV_x <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = cvclassnames,
      showgrid = TRUE,
      showline = TRUE,
      autorange = TRUE,
      showticklabels = TRUE,
      ticks = "outside",
      tickangle = 0
    )
    CV_y <- list(title = "Number")
    chuanshao_CV_p<-plot_ly(chuanshao_cv_fanwei_count,x = ~Groups, y = ~n, color = ~Label, textposition = 'auto',
                            type = 'bar',text=paste(round(chuanshao_cv_fanwei_count$CV_Rate*100, 2), '%')) %>%
      layout(yaxis = CV_y, xaxis = CV_x,autosize = T)
    chuanshao_CV_p
  })

  output$Linearpcaplot<-renderPlot({
    medpcadata<-t(Lineardataout()$niheqc_alldf)
    zidatadf_info<-Lineardataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  Linearpcaplotout<-reactive({
    medpcadata<-t(Lineardataout()$niheqc_alldf)
    zidatadf_info<-Lineardataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  output$Linearpcaplotdl<-downloadHandler(
    filename = function(){paste("Linear.PCA_figure",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width =12,height = 12)
      print(Linearpcaplotout())
      dev.off()
    }
  )

  #################Lasso#####################
  loessdataout<-reactive({
    library(plyr)
    library(lars)
    nqc<-input$pseudoqcnumloess
    cvfiltercvdf<-CVfilterdataout()$cvfiltercv
    cvyz_buyaodf<-CVfilterdataout()$cvfilterdata2
    cvyuzhi<-input$loesscvthresh
    cvyuzhiindex<-apply(cvfiltercvdf,1,function(x){
      xx<-as.numeric(x)
      if(all(xx<=cvyuzhi)){
        return(TRUE)
      }else{
        return(FALSE)
      }
    })
    cvxcdf_noqc_small<-cvyz_buyaodf[cvyuzhiindex,]
    cvxcdf_noqc_big<-cvyz_buyaodf[!cvyuzhiindex,]
    cv_small<-cvfiltercvdf[cvyuzhiindex,]
    noqcclass<-imputedataout()$sampledata$class_char
    gpnames1<-unique(noqcclass)
    niheqcdf<-NULL
    for(i in 1:nrow(cv_small)){
      smallcv1<-which.min(cv_small[i,])
      smalldf1<-cvxcdf_noqc_small[i,noqcclass==gpnames1[smallcv1]]
      smalldf2<-as.matrix(smalldf1[1:nqc])
      niheqcdf<-rbind(niheqcdf,smalldf2)
    }
    colnames(niheqcdf)<-paste0("psQC",1:nqc)
    rownames(niheqcdf)<-rownames(cvxcdf_noqc_small)
    niheqcdf1<-cbind(niheqcdf,cvxcdf_noqc_small)
    niheqcdf_test<-NULL
    for(i in 1:nqc){
      yy<-niheqcdf[,i]
      dindex<-as.matrix(cbind(yy,cvxcdf_noqc_small))
      loessi<-lars(x = dindex[,-1],y=dindex[,1],type = "lasso")
      loesspre<-predict(loessi,cvxcdf_noqc_big)
      loessiminindex<-which.min(as.numeric(loessi$Cp))
      loesspredf<-as.matrix(data.frame(xx=loesspre$fit[,loessiminindex]))
      colnames(loesspredf)<-paste0("psQC",i)
      niheqcdf_test<-cbind(niheqcdf_test,loesspredf)
    }
    niheqcdf_test1<-cbind(niheqcdf_test,cvxcdf_noqc_big)
    niheqc_alldf<-rbind(niheqcdf1,niheqcdf_test1)
    niheqc_sampledf1<-data.frame(sample=paste0("psQC",1:nqc),batch=1,class="psQC")
    niheqc_sampledf2<-samplesdataout()[,-4]
    niheqc_sampledf<-rbind(niheqc_sampledf1,niheqc_sampledf2)
    niheqc_sampledf$order<-1:nrow(niheqc_sampledf)
    list(niheqc_alldf=niheqc_alldf,niheqc_sampledf=niheqc_sampledf)
  })

  output$loessdata<-renderDataTable({
    datatable(loessdataout()$niheqc_alldf)
  })

  output$loessdatadl<-downloadHandler(
    filename = function(){paste("Lasso_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(loessdataout()$niheqc_alldf,file = file)
    }
  )

  output$sampleqcdataloess<-renderDataTable({
    datatable(loessdataout()$niheqc_sampledf)
  })

  output$sampleqcdataloessdl<-downloadHandler(
    filename = function(){paste("Sample_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(loessdataout()$niheqc_sampledf,file = file)
    }
  )

  output$loesscvplot<-renderPlotly({
    quantirawdatasub<-as.data.frame(loessdataout()$niheqc_alldf)
    datafenzudf<-loessdataout()$niheqc_sampledf
    cvclassnames<-dimnames(table(datafenzudf$class))[[1]]
    chuanshao_cv_fanwei<-NULL
    for(i in cvclassnames){
      cvdfi<-quantirawdatasub[,datafenzudf$sample[which(datafenzudf$class==i)]]
      CVsd<-apply(cvdfi,1,sd)
      CVmean<-apply(cvdfi,1,mean)
      CV<-round(CVsd/CVmean,5)
      cvlabel<-data.frame(CV=CV)
      cvlabel$Label<-"NC"
      cvlabel$Label[cvlabel$CV<=0.05 & cvlabel$CV>=0]<-"0 - 05%"
      cvlabel$Label[cvlabel$CV<=0.1 & cvlabel$CV>0.05]<-"05% - 10%"
      cvlabel$Label[cvlabel$CV<=0.15 & cvlabel$CV>0.1]<-"10% - 15%"
      cvlabel$Label[cvlabel$CV<=0.2 & cvlabel$CV>0.15]<-"15% - 20%"
      cvlabel$Label[cvlabel$CV<=0.3 & cvlabel$CV>0.2]<-"20% - 30%"
      cvlabel$Label[cvlabel$CV>0.3]<-"above 30%"
      chuanshao_cv_fanwei<-rbind(chuanshao_cv_fanwei,cvlabel)
    }
    chuanshao_cv_fanweinames<-rep(cvclassnames,rep(dim(quantirawdatasub)[1],length(cvclassnames)))
    chuanshao_cv_fanwei_df<-cbind(chuanshao_cv_fanweinames,chuanshao_cv_fanwei)
    colnames(chuanshao_cv_fanwei_df)<-c("Groups","CV","Label")
    chuanshao_cv_fanwei_count<-chuanshao_cv_fanwei_df %>% dplyr::count(Groups, Label)
    chuanshao_cv_fanwei_count$CV_Rate<-chuanshao_cv_fanwei_count$n/dim(quantirawdatasub)[1]
    CV_x <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = cvclassnames,
      showgrid = TRUE,
      showline = TRUE,
      autorange = TRUE,
      showticklabels = TRUE,
      ticks = "outside",
      tickangle = 0
    )
    CV_y <- list(title = "Number")
    chuanshao_CV_p<-plot_ly(chuanshao_cv_fanwei_count,x = ~Groups, y = ~n, color = ~Label, textposition = 'auto',
                            type = 'bar',text=paste(round(chuanshao_cv_fanwei_count$CV_Rate*100, 2), '%')) %>%
      layout(yaxis = CV_y, xaxis = CV_x,autosize = T)
    chuanshao_CV_p
  })

  output$loesspcaplot<-renderPlot({
    medpcadata<-t(loessdataout()$niheqc_alldf)
    zidatadf_info<-loessdataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  loesspcaplotout<-reactive({
    medpcadata<-t(loessdataout()$niheqc_alldf)
    zidatadf_info<-loessdataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  output$loesspcaplotdl<-downloadHandler(
    filename = function(){paste("Lasso.PCA_figure",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width =12,height = 12)
      print(loesspcaplotout())
      dev.off()
    }
  )

  #################SVR#####################
  SVRdataout<-reactive({
    library(plyr)
    nqc<-input$pseudoqcnumsvr
    cvfiltercvdf<-CVfilterdataout()$cvfiltercv
    cvyz_buyaodf<-CVfilterdataout()$cvfilterdata2
    cvyuzhi<-input$svrcvthresh
    cvyuzhiindex<-apply(cvfiltercvdf,1,function(x){
      xx<-as.numeric(x)
      if(all(xx<=cvyuzhi)){
        return(TRUE)
      }else{
        return(FALSE)
      }
    })
    cvxcdf_noqc_small<-cvyz_buyaodf[cvyuzhiindex,]
    cvxcdf_noqc_big<-cvyz_buyaodf[!cvyuzhiindex,]
    cv_small<-cvfiltercvdf[cvyuzhiindex,]
    noqcclass<-imputedataout()$sampledata$class_char
    gpnames1<-unique(noqcclass)
    niheqcdf<-NULL
    for(i in 1:nrow(cv_small)){
      smallcv1<-which.min(cv_small[i,])
      smalldf1<-cvxcdf_noqc_small[i,noqcclass==gpnames1[smallcv1]]
      smalldf2<-as.matrix(smalldf1[1:nqc])
      niheqcdf<-rbind(niheqcdf,smalldf2)
    }
    colnames(niheqcdf)<-paste0("psQC",1:nqc)
    rownames(niheqcdf)<-rownames(cvxcdf_noqc_small)
    niheqcdf1<-cbind(niheqcdf,cvxcdf_noqc_small)
    niheqcdf_test<-NULL
    for(i in 1:nqc){
      yy<-niheqcdf[,i]
      svmi<-svm(x=cvxcdf_noqc_small,y=yy)
      svmpre<-predict(svmi,cvxcdf_noqc_big)
      svmpredf<-as.matrix(data.frame(xx=svmpre))
      colnames(svmpredf)<-paste0("psQC",i)
      niheqcdf_test<-cbind(niheqcdf_test,svmpredf)
    }
    niheqcdf_test1<-cbind(niheqcdf_test,cvxcdf_noqc_big)
    niheqc_alldf<-rbind(niheqcdf1,niheqcdf_test1)
    niheqc_sampledf1<-data.frame(sample=paste0("psQC",1:nqc),batch=1,class="psQC")
    niheqc_sampledf2<-samplesdataout()[,-4]
    niheqc_sampledf<-rbind(niheqc_sampledf1,niheqc_sampledf2)
    niheqc_sampledf$order<-1:nrow(niheqc_sampledf)
    list(niheqc_alldf=niheqc_alldf,niheqc_sampledf=niheqc_sampledf)
  })

  output$SVRdata<-renderDataTable({
    datatable(SVRdataout()$niheqc_alldf)
  })

  output$SVRdatadl<-downloadHandler(
    filename = function(){paste("SVR_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(SVRdataout()$niheqc_alldf,file = file)
    }
  )

  output$sampleqcdatasvr<-renderDataTable({
    datatable(SVRdataout()$niheqc_sampledf)
  })

  output$sampleqcdatasvrdl<-downloadHandler(
    filename = function(){paste("Sample_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(SVRdataout()$niheqc_sampledf,file = file)
    }
  )

  output$SVRcvplot<-renderPlotly({
    quantirawdatasub<-as.data.frame(SVRdataout()$niheqc_alldf)
    datafenzudf<-SVRdataout()$niheqc_sampledf
    cvclassnames<-dimnames(table(datafenzudf$class))[[1]]
    chuanshao_cv_fanwei<-NULL
    for(i in cvclassnames){
      cvdfi<-quantirawdatasub[,datafenzudf$sample[which(datafenzudf$class==i)]]
      CVsd<-apply(cvdfi,1,sd)
      CVmean<-apply(cvdfi,1,mean)
      CV<-round(CVsd/CVmean,5)
      cvlabel<-data.frame(CV=CV)
      cvlabel$Label<-"NC"
      cvlabel$Label[cvlabel$CV<=0.05 & cvlabel$CV>=0]<-"0 - 05%"
      cvlabel$Label[cvlabel$CV<=0.1 & cvlabel$CV>0.05]<-"05% - 10%"
      cvlabel$Label[cvlabel$CV<=0.15 & cvlabel$CV>0.1]<-"10% - 15%"
      cvlabel$Label[cvlabel$CV<=0.2 & cvlabel$CV>0.15]<-"15% - 20%"
      cvlabel$Label[cvlabel$CV<=0.3 & cvlabel$CV>0.2]<-"20% - 30%"
      cvlabel$Label[cvlabel$CV>0.3]<-"above 30%"
      chuanshao_cv_fanwei<-rbind(chuanshao_cv_fanwei,cvlabel)
    }
    chuanshao_cv_fanweinames<-rep(cvclassnames,rep(dim(quantirawdatasub)[1],length(cvclassnames)))
    chuanshao_cv_fanwei_df<-cbind(chuanshao_cv_fanweinames,chuanshao_cv_fanwei)
    colnames(chuanshao_cv_fanwei_df)<-c("Groups","CV","Label")
    chuanshao_cv_fanwei_count<-chuanshao_cv_fanwei_df %>% dplyr::count(Groups, Label)
    chuanshao_cv_fanwei_count$CV_Rate<-chuanshao_cv_fanwei_count$n/dim(quantirawdatasub)[1]
    CV_x <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = cvclassnames,
      showgrid = TRUE,
      showline = TRUE,
      autorange = TRUE,
      showticklabels = TRUE,
      ticks = "outside",
      tickangle = 0
    )
    CV_y <- list(title = "Number")
    chuanshao_CV_p<-plot_ly(chuanshao_cv_fanwei_count,x = ~Groups, y = ~n, color = ~Label, textposition = 'auto',
                            type = 'bar',text=paste(round(chuanshao_cv_fanwei_count$CV_Rate*100, 2), '%')) %>%
      layout(yaxis = CV_y, xaxis = CV_x,autosize = T)
    chuanshao_CV_p
  })

  output$SVRpcaplot<-renderPlot({
    medpcadata<-t(SVRdataout()$niheqc_alldf)
    zidatadf_info<-SVRdataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  SVRpcaplotout<-reactive({
    medpcadata<-t(SVRdataout()$niheqc_alldf)
    zidatadf_info<-SVRdataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  output$SVRpcaplotdl<-downloadHandler(
    filename = function(){paste("SVR.PCA_figure",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width =12,height = 12)
      print(SVRpcaplotout())
      dev.off()
    }
  )
  #########################RF#######
  ######################################
  RFRdataout<-reactive({
    library(randomForest)
    nqc<-input$pseudoqcnumrfr
    cvfiltercvdf<-CVfilterdataout()$cvfiltercv
    cvyz_buyaodf<-CVfilterdataout()$cvfilterdata2
    cvyuzhi<-input$rfrcvthresh
    cvyuzhiindex<-apply(cvfiltercvdf,1,function(x){
      xx<-as.numeric(x)
      if(all(xx<=cvyuzhi)){
        return(TRUE)
      }else{
        return(FALSE)
      }
    })
    cvxcdf_noqc_small<-cvyz_buyaodf[cvyuzhiindex,]
    cvxcdf_noqc_big<-cvyz_buyaodf[!cvyuzhiindex,]
    cv_small<-cvfiltercvdf[cvyuzhiindex,]
    noqcclass<-imputedataout()$sampledata$class_char
    gpnames1<-unique(noqcclass)
    niheqcdf<-NULL
    for(i in 1:nrow(cv_small)){
      smallcv1<-which.min(cv_small[i,])
      smalldf1<-cvxcdf_noqc_small[i,noqcclass==gpnames1[smallcv1]]
      smalldf2<-as.matrix(smalldf1[1:nqc])
      niheqcdf<-rbind(niheqcdf,smalldf2)
    }
    colnames(niheqcdf)<-paste0("psQC",1:nqc)
    rownames(niheqcdf)<-rownames(cvxcdf_noqc_small)
    niheqcdf1<-cbind(niheqcdf,cvxcdf_noqc_small)
    niheqcdf_test<-NULL
    for(i in 1:nqc){
      yy<-niheqcdf[,i]
      rfri<-randomForest(x=cvxcdf_noqc_small,y=yy)
      rfrpre<-predict(rfri,cvxcdf_noqc_big)
      rfrpredf<-as.matrix(data.frame(xx=rfrpre))
      colnames(rfrpredf)<-paste0("psQC",i)
      niheqcdf_test<-cbind(niheqcdf_test,rfrpredf)
    }
    niheqcdf_test1<-cbind(niheqcdf_test,cvxcdf_noqc_big)
    niheqc_alldf<-rbind(niheqcdf1,niheqcdf_test1)
    niheqc_sampledf1<-data.frame(sample=paste0("psQC",1:nqc),batch=1,class="psQC")
    niheqc_sampledf2<-samplesdataout()[,-4]
    niheqc_sampledf<-rbind(niheqc_sampledf1,niheqc_sampledf2)
    niheqc_sampledf$order<-1:nrow(niheqc_sampledf)
    list(niheqc_alldf=niheqc_alldf,niheqc_sampledf=niheqc_sampledf)
  })

  output$RFRdata<-renderDataTable({
    datatable(RFRdataout()$niheqc_alldf)
  })

  output$RFRdatadl<-downloadHandler(
    filename = function(){paste("RFR_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(RFRdataout()$niheqc_alldf,file = file)
    }
  )

  output$sampleqcdatarfr<-renderDataTable({
    datatable(RFRdataout()$niheqc_sampledf)
  })

  output$sampleqcdatarfrdl<-downloadHandler(
    filename = function(){paste("Sample_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(RFRdataout()$niheqc_sampledf,file = file)
    }
  )

  output$RFRcvplot<-renderPlotly({
    quantirawdatasub<-as.data.frame(RFRdataout()$niheqc_alldf)
    datafenzudf<-RFRdataout()$niheqc_sampledf
    cvclassnames<-dimnames(table(datafenzudf$class))[[1]]
    chuanshao_cv_fanwei<-NULL
    for(i in cvclassnames){
      cvdfi<-quantirawdatasub[,datafenzudf$sample[which(datafenzudf$class==i)]]
      CVsd<-apply(cvdfi,1,sd)
      CVmean<-apply(cvdfi,1,mean)
      CV<-round(CVsd/CVmean,5)
      cvlabel<-data.frame(CV=CV)
      cvlabel$Label<-"NC"
      cvlabel$Label[cvlabel$CV<=0.05 & cvlabel$CV>=0]<-"0 - 05%"
      cvlabel$Label[cvlabel$CV<=0.1 & cvlabel$CV>0.05]<-"05% - 10%"
      cvlabel$Label[cvlabel$CV<=0.15 & cvlabel$CV>0.1]<-"10% - 15%"
      cvlabel$Label[cvlabel$CV<=0.2 & cvlabel$CV>0.15]<-"15% - 20%"
      cvlabel$Label[cvlabel$CV<=0.3 & cvlabel$CV>0.2]<-"20% - 30%"
      cvlabel$Label[cvlabel$CV>0.3]<-"above 30%"
      chuanshao_cv_fanwei<-rbind(chuanshao_cv_fanwei,cvlabel)
    }
    chuanshao_cv_fanweinames<-rep(cvclassnames,rep(dim(quantirawdatasub)[1],length(cvclassnames)))
    chuanshao_cv_fanwei_df<-cbind(chuanshao_cv_fanweinames,chuanshao_cv_fanwei)
    colnames(chuanshao_cv_fanwei_df)<-c("Groups","CV","Label")
    chuanshao_cv_fanwei_count<-chuanshao_cv_fanwei_df %>% dplyr::count(Groups, Label)
    chuanshao_cv_fanwei_count$CV_Rate<-chuanshao_cv_fanwei_count$n/dim(quantirawdatasub)[1]
    CV_x <- list(
      type = "category",
      categoryorder = "array",
      categoryarray = cvclassnames,
      showgrid = TRUE,
      showline = TRUE,
      autorange = TRUE,
      showticklabels = TRUE,
      ticks = "outside",
      tickangle = 0
    )
    CV_y <- list(title = "Number")
    chuanshao_CV_p<-plot_ly(chuanshao_cv_fanwei_count,x = ~Groups, y = ~n, color = ~Label, textposition = 'auto',
                            type = 'bar',text=paste(round(chuanshao_cv_fanwei_count$CV_Rate*100, 2), '%')) %>%
      layout(yaxis = CV_y, xaxis = CV_x,autosize = T)
    chuanshao_CV_p
  })

  output$RFRpcaplot<-renderPlot({
    medpcadata<-t(RFRdataout()$niheqc_alldf)
    zidatadf_info<-RFRdataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  RFRpcaplotout<-reactive({
    medpcadata<-t(RFRdataout()$niheqc_alldf)
    zidatadf_info<-RFRdataout()$niheqc_sampledf
    pca_before<-prcomp(medpcadata,scale=TRUE)
    pca_before_score<-as.data.frame(pca_before$x)
    pca_before_zhanbi<-round(pca_before$sdev/sum(pca_before$sdev),4)
    Class<-factor(zidatadf_info$class)
    Batch_Shape<-factor(zidatadf_info$batch)
    ggplot(pca_before_score,aes(x=PC1,y=PC2,color=Class))+
      geom_point(aes(shape=Batch_Shape),size=3)+
      geom_text(aes(label=rownames(pca_before_score)),size=3,hjust=0.5,vjust=-0.5)+
      scale_color_manual(values=colpalettes[1:length(table(Class))])+
      theme_bw()+stat_ellipse(level=0.95,lty=2,lwd=1.5)+
      geom_hline(yintercept =mean(pca_before_score$PC2),col="grey",lty=2,lwd=1)+
      geom_vline(xintercept =mean(pca_before_score$PC1),col="grey",lty=2,lwd=1)+
      xlim(c(min(pca_before_score$PC1)-50,max(pca_before_score$PC1)+50))+
      xlab(paste0("PC1 (",pca_before_zhanbi[1]*100,"%)")) + ylab(paste0("PC2 (",pca_before_zhanbi[2]*100,"%)"))+
      ggtitle("PCA Score plot of Median Normalization")+
      theme(legend.position="bottom")
  })

  output$RFRpcaplotdl<-downloadHandler(
    filename = function(){paste("RFR.PCA_figure",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width =12,height = 12)
      print(RFRpcaplotout())
      dev.off()
    }
  )


})

shinyApp(ui = ui, server = server)
