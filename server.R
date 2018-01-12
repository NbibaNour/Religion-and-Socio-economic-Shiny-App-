library(shiny)
options(shiny.maxRequestSize = 9*1024^2)
library(readr)
library(ggplot2)
library(leaflet)
library(ggfortify)
library(googleVis)
library(plotly)
library(dplyr)
library(corrplot)
library(ape)
library(d3Network)
library(cluster)
library(networkD3)
library(NbClust)
library(ECharts2Shiny)
library(scatterplot3d)
packageVersion('plotly')

data1<-read.csv("DATA_clean.csv",header=TRUE ,sep=";")


function(input, output,session) {
  
  #importation de la base
  MyData <- reactive({
    data1<-read.csv("DATA_clean.csv",header=TRUE ,sep=";")
    vars <- names(data1)
    data1
  })
  

 
  
  
  output$COUNTRY<- renderUI({
    data1=MyData()
    COUNTRY<- data1$ICOUNTRY
    selectInput("COUNTRY", h4("Choisir une pays"),
                choices=COUNTRY,selected=COUNTRY)
  })
  
  
  output$m <- renderLeaflet({
    data1=MyData()
    
    leaflet() %>%
      setView(lat = 10, lng = 10, zoom=1) %>%
      addTiles()  %>%  
      addMarkers(lat=as.numeric(as.character(data1[which(data1$ICOUNTRY==input$COUNTRY),4])),
                 lng=as.numeric(as.character(data1[which(data1$ICOUNTRY==input$COUNTRY),5]))) 
    
  })
  
  
  
  output$m2 <- renderLeaflet({
    data1=MyData()
    leaflet() %>%
      setView(lat = 10, lng = 10, zoom=1) %>%
      addTiles() %>%
      addMarkers(lat=as.numeric(as.character(data1[which(data1$ICOUNTRY==input$COUNTRIES),4])),
                 lng=as.numeric(as.character(data1[which(data1$ICOUNTRY==input$COUNTRIES),5]))) 
    
    
  })
  output$COUNTRIES<- renderUI({
    data1=MyData()
    COUNTRY<- data1$ICOUNTRY
    selectInput("COUNTRY", h4("Choisir les pays"),
                choices=COUNTRY,selected=TRUE,multiple =TRUE)
  })
  
 
  ############ MAPS
  output$worlmap <- renderLeaflet({
    data1=MyData()
    data2 <- data1 
    if(input$religion == "Atheism") {
      data2 <- data2[data2$RELRECOD == 0 ,]
    }
    else if(input$religion == "Christian") {
      data2 <- data2[data2$RELRECOD == 1 ,]
    }
    else if(input$religion == "Catholic") {
      data2 <- data2[data2$RELRECOD == 2 ,]
    }
    else if(input$religion == "Orthodox Christian") {
      data2 <- data2[data2$RELRECOD == 3 ,]
    } 
    else if(input$religion == "Protestant") {
      data2 <- data2[data2$RELRECOD == 4 ,]
    }
    else if(input$religion == "Anglican/Episcopal") {
      data2 <- data2[data2$RELRECOD == 5 ,]
    }
    
    else if(input$religion == "Protestant") {
      data2 <- data2[data2$RELRECOD == 4 ,]
    }
    else if(input$religion == "Muslim ") {
      data2 <- data2[data2$RELRECOD == 6 ,]
    } 
    else if(input$religion == "Muslim Sunni") {
      data2 <- data2[data2$RELRECOD == 7 ,]
    }
    else if(input$religion == "Muslim Shi'a") {
      data2 <- data2[data2$RELRECOD == 8 ,]
    }
    else if(input$religion == "Jewish") {
      data2 <- data2[data2$RELRECOD == 10 ,]
    } 
    else if(input$religion == "Animist/indigenous/traditional") {
      data2 <- data2[data2$RELRECOD == 11 ,]
    }
    else if(input$religion == "Hindu") {
      data2 <- data2[data2$RELRECOD == 12 ,]
    }
    else if(input$religion == "Buddhist (specified branch)") {
      data2 <- data2[data2$RELRECOD == 14 ,]
    }
    else if(input$religion == "Buddhist (unspecified)") {
      data2 <- data2[data2$RELRECOD == 15 ,]
    }
    else if(input$religion == "Shinto") {
      data2 <- data2[data2$RELRECOD == 17,]
    }
    #democratic
    if(input$Democratic == "All"){
      data2 <- data2
    } 
    else if(input$Democratic == "Nondemocratic") {
      data2 <- data2[data2$UNPOLFRE == 0,]
    }
    else if(input$Democratic == "Democratic with no alternation") {
      data2 <- data2[data2$UNPOLFRE ==1,]
    }
    else if(input$Democratic == "Democratic") {
      data2 <- data2[data2$UNPOLFRE ==2,]
    } 
    #region
    if(input$region == "All"){
      data2 <- data2
    } 
    else if(input$region == "Africa") {
      data2 <- data2[data2$I_REGION == 1,]
    }
    else if(input$region == "Asia") {
      data2 <- data2[data2$I_REGION == 2,]
    }
    else if(input$region == "Australia/New Zealand/Oceania") {
      data2 <- data2[data2$I_REGION == 3,]  
    } 
    else if(input$region == "Europe") {
      data2 <- data2[data2$I_REGION == 4,]
    }
    else if(input$region == "Latin America/Caribbean") {
      data2 <- data2[data2$I_REGION == 5,]
    }
    else if(input$region == "USA/Canada") {
      data2 <- data2[data2$I_REGION == 6,]
    }
    
    #HDI
    attach(data2)
    UNHDI=as.numeric(UNHDI10)
    UNINEDUC=as.numeric(UNINEDUC)
    
    if(input$HDI == "All"){
      data2 <- data2
    } 
    else if(input$HDI == "Dans l'intervalle [0 , 0.25[") {
      data2 <- data2[data2$UNHDI10 > 0 & data2$UNHDI10 <= 0.25,]
    } 
    else if(input$HDI == "Dans l'intervalle [0.25 , 0.50[ ") {
      data2 <- data2[data2$UNHDI10 > 0.25 & data2$UNHDI10 <= 0.50,]
    }  
    else if(input$HDI == "Dans l'intervalle [0.50 , 0.75[") {
      data2 <- data2[data2$UNHDI10 > 0.50 & data2$UNHDI10 <= 0.75,]
    } 
    else if(input$HDI == "Dans l'intervalle [0.75 , 1[") {
      data2 <- data2[data2$UNHDI10 > 0.75 & data2$UNHDI10 <= 1 ,]
    }
    #Inequality-adjusted education 
    if(input$InEduc == "All"){
      data2 <- data2
    } 
    else if(input$InEduc == "Dans l'intervalle [0 , 0.25[") {
      data2 <- data2[data2$UNINEDUC > 0 & data2$UNINEDUC <= 0.25,]
    } 
    else if(input$InEduc == "Dans l'intervalle [0.25 , 0.50[ ") {
      data2 <- data2[data2$UNINEDUC > 0.25 & data2$UNINEDUC <= 0.50,]
    }  
    else if(input$InEduc == "Dans l'intervalle [0.50 , 0.75[") {
      data2 <- data2[data2$UNINEDUC > 0.50 & data2$UNINEDUC0 <= 0.75,]
    } 
    else if(input$InEduc == "Dans l'intervalle [0.75 , 1[") {
      data2 <- data2[data2$UUNINEDUC > 0.75 & data2$UNINEDUC <= 1,]
    }
  
    
    pal <- colorFactor(c( "red","Green" ,"navy","green"), domain = c("All","Nondemocratic ", "Democratic with no alternation"
                                                                     ,"Democratic"))
    attach(data2)
    pop=as.numeric(UNPOP10)
    leaflet() %>%
      setView(lat = 10, lng = 10, zoom=1) %>%
      addTiles() %>%
      addCircleMarkers(as.numeric(as.character(data2$Longitude)),as.numeric(as.character( data2$Latitude))
                       ,
                       popup =data2$ISO3,
                       radius =sqrt(sqrt(pop))/2,
                       color = pal(input$Democratic),
                       fillOpacity = 0.3  
                       
      )
    
  }) 
  
  #affichage de la base
  output$table<-renderDataTable({
    MyData()
  }) 
  
  #les variables quantitatives
  output$var_quanti <- renderUI({
    if(is.null(MyData()))
      return()
    data_quanti=sapply(MyData(),is.numeric)
    col_quanti <- names(data_quanti[data_quanti==TRUE])
    selectInput("var_qt", h3("Choisir une variable"),choices=col_quanti,selected=col_quanti)
  })
  

  
 
  
  #les variables quantitatives histo box 
  output$var_quanti1 <- renderUI({
    if(is.null(MyData()))
      return()
    data_quanti=sapply(MyData(),is.numeric)
    col_quanti <- names(data_quanti[data_quanti==TRUE])
    selectInput("var_qt1", h4("Choisir une variable"),choices=col_quanti,selected=col_quanti)
  })
  
  
  #les variables quantitatives axe x dep
  output$var_quantix <- renderUI({
    if(is.null(MyData()))
      return()
    data_quantix=sapply(MyData(),is.numeric)
    col_quantix <- names(data_quantix[data_quantix==TRUE])
    selectInput("var_qtx", h4("Choisir une première variable"),choices=col_quantix,selected=col_quantix)
  })
  
  #variables quanti axe y dep
  output$var_quantiy <- renderUI({
    if(is.null(MyData()))
      return()
    data_quantiy=sapply(MyData(),is.numeric)
    col_quantiy <- names(data_quantiy[data_quantiy==TRUE])
    selectInput("var_qty", h4("Choisir une deuxième variable"),choices=col_quantiy,selected=col_quantiy)
  }) 
  
  #variables quanti axe z dep
  output$var_quantiz <- renderUI({
    if(is.null(MyData()))
      return()
    data_quantiz=sapply(MyData(),is.numeric)
    col_quantiz <- names(data_quantiz[data_quantiz==TRUE])
    selectInput("var_qtz", h4("Choisir une variable pour la coloration"),choices=col_quantiz,selected=col_quantiz)
  })  
  
  #histogramme  
  output$histo <- renderPlotly({
    if(is.null(input$var_qt1))
      return(NULL)
    dataplot=MyData()
    dat=data.frame(x=dataplot[!is.na(dataplot[,input$var_qt1]),input$var_qt1])
    p=ggplot(dat)+ geom_histogram(aes(x=x,y=..density..),color="#05008A",fill="#79EDED",alpha=0.5)+stat_function(fun=dnorm,args=list(mean=mean(dat$x), sd=sd(dat$x)),linetype="dashed",size=0.8,color="#ff6600")+labs(title=paste("Histogramme de la variable",input$var_qt1))+xlab("")+ylab("")
    Sys.setenv("plotly_username"="")
    Sys.setenv("plotly_api_key"="stnXsb9qevr7VULnB73w")
    gg=ggplotly(p = ggplot2::last_plot(), filename="PLOT", fileopt="overwrite", world_readable = TRUE)
  })
  
  #boxplot 
  output$boxplot <- renderPlotly({
    if(is.null(input$var_qt1))
      return(NULL)
    dataplot=MyData()
    dat=data.frame(y=dataplot[!is.na(dataplot[,input$var_qt1]),input$var_qt1])
    p=ggplot(dat,aes(x="",y=y))+ geom_boxplot(fill="#79EDED",colour="#05008A",alpha=0.2)+labs(title=paste("Boxplot de la variable",input$var_qt1))+xlab("")+ylab("")
    Sys.setenv("plotly_username"="")
    Sys.setenv("plotly_api_key"="stnXsb9qevr7VULnB73w")
    gg=ggplotly(p = ggplot2::last_plot(), filename="PLOT", fileopt="overwrite", world_readable = TRUE)
  })
  
  output$graph <- renderGvis({
    if(is.null(input$var_ql))
      return(NULL)
    t=table(MyData()[,input$var_ql])
    dat=as.data.frame(t)
    if (identical(input$type,"Camembert")) {
      gvisPieChart(dat, options=list(page='enable',fontSize="20",height=800,width=800))
    }else if (identical(input$type,"Diagramme à bandes")){
      gvisBarChart(dat, options=list(page='enable',fontSize="20",height=800,width=800))
    }else if (identical(input$type,"Diagramme à colonnes")){
      gvisColumnChart(dat, options=list(page='enable',fontSize="20",height=800,width=800))
    }
  })
  
  #correlogramme
  output$corrl <- renderPlot({
    if(is.null(MyData()))
      return(NULL)
    col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F",
                               "cyan", "#007FFF", "blue","#00007F"))
    dataplot=MyData()
    data_quanti=sapply(dataplot,is.numeric)
    col_quanti <- names(data_quanti[data_quanti==TRUE])
    data<-dataplot[,col_quanti]
    cor.mtest <- function(mat, conf.level = 0.95) {
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
      diag(p.mat) <- 0
      diag(lowCI.mat) <- diag(uppCI.mat) <- 1
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
          lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
          uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]}}
      return(list(p.mat, lowCI.mat, uppCI.mat))}
    res1 <- cor.mtest(data, 0.95)
    c <- cor(data, method="spearman")
    corrplot(abs(c),order="AOE", col=col4(200), cl.lim=c(0,1),title = "Corrélogramme",insig = "p-value")
  })
  
  output$plot <- renderPlotly({
    if(is.null(MyData()))
      return(NULL)
    dataplot=MyData()
    data_quanti=sapply(dataplot,is.numeric)
    col_quanti <- names(data_quanti[data_quanti==TRUE])
    data<-dataplot[,col_quanti]
    key <- row.names(data)
    if (identical(input$plotType, "Avec couleur")) {
      p <- ggplot(data, aes(x = dataplot[,input$var_qtx], y = dataplot[,input$var_qty], colour = factor(dataplot[,input$var_qtz]), key = key)) + 
        geom_point()+labs(title = "Nuage de dispersion", 
                          x = input$var_qtx, y = input$var_qty,colour = input$var_qtz)
      ggplotly(p) %>% layout(dragmode = "select")
    } else {
      plot_ly(data, x = ~dataplot[,input$var_qtx], y = ~dataplot[,input$var_qty], key = ~key) %>%
        layout(dragmode = "select")
    }
  })
  
  #data choisie non hiea
  selectedData <- reactive({
    data_quantii=sapply(MyData(),is.numeric)
    col_quantii <- names(data_quantii[data_quantii==TRUE])
    MyData()[,col_quantii]
  })
  
 
    

  
  #classification hiea
  output$dendo <- renderPlot({
    if(is.null(MyData()))
      return(NULL)
    dataplot=MyData()
    data_quanti=sapply(dataplot,is.numeric)
    col_quanti <- names(data_quanti[data_quanti==TRUE])
    data<-dataplot[,col_quanti]
    d2=dist(scale(data),method ="euclidian")
    hc1 <- hclust(d2, method = "complete")
    hc1
    dt=as.phylo(hc1)
    plot(dt, edge.color =rainbow(length(dt$edge)/2), tip.color ="blue", edge.width =2, font =2,cex =0.9)
    title("Basket players", line =-29, cex.main =1.8)
    title("NBA data", line =-30, cex.main =0.8, col ="gray")
    axisPhylo( 1, las =1)
  })
  
  output$dendo1 <- renderDendroNetwork({
    if(is.null(MyData()))
      return(NULL)
    dataplot=MyData()
    data_quanti=sapply(dataplot,is.numeric)
    col_quanti <- names(data_quanti[data_quanti==TRUE])
    data<-dataplot[,col_quanti]
    hc <- hclust(dist(data), "avec")
    dendroNetwork(hc,textColour = c("red", "green", "orange")[cutree(hc, 3)], height = 600)
    
  })
  
  output$class4 <- renderPlot({
    if(is.null(MyData()))
      return(NULL)
    dataplot=MyData()
    data_quanti=sapply(dataplot,is.numeric)
    col_quanti <- names(data_quanti[data_quanti==TRUE])
    data<-dataplot[,col_quanti]
    ag =hclust(dist(scale(data)), "ward.D")
    plot(diana(data, metric ="euclidian", stand =TRUE), which =2, hang =-1)
    
  })
  
  
  #variables quanti axe x reg
  output$var_quantix2 <- renderUI({
    df <- MyData()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("var_qtx2", h4("Choisir des variables explicatives"),choices=items,selected=NULL,multiple =TRUE)
  })
  
  #les variables quantitatives axe y reg
  output$var_quantiy2 <- renderUI({
    if(is.null(MyData()))
      return(NULL)
    data_quantiy2=sapply(MyData(),is.numeric)
    col_quantiy2<- names(data_quantiy2[data_quantiy2==TRUE])
    selectInput("var_qty2", h4("Choisir la variable à expliquer"),choices=col_quantiy2,selected=NULL)
  })
  
  output$contents <- renderPrint({
    input$action
    isolate({   
      if (is.null(input$var_qtx2)) return("Vous devez choisir au moins une variable")
      fmla <- as.formula(paste(input$var_qty2," ~ ",paste(input$var_qtx2,collapse="+")))
      summary(lm(fmla,data=MyData()))
    })   
  })
  
  output$reg<- renderPlot({
    if(is.null(MyData()))
      return(NULL)
    input$action
    isolate({
      if (is.null(input$var_qtx2)) {return("Vous devez choisir au moins une variable")}
      fmla <- as.formula(paste(input$var_qty2," ~ ",paste(input$var_qtx2,collapse="+")))
      l=lm(fmla,data=MyData())
      autoplot(l, which = 1:6, colour = 'dodgerblue3',
               smooth.colour = 'black', smooth.linetype = 'dashed',
               ad.colour = 'blue',
               label.size = 3, label.n = 5, label.colour = 'blue',
               ncol = 3,title="Diagnostique du modèle de régression")
    })
  })
  output$reg1<- renderPlot({
    input$action
    isolate({
      if (is.null(input$var_qtx2)) return("Vous devez choisir  une variable")
      fmla <- as.formula(paste(input$var_qty2," ~ ",paste(input$var_qtx2,collapse="+")))
      l=lm(fmla,data=MyData())
      if(length(input$var_qtx2)>1){return("regression multiple")}
      else{
        pred<-predict(l,interval="confidence")
        qplot(MyData()[,input$var_qtx2],MyData()[,input$var_qty2])+
          geom_abline(intercept=coef(l)[1],slope=coef(l)[2],color="red")+
          geom_point(aes(x=MyData()[,input$var_qtx2],y=fitted(l)),col="red",lwd=4)+
          geom_ribbon(aes(ymin=pred[,2],ymax=pred[,3]),alpha=0.3,fill="green")+
          labs(title = "" ,x = input$var_qtx2, y = input$var_qty2)
        
      }
    })
  })
  

  
  
  
}
