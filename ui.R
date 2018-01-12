library(shiny)
library(shinythemes)
library(ggplot2)
library(ggfortify)
library(googleVis)
library(plotly)
library(leaflet)
library(dplyr)
library(d3Network)
library(networkD3)
library(ECharts2Shiny)

shinyUI( 
  
  
  navbarPage(
    
    theme = shinytheme("darkly"),
    strong("Socio-economic and Religion"),
    tabPanel("Notre application",
            
             fluidRow(
              
                 width = 12,
                 height = 500,
                 solidHeader = FALSE,
                 collapsible = FALSE,
                 collapsed = FALSE,
                 h3(" Présentation de l'application"),
                 p(
                   paste("Le nom de l'application: Socio-economic and Religion ")),
                 p(
                   paste("Realisee par: Nbiba Nour El Islem ")),
                 p(
                   paste("Encadree par: Monsieur Malouche Dhafer ")),
                 
                 p(
                   paste("Cette application permet la visualisation et  exploration de donnees de la base  qui contient 11 variables et  146 observations (pays du monde). 
                 Elle comporte :")),
                 tags$ul(tags$li(  
                        " La premiere partie visualise notre base de donnees avec des filtres pour mieux comprendre les variables selectionnees d'une part d'autre part dans cette partie on peut  visualiser et connaitre les emplacements des pays etudies sur une Map
                 "),
                         tags$li("La deuxieme partie est importante, On peut voir  la propagation des religions dans le monde c'est-a-dire la religion la plus large selon les population pour chaque pays etudie en choissiant  le niveau de la democratie (eleve , moyen ..) aussi selon l'indice de developement  humain , l'indice de inegalite d'education.
                  "),
                                 tags$li( "La troisieme partie fournit des statistiques descriptives ainsi que les nuages de dispersion
                                 "),
                                 tags$li("La quatrieme  permet de classifier les donnees selon pluisieur methodes et effectuer une regression 
                                          ")),
                       
                 
                 h4(strong("Sourse des donnees")),             
                
                 p("http://www.thearda.com/ ",align="Justify"),
                 h4(strong("Packages")),
                 
                 p("Cette application nécessite ces packages R ",align="Justify"),
                 tags$ul(tags$li("Shiny"),
                 tags$li("Shinythemes"),
                 tags$li("ggplot2"),
                 tags$li("leaflet"),
                 tags$li("plotly"),
                 tags$li("ggfortify"),
                 tags$li("googleVis"),
                 tags$li("dplyr"),
                 tags$li("d3Network"),
                 tags$li("readr"),
                 tags$li("corrplot"),
                 tags$li("ape"),
                 tags$li("cluster"),
                 tags$li("ECharts2Shiny"),
                 tags$li("NbClust"),
                 tags$li("scatterplot3d")),
                
                 h4(strong("Contact")),
                 p("n.nbiba@gmail.com")
                 
                 
               
  )
             
             
    ),#A propos
    
    
    
    #############################################
    navbarMenu("Nos Donnees ",
               
               tabPanel("Base de Donnees",
                       
                          column(12,h3("Data"),dataTableOutput("table"))
                          
                        )
               ,
              tabPanel("Description des Donnees",
                      fluidRow(
                        width = 12,
                        height = 500,
                        solidHeader = FALSE,
                        collapsible = FALSE,
                        collapsed = FALSE,
                        h3(" Description des variables"),
                        p(
                          paste("Notre base de donnees contient plusieurs variables socio-economiques et religieuses.")),
                        tags$ul( 
                          tags$li("ICOUNTRY:	Country/territory name"),
                          tags$li("Latitude:	Latitude of the country "),
                          tags$li("Longitude: Longitude of the country"),
                          tags$li("RELRECOD:	Largest religion by proportion"),
                          tags$li("UNHDI10:	Human Development Index (HDI) value"),
                          tags$li("UNINHDI:	Inequality-adjusted Human Development Index "),
                          tags$li("UNINEDUC:	 Inequality-adjusted education index"),
                          tags$li("UNPOLFRE:	Democracy Score"),
                          tags$li("UNPOP10:	Total population in millions"),
                          tags$li("I_RELIGION:	Largest religion by proportion "),
                          tags$li("I_REGION:	United Nations region ")),
                        p(
                          paste("Certaines de ces variables sont codees, il y a tous les details dans ce lien:
                          http://www.thearda.com/Archive/Files/Codebooks/ECON11_CB.asp/
                                ")),
                        p(
                          paste("La source de notre base donnees est ce site web: http://www.thearda.com/
                                  la base de donnees est legerement modifiee"))
                        )

                      )
                         ,
    
            
    
    tabPanel("Localisation des pays",
             sidebarLayout(
               sidebarPanel(
                 tags$h5("Pour mieux comprendre les donnees fournies observations, il est necessaire de connaitre les emplacements des pays (observation)"),
                 uiOutput("COUNTRY")
               ),
               mainPanel(
                 
                 fluidRow(
                   column(12,leafletOutput("m"))))))),
  #tabPanel base
    
    
    ##############################################
    
    tabPanel("Visualisation",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("religion", label = "Relegion", 
                             choices = c("All", 
                                         "Atheism",
                                         "Christian",
                                         "Catholic",
                                         "Orthodox Christian",
                                         "Protestant",
                                         "Anglican/Episcopal",
                                         "Muslim "
                                         ,"Muslim Sunni"
                                         ,"Muslim Shi'a"
                                         ,"Jewish"
                                         ,"Animist/indigenous/traditional"
                                         ,"Hindu"
                                         ,"Buddhist (specified branch)"
                                         ,"Buddhist (unspecified)"
                                         ,"Shinto"
                             ), selected = "All") ,
                 selectInput("Democratic", label = "Niveau de Democratic", 
                             choices = c("All",
                                         "Nondemocratic"
                                         ,"Democratic with no alternation"
                                         ,"Democratic")
                             , selected = "All"),
                 
                 selectInput("HDI", label = "Human Development Index (HDI) ", 
                             choices = c("All","Dans l'intervalle [0 , 0.25[",
                                         "Dans l'intervalle [0.25 , 0.50[ ",
                                         "Dans l'intervalle [0.50 , 0.75[",
                                         "Dans l'intervalle [0.75 , 1["), selected = "All"),
                 selectInput("InEduc", label = "Inequality-adjusted education index", 
                             choices = c("All","Dans l'intervalle [0 , 0.25[",
                                         "Dans l'intervalle [0.25 , 0.50[ ",
                                         "Dans l'intervalle [0.50 , 0.75[",
                                         "Dans l'intervalle [0.75 , 1["), selected = "All"),
                 selectInput("region", label = "Region", 
                             choices = c("All","Africa",
                                         "Asia",
                                         "Australia/New Zealand/Oceania",
                                         "Europe",
                                         "Latin America/Caribbean",
                                         "USA/Canada")
                             
                             , selected = "All")),
               mainPanel(
                 column(12,leafletOutput("worlmap"))))),
    
    

    #############################################
    navbarMenu("Statistiques Descriptives",
               tabPanel("Variables quantitatives",
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("var_quanti1"),width = 3
                          ),
                          mainPanel(
                            fluidRow(
                              column(12,plotlyOutput('histo')),
                              column(12,plotlyOutput('boxplot'))
                            )
                          )
                        )),
               
               tabPanel("Dispersion",sidebarLayout(
                 sidebarPanel(
                   uiOutput("var_quantix"),width = 3, 
                   uiOutput("var_quantiy"),
                   uiOutput("var_quantiz")
                 ),
                 mainPanel(
                   radioButtons("plotType", "Type du plot:", choices = c("Avec couleur")),
                   column(12,plotlyOutput("plot"))
                 )))),
    
    ############################################
    navbarMenu("Classification et Régression",
               
               
               tabPanel("classification hiérarchique divisive",
                        fluidRow(
                          plotOutput("class4")
                        )
               ),
              
               tabPanel("Régression linéaire",
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("var_quantiy2"),width = 3, 
                            uiOutput("var_quantix2"),
                            actionButton("action", "Run")
                          ),
                          mainPanel(
                            verbatimTextOutput("contents"),
                            plotOutput("reg"),
                            plotOutput("reg1"))))
    ))#classification et regression
    #############################################
  
  
  
  
  
  
  
)
