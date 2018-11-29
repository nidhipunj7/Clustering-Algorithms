# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(factoextra)
library("fpc")
library(NbClust)


d1 <- read.csv("Z:\\Data Science\\Bimtech\\Clustering\\Data\\Loan.csv")
int_index <- sapply(d1, is.numeric)
idata <- d1[,int_index]

#------------------DBSCAN---------------------------#

data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

#-----------------Hierarchichal clustering----------#

d2<-iris[,-c(5)]
d2 <- scale(d2)

res$Best.nc
?NbClust
# Ward Hierarchical Clustering

dis <- c("euclidean","manhattan","minkowski")
met <- c("single","complete","average","mcquitty","median")
# 
# d <- dist(d2, method = "euclidean")
# fit <- hclust(d, method="average")
# plot(fit) # display dendogram
# 
# # draw dendogram with red borders around the 2 clusters
# rect.hclust(fit, k=2, border="red")



ui <- fluidPage(
 
  tags$style(HTML("
         .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}")),
  
  tabsetPanel(
    tabPanel("K-Mean Clustering", class = "one",
             
             fluidRow(
                
               h2("K Mean Clustering on a Loan Approval DataSet", align = "center"),br(),br(),
               
               column(3,
                      selectInput('xcol', 'X Variable', names(idata),selected = names(idata)[3]),
                      selectInput('ycol', 'Y Variable', names(idata),
                                  selected = names(idata)[2]),
                 numericInput('clusters', 'Cluster count', 3,
                              min = 1, max = 9)
               ),
               column(9, plotOutput("plot1", width = "100%"))
              
               )
             ),
    tabPanel("DBSCAN Clustering", class = "two",
             h2("DBSCAN Clustering on Multishapes DataSet", align = "center"),br(),br(),
             fluidRow(
               column(3,
                      numericInput('Eps','Reachability Distance',0.15),
                      numericInput('MinPts','Reachability minimum no. of points',3)
                      ),
               column(9, plotOutput("plot2"))
             
             
             )
         ),
    tabPanel("Hierarchical Clustering", class = "three",
             h2("Hierarichal Clustering on Irish DataSet", align = "center"),br(),br(),
      
      fluidRow(
        column(3,
      selectInput('dism','Select Distance Method',dis,'euclidean'),
      selectInput('am','Agglomeration Method',met,'complete'),
      numericInput('nc','No. Of Clusters',2)
        ),
      
      column(9, plotOutput("plot3")
             )
      )
      
    ),
    
    tabPanel("Fuzzy C-means Clustering", class = "four",
             h2("Fuzzy C-means Clustering on Multishapes DataSet", align = "center"),br(),br(),
      fluidRow(
        column(3,
               numericInput('nc1','No. Of Clusters',3)
               ),
        column(9, plotOutput("plot4")
               )
      )
    )
    
             
    
             
    )
    )
  
  


server <- function(input, output) {
  #-------------KMean-----------#
  selectedData <- reactive({
    idata[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(idata,input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  }, height = 600, width = 1200)
  
  #----------------DBSCAN------------#
  clus <- reactive({
    dbscan(df,eps = input$Eps, MinPts = input$MinPts)
  })
  
  output$plot2 <- renderPlot({
    plot(clus(),df, main = "DBSCAN Clustering")
  },height = 600, width = 1200)
  
  #------------Hierarchichal--------#
  
  distance <- reactive({
    dist(d2, method = input$dism)
  })
  
  hclus <- reactive({
    hclust(distance(), method = input$am)
    
  })
  output$plot3 <- renderPlot({
    plot(hclus())
    rect.hclust(hclus(), k=input$nc, border="red")
  },height = 600, width = 1200)
  
  #------------Fuzzy Cmean--------#
  
  res.fcm <- reactive({
    fcm(d2, centers=input$nc1, nstart=5)
  })
  res.fcm2 <- reactive({
    ppclust2(res.fcm(), "fanny")
  })
  
  output$plot4 <- renderPlot({
    fviz_cluster(res.fcm2(), data = d2, 
                 ellipse.type = "convex",
                 palette = "jco",
                 repel = TRUE)
    
  },height = 600, width = 1200)
  
  
}

shinyApp(ui = ui, server = server)
