# ======================================================================
## FIT5147 Data exploration and visualisation 
## Semester 1 2018
## Visualization project
## Author: Saaransh Mehta
## Date created: 29/05/2018
## Date modified: 4/06/2018
# ======================================================================


# loading libraries
library(shiny)
library(tidyverse)
library(httpuv)
library(ggplot2) # load ggplot
library("tm") # for text mining
library("SnowballC") # for text stemming
library("RColorBrewer")
library(igraph)
library(wordcloud)
library(memoise)
library(tcltk)

df<- read.csv("Final2.csv",header = TRUE, sep = ",")

function(input, output, session) {

## Tab one - Domain analysis  
  output$selectoption <- renderUI({selectInput("selectoption", "Select option:",
                                               choices =c("Domain VS Year" = "dmvsy",
                                                          "Year VS Number of Projects" = "yvsnb",
                                                          "Domain VS Number of Projects" = "dmvsnb"
                                                          ))})
  output$dmselect <- renderUI({
  # If- else logic is used to select the appropriate menu
    if(input$selectoption =="dmvsnb" ){
      # selectInput("dynamic",label = "Domain",choices = as.character(unique(df$Domain), multipleClasses(details = TRUE)))}
      checkboxGroupInput("dynamic",label = "Domain",choices = as.character(unique(df$Domain)))}
    else if (input$selectoption =="yvsnb" ){
      sliderInput("yrs", label = "Year range",min= min(df$Year), max = max(df$Year)
                  , value = c(min(df$Year),max(df$Year)), step = 1)}
  })
  
  output$sld2 <- renderUI(
    if (input$selectoption =="dmvsy" ){
      sliderInput("sld2", label = "Year range",min= min(df$Year), max = max(df$Year)
                  , value = c(min(df$Year),max(df$Year)), step = 1)})
  
  output$ckbox2 <- renderUI(
    if (input$selectoption =="dmvsy" ){
      selectInput("ckbox2",label = "Domain",choices = as.character(unique(df$Domain)), multiple = TRUE)
      # checkboxGroupInput("ckbox2",label = "Domain",choices = as.character(unique(df$Domain)))
    })

  output$displot <- renderPlot({
    if(input$selectoption=="dmvsnb"){
      dat <- df[df$Domain %in% input$dynamic,]
      print(ggplot(data=dat, aes(x= Domain, fill = Domain))+
              geom_bar(stat = "Count")+
              theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
    }
    else if (input$selectoption=="yvsnb"){
      dat <- df[(df$Year) >= input$yrs[1] & (df$Year) <= input$yrs[2],]
      print(ggplot(data=dat,aes(x=Year,fill = Year))+
              geom_bar(stat = "Count",fill = "Blue")+
              xlim(min(dat$Year),max(dat$Year)))
    }
    else if (input$selectoption=="dmvsy"){
      dat <- df[df$Domain %in% input$ckbox2,]
      dat2 <- dat[dat$Year >= input$sld2[1] & dat$Year <=input$sld2[2],]

      print(ggplot(data=dat2,aes(x=Year,fill = Domain))+
              geom_bar(position = "stack")+
              xlim(min(dat2$Year),max(dat2$Year))
      )
    }
  })
  
## Tab two - Word Cluster
  
  # Using "memoise" to automatically cache the results
  getTermMatrix <- memoise(function() {
    df<- read.csv("Final2.csv",header = TRUE, sep = ",")
    new1 <- lapply(strsplit(as.character(df$Project),split=','),trimws)
    #Load the data as a corpus
    myCorpus = Corpus(VectorSource(new1))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    myCorpus <- tm_map(myCorpus, toSpace, "/")
    myCorpus <- tm_map(myCorpus, toSpace, "@")
    myCorpus <- tm_map(myCorpus, toSpace, "\\|")
    # Convert the text to lower case
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    # Remove numbers
    myCorpus <- tm_map(myCorpus, removeNumbers)
    # Remove english common stopwords
    myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    #docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
    # Remove punctuations
    myCorpus <- tm_map(myCorpus, removePunctuation)
    # Eliminate extra white spaces
    myCorpus <- tm_map(myCorpus, stripWhitespace)
    myCorpus = tm_map(myCorpus, removePunctuation)
    myDTM = TermDocumentMatrix(myCorpus,control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    sort(rowSums(m), decreasing = TRUE)
  })
  
  
  ##Create Text Terms Object ########### 
  dat <-reactive({
  
    new1 <- lapply(strsplit(as.character(df$Project),split=','),trimws)
    #Load the data as a corpus
    input <- Corpus(VectorSource(new1))
    docs <- input
    
    #Inspect the content of the document
    #inspect(docs)
    
    ### Text transformation
    # Transformation is performed using tm_map() function to replace, for example, special characters from the text.
    #Replacing "/", "@" and "|" with space: 
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    #docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    ### Build a term-document matrix
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(
      rowSums(m),
      decreasing = TRUE
    )
    d <- data.frame(
      words = names(v),
      freq = v
    )
    
  })
  
### outputing graphs for second tab  
  output$plot1 <- renderPlot({
    v <- dat()
    barplot(v[1:input$limit,]$freq, las = 2, names.arg = v[1:input$limit,]$word,
            col ="lightblue", main ="Most frequent words",
            ylab = "Word frequencies")
  }) 
  
  output$plot2 <- renderPlot({
    
    data3 <- terms()
    # change it to a Boolean matrix
    data3[data3>=1] <- 1
    # transform into a term-term adjacency matrix
    data3 <- data3 %*% t(data3)
    new_d <-  data3[1:input$limit, 1: input$limit]
    g <- graph.adjacency(new_d, weighted=T, mode = "undirected")
    # set labels of vertices
    V(g)$label <- V(g)$name
    # set seed to make the layout reproducible
    set.seed(3952)
    layout1 <- layout.fruchterman.reingold(g)
    g4s <- simplify( g, remove.multiple = T, remove.loops = T, 
                     edge.attr.comb=c(weight="sum", type="ignore") )
    
    ## 2d interactive
    # tkplot(g4s, edge.arrow.size=.5, vertex.color="gold", vertex.size=20,
    # vertex.frame.color="gray", vertex.label.color="black",
    # vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)
    
    ## 3D interactive
    # rglplot(g4s,  layout=layout_with_fr, vertex.size=4,
    #         vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
    
    plot(g4s,layout=layout_with_fr, edge.arrow.size=.5, vertex.color="gold", vertex.size=20,
         vertex.frame.color="gray", vertex.label.color="black",
         vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)
    
    # com <- cluster_spinglass(g4s, spins=5)
    # V(g4s)$color <- com$membership+1
    # g4s <- set_graph_attr(g4s, "layout", layout_with_kk(g4s))
    # plot(g4s, vertex.label.dist=1.5)
  }) 
  
## Third tab - Word Cloud
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "Refresh" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix()
      })
    })
  })
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })  
### Fourth tab - Show raw data   
  output$table <- DT::renderDataTable({
    DT::datatable(df)
  })
  
}