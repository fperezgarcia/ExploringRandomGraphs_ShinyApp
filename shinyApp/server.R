library(shiny)
library(datasets)
library(igraph)
library(RColorBrewer)

#Variable with the App help text
helpTextCont <- "A random graph is a model network in which some specific set of parameters take fixed values, but the network is random in other respects. One of the simplest examples of a random graph is the G(n,p) network in which we fix the number of vertices n and p, the probability of edges between vertices. In this network the number of edges is not fixed but there are some values of p that suppose some important changes in the network:<br/>
<br/>
- if p = 1/n^2 , the network has some links<br/>
<br/>
- if p = 1/n^(3/2), the network has a component with at least three links<br/>
<br/>
- if p = 1/n, the network has a cycle<br/>
<br/>
- if p = log(n)/n, the network is connected (there are no isolated nodes)<br/>
<br/>
This shiny app, helps you test this characteristics of the random networks."

#Generates an Erdos-Renyi G(n,p) graph
generateGraph <- function (nodes = 100, prob = 1/200){
    g <- erdos.renyi.game(type="gnp", n=nodes, p=prob)
    
    g
}

#Creates a vertex property named color depending on
#the selected node metric (degree, pagerank or betweenness)
colorGraph <- function (g, color = "degree"){
    
    switch(color,
           degree = {
               colIdx = cut(degree(g),9,label=F)            
           },
           pagerank = {
               colIdx = cut(page.rank(g)$vector,9,label=F)
           },
           betweenness = {
               colIdx = cut(betweenness(g),9,label=F)
           }
    )
    
    V(g)$color = brewer.pal(9,"Blues")[colIdx]
    
    g
}

layoutGraph <- function(g){
    l <- layout.fruchterman.reingold(g, niter=10000)
    
    l
}

#plots the graph
plotGraph <- function (g, l){
    l    
    plot.igraph(g,
                vertex.label=NA, 
                edge.arrow.size=0, 
                edge.curved=FALSE,
                layout = l,
                vertex.color=V(g)$color,
                vertex.size=6)
}

paramNames <- c("nodes", "prob")

# Define server logic required to generate the graph 
# and its degree distribution
shinyServer(function(input, output) {

    getParams <- function() {
        #input$update
        
        params <- lapply(paramNames, function(p) {
            input[[p]]
        })
        names(params) <- paramNames
        
        params
    }

    # Called when nodes or probability are changed
    # When color is changed none of the following functions is called
    aGraph <- reactive(do.call(generateGraph, getParams()))
    aLayout <- reactive({layoutGraph(aGraph())})
                        
    output$hint <- renderUI({
        
        str0 <- paste0("Link probability thresholds for <b>", input$nodes, 
                       "</b> nodes:")
        str1 <- paste0("if p = 1/n^2 = <b>", round(1/input$nodes^2,6), 
                       "</b>, the network has some links (avg deg 1/n)")
        str2 <- paste0("if p = 1/n^(3/2) = <b>", round(1/input$nodes^(3/2),3), 
                       "</b>, the network has a component with at least three links")
        str3 <- paste0("if p = 1/n = <b>", round(1/input$nodes,3), 
                       "</b>, the network has a cycle")
        str4 <- paste0("if p = log(n)/n = <b>", round((log(input$nodes) / input$nodes), 3), 
                       "</b>, the network is connected")
        
        HTML(paste(str0, str1, str2, str3, str4, sep = '<br/>'))
    })
    
    output$graphPlot <- renderPlot({
        g <- aGraph()
        l <- aLayout()
        g <- colorGraph(g, input$color)
        
        plotGraph(g, l)
    })
    
    output$histPlot <- renderPlot({
        hist(degree(aGraph()), xlab="Degree", main="Histogram of Degree")
    })
    
    output$helpText <- renderUI({
        HTML(paste0(helpTextCont))
    })
        
})