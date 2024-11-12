####
# Setup the basic server components ###################
####
library(shiny)
library(shinylive)

server <- function(input, output, session) {
  
  output$network <- renderVisNetwork({
    
    plot_network(nodes, edges, 111, "layout_with_dh",
                 pw="100%", ph="100%", fsize=24) %>%
      visOptions(nodesIdSelection = T) %>%
      visExport()
  })
  
  #when user picks a node on the graph ...
  observeEvent(input$network_selected, {
    
    if (is.character(input$network_selected) && nchar(input$network_selected) > 0) {
      selectedNodeId <- input$network_selected
      
      nodes$R_timestamp <- format(nodes$R_timestamp,'%Y-%m-%dT%H:%M:%S+000')
      
      # Get and display node info
      selectedNodeInfo <- nodes %>%
        filter(id == selectedNodeId) %>%
        select(all_of(c("id","label","type","nodesubtype",
                        "group","R_timestamp")))
      
      output$nodeInfo <- renderTable({
        selectedNodeInfo
      })
      
      # get and display multiple attribute info; built table in/out list for code,cols for data
      if (nodes[nodes$id == selectedNodeId, "type"] == "Code") {
        selectedAttrInfo <- all_inout         %>%
          filter(select_id == selectedNodeId)  %>%
          select(all_of(c("id","label","in_out",
                          "nodesubtype","group","R_timestamp")))
      }
      if (nodes[nodes$id == selectedNodeId, "type"] == "Data") {
        selectedAttrInfo <- nodexCols %>%
          filter(id == selectedNodeId) 
      }
      output$multAttrInfo <- renderDataTable({
        selectedAttrInfo
      })
    }
  })
  session$onSessionEnded(function() {stopApp()}) # added due to continuous crashing
}

####
# Setup the basic UI components #################################
####

ui <- fluidPage(
  fluidRow(
    column(12, 
           tags$h2("Codebase Network Graph - Select a Node to see attributes"),
           visNetworkOutput("network")
    )
  ),
  fluidRow(
    column(12,
           tags$h2("Node Attributes"),
           tableOutput("nodeInfo")      
    )
  ),
  fluidRow(
    column(12,
           tags$h2("List Attributes for Node"),
           dataTableOutput("multAttrInfo")
    )
  )
)

####
# Make our first Shiny! ###################################
####

shinyApp(ui, server)