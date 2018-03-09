

ui <- fluidPage(
  titlePanel("Hypothes.is Explorer"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 
                 textInput("url", label = h5("Website"), value = "Enter website..."),
                 
                 actionButton("action", label = "Download"),
                 
                 hr(),
                 
                 h4("Subset"),
                 
                 uiOutput("docs"),
                 uiOutput("pages"),
                 uiOutput("tags"),
                 uiOutput("users"),
                 br(),
                 
                 selectInput("show_vars", "Variables (in Data View)", 
                             c("created", "updated", "user", "user_info.display_name", "uri", "document", 
                               "group", "type", "tags", "hidden", "flagged", "id", "reply.reference", "links.html", 
                               "links.incontext", "votes", "text", "prefix", "highlighted", "suffix"),
                             selected = c("tags", "text", "highlighted"),
                             multiple = TRUE, selectize = TRUE, width = NULL, size = NULL),
                 
                 
                 fluidRow(column(12, verbatimTextOutput("value")))
                 
                 
                 
                 ), #sidebarPanel close
    
    
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           
                           fluidRow(
                             column(width = 8,
                                    br(),
                                    tableOutput("topDocs")
                                    ),
                             column(width = 4, 
                                    br(),
                                    tableOutput("topTags")
                                    )),
                           
                           fluidRow(
                             column(width = 8,
                                    br(),
                                    tableOutput("topPages")
                             ),
                             column(width = 4, 
                                    br(),
                                    tableOutput("topUsers")
                             )),

                           
                           br(),
                           br(),
                           br()
                           
                           
                           ), #Summary Panel close
                  

                  tabPanel("Data", 
                           
                           br(),
                           br(),
                           dataTableOutput("dataset"))
                  
      ) #tabsetPanel close
      

      
      # plotOutput("plot")
              ) #mainPanel close
  )
)

