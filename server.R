library(hypothesisr)

server <- function(input, output, session) {
  
  data <- eventReactive(input$action, {
    return(loadHS(input$url))
  })
  
  
  subsetData <- function(data){

    if(!is.null(input$show_docs))
      data <- data[unlist(data$document %in% input$show_docs), , drop=FALSE]
    
    if(!is.null(input$show_pages))
      data <- data[unlist(data$uri %in% input$show_pages), , drop=FALSE]
    
    if(!is.null(input$show_tags))
      data <- data[unlist(data$tags %in% input$show_tags), , drop=FALSE]
    
    if(!is.null(input$show_users))
      data <- data[unlist(data$user %in% input$show_users), , drop=FALSE]
    
    return(data)
  }

  
  output$dataset <- renderDataTable({
    data <- data()
    data <- subsetData(data)
    data <- data[ , input$show_vars, drop=FALSE]
    return(data)
  }, options = list(pageLength = 10))
  
  
  output$topDocs <- renderTable({
    data <- data()
    data <- subsetData(data)
    topap <- table(unlist(data$document))
    topap <- sort(topap, decreasing = TRUE)
    topap <- data.frame(Document = names(topap), Freq = as.vector(topap))
    if(nrow(topap) > 10) topap <- topap[1:10, , drop=FALSE]
    if(nrow(topap) < 1) topap <- data.frame(Document = NA, Freq = NA)
    return(topap)
  }, caption = "Top Annotated Documents",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  
  output$topPages <- renderTable({
    data <- data()
    data <- subsetData(data)
    topap <- table(unlist(data$uri))
    topap <- sort(topap, decreasing = TRUE)
    topap <- data.frame(Page = names(topap), Freq = as.vector(topap))
    if(nrow(topap) > 10) topap <- topap[1:10, , drop=FALSE]
    if(nrow(topap) < 1) topap <- data.frame(Page = NA, Freq = NA)
    return(topap)
  }, caption = "Top Annotated Pages",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  
  output$topTags <- renderTable({
    data <- data()
    data <- subsetData(data)
    topap <- table(tolower(unlist(data$tags)))
    topap <- sort(topap, decreasing = TRUE)
    topap <- data.frame(Tag = names(topap), Freq = as.vector(topap))
    if(nrow(topap) > 10) topap <- topap[1:10, , drop=FALSE]
    if(nrow(topap) < 1) topap <- data.frame(Tag = NA, Freq = NA)
    return(topap)
  }, caption = "Top Tags",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  
  output$topUsers <- renderTable({
    data <- data()
    data <- subsetData(data)
    topap <- table(tolower(unlist(data$user)))
    topap <- sort(topap, decreasing = TRUE)
    topap <- data.frame(User = names(topap), Freq = as.vector(topap))
    if(nrow(topap) > 10) topap <- topap[1:10, , drop=FALSE]
    if(nrow(topap) < 1) topap <- data.frame(User = NA, Freq = NA)
    return(topap)
  }, caption = "Most Active Users",
  caption.placement = getOption("xtable.caption.placement", "top"))
  

  output$docs <- renderUI({
    observeEvent(input$action, {
        data <- data()
        opts <- unique(unlist(data$document))
      updateSelectInput(session, "show_docs",
                        label = "Documents",
                        choices = opts
      )
      })
    selectizeInput("show_docs", "Documents", choices = " ", multiple = TRUE)
  })
  
  
  output$pages <- renderUI({
    observeEvent(input$action, {
      data <- data()
      opts <- unique(unlist(data$uri))
      updateSelectInput(session, "show_pages",
                        label = "Pages",
                        choices = opts
      )
    })
    selectizeInput("show_pages", "Pages", choices = " ", multiple = TRUE)
  })
  
  
  output$tags <- renderUI({
    observeEvent(input$action, {
      data <- data()
      opts <- unique(unlist(data$tags))
      updateSelectInput(session, "show_tags",
                        label = "Tags",
                        choices = opts
      )
    })
    selectizeInput("show_tags", "Tags", choices = " ", multiple = TRUE)
  })
  
  
  output$users <- renderUI({
    observeEvent(input$action, {
      data <- data()
      opts <- unique(unlist(data$user))
      updateSelectInput(session, "show_users",
                        label = "Users",
                        choices = opts
      )
    })
    selectizeInput("show_users", "Users", choices = " ", multiple = TRUE)
  })
  

  

  

  # loadHS <- function (uri) {
  #   require(hypothesisr)
  #   
  #   raw.data<-hs_search_all(custom = list(uri.parts = uri))
  #   raw.data[,c("highlighted","prefix","suffix","type")]<-NA
  #   
  #   ## loop through rows:
  #   for (i in 1:nrow(raw.data)){
  #     #annotation
  #     t<-raw.data$target[[i]]$selector[[1]]
  #     if(!is.null(t$exact)){
  #       raw.data$highlighted[i]<-t$exact[!is.na(gsub("\n","",t$exact))]
  #     } else {
  #       raw.data$highlighted[i]<- NA
  #       
  #     }
  #     
  #     #prefix
  #     if(!is.null(t$prefix)){
  #       raw.data$prefix[i]<-t$prefix[!is.na(gsub("\n","",t$prefix))]
  #     } else {
  #       raw.data$prefix[i]<- NA
  #     }
  #     
  #     
  #     #suffix
  #     if(!is.null(t$suffix)){
  #       raw.data$suffix[i]<-t$suffix[!is.na(gsub("\n","",t$suffix))]
  #     } else {
  #       raw.data$suffix[i]<- NA
  #     }
  #     
  #     
  #     #type
  #     #if annotation is filled...
  #     if(!is.na(raw.data$highlighted[i])){
  #       raw.data$type[i] <- "annotation"
  #       #else if there's a reference...
  #     } else if (!is.null(raw.data$references[[i]])){
  #       raw.data$type[i] <- "reply"
  #       #else label as page note
  #     } else {
  #       raw.data$type[i] <- "page note"
  #     }
  #     
  #     #clean-up tags
  #     if (length(raw.data$tags[[i]]) > 0){
  #       for(n in 1:length(raw.data$tags[[i]]))
  #         raw.data$tags[[i]][n]<-tolower(gsub("[[:punct:]]", "", raw.data$tags[[i]][n]))
  #     } else {
  #       raw.data$tags[[i]] <- "no tag"
  #     }
  #     
  #     #clean-up username
  #     raw.data$user[i]<-gsub("acct:", "", raw.data$user[i])
  #     raw.data$group[i]<-gsub("[[:punct:]]", "",raw.data$group[i])
  #   }
  #   
  #   
  #   clean.data<-raw.data[,c("created","updated","user","user_info.display_name","uri","document.title","group","type", "tags","hidden","flagged","id","references","links.html","links.incontext","text","prefix","highlighted","suffix")]
  #   
  #   rm(raw.data)
  #   return(clean.data)
  # }
  
loadHS <- function (uri) {
    require(hypothesisr)
    
    raw.data<-hs_search_all(custom = list(uri.parts = uri))
    raw.data[,c("highlighted","prefix","suffix","type","document","reply.reference")]<-NA
    raw.data$votes<-0
    
    ## loop through rows:
    for (i in 1:nrow(raw.data)){
      #annotation
      t<-raw.data$target[[i]]$selector[[1]]
      if(!is.null(t$exact)){
        raw.data$highlighted[i]<-t$exact[!is.na(gsub("\n","",t$exact))]
      } else {
        raw.data$highlighted[i]<- NA
        
      }
      
      #prefix
      if(!is.null(t$prefix)){
        raw.data$prefix[i]<-t$prefix[!is.na(gsub("\n","",t$prefix))]
      } else {
        raw.data$prefix[i]<- NA
      }
      
      
      #suffix
      if(!is.null(t$suffix)){
        raw.data$suffix[i]<-t$suffix[!is.na(gsub("\n","",t$suffix))]
      } else {
        raw.data$suffix[i]<- NA
      }
      
      
      #type
      #if annotation is filled...
      if(!is.na(raw.data$highlighted[i])){
        raw.data$type[i] <- "annotation"
        raw.data$reply.reference[i]<-NA
        #else if there's a reference...
      } else if (!is.null(raw.data$references[[i]])){
        raw.data$type[i] <- "reply"
        raw.data$reply.reference[i]<-unlist(raw.data$references[[i]])
        #else label as page note
      } else {
        raw.data$type[i] <- "page note"
        raw.data$reply.reference[i]<-NA
      }
      
      #clean-up tags
      if (length(raw.data$tags[[i]]) > 0){
        for(n in 1:length(raw.data$tags[[i]]))
          raw.data$tags[[i]][n]<-tolower(gsub("[[:punct:]]", "", raw.data$tags[[i]][n]))
      } else {
        raw.data$tags[[i]] <- "no tag"
      }
      
      #clean-up username
      raw.data$user[i]<-gsub("acct:", "", raw.data$user[i])
      raw.data$group[i]<-gsub("[[:punct:]]", "",raw.data$group[i])
      
      #unlist document titles
      raw.data$document[i]<-unlist(raw.data$document.title[[i]])
      
      #get the votes
      if(!is.na(raw.data$reply.reference[i])){
        if (raw.data$text[i] == "-1" | raw.data$text[i] == "1" | raw.data$text[i] == "+1"){
          raw.data[raw.data$id == raw.data$reply.reference[i],]$votes <-  raw.data[raw.data$id == raw.data$reply.reference[i],]$votes +  as.numeric(raw.data$text[i])
        }
      }
      
      
    }
    
    clean.data<-raw.data[,c("created","updated","user","user_info.display_name","uri","document","group","type", "tags","hidden","flagged","id","reply.reference","links.html","links.incontext","votes", "text","prefix","highlighted","suffix")]
    
    #rm(raw.data)
    return(clean.data)
  }
  
  
}

