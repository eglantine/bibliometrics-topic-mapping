library(shiny)
library(arules)
library(arulesViz)
library(jsonlite)
library(plyr)
library(igraph)
library(httr)

# Get your Elsevier API key at https://dev.elsevier.com 
ElsevierAPIkey = "your Elsevier API key"

# Get your Proxem API key at https://market.mashape.com/Proxem/ontology-based-topic-detection 
ProxemAPIkey = "your Proxem API key"


shinyServer(function(input, output) {
  titleOutput <- eventReactive(input$makeSearch, {
    
    getArticleTitles = function(query, ElsevierAPIkey){
      nb = "25"
      url = paste0("http://api.elsevier.com/content/search/scopus",
                   "?query=TITLE-ABS-KEY(",
                   URLencode(query),
                   ")",
                   "&count=",
                   nb
                   )

      doc = GET(url, add_headers("X-ELS-APIKey" = ElsevierAPIkey))
      totalResults = as.numeric(content(doc)$'search-results'$'opensearch:totalResults')
      output$resultNumber = renderText({paste0("Total amount of results: ",totalResults)})
    
      #Initializes loop with first API call
      itemList = content(doc)$'search-results'$'entry'
      itemList = lapply(itemList,unlist)
      itemList = lapply(itemList, lapply, function(x) ifelse(is.null(x), NA, x))
      itemTable = lapply(itemList, data.frame, stringsAsFactors = F)
      itemTable = rbind.fill(itemTable)

      if(totalResults > 25){
        
        if(totalResults > 5000){
          queriedResults = 5000
        }
        
        if(input$sample == TRUE){
          queriedResults = 100
        }

        # Sets list of API requests URLs
        pagesNb = floor(as.numeric(queriedResults)/as.numeric(nb)) -1
        urlList = paste0(url,"&start=",1:pagesNb*as.numeric(nb))
        
        # Makes API calls and stores results in a data.frame
        
        withProgress(message = 'Title retrieval...', value = 0, {
          
          i=1
          for(i in 1:length(urlList)){
            doc = GET(urlList[i], add_headers("X-ELS-APIKey" = ElsevierAPIkey))
            
            if(length(content(doc)$'search-results'$'entry')>1){
              otherItemList = content(doc)$'search-results'$'entry'
              otherItemList = lapply(otherItemList,unlist)
              otherItemList = lapply(otherItemList, lapply, function(x) ifelse(is.null(x), NA, x))
              otherItemTable = lapply(otherItemList, data.frame, stringsAsFactors = F)
              otherItemTable = rbind.fill(otherItemTable)
              itemTable = merge(itemTable,otherItemTable,all = T)
            }
            print(paste0("Query ",i+1," in ",pagesNb+1, " : ", tail(otherItemTable$dc.title,1)))
            
            incProgress(1/pagesNb, detail = paste0("Query ",i+1," in ",pagesNb+1))
            
          }
          
        })
        
      }
      
      result = itemTable$dc.title
      
      if(input$deduplicate == TRUE){
        result = unique(result)
      }
      
      return(result)
      }
    
    if(is.null(input$query)){
      return(NULL)
    }
    
    titles = NULL
    titles = getArticleTitles(query=input$query,ElsevierAPIkey=ElsevierAPIkey)
    titles
  })
  
  output$titles <- renderTable({
    head(data.frame(titleOutput()), 25)
    },colnames = "F")
  
  topicOutput = reactive({

    getTopicsFromCorpus = function(corpus, ProxemAPIkey, nb=10, lang="") {
      corpus = lapply(corpus, as.character)
      corpus = as.list(corpus)
      corpus = unlist(corpus)
      names(corpus) = NULL
      url = paste0("https://proxem-thematization.p.mashape.com/api/wikiAnnotator/GetCategoriesForCorpus?nbtopcat=",nb,ifelse(lang != "", paste0(url,"&lang=",lang),""))
      json = paste('{"documents":',toJSON(corpus),"}")
      doc = POST(url,body=enc2utf8(json), content_type("application/json"), add_headers('X-Mashape-Key'= ProxemAPIkey))
      # result = fromJSON(content(doc, as="text"))
      result = content(doc)
      
      if(class(result)=="character"){
        print(result)
      } else{
        result = result$documents
        
        i = 1;j=1;topicTable=data.frame()
        for (i in 1:length(result)){
          if(length(result[[i]])>0){
            for (j in 1:length(result[[i]])){
              tmp = result[[i]][[j]]$name
              topicTable[i,j] = tmp        
            }
          }
        }
        topicTable  
      }  
    }
    
    getTopicsBatchCall = function(corpus,batchLenght=25, ProxemAPIkey, nb=10, lang=""){
      
      if(length(corpus)<=batchLenght){
        topicTable = getTopicsFromCorpus(corpus=corpus,ProxemAPIkey = ProxemAPIkey,nb=10,lang="")
      
        } else {
        
        loopLength = (length(corpus)-length(corpus)%%batchLenght)/batchLenght + ifelse(length(corpus)%%batchLenght>0,1,0)
        firstDoc = (1:loopLength)*batchLenght + 1 - batchLenght
        lastDoc = (1:loopLength)*batchLenght
        if(length(corpus)%%batchLenght>0){
          lastDoc[length(lastDoc)] = firstDoc[length(firstDoc)] + length(corpus)%%batchLenght - 1
        }
        
        withProgress(message = 'Topic detection...', value = 0, {
          
          i = 1;topicTable = data.frame()
          for(i in 1:loopLength){
            tempTopicTable = getTopicsFromCorpus(corpus=corpus[firstDoc[i]:lastDoc[i]],ProxemAPIkey = ProxemAPIkey,nb=10,lang="")
            topicTable = rbind.fill(topicTable,tempTopicTable)
            print(paste("Itération",i,"sur",loopLength))
            incProgress(1/loopLength, detail = paste0("Batch ",i," in ",loopLength))
            
          }
        }) 
      }
      topicTable
    }

    topics = NULL
    topics = getTopicsBatchCall(corpus = titleOutput(),batchLenght = 25, ProxemAPIkey = ProxemAPIkey,nb=5,lang="")
    topics
    
  })
  
  output$contents = renderDataTable({
    as.data.frame(topicOutput())
  })
  
  rulesOutput = reactive({
    
    AssociationComputation = function(df, support = input$support, confidence = input$confidence){
      
      df = as.data.frame(unclass(df))
      names(df)= NULL
      df = apply(df,1,as.list)
      df = lapply(df, unlist)
      names(df) = paste0("Tr",c(1:length(df)))
      df = lapply(df,function(x) x[!is.na(x)])      
      transactions = as(df, "transactions")
      formalRules = apriori(transactions,parameter=list(support=support, confidence=confidence))
      return(formalRules)
    }
    
    rules = AssociationComputation(df=topicOutput())
    rules
  })
  
  output$rules <- renderPrint({
    capture.output(rulesOutput())
  })
  
  graphOutput = reactive({
    
    RulesGraphGeneration = function(formalRules){
      
      # Simplifies rules
      if(length(formalRules)>0){
        rulesTable =  as(formalRules,"data.frame")
        rulesTable$rules = gsub("(\\{|\\})","",rulesTable$rules)
        rulesTable$rules = gsub(" => ",",",rulesTable$rules)
        rulesTable$rules = gsub("X[0-9]=",",",rulesTable$rules)
        rulesTable$rules = gsub("=",",",rulesTable$rules)
        rulesTable$rules = gsub("^,","",rulesTable$rules)
        rulesTable$rules = gsub(",$","",rulesTable$rules)
        rulesTable$rules = gsub(",,",",",rulesTable$rules)
        rulesTable$rules = gsub(", ",",",rulesTable$rules)
        rulesTable = rulesTable[grep(",",rulesTable$rules),]
        
        names(rulesTable) = c("rules", "computedSupport", "computedConfidence", "computedLift")
        
        rulesList = rulesTable$rules
        
        rulesList <- strsplit(as.character(rulesList),",")
        rawRulesTable <- rbind.fill(lapply(rulesList, function(x) as.data.frame(t(x),stringsAsFactors=F)))
        
        count = rowSums(!is.na(rawRulesTable))
        mini.count = count-1

        # Transforms associations rules to graph
        i=1; j=1; k=1;index =1;Source=NULL;Target=NULL; computedLift = NULL; computedConfidence = NULL; computedSupport =NULL; 
        
        for(i in 1:nrow(rawRulesTable)){
          
          for (j in 1:mini.count[i]) {
            for (k in (j+1):count[i]) {
              
              Source[index] = rawRulesTable[i,j]
              Target[index] = rawRulesTable[i,k]
              computedLift[index] = rulesTable$computedLift[i]
              computedConfidence[index] = rulesTable$computedConfidence[i]
              computedSupport[index] = rulesTable$computedSupport[i]
              index = index + 1
              
            }
          }
        }
        
        rulesPairsTable = data.frame(Source, Target, computedLift, computedConfidence, computedSupport, stringsAsFactors=F)
        rulesPairsTable = rulesPairsTable[!is.na(rulesPairsTable$Target),]
        rulesPairsTable
      }
      
    }
    
    graph = RulesGraphGeneration(rulesOutput())
    graph
  })

  output$graphPlot <- renderPlot({
    plot(rulesOutput()[1:100], method='graph')
  }, height = 800)
  
  rulesPairsOutput = reactive({
    rulesPairsGraph = graph.data.frame(graphOutput())
    rulesPairsGraph
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      name = switch(input$outputFormat,
                    title = {
                      paste0("Titles - ",input$query," - ",Sys.Date(),".csv")
                    },
                    topic = {
                      paste0("Topics - ",input$query," - ",Sys.Date(),".csv")
                    },
                    gml = {
                      paste0("Rules - ",input$query," - ",Sys.Date(),".gml")
                    })
    },
    content = function(file) {
      
      switch(input$outputFormat,
             title = {
               write.csv(titleOutput(),file,row.names=F)
             },
             topic = {
               write.csv(topicOutput(),file,row.names=F)
             },
             gml = {
               write.graph(rulesPairsOutput(), file, format="gml")
             })
      
    }
  )
})
