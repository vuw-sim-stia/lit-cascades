# Preprocessing script for a tool prototype that enables close and distant reading of literature.
# For demonstration purposes the tool is configured to work on various Victorian Novels from Charles Dickens and other authors.
# Authors: Markus Luczak-Roesch, Tom Goldfinch

# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
#                                               "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

#import libs
library(plyr)
library(tm)
library(stringr)
library(RTextTools)
library(NLP)
library(visNetwork)
library(digest)
library(entropy)
library(scatterplot3d)
library(RColorBrewer)
library(tidyr)
library(igraph)
library(ggplot2)
library(plotly)
library(gtools)
library(networkDynamic)
library(ndtv)
library(tsna)
library(intergraph)
library(openNLP)
library(cldr)
library(RWeka)
library(vegan)
library(poweRlaw)
library(plyr)
library(vegan)

#housekeeping and helpers
options(scipen = 999)

degree.distribution <- function (graph, cumulative = FALSE, ...) 
{
  if (!is.igraph(graph)) {
    stop("Not a graph object")
  }
  cs <- degree(graph, ...)
  hi <- hist(cs, -1:max(cs), plot = FALSE)$count
  if (!cumulative) {
    res <- hi
  }
  else {
    res <- rev(cumsum(rev(hi)))
  }
  res
}

## TIC
tic_generate <- function(inputsequence) {
  nodes <- c()
  links <- c()
  roots <- c()
  last_node <- list()
  
  for(pp in inputsequence$i){
    tags <- unlist(strsplit(as.character(inputsequence[which(inputsequence[,1] == pp),2]), split=", "))
    nodes <- rbind(nodes, c(inputsequence[which(inputsequence[,1]== pp),1],
                            as.character(inputsequence[which(inputsequence[,1] == pp),2]),
                            inputsequence[which(inputsequence[,1] == pp),1]))
    if(length(tags)>0){
      for(jj in 1:length(tags)){
        cur_tag <- tags[jj]
        if(!is.null(unlist(last_node[cur_tag]))){ 
          source_node <- last_node[cur_tag]
          target_node <- pp
          links <- rbind(links, c(source_node, target_node, cur_tag))
        } else {
          roots <- rbind(roots, c(pp, cur_tag))
        }
        last_node[cur_tag] <- pp
      }
    }
    
  }
  return(list(nodes, links, roots))
}

#set working directoy
setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/lit-cascades/src/")

#slice_sizes <- c(1000,500,250)
slice_sizes <- c(1000)

#get all text and char files
allCharFiles <- list.files("../resources/Character Lists/")
allCharFiles <- allCharFiles[which(allCharFiles!="Icon\r")]
allTextFiles <- list.files("../resources/Text Files")
allTextFiles <- allTextFiles[which(allTextFiles!="Icon\r")]
#select which texts to process
#litSources <- c('greatexpectations','davidcopperfield','chuzzlewit','bleakhouse','middlemarch','ourmutualfriend','phineasfinn','pickwick','smallhouse','tessofthedurbervilles')
#litSources <- c('bleakhouse','ourmutualfriend')
allOutput <- c()

method <- "chars" # one of "chars" "ngram" "nouns"

for(sliceSize in slice_sizes){
  dir.create(file.path("TLit/www/output/", sliceSize), showWarnings = FALSE)
  
  for(nextRun in 1:length(allTextFiles)){
    theSource <- gsub(' ','_',gsub('[[:digit:]][[:digit:]] ','',gsub(' text.txt','',allTextFiles[nextRun])))
    allOutput <- c(allOutput,theSource)
    sourceText <- readChar(paste0('../resources/Text Files/',allTextFiles[nextRun]), file.info(paste0('../resources/Text Files/',allTextFiles[nextRun]))$size)
    processedText <- gsub("\\r\\n", " ", sourceText,perl=T)
    processedText <- gsub("\\n", " ", processedText,perl=T)
    
    #split by number of words
    #full_text <- strsplit(processedText, "\\s+")[[1]]
    #groupA <- rep(seq(ceiling(length(full_text)/1200)), each=1200)[1:length(full_text)]
    #words300A <- split(full_text, groupA)
    #words300B <- words300A
    
    #split by number of words and chapters
    #full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\s.]?)", perl=T)[[1]]
    full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\.]?[\\s.]?)", perl=T)[[1]]
    words300B <- c()
    tmp <- sapply(full_text,function(x){
      snippet <- strsplit(x, "\\s+")[[1]]
      if(length(snippet>1)){
        groupA <- rep(seq(ceiling(length(snippet)/sliceSize)), each=sliceSize)[1:length(snippet)]
        words300A <- split(snippet, groupA)
        words300B <- c(words300B,words300A)
      }
    })
    
    for(s in 1:length(tmp)){
      if(length(tmp[[s]]) > 0){
        words300B <- c(words300B,tmp[[s]])
      }
    }
    
    words300A <- words300B
    if(method == "chars"){
      #character list
      tmp <- readLines(paste0('../resources/Character Lists/',allCharFiles[nextRun]))
      
      #for short character lists
      #tmp <- readLines(paste('../resources/',theSource,'_chars_short.txt',sep=''))
      
      charIds <- sapply(strsplit(tmp,": "),function(x){ x[[1]] })
      chars <- sapply(strsplit(tmp,": "),function(x){ strsplit(x[[2]],", ") })
      tmp <- c()
      for(i in 1:length(chars)){
        nextChar <- charIds[i]
        allNames <- chars[[i]]
        for(j in 1:length(allNames)){
          tmp <- rbind(tmp,c(nextChar,allNames[j]))
        }
      }
      chars <- data.frame(tmp,stringsAsFactors = F)
      colnames(chars)<-c('id','name')
      chars$len <- nchar(chars$name)
      chars$len<-as.numeric(chars$len)
      chars <- chars[ order(-chars[,3]), ]
      
      matches <- list()
      for(j in 1:length(words300A)){
        slicematch <- list()
        for(k in 1:nrow(chars)){
          needle <- chars[k,2]
          matched <- unlist(gregexpr(paste("[\\s\\\\\"](",needle,")[\\';,.:\\s\\\\\"]",sep=""), paste(unlist(words300A[j]),collapse=' '),perl=TRUE))
          multiple <- F
          if(k+1<=nrow(chars)){
            if(length(which(chars[k+1:nrow(chars),2]==needle))>0) multiple <- T
          }
          if(!multiple){
            nWords <- sapply(gregexpr("\\W+", needle), length) +1
            words300A[j] <- strsplit(gsub(needle,paste(replicate(nWords, "FOOBAR"), collapse = " "),paste(unlist(words300A[j]),collapse=' '))," ")
          }
          if(matched != -1){
            for(l in 1:length(matched)){
              if(is.null(slicematch[[as.character(matched[l])]])){
                slicematch[[as.character(matched[l])]] <- needle
              } else{
                if(length(slicematch[[as.character(matched[l])]]) < length(needle)){
                  slicematch[[as.character(matched[l])]] <- needle
                }
              }
            }
          }
        }
        matches[[j]]<-slicematch
      }
      
      charDS <- data.frame(x = numeric(), y = character(), stringsAsFactors = FALSE)
      
      for(i in 1:length(matches)){
        if(length(matches[i][[1]])>0){
          nextMatch <- unique(unlist(matches[i]))
          nextMatchStr <- c()
          for(j in 1:length(nextMatch)){
            if(length(nextMatchStr) == 0) {nextMatchStr <- c(chars[which(chars[,2]==nextMatch[j]),1])}
            else {nextMatchStr <- c(nextMatchStr,chars[which(chars[,2]==nextMatch[j]),1])}
          }
          charDS <- rbind(charDS,data.frame(i=i,identifiers=paste(sort(unique(nextMatchStr)),collapse=', ')),stringsAsFactors=F)
        } else{
          charDS <- rbind(charDS,data.frame(i=i,identifiers=""),stringsAsFactors=F)
        }
      }
    } else if(method == "nouns"){
      ## Need sentence and word token annotations.
      sent_token_annotator <- Maxent_Sent_Token_Annotator()
      word_token_annotator <- Maxent_Word_Token_Annotator()
      pos_tag_annotator <- Maxent_POS_Tag_Annotator()
      
      charDS <- data.frame(x = numeric(), y = character(), stringsAsFactors = FALSE)
      for(i in 1:length(words300A)){
        s<-as.String(paste(unlist(words300A[i]),collapse=' '))
        
        a2 <- NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
        a3 <- NLP::annotate(s, pos_tag_annotator, a2)
        a3w <- subset(a3, type == "word")
        tags <- sapply(a3w$features, `[[`, "POS")
        nNouns<-length(which(tags=='NN' | tags=='NNS'))
        nTags <- length(tags)
        nounsOnly <-s[a3w][which(tags=='NN' | tags=='NNS')]
        matrix <- create_matrix(nounsOnly, stemWords=TRUE, removeStopwords=TRUE, minWordLength=3)
        charDS <- rbind(charDS,data.frame(i,paste(unique(colnames(matrix)),collapse=', ')),stringsAsFactors=F)
      }
    } else if(method == "ngram"){
      nGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
      
      charDS <- data.frame(x = numeric(), y = character(), stringsAsFactors = FALSE)
      for(i in 1:length(words300A)){
        a <- VCorpus(VectorSource(c(as.String(paste(unlist(words300A[i]),collapse=' ')))))
        a <- tm_map(a, removeNumbers)
        a <- tm_map(a, removePunctuation)
        a <- tm_map(a , stripWhitespace)
        a <- tm_map(a, tolower)
        a <- tm_map(a, removeWords, stopwords("english")) 
        a <- tm_map(a, stemDocument, language = "english")
        a <- tm_map(a, PlainTextDocument)
        tdm <- TermDocumentMatrix(a, control = list(tokenize = nGramTokenizer))
        #tdm <- removeSparseTerms(tdm, 0.75)
        
        charDS <- rbind(charDS,data.frame(i,paste(unique(tdm$dimnames$Terms),collapse=', ')),stringsAsFactors=F)
      }
    }
    
    tic <- tic_generate(charDS)
    
    ## Extracting nodes, links, roots from network model computed in tic_generate.
    nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tags = unlist(tic[[1]][,2]),
                        dpub = unlist(tic[[1]][,3]), stringsAsFactors = F)
    links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                        tag = unlist(tic[[2]][,3]), stringsAsFactors = F)
    roots <- data.frame(root_node_id = unlist(tic[[3]][,1]),
                        tag = unlist(tic[[3]][,2]), stringsAsFactors = F)
    
    write.table(nodes,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_nodes.csv"))
    write.table(links,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_links.csv"))
    write.table(roots,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_roots.csv"))
    
    linksDelta <- as.integer(links$target)-as.integer(links$source)
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta.jpg"))
    plot(linksDelta,type='l')
    abline(h=mean(linksDelta),col="red")
    abline(h=median(linksDelta),col="blue")
    dev.off()
    
    linksDelta.count <- count(linksDelta)
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri.jpg"))
    plot(linksDelta.count$x,linksDelta.count$freq,pch=20)
    dev.off()
    
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_loglog.jpg"))
    plot(linksDelta.count$x,linksDelta.count$freq,pch=20,log="xy")
    dev.off()
    
    #power law?
    
    m_bl = displ$new(linksDelta)
    est = estimate_xmin(m_bl)
    m_bl$setXmin(est)
    m_ln = dislnorm$new(linksDelta)
    est = estimate_xmin(m_ln)
    m_ln$setXmin(est)
    m_pois = dispois$new(linksDelta)
    est = estimate_xmin(m_pois)
    m_pois$setXmin(est)
    
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_plaw.jpg"))
    plot(m_bl, ylab="CDF")
    #text(100,0.15,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
    lines(m_bl, col=2)
    lines(m_ln, col=3)
    lines(m_pois, col=4)
    dev.off()
    
    
    #export text as HTML file to allow jumoing to respective slice
    htmlHead <- "<htlm><head><style type='text/css'>body{font-family: Verdana, sans-serif;}</style><script>function foo(){var hash = window.location.hash.substring(1); document.getElementById(hash).style.border = '2px solid red'; document.getElementById(hash).style.padding = '15px'; window.onfocus=function(event){location.reload();}}</script></head><body onload='foo()'>"
    htmlTail <- "</body>"
    
    htmlContent <- ""
    
    for(h in 1:length(words300B)){
      targets <- unique(unlist(links[which(links[,1]==h),2]))
      sources <- unique(unlist(links[which(links[,2]==h),1]))
      
      sLinks <- ""
      if(length(sources)>0){
        for(aLink in 1:length(sources)){
          sLinks <- paste0(sLinks," - <a href='../",theSource,"_textchunks.html#slice-",sources[aLink],"' style='text-decoration: none; color: #ccc;'>Slice ",sources[aLink],"</a>")
        }
      }
      
      tLinks <- ""
      if(length(targets)>0){
        for(aLink in 1:length(targets)){
          tLinks <- paste0(tLinks," - <a href='../",theSource,"_textchunks.html#slice-",targets[aLink],"' style='text-decoration: none; color: #ccc;'>Slice ",targets[aLink],"</a>")
        }
      }
      htmlContent <- paste(htmlContent,"<p style='font-size: 0.8em; color:#ccc;'>Slice ",h," is linked from: ",sLinks,"</p><p id='slice-",h,"'><a name='slice-",h,"'>",paste(unlist(words300B[h]),collapse=' '),"</a></p><p style='font-size: 0.8em; color:#ccc;text-decoration: none;'>links to: ",tLinks,"</p><hr>",sep='')
    }
    write(paste(htmlHead,htmlContent,htmlTail,sep=''),file=paste('TLit/www/output/',sliceSize,'/',theSource,'_textchunks.html',sep=''))
    
    library(networkDynamic)
    library(ndtv)
    library(tsna)
    
    nd <- as.networkDynamic(network.initialize(0))
    set.network.attribute(nd,"vertex.pid","vertex.names")
    set.network.attribute(nd,"edge.pid","edge.names")
    for(i in 1:nrow(links)){
      fromN <- get.vertex.id(nd,unlist(links[i,1]))
      if(is.na(fromN)){
        add.vertices(nd,nv=1,vertex.pid=c(unlist(links[i,1])))
        fromN <- get.vertex.id(nd,unlist(links[i,1]))
        set.vertex.attribute(nd,'content',unlist(nodes[which(nodes[,1]==unlist(links[i,1])),2]),v=c(fromN))
        set.vertex.attribute(nd,'content2',paste(unlist(words300B[unlist(nodes[which(nodes[,1]==unlist(links[i,1])),1])]),collapse=' '),v=c(fromN))
        set.vertex.attribute(nd,'step',unlist(nodes[which(nodes[,1]==unlist(links[i,1])),1]),v=c(fromN))
        activate.vertices(nd,onset=as.numeric(unlist(links[i,2])),terminus=max(as.numeric(unlist(links[,2])))+1,v=c(fromN))
      }
      
      toN <- get.vertex.id(nd,unlist(links[i,2]))
      if(is.na(toN)){
        add.vertices(nd,nv=1,vertex.pid=c(unlist(links[i,2])))
        toN <- get.vertex.id(nd,unlist(links[i,2]))
        set.vertex.attribute(nd,'content',unlist(nodes[which(nodes[,1]==unlist(links[i,2])),2]),v=c(toN))
        set.vertex.attribute(nd,'content2',paste(unlist(words300B[unlist(nodes[which(nodes[,1]==unlist(links[i,2])),1])]),collapse=' '),v=c(toN))
        set.vertex.attribute(nd,'step',unlist(nodes[which(nodes[,1]==unlist(links[i,2])),1]),v=c(toN))
        activate.vertices(nd,onset=as.numeric(unlist(links[i,2])),terminus=max(as.numeric(unlist(links[,2])))+1,v=c(toN))
      }
      edgeID <- which(get.edge.attribute(nd,'ident')==paste(unlist(links[i,1]),unlist(links[i,2]),sep='-'))
      if(length(edgeID)==0){
        add.edges.active(nd,onset=as.numeric(unlist(links[i,2])), terminus=max(as.numeric(unlist(links[,2])))+1,head=toN,tail=fromN,names.eval=list(list('set','ident','width')),vals.eval=list(list(links[i,3][[1]],paste(unlist(links[i,1]),unlist(links[i,2]),sep='-'),1)))
      } else{
        linkLabel <- paste(get.edge.attribute(nd,'set',unlist=FALSE)[[edgeID]],unlist(links[i,3]),sep=", ")
        set.edge.attribute(nd, attrname='set', value=linkLabel, e=c(edgeID))
        set.edge.attribute(nd, attrname='width', value=get.edge.attribute(nd,'width',unlist=FALSE)[[edgeID]]+1, e=c(edgeID))
      }
    }
    
    compute.animation(nd, animation.mode = "kamadakawai", chain.direction=c('forward'),default.dist=10)
    
    #interactive
    render.d3movie(nd, filename=paste("TLit/www/output/",sliceSize,"/",theSource,"_dynamic-network.html",sep=''),launchBrowser=F, 
                   displaylabels = T, label=nd %v% "vertex.names", label.cex=.5,
                   vertex.col="orange",edge.col="darkgray",
                   vertex.cex = .5, vertex.border="black",
                   vertex.tooltip = paste("<span style='font-size: 10px;'><b>Slice:</b>", (nd %v% "step") , "<br />","<b>Matched information:</b>", (nd %v% "content"), "<br /><a href='",paste("",theSource,"_textchunks.html#slice-",(nd %v% "step"),sep=''),"' target='blank'>Go to content</a><br />"),
                   edge.lwd = .3,
                   object.scale = 0.1,
                   edge.tooltip = paste("<b>Link:</b>", (nd %e% "set"),"</span>" ))
    
    detach("package:ndtv", unload=TRUE)
    detach("package:tsna", unload=TRUE)
    detach("package:sna", unload=TRUE)
    detach("package:networkDynamic", unload=TRUE)
    
    uniqueLinks <- data.frame(id1=character(0),id2=character(0),label=character(0))
    convLinks <- as.data.frame(links,stringsAsFactors = F)
    for(z in 1:nrow(convLinks)){
      uniqueLinks <- rbind(uniqueLinks,data.frame(id1=unlist(convLinks[z,1]),id2=unlist(convLinks[z,2]),label=paste(convLinks[which(unlist(convLinks[,1])==unlist(convLinks[z,1]) & unlist(convLinks[,2])==unlist(convLinks[z,2])),3],collapse=', '),stringsAsFactors = F))
    }
    uniqueLinks <- unique(uniqueLinks)
    colnames(uniqueLinks) <- c("id1","id2","label")
    colnames(links) <- c("id1","id2","label")
    g <- graph.data.frame(uniqueLinks,directed=TRUE)
    
    nLabels <- c()
    for(z in V(g)$name){
      nLabels <- c(nLabels,paste(unique(unlist(strsplit(paste(uniqueLinks[which(uniqueLinks$id1==z | uniqueLinks$id2==z),3],collapse = ', '),', '))),collapse = ', '))
    }
    
    V(g)$content <- nLabels
    
    V(g)$frame.color <- "white"
    V(g)$color <- "orange"
    
    deg <-  igraph::degree(g, mode="all")
    V(g)$size <- deg*3
    
    E(g)$width <- 0.1
    
    lay <- layout_with_dh(g)
    lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
    minC <- rep(-Inf, vcount(g))
    maxC <- rep(Inf, vcount(g))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(g, minx=minC, maxx=maxC,
                         miny=minC, maxy=maxC)
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_net.pdf",sep=''))
    plot(g, layout=co*1.0, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
    dev.off()
    
    deg <-  igraph::degree(g, mode="all")
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_net_degdistri.pdf",sep=''))
    deg.dist <- degree_distribution(g, cumulative=T, mode="all")
    plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")
    dev.off()
    
    degd <- degree.distribution(g)
    wtc <- cluster_walktrap(g)
    gstat <- c(diameter(g),min(degd),max(degd),mean(degd),edge_density(g),modularity(wtc))
    write.table(c(theSource,gstat),paste0("TLit/www/output/",sliceSize,"/",theSource,"_netstat_combined.csv"),append = T,col.names = F,row.names = F)
    write.csv2(gstat,paste("TLit/www/output/",sliceSize,"/",theSource,"_netstat.csv",sep=''),col.names = F,row.names = F)
    
    nodes <- as.data.frame(nodes,stringsAsFactors=F)
    colnames(nodes) <- c('id','title','label')
    nodes$id <- as.numeric(nodes$id)
    
    links <- as.data.frame(links)
    colnames(links) <- c('from','to','title')
    links$from<-unlist(links$from)
    links$to<-unlist(links$to)
    links$title<-unlist(links$title)
    
    #new static cascade network
    socNodes <- data.frame(id=V(g)$name,label=V(g)$name)
    socEdges <- data.frame(from=head_of(g,E(g))$name,to=tail_of(g,E(g))$name)
    colnames(socNodes) <- c('id','label')
    colnames(socEdges) <- c('from','to')
    socNodes$shape <- "dot"  
    socNodes$shadow <- FALSE # Nodes will drop shadow
    socNodes$borderWidth <- 2 # Node border width
    socNodes$color.border <- "black"
    socNodes$color.highlight.background <- "orange"
    socNodes$color.highlight.border <- "darkred"
    
    socEdges$shadow <- FALSE
    socEdges$color <- "gray"
    socEdges$label <- E(g)$label
    
    ## old static network
    #netw <- visNetwork(socNodes, socEdges, width="1000px", height="1000px") %>%
    #  visNodes(shadow = T,font=list(size=32)) %>% 
    #  visEdges(color=list(color="grey"),font = list(color="grey",size=32)) %>% 
    #  visOptions(highlightNearest = TRUE) %>%
    #  visInteraction(dragNodes = FALSE, dragView = TRUE, zoomView = TRUE) %>% 
    #  visPhysics(stabilization = FALSE,   barnesHut = list(gravitationalConstant = -10000,springConstant = 0.002,springLength = 150))
    #visSave(netw, file = paste("/Users/mlr/Documents/git-projects/lit-cascades/src/TLit/www/output/",theSource,"_static-network-old.html",sep=''))
    library(networkDynamic)
    library(ndtv)
    library(tsna)
    
    hnetwork <- asNetwork(g)
    
    render.d3movie(hnetwork, filename=paste("TLit/www/output/",sliceSize,"/",theSource,"_static-network.html",sep=''),launchBrowser=F, 
                   displaylabels = T, label=hnetwork %v% "vertex.names",
                   vertex.col="orange",edge.col="darkgray",label.cex=0.5,
                   vertex.cex = .5, vertex.border="black",
                   vertex.tooltip = paste("<span style='font-size: 10px;'><b>Slice:</b>", (hnetwork %v% "vertex.names") , "<br />","<b>Matched information:</b>", (hnetwork %v% "content"), "<br /><a href='",paste("",theSource,"_textchunks.html#slice-",(hnetwork %v% "vertex.names"),sep=''),"' target='blank'>Go to content</a><br />"),
                   edge.lwd = .3,
                   object.scale = 0.1,
                   edge.tooltip = paste("<b>Link:</b>", (hnetwork %e% "label"),"</span>" ))
    
    #### create the character network from the cascade
    socN1 <- c()
    for(lin in 1:nrow(nodes)){
      nex <- unlist(strsplit(nodes$title[lin],", "))
      if(length(nex)>1) socN1 <- rbind(socN1,paste(nex,collapse=', '))
      
      if(lin%%(round(nrow(nodes)/10)) == 0){
        socEdges<-c()
        for(lin in 1:length(socN1)){
          
          socEdges<-rbind(socEdges,combinations(length(unlist(strsplit(socN1[lin],', '))),2,unlist(strsplit(socN1[lin],', '))))
        }
        
        h <- graph.data.frame(unique(socEdges),directed=FALSE)
        V(h)$frame.color <- "white"
        V(h)$color <- "orange"
        
        E(h)$width <- 0.1
        
        lay <- layout_with_dh(h)
        lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
        minC <- rep(-Inf, vcount(h))
        maxC <- rep(Inf, vcount(h))
        minC[1] <- maxC[1] <- 0
        co <- layout_with_fr(h, minx=minC, maxx=maxC,
                             miny=minC, maxy=maxC)
        pdf(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet_",lin,".pdf"))
        plot(h, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
        dev.off()
      }
    }
    socEdges<-c()
    for(lin in 1:length(socN1)){
      
      socEdges<-rbind(socEdges,combinations(length(unlist(strsplit(socN1[lin],', '))),2,unlist(strsplit(socN1[lin],', '))))
    }
    
    h <- graph.data.frame(unique(socEdges),directed=FALSE)
    
    write.csv(unique(socEdges),file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_socnet_edgelist.csv"), sep = ';')
    
    V(h)$frame.color <- "white"
    V(h)$color <- "orange"
    
    E(h)$width <- 0.1
    
    lay <- layout_with_dh(h)
    lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
    minC <- rep(-Inf, vcount(h))
    maxC <- rep(Inf, vcount(h))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(h, minx=minC, maxx=maxC,
                         miny=minC, maxy=maxC)
    
    socNodes <- data.frame(id=V(h)$name,label=V(h)$name)
    socEdges <- data.frame(from=head_of(h,E(h))$name,to=tail_of(h,E(h))$name)
    colnames(socNodes) <- c('id','label')
    colnames(socEdges) <- c('from','to')
    socNodes$shape <- "dot"  
    socNodes$shadow <- FALSE # Nodes will drop shadow
    socNodes$borderWidth <- 2 # Node border width
    socNodes$color.border <- "black"
    socNodes$color.highlight.background <- "orange"
    socNodes$color.highlight.border <- "darkred"
    
    socEdges$shadow <- FALSE
    socEdges$color <- "gray"
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet.pdf",sep=''))
    plot(h, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
    dev.off()
    
    #### create the character network from the cascade
    socN1 <- c()
    for(lin in 1:nrow(nodes)){
      nex <- unlist(strsplit(nodes$title[lin],", "))
      if(length(nex)>1) socN1 <- rbind(socN1,paste(nex,collapse=', '))
    }
    socEdges<-c()
    for(lin in 1:length(socN1)){
      socEdges<-rbind(socEdges,combinations(length(unlist(strsplit(socN1[lin],', '))),2,unlist(strsplit(socN1[lin],', '))))
    }
    
    socEdges<-as.data.frame(socEdges,stringsAsFactors=F)
    socEdges<-count(socEdges)
    
    colnames(socEdges) <- c("id1","id2","label")
    h <- graph.data.frame(socEdges,directed=FALSE)
    
    V(h)$frame.color <- "white"
    V(h)$color <- "orange"
    
    deg <- igraph::degree(h, mode="all")
    V(h)$size <- deg*3
    
    E(h)$width <- 0.1
    
    E(h)$weight <- E(h)$label
    minC <- rep(-Inf, vcount(h))
    maxC <- rep(Inf, vcount(h))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(h, minx=minC, maxx=maxC,
                         miny=minC, maxy=maxC)
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet2.pdf",sep=''))
    plot(h, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
    dev.off()
    
    netm <- as_adjacency_matrix(h, attr="weight", sparse=F)
    colnames(netm) <- V(h)$name
    rownames(netm) <- V(h)$name
    
    palf <- colorRampPalette(c("gold", "dark orange")) 
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet_heatmap.pdf",sep=''))
    heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), scale="none", margins=c(20,20) )
    dev.off()
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet_degdistri.pdf",sep=''))
    deg.dist <- degree_distribution(h, cumulative=T, mode="all")
    plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")
    dev.off()
    
    write.table(socEdges,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_socialnetwork_links.csv"))
    
    hnetwork <- asNetwork(h)
    
    socEdges2 <- socEdges
    socEdges2 <- socEdges2[c("id2", "id1","label")]
    names(socEdges2)[names(socEdges2) == "id2"] <- "haha"
    names(socEdges2)[names(socEdges2) == "id1"] <- "id2"
    names(socEdges2)[names(socEdges2) == "haha"] <- "id1"
    
    newsocEdges <- rbind(socEdges,socEdges2)
    
    allChars = unique(c(newsocEdges$id1,newsocEdges$id2))
    linkedChar <- c()
    for(char in 1:length(allChars)){
      linkedChar <- rbind(linkedChar,c(allChars[char],paste(unique(c(newsocEdges[which(newsocEdges$id1==allChars[char]),2],newsocEdges[which(newsocEdges$id2==allChars[char]),1])),collapse = ", ")))
    }
    linkedChar <- as.data.frame(linkedChar)
    colnames(linkedChar) <- c("char","links")
    
    #newsocEdges <- ddply(newsocEdges, .(id1),summarize, id2 = toString(id2))
    
    linkedChar2 <- c()
    
    for(char in 1:length(hnetwork$val)){
      linkedChar2 <- rbind(linkedChar2,linkedChar[which(linkedChar[,1]==hnetwork$val[[char]]$vertex.names),])
    }
    linkedChar2 <- as.data.frame(linkedChar2)
    colnames(linkedChar2) <- c("char","links")
    
    write.csv(linkedChar2$char,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_allcharacters.csv",sep=''))
    
    render.d3movie(hnetwork, filename=paste("TLit/www/output/",sliceSize,"/",theSource,"_social-network.html",sep=''),launchBrowser=F, 
                   displaylabels = T, label=hnetwork %v% "vertex.names",label.cex=.5,
                   vertex.col="orange",edge.col="darkgray",label.cex=0.5,
                   vertex.cex = .5, vertex.border="black",
                   vertex.tooltip = paste("<span style='font-size: 10px;'><b>Character:</b>", (hnetwork %v% "vertex.names"), "<br />","<b>Co-occurring characters:</b>", (linkedChar2$links)),
                   edge.lwd = .3,
                   object.scale = 0.1,
                   edge.tooltip = paste("<b>Number of Links:</b>", (hnetwork %e% "label"),"</span>" ))
    detach("package:ndtv", unload=TRUE)
    detach("package:tsna", unload=TRUE)
    detach("package:sna", unload=TRUE)
    detach("package:networkDynamic", unload=TRUE)
    #stats for the social graph
    degd <- degree.distribution(h)
    wtc <- cluster_walktrap(h)
    gstat <- c(diameter(h),min(degd),max(degd),mean(degd),edge_density(h),modularity(wtc))
    write.table(c(theSource,gstat),paste0("TLit/www/output/",sliceSize,"/",theSource,"_socnetstat_combined.csv"),append = T,col.names = F,row.names = F)
    write.csv2(gstat,paste("TLit/www/output/",sliceSize,"/",theSource,"_socnetstat.csv",sep=''),col.names = F,row.names = F)
    
    ####
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_links_source_nrow.jpg",sep=''))
    plot(links[,2],c(1:nrow(links)),pch=".")
    dev.off()
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_links_targets.jpg",sep=''))
    plot(count(unlist(links[,2]))$freq,type='l')
    dev.off()
    write.csv(cbind(unlist(links[,2]),links[,3]),file=paste("TLit/www/output/",sliceSize,"/",theSource,'_gutenberg_targets.txt',sep=''))
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_links_sources.jpg",sep=''))
    plot(count(unlist(links[,1]))$freq,type='l')
    dev.off()
    write.csv(cbind(unlist(links[,1]),links[,3]),file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_sources.txt",sep=''))
    
    agg <- as.numeric(links[,2]) - as.numeric(links[,1])
    plot(c(1:length(agg)),agg,pch='.')
    hist(agg)
    
    casc <- c()
    inter <- c()
    ent <- data.frame(ww = numeric(0), xx = numeric(0), yy = numeric(0), zz = numeric(0))
    wien <- c()
    colnames(links) <- c('source', 'target', 'tag')
    
    coordinates <- c()
    spec <- list()
    div = 1
    
    g1 <- make_empty_graph(n = 0, directed = TRUE)
    struct <- c()
    props <- c()
    for(z in 1:nrow(nodes)){
      if(nodes[z,]$title == ""){
        if(nrow(ent) > 0){
          ent <- rbind(ent, ent[nrow(ent),])
          wien <- rbind(wien, wien[nrow(wien),])
          
        }else{
          ent <- rbind(ent, c(0, 1, 0, 1))
          wien <- rbind(wien, c(0, 1, 1))
        }
        coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 0, 0))
        
      }else{
        inter <- rbind(inter, paste(sort(unlist(strsplit(nodes[z,2],', '))), collapse = ', '))
        nextI <- digest(paste(sort(unlist(strsplit(nodes[z,2], ', '))), collapse = ', '), algo = "md5")
        if(length(spec) == 0){
          coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 1, 1))
          spec[[nextI]] <- c(1, 1)
        }
        else{
          if(is.null(spec[[nextI]])){
            spec[[nextI]] <- c(1,div)
            coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],div))
            div <- div+1
          }else{
            spec[[nextI]] <- c(spec[[nextI]][1]+1,spec[[nextI]][2])
            coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],spec[[nextI]][2]))
          }
        }
        
        interact <- list()
        cooccure <- list()
        #temp1 <- c()
        for(v in 1:nrow(inter)){
          interactions <- unlist(strsplit(unlist(inter[v,1]),', '))
          #temp1 <- rbind(temp1, gtools::combinations(length(interactions), 2, interactions))
          for( m in 1:length(interactions)){
            if(is.null(interact[[interactions[m]]])) interact[[interactions[m]]] <- 1
            else interact[[interactions[m]]] <- interact[[interactions[m]]] + 1
          }
        }
        df <- data.frame(unlist(interact))
        tmp <- df[,1] / colSums(df)
        df$loga <- log(tmp)
        df$piloga <- tmp * log(tmp)
        if(is.nan((-1 * (colSums(df)[3])) / log(nrow(df)))){
          ent <- rbind(ent,c(entropy.empirical(df[,1], unit = "log2"), 1, -1 * (colSums(df)[3]), 1))
        } else{
          ent <- rbind(ent, c(entropy.empirical(df[,1], unit = "log2"), (-1 * (colSums(df)[3])) / log2(nrow(df)),-1*(colSums(df)[3]),(-1*(colSums(df)[3]))/log(nrow(df))))
        }
        
        if(nrow(df) == 1){
          wien <- rbind(wien, c(0, 1, 1))
        } else{
          H <- vegan::diversity(df[,1])
          S <- nrow(df)
          J <- H/log(S)
          wien <- rbind(wien,c(H, J, S))
        }}
      
      #}
      colnames(ent)<-c('empEntropy', 'evenness_log2', 'entropy', 'evenness')
      colnames(wien)<-c('ShannonWiener', 'Pielou', 'Richness')
      
      #add node
      g1 <- add_vertices(g1,1,attr = list(id = as.numeric(nodes[z,1])))
      
      #add all links to node
      theLinks <- unique(links[which(links[,2] == nodes[z,1]),1:2])
      for(srclnk in theLinks[,1]){
        g1 <- add_edges(g1, c(which(V(g1)$id == srclnk), which(V(g1)$id == as.numeric(nodes[z,1]))))
      }
      
      #degd <- degree.distribution(g1)
      wtc <- cluster_walktrap(g1)
      struct <- rbind(struct, c(diameter(g1), edge_density(g1), modularity(wtc)))
    }
    #colnames(coordinates) <- c("t","specificity","diversity")
    #data.frame(coordinates) %>%
    #ggplot() +
    #aes(x = specificity, y = diversity, colour = t) +
    #geom_jitter()
    
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_coordinates.jpg",sep=''))
    scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
    dev.off()
    
    write.table(cbind(coordinates, wien, struct),
                file=paste0("TLit/www/output/", sliceSize, "/", theSource,
                            "_temporal_statistics.csv"), row.names = F, col.names = F, sep = ";")
    
    write.table(ent, file = paste0("TLit/www/output/", sliceSize, "/",
                                   theSource, "_gutenberg_entropy.txt"), sep = ";")
    
    write.table(wien, file = paste0("TLit/www/output/", sliceSize, "/",
                                    theSource, "_gutenberg_diversity.txt"), sep = ";")
    ent_plot <- as.data.frame(ent) %>%
      mutate(rownumber = seq.int(nrow(.)))
    
    wien_plot <- as.data.frame(wien) %>%
      mutate(rownumber = seq.int(nrow(.)))
    
    ggsave(paste0("TLit/www/output/", sliceSize, "/", theSource,"_gutenberg_entropy.jpg"),
           plot = ent_plot %>%
             ggplot() +
             aes(x = rownumber, y = empEntropy, group = 1) +
             geom_line() +
             labs(y = "Entropy") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/", sliceSize, "/", theSource,"_gutenberg_evenness.jpg"),
           plot = ent_plot %>%
             ggplot() +
             aes(x = rownumber, y = evenness_log2, group = 1) +
             geom_line() +
             labs(y = "Log Evenness") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/", sliceSize, "/",
                  theSource, "_gutenberg_shannonwiener.jpg"),
           plot = wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = ShannonWiener, group = 1) +
             geom_line() +
             labs(y = "Shannon Wiener") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_pielou.jpg"),
           plot = wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = Pielou, group = 1) +
             geom_line() +
             labs(y = "Pielou") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_richness.jpg"),
           plot = wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = Richness, group = 1) +
             geom_line() +
             labs(y = "Richness") +
             theme_classic())
    
    # scatterplot for character frequency
    labelint = as.integer(nodes$label)
    tempDat <- data.frame(nodes$title, labelint, stringsAsFactors = F)
    tmpSplit <- strsplit(tempDat$nodes.title, split=", ")
    newDat <- data.frame(x = (labelint = rep(tempDat$labelint, sapply(tmpSplit, length))), y = (nodes.title = unlist(tmpSplit)))
    names(newDat)[names(newDat)=="x"] <- "Node"
    names(newDat)[names(newDat)=="y"] <- "Character"
    numberchar <- aggregate(Node ~ Character, data = newDat, length)
    newDat <- merge(newDat,numberchar,by="Character")
    names(newDat)[names(newDat)=="Node.x"] <- "Node"
    names(newDat)[names(newDat)=="Node.y"] <- "Appearances"
    ggplot(newDat, aes(x=Node, y=Character)) + 
      geom_point(size=1, shape = 23) + 
      scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50))
    ggsave(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_character_frequency.pdf",sep=''),scale = 1:2)
    write.csv(newDat,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_character_frequency_csv.txt", sep = ''))
    
    # scatterplot for first and last character appearance
    firstNode <- aggregate(Node ~ Character,newDat,min)
    lastNode <- aggregate(Node ~ Character,newDat,max)
    combined <- rbind(firstNode,lastNode)
    combined$Type <- c(rep("First Appearance",nrow(firstNode)),rep("Last Appearance",nrow(lastNode)))
    combined <- merge(combined,numberchar, by='Character')
    names(combined)[names(combined)=="Node.x"] <- "Node"
    names(combined)[names(combined)=="Node.y"] <- "Appearances"
    ggplot(combined, aes(x = Node, y = Character, group=Type, col=Type)) + 
      geom_point(size=1.5, shape = 23) +
      scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50))
    ggsave(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_first_last_character_appearance.pdf",sep=''),scale = 1:2)
    write.csv(combined,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_first_last_character_appearance_csv.txt", sep = ''))
    
    # .txt list of character appearance
    charfreqcsv <- ddply(newDat, .(Character), summarize, Node = toString(Node))
    write.csv(charfreqcsv[,c("Character","Node")],file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_listofcharacterfreq.txt",sep=''), row.names = F)
    
    #.txt list of number of appearances for each character
    write.csv(numberchar,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_totalnumberofcharacterfreq.txt",sep=''), row.names = F)
    
    # new entropy graph
    entgraph <- data.frame(ent)
    entgraph$indx <- as.integer(row.names(entgraph))
    names(entgraph)[names(entgraph)=="indx"] <- "Node"
    names(entgraph)[names(entgraph)=="empEntropy"] <- "Entropy"
    ggplot(entgraph, aes(x = Node, y = Entropy)) +
      geom_point(size=.2, shape = 23) + geom_line(linetype = 2, size=.2) +
      scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50)) +
      scale_y_continuous (limits = c(0,5)) 
    ggsave(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_entropy_new.pdf",sep=''))
    write.csv(entgraph,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_entropy_csv.txt", sep = ''))
  }  
  write.csv2(allOutput,paste0('TLit/www/output/',sliceSize,'/allTexts.csv'))
}



#########################
#########################
#########################
library(plyr)
library(tm)
library(stringr)
library(RTextTools)
library(NLP)
library(visNetwork)
library(digest)
library(entropy)
library(scatterplot3d)
library(RColorBrewer)
library(tidyr)
library(igraph)
library(ggplot2)
library(plotly)
library(gtools)
library(networkDynamic)
library(ndtv)
library(tsna)
library(intergraph)
library(openNLP)
library(cldr)
library(RWeka)

#split only by chapter
#get all text and char files
allCharFiles <- list.files("../resources/Character Lists/")
allCharFiles <- allCharFiles[which(allCharFiles!="Icon\r")]
allTextFiles <- list.files("../resources/Text Files")
allTextFiles <- allTextFiles[which(allTextFiles!="Icon\r")]
#select which texts to process
#litSources <- c('greatexpectations','davidcopperfield','chuzzlewit','bleakhouse','middlemarch','ourmutualfriend','phineasfinn','pickwick','smallhouse','tessofthedurbervilles')
#litSources <- c('bleakhouse','ourmutualfriend')
allOutput <- c()

method <- "chars" # one of "chars" "ngram" "nouns"

slice_sizes <- c("chapter")

for(sliceSize in slice_sizes){
  dir.create(file.path("TLit/www/output/", sliceSize), showWarnings = FALSE)
  
  for(nextRun in 1:length(allTextFiles)){
    theSource <- gsub(' ','_',gsub('[[:digit:]][[:digit:]] ','',gsub(' text.txt','',allTextFiles[nextRun])))
    allOutput <- c(allOutput,theSource)
    sourceText <- readChar(paste0('../resources/Text Files/',allTextFiles[nextRun]), file.info(paste0('../resources/Text Files/',allTextFiles[nextRun]))$size)
    #processedText <- gsub("\\r\\n", " ", sourceText,perl=T)
    #processedText <- gsub("\\n", " ", processedText,perl=T)
    processedText <- sourceText
    
    #split by number of words
    #full_text <- strsplit(processedText, "\\s+")[[1]]
    #groupA <- rep(seq(ceiling(length(full_text)/1200)), each=1200)[1:length(full_text)]
    #words300A <- split(full_text, groupA)
    #words300B <- words300A
    
    #split by number of words and chapters
    #full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\s.]?)", perl=T)[[1]]
    
    #full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\.]?[\\s.]?)", perl=T)[[1]]
    full_text <- strsplit(processedText, "(?i:[\\n]Chapter [0-9A-Z]+[\\.]?[\\s.]?)", perl=T)[[1]]
    words300B <- c()
    
    for(s in 1:length(full_text)){
      tmp <- strsplit(full_text[s], "\\s+")[[1]]
      if(length(tmp) > 0){
        words300B <- c(words300B,list(tmp))
      }
    }
    
    words300A <- words300B
    if(method == "chars"){
      #character list
      tmp <- readLines(paste0('../resources/Character Lists/',allCharFiles[nextRun]))
      
      #for short character lists
      #tmp <- readLines(paste('../resources/',theSource,'_chars_short.txt',sep=''))
      
      charIds <- sapply(strsplit(tmp,": "),function(x){ x[[1]] })
      chars <- sapply(strsplit(tmp,": "),function(x){ strsplit(x[[2]],", ") })
      tmp <- c()
      for(i in 1:length(chars)){
        nextChar <- charIds[i]
        allNames <- chars[[i]]
        for(j in 1:length(allNames)){
          tmp <- rbind(tmp,c(nextChar,allNames[j]))
        }
      }
      chars <- data.frame(tmp,stringsAsFactors = F)
      colnames(chars)<-c('id','name')
      chars$len <- nchar(chars$name)
      chars$len<-as.numeric(chars$len)
      chars <- chars[ order(-chars[,3]), ]
      
      matches <- list()
      for(j in 1:length(words300A)){
        slicematch <- list()
        for(k in 1:nrow(chars)){
          needle <- chars[k,2]
          matched <- unlist(gregexpr(paste("[\\s\\\\\"](",needle,")[\\';,.:\\s\\\\\"]",sep=""), paste(unlist(words300A[j]),collapse=' '),perl=TRUE))
          
          multiple <- F
          if(k+1<=nrow(chars)){
            if(length(which(chars[k+1:nrow(chars),2]==needle))>0) multiple <- T
          }
          if(!multiple){
            nWords <- sapply(gregexpr("\\W+", needle), length) +1
            words300A[j] <- strsplit(gsub(needle,paste(replicate(nWords, "FOOBAR"), collapse = " "),paste(unlist(words300A[j]),collapse=' '))," ")
          }
          
          if(matched != -1){
            for(l in 1:length(matched)){
              if(is.null(slicematch[[as.character(matched[l])]])){
                slicematch[[as.character(matched[l])]] <- needle
              } else{
                if(length(slicematch[[as.character(matched[l])]]) < length(needle)){
                  slicematch[[as.character(matched[l])]] <- needle
                }
              }
            }
          }
        }
        matches[[j]]<-slicematch
      }
      
      charDS <- data.frame(x = numeric(), y = character(), stringsAsFactors = FALSE)
      
      for(i in 1:length(matches)){
        if(length(matches[i][[1]])>0){
          nextMatch <- unique(unlist(matches[i]))
          nextMatchStr <- c()
          for(j in 1:length(nextMatch)){
            if(length(nextMatchStr) == 0) {nextMatchStr <- c(chars[which(chars[,2]==nextMatch[j]),1])}
            else {nextMatchStr <- c(nextMatchStr,chars[which(chars[,2]==nextMatch[j]),1])}
          }
          charDS <- rbind(charDS,data.frame(i=i,identifiers=paste(sort(unique(nextMatchStr)),collapse=', ')),stringsAsFactors=F)
        } else{
          charDS <- rbind(charDS,data.frame(i=i,identifiers=""),stringsAsFactors=F)
        }
      }
    } else if(method == "nouns"){
      ## Need sentence and word token annotations.
      sent_token_annotator <- Maxent_Sent_Token_Annotator()
      word_token_annotator <- Maxent_Word_Token_Annotator()
      pos_tag_annotator <- Maxent_POS_Tag_Annotator()
      
      charDS <- data.frame(x = numeric(), y = character(), stringsAsFactors = FALSE)
      for(i in 1:length(words300A)){
        s<-as.String(paste(unlist(words300A[i]),collapse=' '))
        
        a2 <- NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
        a3 <- NLP::annotate(s, pos_tag_annotator, a2)
        a3w <- subset(a3, type == "word")
        tags <- sapply(a3w$features, `[[`, "POS")
        nNouns<-length(which(tags=='NN' | tags=='NNS'))
        nTags <- length(tags)
        nounsOnly <-s[a3w][which(tags=='NN' | tags=='NNS')]
        matrix <- create_matrix(nounsOnly, stemWords=TRUE, removeStopwords=TRUE, minWordLength=3)
        charDS <- rbind(charDS,data.frame(i,paste(unique(colnames(matrix)),collapse=', ')),stringsAsFactors=F)
      }
    } else if(method == "ngram"){
      nGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
      
      charDS <- data.frame(x = numeric(), y = character(), stringsAsFactors = FALSE)
      for(i in 1:length(words300A)){
        a <- Corpus(VectorSource(c(as.String(paste(unlist(words300A[i]),collapse=' ')))))
        a <- tm_map(a, removeNumbers)
        a <- tm_map(a, removePunctuation)
        a <- tm_map(a , stripWhitespace)
        a <- tm_map(a, tolower)
        a <- tm_map(a, removeWords, stopwords("english")) 
        a <- tm_map(a, stemDocument, language = "english")
        a <- tm_map(a, PlainTextDocument)
        tdm <- TermDocumentMatrix(a, control = list(tokenize = nGramTokenizer))
        #tdm <- removeSparseTerms(tdm, 0.75)
        
        charDS <- rbind(charDS,data.frame(i,paste(unique(tdm$dimnames$Terms),collapse=', ')),stringsAsFactors=F)
      }
    }
    
    tic <- tic_generate(charDS)
    
    ## Extracting nodes, links, roots from network model computed in tic_generate.
    nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tags = unlist(tic[[1]][,2]),
                        dpub = unlist(tic[[1]][,3]), stringsAsFactors = F)
    links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                        tag = unlist(tic[[2]][,3]), stringsAsFactors = F)
    roots <- data.frame(root_node_id = unlist(tic[[3]][,1]),
                        tag = unlist(tic[[3]][,2]), stringsAsFactors = F)
    
    write.table(nodes,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_nodes.csv"))
    write.table(links,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_links.csv"))
    write.table(roots,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_roots.csv"))
    
    linksDelta <- as.integer(links$target)-as.integer(links$source)
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta.jpg"))
    plot(linksDelta,type='l')
    abline(h=mean(linksDelta),col="red")
    abline(h=median(linksDelta),col="blue")
    dev.off()
    
    linksDelta.count <- count(linksDelta)
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri.jpg"))
    plot(linksDelta.count$x,linksDelta.count$freq,pch=20)
    dev.off()
    
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_loglog.jpg"))
    plot(linksDelta.count$x,linksDelta.count$freq,pch=20,log="xy")
    dev.off()
    
    #power law?
    
    m_bl = displ$new(linksDelta)
    est = estimate_xmin(m_bl)
    m_bl$setXmin(est)
    m_ln = dislnorm$new(linksDelta)
    est = estimate_xmin(m_ln)
    m_ln$setXmin(est)
    m_pois = dispois$new(linksDelta)
    est = estimate_xmin(m_pois)
    m_pois$setXmin(est)
    
    jpeg(paste0("TLit/www/output/",sliceSize,"/",theSource,"_links_delta_distri_plaw.jpg"))
    plot(m_bl, ylab="CDF")
    text(100,0.15,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
    lines(m_bl, col=2)
    lines(m_ln, col=3)
    lines(m_pois, col=4)
    dev.off()
    
    
    #export text as HTML file to allow jumoing to respective slice
    htmlHead <- "<htlm><head><style type='text/css'>body{font-family: Verdana, sans-serif;}</style><script>function foo(){var hash = window.location.hash.substring(1); document.getElementById(hash).style.border = '2px solid red'; document.getElementById(hash).style.padding = '15px'; window.onfocus=function(event){location.reload();}}</script></head><body onload='foo()'>"
    htmlTail <- "</body>"
    
    htmlContent <- ""
    
    for(h in 1:length(words300B)){
      targets <- unique(unlist(links[which(links[,1]==h),2]))
      sources <- unique(unlist(links[which(links[,2]==h),1]))
      
      sLinks <- ""
      if(length(sources)>0){
        for(aLink in 1:length(sources)){
          sLinks <- paste0(sLinks," - <a href='../",theSource,"_textchunks.html#slice-",sources[aLink],"' style='text-decoration: none; color: #ccc;'>Slice ",sources[aLink],"</a>")
        }
      }
      
      tLinks <- ""
      if(length(targets)>0){
        for(aLink in 1:length(targets)){
          tLinks <- paste0(tLinks," - <a href='../",theSource,"_textchunks.html#slice-",targets[aLink],"' style='text-decoration: none; color: #ccc;'>Slice ",targets[aLink],"</a>")
        }
      }
      htmlContent <- paste(htmlContent,"<p style='font-size: 0.8em; color:#ccc;'>Slice ",h," is linked from: ",sLinks,"</p><p id='slice-",h,"'><a name='slice-",h,"'>",paste(unlist(words300B[h]),collapse=' '),"</a></p><p style='font-size: 0.8em; color:#ccc;text-decoration: none;'>links to: ",tLinks,"</p><hr>",sep='')
    }
    write(paste(htmlHead,htmlContent,htmlTail,sep=''),file=paste('TLit/www/output/',sliceSize,'/',theSource,'_textchunks.html',sep=''))
    
    library(networkDynamic)
    library(ndtv)
    library(tsna)
    
    nd <- as.networkDynamic(network.initialize(0))
    set.network.attribute(nd,"vertex.pid","vertex.names")
    set.network.attribute(nd,"edge.pid","edge.names")
    for(i in 1:nrow(links)){
      fromN <- get.vertex.id(nd,unlist(links[i,1]))
      if(is.na(fromN)){
        add.vertices(nd,nv=1,vertex.pid=c(unlist(links[i,1])))
        fromN <- get.vertex.id(nd,unlist(links[i,1]))
        set.vertex.attribute(nd,'content',unlist(nodes[which(nodes[,1]==unlist(links[i,1])),2]),v=c(fromN))
        set.vertex.attribute(nd,'content2',paste(unlist(words300B[unlist(nodes[which(nodes[,1]==unlist(links[i,1])),1])]),collapse=' '),v=c(fromN))
        set.vertex.attribute(nd,'step',unlist(nodes[which(nodes[,1]==unlist(links[i,1])),1]),v=c(fromN))
        activate.vertices(nd,onset=as.numeric(unlist(links[i,2])),terminus=max(as.numeric(unlist(links[,2])))+1,v=c(fromN))
      }
      
      toN <- get.vertex.id(nd,unlist(links[i,2]))
      if(is.na(toN)){
        add.vertices(nd,nv=1,vertex.pid=c(unlist(links[i,2])))
        toN <- get.vertex.id(nd,unlist(links[i,2]))
        set.vertex.attribute(nd,'content',unlist(nodes[which(nodes[,1]==unlist(links[i,2])),2]),v=c(toN))
        set.vertex.attribute(nd,'content2',paste(unlist(words300B[unlist(nodes[which(nodes[,1]==unlist(links[i,2])),1])]),collapse=' '),v=c(toN))
        set.vertex.attribute(nd,'step',unlist(nodes[which(nodes[,1]==unlist(links[i,2])),1]),v=c(toN))
        activate.vertices(nd,onset=as.numeric(unlist(links[i,2])),terminus=max(as.numeric(unlist(links[,2])))+1,v=c(toN))
      }
      edgeID <- which(get.edge.attribute(nd,'ident')==paste(unlist(links[i,1]),unlist(links[i,2]),sep='-'))
      if(length(edgeID)==0){
        add.edges.active(nd,onset=as.numeric(unlist(links[i,2])), terminus=max(as.numeric(unlist(links[,2])))+1,head=toN,tail=fromN,names.eval=list(list('set','ident','width')),vals.eval=list(list(links[i,3][[1]],paste(unlist(links[i,1]),unlist(links[i,2]),sep='-'),1)))
      } else{
        linkLabel <- paste(get.edge.attribute(nd,'set',unlist=FALSE)[[edgeID]],unlist(links[i,3]),sep=", ")
        set.edge.attribute(nd, attrname='set', value=linkLabel, e=c(edgeID))
        set.edge.attribute(nd, attrname='width', value=get.edge.attribute(nd,'width',unlist=FALSE)[[edgeID]]+1, e=c(edgeID))
      }
    }
    
    compute.animation(nd, animation.mode = "kamadakawai", chain.direction=c('forward'),default.dist=10)
    
    #interactive
    render.d3movie(nd, filename=paste("TLit/www/output/",sliceSize,"/",theSource,"_dynamic-network.html",sep=''),launchBrowser=F, 
                   displaylabels = T, label=nd %v% "vertex.names", label.cex=.5,
                   vertex.col="orange",edge.col="darkgray",
                   vertex.cex = .5, vertex.border="black",
                   vertex.tooltip = paste("<span style='font-size: 10px;'><b>Slice:</b>", (nd %v% "step") , "<br />","<b>Matched information:</b>", (nd %v% "content"), "<br /><a href='",paste("",theSource,"_textchunks.html#slice-",(nd %v% "step"),sep=''),"' target='blank'>Go to content</a><br />"),
                   edge.lwd = .3,
                   object.scale = 0.1,
                   edge.tooltip = paste("<b>Link:</b>", (nd %e% "set"),"</span>" ))
    
    detach("package:ndtv", unload=TRUE)
    detach("package:tsna", unload=TRUE)
    detach("package:sna", unload=TRUE)
    detach("package:networkDynamic", unload=TRUE)
    
    uniqueLinks <- data.frame(id1=character(0),id2=character(0),label=character(0))
    convLinks <- as.data.frame(links,stringsAsFactors = F)
    for(z in 1:nrow(convLinks)){
      uniqueLinks <- rbind(uniqueLinks,data.frame(id1=unlist(convLinks[z,1]),id2=unlist(convLinks[z,2]),label=paste(convLinks[which(unlist(convLinks[,1])==unlist(convLinks[z,1]) & unlist(convLinks[,2])==unlist(convLinks[z,2])),3],collapse=', '),stringsAsFactors = F))
    }
    uniqueLinks <- unique(uniqueLinks)
    colnames(uniqueLinks) <- c("id1","id2","label")
    colnames(links) <- c("id1","id2","label")
    g <- graph.data.frame(uniqueLinks,directed=TRUE)
    
    nLabels <- c()
    for(z in V(g)$name){
      nLabels <- c(nLabels,paste(unique(unlist(strsplit(paste(uniqueLinks[which(uniqueLinks$id1==z | uniqueLinks$id2==z),3],collapse = ', '),', '))),collapse = ', '))
    }
    
    V(g)$content <- nLabels
    
    V(g)$frame.color <- "white"
    V(g)$color <- "orange"
    
    deg <-  igraph::degree(g, mode="all")
    V(g)$size <- deg*3
    
    E(g)$width <- 0.1
    
    lay <- layout_with_dh(g)
    lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
    minC <- rep(-Inf, vcount(g))
    maxC <- rep(Inf, vcount(g))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(g, minx=minC, maxx=maxC,
                         miny=minC, maxy=maxC)
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_net.pdf",sep=''))
    plot(g, layout=co*1.0, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
    dev.off()
    
    deg <-  igraph::degree(g, mode="all")
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_net_degdistri.pdf",sep=''))
    deg.dist <- degree_distribution(g, cumulative=T, mode="all")
    plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")
    dev.off()
    
    degd <- degree.distribution(g)
    wtc <- cluster_walktrap(g)
    gstat <- c(diameter(g),min(degd),max(degd),mean(degd),edge_density(g),modularity(wtc))
    write.csv2(gstat,paste("TLit/www/output/",sliceSize,"/",theSource,"_netstat.csv",sep=''),col.names = F,row.names = F)
    
    nodes <- as.data.frame(nodes,stringsAsFactors=F)
    colnames(nodes) <- c('id','title','label')
    nodes$id <- as.numeric(nodes$id)
    
    links <- as.data.frame(links)
    colnames(links) <- c('from','to','title')
    links$from<-unlist(links$from)
    links$to<-unlist(links$to)
    links$title<-unlist(links$title)
    
    #new static cascade network
    socNodes <- data.frame(id=V(g)$name,label=V(g)$name)
    socEdges <- data.frame(from=head_of(g,E(g))$name,to=tail_of(g,E(g))$name)
    colnames(socNodes) <- c('id','label')
    colnames(socEdges) <- c('from','to')
    socNodes$shape <- "dot"  
    socNodes$shadow <- FALSE # Nodes will drop shadow
    socNodes$borderWidth <- 2 # Node border width
    socNodes$color.border <- "black"
    socNodes$color.highlight.background <- "orange"
    socNodes$color.highlight.border <- "darkred"
    
    socEdges$shadow <- FALSE
    socEdges$color <- "gray"
    socEdges$label <- E(g)$label
    
    ## old static network
    #netw <- visNetwork(socNodes, socEdges, width="1000px", height="1000px") %>%
    #  visNodes(shadow = T,font=list(size=32)) %>% 
    #  visEdges(color=list(color="grey"),font = list(color="grey",size=32)) %>% 
    #  visOptions(highlightNearest = TRUE) %>%
    #  visInteraction(dragNodes = FALSE, dragView = TRUE, zoomView = TRUE) %>% 
    #  visPhysics(stabilization = FALSE,   barnesHut = list(gravitationalConstant = -10000,springConstant = 0.002,springLength = 150))
    #visSave(netw, file = paste("/Users/mlr/Documents/git-projects/lit-cascades/src/TLit/www/output/",theSource,"_static-network-old.html",sep=''))
    library(networkDynamic)
    library(ndtv)
    library(tsna)
    
    hnetwork <- asNetwork(g)
    
    render.d3movie(hnetwork, filename=paste("TLit/www/output/",sliceSize,"/",theSource,"_static-network.html",sep=''),launchBrowser=F, 
                   displaylabels = T, label=hnetwork %v% "vertex.names",
                   vertex.col="orange",edge.col="darkgray",label.cex=0.5,
                   vertex.cex = .5, vertex.border="black",
                   vertex.tooltip = paste("<span style='font-size: 10px;'><b>Slice:</b>", (hnetwork %v% "vertex.names") , "<br />","<b>Matched information:</b>", (hnetwork %v% "content"), "<br /><a href='",paste("",theSource,"_textchunks.html#slice-",(hnetwork %v% "vertex.names"),sep=''),"' target='blank'>Go to content</a><br />"),
                   edge.lwd = .3,
                   object.scale = 0.1,
                   edge.tooltip = paste("<b>Link:</b>", (hnetwork %e% "label"),"</span>" ))
    
    #### create the character network from the cascade
    socN1 <- c()
    for(lin in 1:nrow(nodes)){
      nex <- unlist(strsplit(nodes$title[lin],", "))
      if(length(nex)>1) socN1 <- rbind(socN1,paste(nex,collapse=', '))
      
      if(lin%%(round(nrow(nodes)/10)) == 0){
        socEdges<-c()
        for(lin in 1:length(socN1)){
          
          socEdges<-rbind(socEdges,combinations(length(unlist(strsplit(socN1[lin],', '))),2,unlist(strsplit(socN1[lin],', '))))
        }
        
        h <- graph.data.frame(unique(socEdges),directed=FALSE)
        V(h)$frame.color <- "white"
        V(h)$color <- "orange"
        
        E(h)$width <- 0.1
        
        lay <- layout_with_dh(h)
        lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
        minC <- rep(-Inf, vcount(h))
        maxC <- rep(Inf, vcount(h))
        minC[1] <- maxC[1] <- 0
        co <- layout_with_fr(h, minx=minC, maxx=maxC,
                             miny=minC, maxy=maxC)
        pdf(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet_",lin,".pdf"))
        plot(h, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
        dev.off()
      }
    }
    socEdges<-c()
    for(lin in 1:length(socN1)){
      
      socEdges<-rbind(socEdges,combinations(length(unlist(strsplit(socN1[lin],', '))),2,unlist(strsplit(socN1[lin],', '))))
    }
    
    h <- graph.data.frame(unique(socEdges),directed=FALSE)
    
    write.csv(unique(socEdges),file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_socnet_edgelist.csv"), sep = ';')
    
    V(h)$frame.color <- "white"
    V(h)$color <- "orange"
    
    E(h)$width <- 0.1
    
    lay <- layout_with_dh(h)
    lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
    minC <- rep(-Inf, vcount(h))
    maxC <- rep(Inf, vcount(h))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(h, minx=minC, maxx=maxC,
                         miny=minC, maxy=maxC)
    
    ## old Social Network visIgraph
    #visIgraph(h,smooth=F) %>% visNodes(borderWidth = 3, shadow = F,font=list(size=18),color = list(background = "blue", border = "black",highlight = "yellow",hover="yellow")) %>% 
    #  visEdges(color=list(color="grey"),font = list(color="grey")) %>% 
    #  visOptions(highlightNearest=T,height = "800px",width="1000px") %>% 
    #visIgraphLayout(physics=FALSE, smooth=TRUE) %>% 
    #  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>% 
    #  visPhysics(stabilization = FALSE,   barnesHut = list(gravitationalConstant = -10000,springConstant = 0.002,springLength = 250,avoidOverlap=0.5)) %>% 
    #  visSave(file=paste("/Users/mlr/Documents/git-projects/lit-cascades/src/TLit/www/output/",theSource,"_social-network-old.html",sep=''),selfcontained=TRUE)
    socNodes <- data.frame(id=V(h)$name,label=V(h)$name)
    socEdges <- data.frame(from=head_of(h,E(h))$name,to=tail_of(h,E(h))$name)
    colnames(socNodes) <- c('id','label')
    colnames(socEdges) <- c('from','to')
    socNodes$shape <- "dot"  
    socNodes$shadow <- FALSE # Nodes will drop shadow
    socNodes$borderWidth <- 2 # Node border width
    socNodes$color.border <- "black"
    socNodes$color.highlight.background <- "orange"
    socNodes$color.highlight.border <- "darkred"
    
    socEdges$shadow <- FALSE
    socEdges$color <- "gray"
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet.pdf",sep=''))
    plot(h, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
    dev.off()
    
    #### create the character network from the cascade
    socN1 <- c()
    for(lin in 1:nrow(nodes)){
      nex <- unlist(strsplit(nodes$title[lin],", "))
      if(length(nex)>1) socN1 <- rbind(socN1,paste(nex,collapse=', '))
    }
    socEdges<-c()
    for(lin in 1:length(socN1)){
      socEdges<-rbind(socEdges,combinations(length(unlist(strsplit(socN1[lin],', '))),2,unlist(strsplit(socN1[lin],', '))))
    }
    
    socEdges<-as.data.frame(socEdges,stringsAsFactors=F)
    socEdges<-count(socEdges)
    
    colnames(socEdges) <- c("id1","id2","label")
    h <- graph.data.frame(socEdges,directed=FALSE)
    
    V(h)$frame.color <- "white"
    V(h)$color <- "orange"
    
    deg <- igraph::degree(h, mode="all")
    V(h)$size <- deg*3
    
    E(h)$width <- 0.1
    
    E(h)$weight <- E(h)$label
    
    lay <- layout_with_dh(h)
    lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
    minC <- rep(-Inf, vcount(h))
    maxC <- rep(Inf, vcount(h))
    minC[1] <- maxC[1] <- 0
    co <- layout_with_fr(h, minx=minC, maxx=maxC,
                         miny=minC, maxy=maxC)
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet2.pdf",sep=''))
    plot(h, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
    dev.off()
    
    netm <- as_adjacency_matrix(h, attr="weight", sparse=F)
    colnames(netm) <- V(h)$name
    rownames(netm) <- V(h)$name
    
    palf <- colorRampPalette(c("gold", "dark orange")) 
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet_heatmap.pdf",sep=''))
    heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), scale="none", margins=c(20,20) )
    dev.off()
    
    pdf(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_dh_socnet_degdistri.pdf",sep=''))
    deg.dist <- degree_distribution(h, cumulative=T, mode="all")
    plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")
    dev.off()
    
    write.table(socEdges,file=paste0("TLit/www/output/",sliceSize,"/",theSource,"_socialnetwork_links.csv"))
    
    hnetwork <- asNetwork(h)
    
    socEdges2 <- socEdges
    socEdges2 <- socEdges2[c("id2", "id1","label")]
    names(socEdges2)[names(socEdges2) == "id2"] <- "haha"
    names(socEdges2)[names(socEdges2) == "id1"] <- "id2"
    names(socEdges2)[names(socEdges2) == "haha"] <- "id1"
    
    newsocEdges <- rbind(socEdges,socEdges2)
    
    allChars = unique(c(newsocEdges$id1,newsocEdges$id2))
    linkedChar <- c()
    for(char in 1:length(allChars)){
      linkedChar <- rbind(linkedChar,c(allChars[char],paste(unique(c(newsocEdges[which(newsocEdges$id1==allChars[char]),2],newsocEdges[which(newsocEdges$id2==allChars[char]),1])),collapse = ", ")))
    }
    linkedChar <- as.data.frame(linkedChar)
    colnames(linkedChar) <- c("char","links")
    
    #newsocEdges <- ddply(newsocEdges, .(id1),summarize, id2 = toString(id2))
    
    linkedChar2 <- c()
    
    for(char in 1:length(hnetwork$val)){
      linkedChar2 <- rbind(linkedChar2,linkedChar[which(linkedChar[,1]==hnetwork$val[[char]]$vertex.names),])
    }
    linkedChar2 <- as.data.frame(linkedChar2)
    colnames(linkedChar2) <- c("char","links")
    
    write.csv(linkedChar2$char,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_allcharacters.csv",sep=''))
    
    render.d3movie(hnetwork, filename=paste("TLit/www/output/",sliceSize,"/",theSource,"_social-network.html",sep=''),launchBrowser=F, 
                   displaylabels = T, label=hnetwork %v% "vertex.names",label.cex=.5,
                   vertex.col="orange",edge.col="darkgray",label.cex=0.5,
                   vertex.cex = .5, vertex.border="black",
                   vertex.tooltip = paste("<span style='font-size: 10px;'><b>Character:</b>", (hnetwork %v% "vertex.names"), "<br />","<b>Co-occurring characters:</b>", (linkedChar2$links)),
                   edge.lwd = .3,
                   object.scale = 0.1,
                   edge.tooltip = paste("<b>Number of Links:</b>", (hnetwork %e% "label"),"</span>" ))
    detach("package:ndtv", unload=TRUE)
    detach("package:tsna", unload=TRUE)
    detach("package:sna", unload=TRUE)
    detach("package:networkDynamic", unload=TRUE)
    #stats for the social graph
    degd <- degree.distribution(h)
    wtc <- cluster_walktrap(h)
    gstat <- c(diameter(h),min(degd),max(degd),mean(degd),edge_density(h),modularity(wtc))
    write.csv2(gstat,paste("TLit/www/output/",sliceSize,"/",theSource,"_socnetstat.csv",sep=''),col.names = F,row.names = F)
    
    ####
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_links_source_nrow.jpg",sep=''))
    plot(links[,2],c(1:nrow(links)),pch=".")
    dev.off()
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_links_targets.jpg",sep=''))
    plot(count(unlist(links[,2]))$freq,type='l')
    dev.off()
    write.csv(cbind(unlist(links[,2]),links[,3]),file=paste("TLit/www/output/",sliceSize,"/",theSource,'_gutenberg_targets.txt',sep=''))
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_links_sources.jpg",sep=''))
    plot(count(unlist(links[,1]))$freq,type='l')
    dev.off()
    write.csv(cbind(unlist(links[,1]),links[,3]),file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_sources.txt",sep=''))
    
    agg <- as.numeric(links[,2]) - as.numeric(links[,1])
    plot(c(1:length(agg)),agg,pch='.')
    hist(agg)
    
    casc <- c()
    inter <- c()
    ent <- data.frame(ww = numeric(0), xx = numeric(0), yy = numeric(0), zz = numeric(0))
    wien <- c()
    colnames(links) <- c('source', 'target', 'tag')
    
    coordinates <- c()
    spec <- list()
    div = 1
    
    g1 <- make_empty_graph(n = 0, directed = TRUE)
    struct <- c()
    props <- c()
    for(z in 1:nrow(nodes)){
      if(nodes[z,]$title == ""){
        if(nrow(ent) > 0){
          ent <- rbind(ent, ent[nrow(ent),])
          wien <- rbind(wien, wien[nrow(wien),])
          
        }else{
          ent <- rbind(ent, c(0, 1, 0, 1))
          wien <- rbind(wien, c(0, 1, 1))
        }
        coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 0, 0))
        
      }else{
        inter <- rbind(inter, paste(sort(unlist(strsplit(nodes[z,2],', '))), collapse = ', '))
        nextI <- digest(paste(sort(unlist(strsplit(nodes[z,2], ', '))), collapse = ', '), algo = "md5")
        if(length(spec) == 0){
          coordinates <- rbind(coordinates, c(as.numeric(nodes[z, 1]), 1, 1))
          spec[[nextI]] <- c(1, 1)
        }
        else{
          if(is.null(spec[[nextI]])){
            spec[[nextI]] <- c(1,div)
            coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],div))
            div <- div+1
          }else{
            spec[[nextI]] <- c(spec[[nextI]][1]+1,spec[[nextI]][2])
            coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],spec[[nextI]][2]))
          }
        }
        
        interact <- list()
        cooccure <- list()
        #temp1 <- c()
        for(v in 1:nrow(inter)){
          interactions <- unlist(strsplit(unlist(inter[v,1]),', '))
          #temp1 <- rbind(temp1, gtools::combinations(length(interactions), 2, interactions))
          for( m in 1:length(interactions)){
            if(is.null(interact[[interactions[m]]])) interact[[interactions[m]]] <- 1
            else interact[[interactions[m]]] <- interact[[interactions[m]]] + 1
          }
        }
        df <- data.frame(unlist(interact))
        tmp <- df[,1] / colSums(df)
        df$loga <- log(tmp)
        df$piloga <- tmp * log(tmp)
        if(is.nan((-1 * (colSums(df)[3])) / log(nrow(df)))){
          ent <- rbind(ent,c(entropy.empirical(df[,1], unit = "log2"), 1, -1 * (colSums(df)[3]), 1))
        } else{
          ent <- rbind(ent, c(entropy.empirical(df[,1], unit = "log2"), (-1 * (colSums(df)[3])) / log2(nrow(df)),-1*(colSums(df)[3]),(-1*(colSums(df)[3]))/log(nrow(df))))
        }
        
        if(nrow(df) == 1){
          wien <- rbind(wien, c(0, 1, 1))
        } else{
          H <- vegan::diversity(df[,1])
          S <- nrow(df)
          J <- H/log(S)
          wien <- rbind(wien,c(H, J, S))
        }}
      
      #}
      colnames(ent)<-c('empEntropy', 'evenness_log2', 'entropy', 'evenness')
      colnames(wien)<-c('ShannonWiener', 'Pielou', 'Richness')
      
      #add node
      g1 <- add_vertices(g1,1,attr = list(id = as.numeric(nodes[z,1])))
      
      #add all links to node
      theLinks <- unique(links[which(links[,2] == nodes[z,1]),1:2])
      for(srclnk in theLinks[,1]){
        g1 <- add_edges(g1, c(which(V(g1)$id == srclnk), which(V(g1)$id == as.numeric(nodes[z,1]))))
      }
      
      #degd <- degree.distribution(g1)
      wtc <- cluster_walktrap(g1)
      struct <- rbind(struct, c(diameter(g1), edge_density(g1), modularity(wtc)))
    }
    #colnames(coordinates) <- c("t","specificity","diversity")
    #data.frame(coordinates) %>%
    #ggplot() +
    #aes(x = specificity, y = diversity, colour = t) +
    #geom_jitter()
    
    
    jpeg(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_coordinates.jpg",sep=''))
    scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
    dev.off()
    
    write.table(cbind(coordinates, wien, struct),
                file=paste0("TLit/www/output/", sliceSize, "/", theSource,
                            "_temporal_statistics.csv"), row.names = F, col.names = F, sep = ";")
    
    write.table(ent, file = paste0("TLit/www/output/", sliceSize, "/",
                                   theSource, "_gutenberg_entropy.txt"), sep = ";")
    
    write.table(wien, file = paste0("TLit/www/output/", sliceSize, "/",
                                    theSource, "_gutenberg_diversity.txt"), sep = ";")
    ent_plot <- as.data.frame(ent) %>%
      mutate(rownumber = seq.int(nrow(.)))
    
    wien_plot <- as.data.frame(wien) %>%
      mutate(rownumber = seq.int(nrow(.)))
    
    ggsave(paste0("TLit/www/output/", sliceSize, "/", theSource,"_gutenberg_entropy.jpg"),
           plot = ent_plot %>%
             ggplot() +
             aes(x = rownumber, y = empEntropy, group = 1) +
             geom_line() +
             labs(y = "Entropy") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/", sliceSize, "/", theSource,"_gutenberg_evenness.jpg"),
           plot = ent_plot %>%
             ggplot() +
             aes(x = rownumber, y = evenness_log2, group = 1) +
             geom_line() +
             labs(y = "Log Evenness") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/", sliceSize, "/",
                  theSource, "_gutenberg_shannonwiener.jpg"),
           plot = wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = ShannonWiener, group = 1) +
             geom_line() +
             labs(y = "Shannon Wiener") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_pielou.jpg"),
           plot = wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = Pielou, group = 1) +
             geom_line() +
             labs(y = "Pielou") +
             theme_classic())
    
    ggsave(paste0("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_richness.jpg"),
           plot = wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = Richness, group = 1) +
             geom_line() +
             labs(y = "Richness") +
             theme_classic())
    
    # scatterplot for character frequency
    labelint = as.integer(nodes$label)
    tempDat <- data.frame(nodes$title, labelint, stringsAsFactors = F)
    tmpSplit <- strsplit(tempDat$nodes.title, split=", ")
    newDat <- data.frame(x = (labelint = rep(tempDat$labelint, sapply(tmpSplit, length))), y = (nodes.title = unlist(tmpSplit)))
    names(newDat)[names(newDat)=="x"] <- "Node"
    names(newDat)[names(newDat)=="y"] <- "Character"
    numberchar <- aggregate(Node ~ Character, data = newDat, length)
    newDat <- merge(newDat,numberchar,by="Character")
    names(newDat)[names(newDat)=="Node.x"] <- "Node"
    names(newDat)[names(newDat)=="Node.y"] <- "Appearances"
    ggplot(newDat, aes(x=Node, y=Character)) + 
      geom_point(size=1, shape = 23) + 
      scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50))
    ggsave(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_character_frequency.pdf",sep=''),scale = 1:2)
    write.csv(newDat,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_character_frequency_csv.txt", sep = ''))
    
    # scatterplot for first and last character appearance
    firstNode <- aggregate(Node ~ Character,newDat,min)
    lastNode <- aggregate(Node ~ Character,newDat,max)
    combined <- rbind(firstNode,lastNode)
    combined$Type <- c(rep("First Appearance",nrow(firstNode)),rep("Last Appearance",nrow(lastNode)))
    combined <- merge(combined,numberchar, by='Character')
    names(combined)[names(combined)=="Node.x"] <- "Node"
    names(combined)[names(combined)=="Node.y"] <- "Appearances"
    ggplot(combined, aes(x = Node, y = Character, group=Type, col=Type)) + 
      geom_point(size=1.5, shape = 23) +
      scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50))
    ggsave(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_first_last_character_appearance.pdf",sep=''),scale = 1:2)
    write.csv(combined,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_first_last_character_appearance_csv.txt", sep = ''))
    
    # .txt list of character appearance
    charfreqcsv <- ddply(newDat, .(Character), summarize, Node = toString(Node))
    write.csv(charfreqcsv[,c("Character","Node")],file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_listofcharacterfreq.txt",sep=''), row.names = F)
    
    #.txt list of number of appearances for each character
    write.csv(numberchar,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_totalnumberofcharacterfreq.txt",sep=''), row.names = F)
    
    # new entropy graph
    entgraph <- data.frame(ent)
    entgraph$indx <- as.integer(row.names(entgraph))
    names(entgraph)[names(entgraph)=="indx"] <- "Node"
    names(entgraph)[names(entgraph)=="empEntropy"] <- "Entropy"
    ggplot(entgraph, aes(x = Node, y = Entropy)) +
      geom_point(size=.2, shape = 23) + geom_line(linetype = 2, size=.2) +
      scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50)) +
      scale_y_continuous (limits = c(0,5)) 
    ggsave(paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_entropy_new.pdf",sep=''))
    write.csv(entgraph,file=paste("TLit/www/output/",sliceSize,"/",theSource,"_gutenberg_entropy_csv.txt", sep = ''))
  }  
  write.csv2(allOutput,paste0('TLit/www/output/',sliceSize,'/allTexts.csv'))
}
