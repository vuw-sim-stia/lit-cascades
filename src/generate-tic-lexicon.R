library(plyr)
library(tm)
library(stringr)
library(RTextTools)
library(NLP)
library(visNetwork)
library(digest)

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

fileName <- '../resources/greatexpectations.txt'
sourceText <- readChar(fileName, file.info(fileName)$size)

# removing special chars and CHAPTER headings

processedText <- gsub("CHAPTER.+?(?=\\r\\n)", "", sourceText,perl=T)
processedText <- gsub("Chapter.+", "", processedText,perl=T)

processedText <- gsub("\\r\\n", " ", processedText)

full_text <- strsplit(processedText, "\\s+")[[1]]
groupA <- rep(seq(ceiling(length(full_text)/300)), each=300)[1:length(full_text)]
words300A <- split(full_text, groupA)

#character list
tmp <- readLines("/Users/mlr/Downloads/GE_chars.txt")
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
chars <- tmp

matches <- list()
for(j in 1:length(words300A)){
  slicematch <- list()
  for(k in 1:nrow(chars)){
    needle <- chars[k,2]
    matched <- unlist(gregexpr(needle, paste(unlist(words300A[j]),collapse=' ')))
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
    charDS <- rbind(charDS,data.frame(i,paste(sort(unique(nextMatchStr)),collapse=', ')),stringsAsFactors=F)
  }
}

nodes <- c()
links <- c()
roots <- c()

last_node <- list()

for(i in 1:nrow(charDS)){
  tags <- unlist(strsplit(as.character(charDS[i,2]), split=", "))
  nodes <- rbind(nodes, c(charDS[i,1],as.character(charDS[i,2]),charDS[i,1]))
  
  for(j in 1:length(tags)){
    cur_tag <- tags[j]
    if(!is.null(unlist(last_node[cur_tag]))){ #link back to last posts with this tag
      source_node <- last_node[cur_tag]
      target_node <- i
      links <- rbind(links, c(source_node,target_node,cur_tag))
    } else {
      roots <- rbind(roots, c(i,cur_tag))
    }
    last_node[cur_tag] <- i
  }
  
}
colnames(nodes) <- c('node_id','tags','dpub')
colnames(links) <- c('source','target','tag')
colnames(roots) <- c('root_node_id','tag')

library(networkDynamic)
library(ndtv)
nd <- as.networkDynamic(network.initialize(0))
set.network.attribute(nd,"vertex.pid","vertex.names")
for(i in 1:nrow(links)){
  #add.edge(net,links[i,1],links[i,2])
  fromN <- get.vertex.id(nd,unlist(links[i,1]))
  if(is.na(fromN)){
    #add.vertices.active(nd,nv=1,vertex.pid=c(unlist(links[i,1])),onset=as.numeric(unlist(links[i,2])), terminus=max(as.numeric(unlist(links[i,2]))))
    add.vertices(nd,nv=1,vertex.pid=c(unlist(links[i,1])))
    fromN <- get.vertex.id(nd,unlist(links[i,1]))
    set.vertex.attribute(nd,'content',unlist(nodes[which(nodes[,1]==unlist(links[i,1])),2]),v=c(fromN))
    set.vertex.attribute(nd,'step',unlist(nodes[which(nodes[,1]==unlist(links[i,1])),1]),v=c(fromN))
    activate.vertices(nd,onset=as.numeric(unlist(links[i,2])),terminus=max(as.numeric(unlist(links[,2])))+1,v=c(fromN))
  }
  
  toN <- get.vertex.id(nd,unlist(links[i,2]))
  if(is.na(toN)){
    #add.vertices.active(nd,nv=1,vertex.pid=c(unlist(links[i,2])),onset=as.numeric(unlist(links[i,2])), terminus=max(as.numeric(unlist(links[i,2]))))
    add.vertices(nd,nv=1,vertex.pid=c(unlist(links[i,2])))
    toN <- get.vertex.id(nd,unlist(links[i,2]))
    set.vertex.attribute(nd,'content',unlist(nodes[which(nodes[,1]==unlist(links[i,2])),2]),v=c(toN))
    set.vertex.attribute(nd,'step',unlist(nodes[which(nodes[,1]==unlist(links[i,2])),1]),v=c(toN))
    activate.vertices(nd,onset=as.numeric(unlist(links[i,2])),terminus=max(as.numeric(unlist(links[,2])))+1,v=c(toN))
  }
  add.edges.active(nd,onset=as.numeric(unlist(links[i,2])), terminus=max(as.numeric(unlist(links[,2])))+1,head=toN,tail=fromN,names.eval=list('set'),vals.eval=list(unlist(links[i,3])))
}

compute.animation(nd, animation.mode = "kamadakawai", chain.direction=c('forward'),weight.dist=T,default.dist=3)

#interactive
render.d3movie(nd,launchBrowser=T, 
               displaylabels = T, label=nd %v% "vertex.names",
               vertex.col="black",edge.col="darkgray",label.cex=.6,
               vertex.cex = function(slice){ degree(slice)/10 }, vertex.border="#333333",
               vertex.tooltip = paste("<b>Name:</b>", (nd %v% "step") , "<br>","<b>Content:</b>", (nd %v% "content")),
               edge.tooltip = paste("<b>Link:</b>", (nd %e% "set") ))

#static slices
timePrism(nd,at=c(10,20,30),
          displaylabels=TRUE,planes = TRUE,
          label.cex=0.5)

#timeline
proximity.timeline(nd,default.dist=6,mode='sammon',labels.at=17,vertex.cex=4)

#stats
library(tsna)
plot( tEdgeFormation(nd) )
plot( tSnaStats(nd,'gtrans') )

#paths
path<-tPath(nd,v = 13,graph.step.time=1)

plotPaths(nd,path,label.cex=0.5)