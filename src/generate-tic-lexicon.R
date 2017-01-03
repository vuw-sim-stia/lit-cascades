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
library(gtools)

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

#set working directoy
setwd("/Users/mlr/Documents/git-projects/lit-cascades/src/")

#select which texts to process
#litSources <- c('greatexpectations','davidcopperfield','chuzzlewit')
litSources <- c('greatexpectations')

for(theSource in litSources){
  fileName <- paste('../resources/',theSource,'.txt',sep='')
  sourceText <- readChar(fileName, file.info(fileName)$size)
  processedText <- gsub("\\r\\n", " ", sourceText,perl=T)
  processedText <- gsub("\\n", " ", processedText,perl=T)
  
  #split by number of words
  #full_text <- strsplit(processedText, "\\s+")[[1]]
  #groupA <- rep(seq(ceiling(length(full_text)/1200)), each=1200)[1:length(full_text)]
  #words300A <- split(full_text, groupA)
  #words300B <- words300A
  
  #split by number of words and chapters
  full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\s.]?)", perl=T)[[1]]
  words300B <- c()
  tmp <- sapply(full_text,function(x){
    snippet <- strsplit(x, "\\s+")[[1]]
    if(length(snippet>1)){
      groupA <- rep(seq(ceiling(length(snippet)/1200)), each=1200)[1:length(snippet)]
      words300A <- split(snippet, groupA)
      words300B <- c(words300B,words300A)
    }
  })
  
  for(s in 1:length(tmp)){
    words300B <- c(words300B,tmp[[s]])
  }
  
  words300A <- words300B
  
  #export text as HTML file to allow jumoing to respective slice
  htmlHead <- "<htlm><head><script>function foo(){var hash = window.location.hash.substring(1); document.getElementById(hash).style.border = '2px solid red'; document.getElementById(hash).style.padding = '15px'; window.onfocus=function(event){location.reload();}}</script></head><body onload='foo()'>"
  htmlTail <- "</body>"
  
  htmlContent <- ""
  
  for(h in 1:length(words300B)){
    htmlContent <- paste(htmlContent,"<p id='slice-",h,"'><a name='slice-",h,"'>",paste(unlist(words300B[h]),collapse=' '),"</a></p>",sep='')
  }
  write(paste(htmlHead,htmlContent,htmlTail,sep=''),file=paste('../output/',theSource,'_textchunks.html',sep=''))
  
  #character list
  tmp <- readLines(paste('../resources/',theSource,'_chars.txt',sep=''))
  
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
      nWords <- sapply(gregexpr("\\W+", needle), length) +1
      words300A[j] <- strsplit(gsub(needle,paste(replicate(nWords, "FOOBAR"), collapse = " "),paste(unlist(words300A[j]),collapse=' '))," ")
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
  
  for(p in charDS$i){
    tags <- unlist(strsplit(as.character(charDS[which(charDS[,1]==p),2]), split=", "))
    nodes <- rbind(nodes, c(charDS[which(charDS[,1]==p),1],as.character(charDS[which(charDS[,1]==p),2]),charDS[which(charDS[,1]==p),1]))
    
    for(j in 1:length(tags)){
      cur_tag <- tags[j]
      if(!is.null(unlist(last_node[cur_tag]))){ 
        source_node <- last_node[cur_tag]
        target_node <- p
        links <- rbind(links, c(source_node,target_node,cur_tag))
      } else {
        roots <- rbind(roots, c(p,cur_tag))
      }
      last_node[cur_tag] <- p
    }
    
  }
  colnames(nodes) <- c('node_id','tags','dpub')
  colnames(links) <- c('source','target','tag')
  colnames(roots) <- c('root_node_id','tag')
  
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
  render.d3movie(nd, filename=paste("../output/",theSource,"_dynamic-network.html",sep=''),launchBrowser=F, 
                 displaylabels = T, label=nd %v% "vertex.names",
                 vertex.col="white",edge.col="darkgray",label.cex=.6,
                 vertex.cex = function(slice){ degree(slice)/10 }, vertex.border="#000000",
                 vertex.tooltip = paste("<span style='font-size: 10px;'><b>Slice:</b>", (nd %v% "step") , "<br />","<b>Matched characters:</b>", (nd %v% "content"), "<br /><a href='",paste("../output/",theSource,"_textchunks.html#slice-",(nd %v% "step"),sep=''),"' target='blank'>Go to content</a><br />"),
                 edge.lwd = (nd %e% "width"),
                 edge.len = 5, uselen = T,object.scale = 0.1,
                 edge.tooltip = paste("<b>Link:</b>", (nd %e% "set"),"</span>" ))
  detach("package:ndtv", unload=TRUE)
  detach("package:tsna", unload=TRUE)
  detach("package:sna", unload=TRUE)
  detach("package:networkDynamic", unload=TRUE)
  
  colnames(links) <- c("id1","id2","label")
  g <- graph.data.frame(links,directed=TRUE)
  
  V(g)$frame.color <- "white"
  V(g)$color <- "orange"
  
  deg <- degree(g, mode="all")
  V(g)$size <- deg*3
  
  E(g)$width <- 0.1
  
  lay <- layout_with_dh(g)
  lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
  minC <- rep(-Inf, vcount(g))
  maxC <- rep(Inf, vcount(g))
  minC[1] <- maxC[1] <- 0
  co <- layout_with_fr(g, minx=minC, maxx=maxC,
                       miny=minC, maxy=maxC)
  
  pdf(paste("../output/",theSource,"_gutenberg_dh_net.pdf",sep=''))
  plot(g, layout=co*1.0, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
  dev.off()
  
  deg <- degree(g, mode="all")
  pdf(paste("../output/",theSource,"_gutenberg_dh_net_degdistri.pdf",sep=''))
  deg.dist <- degree_distribution(g, cumulative=T, mode="all")
  plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")
  dev.off()
  
  degd <- degree.distribution(g)
  wtc <- cluster_walktrap(g)
  gstat <- c(diameter(g),min(degd),max(degd),mean(degd),edge_density(g),modularity(wtc))
  write.csv2(gstat,paste("../output/",theSource,"_netstat.csv",sep=''),col.names = F,row.names = F)
  
  nodes <- as.data.frame(nodes,stringsAsFactors=F)
  colnames(nodes) <- c('id','title','label')
  nodes$id <- as.numeric(nodes$id)
  
  links <- as.data.frame(links)
  colnames(links) <- c('from','to','title')
  links$from<-unlist(links$from)
  links$to<-unlist(links$to)
  links$title<-unlist(links$title)
  
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
  
  deg <- degree(h, mode="all")
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
  
  pdf(paste("../output/",theSource,"_gutenberg_dh_socnet.pdf",sep=''))
  plot(h, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
  dev.off()
  
  netm <- as_adjacency_matrix(h, attr="weight", sparse=F)
  colnames(netm) <- V(h)$name
  rownames(netm) <- V(h)$name
  
  palf <- colorRampPalette(c("gold", "dark orange")) 
  pdf(paste("../output/",theSource,"_gutenberg_dh_socnet_heatmap.pdf",sep=''))
  heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), scale="none", margins=c(20,20) )
  dev.off()
  
  pdf(paste("../output/",theSource,"_gutenberg_dh_socnet_degdistri.pdf",sep=''))
  deg.dist <- degree_distribution(h, cumulative=T, mode="all")
  plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")
  dev.off()
  
  #stats for the social graph
  degd <- degree.distribution(h)
  wtc <- cluster_walktrap(h)
  gstat <- c(diameter(h),min(degd),max(degd),mean(degd),edge_density(h),modularity(wtc))
  write.csv2(gstat,paste("../output/",theSource,"_socnetstat.csv",sep=''),col.names = F,row.names = F)
  
  ####
  
  jpeg(paste("../output/",theSource,"_gutenberg_links_source_nrow.jpg",sep=''))
  plot(links[,2],c(1:nrow(links)),pch=".")
  dev.off()
  
  jpeg(paste("../output/",theSource,"_gutenberg_links_targets.jpg",sep=''))
  plot(count(unlist(links[,2]))$freq,type='l')
  dev.off()
  write.csv(cbind(unlist(links[,2]),links[,3]),file=paste('../gutenberg_targets.txt',sep=''))
  
  jpeg(paste("../output/",theSource,"_gutenberg_links_sources.jpg",sep=''))
  plot(count(unlist(links[,1]))$freq,type='l')
  dev.off()
  write.csv(cbind(unlist(links[,1]),links[,3]),file=paste("../output/",theSource,"_gutenberg_sources.txt",sep=''))
  
  agg <- as.numeric(links[,2]) - as.numeric(links[,1])
  plot(c(1:length(agg)),agg,pch='.')
  hist(agg)
  
  casc <- c()
  inter <- c()
  ent <- c()
  wien <- c()
  colnames(links) <- c('source','target','tag')
  
  coordinates <- c()
  spec <- list()
  div=1
  
  for(z in 1:nrow(nodes)){
    #entropy
    if(z==1){
      ent <- rbind(ent,c(0,1,0,1))
    }
    if(length(links[which(links[,2]==nodes[z,1]),3])>0){
      print(nodes[z,1])
      inter <- rbind(inter,paste(sort(unlist(links[which(links[,2]==nodes[z,1]),3])), collapse=', '))
      nextI <- digest(paste(sort(unlist(links[which(links[,2]==nodes[z,1]),3])), collapse=', '),algo="md5")
      if(length(spec)==0){
        coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),0,0))
        spec[[nextI]] <- c(0,0)
      }
      else{
        if(is.null(spec[[nextI]])){
          spec[[nextI]] <- c(0,div)
          coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],div))
          div <- div+1
        }else{
          spec[[nextI]] <- c(spec[[nextI]][1]+1,spec[[nextI]][2])
          coordinates <- rbind(coordinates,c(as.numeric(nodes[z,1]),spec[[nextI]][1],spec[[nextI]][2]))
        }
      }
      
      interact <- list()
      for(v in 1:nrow(inter)){
        interactions <- strsplit(unlist(inter[v,1]),', ')
        for( m in 1:length(interactions)){
          if(is.null(interact[[interactions[[1]][m]]])) interact[[interactions[[1]][m]]] <- 1
          else interact[[interactions[[1]][m]]] <- interact[[interactions[[1]][m]]] + 1
        }
      }
      df <- data.frame(unlist(interact))
      tmp<-df[,1]/colSums(df)
      df$loga<-log(tmp)
      df$piloga<-tmp*log(tmp)
      if(is.nan((-1*(colSums(df)[3]))/log(nrow(df)))){
        ent <- rbind(ent,c(entropy.empirical(df[,1], unit="log2"),1,-1*(colSums(df)[3]),1))
      } else{
        ent <- rbind(ent,c(entropy.empirical(df[,1], unit="log2"),(-1*(colSums(df)[3]))/log2(nrow(df)),-1*(colSums(df)[3]),(-1*(colSums(df)[3]))/log(nrow(df))))
      }
    }
    colnames(ent)<-c('empEntropy','evenness_log2','entropy','evenness')
  }
  colnames(coordinates) <- c("t","specificity","diversity")
  jpeg(paste("../output/",theSource,"_gutenberg_coordinates.jpg",sep=''))
  scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
  dev.off()
  
  write.csv(ent,file=paste("../output/",theSource,"_gutenberg_entropy.txt",sep=''))
  jpeg(paste("../output/",theSource,"_gutenberg_entropy.jpg",sep=''))
  plot(ent[,1],type="l")
  dev.off()
  jpeg(paste("../output/",theSource,"_gutenberg_evenness.jpg",sep=''))
  plot(ent[,2],type="l")
  dev.off()
  
  # scatterplot for character frequency
  labelint = as.integer(nodes$label)
  tempDat <- data.frame(nodes$title, labelint, stringsAsFactors = F)
  tmpSplit <- strsplit(tempDat$nodes.title, split=", ")
  newDat <- data.frame(x = (labelint = rep(tempDat$labelint, sapply(tmpSplit, length))), y = (nodes.title = unlist(tmpSplit))) 
  ggplot(newDat, aes(x=x, y=y)) + 
    geom_point(size=1, shape = 23) + 
    scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50))
  ggsave(paste("../output/",theSource,"_gutenberg_character_frequency.pdf",sep=''),scale = 1:2)
  
  # scatterplot for first and last character appearance
  firstNode <- aggregate(x ~ y,newDat,min)
  lastNode <- aggregate(x ~ y,newDat,max)
  combined <- rbind(firstNode,lastNode)
  colnames(firstNode) <- c('y','x')
  combined$type <- c(rep("first",nrow(firstNode)),rep("last",nrow(lastNode)))
  ggplot(combined, aes(x = x, y = y,group=type,col=type)) + 
    geom_point(size=1.5, shape = 23) +
    scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50))
  ggsave(paste("../output/",theSource,"_gutenberg_first_last_character_appearance.pdf",sep=''),scale = 1:2)
  
  # .txt list of character appearance
  charfreqcsv <- ddply(newDat, .(y), summarize, x = toString(x))
  write.csv(charfreqcsv[,c("y","x")],file=paste("../output/",theSource,"_gutenberg_listofcharacterfreq.txt",sep=''), row.names = F)
  
  # new entropy graph
  entgraph <- data.frame(ent)
  entgraph$indx <- as.integer(row.names(entgraph))
  ggplot(entgraph, aes(x = indx, y = empEntropy)) +
    geom_point(size=.2, shape = 23) + geom_line(linetype = 2, size=.2) +
    scale_x_continuous (limits = c(0,400), minor_breaks = seq(0 , 400, 5), breaks = seq(0, 400, 50)) +
    scale_y_continuous (limits = c(0,5)) 
  ggsave(paste("../output/",theSource,"_gutenberg_entropy_new.pdf",sep=''))
}