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

#fileName <- '../resources/greatexpectations.txt'
fileName <- '../resources/davidcopperfield.txt'
sourceText <- readChar(fileName, file.info(fileName)$size)

# removing special chars and CHAPTER headings

processedText <- gsub("CHAPTER.+?(?=\\r\\n)", "", sourceText,perl=T)
processedText <- gsub("Chapter.+", "", processedText,perl=T)

processedText <- gsub("\\r\\n", " ", processedText)

full_text <- strsplit(processedText, "\\s+")[[1]]
groupA <- rep(seq(ceiling(length(full_text)/1000)), each=1000)[1:length(full_text)]
words300A <- split(full_text, groupA)

#character list
#tmp <- readLines("../resources/greatexpectations_chars.txt")
tmp <- readLines("../resources/davidcopperfield_chars.txt")
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
    matched <- unlist(gregexpr(needle, paste(unlist(words300A[j]),collapse=' ')))
    nWords <- sapply(gregexpr("\\W+", needle), length) + 1
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
    if(!is.null(unlist(last_node[cur_tag]))){ #link back to last posts with this tag
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
               vertex.col="white",edge.col="darkgray",label.cex=.6,
               vertex.cex = function(slice){ degree(slice)/10 }, vertex.border="#000000",
               vertex.tooltip = paste("<b>Name:</b>", (nd %v% "step") , "<br>","<b>Content:</b>", (nd %v% "content")),
               edge.tooltip = paste("<b>Link:</b>", (nd %e% "set") ))

#static slices
timePrism(nd,at=c(10,50,100),
          displaylabels=TRUE,planes = TRUE,
          label.cex=0.5)

#timeline
proximity.timeline(nd,default.dist=6,mode='sammon',labels.at=17,vertex.cex=4)

#stats
library(tsna)
plot( tEdgeFormation(nd) )
plot( tSnaStats(nd,'gtrans'),main='Transitivity' )
plot( tSnaStats(nd,'gden') ,main='Density')
plot( tSnaStats(nd,'hierarchy') ,main='Hierarchy')
#plot( tSnaStats(nd,'betweenness') ,main='Betweenness')
#plot( tSnaStats(nd,'closeness') ,main='Closeness')

#paths
path<-tPath(nd,v = 13,graph.step.time=1)
plotPaths(nd,path,label.cex=0.5)


# more analyses


library(igraph)
colnames(links) <- c("id1","id2","label")
g <- graph.data.frame(links,directed=TRUE)

V(g)$frame.color <- "white"
V(g)$color <- "orange"

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

colrs<-sample(col_vector, 14)
#E(g)$color <- colrs[as.numeric(E(g)$label)]
E(g)$width <- 0.1
lay <- layout_on_grid(g)
lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
pdf(paste("/Users/mlr/Documents/gutenberg_grid_net.pdf",sep=''))
plot(g,rescale=F,vertex.size=5,vertex.label.cex=0.2,edge.label.cex=0.2,layout=lay,edge.arrow.size=0.1)
dev.off()

#lay <- layout_with_dh(g)
#lay <- norm_coords(lay, ymin=-1, ymax=1, xmin=-1, xmax=1)
minC <- rep(-Inf, vcount(g))
maxC <- rep(Inf, vcount(g))
minC[1] <- maxC[1] <- 0
co <- layout_with_fr(g, minx=minC, maxx=maxC,
                     miny=minC, maxy=maxC)

pdf(paste("/Users/mlr/Documents/gutenberg_dh_net.pdf",sep=''))
#plot(g,rescale=F,vertex.size=5,vertex.label.cex=0.6,edge.label.cex=0.2,layout=lay,edge.arrow.size=0.1)
plot(g, layout=co, vertex.size=2,vertex.label.cex=0.2,edge.label.cex=0.2, edge.arrow.size=0.1, rescale=TRUE,vertex.label.dist=0)
dev.off()

degd <- degree.distribution(g)
jpeg(paste("/Users/mlr/Documents/gutenberg_degree_distri.jpg",sep=''))
plot(c(1:length(degd)),degd,type = 'h',xlab='Node degree',ylab='Number of nodes')
dev.off()
wtc <- cluster_walktrap(g)
gstat <- c(diameter(g),min(degree.distribution(g)),max(degree.distribution(g)),mean(degree.distribution(g)),edge_density(g),modularity(wtc))

tri <- mean(as.numeric(substr(matrix(triangles(g), nrow=3), nchar(matrix(triangles(g), nrow=3))-1+1, nchar(matrix(triangles(g), nrow=3)))))
nods <- mean(as.numeric(substr(V(g)$name, nchar(V(g)$name)-1+1, nchar(V(g)$name))))

detach("package:igraph", unload=TRUE)

nodes <- as.data.frame(nodes,stringsAsFactors=F)
nodes$id <- as.numeric(nodes$id)

colnames(nodes) <- c('id','title','label')
links <- as.data.frame(links)
colnames(links) <- c('from','to','title')
network <-visNetwork(nodes, links, width="100%", height="1000px", main="Great Expectations") %>% #visHierarchicalLayout(direction = "LR",sortMethod="directed") %>%
  visOptions(highlightNearest = TRUE, 
             selectedBy = "title")
visSave(network, file = "/Users/mlr/Documents/gutenberg-network.html")

jpeg(paste("/Users/mlr/Documents/gutenberg_triangles_nodes.jpg",sep=''))
triangles <- cbind(tri,nods)
colnames(triangles)<-c('triangles','all nodes')
midpoints <- barplot(triangles, xlab="Mean unit value of node id")
text(midpoints, 2, labels=triangles)
dev.off()

jpeg(paste("/Users/mlr/Documents/gutenberg_links_source_nrow.jpg",sep=''))
plot(links[,2],c(1:nrow(links)),pch=".")
dev.off()

#jpeg(paste("/Users/mlr/Documents/gutenberg_links_source_target.jpg",sep=''))
#plot(links[,2],links[,1],pch=".")
#dev.off()

jpeg(paste("/Users/mlr/Documents/gutenberg_links_targets.jpg",sep=''))
#plot(unlist(links[,2]),factor(unlist(links[,3])),pch=".")
plot(count(unlist(links[,2]))$freq,type='l')
#for(ab in 1:20){
#  if(ab %% 2 != 0) abline(v=(5*ab), col = "lightgray", lty = "dotted")
#  else abline(v=(5*ab), col = "lightgray", lty = "dashed")
#}
dev.off()
write.csv(cbind(unlist(links[,2]),links[,3]),file=paste('/Users/mlr/Documents/gutenberg_targets.txt',sep=''))

jpeg(paste("/Users/mlr/Documents/gutenberg_links_sources.jpg",sep=''))
#plot(unlist(links[,1]),factor(unlist(links[,3])),pch=".")
plot(count(unlist(links[,1]))$freq,type='l')
#for(ab in 1:20){
#  if(ab %% 2 != 0) abline(v=(5*ab), col = "lightgray", lty = "dotted")
#  else abline(v=(5*ab), col = "lightgray", lty = "dashed")
#}
dev.off()
write.csv(cbind(unlist(links[,1]),links[,3]),file=paste('/Users/mlr/Documents/gutenberg_sources.txt',sep=''))

agg <- as.numeric(links[,2]) - as.numeric(links[,1])
plot(c(1:length(agg)),agg,pch='.')
hist(agg)

casc <- c()
inter <- c()
ent <- c()
wien <- c()
colnames(links) <- c('source','target','tag')
#DT <- data.table(links)
#DT[, interactions:=paste(tag,collapse=", "), by=list(source, target)]
#elinks <- unique(cbind(as.data.frame.matrix(DT)$source,as.data.frame.matrix(DT)$target,as.data.frame.matrix(DT)$interactions))

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
  #if(tail(ent[,1],1)>0) print(tail(ent[,1],1))
}
colnames(coordinates) <- c("t","specificity","diversity")
jpeg(paste("/Users/mlr/Documents/gutenberg_coordinates.jpg",sep=''))
scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
dev.off()

#write.csv(wien,file=paste('/home/mlr/temp_stats_',databases[p],'_wiener.txt',sep=''))
write.csv(ent,file=paste('/Users/mlr/Documents/gutenberg_entropy.txt',sep=''))
jpeg(paste("/Users/mlr/Documents/gutenberg_entropy.jpg",sep=''))
plot(ent[,1],type="l")
#for(ab in 1:20){
#  if(ab %% 2 != 0) abline(v=(5*ab), col = "lightgray", lty = "dotted")
#  else abline(v=(5*ab), col = "lightgray", lty = "dashed")
#}
dev.off()
jpeg(paste("/Users/mlr/Documents/gutenberg_evenness.jpg",sep=''))
plot(ent[,2],type="l")
#for(ab in 1:20){
#  if(ab %% 2 != 0) abline(v=(5*ab), col = "lightgray", lty = "dotted")
#  else abline(v=(5*ab), col = "lightgray", lty = "dashed")
#}
dev.off()