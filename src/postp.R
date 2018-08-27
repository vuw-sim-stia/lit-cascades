#set working directoy
setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/lit-cascades/src/")

#slice_sizes <- c(1000,500,250)
slice_sizes <- c(1000)

options(scipen=10000)


dat <- fread(paste0("TLit/www/output/",sliceSize,"/",theSource,"_temporal_statistics.csv"),sep=" ", header = T,fill = T)
dat<-dat[,-1]
colnames(dat)<-c("dpub","specificity","diversity","ShannonWiener","Pielou","Richness","Diameter","Density","Modularity")
dat$cnt <- c(1:nrow(dat))



# MDS on social net extracted
links <- read.table("TLit/www/output/1000/Great_Expectations_socialnetwork_links.csv",sep = " ",header = T,stringsAsFactors = F)
chars <- unique(c(links$id1,links$id2))
mat <- matrix(data = 0,nrow=length(chars),ncol=length(chars),dimnames = list(chars,chars))
for(i in 1:nrow(links)){
  mat[links[i,1],links[i,2]]<-links[i,3]
  mat[links[i,2],links[i,1]]<-links[i,3]
}

mat_dis <- max(mat)-mat


d <- dist(mat) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
#fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="x", ylab="y",pch='.')
text(x, y, labels = row.names(mat), cex=.5, pos = 4)


#3D 
library(rgl) 
library(RColorBrewer)
data.mds <- cmdscale(d, k=3) 

#Create x,y refs 
data.x <- data.mds[,1] 
data.y <- data.mds[,2] 
data.z <- data.mds[,3] 

#Plot 
plot3d(data.x, data.y, data.z, col=brewer.pal(nrow(mat), "BrBG"),xlab="x", ylab="y",zlab="z") 
text3d(data.x, data.y, data.z,rownames(mat),cex=.8)

#MDS on all network featues extracted

netstat <- read.table("TLit/www/output/1000/netstat_combined.csv",header = F,stringsAsFactors = F)

netstatmat <- netstat[,c(2,5,6,7)]
d <- dist(netstatmat) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="x", ylab="y",pch='.')
text(x, y, labels = netstat$V1, cex=.5, pos = 4)


netstat <- read.table("TLit/www/output/1000/socnetstat_combined.csv",header = F,stringsAsFactors = F)

netstatmat <- netstat[,c(2,5,6,7)]
d <- dist(netstatmat) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="x", ylab="y",pch='.')
text(x, y, labels = netstat$V1, cex=.5, pos = 4)



allsocialnets <- list.files(paste0("TLit/www/output/1000/"),
                          pattern = "(.*)_socialnetwork_links.csv",
                          full.names = T)

for(net in allsocialnets){
  links <- read.table(net,header = T,stringsAsFactors = F, sep=" ")
  chars <- unique(c(links$id1,links$id2))
  mat <- matrix(data = 0,nrow=length(chars),ncol=length(chars),dimnames = list(chars,chars))
  for(i in 1:nrow(links)){
    mat[links[i,1],links[i,2]]<-links[i,3]
    mat[links[i,2],links[i,1]]<-links[i,3]
  }
  
  d <- dist(mat) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  
  # plot solution 
  x <- fit$points[,1]
  y <- fit$points[,2]
  
  jpeg(paste0("TLit/www/output/1000/",gsub("_socialnetwork_links.csv","",gsub("TLit/www/output/1000//","",net)),"_mds_social_network.jpg"))
  plot(x, y, xlab="x", ylab="y",pch='.')
  text(x, y, labels = chars, cex=.5, pos = 4)
  dev.off()
}



