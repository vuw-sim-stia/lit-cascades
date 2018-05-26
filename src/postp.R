#set working directoy
setwd("/Users/mlr/Documents/git-projects/lit-cascades/src/")

#slice_sizes <- c(1000,500,250)
slice_sizes <- c(1000)

options(scipen=10000)


dat <- fread(paste0("TLit/www/output/",sliceSize,"/",theSource,"_temporal_statistics.csv"),sep=" ", header = T,fill = T)
dat<-dat[,-1]
colnames(dat)<-c("dpub","specificity","diversity","ShannonWiener","Pielou","Richness","Diameter","Density","Modularity")
dat$cnt <- c(1:nrow(dat))

