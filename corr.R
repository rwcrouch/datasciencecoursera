corr<-function(directory, threshold=0){
 
  file_list <- list.files(directory)
  setwd(directory)
   
  x<-matrix(, nrow = 0, ncol = 4)
  dataset<-data.frame(x)
  for (file in file_list){
    
    temp_dataset <-read.csv(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  setwd('..')
  
 correlation<-vector(mode="numeric", length=0)

   
 dataset.complete<-dataset[complete.cases(dataset),]
 
 nobs<-vector(mode="numeric", length=0)
  for (i in 1:332){
    if (nrow(dataset.complete[which(dataset.complete$ID==i),])>threshold){
      nitrate.thresh<-dataset.complete[which(dataset.complete$ID==i),2]
      sulfate.thresh<-dataset.complete[which(dataset.complete$ID==i),3]

      correlation[i]<-cor(nitrate.thresh,sulfate.thresh)

      }
  
    }
 corr<-na.omit(correlation)
}