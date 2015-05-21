complete<-function(directory="C:/Users/Randy/Desktop/Coursera/specdata", id=1:332){
  
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
  
data2<-matrix(, nrow = id, ncol = 2)

  nobs<-vector(mode="numeric", length=0)
  for (i in id){
    
    nobs<-sum(complete.cases(dataset[which(dataset$ID==i),]))
    data2[i,1]<-i
    data2[i,2]<-nobs
   
  }
data2<-data.frame(data2[id,])
data2<-na.omit(data2)
  
data2<-data.frame(data2)
names(data2)<-c("id","nobs")
data2
}