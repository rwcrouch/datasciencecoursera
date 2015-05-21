pollutantmean<-function(directory, pollutant, id=1:332){
  file_list <- list.files(directory)
  x<-matrix(, nrow = 0, ncol = 4)
  dataset<-data.frame(x)
  
  setwd(directory)
  for (file in file_list){    
    temp_dataset <-read.csv(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 setwd('..')  
  x<-matrix(, nrow = 0, ncol = 2)
data<-data.frame(x)

for (i in id){
  
temp_data<-dataset[which(dataset$ID==i),2:3]
data<-rbind(data,temp_data)
}

if (pollutant=="sulfate"){y<-data[,1]
}else{
  y<-data[,2]
}                     


mean(y,na.rm=TRUE)


}
