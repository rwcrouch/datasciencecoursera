rankall <- function(outcome, num="best") {
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
  data1[data1=="Not Available"]<-NA
  valid_state<-0
  #for (i in data$State){
   # if (state==i){
   #   valid_state<-1
  #  }
 # }  
 # if (valid_state!=1){
  #  stop("invalid state")
  #  gettermessage("invalid state")
 # }
  illness<-c("heart attack", "heart failure", "pneumonia")
  
  if (outcome!= illness[1] & outcome!= illness[2] & outcome!= illness[3]){
    stop("invalid outcome")
  }
  #attack<-as.numeric(data1$Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  #failure<-as.numeric(data1$Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  #pneum<-as.numeric(data1$Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  data1$mortality.Heart.Attack<-as.numeric(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  data1$mortality.Heart.Failure<-as.numeric(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  data1$mortality.Pneumonia<-as.numeric(data1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  # data1$MHA<-sort(data1$mortality.Heart.Attack)
  #data1$MHF<-sort(data1$mortality.Heart.Failure)
  #data1$MP<-sort(data1$mortality.Pneumonia)
  matrix<-matrix(,nrow=0,ncol=2)
 matrix<-data.frame(matrix)
 names(matrix)<-c("hospital","state")
  code<-c("Heart.Attack", "Heart.Failure", "Pneumonia")
 #data1<-data1[complete.cases(data1),]
 state.x<-unique(data1$State)
 state<-unique(data1$State)
 data<-data1
 for (i in state){
   
   data1<-data[data$State==i,]
 
  if (outcome=="heart attack"){
 sum.na<-is.na(data1$mortality.Heart.Attack)
    data1<-data1[order(data1$mortality.Heart.Attack, data1$Hospital.Name, decreasing=FALSE),]
    data1$rank<-order(data1$mortality.Heart.Attack, data1$Hospital.Name)
    
    
    #data2<-data2[complete.cases(data2),]
    HA.names<-subset(data1,select=c(Hospital.Name,State))
    names(HA.names)<-c("hospital","state")
    if (num=="best"){
      HA.names<-HA.names[data1$rank==1,]
    }
 if (num=="worst"){
   HA.names<-HA.names[data1$rank==nrow(HA.names)-sum.na,]
   
 }
    if (num!="best" & num!="worst"){
      x<-max(data1$rank)
      
      HA.names<-HA.names[data1$rank==num,]
      if (x<num){
        HA.names<-c(NA,i)
      }
    }
  }
  if (outcome=="heart failure"){
    sum.na<-is.na(data1$mortality.Heart.Failure)
    data1<-data1[order(data1$mortality.Heart.Failure, data1$Hospital.Name, decreasing=FALSE),]
    data1$rank<-order(data1$mortality.Heart.Failure, data1$Hospital.Name)
    
    
    #data2<-data2[complete.cases(data2),]
    HA.names<-subset(data1,select=c(Hospital.Name,State))
    names(HA.names)<-c("hospital","state")
    if (num=="best"){
      HA.names<-HA.names[data1$rank==1,]
    }
    if (num=="worst"){
      HA.names<-HA.names[data1$rank==nrow(HA.names)-sum.na,]
      
    }
    if (num!="best" & num!="worst"){
      x<-max(data1$rank)
      
      HA.names<-HA.names[data1$rank==num,]
      if (x<num){
        HA.names<-c(NA,i)
      }
    }
  }
  
  if (outcome=="pneumonia"){
    sum.na<-sum(is.na(data1$mortality.Pneumonia))
    data1<-data1[order(data1$mortality.Pneumonia, data1$Hospital.Name, decreasing=FALSE),]
    data1$rank<-order(data1$mortality.Pneumonia, data1$Hospital.Name, decreasing=FALSE)
    
    
    
    #data2<-data2[complete.cases(data2),]
    HA.names<-subset(data1,select=c(Hospital.Name,State))
    names(HA.names)<-c("hospital","state")
    if (num=="best"){
      HA.names<-HA.names[data1$rank==1,]
    }
    if (num=="worst"){
      HA.names<-HA.names[data1$rank==nrow(HA.names)-sum.na,]
      
    }
    if (num!="best" & num!="worst"){
      x<-max(data1$rank)
      
      HA.names<-HA.names[data1$rank==num,]
      if (x<num){
        HA.names<-c(NA,i)
      }
    }
  }
 
 
 matrix<-rbind(matrix,HA.names)
}

matrix<-matrix[order(matrix$state),]
matrix


}
