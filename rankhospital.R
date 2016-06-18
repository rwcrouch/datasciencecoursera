rankhospital <- function(state, outcome, num="best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data1<-data[data$State==state,]
  data1[data1=="Not Available"]<-NA
  valid_state<-0
  for (i in data$State){
    if (state==i){
      valid_state<-1
    }
  }  
  if (valid_state!=1){
    stop("invalid state")
    gettermessage("invalid state")
  }
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
  
  code<-c("Heart.Attack", "Heart.Failure", "Pneumonia")
  
  if (outcome=="heart attack"){
    data2<-data1[order(data1$mortality.Heart.Attack, data1$Hospital.Name, decreasing=FALSE),]
    data2<-data2[complete.cases(data2),]
    HA.names<-data2$Hospital.Name
  if (num=="best"){
    HA.names<-HA.names[1]
  }
  if (num=="worst"){
    HA.names<-HA.names[length(HA.names)]
  }
 if (num!="best" & num!="worst"){
   
   HA.names<-HA.names[num]
    }
  }
  if (outcome=="heart failure"){
    
    data2<-data1[order(data1$mortality.Heart.Failure, data1$Hospital.Name, decreasing=FALSE),]
    data2<-data2[complete.cases(data2),]
    HA.names<-data2$Hospital.Name
    if (num=="best"){
      HA.names<-HA.names[1]
    }
    if (num=="worst"){
      HA.names<-HA.names[length(HA.names)]
    }
    if (num!="best" & num!="worst"){
      HA.names<-HA.names[num]
    }
  }
 
  
  if (outcome=="pneumonia"){
    
    data2<-data1[order(data1$mortality.Pneumonia, data1$Hospital.Name, decreasing=FALSE),]
    data2<-data2[complete.cases(data2),]
    HA.names<-data2$Hospital.Name
    if (num=="best"){
      HA.names<-HA.names[1]
    }
    if (num=="worst"){
      HA.names<-HA.names[length(HA.names)]
    }
    if (num!="best" & num!="worst"){
      HA.names<-HA.names[num]
    }
  }
 
  HA.names
}
