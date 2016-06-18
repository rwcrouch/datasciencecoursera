best <- function(state, outcome) {
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
MHA<-min(data1$mortality.Heart.Attack, na.rm=TRUE)
MHF<-min(data1$mortality.Heart.Failure, na.rm=TRUE)
MP<-min(data1$mortality.Pneumonia, na.rm=TRUE)

code<-c("Heart.Attack", "Heart.Failure", "Pneumonia")

if (outcome=="heart attack"){
  data2<-data1[data1$mortality.Heart.Attack==MHA,]
  HA.names<-data2$Hospital.Name
  HA.names<-sort(HA.names)
  HA.names<-unlist(HA.names)
  HA.names[1]
  }


if (outcome=="heart failure"){
  
      data2<-data1[data1$mortality.Heart.Failure==MHF,]
      HA.names<-data2$Hospital.Name
      HA.names<-sort(HA.names)
      HA.names<-unlist(HA.names)
      HA.names[1]
  }

if (outcome=="pneumonia"){
  
  data2<-data1[data1$mortality.Pneumonia==MP,]
  HA.names<-data2$Hospital.Name
  HA.names<-sort(HA.names)
  HA.names<-unlist(HA.names)
  HA.names[1]
}
HA.names[1]
}


