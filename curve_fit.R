library(plyr)
library(data.table)

# Read Data
train <- fread("RepairTrain.csv", data.table = FALSE)
names(train)[4] <- "year_month_repair"

repair_train <- transform(train, 
                          year_repair  = as.integer(substr(year_month_repair, 1, 4)), 
                          month_repair = as.integer(substr(year_month_repair, 6, 7)))

repair_train <- transform(repair_train, 
                          year_month_repair = year_repair * 100 + month_repair)


repair_train <- subset(repair_train, year_month_repair >= 200905)


repair_agg <- aggregate(number_repair ~ module_category + component_category +
                          year_month_repair, repair_train, sum)

repair_agg$mcID<-paste0(repair_agg[,1],'_',repair_agg[,2],sep='')
repair_agg<-repair_agg[,c(5,1:4)]

#Sorting
repair_agg.sort <- repair_agg[with(repair_agg, order(mcID,year_month_repair)), ]

repair_agg.sort[1,]

train_component<- repair_agg.sort[ !duplicated( repair_agg.sort$mcID, fromLast=FALSE ) , ]

train_component$mon5Repair<-0*train_component[,5]
train_component$mon6Repair<-0*train_component[,5]
train_component$mon7Repair<-0*train_component[,5]
train_component$mon8Repair<-0*train_component[,5]
train_component$mon9Repair<-0*train_component[,5]
train_component$mon10Repair<-0*train_component[,5]
train_component$mon11Repair<-0*train_component[,5]
train_component$mon12Repair<-0*train_component[,5]
train_component$Repairsum<-0*train_component[,5]
train_component<-train_component[,-5]

m=dim(repair_agg.sort)[1]
tempindex=1
for (i in 1:m)
{
  if(repair_agg.sort[i,1]==train_component[tempindex,1]){
    train_component[tempindex,(repair_agg.sort[i,4]-200905+5)]<-repair_agg.sort[i,5]
  }
  else{
    tempindex<-tempindex+1
    train_component[tempindex,(repair_agg.sort[i,4]-200905+5)]<-repair_agg.sort[i,5]
  }
  
}

n=dim(train_component)[1]
for (i in 1:n)
{
  train_component[i,13]=sum(train_component[i,5:12])
}

names(train_component)[4]<-"year_repair"
train_component[,4]<-train_component[,4]*0+2009

train_component.sort <- train_component[with(train_component, order(Repairsum,decreasing = TRUE)), ]
write.csv(train_component.sort,"trainForExcelAnalysis.csv",row.names=FALSE)
trainfinal<-train_component
pred <- read.csv("Output_TargetID_Mapping.csv", header=TRUE)

# Add variable 
pred$mcID<-paste0(pred[,1],'_',pred[,2],sep='')

pred$id<-0*pred[,4]
m=dim(pred)[1]
for (i in 1:m)
{
  pred[i,6]=i
}


predsubmit<-pred[,c(6,4)]
predsubmit$target<-0*predsubmit[,2]
predsubmit<-predsubmit[,c(1,3)]
summary(predsubmit)

prediction_int<- pred[ !duplicated( pred$mcID, fromLast=FALSE ) , ]
prediction_int<-prediction_int[,c(5,1:2,6,3)]
prediction_int[1,]
summary(prediction_int)

#o value for 19 months
prediction_int$p1<-0*prediction_int[,4]
prediction_int$p2<-0*prediction_int[,4]
prediction_int$p3<-0*prediction_int[,4]
prediction_int$p4<-0*prediction_int[,4]
prediction_int$p5<-0*prediction_int[,4]
prediction_int$p6<-0*prediction_int[,4]
prediction_int$p7<-0*prediction_int[,4]
prediction_int$p8<-0*prediction_int[,4]
prediction_int$p9<-0*prediction_int[,4]
prediction_int$p10<-0*prediction_int[,4]
prediction_int$p11<-0*prediction_int[,4]
prediction_int$p12<-0*prediction_int[,4]
prediction_int$p13<-0*prediction_int[,4]
prediction_int$p14<-0*prediction_int[,4]
prediction_int$p15<-0*prediction_int[,4]
prediction_int$p16<-0*prediction_int[,4]
prediction_int$p17<-0*prediction_int[,4]
prediction_int$p18<-0*prediction_int[,4]
prediction_int$p19<-0*prediction_int[,4]


names(prediction_int)[5]="tag"
prediction_int[,5]=0*prediction_int[,5]

m=dim(trainfinal)[1]
tempfirst=1;
for (i in 1:m)
{
  for (j in 0:20){
    if(trainfinal[i,1]==prediction_int[tempfirst+j,1])
    {prediction_int[tempfirst+j,5]<-1#set tag=1
    tempfirst=tempfirst+j
    break;}
  }
}


prediction_int <- prediction_int[with(prediction_int, order(tag,decreasing = TRUE)), ]


prediction_int[20,]
trainfinal[20,]
predsubmit[20,]

# Exponential decay 
m<-dim(trainfinal)[1]
for(i in 1:m)
{
  if(trainfinal[i,13]>5&trainfinal[i,13]<10){
    decay1=0.9
    for(j in 6:24){
      prediction_int[i,j]<-(1/3.0*(trainfinal[i,10]+trainfinal[i,11]+trainfinal[i,12])*decay1^(j-5))
    }
  }
  
  else if(trainfinal[i,13]>3000){
    if(trainfinal[i,13]==11788){
      decay=(0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        else if(j==7){
          decay=0.85
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<13)
        {decay=0.995
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<17)
        {decay=0.885
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<20)
        {decay=0.84
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else
        {decay=0.87
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==10044){
      decay=(0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        if(j==7){
          prediction_int[i,j]<-prediction_int[i,j-1]*min(max(decay,0.75),decay+0.05)
        }
        if(j>7)
        {decay=0.68
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        if(j>9)
        {decay=0.72
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    
    
    else if(trainfinal[i,13]==8471){
      decay=0.96 
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        else if(j<14)
        {decay=0.965
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<15)
        {decay=0.94
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<18)
        {decay=0.9
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<20)
        {decay=0.85
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else
        {decay=0.83
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==6239){
      decay=(0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        else if(j<10){
          decay=0.62
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else{
          decay=0.55
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    
    else if(trainfinal[i,13]==4526){
      decay=(0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        else if(j<9)
        {decay=0.75
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<12)
        {decay=0.8
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<14)
        {decay=0.92
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<18)
        {decay=0.96
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<20)
        {decay=0.9
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else
        {decay=0.88
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==4167){
      decay=0.9 
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        else if(j==7)
        {decay=0.96
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<18)
        {decay=0.965
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<20)
        {decay=0.92
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else
        {decay=0.885
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==3380){
      decay=(0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        else if(j<9)
        {decay=0.75
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<11)
        {decay=0.8
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<18)
        {decay=0.82
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else
        {decay=0.85
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    
    else if(trainfinal[i,13]==3120){
      decay=0.75 
      for(j in 6:24){
        if(j==6){
          prediction_int[i,j]<-trainfinal[i,12]*decay  
        }
        else if(j==7)
        {decay=0.85
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<10)
        {decay=0.9
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<12) 
        {decay=0.93
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else
        {decay=0.94
        prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    
  }
  
  else if(trainfinal[i,13]>1370){
    if(trainfinal[i,13]==1733){
      decay=0.96   
      for(j in 6:24){
        if(j<12){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<18){
          decay=0.92
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.89
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    if(trainfinal[i,13]==1380){
      decay=0.983   
      for(j in 6:24){
        if(j<17){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<20) {
          decay=0.93
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.85
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
  }
  
  else if(trainfinal[i,13]>=142){
    if(trainfinal[i,13]==1360){
      decay=0.975   
      for(j in 6:24){
        if(j<16){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<18){
          decay=0.95
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<21){
          decay=0.92
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.88
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==1060){
      decay=0.72   
      for(j in 6:24){
        if(j<8){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else {
          decay=0.74
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==885){
      decay=0.8   
      for(j in 6:24){
        if(j<7){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<10){
          decay=0.9
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<18){
          decay=0.93
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.88
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==685){
      decay=0.985   
      for(j in 6:24){
        if(j<15){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<18){
          decay=0.92
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.84
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    
    else if(trainfinal[i,13]==609){
      decay=0.8   
      for(j in 6:24){
        if(j<8){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<10){
          decay=0.7
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.65
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==569){
      decay=0.75   
      for(j in 6:24){
        if(j<7){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<8){
          decay=0.8
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<10){
          decay=0.85
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else if(j<18){
          decay=0.9
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.85
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    else if(trainfinal[i,13]==508){
      decay=0.94   
      for(j in 6:24){
        if(j<8){
          prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))}
        else if(j<18){
          decay=0.95
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
        else {
          decay=0.9#0.92
          prediction_int[i,j]<-(prediction_int[i,j-1]*decay)}
      }
    }
    
    else{
      decay=min(0.9,0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j>7)
        {decay=max(decay,0.8)}
        if(j>9)
        {decay=0.85}
        if(j>11)
        {decay=max(decay,0.90)}
        if(j>18)
        {decay=max(decay,0.91)}
        prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))
      }
    }  
  }
  
  
  else if(trainfinal[i,13]>=64){
    if(trainfinal[i,12]<8){
      decay=min(0.9,0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j>7)
        {decay=max(decay,0.8)}
        if(j>9)
        {decay=0.85}
        if(j>11)
        {decay=max(decay,0.90)}
        prediction_int[i,j]<-(trainfinal[i,12]*decay^(j-5))
      }
    }
    else{
      decay=min(0.95,0.7*trainfinal[i,12]/trainfinal[i,11]+0.3*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j>8)
        {decay=max(decay,0.9)}
        prediction_int[i,j]<-(1/3.0*(trainfinal[i,10]+trainfinal[i,11]+trainfinal[i,12]))*decay^(j-5)
      }
    }
  }
  
  else if(trainfinal[i,13]>=42){
    {
      decay=min(0.95,0.5*trainfinal[i,12]/trainfinal[i,11]+0.5*trainfinal[i,11]/trainfinal[i,10])
      for(j in 6:24){
        if(j>7)
        {decay=max(decay,0.9)}
        prediction_int[i,j]<-trainfinal[i,12]*decay^(j-5)
      }
    }
  }
  
  else if(trainfinal[i,13]>=10){
    {
      decay=0.9
      for(j in 6:24){
        prediction_int[i,j]<-(1/3.0*(trainfinal[i,10]+trainfinal[i,11]+trainfinal[i,12]))*decay^(j-5)
      }
    }
  }
  
  
  if(trainfinal[i,13]==885||trainfinal[i,13]==289||trainfinal[i,13]==220||trainfinal[i,13]==209||trainfinal[i,13]==442){
    decay=0.85
    for(j in 18:24){
      if(j==18){
        prediction_int[i,j]<-prediction_int[i,j-1]*decay*0.7}
      else{
        prediction_int[i,j]<-prediction_int[i,j-1]*decay}
    }
  }
  
  if(trainfinal[i,13]==115||trainfinal[i,13]==103||trainfinal[i,13]==78||trainfinal[i,13]==60||trainfinal[i,13]==88||trainfinal[i,13]==133||trainfinal[i,13]==50){
    decay=0.85
    for(j in 18:24){
      if(j==18){
        prediction_int[i,j]<-prediction_int[i,j-1]*decay*0.7}
      else{
        prediction_int[i,j]<-prediction_int[i,j-1]*decay}
    }
  }
  
  for(j in 6:24){
    if((prediction_int[i,j]<0.7&j>10)||prediction_int[i,j]<0.3){
      prediction_int[i,j]<-0}
  }
  
}

m=dim(trainfinal)[1]
m
for(i in 1:m)
{
  for(j in 0:18){
    predsubmit[(prediction_int[i,4]+j),2]<-prediction_int[i,6+j]}
}
write.csv(predsubmit,"predsubmit.csv",row.names=FALSE)



