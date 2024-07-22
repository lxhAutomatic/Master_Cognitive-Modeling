load("C:\\Users\\75581\\Desktop\\CM\\Lab 1\\keyPressDataWithLaneDeviation.Rdata")
data<-keyPressDataWithLaneDeviation
data_3<-data[which(data$partOfExperiment == "singleDialing2" 
                  & data$typingErrorMadeOnTrial == 0
                  & (data$Event1 == "Keypress" | data$Event1 == "Correct")),]
rownames(data_3)<-NULL
past<-0
sum<-0
count<-1
average<-vector()
number<-c(0,0,0,0,0,0,0,0,0,0,0,0)
for(i in 1:674)
{
  if(data_3[i,'phoneNrLengthBeforeKeyPress']==0)
  {
    past<-data_3[i,'timeRelativeToTrialStart']
  }
  else
  {
    sum<-sum+data_3[i,'timeRelativeToTrialStart']-past
    #print(data_3[i,'timeRelativeToTrialStart']-past)
    #count<-count+1
    past<-data_3[i,'timeRelativeToTrialStart']
  }
  if(data_3[i,'phoneNrLengthBeforeKeyPress']==11)
  {
    average[count]<-sum/11
    count<-count+1
    sum<-0
  }
  if(data_3[i,'phoneNrLengthBeforeKeyPress']==1 & data_3[i+1,'phoneNrLengthBeforeKeyPress']==0)
  {
    average[count]<-sum
    count<-count+1
    sum<-0
  }
  #for(j in 0:11)
  #{
  #  if(data_3[i,'phoneNrLengthBeforeKeyPress']==j)
  #  {
  #    number[j+1]<-number[j+1]+1
  #  }
  #}
  #if(number[1]==number[3]+2)
    #print(i)
}
#value<-sum/count



library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

# Assignment 1

se <- function(x) sd(x)/sqrt(length(x))
tabledrift <- read.csv("C:\\Users\\75581\\Desktop\\CM\\Lab 1\\tableOfDriftValuesCalibration.csv")
tabledrift$trialTime = round(tabledrift$trialTime/1000, 2)
tabledrift$trial = factor(tabledrift$trial)
head(tabledrift)



### Question 3A


filtered_data = keyPressDataWithLaneDeviation %>% filter(partOfExperiment == "singleDialing2") %>% filter(typingErrorMadeOnTrial==0) %>% filter(Event1 == "Keypress")
head(filtered_data)

filtered_data$diffs = 0
filtered_data$diffs[2:nrow(filtered_data)] = diff(filtered_data$timeRelativeToTrialStart)
filtered_data$diffs[filtered_data$phoneNrLengthBeforeKeyPress == 0] = filtered_data$timeRelativeToTrialStart[filtered_data$phoneNrLengthBeforeKeyPress == 0]
head(filtered_data)

per_participant_per_key_data <- filtered_data %>% group_by(pp, trial, phoneNrLengthBeforeKeyPress) %>% summarise("adjMeanDiffs"=mean(diffs))
head(per_participant_per_key_data)

tmp <- per_participant_per_key_data %>% group_by(pp, phoneNrLengthBeforeKeyPress) %>% summarise("adjMeanDiffs"=mean(adjMeanDiffs))
head(tmp)

per_key_data <- tmp %>% group_by(pp) %>% summarise("adjMeanDiffs"=mean(adjMeanDiffs))
head(per_key_data)

per_key_wo_zeros = per_key_data[1:nrow(per_key_data),]$adjMeanDiffs
average_key_press_intervall <- round(mean(per_key_wo_zeros), 2)
average_key_press_intervall

###
load("keyPressDataWithLaneDeviation.Rdata")

subframe <- subset(keyPressDataWithLaneDeviation, partOfExperiment =="singleDialing2" & typingErrorMadeOnTrial == 0)
keypressInterval <- diff(subframe$timeRelativeToTrialStart, 1)
keypressInterval <- keypressInterval[keypressInterval > 0]

meanKpi <- mean(keypressInterval)
meanKpi
meanKpi <- round(meanKpi / 10) * 10
meanKpi
