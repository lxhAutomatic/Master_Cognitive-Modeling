load("C:\\Users\\75581\\Desktop\\CM\\Lab 1\\keyPressDataWithLaneDeviation.Rdata")
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
se <- function(x) sd(x)/sqrt(length(x))
subsetted_data <- subset(keyPressDataWithLaneDeviation, typingErrorMadeOnTrial == 0)
subsetted_data <- subset(subsetted_data, Event1 == "Keypress")
subsetted_data <- subset(subsetted_data, (partOfExperiment == "dualSteerFocus" | partOfExperiment == "dualDialFocus"))
subsetted_data$absLanePosition <- abs(subsetted_data$lanePosition)
subsetted_data$partOfExperiment <- factor(subsetted_data$partOfExperiment)
subsetted_data$timeRelativeToTrialStart <- round(subsetted_data$timeRelativeToTrialStart/1000, 2)
per_key_and_participant_data <- subsetted_data %>% group_by(pp, phoneNrLengthAfterKeyPress, partOfExperiment) %>% summarise("adjMeanTime"=mean(timeRelativeToTrialStart), "adjMeanLanePosition"=mean(absLanePosition))
per_key_data <- per_key_and_participant_data %>% group_by(phoneNrLengthAfterKeyPress, partOfExperiment) %>% summarise("meanTime"=mean(adjMeanTime), "meanLanePosition"=mean(adjMeanLanePosition), "seLanePosition"=round(se(adjMeanLanePosition),2))
per_key_data$partOfExperiment = factor(per_key_data$partOfExperiment)
mlegend = guide_legend(title="Trial Conditions")
ggplot(data = per_key_data,  aes(x = meanTime, y = meanLanePosition, colour = partOfExperiment), group=partOfExperiment) +
  geom_point(aes(shape=partOfExperiment), size=2) +
  geom_errorbar(aes(ymin=meanLanePosition-seLanePosition, ymax=meanLanePosition+seLanePosition), width=.05) +
  guides(colour=mlegend,shape=mlegend) + 
  labs(x="Dialing Time (sec)", y="Lateral Deviation (m)")+
  geom_line() 
