load("C:\\Users\\75581\\Desktop\\CM\\Lab 1\\keyPressDataWithLaneDeviation.Rdata")
data<-keyPressDataWithLaneDeviation
data_A_1<-data[which(data$partOfExperiment == "dualSteerFocus" 
                   & data$phoneNrLengthAfterKeyPress == 11
                   & data$typingErrorMadeOnTrial == 0),]
data_A_2<-data[which(data$partOfExperiment == "dualDialFocus" 
                   & data$phoneNrLengthAfterKeyPress == 11
                   & data$typingErrorMadeOnTrial == 0),]
mean_A_1 = mean(data_A_1[,"timeRelativeToTrialStart"])
sd_A_1 = sd(data_A_1[,"timeRelativeToTrialStart"])
sde_A_1 = sd_A_1/sqrt(dim(data_A_1)[1])
mean_A_2 = mean(data_A_2[,"timeRelativeToTrialStart"])
sd_A_2 = sd(data_A_2[,"timeRelativeToTrialStart"])
sde_A_2 = sd_A_2/sqrt(dim(data_A_2)[1])

data_B_1<-data[which(data$partOfExperiment == "dualSteerFocus"),]
data_B_2<-data[which(data$partOfExperiment == "dualDialFocus"),]
mean_B_1 = mean(abs(data_B_1[,"lanePosition"]))
mean_B_2 = mean(abs(data_B_2[,"lanePosition"]))

### Question 1A

subsetted_data <- subset(keyPressDataWithLaneDeviation, typingErrorMadeOnTrial == 0)
subsetted_data <- subsetted_data[subsetted_data$phoneNrLengthAfterKeyPress==11,]
subsetted_data <- subset(subsetted_data, (partOfExperiment == "dualSteerFocus" | partOfExperiment == "dualDialFocus"))
# subsetted_data = subset(subsetted_data, partOfExperiment == c("dualDialFocus", "dualSteerFocus"))
head(subsetted_data)



aggregated_data <- aggregate(timeRelativeToTrialStart ~ pp + partOfExperiment, subsetted_data, FUN = mean)
head(aggregated_data)

grand_mean <- aggregate(timeRelativeToTrialStart ~ partOfExperiment, aggregated_data, FUN = mean)
grand_standard_deviation <- aggregate(timeRelativeToTrialStart ~ partOfExperiment, aggregated_data, FUN = sd)
grand_standard_error <- aggregate(timeRelativeToTrialStart ~ partOfExperiment, aggregated_data, FUN = se)
final_stats <- data.frame(cbind(grand_mean$timeRelativeToTrialStart, grand_standard_deviation$timeRelativeToTrialStart, grand_standard_error$timeRelativeToTrialStart))
colnames(final_stats) <- c("means", "std", "se")
results_question_a = round(final_stats / 1000, digits = 2)
# results_question_a
row.names(results_question_a) <- c("Dual Dial Focus", "Dual Steer Focus")
results_question_a

