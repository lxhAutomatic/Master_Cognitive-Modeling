#data preprocessing
NeuroRDM <- read.table('C:/Users/75581/Desktop/CM/Lab assignment 2/NeuroRDM')
original_data <- read.table('C:/Users/75581/desktop/CM/Lab assignment 2/NeuralResponses')
#Q11
correlations <- c()
for(i in 1:12){
  ns <- rnorm(nrow(original_data)*ncol(original_data), mean = 0, sd = 1)
  noise <- matrix(ns,ncol=100,byrow=TRUE)
  subject <- original_data+noise
  correlations[[i]] <- 1-cor(t(subject))
}
average_subject <- Reduce("+", correlations) / length(correlations)
group_11_1 <- average_subject[lower.tri(average_subject)]
group_11_2 <- NeuroRDM[lower.tri(NeuroRDM)]

t.test(x = group_11_1, y = group_11_2, method = "pearson")
cor.test(x = group_11_1, y = group_11_2, method = "pearson")
plot(group_11_1,group_11_2)

# t-tes result
# data:  group_1 and group_2
# t = -506.1, df = 4293, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -5.804251 -5.759455
# sample estimates:
#  mean of x mean of y 
# 1.001643  6.783497 

# correaltion test result
# data:  group_1 and group_2
# t = 58.229, df = 4184, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6519731 0.6854548
# sample estimates:
#  cor 
# 0.6690533 

#Q11.5
average_subject_animate <- average_subject[1:48,1:48]
neuroRDM_animate <- NeuroRDM[1:48,1:48]
group_11.5_1 <- average_subject_animate[lower.tri(average_subject_animate)]
group_11.5_2 <- neuroRDM_animate[lower.tri(neuroRDM_animate)]
t.test(x = group_11.5_1, y = group_11.5_2, method = "pearson")
cor.test(x = group_11.5_1, y = group_11.5_2, method = "pearson")
plot(group_11.5_1,group_11.5_2)

#Q12
original_animate <- original_data[1:48,]
NeuroRDM_animate <- NeuroRDM[1:48,1:48]
correlations_2 <- c()
for(i in 1:12){
  ns <- rnorm(nrow(original_animate)*ncol(original_animate), mean = 0, sd = 1)
  noise <- matrix(ns,ncol=ncol(original_animate),byrow=TRUE)
  subject <- original_animate+noise
  correlations_2[[i]] <- 1-cor(t(subject))
}
subject_cor_average_animate <- Reduce("+", correlations_2) / length(correlations_2)

group_12_1 <- subject_cor_average_animate[lower.tri(subject_cor_average_animate)]
group_12_2 <- NeuroRDM_animate[lower.tri(NeuroRDM_animate)]

t.test(x=group_12_1,y=group_12_2,method = "pearson")
cor.test(x=group_12_1,y=group_12_2,method = "pearson")
plot(group_12_1,group_12_2)

#Q13
original_inanimate <- original_data[49:92,]
NeuroRDM_inanimate <- NeuroRDM[49:92,49:92]

correlations_3 <- c()
for(i in 1:12){
  ns <- rnorm(nrow(original_inanimate)*ncol(original_inanimate), mean = 0, sd = 1)
  noise <- matrix(ns,ncol=ncol(original_inanimate),byrow=TRUE)
  subject <- original_inanimate+noise
  correlations_3[[i]] <- 1-cor(t(subject))
}
subject_cor_average_inanimate <- Reduce("+", correlations_3) / length(correlations_3)

group_13_1 <- subject_cor_average_inanimate[lower.tri(subject_cor_average_inanimate)]
group_13_2 <- NeuroRDM_inanimate[lower.tri(NeuroRDM_inanimate)]

t.test(x=group_13_1,y=group_13_2,method = "pearson")
cor.test(x=group_13_1,y=group_13_2,method = "pearson")
plot(group_13_1,group_13_2)

#Q14 data preprocessing
library(ggplot2)
library(reshape2)
BehaviourRDM <- read.table('C:/Users/75581/desktop/CM/Lab assignment 2/BehaviourRDM')
colnames(BehaviourRDM) = c(1:92)
BehaviourRDM$row_num <- c(1:92)
melt_BehaviourRDM <- melt(BehaviourRDM, id.vars = c("row_num"))
ggplot(data = melt_BehaviourRDM, aes(x=row_num, y=variable, fill=value)) + geom_tile()

#Q14-1
group_14_1 <- average_subject[lower.tri(average_subject)]
group_14_2 <- BehaviourRDM[lower.tri(BehaviourRDM)]

t.test(x=group_14_1,y=group_14_2,method = "pearson")
cor.test(x=group_14_1,y=group_14_2,method = "pearson")
plot(group_14_1,group_14_2)

#Q14-2
average_subject_animate <- average_subject[1:48,1:48]
BehaviourRDM_animate <- BehaviourRDM[1:48,1:48]
group_14_an_1 <- average_subject_animate[lower.tri(average_subject_animate)]
group_14_an_2 <- BehaviourRDM_animate[lower.tri(BehaviourRDM_animate)]

t.test(x=group_14_an_1,y=group_14_an_2,method = "pearson")
cor.test(x=group_14_an_1,y=group_14_an_2,method = "pearson")
plot(group_14_an_1,group_14_an_2)

#Q14-3 the same as Q12
#original_animate <- original_data[1:48,]
#NeuroRDM_animate <- NeuroRDM[1:48,1:48]

#correlations_2 <- c()
#for(i in 1:12){
#  ns <- rnorm(nrow(original_animate)*ncol(original_animate), mean = 0, sd = 1)
#  noise <- matrix(ns,ncol=ncol(original_animate),byrow=TRUE)
#  subject <- original_animate+noise
#  correlations_2[[i]] <- 1-cor(t(subject))
#}
#average_subject_animate <- Reduce("+", correlations_2) / length(correlations_2)

#group_12_1 <- average_subject_animate[lower.tri(average_subject_animate)]
#group_12_2 <- NeuroRDM_animate[lower.tri(NeuroRDM_animate)]

#t.test(x=group_12_1,y=group_12_2,method = "pearson")
#cor.test(x=group_12_1,y=group_12_2,method = "pearson")
#plot(group_12_1,group_12_2)

#Q14-4
average_subject_inanimate <- average_subject[49:92,49:92]
BehaviourRDM_inanimate <- BehaviourRDM[49:92,49:92]

group_14_in_1 <- average_subject_inanimate[lower.tri(average_subject_inanimate)]
group_14_in_2 <- BehaviourRDM_inanimate[lower.tri(BehaviourRDM_inanimate)]

t.test(x=group_14_in_1,y=group_14_in_2,method = "pearson")
cor.test(x=group_14_in_1,y=group_14_in_2,method = "pearson")
plot(group_14_in_1,group_14_in_2)

#Q15
hmaxRDM <- read.table("'C:/Users/75581/desktop/CM/Lab assignment 2/HmaxRDM")

colnames(hmaxRDM) = c(1:92)
hmaxRDM$row_num <- c(1:92)

melt_hmaxRDM <- melt(hmaxRDM,id.vars = c("row_num"))
ggplot(data = melt_hmaxRDM, aes(x=row_num, y=variable, fill=value)) + geom_tile()

#Q15-1
group_15_1 <- average_subject[lower.tri(average_subject)]
group_15_2 <- hmaxRDM[lower.tri(hmaxRDM)]

cor.test(x=group_15_1,y=group_15_2,method = "pearson")
plot(group_15_1,group_15_2)

#Q15-2
average_subject_animate <- average_subject[1:48,1:48]
hmaxRDM_animate <- hmaxRDM[1:48,1:48]
group_15_an_1 <- average_subject_animate[lower.tri(average_subject_animate)]
group_15_an_2 <- hmaxRDM_animate[lower.tri(hmaxRDM_animate)]

cor.test(x=group_15_an_1,y=group_15_an_2,method = "pearson")
plot(group_15_an_1,group_15_an_2)

#Q15-3
average_subject_inanimate <- average_subject[49:92,49:92]
hmaxRDM_inanimate <- hmaxRDM[49:92,49:92]

group_15_in_1 <- average_subject_inanimate[lower.tri(average_subject_inanimate)]
group_15_in_2 <- hmaxRDM_inanimate[lower.tri(hmaxRDM_inanimate)]

cor.test(x=group_15_in_1,y=group_15_in_2,method = "pearson")
plot(group_15_in_1,group_15_in_2)
