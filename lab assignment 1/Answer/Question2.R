# for question A
library(ggplot2)
load("C:\\Users\\75581\\Desktop\\CM\\Lab 1\\tableOfDriftValuesCalibration.Rdata")
data_2_A<-tableOfDriftValuesCalibration[(tableOfDriftValuesCalibration$trialTime>15000 
                                         & tableOfDriftValuesCalibration$trialTime<18000),]

plot1 <- ggplot(data_2_A, aes(x =trialTime, y = posX)) + 
  theme_bw(base_family = "STKaiti", base_size = 9) +
  geom_point(aes(colour = data_2_A$trial)) + 
  labs(title = "graph_2_A")
plot
group_number = as.factor(data_2_A$trial)
ggplot(data_2_A,aes(x=trialTime, y=posX, color=group_number,group=group_number))+geom_line()+labs(x="Trial Time(ms)", y='Lateral Deviation(m)')
#ggplot(data_2_A,aes(x=trialTime,y=posX,color=as.factor(trial),group=as.factor(trial)))+geom_line()

# for question B
group_type<-vector(mode="numeric",length=1220)
x_value<-vector(mode="numeric",length=1220)
y_value<-vector(mode="numeric",length=1220)
for(i in 1:1220)
{
  group_type[i] = (i-1)%/%61 + 1
}

for(i in 1:20)
{
  for(j in 1:61)
  {
    random_value = rnorm(1,0,0.13)
    x_value[(i-1)*61+j] = 50*(j-1)+15000
    if(j%%61==1)
    {
      y_value[(i-1)*61+j]=0
    }
    else
    {
      y_value[(i-1)*61+j]=y_value[(i-1)*61+j-1]+random_value
    }
  }
}
data_2_B<-data.frame(x_value,y_value,group_type)
type = as.factor(data_2_B$group_type)
ggplot(data_2_B,aes(x=x_value,y=y_value,color=type,group=type))+geom_line()+labs(x='Trial Time(ms)',y='Lateral Deviation(m)')

# for question C
hist(x=data_2_A$posX,breaks = 50,freq = F, col = "red", xlab = "Lateral Position(m)", xlim = c(-2,2), main="Actual Human Data")
lines(density(data_2_A$posX))
hist(x=data_2_B$y_value,breaks = 50,freq = F, col = "blue", xlab = "Lateral Position(m)", xlim = c(-2,2),main="Simulated Human Data")
lines(density(data_2_B$y_value))

# for question D
sd_D_1 = sd(data_2_A[,"posX"])
sd_D_2 = sd(data_2_B[,"y_value"])

# for question E
