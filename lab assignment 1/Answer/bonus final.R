
## Cognitive Model of dialing while driving task for multiple phone number representations
## Developed by Christian P. Janssen of Utrecht University
## This model is described in more detail in Janssen & Brumby (2010, Cognitive Science)
## However, the model is meant as an exercise in class, so not all components are included (specifically: the part that explores *all* strategies)
## If you use this model, please cite the paper: Janssen, C. P., & Brumby, D. P. (2010). Strategic Adaptation to Performance Objectives in a Dual‐Task Setting. Cognitive Science, 34(8), 1548–1560. http://doi.org/10.1111/j.1551-6709.2010.01124.x

## for questions, please contact Christian P. Janssen: c.p.janssen@uu.nl
## www.cpjanssen.nl

library(gtools)
library(ggplot2)
library(plyr)
library(reshape2)
library(miceadds)

##global parameters
{
  ### parameters related to steering
  steeringTimeOptions <- c(1,2,3,4,5,6,7,8,9,10,11,12)    #list op options for how many steering corrections can be made each time that attention is paid to steering (of steeringUpdateTime sec each) (this influences the strategy alternatives)
  steeringUpdateTime <- 250    #in milliseconds
  startingPositionInLane <- 0.27 			#assume that car starts already away from lane centre (in meters)
  
  #parameters for deviations in car drift due the simulator environment: See Janssen & Brumby (2010) page 1555
  gaussDeviateMean <- 0
  gaussDeviateSD <- 0.05 
  #gaussDeviateSD <- 0.13 # original value
  
  #When the car is actively contorlled, we calculate a value using equation (1) in Janssen & Brumby (2010). However, some noise is added on top of this equation to account for variation in human behavior. See Janssen & Brumby (2010) page 1555. Also see function "updateSteering" on how this function is used
  gaussDriveNoiseMean <- 0
  gaussDriveNoiseSD <- 0.02  
  #gaussDriveNoiseSD <- 0.1	#in meter/sec original value
  
  timeStepPerDriftUpdate <- 50 ### msec: what is the time interval between two updates of lateral position?
  
  maxLateralVelocity <- 1.7	#maximum lateral velocity: what is the maximum that you can steer?
  minLateralVelocity <- -1* maxLateralVelocity
  
  startvelocity <- 0 	#a global parameter used to store the lateral velocity of the car
  
  ### all times in milliseconds
  ## times for dialing
  singleTaskKeyPressTimes <- c(344,344,344,344,344,344,344,344,344,344,344) 
  #singleTaskKeyPressTimes <- c(322,303,299,283,295,239,277,315,257,272,239)
  # original value
  #singleTaskKeyPressTimes <- c(400,400,400,400,400,400,400,400,400,400,400)   #digit times needed per keypress at that specific position (note: normalized for chunk retrieval time at digits 1 and 6 --- a retrieval cost would come on top of this)
  
  digitTypeUK <- c("chunk","oth","oth","oth","oth","chunk","oth","oth","oth","oth","oth")  ### is each digit either the start of a chunk or some other digit?
  
  #### parameters related to task switching: see Janssen & Brumby page 1556
  chunkRetrievalTime <- 100    ## extra time needed to retrieve first digit of a chunk (100 msec in Janssen & Brumby 2010). This time cost is ALWAYS incurred
  stateInformationRetrievalTime <- 100 #time required to retrieved state information if the FIRST digit of a sequence of keypresses is not at chunk boundary (100 in Janssen & Brumby paper). This cost is only incurred when you switch at a position that differs from the chunk boundary
  switchCost <- 200      ### Janssen & Brumby 2010: time needed when you switch back to dialing after driving (always incurred when switching)
  
  ####### startTime <- 500 	#msec; time before starting to retrieve digit after start of trial (i.e., after car has been driven for a while)
  
  simpleSetOfStrategyVariations <- TRUE		#set to true if only steeringTimeOptions in the form {2,2,2,2,2}, {4,4,4,4,4},etc are used and not {2,4,2,2,4},etc. That is: if a consistent number of digits is dialed each moment when attention is paid to dialing.
  giveDetailedOuput <- FALSE  ## how detailed is the output; at the individual keypress level, or average per trial?
}

### use this function to analyze the human data if you want to
analysisOfHumanData <- function()
{
  ### load the relevant data files
  load.Rdata("C:/Users/75581/Desktop/CM/Lab 1/keyPressDataWithLaneDeviation.Rdata", "keyPressDataWithLaneDeviation")
  load.Rdata("C:/Users/75581/Desktop/CM/Lab 1/tableOfDriftValuesCalibration.Rdata", "tableOfDriftValuesCalibration")
  
  ### calculate the necessary averages and SDs
  ## Question 1
  {
    no_errors <- subset(keyPressDataWithLaneDeviation, typingErrorMadeOnTrial == 0)
    
    # A
    {
      total_time <- subset(no_errors, no_errors$phoneNrLengthAfterKeyPress == 11)
      steer_dial_mean <- aggregate(total_time$timeRelativeToTrialStart ,by=list(total_time$partOfExperiment),FUN=mean, na.rm=TRUE)
      steer_dial_sd <- aggregate(total_time$timeRelativeToTrialStart,by=list(total_time$partOfExperiment),FUN=sd, na.rm=TRUE)
      steer_dial_se <- steer_dial_sd[2] / sqrt(max(unique(total_time$pp)))
      #no_errors$timeRelativeToTrialStart[with(no_errors, no_errors$phoneNrLengthAfterKeyPress == 11)]
    }
    
    # B
    {
      lateral_deviation_mean <- aggregate(abs(no_errors$lanePosition), by=list(no_errors$partOfExperiment),FUN=mean, na.rm=TRUE)
      lateral_deviation_sd <- aggregate(abs(no_errors$lanePosition), by=list(no_errors$partOfExperiment),FUN=sd, na.rm=TRUE)
      lateral_deviation_se <- lateral_deviation_sd[2] / sqrt(max(unique(no_errors$pp)))
    }
    
    # C
    {
      two_conditions <- subset(no_errors, no_errors$partOfExperiment == "dualDialFocus" | no_errors$partOfExperiment == "dualSteerFocus")
      per_participant <- aggregate(abs(two_conditions$lanePosition), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress), FUN=mean, na.rm=TRUE)
      time_participant <- aggregate(abs(two_conditions$timeRelativeToTrialStart), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress), FUN=mean, na.rm=TRUE)
      per_participant <- cbind.data.frame(per_participant, time_participant[,3]/1000)
      
      pp <- aggregate(abs(two_conditions$lanePosition), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress, two_conditions$pp), FUN=mean, na.rm=TRUE)
      tp <- aggregate(abs(two_conditions$timeRelativeToTrialStart), by=list(two_conditions$partOfExperiment, two_conditions$phoneNrLengthAfterKeyPress, two_conditions$pp), FUN=mean, na.rm=TRUE)
      ppt <- cbind.data.frame(pp, tp[,3]/1000)
      
      g1 <- subset(ppt, ppt$Group.1 == "dualDialFocus")
      min_g1 <- c()
      max_g1 <- c()
      min_g2 <- c()
      max_g2 <- c()
      mean_g1 <- c()
      sd_g1 <- c()
      mean_g2 <- c()
      sd_g2 <- c()
      
      for(i in 0:12){
        min_g1[i+1] <- min(subset(g1, Group.2 == i)[,4])
        max_g1[i+1] <- max(subset(g1, Group.2 == i)[,4])
        mean_g1[i+1] <- mean(subset(g1, Group.2 == i)[,4])
        sd_g1[i+1] <- sd(subset(g1, Group.2 == i)[,4])
      }
      
      g2 <- subset(ppt, ppt$Group.1 == "dualSteerFocus")
      for(i in 0:12){
        min_g2[i+1] <- min(subset(g2, Group.2 == i)[,4])
        max_g2[i+1] <- max(subset(g2, Group.2 == i)[,4])
        mean_g2[i+1] <- mean(subset(g2, Group.2 == i)[,4])
        sd_g2[i+1] <- sd(subset(g2, Group.2 == i)[,4])
      }
      min_val <- c()
      max_val <- c()
      mean_val <- c()
      sd_val <- c()
      for(i in 0:12){
        min_val[2*i + 1] <- min_g1[i+1]
        min_val[2*i + 2] <- min_g2[i+1]
        max_val[2*i + 1] <- max_g1[i+1]
        max_val[2*i + 2] <- max_g2[i+1]
        mean_val[2*i + 1] <- mean_g1[i+1]
        mean_val[2*i + 2] <- mean_g2[i+1]
        sd_val[2*i + 1] <- sd_g1[i+1]
        sd_val[2*i + 2] <- sd_g2[i+1]
      }
      
      se_val <- c()
      se_val <- sd_val / sqrt(12)
      names(per_participant)[1] <- "Condition"
      ggplot(per_participant, aes(per_participant$`time_participant[, 3]`, per_participant$x, group = (Condition), colour = Condition)) + 
        geom_line() + geom_point(aes(shape = Condition)) + geom_errorbar(aes(ymin = mean_val-se_val , ymax = mean_val+se_val))  + 
        xlab("Dialing Time (sec)") + ylab("Lateral Deviation (m)")
    }
  }
}
analysisOfHumanData_2 <- function()
{
  ## Question 2
  {
    # A
    {
      position_change <- subset(tableOfDriftValuesCalibration, trialTime >= 15000 & trialTime <= 18000)
      pc <- position_change[,c(2,16,17)]
      names(pc)[2] <- "Trial"
      
      ggplot(position_change, aes(trialTime, posX, group = trial, color = trial)) + 
        geom_line() + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
      
      tn <- 1 
      clr <- c()
      clr <- rgb(runif(20),runif(20),runif(20)) 
      while(tn<=20){
        x <- pc$trialTime[pc$Trial==tn]
        y <- pc$posX[pc$Trial==tn]
        if(tn==1){
          plot(x,y, col = clr[tn], type="l",xlab="Trial time (ms)",ylab="Lateral Position (m)",ylim = c(-1,2), xlim = c(15000,18700))
          
        }
        else{
          li <- lines(x,y,col=clr[tn])
        }
        legend(18100, 2, legend=c("Trial 1", "Trial 2", "Trial 3", "Trial 4", "Trial 5", "Trial 6", "Trial 7", "Trial 8", "Trial 9", 
                                  "Trial 10", "Trial 11", "Trial 12", "Trial 13", "Trial 14", "Trial 15", "Trial 16", "Trial 17", 
                                  "Trial 18", "Trial 19", "Trial 20"),
               cex=0.8, col = clr, lty = 1)
        tn=tn+1
      }
    }
    
    # B
    {
      position <- list()
      for (i in 1:20){
        distribution <- rnorm(60, mean = 0, sd = 0.13)
        position[[i]] <- cumsum(tableOfDriftValuesCalibration$posX[with(tableOfDriftValuesCalibration, tableOfDriftValuesCalibration$trial == i)][1:60] + distribution)
      }
      
      time <- c()
      for (j in 1:60){
        time[j] <- c(50*j-50)
      }
      
      position_dataframe <- as.data.frame(time)
      #names(position_dataframe)[1] <- 'time' 
      for (i in 1:length(position)){
        position_dataframe <- cbind.data.frame(position_dataframe, position[[i]])
        names(position_dataframe)[i+1] <- i
      }
      
      position_dataframe <- melt(position_dataframe ,  id.vars = 'time', variable.name = 'series')
      names(position_dataframe)[2] <- "Trial"
      ggplot(position_dataframe, aes(time,value)) + geom_line(aes(colour = Trial)) + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
    }
    
    # C
    {
      human_trial <- hist(position_change$posX, xlab = "Lateral position (m)", ylab = "Frequency", xlim = range(-2:3), 
                          ylim = range(0:350), col = "gray", main = "Distribution of car positions (human data)", breaks = seq(-2,3,0.2))
      simulated_trial <- hist(position_dataframe$value, xlab = "Lateral position (m)", ylab = "Frequency", xlim = range(-2:3), 
                              ylim = range(0:350), col = "gray", main = "Distribution of car positions (simulated data)", breaks = seq(-2,3,0.2))
    }
    
    # D
    {
      sd_human <- sd(position_change$posX)
      sd_simulated <- sd(position_dataframe$value)
    }
    
    # E
    {
      position_simulated <- list()
      for (i in 1:20){
        distribution_simulated <- rnorm(60, mean = 0, sd = 0.05)
        position_simulated[[i]] <- cumsum(tableOfDriftValuesCalibration$posX[with(tableOfDriftValuesCalibration, tableOfDriftValuesCalibration$trial == i)][1:60] + distribution_simulated)
      }
      
      position_simulated_dataframe <- as.data.frame(time)
      for (i in 1:length(position_simulated)){
        position_simulated_dataframe <- cbind.data.frame(position_simulated_dataframe, position_simulated[[i]])
        names(position_simulated_dataframe)[i+1] <- i
      }
      
      position_simulated_dataframe <- melt(position_simulated_dataframe ,  id.vars = 'time', variable.name = 'series')
      names(position_simulated_dataframe)[2] <- "Trial"
      ggplot(position_simulated_dataframe, aes(time,value)) + geom_line(aes(colour = Trial)) + xlab("Trial time (ms)") + ylab("Lateral Position (m)")
      
      simulated_data_trial <- hist(position_simulated_dataframe$value, xlab = "Lateral position (m)", ylab = "Frequency", xlim = range(-2:3), 
                                   ylim = range(0:400), col = "gray", main = "Distribution of car positions (simulated data)", breaks = seq(-2,3,0.2))
      
      sd_simulated_data <- sd(position_simulated_dataframe$value)
    }
  }
  
  ## Question 3
  {
    keypress_time <- subset(no_errors, partOfExperiment == "singleDialing2")
    keypress_intervals <- aggregate(keypress_time$timeRelativeToTrialStart, by=list(keypress_time$phoneNrLengthAfterKeyPress, keypress_time$pp), FUN=mean, na.rm=TRUE)
    names(keypress_intervals) <- c("Digit", "Participant", "Time")
    ki <- c()
    for(i in 1:(length(keypress_intervals$Time)-1)){
      ki[i] <- keypress_intervals$Time[i+1] - keypress_intervals$Time[i]
    }
    ki[156] = 0
    keypress_intervals <- cbind.data.frame(keypress_intervals, ki)
    intervals <- subset(keypress_intervals, Digit != 12)
    average_keypress_time <- aggregate(intervals$ki, by=list(intervals$Participant), FUN=mean, na.rm=TRUE)
    new_keypress_time <- mean(average_keypress_time$x)
  }
}

analysisOfHumanData()

##new function
runOneTrial <- function(strategy,nrSteeringUpdates,normalPhoneStructure,phoneStringLength,phoneNumber)   #### strategy stores where participant interleaves
{
  
  #first make vector variables to store detailed output of the model. Note that each vector has a starting value
  times <- c(0)  
  events <- c("none")  ### events will log what happens, for example a keypress or nothing at all ("none"). Later these can be used to filter out specific events
  drifts <- c(startingPositionInLane)   ### where does the car start in the lane? At the start of a trial at the startingPositionInLan position
  newVelocity <- startvelocity         ### what is the start velocity of the car?
  
  
  ##calculate basic dialing times, to be used later
  dialTimes <- singleTaskKeyPressTimes  ##if there is no interleaving, this is the single-task interkeypress interval time
  
  ### at chunk positions: add a chunk retrieval cost time to the dial times, as (assumption) this is a time where you are not paying attention to the driving
  for (chunkPosition in normalPhoneStructure)
  {
    dialTimes[chunkPosition] <- dialTimes[chunkPosition] + chunkRetrievalTime
  }	
  
  
  ### now go through the various numbers
  for (digitindex in 1:length(dialTimes))
  {
    
    ### determine dial time, so additional costs can be added later
    locDialTime <- dialTimes[digitindex]
    
    
    if (length(which(strategy== digitindex)))  ### if this is a position where you switch, then switch
    {
      ## experience switch cost
      time <- switchCost          #switching between dialing & driving
      times <- updateTimestampslist(times, time)   #### Run a function that determines at what time points a drift update is made. Note that for the first digit, "times" only contains the value 0
      driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)   ### now also calculate the drift for these time intervals
      newVelocity <- driftOutput[1]  ## determine the new velocity
      drifts <- c(drifts,driftOutput[2:length(driftOutput)])   ### add these drifts to the table
      events <- c(events,rep("switch1",(length(driftOutput)-1)))  ### also update the events
      
      ### after switching you perform corrective driving. For this we use the "updateSteering" function
      lastDrift <- drifts[length(drifts)]
      steerOutput <- updateSteering(newVelocity,nrSteeringUpdates,lastDrift)
      newVelocity <- steerOutput[1]
      drifts <- c(drifts,steerOutput[2:length(steerOutput)])
      events <- c(events,rep("steer",(length(steerOutput)-1)))
      times <- updateTimestampslist(times,(nrSteeringUpdates* steeringUpdateTime))
      
      
      
      ### now switch back to dialing the number (using the drift parameters for distracted driving). First, you incur some time due to switching from driving to dialing
      time <- switchCost          #first: incur a switch cost for switching between dialing to driving
      times <- updateTimestampslist(times, time)
      driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)
      newVelocity <- driftOutput[1]
      drifts <- c(drifts,driftOutput[2:length(driftOutput)])
      events <- c(events,rep("switch2",(length(driftOutput)-1)))
      
      
      
      
      #### if you are NOT switching at a chunk boundary (i.e., at one of the indexes of normalPhoneStructure), then experience additional retrieval cost. This is again time that you are distracted
      if(length(which(normalPhoneStructure == digitindex)) ==0)
      {
        locDialTime <- locDialTime + stateInformationRetrievalTime
      }	
      
      
      
    }
    
    ##now calculate drift for typing a digit (NOTE: this is always done for every digit that is typed, regardless of whether you were retrieving a chunk or not)
    time <- locDialTime
    times <- updateTimestampslist(times, time)
    driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)
    newVelocity <- driftOutput[1]
    drifts <- c(drifts,driftOutput[2:length(driftOutput)])
    events <- c(events,rep("none",(length(driftOutput)-2)))
    events <- c(events,"keypress")
    
    
    
  }  #end for digit index
  
  
  table <- data.frame(times,events,drifts)
  
  #with(table[table$events == "keypress",],plot(times,drifts,ylim=c(-2,2)))
  
  table ### return the table
  
}

#### Main function to run the code. For example: runAllSimpleStrategies(5,"07854325698") will run 5 simulations for each (simple) strategy on the phone number to the right. The default assumption is that the chunk boundary is between the 5th and 6th digit
runAllSimpleStrategies <- function(nrSimulations,phoneNumber)
{
  normalPhoneStructure <- c(1,6)  ### indicate at what digit positions a chunk needs to be retrieved (1st and 6th digit)
  phoneStringLength <- 11   ### how many digits does the number have?
  
  
  ### vectors that will contain output of the simulation. These are later used to create 1 table with all values
  keypresses <- c()
  times <- c()
  deviations <- c()
  strats <- c()
  steers <- c()	
  
  ### iterate through all strategies
  ## in this simple model we assume that a participant uses a consistent strategy throughout the trial. That is, they only type each time 1 digit, or type 2 digits at a time, or type 3 digits at a time (i.e., all possible ways of 1:phoneStringLength: 1, 2,3,4, ...11)
  for (nrDigitsPerTime in 1: phoneStringLength)
  {
    ## quick way of calculating positions to interleave: repeat strategy & multiply with position in vector (e.g., 333*123 = 369 this means: you interleave BEFORE every 3rd digit (333), and there are 3 positions to interleave (1st, 2nd, 3rd, or 123). Therefore you interleave BEFORE digits 3 (3*1), 6 (3*2), and 9 (3*3))
    
    if (nrDigitsPerTime != 11)
    {
      strategy <- rep(nrDigitsPerTime ,floor(phoneStringLength/nrDigitsPerTime))  ### stores at which positions the number is interleaved
      positions <- 1:length(strategy)
      strategy <- strategy * positions
      
      ### remove last digit, as driver does not interleave after typing the last digit (they are done with the trial :-)  )
      strategy <- strategy[strategy != phoneStringLength]
    }
    else
    {
      strategy <- c()	
      
    }
    
    
    locSteerTimeOptions <- steeringTimeOptions
    if (length(strategy) == 0)
    {
      locSteerTimeOptions <- c(0)
    }
    
    
    
    ### now run a trial (runOneTrial) for all combinations of how frequently you update the steering when you are steering (locSteerTimeOptions) and for the nuber of simulations that you want to run for each strategy (nrSimulations)
    for (steerTimes in locSteerTimeOptions)
    {
      for (i in 1:nrSimulations)
      {
        
        ### run the simulation and store the output in a table
        locTab <- runOneTrial(strategy, steerTimes,normalPhoneStructure,phoneStringLength,phoneNumber)
        
        ##only look at rows where there is a keypress
        locTab <- locTab[locTab$events == "keypress",]
        
        ### add the relevant data points to variables that are stored in a final table
        keypresses <- c(keypresses,1:nrow(locTab))
        times <- c(times,locTab$times)
        deviations <- c(deviations,locTab$drifts)
        strats <- c(strats,rep(nrDigitsPerTime,nrow(locTab)))
        steers <- c(steers,rep(steerTimes,nrow(locTab)))
        
      }
    }#end of for steerTimes	
    
  }##end of for nr strategies
  
  
  ### now make a new table based on all the data that was collected
  tableAllSamples <- data.frame(keypresses,times,deviations,strats,steers)
  
  
  #### In the table we collected data for multiple simulations per strategy. Now we want to know the average performane of each strategy.
  #### These aspects are calculated using the "aggregate" function
  
  
  ## calculate average deviation at each keypress (keypresses), for each unique strategy variation (strats and steers)
  agrResults <- with(tableAllSamples,aggregate(deviations,list(keypresses=keypresses, strats= strats, steers= steers),mean))
  agrResults$dev <- agrResults$x
  
  
  ### also calculate the time interval
  agrResults$times <- with(tableAllSamples,aggregate(times,list(keypresses=keypresses, strats= strats, steers= steers),mean))$x
  
  
  ###now calculate mean drift across the trial
  agrResultsMeanDrift <-  with(agrResults,aggregate(dev,list(strats= strats, steers= steers),mean))
  agrResultsMeanDrift$dev <- agrResultsMeanDrift$x
  
  ### and mean trial time
  agrResultsMeanDrift$TrialTime <-  with(agrResults[agrResults$keypresses ==11,],aggregate(times,list( strats= strats, steers= steers),mean))$x	
  
  
  #### make a plot that visualizes all the strategies: note that trial time is divided by 1000 to get the time in seconds
  with(agrResultsMeanDrift,plot(TrialTime/1000,abs(dev),pch=21,bg="dark grey",col="dark grey",log="x",xlab="Dial time (s)",ylab="Average Lateral Deviation (m)"))
  
  
  ### give a summary of the data	
  summary(agrResultsMeanDrift$TrialTime)
  
}

### function that generates the points at which car data should be collected (specifically: if you know that a keypress happens after a specific time, then find out at what points a drift update occurs, this depends on the ength of "timeStepPerDriftUpdate" (50 msec by default))	
updateTimestampslist <- function(timestampsList, totalTime)
{
  lastTime <- timestampsList[length(timestampsList)]
  newTimes <- cumsum(c(lastTime,rep(timeStepPerDriftUpdate ,trunc(totalTime/timeStepPerDriftUpdate))))[-1]
  
  timestampsList <- c(timestampsList, newTimes)
  
  if (totalTime%%timeStepPerDriftUpdate > 0)
  {
    newTime <- timestampsList[length(timestampsList)] + totalTime%%timeStepPerDriftUpdate
    timestampsList <- c(timestampsList, newTime)
  }
  timestampsList
  
}

### This function calculates how much the car drifts during episodes where the driver/model is not actively driving
calculateLaneDrift <- function(startPositionOfDrift, startVelocityOfDrift, driftTimeInMilliSeconds)
{
  laneDriftList <- c()  ### keep a list of lane positions
  #locVelocity <- velocity	#velocity is a global variable
  
  locVelocity <- startVelocityOfDrift
  
  lastLaneDrift <- startPositionOfDrift
  
  
  for (i in 1:(trunc(driftTimeInMilliSeconds/timeStepPerDriftUpdate)))
  {
    locVelocity <- locVelocity + rnorm(1,gaussDeviateMean,gaussDeviateSD)
    
    ### make sure velocity is not higher than max
    locVelocity <- velocityCheck(locVelocity)
    
    lastLaneDrift <- lastLaneDrift + locVelocity* timeStepPerDriftUpdate / 1000     #velocity is in m/second
    
    
    
    #laneDriftList <- c(laneDriftList, lastLaneDrift)
    laneDriftList <- c(laneDriftList, abs(lastLaneDrift))    ### only absolute values
    
  }
  
  
  
  #now do drift for last few milliseconds (using modulo function)
  locVelocity <-locVelocity + rnorm(1,gaussDeviateMean,gaussDeviateSD)
  ### make sure velocity is not higher than max
  locVelocity <- velocityCheck(locVelocity)
  
  if (driftTimeInMilliSeconds%% timeStepPerDriftUpdate > 0)
  {
    
    lastLaneDrift <- lastLaneDrift + locVelocity*(driftTimeInMilliSeconds%% timeStepPerDriftUpdate)/1000
    
    
    #laneDriftList <- c(laneDriftList, lastLaneDrift)
    laneDriftList <- c(laneDriftList, abs(lastLaneDrift))  ### only absolute values
    
  }
  #velocity <<- locVelocity
  #laneDrift
  
  returnValues <- c(locVelocity, laneDriftList) 
  returnValues
}

##calculates if the car is not accelerating more than it should (maxLateralVelocity) or less than it should (minLateralVelocity)
velocityCheck <- function(localVelocity)
{
  localVelocity <- min(localVelocity, maxLateralVelocity)
  localVelocity <- max(localVelocity, minLateralVelocity)
  
  localVelocity
  
}

##calculates if the car is not accelerating more than it should (maxLateralVelocity) or less than it should (minLateralVelocity)  (done for a vector of numbers)
velocityCheckForVectors <- function(velocityVectors)
{
  
  velocityVectors[which(velocityVectors > maxLateralVelocity)] <- maxLateralVelocity
  velocityVectors[which(velocityVectors < minLateralVelocity)] <- minLateralVelocity
  
  velocityVectors
  
}

### this function is used to update the velocity (and in effect lateral lane position) when the driver/model is actively driving
updateSteering <- function(velocity,nrUpdates,startPosLane)
{
  locDrifts <- c()
  
  localVelocity <- velocity
  
  for (steers in 1: nrUpdates)
  {
    localLanePos <- startPosLane
    
    if (steers > 1)
    {
      localLanePos <- locDrifts[length(locDrifts)]
    }
    
    
    ### update direction every 250 milliseconds. Following equation (1) in Janssen & Brumby (2010)
    updateVelocity <- 0.2617 * localLanePos ^2 + 0.0233* localLanePos - 0.022  #velocity in meter/sec
    updateVelocity <- updateVelocity + rnorm(1, gaussDriveNoiseMean, gaussDriveNoiseSD)    ### a noise value is added for driving (only done once)
    updateVelocity <- velocityCheck(updateVelocity)
    
    ###calculate updates locally (i.e., add some noise to updateVelocity, but do not make it transfer to other values)
    ## calculate using cumsum to save computer time :-)
    
    nrUpdatesOf50Msec <- steeringUpdateTime/timeStepPerDriftUpdate
    velocityVector <- rnorm(nrUpdatesOf50Msec,(updateVelocity + gaussDeviateMean), gaussDeviateSD)
    velocityVector  <- velocityCheckForVectors(velocityVector)
    
    
    directionUpdates <- -1 * velocityVector * 0.050    ##only driving for 0.050 seconds
    newDrifts <- cumsum(c(localLanePos, directionUpdates))
    newDrifts  <- newDrifts[2:length(newDrifts)]
    
    locDrifts <- c(locDrifts , abs(newDrifts))   #### only absolute values
    
  }
  returnValues <- c(updateVelocity,locDrifts)
  
  
}

runAllComplexStrategies <- function(nrSimulations,phoneNumber)
{
  normalPhoneStructure <- c(1,6)  ### indicate at what digit positions a chunk needs to be retrieved (1st and 6th digit)
  phoneStringLength <- 11   ### how many digits does the number have?
  
  ### vectors that will contain output of the simulation. These are later used to create 1 table with all values
  keypresses <- c()
  times <- c()
  deviations <- c()
  strats <- c()
  steers <- c()	
  strategy <- c()
  all_combinations <- c()
  agr_results <- c()
  l <- 1
  
  for (i in 0:(phoneStringLength-1)){
    # calculate all combinations
    all_combinations <- combn(1:(phoneStringLength-1), i, simplify = FALSE)
    strategy <- c(strategy, all_combinations)
  }
  
  for (strategies in strategy){
    
    locSteerTimeOptions <- steeringTimeOptions
    
    for (steerTimes in locSteerTimeOptions)
    {
      for (simulation in 1:nrSimulations)
      {
        if (length(strategies) == 0){
          locSteerTimeOptions <- c(0)
          strategies <- c()
        }
        
        ### run the simulation and store the output in a table
        strategies <- unlist(strategies)
        
        locTab <- runOneTrial(strategies, steerTimes,normalPhoneStructure,phoneStringLength,phoneNumber)
        
        #only look at rows where there is a keypress
        locTab <- locTab[locTab$events == "keypress",]
        
        ### add the relevant data points to variables that are stored in a final table
        keypresses <- c(keypresses,1:nrow(locTab))
        times <- c(times,locTab$times)
        deviations <- c(deviations,locTab$drifts)
        strats <- c(strats,rep(toString(strategies),nrow(locTab)))
        steers <- c(steers,rep(steerTimes,nrow(locTab)))
      }#end of for nrSimulations
      
      agrResults <- data.frame(keypresses,times,deviations,strats,steers)
      agrResultsMeanDrift <- agrResults
      agrResultsMeanDrift$dev<-  with(agrResults,aggregate(deviations,list(strats= strats, steers= steers,keypresses=keypresses), FUN = mean, na.rm = TRUE))$x
      agrResultsMeanDrift$TrialTime <-  with(agrResults[agrResults$keypresses ==11,],aggregate(times,list( strats= strats, steers= steers), FUN = mean, na.rm = TRUE))$x
      agr_matrix <- with(agrResultsMeanDrift,aggregate(dev,list(strats=strats,steers=steers,TrialTime=TrialTime), FUN = mean, na.rm = TRUE))
      colnames(agr_matrix) <- c("strats", "steers", "TrialTime","dev")
      
      agr_results <- rbind(agr_results,agr_matrix)
      keypresses <- c()
      times <- c()
      deviations <- c()
      strats <- c()
      steers <- c()
      
    }#end of for steerTimes
    
    l <- l + 1
  } # end of for strategy
  
  with(agr_results,plot(TrialTime/1000,abs(dev),pch=21,bg="dark grey",col="dark grey",log="x",xlab="Dial time (s)",ylab="Average Lateral Deviation (m)"))
  
  return(agr_results)
}

# runAllComplexStrategies models
{
  s1 <- runAllComplexStrategies(1, "07854325698")
  s5 <- runAllComplexStrategies(5, "07854325698")
  s10 <- runAllComplexStrategies(10, "07854325698")
  s50 <- runAllComplexStrategies(50, "07854325698")
  #s100 <- runAllComplexStrategies(100, "07854325698")
  #s200 <- runAllComplexStrategies(200, "07854325698")
}

# plots
plot_complex <- function(complex_model, y_upper, x_leg, y_leg, analysis = FALSE){
  
  condition_1_model <- complex_model$TrialTime[with(complex_model, complex_model$strats == 5)]
  condition_2_model <- complex_model$dev[with(complex_model, complex_model$strats == 5)]
  conditions_model <- data.frame()
  conditions_model <- cbind.data.frame(condition_1_model, condition_2_model)
  
  b <- c(3, complex_model$TrialTime/1000)
  c <- c(7, abs(complex_model$dev))
  
  plot(b,c,pch=21,
       bg="grey",col="grey", log="x",
       ylim = c(0,y_upper),
       xlab="Dial time (s)",ylab="Average Lateral Deviation (m)")
  if(analysis){
    
    lateral_deviation_strategy <- subset(complex_model, (complex_model$dev < lateral_deviation_mean$x[2] + lateral_deviation_se$x[2] & 
                                                           complex_model$dev > lateral_deviation_mean$x[2] - lateral_deviation_se$x[2]))
    
    time_strategy <- subset(lateral_deviation_strategy, (lateral_deviation_strategy$TrialTime < steer_dial_mean$x[2] + steer_dial_se$x[2] & 
                                                           lateral_deviation_strategy$TrialTime > steer_dial_mean$x[2] - steer_dial_se$x[2]))
    
    points(time_strategy$TrialTime/1000, abs(time_strategy$dev), pch=18, bg="red",col="red")
    legend(x_leg, y_leg, legend=c("Dialing-focus data", "Steering-focus data", "Model alternative", "Chunk interleaving only strategy", "Strategy inside error bars (human data)"), 
           pch=c(0,5,21,4,18), cex=0.8)
  }else{
    legend(x_leg, y_leg, legend=c("Dialing-focus data", "Steering-focus data", "Model alternative", "Chunk interleaving only strategy"), 
           pch=c(0,5,21,4), cex=0.8)
  }
  points(conditions_model$condition_1_model/1000, abs(conditions_model$condition_2_model), pch=4,
         bg="black",col="black")
  # dialing focus data
  points(steer_dial_mean$x[1]/1000, lateral_deviation_mean$x[1], pch=0,
         bg="black",col="black")
  arrows(steer_dial_mean$x[1]/1000, lateral_deviation_mean$x[1] - lateral_deviation_se$x[1],
         steer_dial_mean$x[1]/1000, lateral_deviation_mean$x[1] + lateral_deviation_se$x[1], 
         length=0.05, angle=90, code=3)
  arrows((steer_dial_mean$x[1] - steer_dial_se$x[1])/1000, lateral_deviation_mean$x[1], 
         (steer_dial_mean$x[1] + steer_dial_se$x[1])/1000, lateral_deviation_mean$x[1], length=0.05, angle=90, code=3)
  # steering focus data
  points(steer_dial_mean$x[2]/1000, lateral_deviation_mean$x[2], pch=5,
         bg="black",col="black")
  arrows(steer_dial_mean$x[2]/1000, lateral_deviation_mean$x[2] - lateral_deviation_se$x[2],
         steer_dial_mean$x[2]/1000, lateral_deviation_mean$x[2] + lateral_deviation_se$x[2], 
         length=0.05, angle=90, code=3)
  arrows((steer_dial_mean$x[2] - steer_dial_se$x[2])/1000, lateral_deviation_mean$x[2], 
         (steer_dial_mean$x[2] + steer_dial_se$x[2])/1000, lateral_deviation_mean$x[2], length=0.05, angle=90, code=3)
  
}

# different models with different conditions
{
  # A: our values for sd (0.05 - 0.02)
  # B: old values for sd (0.13 - 0.1)
  # C: our values for IKI (275)
  # D: old values for IKI (400)
  # E: 10 simulations
  # F: 50 simulations
  
  model_ACE <- runAllComplexStrategies(10, "07854325698")
  model_ACF <- runAllComplexStrategies(50, "07854325698")
  model_ADE <- runAllComplexStrategies(10, "07854325698")
  model_ADF <- runAllComplexStrategies(50, "07854325698")
  model_BCE <- runAllComplexStrategies(10, "07854325698")
  model_BCF <- runAllComplexStrategies(50, "07854325698")
  model_BDE <- runAllComplexStrategies(10, "07854325698")
  model_BDF <- runAllComplexStrategies(50, "07854325698")
  
  # 7-1.4
  plot_complex(model_ACE, 1.4, 7, 1.4)
  # 7-1
  plot_complex(model_ACF, 1, 7, 1)
  # 7.5-1.5
  plot_complex(model_ADE, 1.5, 7.5, 1.5)
  # 7.5-1
  plot_complex(model_ADF, 1, 7.5, 1)
  # 7.5-2.5
  plot_complex(model_BCE, 2.5, 7.5, 2.5)
  # 7.5-2
  plot_complex(model_BCF, 2, 7.5, 2)
  # 7.5-4
  plot_complex(model_BDE, 4, 7.5, 4)
  # 7.5-2
  plot_complex(model_BDF, 2, 7.5, 2)
}

# analysis
{
  lateral_deviation_strategy <- subset(s50, (s50$dev < lateral_deviation_mean$x[2] + lateral_deviation_se$x[2] & 
                                               s50$dev > lateral_deviation_mean$x[2] - lateral_deviation_se$x[2]))
  
  time_strategy <- subset(lateral_deviation_strategy, (lateral_deviation_strategy$TrialTime < steer_dial_mean$x[2] + steer_dial_se$x[2] & 
                                                         lateral_deviation_strategy$TrialTime > steer_dial_mean$x[2] - steer_dial_se$x[2]))
  
  plot_complex(s50, 1, 8, 1, TRUE)
  
  #one_interleave <- nrow(a[a$`as.numeric(time_strategy$strats)` <= 11,])
  #two_interleave <- nrow(a[a$`as.numeric(time_strategy$strats)` >= 12 & a$`as.numeric(time_strategy$strats)` <=56,])
  #three_interleave <- nrow(a[a$`as.numeric(time_strategy$strats)` >= 57,])
  
  #one_per <- (one_interleave / 248)*100
  #two_per <- (two_interleave / 248)*100
  #three_per <- (three_interleave / 248)*100
}

# bonus question
{
  #model_IKI_50 <- runAllComplexStrategies(50, "07854325698")
  #model_IKI_100 <- runAllComplexStrategies(100, "07854325698")
  #model_IKI_200 <- runAllComplexStrategies(200, "07854325698")
  
  #plot_complex(model_IKI_50, 1, 7, 1)
  #plot_complex(model_IKI_100, 1, 7, 1)
  #plot_complex(model_IKI_200, 1, 7, 1)
}


