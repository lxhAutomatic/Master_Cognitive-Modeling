source("C:/Users/75581/Desktop/CM/Lab 1/drivingModel.R")
library(dplyr)

##global parameters

## times for dialing
standardSingleTaskKeyPressTimes <- c(400,400,400,400,400,400,400,400,400,400,400)   #digit times needed per keypress at that specific position (note: normalized for chunk retrieval time at digits 1 and 6 --- a retrieval cost would come on top of this)

se <- function(x) sd(x)/sqrt(length(x))
compute_avg_keypress_per_person <- function(data){
  data$diffs = 0
  data$diffs[2:nrow(data)] = diff(data$timeRelativeToTrialStart)
  data$diffs[data$phoneNrLengthBeforeKeyPress == 0] = data$timeRelativeToTrialStart[data$phoneNrLengthBeforeKeyPress == 0]
  per_participant_per_key_data <- data %>% group_by(pp, trial, phoneNrLengthBeforeKeyPress) %>% summarise("adjMeanDiffs"=mean(diffs))
  per_participant_per_key_data <- per_participant_per_key_data %>% group_by(pp, phoneNrLengthBeforeKeyPress) %>% 
    summarise("meanDiffs"=mean(adjMeanDiffs), "errorDiffs"=se(adjMeanDiffs))
  per_participant_per_key_data
}

compute_avg_digit_per_person <- function(data){
  data$diffs = 0
  data$diffs[2:nrow(data)] = diff(data$timeRelativeToTrialStart)
  data$diffs[data$phoneNrLengthBeforeKeyPress == 0] = data$timeRelativeToTrialStart[data$phoneNrLengthBeforeKeyPress == 0]
  data = data[data$Event2 != 190,]
  per_participant_per_digit_per_trial_data <- data %>% group_by(pp, trial, Event2, phoneNrLengthBeforeKeyPress) %>% summarise("adjMeanDiffs"=mean(diffs))
  per_participant_per_digit_per_trial_data
}

keyPressDataWithLaneDeviation = read.csv("C:/Users/75581/Desktop/CM/Lab 1/keyPressDataWithLaneDeviation.csv")
# TODO: Think about the part of experiment filter
filtered_data = keyPressDataWithLaneDeviation %>% filter(partOfExperiment == "singleDialing2") %>% 
  filter(typingErrorMadeOnTrial==0) %>% 
  filter(Event1 == "Keypress")

per_participant_per_key_data = compute_avg_keypress_per_person(filtered_data)
per_participant_data = per_participant_per_key_data %>% group_by(pp) %>% summarise("meanDiffPP"=mean(meanDiffs))
per_participant_data = per_participant_data[order(per_participant_data$meanDiffPP),]


typing_speed_average = per_participant_data %>% summarise("val"=mean(meanDiffPP))
typing_speed_slowest = per_participant_data %>% summarise("val"=min(meanDiffPP))
typing_speed_fastest = per_participant_data %>% summarise("val"=max(meanDiffPP))

per_participant_per_digit_per_trial_data = compute_avg_digit_per_person(filtered_data)
typing_speed_average_per_digit = per_participant_per_digit_per_trial_data %>% group_by(Event2) %>% 
  summarise("meanDiffs"=mean(adjMeanDiffs), "stdDiffs"=sd(adjMeanDiffs))
typing_speed_average_per_digit = per_participant_per_digit_per_trial_data %>% group_by(Event2, phoneNrLengthBeforeKeyPress) %>%
  summarise("meanDiffs"=mean(adjMeanDiffs), "stdDiffs"=sd(adjMeanDiffs))
typing_speed_average_per_digit = typing_speed_average_per_digit[order(typing_speed_average_per_digit$phoneNrLengthBeforeKeyPress),]
typing_speed_average_per_digit$digit = as.integer(substr(typing_speed_average_per_digit$Event2, 7,8))


buildGenerateChunkStrategiesModelbased <- function(num_strategies){
  generateChunkStrategies <- function(phoneNumberLengths){
    all_chunks = 1:phoneNumberLengths
    no_chunks = c()
    natural_uk = c(1,6)
    natural_us = c(1,4,7)
    rand_chunk_switch_numbers = rbinom(num_strategies, phoneNumberLengths-1, 3/10) + 1
    print("=============================")
    print(rand_chunk_switch_numbers)
    strategies = list(all_chunks, no_chunks, natural_us, natural_uk)
    # TODO: Exclude repeating strategies
    # TODO: Use while to fill it up
    for (num_chumks in rand_chunk_switch_numbers){
      chunk_pos = sort(sample(1:phoneNumberLengths, size=num_chumks, replace = FALSE))
      strategies[[length(strategies) + 1]] <- chunk_pos
    }
    strategies
  }
  generateChunkStrategies
}

generateChunkStrategiesSystematic <- function(phoneNumberLengths){
  all_chunks = 1:phoneNumberLengths
  strategies = list()
  for (skip in 1:phoneNumberLengths){
    num_configurations = max(phoneNumberLengths %% skip)
    indices = (all_chunks %% skip)
    for (config in 1:skip){
      selections = indices == (config - 1)
      item = which(selections)
      strategies[[length(strategies) + 1]] <- item
    }
  }
  
  strategies <- append(strategies, list(NULL))
  strategies <- unique(strategies)
  strategies
}

generateChunkStrategiesSpecifics <- function(phoneNumberLengths){
  all_chunks = 1:phoneNumberLengths
  frequent_chunks = c(1,3,5, 7, 9, 11)
  no_chunks = c()
  natural_uk = c(1,6)
  natural_us = c(1,4,7)
  natural_de = c(1,4,8)
  strategies = list(all_chunks, frequent_chunks,  no_chunks, natural_us, natural_uk, natural_de)
  
  
  strategies <- unique(strategies)
  strategies
}

generateChunkStrategiesModelbased <- buildGenerateChunkStrategiesModelbased(5)
# View(generateChunkStrategiesModelbased(11))
# generateChunkStrategiesModelbased(11)
# View(generateChunkStrategiesSystematic(11))
# generateChunkStrategiesSystematic(11
generateChunkStrategiesSpecifics(11)

runAllComplexStrategies <- function(nrSimulations, phoneNumber, generateChunkStrategies, runOneTrial)
{
  
  
  normalPhoneStructure <- c(1,6)  ### indicate at what digit positions a chunk needs to be retrieved (1st and 6th digit)
  phoneStringLength <- 11   ### how many digits does the number have?
  
  
  ### vectors that will contain output of the simulation. These are later used to create 1 table with all values
  keypresses <- c()
  times <- c()
  deviations <- c()
  strats <- c()
  steers <- c()	
  chunkStrategies <- generateChunkStrategies(phoneStringLength)
  
  ### iterate through all strategies
  ## in this simple model we assume that a participant uses a consistent strategy throughout the trial. That is, they only type each time 1 digit, or type 2 digits at a time, or type 3 digits at a time (i.e., all possible ways of 1:phoneStringLength: 1, 2,3,4, ...11)
  for (chunkStrategyIdx in 1: length(chunkStrategies))
  {
    ## quick way of calculating positions to interleave: repeat strategy & multiply with position in vector (e.g., 333*123 = 369 this means: you interleave BEFORE every 3rd digit (333), and there are 3 positions to interleave (1st, 2nd, 3rd, or 123). Therefore you interleave BEFORE digits 3 (3*1), 6 (3*2), and 9 (3*3))
    
    # str = sprintf("============ Round: %d", chunkStrategyIdx)
    # writeLines(str)
    strategy <- unlist(chunkStrategies[chunkStrategyIdx])
    # cat(strategy)
    ### remove last digit, as driver does not interleave after typing the last digit (they are done with the trial :-)  )
    strategy <- strategy[strategy != phoneStringLength]
    
    
    locSteerTimeOptions <- c(1,2,3,4,5,6,7,8,9,10,11,12)
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
        strats <- c(strats, rep(paste(strategy, collapse = " "), nrow(locTab)))
        steers <- c(steers,rep(steerTimes,nrow(locTab)))
        
      }
    }#end of for steerTimes	
    
  }##end of for nr strategies
  
  
  ### now make a new table based on all the data that was collected
  tableAllSamples <- data.frame(keypresses,times,deviations,strats,steers)
  tableAllSamples$strats = factor(tableAllSamples$strats)
  
  View(tableAllSamples)
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
  
  agrResultsMeanDrift
  #### make a plot that visualizes all the strategies: note that trial time is divided by 1000 to get the time in seconds
  with(agrResultsMeanDrift,plot(TrialTime/1000,abs(dev),pch=21,bg="dark grey",col="dark grey",log="x",xlab="Dial time (s)",ylab="Average Lateral Deviation (m)"))
  
  
  ### give a summary of the data	
  #summary(agrResultsMeanDrift$TrialTime)
  
}

buildRunOneTrial <- function(singleTaskKeyPressTimes, singleTaskKeyPressData = NULL) {
  runOneTrial <- function(strategy,nrSteeringUpdates,normalPhoneStructure,phoneStringLength,phoneNumber)   #### strategy stores where participant interleaves
  {
    
    #first make vector variables to store detailed output of the model. Note that each vector has a starting value
    times <- c(0)  
    events <- c("none")  ### events will log what happens, for example a keypress or nothing at all ("none"). Later these can be used to filter out specific events
    drifts <- c(startingPositionInLane)   ### where does the car start in the lane? At the start of a trial at the startingPositionInLan position
    newVelocity <- startvelocity         ### what is the start velocity of the car?
    
    
    ##calculate basic dialing times, to be used later
    dialTimes <- singleTaskKeyPressTimes  ##if there is no interleaving, this is the single-task interkeypress interval time
    # print(singleTaskKeyPressData)
    if(!is.null(singleTaskKeyPressData)){
      dialTimes = round(rnorm(singleTaskKeyPressData$meanDiffs, singleTaskKeyPressData$stdDiffs))
      
      # print("NEW TRIAL AVERAGE")
    }
    print(paste(sprintf("L-%d:", length(dialTimes)), paste(dialTimes, collapse = " ")))
    
    
    ### at chunk positions: add a chunk retrieval cost time to the dial times, as (assumption) this is a time where you are not paying attention to the driving
    for (chunkPosition in normalPhoneStructure)
    {
      dialTimes[chunkPosition] <- dialTimes[chunkPosition] + chunkRetrievalTime
    }	
    
    
    ### now go through the various numbers
    for (digitindex in 1:length(dialTimes))
    {
      print(paste("----------------------------------------- ROUND", digitindex, "---------------------------------------------------------"))
      
      ### determine dial time, so additional costs can be added later
      locDialTime <- dialTimes[digitindex]
      
      print("BEGIN")
      print(locDialTime)
      print("BEGIN-END")
      
      if (length(which(strategy==digitindex)))  ### if this is a position where you switch, then switch
      {
        print("SWITCH")
        ## experience switch cost
        time <- switchCost          
        #switching between dialing & driving
        times <- updateTimestampslist(times, time)   
        #### Run a function that determines at what time points a drift update is made. Note that for the first digit, "times" only contains the value 0
        driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)   
        ### now also calculate the drift for these time intervals
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
        
        
        
        print("SWITCH-END")
      }
      
      print("NORMAL")
      ##now calculate drift for typing a digit (NOTE: this is always done for every digit that is typed, regardless of whether you were retrieving a chunk or not)
      
      time <- locDialTime
      times <- updateTimestampslist(times, time)
      driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)
      newVelocity <- driftOutput[1]
      prevD = length(drifts)
      drifts <- c(drifts,driftOutput[2:length(driftOutput)])
      print(paste(prevD, "------>", length(drifts)))
      events <- c(events,rep("none",(length(driftOutput)-2)))
      events <- c(events,"keypress")
      
      print("NORMAL-END")
      
      
    }  #end for digit index
    # print(singleTaskKeyPressData)
    print(paste(length(times), length(events), length(drifts)))
    # if (length(times) != length(events)) {
    # print(time)
    # print(phoneStringLength)
    # print(phoneNumber)
    # print(times)
    # print(events)
    # print(drifts)
    
    # }
    table <- data.frame(times,events,drifts)
    
    #with(table[table$events == "keypress",],plot(times,drifts,ylim=c(-2,2)))
    
    table ### return the table#
    
  }
  runOneTrial
}



strPhoneNr = "07854325698"
generateChunkStrategiesModelbased <- buildGenerateChunkStrategiesModelbased(5)
num_sim = 10
runAllComplexStrategies(num_sim, strPhoneNr, generateChunkStrategiesSystematic, runOneTrial)
num_sim = 50
runAllComplexStrategies(num_sim, strPhoneNr, generateChunkStrategiesSystematic, runOneTrial)





