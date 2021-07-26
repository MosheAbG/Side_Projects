library(ggplot2)

#Upload and clean data
dataValues = read.csv("Research_hireexercise_data.csv", TRUE, ",")

UpdatedDataValues = tail(dataValues, -2)


# Create 4 age groups for classifying people
AgeGroup1Total = 0
AgeGroup2Total = 0
AgeGroup3Total = 0
AgeGroup4Total = 0

# Get the number of people in each age group
for(i in 1:nrow(UpdatedDataValues)){
  
  if (as.numeric(UpdatedDataValues$birthyear[i]) >= 1987){
    AgeGroup1Total = AgeGroup1Total + 1
  }
  else if(as.numeric(UpdatedDataValues$birthyear[i]) >= 1972){
    AgeGroup2Total = AgeGroup2Total + 1
  }
  else if(as.numeric(UpdatedDataValues$birthyear[i]) >= 1957){
    AgeGroup3Total = AgeGroup3Total + 1
  } else {
    
    AgeGroup4Total = AgeGroup4Total + 1
  }
  
}


#Sorting values by birth year, so excel file could be looped through in order
newData = UpdatedDataValues[order(UpdatedDataValues$birthyear),]

#The 10 tested categories will be split into 2 groups of 5
#Based on the activity risk level and/or if the activity is essential
Data65Plus = newData[1:AgeGroup4Total,]
keeps = c("whenready_1","whenready_2","whenready_5","whenready_6","whenready_10" )
newData65 = Data65Plus[keeps]

Data50to64 = newData[(AgeGroup4Total + 1):(AgeGroup4Total + AgeGroup3Total),]
newData50to64 = Data50to64[keeps]

Data35to49 = newData[(AgeGroup4Total + AgeGroup3Total + 1):(AgeGroup4Total + AgeGroup3Total + AgeGroup2Total),]
newData35to49 = Data35to49[keeps]

Data34Under = newData[(AgeGroup4Total + AgeGroup3Total + AgeGroup2Total + 1):nrow(UpdatedDataValues),]
newData34Under = Data34Under[keeps]

#Create a function determining when people will feel comfortable resuming essential/low risk activities
Essentials = function(AgeMat){
  
  
  Ready = 0
  AlmostReady = 0 # 1 -3 months
  Waiting = 0 #3-6 months
  MoreThan6 = 0 #More than 6 months
  Vaccine = 0 #When Vaccine ready or virus disappeared
  NotReady = 0 # Don't foresee a time
  NotApplicable = 0 # Not Applicable
  for(i in 1:5){
    for(j in 1:nrow(AgeMat)){
      if(AgeMat[j,i] == "I am already ready or doing it "){
        Ready = Ready + 1
      }
      if(AgeMat[j,i] == "		In 1-3 months"){
        AlmostReady = AlmostReady + 1
      }
      if(AgeMat[j,i] == "		In 3-6 months"){
        Waiting = Waiting + 1
      }
      if(AgeMat[j,i] == "		In more than 6 months"){
        MoreThan6 = MoreThan6 + 1
      }
      if(AgeMat[j,i] == "		When a vaccine will be ready or the virus will have disappeared"){
        Vaccine = Vaccine + 1
      }
      if(AgeMat[j,i] == "		I don't foresee a time when I will be ready to do it again"){
        NotReady = NotReady + 1
      }
      if(AgeMat[j,i] == "Not applicable for me"){
        NotApplicable = NotApplicable + 1
      }
    }
  }
  return(list(Ready,AlmostReady,Waiting,MoreThan6,Vaccine,NotReady,NotApplicable))
}




#For each age group calculate the percent of people who feel comfortable doing activities at different times
#over 65
TotalOver65 = 0
for(i in 1:7){
  TotalOver65 = Essentials(newData65)[[i]] + TotalOver65
}
FinalOver65 = c()
for(i in 1:7){
  FinalOver65[i] = Essentials(newData65)[[i]] / TotalOver65 * 100
}

#50 to 64
Total50to64 = 0
for(i in 1:7){
  Total50to64 = Essentials(newData50to64)[[i]] + Total50to64
}
Final50to64 = c()
for(i in 1:7){
  Final50to64[i] = Essentials(newData50to64)[[i]] / Total50to64 * 100
}

#35 to 49
Total35to49 = 0
for(i in 1:7){
  Total35to49 = Essentials(newData35to49)[[i]] + Total35to49
}
Final35to49 = c()
for(i in 1:7){
  Final35to49[i] = Essentials(newData35to49)[[i]] / Total35to49 * 100
}

#34 And Under
Total34Under = 0
for(i in 1:7){
  Total34Under = Essentials(newData34Under)[[i]] + Total34Under
}
Final34Under = c()
for(i in 1:7){
  Final34Under[i] = Essentials(newData34Under)[[i]] / Total34Under * 100
}

#Set up data and format the axis for the plot
Outlook = rep(c("Ready", "1-3 Months", "3-6 months", "more than 6 months", "Vaccine", "Not Ready", "NA"), 4)
results= c(FinalOver65, Final50to64, Final35to49, Final34Under)
AgeGroups = c(rep("65 +", 7), rep("50-64", 7), rep("35-49", 7), rep("18-34", 7))
AllData = data.frame(AgeGroups)

#Plot
ggplot(AllData, aes(fill = Outlook,y = results, x = AgeGroups)) + geom_bar(position = "stack", stat = "identity") +
  labs(title = "Peoples' attitiude to different essential or low risk activities",
       x = "Diffrent Age Groups", y = "Percent of people willing to do activities")



# This next part essentially repeats the previous steps, but for high risk and/or non essential activities
#Sorting values by birth year
newData = UpdatedDataValues[order(UpdatedDataValues$birthyear),]

Data65Plus = newData[1:AgeGroup4Total,]
keeps = c("whenready_3","whenready_4","whenready_7","whenready_8","whenready_9" )
newData65 = Data65Plus[keeps]

Data50to64 = newData[(AgeGroup4Total + 1):(AgeGroup4Total + AgeGroup3Total),]
newData50to64 = Data50to64[keeps]

Data35to49 = newData[(AgeGroup4Total + AgeGroup3Total + 1):(AgeGroup4Total + AgeGroup3Total + AgeGroup2Total),]
newData35to49 = Data35to49[keeps]

Data34Under = newData[(AgeGroup4Total + AgeGroup3Total + AgeGroup2Total + 1):nrow(UpdatedDataValues),]
newData34Under = Data34Under[keeps]

NonEssentials = function(AgeMat){
  
  Ready = 0
  AlmostReady = 0 # 1 -3 months
  Waiting = 0 #3-6 months
  MoreThan6 = 0 #More than 6 months
  Vaccine = 0 #When Vaccine ready or virus disappeared
  NotReady = 0 # Don't foresee a time
  NotApplicable = 0 # Not Applicable
  for(i in 1:5){
    for(j in 1:nrow(AgeMat)){
      if(AgeMat[j,i] == "I am already ready or doing it "){
        Ready = Ready + 1
      }
      if(AgeMat[j,i] == "		In 1-3 months"){
        AlmostReady = AlmostReady + 1
      }
      if(AgeMat[j,i] == "		In 3-6 months"){
        Waiting = Waiting + 1
      }
      if(AgeMat[j,i] == "		In more than 6 months"){
        MoreThan6 = MoreThan6 + 1
      }
      if(AgeMat[j,i] == "		When a vaccine will be ready or the virus will have disappeared"){
        Vaccine = Vaccine + 1
      }
      if(AgeMat[j,i] == "		I don't foresee a time when I will be ready to do it again"){
        NotReady = NotReady + 1
      }
      if(AgeMat[j,i] == "Not applicable for me"){
        NotApplicable = NotApplicable + 1
      }
    }
  }
  return(list(Ready,AlmostReady,Waiting,MoreThan6,Vaccine,NotReady,NotApplicable))
}


#over 65
TotalOver65 = 0
for(i in 1:7){
  TotalOver65 = NonEssentials(newData65)[[i]] + TotalOver65
}
FinalOver65 = c()
for(i in 1:7){
  FinalOver65[i] = NonEssentials(newData65)[[i]] / TotalOver65 * 100
}

#50 to 64
Total50to64 = 0
for(i in 1:7){
  Total50to64 = NonEssentials(newData50to64)[[i]] + Total50to64
}
Final50to64 = c()
for(i in 1:7){
  Final50to64[i] = NonEssentials(newData50to64)[[i]] / Total50to64 * 100
}

#35 to 49
Total35to49 = 0
for(i in 1:7){
  Total35to49 = NonEssentials(newData35to49)[[i]] + Total35to49
}
Final35to49 = c()
for(i in 1:7){
  Final35to49[i] = NonEssentials(newData35to49)[[i]] / Total35to49 * 100
}

#34 And Under
Total34Under = 0
for(i in 1:7){
  Total34Under = NonEssentials(newData34Under)[[i]] + Total34Under
}
Final34Under = c()
for(i in 1:7){
  Final34Under[i] = NonEssentials(newData34Under)[[i]] / Total34Under * 100
}


Outlook = rep(c("Ready", "1-3 Months", "3-6 months", "more than 6 months", "Vaccine", "Not Ready", "NA"), 4)
results= c(FinalOver65, Final50to64, Final35to49, Final34Under)
AgeGroups = c(rep("65 +", 7), rep("50-64", 7), rep("35-49", 7), rep("18-34", 7))
AllData = data.frame(AgeGroups)


ggplot(AllData, aes(fill = Outlook,y = results, x = AgeGroups)) + geom_bar(position = "stack", stat = "identity") +
  labs(title = "Peoples' attitiude to different Non-essential or high risk activities",
       x = "Diffrent Age Groups", y = "Percent of people willing to do activities")