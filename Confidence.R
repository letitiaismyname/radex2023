#Confidence in Reading Modalities Grouped By Timepoint#

#Load dataset
ConfidenceOverTime <-read.csv('Combined Data - Confidence.csv')
ConfidenceOverTime

#Isolate Before-Event Data (Integers)
ConfidenceBefore <- ConfidenceOverTime$Before.Event
ConfidenceBefore
#Convert to vector
ConfidenceBefore = rep(ConfidenceBefore) 
ConfidenceBefore
#Change 4 score-levels to categorical data and set values as factors
ConfidenceBefore <-factor(ConfidenceBefore, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceBefore

#Make vector of After-Event Data (Integers)
ConfidenceAfter <- ConfidenceOverTime$After.Event
ConfidenceAfter
#Convert to vector
ConfidenceAfter = rep(ConfidenceAfter) 
ConfidenceAfter
#Change 4 score-levels to categorical data and set values as factors
ConfidenceAfter <-factor(ConfidenceAfter, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceAfter

#Set column names in order for Confidence-over-time dataset
ORDERCOT <-c("Group","Before Event", "After Event")
ORDERCOT

#Make a dataframe combining the three columns in order
ConfidenceOverTime <-data.frame(ConfidenceOverTime$Group,ConfidenceBefore, ConfidenceAfter)
ConfidenceOverTime 
#Rename columns for clarity
names(ConfidenceOverTime) <- ORDERCOT
ConfidenceOverTime

#Outdated#ConfidenceOverTime[2:3] <- lapply(ConfidenceOverTime[2:3], levels=1:4)
#ConfidenceOverTime

#Create likert summary data using Before (c2) and After (c3) data and grouping by modality (c1)
COTLikert <-likert(ConfidenceOverTime[,c(2:3)], grouping = ConfidenceOverTime$Group)

#Plot likert using ggplot2 and add question as title
plot(COTLikert) + labs(title = "How confident are you in interpreting:")

###############################################

#Change in Confidence Reading For Each Modality
ConfidenceByModality <-read.csv('Combined Data - Confidence 2.csv')
ConfidenceByModality

#Isolate Ultrasound Data (Integers)
ConfidenceUS <- ConfidenceByModality$Ultrasound
ConfidenceUS
#Convert to vector
ConfidenceUS = rep(ConfidenceUS) 
ConfidenceUS
#Change 4 score-levels to categorical data and set values as factors
ConfidenceUS <-factor(ConfidenceUS, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceUS

#Isolate MRI Data (Integers)
ConfidenceMRI <- ConfidenceByModality$MRI
ConfidenceMRI
#Convert to vector
ConfidenceMRI = rep(ConfidenceMRI) 
ConfidenceMRI
#Change 4 score-levels to categorical data and set values as factors
ConfidenceMRI <-factor(ConfidenceMRI, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceMRI

#Isolate CT Data (Integers)
ConfidenceCT <- ConfidenceByModality$CT
ConfidenceCT
#Convert to vector
ConfidenceCT = rep(ConfidenceCT) 
ConfidenceCT
#Change 4 score-levels to categorical data and set values as factors
ConfidenceCT <-factor(ConfidenceCT, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceCT

#Isolate XRay Data (Integers)
ConfidenceXR <- ConfidenceByModality$Xray
ConfidenceXR
#Convert to vector
ConfidenceXR = rep(ConfidenceXR) 
ConfidenceXR
#Change 4 score-levels to categorical data and set values as factors
ConfidenceXR <-factor(ConfidenceXR, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceXR

#Set Column names in order for Modalities
ORDERMOD <-c("Group","Ultrasound","X-Ray","CT","MRI")
ORDERMOD

#Make a dataframe combining the three columns in order
ConfidenceByModality <-data.frame(ConfidenceByModality$Group,ConfidenceUS, ConfidenceXR, ConfidenceCT, ConfidenceMRI)
ConfidenceByModality
#Rename columns for clarity
names(ConfidenceByModality) <- ORDERMOD
ConfidenceByModality

#Create likert summary data using modalities (c2-c5) data and grouping by event (c1)
CBMLikert <-likert(ConfidenceByModality[,c(2:5)], grouping = ConfidenceByModality$Group) 

#Oudated#CBMItemOrder <-CBMLikert$results$Item
#Plot likert using ggplot2 and add question as title, hide percentages to allow plotting
plot(CBMLikert, plot.percent.low = FALSE, plot.percent.neutral = FALSE, plot.percent.high = FALSE) + labs(title = "How confident are you in interpreting:")


