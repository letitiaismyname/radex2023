#Confidence in Reading Modalities Grouped By Timepoint
ConfidenceOverTime <-read.csv('Combined Data - Confidence.csv')
ConfidenceOverTime

ConfidenceBefore <- ConfidenceOverTime$Before.Event
ConfidenceBefore
ConfidenceBefore = rep(ConfidenceBefore) 
ConfidenceBefore
ConfidenceBefore <-factor(ConfidenceBefore, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceBefore

ConfidenceAfter <- ConfidenceOverTime$After.Event
ConfidenceAfter
ConfidenceAfter = rep(ConfidenceAfter) 
ConfidenceAfter
ConfidenceAfter <-factor(ConfidenceAfter, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceAfter

ORDERCOT <-c("Group","Before Event", "After Event")
ORDERCOT

ConfidenceOverTime <-data.frame(ConfidenceOverTime$Group,ConfidenceBefore, ConfidenceAfter)
ConfidenceOverTime 
names(ConfidenceOverTime) <- ORDERCOT
ConfidenceOverTime

#ConfidenceOverTime[2:3] <- lapply(ConfidenceOverTime[2:3], levels=1:4)
#ConfidenceOverTime
COTLikert <-likert(ConfidenceOverTime[,c(2:3)], grouping = ConfidenceOverTime$Group)
plot(COTLikert) + labs(title = "How confident are you in interpreting:")

#Change in Confidence Reading Each Modality
ConfidenceByModality <-read.csv('Combined Data - Confidence 2.csv')
ConfidenceByModality

ConfidenceUS <- ConfidenceByModality$Ultrasound
ConfidenceUS
ConfidenceUS = rep(ConfidenceUS) 
ConfidenceUS
ConfidenceUS <-factor(ConfidenceUS, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceUS

Ultrasound <- ConfidenceByModality$Ultrasound
Ultrasound
Ultrasound = rep(Ultrasound) 
Ultrasound
Ultrasound <-factor(Ultrasound, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
Ultrasound

ConfidenceMRI <- ConfidenceByModality$MRI
ConfidenceMRI
ConfidenceMRI = rep(ConfidenceMRI) 
ConfidenceMRI
ConfidenceMRI <-factor(ConfidenceMRI, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceMRI

ConfidenceCT <- ConfidenceByModality$CT
ConfidenceCT
ConfidenceCT = rep(ConfidenceCT) 
ConfidenceCT
ConfidenceCT <-factor(ConfidenceCT, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceCT

ConfidenceXR <- ConfidenceByModality$Xray
ConfidenceXR
ConfidenceXR = rep(ConfidenceXR) 
ConfidenceXR
ConfidenceXR <-factor(ConfidenceXR, levels = c(1, 2, 3,4),labels = c("Not at all confident","Not very confident","Somewhat confident","Very confident"))
ConfidenceXR

ORDERMOD <-c("Group","Ultrasound","X-Ray","CT","MRI")
ORDERMOD

ConfidenceByModality <-data.frame(ConfidenceByModality$Group,ConfidenceUS, ConfidenceXR, ConfidenceCT, ConfidenceMRI)
ConfidenceByModality
names(ConfidenceByModality) <- ORDERMOD
ConfidenceByModality

CBMLikert <-likert(ConfidenceByModality[,c(2:5)], grouping = ConfidenceByModality$Group) 
CBMItemOrder <-CBMLikert$results$Item
plot(CBMLikert, plot.percent.low = FALSE, plot.percent.neutral = FALSE, plot.percent.high = FALSE) + labs(title = "How confident are you in interpreting:")




