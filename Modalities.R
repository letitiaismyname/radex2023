library(readr)

#Set column values as characters or factors
PrePost <- read_csv("Combined Data - Pre&Post Likert.csv", col_types = list(
    `ID` = col_character(),
    `Familiar - PRE` = col_factor(),
    `Familiar - POST` = col_factor(),
    `Ultrasound - PRE` = col_factor(),
    `Ultrasound - POST` = col_factor(),
    `CT - PRE` = col_factor(),
    `CT - POST` = col_factor(),
    `MRI - PRE` = col_factor(),
    `MRI - POST` = col_factor(),
    `X-ray - PRE` = col_factor(),
    `X-ray - POST` = col_factor(),
    `PatientTime - PRE` = col_factor(),
    `PatientTime - POST` = col_factor(),
    `Interest - PRE` = col_factor(),
    `Interest - POST` = col_factor(),
    `Consider - PRE` = col_factor(),
    `Consider - POST` = col_factor(),
    `SeeSelf - PRE` = col_factor(),
    `SeeSelf  - POST` = col_factor(),
    `Barriers - PRE` = col_factor(),
    `Barriers - POST` = col_factor(),
    `Mentor - PRE` = col_factor(),
    `Mentor - POST` = col_factor()))
PrePost

#Convert to dataframe
LikertID <-as.data.frame(PrePost)
#Remove ID column
LikertOnly <-LikertID[,-1]
#Set levels
lvls<-c(1,2,3,4)
LikertOnly

#Apply levels to factor columns
LikertOnly[] <-  lapply(LikertOnly, factor, levels=lvls)
#Ignore blank/NA row
LikertNA <-LikertOnly[!(is.na(LikertOnly$`PatientTime - PRE`)), ]
LikertNA

#Make likert dataset
LikertSummary <-likert(LikertNA)
LikertSummary

#Set item order to enforce not alphabetical when plotting
ItemOrder <-LikertSummary$results$Item
ItemOrder
#Test plot that all results are in order
likert.bar.plot(LikertSummary, group.order = ItemOrder)

#############Supporting Info Set-up###################

#Column titles using full question text
Questions<- read_csv("Combined Data - Pre&Post Questions.csv")
#Convert to factors
Questions <- Questions %>% mutate_if(is.character, as.factor)
#Isolate questions as column names
names(Questions)
#Make a dataframe for future use, will include a NA row
Questions <- as.data.frame(Questions)
Questions

#Covert Likert data columns to individual vectors
LikertVectors <-rep(LikertNA)

#Create order and column names for future
ORDER <-c("Before Event", "After Event")
ORDER

##############"How confident are you in interpreting: [Ultrasound]"#############

#Use questions dataframe to set title
UltrasoundTITLE <-names(Questions[2]) 
UltrasoundTITLE

####Isolate vector
UltrasoundPRE <-LikertVectors$`Ultrasound - PRE`
UltrasoundPRE
#Convert to factors
UltrasoundPRE = rep(UltrasoundPRE) 
UltrasoundPRE
#Convert to categorical names
UltrasoundPRE <-factor(UltrasoundPRE, levels = c(1, 2, 3,4),labels = c("Not at all Confident","Not very Confident","Somewhat Confident","Very Confident"))
UltrasoundPRE

####Isolate vector
UltrasoundPOST <-LikertVectors$`Ultrasound - POST`
UltrasoundPOST
#Convert to factors
UltrasoundPOST = rep(UltrasoundPOST) 
UltrasoundPOST
#Convert to categorical names
UltrasoundPOST <-factor(UltrasoundPOST, levels = c(1, 2, 3,4),labels = c("Not at all Confident","Not very Confident","Somewhat Confident","Very Confident"))
UltrasoundPOST

#Make dataframe of before and after-event data
UltrasoundQUAL <-data.frame(UltrasoundPRE,UltrasoundPOST)
UltrasoundQUAL
#Rename columns
names(UltrasoundQUAL) <- ORDER
UltrasoundQUAL

#Make likert dataset
UltrasoundLIKERT <-likert(UltrasoundQUAL)
UltrasoundLIKERT 
#Plot likert data, enforce result order with ORDER, insert question as title
likert.bar.plot(UltrasoundLIKERT, group.order = ORDER) + labs(title = UltrasoundTITLE)

##########Rapid Analysis of Other Modalities############

###Rapid data without title, categorical names, or forced (non-alphabetical) order
#Isolate CT data
CT <- LikertNA[,c(5,6)]
#Rename columns
names(CT) <- ORDER
#Make likert data summary
LikertCT <- likert(CT)
#Plot likert data
likert.bar.plot(LikertCT)

###Rapid data without title, categorical names, or forced (non-alphabetical) order
#Isolate MRI data
MRI <- LikertNA[,c(7,8)]
#Rename columns
names(MRI) <- ORDER
#Make likert data summary
LikertMRI <- likert(MRI)
#Plot likert data
likert.bar.plot(LikertMRI)

###Rapid data without title, categorical names, or forced (non-alphabetical) order
#Isolate XRay data
XRay <- LikertNA[,c(9,10)]
#Rename columns
names(XRay) <- ORDER
#Make likert data summary
LikertXRay <- likert(XRay)
#Plot likert data
likert.bar.plot(LikertXRay)
