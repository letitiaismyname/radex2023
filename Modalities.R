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

LikertID <-as.data.frame(PrePost)
LikertOnly <-LikertID[,-1]
lvls<-c(1,2,3,4)
LikertOnly

LikertOnly[] <-  lapply(LikertOnly, factor, levels=lvls)
LikertNA <-LikertOnly[!(is.na(LikertOnly$`PatientTime - PRE`)), ]
LikertNA

LikertSummary <-likert(LikertNA)
LikertSummary
ItemOrder <-LikertSummary$results$Item
likert.bar.plot(LikertSummary, group.order = ItemOrder)


Questions<- read_csv("Combined Data - Pre&Post Questions.csv")
Questions <- Questions %>% mutate_if(is.character, as.factor)
names(Questions)
Questions <- as.data.frame(Questions)
Questions

LikertVectors <-rep(LikertNA)

ORDER <-c("Before Event", "After Event")
ORDER

##############"How confident are you in interpreting: [Ultrasound]"#############
UltrasoundTITLE <-names(Questions[2]) 
UltrasoundTITLE

UltrasoundPRE <-LikertVectors$`Ultrasound - PRE`
UltrasoundPRE
UltrasoundPRE = rep(UltrasoundPRE) 
UltrasoundPRE <-factor(UltrasoundPRE, levels = c(1, 2, 3,4),labels = c("Not at all Confident","Not very Confident","Somewhat Confident","Very Confident"))
UltrasoundPRE

UltrasoundPOST <-LikertVectors$`Ultrasound - POST`
UltrasoundPOST
UltrasoundPOST = rep(UltrasoundPOST) 
UltrasoundPOST <-factor(UltrasoundPOST, levels = c(1, 2, 3,4),labels = c("Not at all Confident","Not very Confident","Somewhat Confident","Very Confident"))
UltrasoundPOST

UltrasoundQUAL <-data.frame(UltrasoundPRE,UltrasoundPOST)
UltrasoundQUAL
names(UltrasoundQUAL) <- ORDER
UltrasoundQUAL

UltrasoundLIKERT <-likert(UltrasoundQUAL)
UltrasoundLIKERT 
likert.bar.plot(UltrasoundLIKERT, group.order = ORDER) + labs(title = UltrasoundTITLE)

#-####################3--------

CT <- LikertNA[,c(5,6)]
names(CT) <- ORDER
LikertCT <- likert(CT)
likert.bar.plot(LikertCT)

MRI <- LikertNA[,c(7,8)]
names(MRI) <- ORDER
LikertMRI <- likert(MRI)
likert.bar.plot(LikertMRI)

XRay <- LikertNA[,c(9,10)]
names(XRay) <- ORDER
LikertXRay <- likert(XRay)
likert.bar.plot(LikertXRay)