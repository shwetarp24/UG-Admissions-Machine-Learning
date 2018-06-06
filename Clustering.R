setwd('/Users/shwet/Desktop/ML dataset/Fresh/Clustering')
getwd()
TargetVariables <- read.csv("TargetVariables.csv")

library(dplyr)
Admits <-  filter(TargetVariables,TargetVariables$app.final.decision == "Admit" |TargetVariables$app.final.decision == "Conditional" | TargetVariables$app.final.decision == "Defer")
Deny <-  filter(TargetVariables,TargetVariables$app.final.decision == "Deny" |TargetVariables$app.final.decision == "Withdraw" | TargetVariables$app.final.decision == "NA" )

ameanGPAs <- mean(Admits$hs.gpa, na.rm = TRUE)
ameanGPAunweighted <- mean(Admits$hs.gpa.unweighted, na.rm = TRUE)
ameanACTcomp <- mean(Admits$score.act.composite,na.rm = TRUE)
ameanACTenglish <- mean(Admits$score.act.english, na.rm = TRUE)
ameanACTengwriting <- mean(Admits$score.act.english.writing, na.rm = TRUE)
ameanACTmath <- mean(Admits$score.act.math, na.rm = TRUE)
ameanACTreading <- mean(Admits$score.act.reading, na.rm = TRUE)
ameanACTscience <- mean(Admits$score.act.science, na.rm = TRUE)
ameanEQmath <- mean(Admits$score.equivalent.math, na.rm = TRUE)
ameanEQverbal <- mean(Admits$score.equivalent.verbal, na.rm = TRUE)
ameanEQwriting <- mean(Admits$score.equivalent.writing, na.rm = TRUE)
ameanMAXRAWSATtot <- mean(Admits$score.max.raw.sat.total, na.rm = TRUE)
ameanMAXSATmath <- mean(Admits$score.max.sat.math, na.rm = TRUE)
ameanMAXSATtotal <- mean(Admits$score.max.sat.total, na.rm = TRUE)
ameanMAXSATverbal <- mean(Admits$score.max.sat.verbal, na.rm = TRUE)
ameanMaxSATwriting <- mean(Admits$score.max.sat.writing, na.rm = TRUE)
ameanPGPA <- mean(Admits$score.pgpa, na.rm = TRUE)
ameanSATessay <- mean(Admits$score.sat.essay, na.rm = TRUE)
ameanSATmc <- mean(Admits$score.sat.mc, na.rm = TRUE)
ameanSATmath <- mean(Admits$score.sat.math, na.rm = TRUE)
ameanSATtotalvm <- mean(Admits$score.sat.total..v.m., na.rm = TRUE)
ameanSATverbal <- mean(Admits$score.sat.verbal, na.rm = TRUE)
ameanSATwriting <- mean(Admits$score.sat.writing, na.rm = TRUE)

View(Admits)

Admits$hs.gpa[is.na(Admits$hs.gpa)]<-ameanGPAs
Admits$hs.gpa.unweighted[is.na(Admits$hs.gpa.unweighted)]<-ameanGPAunweighted
Admits$score.act.composite[is.na(Admits$score.act.composite)]<-ameanACTcomp 
Admits$score.act.english[is.na(Admits$score.act.english)]<-ameanACTenglish 
Admits$score.act.english.writing[is.na(Admits$score.act.english.writing)]<-ameanACTengwriting 
Admits$score.act.math[is.na(Admits$score.act.math)]<-ameanACTmath 
Admits$score.act.reading[is.na(Admits$score.act.reading)]<-ameanACTreading 
Admits$score.act.science[is.na(Admits$score.act.science)]<-ameanACTscience 
Admits$score.equivalent.math[is.na(Admits$score.equivalent.math)]<-ameanEQmath 
Admits$score.equivalent.verbal[is.na(Admits$score.equivalent.verbal)]<-ameanEQverbal 
Admits$score.equivalent.writing[is.na(Admits$score.equivalent.writing)]<-ameanEQwriting 
Admits$score.max.raw.sat.total[is.na(Admits$score.max.raw.sat.total)]<-ameanMAXRAWSATtot 
Admits$score.max.sat.math[is.na(Admits$score.max.sat.math)]<-ameanMAXSATmath 
Admits$score.max.sat.total[is.na(Admits$score.max.sat.total)]<-ameanMAXSATtotal 
Admits$score.max.sat.verbal[is.na(Admits$score.max.sat.verbal)]<-ameanMAXSATverbal 
Admits$score.max.sat.writing[is.na(Admits$score.max.sat.writing)]<-ameanMaxSATwriting 
Admits$score.pgpa[is.na(Admits$score.pgpa)]<-ameanPGPA 
Admits$score.sat.essay[is.na(Admits$score.sat.essay)]<-ameanSATessay 
Admits$score.sat.mc[is.na(Admits$score.sat.mc)]<-ameanSATmc 
Admits$score.sat.math[is.na(Admits$score.sat.math)]<-ameanSATmath 
Admits$score.sat.total..v.m.[is.na(Admits$score.sat.total..v.m.)]<-ameanSATtotalvm 
Admits$score.sat.verbal[is.na(Admits$score.sat.verbal)]<-ameanSATverbal 
Admits$score.sat.writing[is.na(Admits$score.sat.writing)]<-ameanSATwriting

Admits$hs.gpa[Admits$hs.gpa == 0]<-ameanGPAs
Admits$hs.gpa.unweighted[Admits$hs.gpa.unweighted == 0 ]<-ameanGPAunweighted
Admits$score.act.composite[Admits$score.act.composite == 0 ]<-ameanACTcomp 
Admits$score.act.english[Admits$score.act.english == 0 ]<-ameanACTenglish 
Admits$score.act.english.writing[Admits$score.act.english.writing == 0 ]<-ameanACTengwriting 
Admits$score.act.math[Admits$score.act.math == 0 ]<-ameanACTmath 
Admits$score.act.reading[Admits$score.act.reading == 0 ]<-ameanACTreading 
Admits$score.act.science[Admits$score.act.science == 0 ]<-ameanACTscience 
Admits$score.equivalent.math[Admits$score.equivalent.math == 0 ]<-ameanEQmath 
Admits$score.equivalent.verbal[Admits$score.equivalent.verbal == 0 ]<-ameanEQverbal 
Admits$score.equivalent.writing[Admits$score.equivalent.writing == 0 ]<-ameanEQwriting 
Admits$score.max.raw.sat.total[Admits$score.max.raw.sat.total== 0]<-ameanMAXRAWSATtot 
Admits$score.max.sat.math[Admits$score.max.sat.math == 0 ]<-ameanMAXSATmath 
Admits$score.max.sat.total[Admits$score.max.sat.total == 0 ]<-ameanMAXSATtotal 
Admits$score.max.sat.verbal[Admits$score.max.sat.verbal == 0 ]<-ameanMAXSATverbal 
Admits$score.max.sat.writing[Admits$score.max.sat.writing == 0 ]<-ameanMaxSATwriting 
Admits$score.pgpa[Admits$score.pgpa == 0 ]<-ameanPGPA 
Admits$score.sat.essay[Admits$score.sat.essay == 0 ]<-ameanSATessay 
Admits$score.sat.mc[Admits$score.sat.mc == 0 ]<-ameanSATmc 
Admits$score.sat.math[Admits$score.sat.math == 0 ]<-ameanSATmath 
Admits$score.sat.total..v.m.[Admits$score.sat.total..v.m. == 0 ]<-ameanSATtotalvm 
Admits$score.sat.verbal[Admits$score.sat.verbal == 0 ]<-ameanSATverbal 
Admits$score.sat.writing[Admits$score.sat.writing == 0 ]<-ameanSATwriting 

#means for deny decisions

dmeanGPAs <- mean(Deny$hs.gpa, na.rm = TRUE)
dmeanGPAunweighted <- mean(Deny$hs.gpa.unweighted, na.rm = TRUE)
dmeanACTcomp <- mean(Deny$score.act.composite,na.rm = TRUE)
dmeanACTenglish <- mean(Deny$score.act.english, na.rm = TRUE)
dmeanACTengwriting <- mean(Deny$score.act.english.writing, na.rm = TRUE)
dmeanACTmath <- mean(Deny$score.act.math, na.rm = TRUE)
dmeanACTreading <- mean(Deny$score.act.reading, na.rm = TRUE)
dmeanACTscience <- mean(Deny$score.act.science, na.rm = TRUE)
dmeanEQmath <- mean(Deny$score.equivalent.math, na.rm = TRUE)
dmeanEQverbal <- mean(Deny$score.equivalent.verbal, na.rm = TRUE)
dmeanEQwriting <- mean(Deny$score.equivalent.writing, na.rm = TRUE)
dmeanMAXRAWSATtot <- mean(Deny$score.max.raw.sat.total, na.rm = TRUE)
dmeanMAXSATmath <- mean(Deny$score.max.sat.math, na.rm = TRUE)
dmeanMAXSATtotal <- mean(Deny$score.max.sat.total, na.rm = TRUE)
dmeanMAXSATverbal <- mean(Deny$score.max.sat.verbal, na.rm = TRUE)
dmeanMaxSATwriting <- mean(Deny$score.max.sat.writing, na.rm = TRUE)
dmeanPGPA <- mean(Deny$score.pgpa, na.rm = TRUE)
dmeanSATessay <- mean(Deny$score.sat.essay, na.rm = TRUE)
dmeanSATmc <- mean(Deny$score.sat.mc, na.rm = TRUE)
dmeanSATmath <- mean(Deny$score.sat.math, na.rm = TRUE)
dmeanSATtotalvm <- mean(Deny$score.sat.total..v.m., na.rm = TRUE)
dmeanSATverbal <- mean(Deny$score.sat.verbal, na.rm = TRUE)
dmeanSATwriting <- mean(Deny$score.sat.writing, na.rm = TRUE)

Deny$hs.gpa[is.na(Deny$hs.gpa)]<-dmeanGPAs
Deny$hs.gpa.unweighted[is.na(Deny$hs.gpa.unweighted)]<-dmeanGPAunweighted
Deny$score.act.composite[is.na(Deny$score.act.composite)]<-dmeanACTcomp 
Deny$score.act.english[is.na(Deny$score.act.english)]<-dmeanACTenglish 
Deny$score.act.english.writing[is.na(Deny$score.act.english.writing)]<-dmeanACTengwriting 
Deny$score.act.math[is.na(Deny$score.act.math)]<-dmeanACTmath 
Deny$score.act.reading[is.na(Deny$score.act.reading)]<-dmeanACTreading 
Deny$score.act.science[is.na(Deny$score.act.science)]<-dmeanACTscience 
Deny$score.equivalent.math[is.na(Deny$score.equivalent.math)]<-dmeanEQmath 
Deny$score.equivalent.verbal[is.na(Deny$score.equivalent.verbal)]<-dmeanEQverbal 
Deny$score.equivalent.writing[is.na(Deny$score.equivalent.writing)]<-dmeanEQwriting 
Deny$score.max.raw.sat.total[is.na(Deny$score.max.raw.sat.total)]<-dmeanMAXRAWSATtot 
Deny$score.max.sat.math[is.na(Deny$score.max.sat.math)]<-dmeanMAXSATmath 
Deny$score.max.sat.total[is.na(Deny$score.max.sat.total)]<-dmeanMAXSATtotal 
Deny$score.max.sat.verbal[is.na(Deny$score.max.sat.verbal)]<-dmeanMAXSATverbal 
Deny$score.max.sat.writing[is.na(Deny$score.max.sat.writing)]<-dmeanMaxSATwriting 
Deny$score.pgpa[is.na(Deny$score.pgpa)]<-dmeanPGPA 
Deny$score.sat.essay[is.na(Deny$score.sat.essay)]<-dmeanSATessay 
Deny$score.sat.mc[is.na(Deny$score.sat.mc)]<-dmeanSATmc 
Deny$score.sat.math[is.na(Deny$score.sat.math)]<-dmeanSATmath 
Deny$score.sat.total..v.m.[is.na(Deny$score.sat.total..v.m.)]<-dmeanSATtotalvm 
Deny$score.sat.verbal[is.na(Deny$score.sat.verbal)]<-dmeanSATverbal 
Deny$score.sat.writing[is.na(Deny$score.sat.writing)]<-dmeanSATwriting 

Deny$hs.gpa[Deny$hs.gpa == 0]<-dmeanGPAs
Deny$hs.gpa.unweighted[Deny$hs.gpa.unweighted == 0]<-dmeanGPAunweighted
Deny$score.act.composite[Deny$score.act.composite == 0]<-dmeanACTcomp 
Deny$score.act.english[Deny$score.act.english == 0]<-dmeanACTenglish 
Deny$score.act.english.writing[Deny$score.act.english.writing == 0]<-dmeanACTengwriting 
Deny$score.act.math[Deny$score.act.math == 0]<-dmeanACTmath 
Deny$score.act.reading[Deny$score.act.reading == 0]<-dmeanACTreading 
Deny$score.act.science[Deny$score.act.science == 0]<-dmeanACTscience 
Deny$score.equivalent.math[Deny$score.equivalent.math == 0]<-dmeanEQmath 
Deny$score.equivalent.verbal[Deny$score.equivalent.verbal == 0]<-dmeanEQverbal 
Deny$score.equivalent.writing[Deny$score.equivalent.writing == 0]<-dmeanEQwriting 
Deny$score.max.raw.sat.total[Deny$score.max.raw.sat.total == 0]<-dmeanMAXRAWSATtot 
Deny$score.max.sat.math[Deny$score.max.sat.math == 0]<-dmeanMAXSATmath 
Deny$score.max.sat.total[Deny$score.max.sat.total == 0]<-dmeanMAXSATtotal 
Deny$score.max.sat.verbal[Deny$score.max.sat.verbal == 0]<-dmeanMAXSATverbal 
Deny$score.max.sat.writing[Deny$score.max.sat.writing == 0]<-dmeanMaxSATwriting 
Deny$score.pgpa[Deny$score.pgpa == 0]<-dmeanPGPA 
Deny$score.sat.essay[Deny$score.sat.essay == 0]<-dmeanSATessay 
Deny$score.sat.mc[Deny$score.sat.mc == 0]<-dmeanSATmc 
Deny$score.sat.math[Deny$score.sat.math == 0]<-dmeanSATmath 
Deny$score.sat.total..v.m.[Deny$score.sat.total..v.m. == 0]<-dmeanSATtotalvm 
Deny$score.sat.verbal[Deny$score.sat.verbal == 0]<-dmeanSATverbal 
Deny$score.sat.writing[Deny$score.sat.writing == 0]<-dmeanSATwriting 

View(Admits)
View(Deny)

Admits$hs.gpa<-round(Admits$hs.gpa, 2)
Admits$hs.gpa.unweighted<-round(Admits$hs.gpa.unweighted, 2)
Admits$score.act.composite<-round(Admits$score.act.composite, 2)
Admits$score.act.english<-round(Admits$score.act.english, 2)
Admits$score.act.english.writing<-round(Admits$score.act.english.writing, 2)
Admits$score.act.math<-round(Admits$score.act.math, 2)
Admits$score.act.reading<-round(Admits$score.act.reading, 2)
Admits$score.act.science<-round(Admits$score.act.science, 2)
Admits$score.equivalent.math<-round(Admits$score.equivalent.math, 2)
Admits$score.equivalent.verbal<-round(Admits$score.equivalent.verbal, 2)
Admits$score.equivalent.writing<-round(Admits$score.equivalent.writing, 2)
Admits$score.max.raw.sat.total<-round(Admits$score.max.raw.sat.total, 2)
Admits$score.max.sat.math<-round(Admits$score.max.sat.math, 2)
Admits$score.max.sat.total<-round(Admits$score.max.sat.total, 2)
Admits$score.max.sat.verbal<-round(Admits$score.max.sat.verbal, 2)
Admits$score.max.sat.writing<-round(Admits$score.max.sat.writing, 2)
Admits$score.pgpa<-round(Admits$score.pgpa, 2)
Admits$score.sat.essay<-round(Admits$score.sat.essay, 2)
Admits$score.sat.mc<-round(Admits$score.sat.mc, 2)
Admits$score.sat.math<-round(Admits$score.sat.math, 2)
Admits$score.sat.total..v.m.<-round(Admits$score.sat.total..v.m., 2)
Admits$score.sat.verbal<-round(Admits$score.sat.verbal, 2)
Admits$score.sat.writing<-round(Admits$score.sat.writing, 2)

Deny$hs.gpa<-round(Deny$hs.gpa, 2)
Deny$hs.gpa.unweighted<-round(Deny$hs.gpa.unweighted, 2)
Deny$score.act.composite<-round(Deny$score.act.composite, 2)
Deny$score.act.english<-round(Deny$score.act.english, 2)
Deny$score.act.english.writing<-round(Deny$score.act.english.writing, 2)
Deny$score.act.math<-round(Deny$score.act.math, 2)
Deny$score.act.reading<-round(Deny$score.act.reading, 2)
Deny$score.act.science<-round(Deny$score.act.science, 2)
Deny$score.equivalent.math<-round(Deny$score.equivalent.math, 2)
Deny$score.equivalent.verbal<-round(Deny$score.equivalent.verbal, 2)
Deny$score.equivalent.writing<-round(Deny$score.equivalent.writing, 2)
Deny$score.max.raw.sat.total<-round(Deny$score.max.raw.sat.total, 2)
Deny$score.max.sat.math<-round(Deny$score.max.sat.math, 2)
Deny$score.max.sat.total<-round(Deny$score.max.sat.total, 2)
Deny$score.max.sat.verbal<-round(Deny$score.max.sat.verbal, 2)
Deny$score.max.sat.writing<-round(Deny$score.max.sat.writing, 2)
Deny$score.pgpa<-round(Deny$score.pgpa, 2)
Deny$score.sat.essay<-round(Deny$score.sat.essay, 2)
Deny$score.sat.mc<-round(Deny$score.sat.mc, 2)
Deny$score.sat.math<-round(Deny$score.sat.math, 2)
Deny$score.sat.total..v.m.<-round(Deny$score.sat.total..v.m., 2)
Deny$score.sat.verbal<-round(Deny$score.sat.verbal, 2)
Deny$score.sat.writing<-round(Deny$score.sat.writing, 2)

Alldata <- merge(Admits, Deny, by=c("applicant.id","app.applicant.response","Date.Difference.Submission...Create","app.fee.status","app.final.decision","Difference.Decision...Submitted","app.major.primary.","app.major.secondary.","app.semester..applied.","app.semester..decision.","app.status","attrib.enroll..banner.","hs.city","hs.country","hs.gpa","hs.gpa.unweighted","hs.state","hs.zip","score.act.composite","score.act.english","score.act.english.writing","score.act.math","score.act.reading","score.act.science","score.equivalent.math","score.equivalent.verbal","score.equivalent.writing","score.max.raw.sat.total","score.max.sat.math","score.max.sat.total","score.max.sat.verbal","score.max.sat.writing","score.pgpa","score.sat.essay","score.sat.mc","score.sat.math","score.sat.total..v.m.","score.sat.verbal","score.sat.writing","tour.attended","yield.probability","funnel"), all = T)
View(Alldata)

data.drop <- c("applicant.id","app.major.secondary.","hs.country","hs.state","hs.zip","yield.probability")
Tobin <-  Alldata[, ! names(Alldata) %in% data.drop, drop = F]

View(Tobin)

#File export to desired location
write.csv(Tobin2, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Clustering/Tobin2.csv")

#Reading files with all string/textual values and allowing to be read as variables
Tobin <- read.csv(file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Clustering/Tobin.csv", stringsAsFactors=TRUE)


#Finding out numbr of levels in a the attributes in consideration
levels(Tobin$app.applicant.response)
levels(Tobin$Date.Difference.Submission...Create)
levels(Tobin$app.fee.status)
levels(Tobin$app.final.decision)
levels(Tobin$Difference.Decision...Submitted)
levels(Tobin$app.major.primary.)
levels(Tobin$app.semester..applied.)
levels(Tobin$app.semester..decision.)
levels(Tobin$app.status)
levels(Tobin$attrib.enroll..banner.)
levels(Tobin$hs.city)
levels(Tobin$hs.gpa)
levels(Tobin$hs.gpa.unweighted)
levels(Tobin$score.act.composite)
levels(Tobin$score.act.english)
levels(Tobin$score.act.english.writing)
levels(Tobin$score.act.math)
levels(Tobin$score.act.reading)
levels(Tobin$score.act.science)
levels(Tobin$score.equivalent.math)
levels(Tobin$score.equivalent.verbal)
levels(Tobin$score.equivalent.writing)
levels(Tobin$score.max.raw.sat.total)
levels(Tobin$score.max.sat.math)
levels(Tobin$score.max.sat.total)
levels(Tobin$score.max.sat.verbal)
levels(Tobin$score.max.sat.writing)
levels(Tobin$score.pgpa)
levels(Tobin$score.sat.essay)
levels(Tobin$score.sat.mc)
levels(Tobin$score.sat.math)
levels(Tobin$score.sat.total..v.m.)
levels(Tobin$score.sat.verbal)
levels(Tobin$score.sat.writing)
levels(Tobin$tour.attended)
levels(Tobin$funnel)

#Categorizing levels into customized lables: Here we are categorizing different majors into their macro levels
levels(Tobin$app.major.primary.)[c(4,5)] <- "1"
levels(Tobin$app.major.primary.)[c(6, 7, 8, 9, 25, 26, 27, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 104, 116, 121, 122)] <- "2"
levels(Tobin$app.major.primary.)[c(1, 29, 30, 31, 66, 78, 84, 85, 86, 103, 109)] <- "3"
levels(Tobin$app.major.primary.)[c(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47)] <- "4"
levels(Tobin$app.major.primary.)[c(17, 18, 19, 20, 21, 22, 23, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 75, 76, 77, 83)] <- "5"
levels(Tobin$app.major.primary.)[c(72, 102, 110, 111, 112, 113, 114, 115)] <- "6"
levels(Tobin$app.major.primary.)[c(63, 67, 71, 120)] <- "7"
levels(Tobin$app.major.primary.)[c(87, 88, 89, 90)] <- "8"
levels(Tobin$app.major.primary.)[c(3, 11, 12, 13, 14, 15, 16, 68, 69, 105, 106, 107)] <- "9"
levels(Tobin$app.major.primary.)[c(2, 24, 28, 64, 70, 73, 74, 79, 80, 81, 82, 91, 108, 117, 118, 119)] <- "10"
levels(Tobin$app.major.primary.)[c(10, 65)] <- "11"
levels(Tobin$app.major.primary.)[c(123)] <- "12"


levels(To)
View(Tobin)


newmeanscoreMAXSATtotal <- mean(Tobin$score.max.raw.sat.total, na.rm = TRUE)
Tobin$score.max.raw.sat.total[is.na(Tobin$score.max.raw.sat.total)]<- newmeanscoreMAXSATtotal
Tobin$score.max.raw.sat.total[Tobin$score.max.raw.sat.total == 0]<- newmeanscoreMAXSATtotal

newmeangpaunweighted <- mean(Tobin$hs.gpa.unweighted, na.rm = TRUE)
Tobin$hs.gpa.unweighted[is.na(Tobin$hs.gpa.unweighted)] <- newmeangpaunweighted
Tobin$hs.gpa.unweighted[Tobin$hs.gpa.unweighted == 0] <- newmeangpaunweighted


newmeanMAXSATwrit <- mean(Tobin$score.max.sat.writing, na.rm = TRUE)
Tobin$score.max.sat.writing[is.na(Tobin$score.max.sat.writing)] <- newmeanMAXSATwrit
Tobin$score.max.sat.writing[Tobin$score.max.sat.writing == 0] <- newmeanMAXSATwrit

newmeanpgpa <- mean(Tobin$score.pgpa, na.rm = TRUE)
Tobin$score.pgpa[is.na(Tobin$score.pgpa)] <- newmeanpgpa
Tobin$score.pgpa[Tobin$score.pgpa == 0] <- newmeanpgpa


#Removing outliers
Tobin<-Tobin[!(Tobin$hs.gpa.unweighted==2085 | Tobin$hs.gpa.unweighted==245),]
Tobin<-Tobin[!(Tobin$score.pgpa>4),]



hist(Tobin$Date.Difference.Submission...Create)
hist(Tobin$Difference.Decision...Submitted)
hist(Tobin$attrib.enroll..banner.)
hist(Tobin$hs.gpa)
hist(Tobin$hs.gpa.unweighted) #
hist(Tobin$score.act.composite)
hist(Tobin$score.act.english)
hist(Tobin$score.act.english.writing)
hist(Tobin$score.act.math)
hist(Tobin$score.act.reading)
hist(Tobin$score.act.science)
hist(Tobin$score.equivalent.math)
hist(Tobin$score.equivalent.verbal)
hist(Tobin$score.equivalent.writing)
hist(Tobin$score.max.raw.sat.total) 
hist(Tobin$score.max.sat.math)
hist(Tobin$score.max.sat.total)
hist(Tobin$score.max.sat.verbal)
hist(Tobin$score.max.sat.writing) 
hist(Tobin$score.pgpa) #
hist(Tobin$score.sat.essay)
hist(Tobin$score.sat.mc)
hist(Tobin$score.sat.math)
hist(Tobin$score.sat.total..v.m.)
hist(Tobin$score.sat.verbal)
hist(Tobin$score.sat.writing)
hist(Tobin$tour.attended)
hist(Tobin$funnel)





sample <- sample.int(n = nrow(Tobin), size = floor(0.80*nrow(Tobin)), replace = F)
train <- Tobin2[sample, ]
test  <- Tobin2[-sample, ]

write.csv(train, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Standardized.csv")
write.csv(test, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/ClusterTest.csv")

standardized <- read.csv(file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Standardized.csv")
View(standardized)




#Clustering: Determining Centroids
wss <- (nrow(clustert2)-1)*sum(apply(clustert2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(clustert2, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Clustering Model
fit <- kmeans(clustert2, 5) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)






















