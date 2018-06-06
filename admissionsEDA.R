setwd('/Users/shwet/Desktop/ML dataset/Fresh')
getwd()
dataset <- read.csv("admissionsdata.csv")

library(dplyr)
SQRNC <-  filter(dataset,dataset$hs.state == "NC" & dataset$hs.country == "UNITED STATES")
SQRNC <-  filter(dataset,dataset$hs.state == "NC" & toupper(dataset$hs.country) == "UNITED STATES")
write.csv(SQRNC, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/SQRnc.csv")

keep <- c("applicant.id", "hs.school.type", "app.completed.date", "app.applicant.response", "app.created.date", "app.date.applied", "app.fee.status", "app.final.decision", "app.final.decision.date", "app.major.primary.", "app.major.secondary.", "app.semester..applied.", "app.semester..decision.", "app.status", "attrib.enroll..banner.", "hs.city", "hs.country", "hs.gpa", "hs.gpa.unweighted", "hs.graduated", "hs.state", "hs.zip", "score.act.composite", "score.act.english", "score.act.english.writing", "score.act.math", "score.act.reading", "score.act.science", "score.equivalent.math", "score.equivalent.verbal", "score.equivalent.writing", "score.max.raw.sat.total", "score.max.sat.math", "score.max.sat.total", "score.max.sat.verbal", "score.max.sat.writing", "score.pgpa", "score.sat.essay", "score.sat.mc", "score.sat.math", "score.sat.total..v.m.", "score.sat.verbal", "score.sat.writing", "tour.attended", "yield.probability")
toanalyse <- SQRNC[, names(SQRNC) %in% keep]
toanalyse[toanalyse == ""] <- NA
toanalyse[toanalyse == " "] <- NA
write.csv(toanalyse, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/toanalyse.csv")

#Building the funnel : target variable
#Application Start:
toanalyse$funnel[tolower(toanalyse$app.status)=='submitted' | tolower(toanalyse$app.status)=='new app' | tolower(toanalyse$app.status)=='incomplete' | tolower(toanalyse$app.status)=='need mcr/pgi' | tolower(toanalyse$app.status)=='ready for review' | tolower(toanalyse$app.status)=='needs articulation' | (tolower(toanalyse$app.status)=='withdrawn' & (is.na(toanalyse$app.applicant.response)))] <- "Application Start";

#Application Completed - Accept
toanalyse$funnel[((tolower(toanalyse$app.status)=='decisioned' & (!is.na(toanalyse$app.final.decision) & (tolower(toanalyse$app.final.decision)=='admit' | tolower(toanalyse$app.final.decision)=='conditional' | tolower(toanalyse$app.final.decision)=='defer'))) & (toanalyse$attrib.enroll..banner. == '0'))] <- "Application Completed - Accept";

#Application Completed - Deny
toanalyse$funnel[(tolower(toanalyse$app.status)=='decisioned' & tolower(toanalyse$app.final.decision)=='deny')]<-"Application Completed - Deny";

#Intend to Enroll
toanalyse$funnel[(toanalyse$attrib.enroll..banner. == '0') & (tolower(toanalyse$app.status) == 'decisioned') & (tolower(toanalyse$app.final.decision) == 'admit') & (tolower(toanalyse$app.applicant.response) == 'enroll')]<-"Intend to Enroll";

#Enrolled
toanalyse$funnel[(toanalyse$attrib.enroll..banner. == '1') & (tolower(toanalyse$app.status)=='decisioned') & (tolower(toanalyse$app.final.decision))=='admit' & (tolower(toanalyse$app.applicant.response)=='enroll')] <- "Enrolled";


write.csv(toanalyse, file = "/Users/shwet/Desktop/ML dataset/Fresh/TargetVariables.csv",row.names=FALSE)

TargetVariables <- read.csv("TargetVariables.csv")

library(dplyr)
Admits <-  filter(TargetVariables,TargetVariables$app.final.decision == "Admit" |TargetVariables$app.final.decision == "Conditional" | TargetVariables$app.final.decision == "Defer")
Other <-  filter(TargetVariables,TargetVariables$app.final.decision == "Deny" |TargetVariables$app.final.decision == "Withdraw" | is.na(TargetVariables$app.final.decision))


#Calculating means for "Admits df"
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
Admits$hs.gpa.unweighted[is.na(Admits$hs.gpa.unweighted)]<-ameanGPAunweighted

#Assigning mean to missing values in "Admits df"
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

##Calculating means for "Other df"
omeanGPAs <- mean(Other$hs.gpa, na.rm = TRUE)
omeanGPAunweighted <- mean(Other$hs.gpa.unweighted, na.rm = TRUE)
omeanACTcomp <- mean(Other$score.act.composite,na.rm = TRUE)
omeanACTenglish <- mean(Other$score.act.english, na.rm = TRUE)
omeanACTengwriting <- mean(Other$score.act.english.writing, na.rm = TRUE)
omeanACTmath <- mean(Other$score.act.math, na.rm = TRUE)
omeanACTreading <- mean(Other$score.act.reading, na.rm = TRUE)
omeanACTscience <- mean(Other$score.act.science, na.rm = TRUE)
omeanEQmath <- mean(Other$score.equivalent.math, na.rm = TRUE)
omeanEQverbal <- mean(Other$score.equivalent.verbal, na.rm = TRUE)
omeanEQwriting <- mean(Other$score.equivalent.writing, na.rm = TRUE)
omeanMAXRAWSATtot <- mean(Other$score.max.raw.sat.total, na.rm = TRUE)
omeanMAXSATmath <- mean(Other$score.max.sat.math, na.rm = TRUE)
omeanMAXSATtotal <- mean(Other$score.max.sat.total, na.rm = TRUE)
omeanMAXSATverbal <- mean(Other$score.max.sat.verbal, na.rm = TRUE)
omeanMaxSATwriting <- mean(Other$score.max.sat.writing, na.rm = TRUE)
omeanPGPA <- mean(Other$score.pgpa, na.rm = TRUE)
omeanSATessay <- mean(Other$score.sat.essay, na.rm = TRUE)
omeanSATmc <- mean(Other$score.sat.mc, na.rm = TRUE)
omeanSATmath <- mean(Other$score.sat.math, na.rm = TRUE)
omeanSATtotalvm <- mean(Other$score.sat.total..v.m., na.rm = TRUE)
omeanSATverbal <- mean(Other$score.sat.verbal, na.rm = TRUE)
omeanSATwriting <- mean(Other$score.sat.writing, na.rm = TRUE)

#Assigning mean to missing values in "Other df"
Other$hs.gpa[is.na(Other$hs.gpa)]<-omeanGPAs
Other$hs.gpa.unweighted[is.na(Other$hs.gpa.unweighted)]<-omeanGPAunweighted
Other$score.act.composite[is.na(Other$score.act.composite)]<-omeanACTcomp 
Other$score.act.english[is.na(Other$score.act.english)]<-omeanACTenglish 
Other$score.act.english.writing[is.na(Other$score.act.english.writing)]<-omeanACTengwriting 
Other$score.act.math[is.na(Other$score.act.math)]<-omeanACTmath 
Other$score.act.reading[is.na(Other$score.act.reading)]<-omeanACTreading 
Other$score.act.science[is.na(Other$score.act.science)]<-omeanACTscience 
Other$score.equivalent.math[is.na(Other$score.equivalent.math)]<-omeanEQmath 
Other$score.equivalent.verbal[is.na(Other$score.equivalent.verbal)]<-omeanEQverbal 
Other$score.equivalent.writing[is.na(Other$score.equivalent.writing)]<-omeanEQwriting 
Other$score.max.raw.sat.total[is.na(Other$score.max.raw.sat.total)]<-omeanMAXRAWSATtot 
Other$score.max.sat.math[is.na(Other$score.max.sat.math)]<-omeanMAXSATmath 
Other$score.max.sat.total[is.na(Other$score.max.sat.total)]<-omeanMAXSATtotal 
Other$score.max.sat.verbal[is.na(Other$score.max.sat.verbal)]<-omeanMAXSATverbal 
Other$score.max.sat.writing[is.na(Other$score.max.sat.writing)]<-omeanMaxSATwriting 
Other$score.pgpa[is.na(Other$score.pgpa)]<-omeanPGPA 
Other$score.sat.essay[is.na(Other$score.sat.essay)]<-omeanSATessay 
Other$score.sat.mc[is.na(Other$score.sat.mc)]<-omeanSATmc 
Other$score.sat.math[is.na(Other$score.sat.math)]<-omeanSATmath 
Other$score.sat.total..v.m.[is.na(Other$score.sat.total..v.m.)]<-omeanSATtotalvm 
Other$score.sat.verbal[is.na(Other$score.sat.verbal)]<-omeanSATverbal 
Other$score.sat.writing[is.na(Other$score.sat.writing)]<-omeanSATwriting 

#Rounding off to two decimal places
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

Other$hs.gpa<-round(Other$hs.gpa, 2)
Other$hs.gpa.unweighted<-round(Other$hs.gpa.unweighted, 2)
Other$score.act.composite<-round(Other$score.act.composite, 2)
Other$score.act.english<-round(Other$score.act.english, 2)
Other$score.act.english.writing<-round(Other$score.act.english.writing, 2)
Other$score.act.math<-round(Other$score.act.math, 2)
Other$score.act.reading<-round(Other$score.act.reading, 2)
Other$score.act.science<-round(Other$score.act.science, 2)
Other$score.equivalent.math<-round(Other$score.equivalent.math, 2)
Other$score.equivalent.verbal<-round(Other$score.equivalent.verbal, 2)
Other$score.equivalent.writing<-round(Other$score.equivalent.writing, 2)
Other$score.max.raw.sat.total<-round(Other$score.max.raw.sat.total, 2)
Other$score.max.sat.math<-round(Other$score.max.sat.math, 2)
Other$score.max.sat.total<-round(Other$score.max.sat.total, 2)
Other$score.max.sat.verbal<-round(Other$score.max.sat.verbal, 2)
Other$score.max.sat.writing<-round(Other$score.max.sat.writing, 2)
Other$score.pgpa<-round(Other$score.pgpa, 2)
Other$score.sat.essay<-round(Other$score.sat.essay, 2)
Other$score.sat.mc<-round(Other$score.sat.mc, 2)
Other$score.sat.math<-round(Other$score.sat.math, 2)
Other$score.sat.total..v.m.<-round(Other$score.sat.total..v.m., 2)
Other$score.sat.verbal<-round(Other$score.sat.verbal, 2)
Other$score.sat.writing<-round(Other$score.sat.writing, 2)

write.csv(Admits, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Admits.csv")
write.csv(Other, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Other.csv")

#Combining two dfs in one dataframe for next step : binning
Alldata <- merge(Admits, Other, by=c("applicant.id", "app.completed.date", "app.applicant.response", "app.created.date", "app.date.applied", "app.fee.status", "app.final.decision", "app.final.decision.date", "app.major.primary.", "app.major.secondary.", "app.semester..applied.", "app.semester..decision.", "app.status", "attrib.enroll..banner.", "hs.city", "hs.country", "hs.gpa", "hs.gpa.unweighted", "hs.graduated", "hs.school.type", "hs.state", "hs.zip", "score.act.composite", "score.act.english", "score.act.english.writing", "score.act.math", "score.act.reading", "score.act.science", "score.equivalent.math", "score.equivalent.verbal", "score.equivalent.writing", "score.max.raw.sat.total", "score.max.sat.math", "score.max.sat.total", "score.max.sat.verbal", "score.max.sat.writing", "score.pgpa", "score.sat.essay", "score.sat.mc", "score.sat.math", "score.sat.total..v.m.", "score.sat.verbal", "score.sat.writing", "tour.attended", "yield.probability", "funnel"), all = T)
View(Alldata)

#Feature selection
data.drop <- c("app.fee.status", "app.semester..applied.","app.semester..decision.", "tour.attended", "applicant.id","app.major.secondary.","hs.country","hs.state","hs.zip","yield.probability")
Tobin <-  Alldata[, ! names(Alldata) %in% data.drop, drop = F]

View(Tobin)

#File export to desired location
write.csv(Tobin, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Tobin.csv")

#Removing impurities: records with application submission date older than application creation date is an invalid scenario and hence corresponding data was removed.
#Removed actual dates and calculated date differences: Difference in days between Application creation date and application submission date & Difference in days between application decision date and application submisison date.
#Columns - app.completed.date, app.created.date, app.applied.date, app.decision.date, hs.graduated droppped manually
#We now have 33 variables(columns) and 48892 records(rows)
#Reading files with all string/textual values and allowing to be read as variables
Tobin <- read.csv(file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Tobin.csv", stringsAsFactors=TRUE)

#Finding out numbr of levels in a the attributes in consideration
levels(Tobin$app.applicant.response)
levels(Tobin$DateDiffSubmission.Created)
levels(Tobin$app.final.decision)
levels(Tobin$DateDiffDecision.Submission)
levels(Tobin$app.major.primary.)
levels(Tobin$app.status)
levels(Tobin$attrib.enroll..banner.)
levels(Tobin$hs.city)
levels(Tobin$hs.gpa)
levels(Tobin$hs.gpa.unweighted)
levels(Tobin$hs.school.type)
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
levels(Tobin$funnel)

#Categorizing levels into customized lables: Here we are categorizing different majors into their macro levels
levels(Tobin$app.major.primary.)[c(4,5)] <- "Architecture"
levels(Tobin$app.major.primary.)[c(6, 7, 8, 9, 28, 29, 30, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 125, 126)] <- "Arts"
levels(Tobin$app.major.primary.)[c(1, 69, 78, 81, 87, 88, 89, 93, 94, 107, 113)] <- "Business"
levels(Tobin$app.major.primary.)[c(32, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,45, 46, 47, 48, 49, 50)] <- "Education"
levels(Tobin$app.major.primary.)[c(19, 20, 21, 22, 23, 24, 25, 26, 51, 52, 53,54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 79, 80, 123)] <- "Engineering"
levels(Tobin$app.major.primary.)[c(75, 106, 114, 115, 116, 119)] <- "HealthSciences"
levels(Tobin$app.major.primary.)[c(66, 70, 74, 124)] <- "Languages"
levels(Tobin$app.major.primary.)[c(90, 91 ,92)] <- "Mathematics"
levels(Tobin$app.major.primary.)[c(11, 12, 13, 14, 15, 1617, 18, 31, 109, 110,111)] <- "Sciences"
levels(Tobin$app.major.primary.)[c(2, 3, 33, 34, 67, 7172, 73, 76, 77, 82,83, 84, 85, 86, 95, 108, 112, 117, 118, 120, 121, 122)] <- "SocialSciences"
levels(Tobin$app.major.primary.)[c(10, 68)] <- "Sports"
levels(Tobin$app.major.primary.)[c(127)] <- "Undecided"

# Equal width binning
install.packages('Hmisc')
library(Hmisc) # cut2
Tobin$hs.gpa_binned <- as.numeric( cut2(Tobin$hs.gpa, g = 5))
Tobin$hs.gpa.unweighted_binned <- as.numeric( cut2(Tobin$hs.gpa.unweighted, g = 5))
Tobin$score.equivalent.writing_binned <- as.numeric( cut2(Tobin$score.equivalent.writing, g = 5))
Tobin$score.max.raw.sat.total_binned <- as.numeric( cut2(Tobin$score.max.raw.sat.total, g = 5))
Tobin$score.max.sat.math_binned <- as.numeric( cut2(Tobin$score.max.sat.math, g = 5))
Tobin$score.max.sat.total_binned <- as.numeric( cut2(Tobin$score.max.sat.total, g = 5))
Tobin$score.max.sat.verbal_binned <- as.numeric( cut2(Tobin$score.max.sat.verbal, g = 5))
Tobin$score.max.sat.writing_binned <- as.numeric( cut2(Tobin$score.max.sat.writing, g = 5))
Tobin$score.pgpa_binned <- as.numeric( cut2(Tobin$score.pgpa, g = 5))
Tobin$score.sat.essay_binned <- as.numeric( cut2(Tobin$score.sat.essay, g = 5))
Tobin$score.sat.mc_binned <- as.numeric( cut2(Tobin$score.sat.mc, g = 5))
Tobin$score.sat.math_binned <- as.numeric( cut2(Tobin$score.sat.math, g = 5))
Tobin$score.sat.total..v.m._binned <- as.numeric( cut2(Tobin$score.sat.total..v.m., g = 5))
Tobin$score.sat.verbal_binned <- as.numeric( cut2(Tobin$score.sat.verbal, g = 5))
Tobin$score.sat.writing_binned <- as.numeric( cut2(Tobin$score.sat.writing, g = 5))


drop4 <- c("hs.gpa", "hs.gpa.unweighted", "score.act.composite", "score.act.english", "score.act.english.writing", "score.act.math", "score.act.reading", "score.act.science", "score.equivalent.math", "score.equivalent.verbal", "score.equivalent.writing", "score.max.raw.sat.total", "score.max.sat.math", "score.max.sat.total", "score.max.sat.verbal", "score.max.sat.writing", "score.pgpa", "score.sat.essay", "score.sat.mc", "score.sat.math", "score.sat.total..v.m.", "score.sat.verbal", "score.sat.writing")
Binned <-  Tobin[, ! names(Tobin) %in% drop4, drop = F]

View(Binned)

write.csv(Binned, "C:/Users/shwet/Desktop/ML dataset/Fresh/Binned.csv")

Binned <- read.csv(file = "C:/Users/shwet/Desktop/ML dataset/Fresh/Binned.csv")

Splitting data into test and train set
sample <- sample.int(n = nrow(Binned), size = floor(0.80*nrow(Binned)), replace = F)
train <- Binned[sample, ]
test  <- Binned[-sample, ]

write.csv(train, file = "C:/Users/shwet/Desktop/ML dataset/Fresh/train.csv",row.names=FALSE)


train <- read.csv(file = "C:/Users/shwet/Desktop/ML dataset/Fresh/train.csv")

install.packages("rpart")
library(rpart)

colnames(train)

#Bagging
Decisiontree = rpart(funnel ~ app.applicant.response + app.final.decision + app.status + attrib.enroll..banner. + hs.school.type , data = train, method = "class")

DecTree2 = rpart(app.final.decision ~ hs.gpa.unweighted_binned + score.max.raw.sat.total_binned + score.pgpa_binned , data = train, method = "class")

DecTree3 = rpart(app.applicant.response ~ hs.school.type + hs.gpa.unweighted_binned + score.max.raw.sat.total_binned, data = train, method = "class" )

DecTree4 = rpart(app.final.decision ~ hs.gpa.unweighted_binned + score.max.raw.sat.total_binned , data = train, method = "class")

DecTree5 = rpart(funnel ~ hs.gpa.unweighted_binned + score.max.raw.sat.total_binned , data = train, method = "class")



print(Decisiontree)

rpart.plot(Decisiontree)
install.packages('rpart.plot')
library(rpart.plot)

install.packages('caret')
library(rattle)					# Fancy tree plot
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
# but probably one of the best R packages ever

#Extracting the decision split

install.packages('rpart.plot')
library(rpart.plot)

rpart.plot(Decisiontree)
rpart.plot(DecTree2)
rpart.plot(DecTree3)
rpart.plot(DecTree4)
rpart.plot(DecTree5)

      
install.packages('partykit')
library(partykit)
prp <- as.party(Decisiontree)
decision_Split<-data_party(prp, id = 2)
View(decision_Split)
write.csv(decision_Split, file = "C:/Users/suraj/Desktop/ML/ML UNCC Admissions dataset/2ndAug_DecisionTree_Extracted.csv",row.names=FALSE)

