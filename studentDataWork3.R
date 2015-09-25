
library(plyr)
library(stringr)

## Load the data

# get the academic record
acRec <- read.csv("Downloads/fullStudentData/DEID_AcadRec.csv") #, stringsAsFactors = F)
# get the background record
backG <- read.csv("Downloads/fullStudentData/DEID_background.csv")
# get the culmination record
culmR <- read.csv("Downloads/fullStudentData/DEID_culmination.csv")
# get the test scores
testS <- read.csv("Downloads/fullStudentData/DEID_test.csv")
# get the transfer record
transf <- read.csv("Downloads/fullStudentData/DEID_transfer.csv")


# CS majors only
cs_majors <- unique(acRec$studentID[acRec$Acad.Plan == "COS-BS" | acRec$Acad.Plan == "COS-GO" | acRec$Acad.Plan == "COS-DM"])
acRecCS <- acRec[acRec$studentID %in% cs_majors,]

# remove duplicates
acRecCS <- subset(acRecCS, !duplicated(acRecCS[,-5]))
# remove grad students
grad <- unique(acRec$studentID[acRec$Career == "GRAD" & acRec$Acad.Plan == "COS-MS"])
acRecCS <- acRecCS[!acRecCS$studentID %in% grad,]
length(unique(acRecCS$studentID))
#acRecCS <- subset(acRecCS, Career != "GRAD")

# merge background files

# students who got COS BS
culmR <- culmR[!culmR$studentID %in% grad,]
culmCS <- unique(subset(culmR, Acad.Plan == "COS-BS" | Acad.Plan == "COS-GO" | Acad.Plan == "COS-DM"))
# other students
ids <- culmCS$studentID
culmOther <- unique(subset(culmR, !culmR$studentID %in% ids))
culmOther <- culmOther[!duplicated(culmOther[,1]),]
# combine
culmAll <- rbind(culmCS, culmOther)


# merge with background
backG <- backG[!duplicated(backG[,c(1,2,4)]),]
back <- merge(culmAll, backG[,-8], by = "studentID", all.x = T, all.y = T)
#backCS <- backCS[backCS$studentID %in% cs_majors,]

# add transfers
transf_relevant <- subset(transf, Subject == "MAT" | Subject == "COS")
transfer <- ddply(transf_relevant, .(studentID), summarise, 
                  #Relev_Transf = length(Subject), 
                  MAT = sum(Subject == "MAT"),
                  COS = sum(Subject == "COS"))
# merge with back
backWTransfer <- merge(back, transfer, by = "studentID", all.x = T, all.y = T)

# remove 18 records with a death date
deaths <- which(backWTransfer$Death.Date != "")
backWTr <- backWTransfer[-deaths,]
backWTr <- backWTr[, -10]
colnames(backWTr) <- c("studentID", "Career", "Degree.Comp", "Acad.Plan.Comp", 
                       "Confer.Dt", "Compl.Term", "Descr.Comp.Term",
                       "Degr.GPA",  "Birthdate", "Sex", "Degree.HS", 
                       "Org.ID.HS", "Descr.HS", "MAT", "COS")


allCS <- as.data.frame(unique(acRecCS$studentID))
colnames(allCS) <- c("studentID")
allCS <- merge(allCS, backWTr, by.x = "studentID", by.y = "studentID", all.x = T, all.y = F)
# acrec160
acRec160 <- subset(acRecCS, acRecCS$Subject == "COS" & acRecCS$Catalog == " 160")
acRec160 <- subset(acRec160, !duplicated(acRec160[, c(1, 9, 10)]))
allCS <- merge(allCS, acRec160, by.x = "studentID", by.y = "studentID", all.x = T, all.y = F)
allCS <- droplevels(allCS)

allCS$yearBorn <- NA
allCS$year160 <- NA
allCS$year160 <- str_split_fixed(allCS$Descr, " ", 2)[,1]

allCS$yearBorn <- str_split_fixed(allCS$Birthdate, "/", 3)[,3]
allCS$ageAt160 <- as.integer(allCS$year160) - as.integer(allCS$yearBorn)

allCS$ageGroup160 <- NA
allCS$ageGroup160 <- ifelse(!is.na(allCS$ageAt160) & 
                                           allCS$ageAt160 <= 20, "<20", 
                                         allCS$ageGroup160)

allCS$ageGroup160 <- ifelse(!is.na(allCS$ageAt160) 
                                         & allCS$ageAt160 > 20 
                                         & allCS$ageAt160 <= 25,
                                         ">20,<=25", allCS$ageGroup160)

allCS$ageGroup160 <- ifelse(!is.na(allCS$ageAt160) 
                                         & allCS$ageAt160 > 25 
                                         & allCS$ageAt160 <= 30,
                                         ">25,<=30", allCS$ageGroup160)

allCS$ageGroup160 <- ifelse(!is.na(allCS$ageAt160) & 
                                           allCS$ageAt160 > 30, ">30", 
                                         allCS$ageGroup160)




term_lookup1909 <- unique(acRec[,2:3])
term_lookup1909 <- term_lookup1909[order(term_lookup1909$Term), ]
term_lookup1909$newTerm <- 1:nrow(term_lookup1909)

acRecCS$newTerm <- with(term_lookup1909, term_lookup1909$newTerm[match(acRecCS$Term, term_lookup1909$Term)])


allCS <- droplevels(allCS)

allCS$MAT[is.na(allCS$MAT)] <- 0
allCS$COS[is.na(allCS$COS)] <- 0


last6terms <- c(1410, 1420, 1430, 1510, 1520, 1530)
allCS$GotCSDegree <- NA
allCS$GotCSDegree <- ifelse(allCS$Acad.Plan.Comp == "COS-BS" 
                                         & !is.na(allCS$Compl.Term), T, F)
allCS$GotOtherDegree <- NA
allCS$GotOtherDegree <- ifelse(allCS$Acad.Plan.Comp != ""
                                            & !is.na(allCS$Compl.Term), T, F)

allCS_wt <- subset(allCS, GotOtherDegree == F)
allCS_wt <- droplevels(allCS_wt)
allCS_wt$Sex <- relevel(allCS_wt$Sex, "M")


letterToGPA <- function(df, x, y) {
  #df[[y]] <- 0.0
  df[[y]] <- ifelse(df[[x]] == "A", 4.0, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "A-", 3.67, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "B+", 3.33, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "B", 3.0, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "B-", 2.67, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "C+", 2.33, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "C", 2.00, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "C-", 1.67, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "D+", 1.33, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "D", 1.00, df[[y]])
  df[[y]] <- ifelse(df[[x]] == "D-", 0.67, df[[y]])
  return(df)
}

acRecCS$Class.GPA <- NA
acRecCS <- letterToGPA(acRecCS, "Grade", "Class.GPA")
acRecCS <- droplevels(acRecCS)

acRecCS$Year <- NA
acRecCS$Year <- str_split_fixed(acRecCS$Descr, " ", 2)[,1]

acRecSummary <- ddply(acRecCS, .(studentID), summarise,
                      First_term = min(newTerm),
                      First_cs_year = min(Year[Acad.Plan == "COS-BS"]),
                      First_cs_term = min(newTerm[Acad.Plan == "COS-BS" | Acad.Plan == "COS-GO" | Acad.Plan == "COS-DM"]),
                      Last_term = max(newTerm),
                      Last_cs_term = max(newTerm[Acad.Plan == "COS-BS" | Acad.Plan == "COS-GO" | Acad.Plan == "COS-DM"]),
                      Total_terms = length(unique(Term)),
                      AveUnitsPerTerm = sum(Units)/length(unique(Term)),
                      MathUnits = sum(Units[Subject == "MAT"]),
                      CSUnits = sum(Units[Subject == "COS"]))


workTable1 <- merge(allCS_wt, acRecSummary, by = "studentID", all.x = T, all.y = F)

