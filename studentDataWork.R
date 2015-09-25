library(plyr)
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

length(unique(acRec$studentID))
# [1] 7832
length(unique(acRec$Term))
# [1] 150
table(acRec$Career)
#   GRAD    LAW   NCRD   UGRD 
#   7959    533    68   207064
length(unique(acRec$Acad.Plan))
# [1] 303
length(unique(acRec$Course.ID))
# [1] 5623
length(unique(acRec$Catalog))
# [1] 1161
length(unique(acRec$Class.Nbr))
# [1] 13933

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
                  Relev_Transf = length(Subject), 
                  MAT = sum(Subject == "MAT"),
                  COS = sum(Subject == "COS"))
# merge with back
backWTransfer <- merge(back, transfer, by = "studentID", all.x = T, all.y = T)

# remove 18 records with a death date
deaths <- which(backWTransfer$Death.Date != "")
backWTr <- backWTransfer[-deaths,]
backWTr <- backWTr[, -10]



# acrec160
acRec160 <- subset(acRecCS, acRecCS$Subject == "COS" & acRecCS$Catalog == " 160")
acRec160 <- subset(acRec160, !duplicated(acRec160[, c(1, 9, 10)]))

# acrec161
acRec161 <- subset(acRecCS, acRecCS$Subject == "COS" & acRecCS$Catalog == " 161")
acRec161 <- subset(acRec161, !duplicated(acRec161[, c(1, 9, 10)]))

# acrec285
acRec285 <- subset(acRecCS, acRecCS$Subject == "COS" & acRecCS$Catalog == " 285")
acRec285 <- subset(acRec285, !duplicated(acRec285[, c(1, 9, 10)]))

# combine the three tables in one
# so that there is a row per student
allCS <- as.data.frame(unique(acRecCS$studentID))
colnames(allCS) <- c("studentID")

allCS <- merge(allCS, acRec160, by.x = "studentID", by.y = "studentID", all.x = T, all.y = F)
allCS <- merge(allCS, acRec161, by.x = "studentID", by.y = "studentID", all = T)
allCS <- merge(allCS, acRec285, by.x = "studentID", by.y = "studentID", all = T)

colnames(allCS) <-
  c("studentID", "Term.160", "Descr.160", "Career.160", "Acad.Plan.160", "Course.ID.160", "Class.Nbr.160", "Units.160", 
    "Subject.160", "Catalog.160", "Section.160", "Grade.160", "Start.Date.160", "ID.1.160", "Term.161", "Descr.161", 
    "Career.161", "Acad.Plan.161", "Course.ID.161", "Class.Nbr.161", "Units.161", "Subject.161", "Catalog.161","Section.161",
    "Grade.161",   "Start.Date.161", "ID.1.161", "Term.285", "Descr.285", "Career.285", "Acad.Plan.285", "Course.ID.285", 
    "Class.Nbr.285", "Units.285", "Subject.285", "Catalog.285", "Section.285", "Grade.285", "Start.Date.285", "ID.1.285")

colnames(backWTr) <- c("studentID", "Career", "Degree.Comp", "Acad.Plan.Comp", "Confer.Dt", "Compl.Term", "Descr.Comp.Term",
                       "Degr.GPA",  "Birthdate", "Sex", "Degree.HS", "Org.ID.HS", "Descr.HS", "Relev_Transf", "MAT", "COS")

allCS <- merge(allCS, backWTr, by.x = "studentID", by.y = "studentID", all.x = T, all.y = F)

# remove unneeded columns
allCS_fixed <- allCS[, -c(4, 6, 8, 9, 10, 17, 19, 21, 22, 23, 30, 32, 33, 34, 35)]

fixGrade <- function(df, x, y) {
  df[[y]] <- NA
  df[[y]] <- ifelse(df[[x]] == "A" | df[[x]] == "A-", "A", df[[y]])
  df[[y]] <- ifelse(df[[x]] == "B" | df[[x]] == "B-"| df[[x]] == "B+", "B", df[[y]])
  df[[y]] <- ifelse(df[[x]] == "C" | df[[x]] == "C-"| df[[x]] == "C+", "C", df[[y]])
  df[[y]] <- ifelse(df[[x]] == "D" | df[[x]] == "D-"| df[[x]] == "D+", "D", df[[y]])
  df[[y]] <- ifelse(df[[x]] == "W", "W", df[[y]])
  df[[y]] <- ifelse(df[[x]] %in% c("F","H","I","I*","INC","L","LP","M*","MG"), "F", df[[y]])
  return(df)
}

allCS_fixed_grades <- fixGrade(allCS_fixed, "Grade.160", "Grade.160.fixed")
allCS_fixed_grades <- fixGrade(allCS_fixed_grades, "Grade.161", "Grade.161.fixed")
allCS_fixed_grades <- fixGrade(allCS_fixed_grades, "Grade.285", "Grade.285.fixed")

library(stringr)

allCS_fixed_grades$yearBorn <- NA
allCS_fixed_grades$year160 <- NA
allCS_fixed_grades$year160 <- str_split_fixed(allCS_fixed_grades$Descr.160, " ", 2)[,1]

allCS_fixed_grades$yearBorn <- str_split_fixed(allCS_fixed_grades$Birthdate, "/", 3)[,3]
allCS_fixed_grades$ageAt160 <- as.integer(allCS_fixed_grades$year160) - as.integer(allCS_fixed_grades$yearBorn)

allCS_fixed_grades$ageGroup160 <- NA
allCS_fixed_grades$ageGroup160 <- ifelse(!is.na(allCS_fixed_grades$ageAt160) & 
                                           allCS_fixed_grades$ageAt160 <= 20, "<20", 
                                         allCS_fixed_grades$ageGroup160)

allCS_fixed_grades$ageGroup160 <- ifelse(!is.na(allCS_fixed_grades$ageAt160) 
                                         & allCS_fixed_grades$ageAt160 > 20 
                                         & allCS_fixed_grades$ageAt160 <= 25,
                                         ">20,<=25", allCS_fixed_grades$ageGroup160)

allCS_fixed_grades$ageGroup160 <- ifelse(!is.na(allCS_fixed_grades$ageAt160) 
                                         & allCS_fixed_grades$ageAt160 > 25 
                                         & allCS_fixed_grades$ageAt160 <= 30,
                                         ">25,<=30", allCS_fixed_grades$ageGroup160)

allCS_fixed_grades$ageGroup160 <- ifelse(!is.na(allCS_fixed_grades$ageAt160) & 
                                           allCS_fixed_grades$ageAt160 > 30, ">30", 
                                         allCS_fixed_grades$ageGroup160)

plot(as.factor(allCS_fixed_grades$Grade.160.fixed), 
     allCS_fixed_grades$ageAt160, xlab = "Grade COS160", ylim = c(10, 50),
     ylab = "Age (continuous)", main = "Distribution of COS 160 Grades by age")

term_lookup1909 <- unique(acRec[,2:3])
term_lookup1909 <- term_lookup1909[order(term_lookup1909$Term), ]
term_lookup1909$newTerm <- 1:nrow(term_lookup1909)

acRecCS$newTerm <- with(term_lookup1909, term_lookup1909$newTerm[match(acRecCS$Term, term_lookup1909$Term)])

allCS_fixed_grades <- droplevels(allCS_fixed_grades)

allCS_fixed_grades$Relev_Transf[is.na(allCS_fixed_grades$Relev_Transf)] <- 0
allCS_fixed_grades$MAT[is.na(allCS_fixed_grades$MAT)] <- 0
allCS_fixed_grades$COS[is.na(allCS_fixed_grades$COS)] <- 0

allCS_fixed_grades$Grade.160.fixed[is.na(allCS_fixed_grades$Grade.160.fixed)] <- "NA"
allCS_fixed_grades$Grade.161.fixed[is.na(allCS_fixed_grades$Grade.161.fixed)] <- "NA"
allCS_fixed_grades$Grade.285.fixed[is.na(allCS_fixed_grades$Grade.285.fixed)] <- "NA"

last6terms <- c(1410, 1420, 1430, 1510, 1520, 1530)
allCS_fixed_grades$GotCSDegree <- ifelse(allCS_fixed_grades$Acad.Plan.Comp == "COS-BS" 
                                         & !is.na(allCS_fixed_grades$Acad.Plan.Comp), T, F)
allCS_fixed_grades$GotOtherDegree <- ifelse(allCS_fixed_grades$Acad.Plan.Comp != "COS-BS" 
                                            & !is.na(allCS_fixed_grades$Acad.Plan.Comp), T, F)

letterToGPA <- function(df, x, y) {
  df[[y]] <- 0.0
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

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



acRecCS <- letterToGPA(acRecCS, "Grade", "Class.GPA")
acRecCS <- droplevels(acRecCS)

term_lookup1909 <- unique(acRec[,2:3])
term_lookup1909 <- term_lookup1909[order(term_lookup1909$Term), ]
term_lookup1909$newTerm <- 1:nrow(term_lookup1909)

acRecCS$newTerm <- with(term_lookup1909, term_lookup1909$newTerm[match(acRecCS$Term, term_lookup1909$Term)])

acRecSummary <- ddply(acRecCS, .(studentID), summarise,
                      First_term = min(newTerm),
                      First_cs_term = min(newTerm[Acad.Plan == "COS-BS" | Acad.Plan == "COS-GO" | Acad.Plan == "COS-DM"]),
                      Last_term = max(newTerm),
                      Last_cs_term = max(newTerm[Acad.Plan == "COS-BS" | Acad.Plan == "COS-GO" | Acad.Plan == "COS-DM"]),
                      Total_terms = length(unique(Term)),
                      AveUnitsPerTerm = sum(Units)/length(unique(Term)),
                      MathUnits = sum(Units[Subject == "MAT"]),
                      CSUnits = sum(Units[Subject == "COS"]),
                      AveGPA = sum(Class.GPA)/length(Class.GPA),
                      AveCSGPA = mean(Class.GPA[Subject == "COS"]),
                      AveMathGPA = mean(Class.GPA[Subject == "MAT"]))
                      #MajorIn160 = Acad.Plan[Subject == "COS" & Catalog == " 160"]
#                       MajorIn161 = ifelse(Acad.Plan[Subject == "COS" & Catalog == " 161"] == "COS-BS" |
#                                             Acad.Plan[Subject == "COS" & Catalog == " 161"] == "COS-GO" |
#                                             Acad.Plan[Subject == "COS" & Catalog == " 161"] == "COS-DM", T, F),
#                       MajorIn285 = ifelse(Acad.Plan[Subject == "COS" & Catalog == " 285"] == "COS-BS" |
#                                             Acad.Plan[Subject == "COS" & Catalog == " 285"] == "COS-GO" |
#                                             Acad.Plan[Subject == "COS" & Catalog == " 285"] == "COS-DM", T, F)
                     # )

# acrec160
acRec160 <- subset(acRecCS, acRecCS$Subject == "COS" & acRecCS$Catalog == " 160")
acRec160 <- subset(acRec160, !duplicated(acRec160[, c(1, 9, 10)]))

# acrec161
acRec161 <- subset(acRecCS, acRecCS$Subject == "COS" & acRecCS$Catalog == " 161")
acRec161 <- subset(acRec161, !duplicated(acRec161[, c(1, 9, 10)]))

# acrec285
acRec285 <- subset(acRecCS, acRecCS$Subject == "COS" & acRecCS$Catalog == " 285")
acRec285 <- subset(acRec285, !duplicated(acRec285[, c(1, 9, 10)]))

allCS <- as.data.frame(unique(acRecCS$studentID))
colnames(allCS) <- c("studentID")

allCS <- merge(allCS, acRec160, by.x = "studentID", by.y = "studentID", all.x = T, all.y = F)
allCS <- merge(allCS, acRec161, by.x = "studentID", by.y = "studentID", all = T)
allCS <- merge(allCS, acRec285, by.x = "studentID", by.y = "studentID", all = T)

colnames(allCS) <-
   c("studentID", "Term.160", "Descr.160", "Career.160", "Acad.Plan.160", "Course.ID.160", "Class.Nbr.160", "Units.160", 
    "Subject.160", "Catalog.160", "Section.160", "Grade.160", "Start.Date.160", "ID.1.160", "Class.GPA.160", "newTerm.160", "Term.161", "Descr.161", 
    "Career.161", "Acad.Plan.161", "Course.ID.161", "Class.Nbr.161", "Units.161", "Subject.161", "Catalog.161","Section.161",
    "Grade.161",   "Start.Date.161", "ID.1.161", "Class.GPA.161", "newTerm.161", "Term.285", "Descr.285", "Career.285", "Acad.Plan.285", "Course.ID.285", 
    "Class.Nbr.285", "Units.285", "Subject.285", "Catalog.285", "Section.285", "Grade.285", "Start.Date.285", "ID.1.285", "Class.GPA.285", "newTerm.285")

colnames(backWTr) <- c("studentID", "Career", "Degree.Comp", "Acad.Plan.Comp", "Confer.Dt", "Compl.Term", "Descr.Comp.Term",
                       "Degr.GPA",  "Birthdate", "Sex", "Degree.HS", "Org.ID.HS", "Descr.HS", "Relev_Transf", "MAT", "COS")

allCS <- merge(allCS, backWTr, by.x = "studentID", by.y = "studentID", all.x = T, all.y = F)

allCS_fixed <- allCS[, -c(4, 6, 8, 9, 10, 19, 21, 23, 24, 25, 34, 36, 38, 39, 40)]

workTable1 <- merge(allCS_fixed, acRecSummary, by = "studentID", all.x = T, all.y = F)

# workTable1 <- allCS_fixed_grades[,c(1,2,10,18,32,34,39:43,47:49)]
# workTable1 <- merge(workTable1, acRecSummary, by = "studentID", all.x = T, all.y = F)
# 
last6terms <- c(1410, 1420, 1430, 1510, 1520, 1530)
workTable1$GotCSDegree <- ifelse(workTable1$Acad.Plan.Comp == "COS-BS" 
                                         & !is.na(workTable1$Acad.Plan.Comp), T, F)
workTable1$GotOtherDegree <- ifelse(workTable1$Acad.Plan.Comp != "COS-BS" 
                                            & !is.na(workTable1$Acad.Plan.Comp), T, F)
workTable1$CurrentStudent <- ifelse(workTable1$Last_term %in% last6terms
                                    & workTable1$GotCSDegree == F
                                    & workTable1$GotOtherDegree == F, T, F)

workTable1$DroppedOut <- ifelse(!workTable1$Last_term %in% last6terms
                                    & workTable1$GotCSDegree == F
                                    & workTable1$GotOtherDegree == F, T, F)
workTableNotCurrent <- subset(workTable1, CurrentStudent == F)
# 
workTableNotCurrent$binResponse <- ifelse(workTableNotCurrent$GotCSDegree == T, 1, 0)
workTableNotCurrent <- workTableNotCurrent[, c(1,4,7,10,11,14,17,20,21,24,27,30,31:59)]

workTableNotCurrent$MAT <- ifelse(is.na(workTableNotCurrent$MAT), 0, workTableNotCurrent$MAT)
workTableNotCurrent$COS <- ifelse(is.na(workTableNotCurrent$COS), 0, workTableNotCurrent$COS)





