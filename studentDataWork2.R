## describe the data
library(plyr)
library(ggplot2)


wt1 <- workTableNotCurrent
wt1 <- droplevels(wt1)
acRecCS1 <- subset(acRecCS, !duplicated(acRecCS[, c(1,6)]))[,c(1,5,16)]
acRecCS1 <- ddply(acRecCS[,c(1,5,16)], .(studentID, newTerm), catcolwise(as.factor))

acRecCS1 <- subset(acRecCS1, !duplicated(acRecCS1[,1:2]))


x <- as.data.frame(cbind(studentID = acRecCS1$studentID,
                         newTerm = acRecCS1$newTerm, 
                         studentTerm = sequence(unname(table(acRecCS1$studentID)))))
acRecCS1 <- merge(acRecCS1, x, by = c("studentID", "newTerm"), all = T)
acRecCS1 <- acRecCS1[order(acRecCS1$studentID, acRecCS1$newTerm),]

wt1$startCS <- ifelse(workTableNotCurrent$First_term == workTableNotCurrent$First_cs_term, T, F)

wt1 <- wt1[rowSums(is.na(wt1[c("Grade.160","Grade.161","Grade.285")])) < 3,]

#160
wt1 <- merge(wt1, acRecCS1[,c(1,2,4)], by.x = c("studentID", "newTerm.160"), by.y = c("studentID", "newTerm"), all.x = T, all.y = F)
colnames(wt1)[ncol(wt1)] <- "studentTerm160"

wt1$studentTermAfter160 <- wt1$studentTerm160+1

wt1 <- merge(wt1, acRecCS1[,c(1,3,4)], by.x = c("studentID", "studentTermAfter160"), by.y = c("studentID", "studentTerm"), all.x = T, all.y = F)
colnames(wt1)[ncol(wt1)] <- "Acad.Plan.After160"

wt1$switchAfter160 <- NA
wt1$switchAfter160 <- ifelse(as.character(wt1$Acad.Plan.160) != as.character(wt1$Acad.Plan.After160), T, F)

#161
wt1 <- merge(wt1, acRecCS1[,c(1,2,4)], by.x = c("studentID", "newTerm.161"), by.y = c("studentID", "newTerm"), all.x = T, all.y = F)
colnames(wt1)[ncol(wt1)] <- "studentTerm161"

wt1$studentTermAfter161 <- wt1$studentTerm161+1

wt1 <- merge(wt1, acRecCS1[,c(1,3,4)], by.x = c("studentID", "studentTermAfter161"), by.y = c("studentID", "studentTerm"), all.x = T, all.y = F)
colnames(wt1)[ncol(wt1)] <- "Acad.Plan.After161"

wt1$switchAfter161 <- NA
wt1$switchAfter161 <- ifelse(as.character(wt1$Acad.Plan.161) != as.character(wt1$Acad.Plan.After161), T, F)

#285
wt1 <- merge(wt1, acRecCS1[,c(1,2,4)], by.x = c("studentID", "newTerm.285"), by.y = c("studentID", "newTerm"), all.x = T, all.y = F)
colnames(wt1)[ncol(wt1)] <- "studentTerm285"

wt1$studentTermAfter285 <- wt1$studentTerm285+1

wt1 <- merge(wt1, acRecCS1[,c(1,3,4)], by.x = c("studentID", "studentTermAfter285"), by.y = c("studentID", "studentTerm"), all.x = T, all.y = F)
colnames(wt1)[ncol(wt1)] <- "Acad.Plan.After285"

wt1$switchAfter285 <- NA
wt1$switchAfter285 <- ifelse(as.character(wt1$Acad.Plan.285) != as.character(wt1$Acad.Plan.After285), T, F)

# counts
wt1$csMajor160 <- NA
wt1$csMajor160 <- ifelse(wt1$Acad.Plan.160 == "COS-BS" | wt1$Acad.Plan.160 == "COS-GO" | wt1$Acad.Plan.160 == "COS-DM", T, F)

wt1$csMajor161 <- NA
wt1$csMajor161 <- ifelse(wt1$Acad.Plan.161 == "COS-BS" | wt1$Acad.Plan.161 == "COS-GO" | wt1$Acad.Plan.161 == "COS-DM", T, F)

wt1$csMajor285 <- NA
wt1$csMajor285 <- ifelse(wt1$Acad.Plan.285 == "COS-BS" | wt1$Acad.Plan.285 == "COS-GO" | wt1$Acad.Plan.285 == "COS-DM", T, F)

wt1$dropCSAfter160 <- NA
wt1$dropCSAfter160 <- ifelse(wt1$csMajor160 & wt1$switchAfter160, T, F)

wt1$becameCSAfter160 <- NA
wt1$becameCSAfter160 <- ifelse(!wt1$csMajor160 & wt1$switchAfter160, T, F)

wt1$dropCSAfter161 <- NA
wt1$dropCSAfter161 <- ifelse(wt1$csMajor161 & wt1$switchAfter161, T, F)

wt1$becameCSAfter161 <- NA
wt1$becameCSAfter161 <- ifelse(!wt1$csMajor161 & wt1$switchAfter161, T, F)

wt1$dropCSAfter285 <- NA
wt1$dropCSAfter285 <- ifelse(wt1$csMajor285 & wt1$switchAfter285, T, F)

wt1$becameCSAfter285 <- NA
wt1$becameCSAfter285 <- ifelse(!wt1$csMajor285 & wt1$switchAfter285, T, F)

wt1$outcome160 <- NA
wt1$outcome160 <- ifelse(wt1$csMajor160 & !wt1$dropCSAfter160, 0, wt1$outcome160)
wt1$outcome160 <- ifelse(wt1$csMajor160 & wt1$dropCSAfter160, 1, wt1$outcome160)
wt1$outcome160 <- ifelse(!wt1$csMajor160 & !wt1$becameCSAfter160, 2, wt1$outcome160)
wt1$outcome160 <- ifelse(!wt1$csMajor160 & wt1$becameCSAfter160, 3, wt1$outcome160)
wt1$outcome160 <- ifelse(wt1$csMajor160 & !is.na(wt1$Acad.Plan.160) & is.na(wt1$Acad.Plan.After160), 4, wt1$outcome160)
wt1$outcome160 <- ifelse(!wt1$csMajor160 & !is.na(wt1$Acad.Plan.160) & is.na(wt1$Acad.Plan.After160), 5, wt1$outcome160)

wt1$outcome161 <- NA
wt1$outcome161 <- ifelse(wt1$csMajor161 & !wt1$dropCSAfter161, 0, wt1$outcome161)
wt1$outcome161 <- ifelse(wt1$csMajor161 & wt1$dropCSAfter161, 1, wt1$outcome161)
wt1$outcome161 <- ifelse(!wt1$csMajor161 & !wt1$becameCSAfter161, 2, wt1$outcome161)
wt1$outcome161 <- ifelse(!wt1$csMajor161 & wt1$becameCSAfter161, 3, wt1$outcome161)
wt1$outcome161 <- ifelse(wt1$csMajor161 & !is.na(wt1$Acad.Plan.161) & is.na(wt1$Acad.Plan.After161), 4, wt1$outcome161)
wt1$outcome161 <- ifelse(!wt1$csMajor161 & !is.na(wt1$Acad.Plan.161) & is.na(wt1$Acad.Plan.After161), 5, wt1$outcome161)

wt1$outcome285 <- NA
wt1$outcome285 <- ifelse(wt1$csMajor285 & !wt1$dropCSAfter285, 0, wt1$outcome285)
wt1$outcome285 <- ifelse(wt1$csMajor285 & wt1$dropCSAfter285, 1, wt1$outcome285)
wt1$outcome285 <- ifelse(!wt1$csMajor285 & !wt1$becameCSAfter285, 2, wt1$outcome285)
wt1$outcome285 <- ifelse(!wt1$csMajor285 & wt1$becameCSAfter285, 3, wt1$outcome285)
wt1$outcome285 <- ifelse(wt1$csMajor285 & !is.na(wt1$Acad.Plan.285) & is.na(wt1$Acad.Plan.After285), 4, wt1$outcome285)
wt1$outcome285 <- ifelse(!wt1$csMajor285 & !is.na(wt1$Acad.Plan.285) & is.na(wt1$Acad.Plan.After285), 5, wt1$outcome285)

table160 <- prop.table(table(wt1$outcome160))
table161 <- prop.table(table(wt1$outcome161))
table285 <- prop.table(table(wt1$outcome285))
tableAll <- as.data.frame(rbind(table160, table161, table285))


colnames(tableAll) <- c("CsCs", "CsOt", "OtOt", "OtCs", "CsDr", "OtDr")
rownames(tableAll) <- c("After160", "After161", "After285")

par(mar = c(10, 4.1, 4.1, 2.1), mgp = c(7,1,0))
barplot(as.matrix(t(tableAll)), 
        main = "Change of major in CS after first three required classes:",
        sub = "CsCs --> was CS major before and during the class and stayed CS major after,
        CsOt --> was CS major before and during the class but switched major after,     
        OtOt --> was another major and did not become a CS major after,
        OtCs --> was another major but became a CS major after,
        CsDr --> was CS major but dropped out the following semester,
        OtDr --> was another major but dropped out after", 
        cex.main = .9,xlab = "", legend = colnames(tableAll), beside = T, col = rainbow(6))

