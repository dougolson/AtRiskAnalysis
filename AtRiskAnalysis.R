library(data.table)
### remember to reset the wd in lines 3, 6 and 53 going from work to home
path1 <- "/Users/dolson/Google\ Drive/Code/R/MMIAtRisk/Current/"
file1 <- list.files(path=path1, pattern="*.csv")
loc1<- paste(path1,file1,sep="")
path2 <- "/Users/dolson/Google\ Drive/Code/R/MMIAtRisk/2weeksAgo/"
file2 <- list.files(path=path2, pattern="*.csv")
loc2<- paste(path2,file2,sep="")
path3 <- "/Users/dolson/Google\ Drive/Code/R/MMIAtRisk/Semester/"
file3 <- list.files(path=path3, pattern="*.csv")
loc3<- paste(path3,file3,sep="")
###################################################
### perform calculations on the current dataset ###
###################################################
DT.current <- data.table(read.csv(loc1, strip.white=TRUE))
DT.old <- data.table(read.csv(loc2, strip.white=TRUE))
DT.semester <- data.table(read.csv(loc3, strip.white=TRUE))
DT.current <- DT.current[,.(Student, Prg, Miss, CalcGr,Class)]   #remove extraneous columns
DT.current[,CourseCode:=substr(as.character(DT.current$Class),4,8)]
DT.current <- DT.current[,.(Student, Prg, Miss, CalcGr, CourseCode)]   #remove extraneous column
DT.current <- data.table(merge.data.frame(DT.current,DT.semester,all.x = TRUE))
grvec <- DT.current$CalcGr   #create a grade vector
grvec <- as.numeric(grvec) #coerce grade vector to numeric
convert <- function(x) abs(x-5) #function on grade vector so A=4, B=3,...,F=0
grvec <- convert(grvec) #create vector of numeric grades
DT.current$NumGr <- grvec #update the data table to include the numeric grade vector
fvec <- as.numeric(grvec==0) #create a vector for F count
DT.current$F_Count <- fvec
DT.current <- DT.current[,.(Student, Prg, Miss, Semester,NumGr,F_Count)]   #remove extraneous column
DT.current <- DT.current[, j=list(Abs=sum(Miss, na.rm = TRUE), GPA=round(mean(NumGr, na.rm = TRUE),2), F_Count=sum(F_Count, na.rm = TRUE), Semester=round(mean(Semester, na.rm = TRUE),0)), by = list(Prg, Student)] #aggregate means by program and student
DT.current <- DT.current[order(DT.current$Student),] #sort by student
scorefun <- function(Abs,GPA,F_Count){sqrt(((Abs)/max(Abs))^2+(1-(GPA)/max(GPA))^2+(F_Count/max(F_Count))^2)/3}
score <- scorefun(DT.current$Abs,DT.current$GPA,DT.current$F_Count)
score <- scale(score, center = FALSE) #scale the score without centering
score <- 10*score/max(range(score)) #convert to 0-10 format
DT.current$Score <- score #add at risk score column to DT.current
########################################################
### perform calculations on the two week old dataset ###
########################################################
DT.old <- DT.old[,.(Student, Prg, Miss, CalcGr,Class)]   #remove extraneous columns
DT.old[,CourseCode:=substr(as.character(DT.old$Class),4,8)]
DT.old <- DT.old[,.(Student, Prg, Miss, CalcGr, CourseCode)]   #remove extraneous column
DT.old <- data.table(merge.data.frame(DT.old,DT.semester,all.x = TRUE))
grvec2 <- DT.old$CalcGr   #create a grade vector
grvec2 <- as.numeric(grvec2) #coerce grade vector to numeric
convert <- function(x) abs(x-5) #function on grade vector so A=4, B=3,...,F=0
grvec2 <- convert(grvec2) #create vector of numeric grades
DT.old$NumGr <- grvec2 #update the data table to include the numeric grade vector
fvec2 <- as.numeric(grvec2==0) #create a vector for F count
DT.old$F_Count <- fvec2
DT.old <- DT.old[,.(Student, Prg, Miss, Semester,NumGr,F_Count)]   #remove extraneous column
DT.old <- DT.old[, j=list(Abs=sum(Miss, na.rm = TRUE), GPA=round(mean(NumGr, na.rm = TRUE),2), F_Count=sum(F_Count, na.rm = TRUE), Semester=round(mean(Semester, na.rm = TRUE),0)), by = list(Prg, Student)] #aggregate means by program and student
DT.old <- DT.old[order(DT.old$Student),] #sort by student
scorefun <- function(Abs,GPA,F_Count){sqrt(((Abs)/max(Abs))^2+(1-(GPA)/max(GPA))^2+(F_Count/max(F_Count))^2)/3}
score2 <- scorefun(DT.old$Abs,DT.old$GPA,DT.old$F_Count)
score2 <- scale(score2, center = FALSE) #scale the score2 without centering
score2 <- 10*score2/max(range(score2)) #convert to 0-10 format
DT.old$Score <- score2 #add at risk score column to DT.old
#############################
### merge the two outputs ###
#############################
DT.merge <- data.table(merge(DT.old,DT.current,"Student",all=TRUE)) #Merge the data sets by student
DT.merge$Trend <- DT.merge$Score.y-DT.merge$Score.x
DT.final <- DT.merge[,.(Student,Prog=Prg.x,Score=Score.y,Abs=Abs.y,GPA=GPA.y,F_Count=F_Count.y,ScoreTwoWeeks=Score.x,Trend,Semester.x)]
DT.final <- DT.final[order(DT.final$Prog,-DT.final$Score)] # Sort by program with descending score
DT.final <- DT.final[, j=list(Student,Prog,Score=round(Score,2),Abs=round(Abs,2),GPA=round(GPA,2),F_Count,
                              ScoreTwoWeeks=round(ScoreTwoWeeks,2),Trend=round(Trend,2),Semester.Estimate=Semester.x)]
# Write File Output
date <- Sys.Date()
filename <- paste("At_Risk_",date,".csv",sep="")
wdOrig <- getwd()
setwd("/Users/dolson/Google\ Drive/MMI/At\ Risk/2016-1\ At\ Risk/")
write.csv(DT.final, file = filename)
setwd(wdOrig)




 


