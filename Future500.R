getwd()
setwd("C:/Users/sreejay/Desktop/sem2/Udemy")
future <- read.csv("Future-500.csv",na.strings = c(""))
head(future)
str(future)
summary(future)
#changing from non fact to fact 
future$ID <- factor(future$ID)
future$Inception <- factor(future$Inception)
future$Profit <- factor(future$Profit)
str(future)
future$Profit <- as.numeric(as.character(future$Profit))
summary(future$Profit)
future$Revenue <- gsub("\\$","",future$Revenue)
future$Revenue <- gsub(",","",future$Revenue)
future$Revenue <- as.numeric(future$Revenue)
summary(future$Revenue)
future$Expenses <-gsub(",","",future$Expenses)
future$Expenses <-gsub(" Dollars","",future$Expenses)
future$Expenses <- as.numeric(future$Expenses)
summary(future$Expenses)
future$Growth <- gsub("%","",future$Growth)
future$Growth <- as.numeric(future$Growth)
summary(future$Growth)
fin<-future[!complete.cases(future),]
fin
fin[!which(fin$Employees == 75),]
is.na(fin$Revenue)
fin[is.na(fin$Revenue),]

fin1 <- future[!is.na(future$Industry),]
fin1
head(fin1,24)
rownames(fin1) <- 1:nrow(fin1)
fin1
fin1[is.na(fin1$State) & fin1$City == "New York", "State"] <- "NY"
fin1[is.na(fin1$State) & fin1$City == "San Francisco", "State"] <- "CA"
fin1[!complete.cases(fin1),]
#Replacing Missing Data : Median Imputation Method
median(fin1[,"Employees"],na.rm = TRUE) #OVERALL MEDIAN
median(fin1[fin1$Industry == "Retail","Employees"], na.rm = TRUE)
fin1[is.na(fin1$Employees) & fin1$Industry == "Retail","Employees"] <- median(fin1[fin1$Industry == "Retail","Employees"],na.rm = TRUE)
fin1[is.na(fin1$Employees) & fin1$Industry == "Financial Services","Employees"] <- median(fin1[fin1$Industry == "Financial Services","Employees"],na.rm = TRUE)

fin1[!complete.cases(fin1),]
fin1[is.na(fin1$Growth) & fin1$Industry == "Construction","Growth"] <- median(fin1[fin1$Industry == "Construction","Growth"],na.rm = TRUE)
fin1[is.na(fin1$Revenue) & fin1$Industry == "Construction","Revenue"] <- median(fin1[fin1$Industry == "Construction","Revenue"],na.rm = TRUE)
fin1[is.na(fin1$Expenses) & fin1$Industry == "Construction" & is.na(fin1$Profit),"Expenses"] <- median(fin1[fin1$Industry == "Construction","Expenses"],na.rm = TRUE)
fin1[is.na(fin1$Profit) & fin1$Industry == "Construction","Profit"] <- median(fin1[fin1$Industry == "Construction","Profit"],na.rm = TRUE)
fin1[is.na(fin1$Expenses), "Expenses"] = fin1[is.na(fin1$Expenses),"Revenue"] - fin1[is.na(fin1$Expenses),"Profit"]  
fin1[c(15),]

#visualization
library(ggplot2)
p <- ggplot(fin1)
p + geom_point(aes(x=Revenue,y=Expenses,colour = Industry, size = Profit))
d <- ggplot(data = fin1,aes(x=Revenue,y=Expenses,colour = Industry, size = Profit))
d+ geom_point() + geom_smooth(fill=NA,size =1.2)
#Boxplot
f <- ggplot(data = fin1,aes(x=Industry,y=Growth,colour = Industry))
f + geom_boxplot()

#Machine-Utilization
Mac <-read.csv("Machine-Utilization.csv",na.strings = c(""))
head(Mac)
str(Mac)
summary(Mac)
#dervie utulization column
Mac$Utilization <- 1 - Mac$Percent.Idle
?POSIXlt
Mac$Timestamp <- as.POSIXct(Mac$Timestamp, format= "%d/%m/%Y %H:%M")

Rl1 <- Mac[Mac$Machine == 'RL1',] 
summary(Rl1)
Rl1$Machine <- factor(Rl1$Machine)
Rl1_ut <- c(min(Rl1$Utilization, na.rm = TRUE), mean(Rl1$Utilization, na.rm = TRUE), max(Rl1$Utilization, na.rm = TRUE))

rl1_t <- length(which(Rl1$Utilization < .90)) > 0
list_rl1 <- list(Machine="RL1",stats=Rl1_ut,lowthreshold =rl1_t)
 