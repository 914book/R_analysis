mecha_table <- read.csv(file = 'MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)
?read.csv
mecha_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
mecha_table <- read.csv(file='mechacar.csv',check.names=F,stringsAsFactors = F)
setwd("C:/AllOneDriveAccounts/OneDrive/Education/Toral/ClassFolder/Module15_R/R_Anlysis/challenge/")
mecha_car_table <- read.csv(file='mechacar.csv',check.names=F,stringsAsFactors = F)
head(mecha_car_table)
plt <- ggplot(mecha_car_table,aes(x=angle,y=mpg)) #import dataset into ggplot2
library(ggplot2)
plt <- ggplot(mecha_car_table,aes(x=length,y=mpg)) #import dataset into ggplot2
plt + geom_point() #create scatter plot
cor(mecha_car_table$length,mecha_car_table$mpg) #calculate correlation coefficient
mecha_matrix <- as.matrix(mecha_car_table[,c("mpg","length","weight")]) #convert data frame into numeric matrix
cor(mecha_matrix)

lm(mpg ~ length,mecha_car_table) #create linear model
summary(lm(mpg~length,mecha_car_table)) #summarize linear model

model <- lm(mpg ~ length,mecha_car_table) #create linear model
yvals <- model$coefficients['length']*mecha_car_table$length + model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(mecha_car_table,aes(x=length,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model


lm(mpg ~ length + weight + angle + clearance + awd,data=mecha_car_table) #generate multiple linear regression model
summary(lm(mpg ~ length + weight + angle + clearance + awd,data=mecha_car_table)) #generate summary statistics


Suspension_coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
head(Suspension_coil_table)

mean(Suspension_coil_table$PSI)
median(Suspension_coil_table$PSI)
var(Suspension_coil_table$PSI)
sd(Suspension_coil_table$PSI)

library(dplyr)
Suspension_coil_table%>%group_by(Manufacturing_Lot)%>%summarise(Mean=mean(PSI), Median=median(PSI), Std=sd(PSI), Var=var(PSI))


t.test(subset(Suspension_coil_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
t.test(subset(Suspension_coil_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
t.test(subset(Suspension_coil_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
