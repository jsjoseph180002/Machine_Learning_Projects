library("ggplot2")
library(car)
library(MASS)


# DATA CLEANING 
#-----------------------------------
Data <- read.csv("cancer_reg.csv")
str(Data)

#Finding amount of Na's per column
for (i in colnames(Data)) {
  x <- sum(is.na(Data[,i]))
  print(i)
  print(x) 
}

#Getting rid of columns with Na's 
Data$PctPrivateCoverageAlone <- NULL 
Data$PctEmployed16_Over <- NULL
Data$PctSomeCol18_24 <- NULL 

#Separting Income Decile 
Data$binnedInc <- as.factor(Data$binnedInc)
str(Data$binnedInc)
levels(Data$binnedInc) <- c("2","3", "4", "5","6","7","8","9","10","1")
Data$binnedInc <- relevel(Data$binnedInc, "1")
str(Data$binnedInc)



#INITIAL DATA EXPLORATION
#-----------------------------------

#Economic Exploration  
ggplot ( data = Data) + geom_point(mapping=aes(x =medIncome, y = TARGET_deathRate),color="#9999CC")   +
  labs(x="Median Income", y= "Death Rate ", title = "Death Rate vs County Median Income")    
ggplot ( data = Data ) + geom_point(mapping=aes(x =povertyPercent, y = TARGET_deathRate), color="#CC6666") +
  labs(x="Poverty Percent", y= "Death Rate ", title = "Death Rate vs County Poverty Percent") 
ggplot ( data = Data ) + geom_point(mapping=aes(x =PctUnemployed16_Over, y = TARGET_deathRate), color="#66CC99") + 
  labs(x="Unemployment 16 and Over", y= "Death Rate ", title = "Death Rate vs Unemployment 16 and older") 

ggplot(Data, aes(x = binnedInc, y = TARGET_deathRate, fill=binnedInc)) + 
  geom_bar(stat = "summary", fun = "mean") + labs(title = "Death Rate by binned Income- (Rates Per 100,000)", y= "Death Rate", x = "Binned Income")
ggplot(Data, aes(x = binnedInc, y = incidenceRate, fill=binnedInc)) + 
  geom_bar(stat = "summary", fun = "mean") + labs(title = "Cancer Incidence Rate by binned Income- (Rates Per 100,000)", y= "Incidence Rate", x = "Binned Income")


#Health Exploration
ggplot ( data = Data ) + geom_point(mapping=aes(x =incidenceRate, y = TARGET_deathRate), color="#9999CC") + 
  labs(x="Incidence Rate", y= "Death Rate ", title = "Death Rate vs Incidence Rate")
ggplot ( data = Data ) + geom_point(mapping=aes(x =avgAnnCount, y = TARGET_deathRate)) + 
  labs(x="Avg Annual Count", y= "Death Rate ", title = "Death Rate vs Avg Annual Count")


#Age Exploration
ggplot ( data = Data ) + geom_point(mapping=aes(x =MedianAge, y = TARGET_deathRate)) + 
  labs(x="Median Age", y= "Death Rate ", title = "Death Rate vs Median Age")


#Educational Data overview plots
#PctNoHS18_24, PctBachDeg18_24, PctHS18_24
NoHsPlot <- ggplot(Data, aes(x = PctNoHS18_24)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Pop. % aged 18-24 without high school diploma", x  =  "Percent", y = "Number of Counties")
NoHsPlot
HsPlot <- ggplot(Data, aes(x = PctHS18_24)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Pop. % aged 18-24 with high school diploma", x  =  "Percent", y = "Number of Counties")
HsPlot
BachPlot <- ggplot(Data, aes(x = PctBachDeg25_Over)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Pop. % aged 25+ with bachelor's degree", x  =  "Percent", y = "Number of Counties")
BachPlot

#Insurance Exploration
ggplot() +
  labs(title = "Insurance Coverage", x = "Percent") +
  geom_histogram(binwidth = 1, data = Data, aes(x=PctPrivateCoverage, fill = 'PrivateCoverage', alpha = 0.6)) +
  geom_histogram(binwidth = 1, data = Data, aes(x=PctEmpPrivCoverage, fill = 'EmployeePrivateCoverage', alpha = 0.6)) +
  geom_histogram(binwidth = 1, data = Data, aes(x=PctPublicCoverageAlone, fill = 'PublicCoverageAlone', alpha = 0.6)) +
  geom_histogram(binwidth = 1, data = Data, aes(x=PctPublicCoverageAlone, fill = 'PublicCoverageAlone', alpha = 0.6))

ggplot(Data)+
  geom_point(mapping = aes(x = PctPrivateCoverage, y = TARGET_deathRate), color = "dark green", alpha = 0.6) +
  labs( title = "Percent Private Coverage vs Target Death Rate", x= "Percent Private Coverage", y="Death Rage") 
ggplot ( data = Data ) + geom_point(mapping=aes(x =PctPublicCoverageAlone, y = TARGET_deathRate), color="blue") + 
  labs(x="Public Coverage Alone", y= "Death Rate ", title = "Death Rate vs Percent Public Coverage Alone")
ggplot ( data = Data ) + geom_point(mapping=aes(x =PctEmpPrivCoverage, y = TARGET_deathRate), color="red") + 
  labs(x="Public Employee Private Coverage", y= "Death Rate ", title = "Death Rate vs Percent Employee Private Coverage ")

#Overall deathrate Exploration
ggplot() + geom_histogram(binwidth = 1, data = Data, aes(x=TARGET_deathRate)) + labs(title= "Death Rate per 100,000 Distribution", x= "Death Rate")



#INITIAL MODEL
#-----------------------------------
Data$Geography <- NULL 
Data$avgDeathsPerYear <- NULL
Data$binnedInc <- NULL
Data$avgAnnCount <- NULL
Data$popEst2015 <- NULL

#Full Model
full_model <- lm(TARGET_deathRate ~., data = Data)
summary(full_model)

#Backwards Selection
Data$PctPublicCoverageAlone <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data = Data)
summary(backwards_fit)
Data$MedianAgeFemale <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data = Data)
summary(backwards_fit)
Data$studyPerCap <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit) 
Data$MedianAge <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit) 
Data$AvgHouseholdSize <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit) 
Data$PctAsian <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit) 
Data$PctBlack <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit) 
Data$PctPublicCoverage <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit) 
Data$PctBachDeg18_24 <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit)
Data$medIncome <- NULL
backwards_fit <- lm(TARGET_deathRate ~., data=Data)
summary(backwards_fit)

#running anova on the model to see if there was improvment
anova(backwards_fit, full_model)

#preforming vif to look for multicolinearity
vif(backwards_fit)

#Residual Analysis
par(mfrow=c(1,2))
hist(studres(backwards_fit), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(backwards_fit, main= "QQ plot of StuResiduals vs t Quantiles")
par(mfrow=c(1,1))

residualPlot(backwards_fit, type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, main="Rstudent Residuals vs Fitted Values") 

#TRANSFORMATION ON MODEL
#-----------------------------------

ggplot ( data = Data) + geom_point(mapping=aes(x =incidenceRate, y = TARGET_deathRate)) 
ggplot ( data = Data) + geom_point(mapping=aes(x =povertyPercent, y = TARGET_deathRate)) 
ggplot ( data = Data) + geom_point(mapping=aes(x =MedianAgeMale, y = TARGET_deathRate)) 
ggplot ( data = Data) + geom_point(mapping=aes(x =PercentMarried, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctHS25_Over, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctBachDeg25_Over, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =log(1/(PctBachDeg25_Over)), y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctHS18_24, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctUnemployed16_Over, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctPrivateCoverage, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctEmpPrivCoverage, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctWhite, y = TARGET_deathRate)) + geom_smooth(method = "lm", aes(x = PctWhite, y = TARGET_deathRate)) + labs(title =  "Death Rate vs Percent White", x= "Percent White", y="Death Rate")
ggplot ( data = Data) + geom_point(mapping=aes(x =PctOtherRace, y = TARGET_deathRate)) + geom_smooth(method = "lm", aes(x = PctOtherRace, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =PctMarriedHouseholds, y = TARGET_deathRate)) + geom_smooth(method = "lm", aes(x = PctMarriedHouseholds, y = TARGET_deathRate))
ggplot ( data = Data) + geom_point(mapping=aes(x =BirthRate, y = TARGET_deathRate)) + geom_smooth(method = "lm", aes(x = BirthRate, y = TARGET_deathRate))

Data$PctBachDeg25_Over <- log(1/(Data$PctBachDeg25_Over))
fitTransformed <- lm(TARGET_deathRate ~., data=Data)
summary(fitTransformed)

#Code for transformation:
#ggplot ( data = Data) + geom_point(mapping=aes(x =log(1/PctBachDeg25_Over), y = TARGET_deathRate), color = "orange") +
  #labs(x="% of Pop. with bachelor's Degree", y= "Death Rate ", title = "Inverse log of % of Pop. 25+ with Bachelors Degree vs Death Rate") +
  #geom_smooth(method = "lm", aes(x = log(1/PctBachDeg25_Over), y = TARGET_deathRate))
#summary(lm(log(1/Data$PctBachDeg25_Over) ~ Data$TARGET_deathRate))


anova(fitTransformed, backwards_fit)

vif(fitTransformed)

#residual analysis
par(mfrow=c(1,2))
hist(studres(fitTransformed), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(fitTransformed, main= "QQ plot of StuResiduals vs t Quantiles")

par(mfrow = c(1, 1))

residualPlot(fitTransformed, type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, main="Rstudent Residuals vs Fitted Values")

#INFLUENTIAL ANLYSIS 
#-----------------------------------
#Graphing standardized residuals
#Find the range of the values.
range(stdres(fitTransformed))
#Set the range of y axis with argument ylim. Centering to zero is recomended.
barplot(height = stdres(fitTransformed), names.arg = 1:3047, 
        main = "Standardized Residuals", xlab = "Index", 
        ylab = "Standardized Resid", ylim=c(-8,8))
#Add cutoff values. Either 2 or 3 can be chosen.
abline(h=3, col = "Red", lwd=2)
abline(h=-3, col = "Red", lwd=2)


#Graphing studentized residuals
range(studres(fitTransformed))
#Set the range of y axis with argument ylim. Centering to zero is recomended.
barplot(height = studres(fitTransformed), names.arg = 1:3047, 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim=c(-8,8))
#Add cutoff values. Either 2 or 3 can be chosen.
abline(h=3, col = "Red", lwd=3)
abline(h=-3, col = "Red", lwd=3)


#Graphing R student residuals
RStudent <- rstudent(fitTransformed)
#Find the range of the values.
range(RStudent)
#Set the range of y axis with argument ylim. Centering to zero is recommended.
barplot(height = RStudent, names.arg = 1:3047, 
        main = "R Student Residuals", xlab = "Index", 
        ylab = "R Student Resid", ylim=c(-8,8))
cor.level <- 0.05/(2*3047)
cor.qt <- qt(cor.level, 3031, lower.tail=F) 
abline(h=cor.qt , col = "Red", lwd=3)
abline(h=-cor.qt , col = "Red", lwd=3)

#influential measures
myInf <- influence.measures(fitTransformed)
summary(myInf)

#plot of hat values
influenceIndexPlot(fitTransformed, vars = c("hat"))

hatCutoff <- (2 * 17) / 3047
outliers_index <- which((RStudent <= -cor.qt | RStudent >= cor.qt) | myInf$infmat[,'hat'] > hatCutoff)





#MODEL POST OUTLIERS
#-----------------------------------
#removing outliers
noOutliersData <- Data[-outliers_index, ]

#fitting model without outliers
outlier_fit <- lm(TARGET_deathRate ~., data=noOutliersData)
summary(outlier_fit)
outlier_fit <- lm(TARGET_deathRate ~.-PctWhite, data=noOutliersData)
summary(outlier_fit)

#residual analysis
par(mfrow=c(1,2))
hist(studres(outlier_fit), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(outlier_fit)

par(mfrow = c(1, 1))

residualPlot(outlier_fit, type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

#influence graphs
influenceIndexPlot(outlier_fit)
dfbetasPlots(outlier_fit, intercept = T)








