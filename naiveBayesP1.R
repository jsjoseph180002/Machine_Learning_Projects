#--------------------------------------------------------------------------
#SET UP AND DATA EXPLORATION:
#--------------------------------------------------------------------------

#getting data 
df <- read.csv("titanic_project.csv")

#changing pclass, sex and survived to factors
df$pclass <- as.factor(df$pclass)
df$survived <- as.factor(df$survived)
df$sex <- as.factor(df$sex)

#dividing between train and test
train <- df[1:900,]
test <- df[901:1046,]

#getting the structure of the data (Exploration #1)
str(df) 

#seeing the distribution of sexes to check if data is balanced (Exploration #2)
summary(df$sex)

#seeing the average age (Exploration #3)
mean(df$age)

#seeing the average age of died (Exploration #4)
mean(df$age[df$survived == 0])

#seeing last few values (Exploration #5)
tail(df)

#Box plots for survived with the age (Graph #1)
plot(df$survived, df$age, main= "survived by age", xlab= "survived", ylab= "age")

#Box plots for survived with the age (Graph #2)
plot(df$age~df$pclass, varwidth = TRUE, main= "age by pclass", xlab= "pclass", ylab="age")



#--------------------------------------------------------------------------
#MAKING MODEL
#--------------------------------------------------------------------------

#time start 
start_time <- Sys.time()

#making model 
library(e1071)
nb1 <- naiveBayes(survived~pclass+sex+age, data = train)

#time end
end_time <- Sys.time()

#Getting time
end_time - start_time

#outputting the model
nb1

#--------------------------------------------------------------------------
#TESTING MODEL
#--------------------------------------------------------------------------

#getting predictions
pred <- predict(nb1, newdata = test, type = "class")

#Getting and printing testing metrics
library(caret)
cm <- confusionMatrix(pred, test$survived)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]

