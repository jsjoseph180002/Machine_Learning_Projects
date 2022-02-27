#--------------------------------------------------------------------------
#SET UP AND DATA EXPLORATION:
#--------------------------------------------------------------------------

#getting data 
df <- read.csv("titanic_project.csv")

#changing pclass and survived to factors
df$pclass <- as.factor(df$pclass)
df$survived <- as.factor(df$survived)

#dividing between train and test
train <- df[1:900,]
test <- df[901:1046,]

#getting the structure of the data (Exploration #1)
str(df)

#seeing first few rows (Exploration #2)
head(df)

#getting the contrast for the pclass (Exploration #3)
contrasts(df$pclass)

#summary on pclass (Exploraton #4)
summary(df$pclass)

#summary on survived to see if data is balanced (Exploration #5)
summary(df$survived)

#cd plot for survived and pclass (Graph #1)
plot(df$pclass, main= "pclass plot", xlab= "pclass")

#plot for survived vs pclass (Graph #2)
plot(df$survived~df$pclass, main= "survival rate by pclass", xlab= "pclass", ylab = "survived")

#--------------------------------------------------------------------------
#MAKING MODEL
#--------------------------------------------------------------------------

#time start 
start_time <- Sys.time()

#making model 
glm1 <- glm(survived~pclass, data=train, family=binomial)

#time end
end_time <- Sys.time()

#Getting time
end_time - start_time


summary(glm1)

#printing coefficients
glm1$coefficients[1]
glm1$coefficients[2]
glm1$coefficients[3]

#--------------------------------------------------------------------------
#TESTING MODEL
#--------------------------------------------------------------------------

#getting predictions
probs <- predict(glm1, newdata = test, type = "response")
pred <- ifelse(probs>0.5, 1, 0)

#getting metrics through confusion matrix function
library(caret)
cm <- confusionMatrix(as.factor(pred), reference=test$survived)
cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]

