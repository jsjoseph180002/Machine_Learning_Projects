library(ggplot2)
library(tidyverse)
library(dplyr)
library(maps)
library(ggpubr)

#reading data sets
URSUS_Civilian_Officer_2018 <- read.csv("URSUS_Civilian-Officer_2018.csv")
URSUS_Civilian_Officer_2019 <- read.csv("URSUS_Civilian-Officer_2019.csv")
URSUS_Civilian_Officer_2017 <- read.csv("URSUS_Civilian-Officer_2017.csv")
URSUS_Civilian_Officer_2016 <- read.csv("URSUS_Civilian-Officer_2016.csv")

incident_2019 <- read.csv("incident_2019.csv")
incident_2018 <- read.csv("incident_2018.csv")
incident_2017 <- read.csv("incident_2017.csv")
incident_2016 <- read.csv("incident_2016.csv")

#cleaning incident IDs
colnames(incident_2019)[1] <- "INCIDENT_ID"
incident_2017$PRIMARY_AGENCY_INDICATOR <- NULL 
incident_2018$NUM_INVOLVED_AGENCIES <- NULL 
incident_2019$NUM_INVOLVED_AGENCIES <- NULL 
colnames(incident_2016) <- toupper(colnames(incident_2016))
incident_2016$COUNTY <- substr(incident_2016$COUNTY, 1, nchar(incident_2016$COUNTY) - 6)

#combining data sets
total_incident <- rbind(incident_2019, incident_2018, incident_2017, incident_2016)

#getting factor variable 
URSUS_Civilian_Officer_2019$Gender <- factor(URSUS_Civilian_Officer_2019$Gender, levels = c("male", "female"))

#Adding table of year 
URSUS_Civilian_Officer_2019$year <- 2019
URSUS_Civilian_Officer_2018$year <- 2018
URSUS_Civilian_Officer_2017$year <- 2017 
URSUS_Civilian_Officer_2016$year <- 2016


#factor variable for gender 
URSUS_Civilian_Officer_2018$Gender <- factor(URSUS_Civilian_Officer_2018$Gender, levels = c("male", "female"))
URSUS_Civilian_Officer_2017$Gender <- factor(URSUS_Civilian_Officer_2017$Gender, levels = c("male", "female"))
URSUS_Civilian_Officer_2016$Gender <- factor(URSUS_Civilian_Officer_2016$Gender, levels = c("Male", "Female"))
levels(URSUS_Civilian_Officer_2016$Gender) <- c("male", "female")

#getting rid of variables that are not common
URSUS_Civilian_Officer_2018$HI_Islander_Race <- NULL 
URSUS_Civilian_Officer_2019$HI_Islander_Race <- NULL
colnames(URSUS_Civilian_Officer_2019)[21] <- "CIVILIAN_Assaulted_Officer"
colnames(URSUS_Civilian_Officer_2019)[20] <- "CIVILIAN_Mental_Status"
colnames(URSUS_Civilian_Officer_2018)[20] <- "CIVILIAN_Mental_Status"
colnames(URSUS_Civilian_Officer_2019)[1] <- "Incident_ID"
URSUS_Civilian_Officer_2017$CIVILIAN_Crime_Qualifier <- NULL
URSUS_Civilian_Officer_2016$CIVILIAN_Crime_Qualifier <- NULL

#combining data sets 
total <- rbind(URSUS_Civilian_Officer_2019, URSUS_Civilian_Officer_2018, URSUS_Civilian_Officer_2017, URSUS_Civilian_Officer_2016)

#combine both total data sets:
colnames(total)[1] <- "INCIDENT_ID"
total <- right_join(total, total_incident, id = "INCIDENT_ID")

#Taking out irrelevant info 
total$OFFICER_Dress <- NULL 
total$Asian_Race <- NULL
total$Received_Force_Location <- NULL 
total$NUM_INVOLVED_CIVILIANS <- NULL
total$NUM_INVOLVED_OFFICERS <- NULL
total$IN_CUSTODY_REASON <- NULL
total$CONTACT_REASON <- NULL
total$ON_K12_CAMPUS <- NULL
total$CIVILIAN_K12_Type <- NULL
total$Medical_Aid <- NULL
total$MULTIPLE_LOCATIONS <- NULL
total$OFFICER_Officer_Used_Force_Reason <- NULL
total$OFFICER_On_Duty <- NULL
total$OFFICER_Officer_Used_Force <- NULL
total$CIVILIAN_Custody_Status <- NULL
total$CIVILIAN_Resistance_Type <- NULL
total$CIVILIAN_Firearm_Type <- NULL
total$CIVILIAN_Highest_Charge <- NULL
total$Injury_From_Preexisting_Condition <- NULL
total$Order_Of_Force_Specified <- NULL
total$Order_Of_Force_Str <- NULL
total$Received_Force <- NULL
total$Received_Force_Type <- NULL
total$DISCHARGE_OF_FIREARM_INDIVIDUAL <- NULL
total$DISCHARGE_OF_FIREARM_INCIDENT <- NULL
total$CIVILIAN_Mental_Status <- NULL
total$CIVILIAN_Assaulted_Officer <- NULL
total$CIVILIAN_Perceived_Armed_Weapon <- NULL
total$CIVILIAN_Confirmed_Armed_Weapon <- NULL
total$STATE <- NULL

#Making lower case 
total$Race_Ethnic_Group <- tolower(total$Race_Ethnic_Group) 

#creating race as factor 
total$Race_Ethnic_Group <- addNA(factor(total$Race_Ethnic_Group, levels = c("asian", "black", "hispanic", "white" )))
total$Race_Ethnic_Group[which(is.na.data.frame(total$Race_Ethnic_Group))] <- "other"
levels(total$Race_Ethnic_Group) <- c("asian", "black", "hispanic", "white",  "other")

#creating injury level as factor 
total$Injury_Level <- tolower((total$Injury_Level))
total$Injury_Level[which(total$Injury_Level == "serious bodily injury")] <- "serious_injury"
total$Injury_Level <- addNA(factor(total$Injury_Level, levels = c("injury", "serious_injury", "death")))
levels(total$Injury_Level) <- c("Injury", "Serious Injury", "Death", "Not Reported")

#------------------------------------------------------------------------------------------------------------------------
#Creating graphs

#RACE
#Comparing race injury levels when resistance is happened vs not
ggplot(data = subset(total, Civilian_Officer == "Civilian")) + 
  geom_bar(mapping = aes(x = Race_Ethnic_Group,fill = Injury_Level), position = "dodge") + 
  labs(x = "Race/Ethnic Group", y = "Count", fill = "Injury Level", title = "Injury Level by Race") +
  scale_fill_brewer(palette = "Dark2")

ggplot(data = subset(total, Civilian_Officer == "Civilian" & CIVILIAN_Resisted == FALSE)) + 
  geom_bar(mapping = aes(x = Race_Ethnic_Group,fill = Injury_Level), position = "dodge") +
  labs (x = "Race/Ethnic Group", y = "Count", fill = "Injury Level", title = "Injury Level by Race with No Resistance") + 
  scale_fill_brewer(palette = "Dark2") 

#Looking how being armed effects different races 
ggplot(data = subset(total, Civilian_Officer == "Civilian" & CIVILIAN_Perceived_Armed == TRUE)) + 
  geom_bar(mapping = aes(x = Race_Ethnic_Group,fill = Injury_Level), position = "dodge") + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(title = "Injury Level by Race for Armed Civilians", x = "Race/Ethnic Group", y = "Count", fill = "Injury Level")

#Looking at how difference between confirmed and non confirmed weapons based on race 
ggplot(data = subset(total, Civilian_Officer == "Civilian" )) + 
  geom_bar(mapping = aes(x = Race_Ethnic_Group,fill = CIVILIAN_Perceived_Armed), position = "fill") +
  labs(x = "Race/Ethnic Group", y = "Proportion", fill = "Civilians Perceived Armed", title = "Perceived Armed Civilians by Race") +
  scale_fill_brewer(palette = "Dark2", labels = c("Not Percieved Armed", "Perceived Armed")) 

ggplot(data = subset(total, !is.na.data.frame(total$CIVILIAN_Confirmed_Armed) &Civilian_Officer == "Civilian" )) + 
  geom_bar(mapping = aes(x = Race_Ethnic_Group,fill = CIVILIAN_Confirmed_Armed), position = "fill") +
  labs(x = "Race/Ethnic Group", y = "Proportion", fill = "Civilians Confirmed Armed", title = "Confirmed Armed Civilians by Race") +
  scale_fill_brewer(palette = "Dark2", labels = c("Not Confirmed Armed", "Confirmed Armed"))

#arrest made 
ggplot(data = subset(total, Civilian_Officer == "Civilian" & CIVILIAN_Resisted == TRUE)) + 
  geom_bar(mapping = aes(x = Race_Ethnic_Group,fill = ARREST_MADE), position = "fill") +
  labs(x = "Race/Ethnic Group", y = "Proportion", title = "Arrest Made by Race of Civilians Resisted", fill = "Arrest Made") +
  scale_fill_brewer(palette = "Dark2", labels = c("Arrest Not Made", "Arrest Made"))


#RACE OF OFFICER 
ggplot(data = subset(total, Civilian_Officer == "Officer")) + 
  geom_bar(mapping = aes(x = Race_Ethnic_Group), fill = "#7470b3") + 
  labs(x = "Race/ Ethnic Group", y = "Count", title = "Race/Ethnic Group of Officers") 


#FIREARM
#Seeing how the introduction of a weapon changes endings of encounters 
x <- ggplot(data = subset(total, Civilian_Officer == "Civilian" & Injury_Level != "Not Reported")) + 
  geom_bar(mapping = aes(x = Injury_Level, fill = CIVILIAN_Confirmed_Armed), position = "stack") +
  labs(y = "Count", x = "Injury Level", fill = "Civilian Confirmed Armed", title = "Confirmed Armed Civilians by Injury Level") +
  scale_fill_brewer(palette = "Dark2", labels = c("Not Confirmed Armed", "Confirmed Armed"))
x + coord_flip()

#Seeing how the introduction of a weapon changes endings of encounters 
ggplot(data = subset(total, Civilian_Officer == "Civilian" & Injury_Level != "Not Reported")) + 
  geom_bar(mapping = aes(x = CIVILIAN_Confirmed_Armed, fill = Injury_Level), position = "fill") + 
  labs(x = "Confirmed Armed", y = "Proportion", title = "Effect of Being Armed on Injury Outcomes", fill = "Injury Level") + 
  scale_fill_brewer(palette = "Dark2") + scale_x_discrete(labels = c("Unarmed", "Armed"))


#GENDER 
ggplot(data = subset(total, Civilian_Officer == "Civilian" & !is.na.data.frame(total$Gender) & CIVILIAN_Confirmed_Armed == TRUE)) + 
  geom_bar(mapping = aes(x = Gender,fill = Injury_Level), position = "dodge") +
  labs(y = "Count", fill = "Injury Level", title = "Effects of Gender on Injury Level") 

ggplot(data = subset(total, Civilian_Officer == "Civilian" & !is.na.data.frame(total$Gender))) + 
  geom_bar(mapping = aes(x = Gender,fill = CIVILIAN_Resisted), position = "fill") +
  labs(y = "Proportion", fill = "Civilians Resisted", title = "Proportion of Civilians Resisted with Gender") +
  scale_fill_discrete(labels = c("Not Resisted", "Resisted"))


#AGE
#Box plot of Age vs Injury Level
total$Age <- substr(total$Age, 1, 2)
total$Age <- as.numeric(total$Age)

ggplot(total) + geom_boxplot(mapping = aes(x = Injury_Level, y = Age), fill = c("#C4961A", "#f7b602","#4e84c4", "#235591")) + 
  labs(x = "Injury Level", title = "Age Distribution with Injury Level")

ggplot(data = subset(total, Civilian_Officer == "Civilian")) + 
  geom_boxplot(mapping = aes(x = Injury_Level, y = Age), fill = c("#C4961A", "#f7b602","#4e84c4", "#235591")) +
  labs(x = "Injury Level", title = "Age Distribution of Civilians with Injury Level")

#Box plot of Age vs Confirmed Armed 
ggplot(data = subset(total, !is.na.data.frame(total$CIVILIAN_Confirmed_Armed) & Civilian_Officer == "Civilian")) + 
  geom_boxplot(mapping = aes(x = CIVILIAN_Confirmed_Armed, y = Age), fill = c("#C4961A", "#4E84C4")) +
  labs(x = "Civilian Confirmed Armed", title = "Age Distribution with Armed Civilians") +
  scale_x_discrete(labels = c("Not Confirmed Armed", "Confirmed Armed"))

#Box plot of age and resistance with Injury Level
ggplot(subset(total, !is.na.data.frame(total$CIVILIAN_Resisted))) + 
  geom_boxplot(mapping = aes(x = CIVILIAN_Resisted, y = Age, fill = Injury_Level)) + 
  labs(x = "Civilians Resisted", fill = "Injury Level", title = "Age Distribution by Injury Level for Civilian Resisted") +
  scale_fill_manual(values = c("#C4961A", "#f7b602","#4e84c4", "#235591")) +
  scale_x_discrete(labels = c("Not Resisted", "Resisted"))

#Box plot of age and resistance
ageresist <- ggplot(subset(total, !is.na.data.frame(total$CIVILIAN_Resisted) & Civilian_Officer == "Civilian")) + 
  geom_boxplot(mapping = aes(x = CIVILIAN_Resisted, y = Age), fill = c("#C4961A", "#4E84C4")) +
  labs(x = "Civilians' Resistance", title = "Resistance Distribution with Age") +
  scale_x_discrete(labels = c("Not Resisted", "Resisted"))

#Age Bar plot
total$Age <- as.character(total$age)
total$Age[which(total$Age == "0")] <- "0-9"
total$Age[which(total$Age == "10" | total$Age == "17")] <- "10-17"
total$Age[which(total$Age == "18")] <- "18-20"
total$Age[which(total$Age == "21")] <- "21-25"
total$Age[which(total$Age == "26")] <- "26-30"
total$Age[which(total$Age == "31")] <- "31-35"
total$Age[which(total$Age == "36")] <- "36-40"
total$Age[which(total$Age == "41")] <- "41-45"
total$Age[which(total$Age == "46")] <- "46-50"
total$Age[which(total$Age == "51")] <- "51-55"
total$Age[which(total$Age == "56")] <- "56-60"
total$Age[which(total$Age == "61")] <- "61-65"
total$Age[which(total$Age == "66")] <- "66-70"
total$Age[which(total$Age == "71")] <- "71-75"
total$Age[which(total$Age == "76")] <- "76-80"
total$Age[which(total$Age == "81")] <- "81-85"
total$Age[which(total$Age == "86")] <- "86-90"

agebar <- ggplot(data = subset(total, Civilian_Officer == "Civilian" & !is.na.data.frame(total$Age))) + 
  geom_bar(mapping = aes(x=as.factor(Age)), fill = "steelblue") +
  labs(x = "Age", y = "Count", title = "Age of Victims") + theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggarrange(ageresist, agebar, nrow = 2, ncol = 1)


#COUNTY
#map for county
total$COUNTY <-toupper(total$COUNTY)

co_num <- data.frame(table((total$COUNTY[which(total$Civilian_Officer == "Civilian")])))

colnames(co_num)[1] <- "subregion"
co_num$subregion <- tolower(co_num$subregion)

pop <- read_excel("pop.xlsx")
colnames(pop)[1] <- "subregion"
colnames(pop)[2] <- "population"

pop$subregion <- substr(pop$subregion, 2, nchar(pop$subregion) - 19)
pop$subregion <- tolower(pop$subregion)

pop$population <- as.numeric(pop$population)
co_num$population <- as.numeric(co_num$population)
co_num <- right_join(co_num, pop, id = "subregion")

co_num$fre <- co_num$Freq / (co_num$population / 100000)
co_num$fre[which(is.na.data.frame(co_num$fre))] <- 0.1

#calculating standard deviation
sd(co_num$fre)

counties <- map_data("county")
ca_county <- counties%>%
  filter(region == "california")
head(ca_county)
ggplot(ca_county) + geom_polygon(mapping = aes(x = long, y = lat)) + coord_quickmap()
ggplot(ca_county) + geom_polygon(mapping = aes(x = long, y = lat, group = group), color = "white") + coord_quickmap()
borders2 <- left_join(ca_county, co_num, by = "subregion")

borders2$bin <- cut((borders2$fre) / 5,
                    breaks = 0:6, labels = c("0-4.99", "5-9.99", "10-14.99", "15-19.99", "20-24.99", "25+"))
ggplot(borders2) + geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = bin), color = "black") + 
  coord_quickmap() + scale_fill_brewer(palette = "Oranges", direction = 1) + 
  labs(title = "Incidents per 100,000 People", fill = "Incidents") + theme_void()

#map for income
total$COUNTY <-toupper(total$COUNTY)

counties <- map_data("county")
ca_county <- counties%>%
  filter(region == "california")
head(ca_county)
ggplot(ca_county) + geom_polygon(mapping = aes(x = long, y = lat)) + coord_quickmap()
ggplot(ca_county) + geom_polygon(mapping = aes(x = long, y = lat, group = group), color = "white") + coord_quickmap()

Counties_by_Income <- read_excel("Counties by Income.xlsx")
colnames(Counties_by_Income)[1] <- "subregion"

Counties_by_Income$subregion <- tolower(Counties_by_Income$subregion)
Counties_by_Income$Income <- as.numeric(Counties_by_Income$Income)
borders <- left_join(ca_county, Counties_by_Income, by = "subregion")
borders$bin <- cut((borders$Income) / 20000,
                   breaks = 1:6, labels = c("0-39,999", "40,000-59,999", "60,000-79,999", "80,000-99,999", "100,000+"))
ggplot(borders) + geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = bin), color = "black") + 
  coord_quickmap() + scale_fill_brewer(palette = "Oranges", direction = 1) + 
  labs(title = "Income of California Counties", fill = "Income") + theme_void() 

#scatter plot for income
ggplot(data = borders2, mapping = aes(x = borders$Income, y = fre)) + geom_point() + 
  geom_smooth(color = "Orange") +
  labs(title = "Number of Incidents per Income", x = "Income", y = "Number of Incidents")


#TIME
#Making Histogram of time of incidents
count <- 0
for(val in incident_2016$INCIDENT_TIME_STR) {
  count <- count + 1
  if(nchar(val) == 3) {
    incident_2016$INCIDENT_TIME_STR[count] <- paste0("0", incident_2016$INCIDENT_TIME_STR[count])
  } else if(nchar(val) == 2) {
    incident_2016$INCIDENT_TIME_STR[count] <- paste0("00", incident_2016$INCIDENT_TIME_STR[count])
  } else if(nchar(val) == 1) {
    incident_2016$INCIDENT_TIME_STR[count] <- paste0("000", incident_2016$INCIDENT_TIME_STR[count])
  }
}

count <- 0
for(val in total$INCIDENT_TIME_STR) {
  count <- count + 1
  if(total$year[count] == 2016) {
    if(nchar(val) == 3) {
      total$INCIDENT_TIME_STR[count] <- paste0("0", total$INCIDENT_TIME_STR[count])
    } else if(nchar(val) == 2) {
      total$INCIDENT_TIME_STR[count] <- paste0("00", total$INCIDENT_TIME_STR[count])
    } else if(nchar(val) == 1) {
      total$INCIDENT_TIME_STR[count] <- paste0("000", total$INCIDENT_TIME_STR[count])
    }
  } 
}

count <- 0
for(val in total$INCIDENT_TIME_STR) {
  count <- count + 1
  if(total$year[count] == 2017 & nchar(val) == 4) {
    total$INCIDENT_TIME_STR[count] <- paste0("0", total$INCIDENT_TIME_STR[count])
  }
}

total$INCIDENT_TIME_STR <- substr(total$INCIDENT_TIME_STR, 1, 2)
total$INCIDENT_TIME_STR <- as.numeric(total$INCIDENT_TIME_STR)

ggplot(total) + geom_histogram(mapping = aes(x = INCIDENT_TIME_STR), bins = 24, fill = c("#e7298b")) +
  labs(x = "Time of Incident", y = "Count", title = "Time of Incidents")

#Bar plot of Incident Time and Crime Report Filed
total$INCIDENT_DATE_STR <- substr(total$INCIDENT_DATE_STR, 1, 2)
total$INCIDENT_DATE_STR <- factor(total$INCIDENT_DATE_STR, levels = c("1/", "2/", "3/", "4/", "5/", "6/", "7/", "8/", "9/", "10", "11", "12"))
levels(total$INCIDENT_DATE_STR) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")

ggplot(data = subset(total, Civilian_Officer == "Civilian")) + 
  geom_bar(mapping = aes(x = INCIDENT_DATE_STR, fill = CRIME_REPORT_FILED), position = "fill") +
  labs(x = "Incident Month", y = "Proportion", fill = "Crime Report Filed", title = "Crime Report Status by Incident Date") +
  scale_fill_manual(values = c("#e7298b", "#7470b3"), labels = c("Not Filed", "Filed"))

