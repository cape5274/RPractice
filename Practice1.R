install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(Rmisc)

data <- read.csv("Brain_Tumor_Prediction_Dataset.csv")
names(data)[names(data) == "Survival_Rate..."] <- "Survived"
is.numeric(data$Survived)
table(data$Survived)
head(data)

data$AgeGroup <- cut(data$Age, breaks=seq(0,100,10), right=FALSE,
                     labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99"))

t.test(data$Age, data$Survived)

summarySE(data, measurevar = c("Survived"), groupvars = c("AgeGroup", "Country"), na.rm = TRUE)

survival_by_age <- data %>%
  group_by(AgeGroup) %>%
  summarise(Avg_SurvivalRate = mean(`Survived`, na.rm=TRUE))

ggplot(data, aes(x = Age, y = Survived)) +
  geom_rug(color = "skyblue", size = 3, alpha=.005)

barplot(survival_by_age$Avg_SurvivalRate,
        names.arg = survival_by_age$AgeGroup,
        main = "Average Survival Rate by Age Group",
        ylab = "Average Survival Rate (%)",
        col = "skyblue",
        ylim = c(0,100))
