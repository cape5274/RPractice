install.packages("dplyr")
library(dplyr)

data <- read.csv(file.choose())
names(data)[names(data) == "Survival_Rate..."] <- "Survived"
table(data$Survived)
head(data)

data$AgeGroup <- cut(data$Age, breaks=seq(0,100,10), right=FALSE,
                     labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99"))

survival_by_age <- data %>%
  group_by(AgeGroup) %>%
  summarise(Avg_SurvivalRate = mean(`Survived`, na.rm=TRUE))

barplot(survival_by_age$Avg_SurvivalRate,
        names.arg = survival_by_age$AgeGroup,
        main = "Average Survival Rate by Age Group",
        ylab = "Average Survival Rate (%)",
        col = "skyblue",
        ylim = c(0,100))
