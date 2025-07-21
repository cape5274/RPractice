library(ggplot2)
install.packages("Rmisc")
library(Rmisc)
library(dplyr)

# Being able to choose our data file
df <- read.csv(file.choose())
head(df) 

#We do have some missing values for R here and so we are triying to clean them up 
# Data Cleaning 

mean(df$SkinThickness)
mean(df$BloodPressure)
mean(df$Insulin)
# Means with eh additional zeros 

#Taking out the zeros from the dataset 
df_filtered <- df %>% 
  filter(SkinThickness != 0 & BloodPressure != 0 & Insulin != 0 )

#Passing the data through summarySr in order to plot 

summarySE(data = df_filtered, measurevar = 'BloodPressure', na.rm = TRUE)
summarySE(data = df_filtered, measurevar = 'Insulin', na.rm = TRUE)
summarySE(data = df_filtered, measurevar = 'SkinThickness', na.rm = TRUE)

#Finding our correlations between the variables 
cor.test(df_filtered$SkinThickness,df_filtered$Insulin)
cor.test(df_filtered$BloodPressure, df_filtered$Insulin)
cor.test(df_filtered$SkinThickness, df_filtered$BloodPressure)

#Our findings based off the p_values
#1. Skin Thickness vs insulin - weak correlation based off correlation coefficient but was significant
#2. BP vs Insulin - weak correlation and not significant based off correlation coefficent and p_values
#3. BP and Skin Thickness - p_value is significant! p-value = 3.145e-06


#Creating our plot using significant relationship - Skin Thickness vs Blood Pressure
ggplot(df_filtered, aes(x = SkinThickness, y = BloodPressure)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SkinThickness vs BloodPressure",
       subtitle = "Significant positive correlation",
       x = "SkinThickness",
       y = "Blood Pressure")

#OTHER plots 

#SkinThickness vs Insulin
ggplot(df_filtered, aes(x = SkinThickness, y = Insulin)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SkinThickness vs Insulin",
       subtitle = "Weak correlation",
       x = "SkinThickness",
       y = "Insulin")


#BP vs Insulin
ggplot(df_filtered, aes(x = BloodPressure, y = Insulin)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "BloodPressure vs Insulin",
       subtitle = "Weak and not significant correlation",
       x = "Blood Pressure",
       y = "Insulin")


