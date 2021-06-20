library(class)
library(foreign)
library(dplyr)
library(broom)
library(caTools)
library(ggplot2)
library(readr)
library(tm)

# upload csv
dod <- read.csv("dodgers.csv")

# look at STrture of dataframe
str(dod)

# glimpse at data of dataframe
glimpse(dod)

# create column to indicate promo 
dod$promo = dod$cap
dod$promo <- dod$cap=='NO' & dod$bobblehead=='NO' & dod$shirt=='NO' & dod$fireworks=='NO'
summary(dod$promo)

#FALSE indicates promootion occured whereas True indicates no promotion


## Box plot to explore attendance by day of week
ggplot(dod, aes(x=day_of_week, y=attend)) + 
  geom_boxplot(outlier.color="red", outlier.shape=8,
               outlier.size=4, color = 'blue',
               fill='#A4A4A4') + labs(title="Dodger Attendnece",x="Day of Week", y = "Total Attendance")

#Outlier for monday and wednesday observed, overall monday is the day with the average lowest attendence

#Box plot for attendance by month
ggplot(dod, aes(x=month, y=attend)) + 
  geom_boxplot(outlier.color="red", outlier.shape=8,
               outlier.size=4, color = 'blue',
               fill='#A4A4A4') + labs(title="Dodger Attendnece",x="Day of Week", y = "Total Attendance")

#lowest attendance is in the month of october followed by spetember with an outlier observed that month as well


# chnage label for facet graph
dod$promo <- factor(dod$promo, levels = c(FALSE, TRUE), 
                  labels = c("PROMO", "NO PROMO"))
str(dod$promo)


#Evaluate attendance by promotion and temperature
ggplot(dod, aes(x=temp, y=attend )) + 
  geom_point() + 
  facet_wrap(dod$promo) + 
  ggtitle("Attendance by Temperature and Promotion") +
  theme(plot.title = element_text(lineheight=4, color="red")) +
  xlab("Temperature") +
  ylab("Attendance")

# there was not any attendance lower than 30,000 when a promo was offered and none lower than 30,000 when temperature was 70 or above. 
# intial estimate would be to look at which days had temperature lower than 70 and which team played with no promo
# and attendence under 30,000 and look at difference if same team played with promo with wather greater than 70

# Create a model with romotion temperature interaction
mod <- lm(attend ~ promo:temp, data = dod)
summary(mod)

# visulize model  with temp and promo as interactions
ggplot(data =dod ,aes(y = attend, x = temp, color = promo)) + 
  geom_line() + 
  geom_smooth(method ="lm",se =FALSE)

#sweet spot for promo appears to be between 65 amd 70 degress 

# visulize model  with opponent and promo as interactions
ggplot(data =dod ,aes(y = attend, x = opponent, color = promo)) + 
  geom_point() + 
  geom_smooth(method ="lm",se =FALSE)

# visulize model  with temp and promo as interactions divided by opponent facet
ggplot(data =dod ,aes(y = attend, x = temp, color = promo)) + 
  geom_point() + 
  facet_wrap(dod$opponent) +
  geom_smooth(method ="lm",se =FALSE)

# visulize model  with temp and fireworks as interactions 
ggplot(data =dod ,aes(y = attend, x = temp, color = fireworks)) + 
  geom_point() + 
  geom_smooth(method ="lm",se =FALSE)

#fireworks do not boost attendance when temp is below 70 degrees

# visulize model  with temp and cap as interactions 
ggplot(data =dod ,aes(y = attend, x = temp, color = cap)) + 
  geom_point() + 
  geom_smooth(method ="lm",se =FALSE)

#inconclusive due to lack of data points


# visulize model  with temp and bobblehead as interactions 
ggplot(data =dod ,aes(y = attend, x = temp, color = bobblehead)) + 
  geom_point() + 
  geom_smooth(method ="lm",se =FALSE)

# bobble head pomotion did much bettter than the others

# visulize model  with temp and shirt as interactions 
ggplot(data =dod ,aes(y = attend, x = temp, color = shirt)) + 
  geom_point() + 
  geom_smooth(method ="lm",se =FALSE)

# inconclusive as not enough data points for shirt 

# visulize model  with temp and bobblehead as interactions divided by opponent facet
ggplot(data =dod ,aes(y = attend, x = temp, color = bobblehead)) + 
  geom_point() + 
  facet_wrap(dod$opponent) +
  geom_smooth(method ="lm",se =FALSE)

#bobblehead outperformed games at same temp for same oppoenent 

#create formula leveraging bobblehead and other values to help predict
mod <- glm(attend ~ month + day_of_week + bobblehead, data = dod)
summary(mod)

# slit data for training
split <- sample.split(dod, SplitRatio = 0.8)
train <- subset(dod, split == "TRUE")
test <- subset(dod, split == "FALSE")

# set predict veriables for response of test with model
res <- predict(mod,test,type="response")
res <- predict(mod,train,type="response")

# train and output results
table(Actual_Value = train$attend, Predicted_Value = res > 0.5)

#model is viable as all test came back as true.  


#lets try and observe the best time to tomplement the bobble head promotion
ggplot(data =dod ,aes(y = attend, x = bobblehead)) +  
  geom_point() + 
  facet_wrap(dod$month) + 
  geom_smooth(method ="lm",se =FALSE)

# data suggests that bobblehead has best attendance when compared to non bobblehead in August


ggplot(data =dod ,aes(y = attend, x = bobblehead)) +  
  geom_point() + 
  facet_wrap(dod$day_of_week) + 
  geom_smooth(method ="lm",se =FALSE)

# data suggests that bobblehead has best attendance when compared to non bobblehead on Thurday

# My final assessment is that bobblehead promotion should occur on a Thursday in August against the Snakes
