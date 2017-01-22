
#load data and check data
airfoil_data <- read.csv(file.choose(), header = TRUE, sep=",")
View(airfoil_data)

names(airfoil_data)
dim(airfoil_data)
str(airfoil_data)

#check missing values in each column
colSums(is.na(airfoil_data))
table(is.na(airfoil_data))
# Total NA values in the dataset
sum(is.na(airfoil_data))

#Correlation
cor(airfoil_data)

regmodel <- lm(Sound_pressure_level ~ ., data=airfoil_data)
summary(regmodel)

lm(formula = Sound_pressure_level ~ ., data = airfoil_data)

#Improve the model
#set graphic output
par(mfrow=c(2,2))

#create residual plots
plot (regmodel)

#The first plot (Residuals vs Fitted) shows some level heteroskedasticity, so convert the predicted variable to log

regmodel <- update(regmodel, log(Sound_pressure_level)~.)
summary(regmodel)

lm(formula = log(Sound_pressure_level) ~ Frquency.Hz. + Angle_of_Attack +
     Chord_Length + Free_stream_velocity + Displacement, data = airfoil_data)

# Check the final evaluation metric
# Divide the dataset into training and testing data

set.seed(1)

d <- sample(x = nrow(airfoil_data),size = nrow(airfoil_data)*0.7)

train <- airfoil_data[d,]
nrow(train) # 1052 rows
test <- airfoil_data[-d,]
nrow(test) # 451 rows

#Train model
regmodel <- lm(log(Sound_pressure_level)~., data=train)
summary(regmodel)

#Test model
regpred <- predict(regmodel,test)

#convert back to original value
regpred <- exp(regpred)

library(Metrics)
rmse(actual = test$Sound_pressure_level, predicted = regpred)

d <- boxplot(train$Displacement,varwidth = T,outline = T,border = T,plot = T)
d$out #enlist outlier observations

