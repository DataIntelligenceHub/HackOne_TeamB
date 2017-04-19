library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(readr)
library(lubridate)
library(rpart)
library(rattle)
library(car)
library(caret)
library(corrplot)
library(rpart.plot)
library(plotly)

#Retrieving data
train<- read_csv("train.csv")
stores <- read_csv("stores.csv")
features <- read_csv("features.csv")

View(train)

#join data for train and store
stores$Store <- factor(stores$Store)
train$Store <- factor(train$Store)
train <- full_join(train,stores,by=c("Store"))

View(train)
View(stores)

length(unique(train$Date))
#weeknum
train$WeekNum <- as.numeric(format(train$Date+3,"%U"))
temp <- as.numeric(format(train$Date,"%U"))
head(temp)

train$Returns <- lapply(train$Weekly_Sales,function(sales){
  ifelse(sales < 0,sales,0)
})
train$Weekly_Sales <- lapply(train$Weekly_Sales,function(sales){
  ifelse(sales > 0,sales,0)
})


final_data <- data.frame(Store=factor(),Date=as.Date(character()),Weekly_Sales=numeric(),IsHoliday=logical(),Type=factor(),WeekNum=factor())

sum(unlist((train$Weekly_Sales)))

aggregate_sales <- function(){
  for(i in 1:45){
    store_data <- train %>% filter(Store == i)
    dates <- unique(train$Date)
    for(next_date in seq_along(dates)){
      current_date <- unique(train$Date)[[next_date]]
      date_data <- store_data %>% filter(Date==current_date)
      
      
      #Add all the weekly sales
      net_sales <- sum(unlist(date_data$Weekly_Sales)) - sum(unlist(date_data$Returns))
      #Construct the data frame and append it
      next_row <- data.frame(Store=i,Date=current_date,Weekly_Sales=net_sales,IsHoliday=date_data$IsHoliday[[1]],Type=date_data$Type[[1]],WeekNum=date_data$WeekNum)
      next_row$Store <- factor(next_row$Store)
      final_data <- rbind(final_data,next_row)
    }
  }
  return(final_data)
}
# Sum the sales by store without taking into account each department
#final_data <- aggregate_sales()
#final_data_unique <- unique(final_data)
#final_data_repeated <- final_data
#final_data <- final_data_unique
#length(unique(final_data$Date))
View(final_data)
View(train)

features$Store <- factor(features$Store)
#Merge our final_data with our features
train <- left_join(train,features,by=c("Store","Date","IsHoliday"))
# Make the NA markdown as 0
train$MarkDown1 <- sapply(train$MarkDown1, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown2 <- sapply(train$MarkDown2, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown3 <- sapply(train$MarkDown3, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown4 <- sapply(train$MarkDown4, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown5 <- sapply(train$MarkDown5, function(value){
  ifelse(is.na(value),0,value)
})
View(train)
write.csv(train, 'test_full_sheet.csv')

?createDataPartition
min(unlist(train$Weekly_Sales))
max(unlist(train$Weekly_Sales))
head(train$Weekly_Sales)
temp_col <- unlist(train$Weekly_Sales)

train_temp <- train

train$Returns <- unlist(train$Returns)

#train_temp$Weekly_Sales <- unlist(train_temp$Weekly_Sales)
typeof(train$Weekly_Sales)
train$Weekly_Sales <- unlist(train$Weekly_Sales)


train$Rank <- cut(train$Weekly_Sales, 5, include.lowest = T, labels = c(1,2,3,4,5))
train$Weekly_Sales <- unlist(train$Weekly_Sales)
typeof(train$Weekly_Sales)


train$Weekly_Sales <- unlist(train$Weekly_Sales)
train$Rank <- cut(train$Weekly_Sales, 5, include.lowest = T, labels = c('A','B','C','D','E'))

mysample <- train[sample(1:nrow(train), 400,replace=FALSE),]
View(mysample)

mysample$Unemployment <- as.numeric(mysample$Unemployment)
myrank <- mysample$Rank
myrank


write.table(myrank, "unemployment.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)


View(train)
dim(train)

View(train_temp)

train$Rank <- cut(train$Weekly_Sales, 5, include.lowest = T, labels = c(1, 2,3,4,5))

index <- createDataPartition(train$Weekly_Sales,list = FALSE,p=0.7)
trainingData <-train[index,]
trainTest <- train[-index,]
#head(train.train)
#View(train)
#train$Returns <- unlist(train$Returns)
#write.csv(train,'trainData.csv')
#View(train.train)
#train$IsHoliday <- as.integer(train$IsHoliday)
#View(train)
#typeof(train$Type)
dim(trainTest)
dim(trainingData)

dim(train)
temp <- as.Date(train$Date)
train$year <- as.numeric(format(train$Date, '%Y'))
train$month <- as.numeric(format(train$Date, '%m'))
train$day <- as.numeric(format(train$Date, '%d'))
train$Date <- NULL
View(train)

train$Type = as.integer(train$Type)
unique(train$Rank)

?varImp



fit <- lm(Weekly_Sales ~.-Date-Type-CPI, data=train.train)


####
saad_train<-train.train
saad_train$Store<-as.numeric(as.character(saad_train$Store))
fit <- lm(Weekly_Sales ~ Dept+Size+MarkDown3+Rank, data=saad_train)
varImp(fit)
summary(fit)
unique(saad_train$Rank)

####


predict_fit_confidence <- predict(fit, newdata=train.test, interval="confidence", level=0.95)
predict_fit_confidence
View(train.train)
dim(train.train)
varImp(fit)
summary(fit)
typeof(train$Temperature)
plot(data = train.train, Weekly_Sales, Dept)
train.train$Unemployment[train.train$Unemployment == 'NA']
head(train)
summary(lm(formula = Weekly_Sales ~ . - Store, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - Dept, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - IsHoliday, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - Type, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - Size, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - WeekNum, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - Returns, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - Temperature, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - Fuel_Price, data = trainingData))
summary(lm(formula = Weekly_Sales ~ . - MarkDown1 - MarkDown2- MarkDown3- MarkDown4- MarkDown5 , data = trainingData))
summary(lm(formula = Weekly_Sales ~ WeekNum, data = trainingData))
#Using a decision tree we will like to predict the Type of a store based on all the other parameters
train.rpart <-rpart(Type ~ Weekly_Sales + Size,data=train.train, control=rpart.control(minsplit=5,cp=0.05))
summary(train.rpart)
fancyRpartPlot(train.rpart)
summary(lm(formula = Weekly_Sales ~ . - day - month - year - Type - CPI, data = train.train))
scatter.smooth(trainingData$Weekly_Sales ~ trainingData$Dept)
scatter.smooth(trainingData$Weekly_Sales ~ trainingData$Rank)



f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Weekly_Sales",
  titlefont = f
)
y <- list(
  title = "Store",
  titlefont = f
)
p <- plot_ly(x = train$Date, type = 'scatter', mode = "markers", marker = list(color="blue")) %>%
  layout(xaxis = x, yaxis = y)
p

p <- plot_ly(y = ~train$Store, type = "box") %>%
  layout(xaxis = x, yaxis = y, title = "Store Boxplot")
p<-plot_ly(y= ~train$Store,type="scatter") %>%
  layout(xaxis = x, yaxis = y, title = "Store ScatterPlot")



p <- plot_ly(data = train, x = train$Weekly_Sales, y = train$Store, color = ~train$Rank, type = "scatter")%>%
  layout(title='Styled Scatter',yaxis = y,
         xaxis = x)
p

p <- plot_ly(data = mysample, x = ~Temperature, y = ~Weekly_Sales, name = 'trace 0', type = 'scatter') %>%
  layout(title='Styled Scatter',yaxis = y, xaxis = x)
p
