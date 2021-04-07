library(readr)
library(psych)
#read in the dataset into the object called "complete"
complete <- read_excel("train_semi_clean.xlsx")

#first, i need to factorize some predictors since they are not on a continuous scale. I basically started by reading in 
#each predictor name into a vector called "names"
names <- c("Biodata_01","Biodata_02","Biodata_03","Biodata_04","Biodata_05","Biodata_06","Biodata_07","Biodata_08","Biodata_09",
           "Biodata_10","Biodata_11","Biodata_12","Biodata_13","Biodata_14","Biodata_15","Biodata_16","Biodata_17","Biodata_18",
           "Biodata_19","Biodata_20",
           "Scenario1_1","Scenario1_2","Scenario1_3","Scenario1_4","Scenario1_5","Scenario1_6","Scenario1_7","Scenario1_8",
           "Scenario2_1","Scenario2_2","Scenario2_3","Scenario2_4","Scenario2_5","Scenario2_6","Scenario2_7","Scenario2_8",
           "SJ_Most_1","SJ_Least_1",
           "SJ_Most_2","SJ_Least_2",
           "SJ_Most_3","SJ_Least_3",
           "SJ_Most_4","SJ_Least_4",
           "SJ_Most_5","SJ_Least_5",
           "SJ_Most_6","SJ_Least_6",
           "SJ_Most_7","SJ_Least_7",
           "SJ_Most_8","SJ_Least_8",
           "SJ_Most_9","SJ_Least_9"
)
#next, i use the lapply function to factorize multiple predictor variables. For the first argument,
# I need to take the "names" vector and denote it grab from the "complete" object.  For the second argument, i denote what
# I want the lapply function to do; in this case, to factor the variables that match the variable names from the "names" vector
complete[,names] <- lapply(complete[,names] , factor)


#lets start modeling
#first we need to split data between train and test. The "complete" object is entirely the training dataset provided to us,
#however, i decided to further split the training dataset into train and test because a significant portion of the cases within
#the training dataset were missing criterion variables. Therefore, i decided to divide the "complete" dataset between cases that are
#complete and cases that are missing criterion variables. I used the crtierion variable "High_Performer" as a way to easily split
#the data, but other criterion variables will also work. 
test <- subset(complete, High_Performer=="1" | High_Performer=="0")
#cases with no missing data went into the "train" object.
train <- complete[is.na(complete$High_Performer),]

#lets look at the summaries of each object
summary(test)
summary(train)

#next, lets specify the predictor variables. Note that this step is not necessary to run the model and there are more "efficient"
# ways of indicating the predictor variables, however, we specify each predictor variable name here in adherence to good data hygiene

xVars <- c("Biodata_01","Biodata_02","Biodata_03","Biodata_04","Biodata_05","Biodata_06","Biodata_07","Biodata_08","Biodata_09",
           "Biodata_10","Biodata_11","Biodata_12","Biodata_13","Biodata_14","Biodata_15","Biodata_16","Biodata_17","Biodata_18",
           "Biodata_19","Biodata_20",
           "PScale01_Q1","PScale01_Q2","PScale01_Q3","PScale01_Q4",
           "PScale02_Q1","PScale02_Q2","PScale02_Q3","PScale02_Q4",
           "PScale03_Q1","PScale03_Q2","PScale03_Q3","PScale03_Q4",
           "PScale04_Q1","PScale04_Q2","PScale04_Q3","PScale04_Q4",
           "PScale05_Q1","PScale05_Q2","PScale05_Q3","PScale05_Q4",
           "PScale06_Q1","PScale06_Q2","PScale06_Q3","PScale06_Q4","PScale06_Q5","PScale06_Q6",
           "PScale07_Q1","PScale07_Q2","PScale07_Q3","PScale07_Q4",
           "PScale08_Q1","PScale08_Q2","PScale08_Q3","PScale08_Q4",
           "PScale09_Q1","PScale09_Q2","PScale09_Q3","PScale09_Q4",
           "PScale10_Q1","PScale10_Q2","PScale10_Q3","PScale10_Q4",
           "PScale11_Q1","PScale11_Q2","PScale11_Q3","PScale11_Q4",
           "PScale12_Q1","PScale12_Q2","PScale12_Q3","PScale12_Q4",
           "PScale13_Q1","PScale13_Q2","PScale13_Q3","PScale13_Q4",
           "Scenario1_1","Scenario1_2","Scenario1_3","Scenario1_4","Scenario1_5","Scenario1_6","Scenario1_7","Scenario1_8",
           "Scenario2_1","Scenario2_2","Scenario2_3","Scenario2_4","Scenario2_5","Scenario2_6","Scenario2_7","Scenario2_8",
           "Scenario1_Time",
           "Scenario2_Time",
           "SJ_Most_1","SJ_Least_1","SJ_Time_1",
           "SJ_Most_2","SJ_Least_2","SJ_Time_2",
           "SJ_Most_3","SJ_Least_3","SJ_Time_3",
           "SJ_Most_4","SJ_Least_4","SJ_Time_4",
           "SJ_Most_5","SJ_Least_5","SJ_Time_5",
           "SJ_Most_6","SJ_Least_6","SJ_Time_6",
           "SJ_Most_7","SJ_Least_7","SJ_Time_7",
           "SJ_Most_8","SJ_Least_8","SJ_Time_8",
           "SJ_Most_9","SJ_Least_9","SJ_Time_9"
)
#specify the target variable. right now it is set to the "Retained" variable, but you can also switch to the "High_Performer" variable
targetVar<-c("Retained")

#now we subset using the vector called "XVars". 
x <- train[,xVars]
#lets turn this into a data frame
train<-as.data.frame(train)
#lets subset again using the vector "targetVar" that contains the variable name "Retained". We also tell R to factorize this variable. 
y <- as.factor(train[, targetVar])


#getting data ready for caret... Do not mess with lines 87 to 111. 
cleanNames <- function(x){
  feature.names=names(x)
  for (f in feature.names) {
    if (class(x[[f]])=="factor") {
      levels <- unique(c(x[[f]]))
      x[[f]] <- factor(x[[f]],
                       labels=make.names(levels))
    }
  }
  return(x)}
xOld <-x
x <- cleanNames(x)
str(xOld)
str(x)
y = make.names(y)

test <- cleanNames(test)
str(test)
str(Prediction)
levels(test$Retained) <- c("X0","X1")
str(test)
Actual <-test$Retained
Actual<-as.factor(Actual)
levels(Actual) <- c("X0","X1")

#load in the caret library. we wont use the caret package just yet.
library(caret)

#load in the random forest package
library(randomForest)
yRf <-as.factor(y)
#lets fit the model. specify how many trees you want. you can play with that
fit <- randomForest(x = x, y = yRf
                     , data=train,
                     importance=TRUE,
                     # fit 2000 decision trees!
                     ntree=1)
#set your fit
fit
#plot the variable importance as measured by the RF(Random Forest)
varImpPlot(fit2)


#let's test our model
Prediction <- predict(fit, test)
confusionMatrix(reference = Actual, data = Prediction2)

#alright, now we can use the caret package to tune the model. first we need a training control for cross validation
trctrl <- trainControl(method = "repeatedcv"
                       , number = 10, repeats = 3
                       , classProbs = TRUE
                       , summaryFunction = twoClassSummary
)
set.seed(3875)
# we can adjust the parameters: tunelength and tunegrid. you can reference the site below for information regarding those parameters
# https://bookdown.org/mpfoley1973/data-sci/classification-tree.html
?train
fit2<- train(x = x
             , y = y
             , method = "rf",
             tuneLength=20,
             tuneGrid = expand.grid(cp = seq(from = 0.0001, to = 0.01, length = 11)),
             metric="ROC",
             trControl = trctrl)
fit2
plot(fit2)

Prediction2 <- predict(fit2, test, type = "raw")
confusionMatrix(reference = Actual, data = Prediction2)
