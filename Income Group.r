library("tidyr")
setwd("C:/softwares/R Programs/Classification")
data_train<-read.csv("Income Group train.csv",header = TRUE,na.strings = c(" ",""))
View(data_train)
###############Missing data treatment ##################
sapply(data_train,function(x)sum(is.na(x))) # gives you the count of NA across all columns in dataset
#so we have to treat work Class, Occupation and Native.Country column for NA s all of the colunns are categorical columns
# we will do Mode imputation
Mode <- function (x, na.rm) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    if (length(xmode) > 1) xmode <- ">1 mode"
    return(xmode)
}

for (var in 1:ncol(data_train)) {
    if (class(data_train[,var])=="numeric") {
        data_train[is.na(data_train[,var]),var] <- mean(data_train[,var], na.rm = TRUE)
    } else if (class(data_train[,var]) %in% c("character", "factor")) {
        data_train[is.na(data_train[,var]),var] <- Mode(data_train[,var], na.rm = TRUE)
    }
}
View(data_train)
sapply(data_train,function(x)sum(is.na(x)))

#######Now comes categorical values treatment  Use Superml package to do LabelEncoding ########
summary(data_train)
#install.packages("superml",dependencies = TRUE)
library("superml")
#first treat the Workclass column
lbl <- LabelEncoder$new()
data_train$Workclass <- lbl$fit_transform(data_train$Workclass)
data_train$Education <- lbl$fit_transform(data_train$Education)
data_train$Marital.Status <- lbl$fit_transform(data_train$Marital.Status)
data_train$Occupation <- lbl$fit_transform(data_train$Occupation)
data_train$Relationship <- lbl$fit_transform(data_train$Relationship)
data_train$Race <- lbl$fit_transform(data_train$Race)
data_train$Sex <- lbl$fit_transform(data_train$Sex)
data_train$Native.Country <- lbl$fit_transform(data_train$Native.Country)
library("ggplot2")
#####################Exploratory data analysis (EDA)#################
#Let’s start with univariate EDA. It involves exploring variables individually. We will try to visualize the continuous variables using histograms and categorical variables using bar plots
ggplot(data_train) + geom_histogram(aes(data_train$Age), binwidth = 5, fill = "darkgreen") +  xlab("Age")
#install.packages("cowplot",dependencies = TRUE)
#install.packages("caret",dependencies = TRUE)
#install.packages("corrplot",dependencies = TRUE)
library("cowplot")
library("caret")
library("corrplot")
p1 = ggplot(data_train) + geom_histogram(aes(Age), binwidth = 5, fill = "blue")
p2 = ggplot(data_train) + geom_histogram(aes(Workclass), binwidth = 1, fill = "green")
p3 = ggplot(data_train) + geom_histogram(aes(Education), binwidth = 1, fill = "red") 
p4 = ggplot(data_train) + geom_histogram(aes(Marital.Status), binwidth = 1, fill = "black") 
p5 = ggplot(data_train) + geom_histogram(aes(Occupation), binwidth = 1, fill = "blue") 
p6 = ggplot(data_train) + geom_histogram(aes(Relationship), binwidth = 1, fill = "red") 
p7 = ggplot(data_train) + geom_histogram(aes(Race), binwidth = 1, fill = "black")
p8 = ggplot(data_train) + geom_histogram(aes(Native.Country), binwidth = 1, fill = "red")
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2) # plot_grid() from cowplot package
library("dplyr")
#from the display above, we can infer that the Native.Country column needs binning together of all countries other than USA into non USA
data_train = data_train %>% mutate(candy.flag = factor(ifelse(Native.Country == 38, 38, 37)))
#remove the id column as it is of no use
data_train <- data_train[,-1]
data_train <- subset( data_train, select = -candy.flag)
#Check the balance of the dataset with respect to the predicted column
print(prop.table(table(data_train$Income.Group))) # at 75% it is a rare event So we have to use SMOTE to balance the class before classification
install.packages("DMwR")
library("DMwR")
smoted_dataset <- SMOTE(Income.Group~., data_train, perc.over=200, perc.under=100, k=5, learner=NULL)
print(prop.table(table(smoted_dataset$Income.Group)))  # so now the % is 60 and 40

