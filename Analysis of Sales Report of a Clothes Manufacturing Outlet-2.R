install.packages("readxl")
install.packages("plyr")
install.packages("dplyr")
install.packages("caTools")
install.packages("e1071")
install.packages("caret")
install.packages("randomForest")
install.packages("pscl", repos = "https://cran.rstudio.com")

# import necessary libraries
library(readxl) 
# to read excel
library(plyr) 
library(dplyr)
library(caTools)
library(e1071) 
library(caret) 
library(randomForest)

attrib1 = read_excel('Attribute DataSet.xlsx')
dresssale1 = read_excel('Dress Sales.xlsx')
#remove Dress_ID column
attrib2 = attrib1[2:14] 
dresssale2 = dresssale1[2:24]

# check the unique values for each columns
lapply(attrib2, unique)

# values checking
attrib2$Style<-mapvalues(attrib2$Style, c('sexy'), c('Sexy'))
attrib2$Price<-mapvalues(attrib2$Price, c('low','high'), c('Low','High'))
attrib2$Size<-mapvalues(attrib2$Size, c('s','small'), c('S','S'))
attrib2$Season<-mapvalues(attrib2$Season, c('spring','summer','Automn','winter'), c('Spring','Summer','Autumn','Winter'))
attrib2$NeckLine<-mapvalues(attrib2$NeckLine, c('sweetheart'), c('Sweetheart'))
attrib2$SleeveLength<-mapvalues(attrib2$SleeveLength, c('sleevless','sleeevless','sleveless','threequater','thressqatar','urndowncollor'), c('sleeveless','sleeveless','sleeveless','threequarter','threequarter','turndowncollar'))
attrib2$FabricType<-mapvalues(attrib2$FabricType, c('shiffon','sattin','wollen','flannael','knitting'), c('chiffon','satin','woolen','flannel','knitted'))
attrib2$Decoration<-mapvalues(attrib2$Decoration, c('embroidary','sequined','ruched','none'), c('embroidery','sequins','ruche','null'))
attrib2$`Pattern Type`<-mapvalues(attrib2$`Pattern Type`, c('none','leapord'), c('null','leopard'))

# factoring 
sapply(attrib2,class)
cols<-c("Style","Price","Size","Season","NeckLine","SleeveLength","waiseline","Material",
        "FabricType","Decoration","Pattern Type","Recommendation")
attrib2[cols]<-lapply(attrib2[cols],factor)
sapply(attrib2,class)
# OR ###
#attrib2$Style <- factor(attrib2$Style,levels=unique(attrib2$Style))
#table(attrib2$Style)
#attrib2$Price <- factor(attrib2$Price,levels=unique(attrib2$Price))
#summary(attrib2$Price)
#attrib2$Recommendation <- sapply(attrib2$Recommendation, factor)

# count of missing values in attrib2 dataset
colSums(is.na(attrib2))

# fill missing Value with mode # Categorical data. So, no use of mean & median. So, use Mode. # The mode is the value that has highest number of occurrences in a set of data. # R does not have a standard in-built function to calculate mode. So we create a user function to calculate mode of a data set in R.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# fill missing Value with mode
attrib2$Price[is.na(attrib2$Price) ==TRUE] <- getmode(attrib2$Price)
attrib2$Season[is.na(attrib2$Season) ==TRUE] <- getmode(attrib2$Season)
attrib2$NeckLine[is.na(attrib2$NeckLine) ==TRUE] <- getmode(attrib2$NeckLine)
attrib2$waiseline[is.na(attrib2$waiseline) ==TRUE] <- getmode(attrib2$waiseline)
attrib2$Material[is.na(attrib2$Material) ==TRUE] <- getmode(attrib2$Material)
attrib2$FabricType[is.na(attrib2$FabricType) ==TRUE] <- getmode(attrib2$FabricType)
attrib2$Decoration[is.na(attrib2$Decoration) ==TRUE] <- getmode(attrib2$Decoration)
attrib2$`Pattern Type`[is.na(attrib2$`Pattern Type`) ==TRUE] <- getmode(attrib2$`Pattern Type`)


attrib2data <- data.frame(attrib2)
str(attrib2data)

#Dresses dataset
head(dresssale2)
names(dresssale2)

# Update columns name in dresssale2 dataset
# Change any column names you want to, all at once
colnames(dresssale2)[colnames(dresssale2) %in% c("41314","41373","41434","41495","41556","41617","41315","41374","41435","40400","41557","41618")] <- c("2/9/2013","4/9/2013", "6/9/2013","8/9/2013","10/9/2013","12/9/2013","2/10/2013","4/10/2013","6/10/2013","8/10/2013","10/10/2013","12/10/2013")
# names(dresssale2) <- c("29/8/2013", "31/8/2013", "2/9/2013",     "41373"  ,   "41434" ,    "41495"  ,   "41556" ,    "41617",    
#                       "14/9/2013", "16/9/2013" ,"18/9/2013", "20/9/2013", "22/9/2013", "24/9/2013", "26/9/2013", "28/9/2013",
#                       "30/9/2013", "41315",     "41374",     "41435",     "40400"    , "41557" ,    "41618")

head(dresssale2)

# Convert all variable types to numeric
dresssale2 <- as.data.frame(apply(dresssale2, 2, as.numeric))
# mean row 
dresssale2 = as.matrix(dresssale2)
k <- which(is.na(dresssale2), arr.ind=TRUE)
dresssale2[k] <- rowMeans(dresssale2, na.rm=TRUE)[k[,1]]
dresssale2 = as.data.frame(dresssale2)

# sum all values on row on (total sales column)-New column created 
dresssale2$total_sales = rowSums(dresssale2)
head(dresssale2)

#Merged data
merged_data <- data.frame(attrib2 ,dresssale2)
head(merged_data)

str(merged_data)

# spliting dataset 
set.seed(100)
spl = sample.split(merged_data$Recommendation, SplitRatio = 0.7)
train = subset(merged_data, spl==TRUE)
test = subset(merged_data, spl==FALSE)
print(dim(train)); 
print(dim(test))

#Classification - Predict recommendation
#First model (Naive Bayes): # non-linear model # simple & fast
options(scipen = 999)
naive_model = naiveBayes(Recommendation ~.,data = train) # build model # . means all column
confusionMatrix(train$Recommendation, predict(naive_model,train), positive = '1') # create confusion Matrix
naive_predict = predict(naive_model,test)  # predict test set
table(naive_predict,test$Recommendation)  # create table

# Support vector machine (SVM): # Linear model # complex
svm_model = svm(Recommendation ~.,train) # build model
confusionMatrix(train$Recommendation,predict(svm_model),positive = '1') # create confusion Matrix
svm_predict = predict(svm_model,test) # predict test set
table(svm_predict,test$Recommendation) # create table


# Third model (Random Forest) 
randomForest_model = randomForest(x = train, y = train$Recommendation,ntree =800) # build model
confusionMatrix(train$Recommendation,predict(randomForest_model),positive = '1') # create confusion Matrix
randomForest_predict = predict(randomForest_model,test) # predict test set
table(randomForest_predict,test$Recommendation ) # create table

# Regresstion model (total sales and (Style+Season+Material+Price)) 
regressor_Sales = lm(formula = total_sales ~ Style+Season+Material+Price, data = train) # build model
summary(regressor_Sales) # print model summary
plot(regressor_Sales, pch = 16, col = "blue") # Plot the results
abline(regressor_Sales) # Add regression line

# Regression (total sales and Rating) 
regressor_Rating = lm(formula = total_sales ~ Rating, data = train) # build model
summary(regressor_Rating) # print model summary
plot(regressor_Rating, pch = 16, col = "blue") # Plot the results
abline(regressor_Rating) # Add regression line