rm(list=ls(all=T))
setwd("F:/Rittal Docs/edwisor.com/02.Portfolio/02.Project-2")

getwd()

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

install.packages(c("dplyr","plyr","reshape","ggplot2","data.table","DMwR","caret",'unbalanced',"C50","dummies","e1071","Information","gbm","ROSE","sampling","inTrees"))
`install.packages("GGally")

# Install  Libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")
library(tidyr)
for ( i in x ) {
  print(i)
  library("ggplot2")
  
}

## Read the data
df1 = read.csv("bank-loan.csv", header = T, na.strings = c(" ", "", "NA"))

###########################################Explore the data##########################################
str(df1)
# list types for each attribute
sapply(df1, class)

#Unique values in a column
unique(df1$ed)
summary(df1)
str(df1)
#convert each varaible
df1$age = as.numeric(df1$age)
df1$ed = as.numeric(df1$ed)
df1$employ = as.numeric(df1$employ)
df1$address = as.numeric(df1$address)
df1$debtinc = as.numeric(df1$debtinc)
df1$income = as.numeric(df1$income)
df1$creddebt = as.numeric(df1$creddebt)
df1$othdebtdebt = as.numeric(df1$othdebt)

typeof(df1)
str(df1)
df1$default = as.factor(df1$default)

str(df1)

# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {
  
  ggplot(df1)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}
# analyze the distribution of  target variable 'default'
univariate_numeric(df1$default)

# analyse the distribution of  independence variable 'age'
univariate_numeric(df1$age)

# analyse the distrubution of  independence variable 'ed'
univariate_numeric(df1$ed)

# analyse the distrubution of  independence variable 'employ'
univariate_numeric(df1$employ)

# analyse the distrubution of  independence variable 'address'
univariate_numeric(df1$address)

# analyse the distrubution of  independence variable 'debtinc'
univariate_numeric(df1$debtinc)

# analyse the distrubution of  independence variable 'income'
univariate_numeric(df1$income)

# analyse the distrubution of  independence variable 'creddebt'
univariate_numeric(df1$creddebt)

# analyse the distrubution of  independence variable 'othdebt'
univariate_numeric(df1$othdebt)


# the above graph is showing   'default'is normally   distributed
# Visualize categorical Variable 'ed with target variable 'default'

ggplot(df1, aes(x=as.factor(ed), y=default),fill="grey") + 
  stat_summary(fun.y="mean", geom="bar")

ggplot(df1)+
  geom_histogram(aes(x=default,y=..density..),
                 fill= "grey")+
  geom_density(aes(x=default,y=..density..))


# *****************bivariate  relationship ****************************

#check the relationship between 'age' and 'default' variable
ggplot(df1, aes(x= default,y=age)) +
  geom_point()+
  geom_smooth()

#check the relationship between 'employ' and 'default' variable
ggplot(df1, aes(x= default,y=employ)) +
  geom_point()+
  geom_smooth()

#check the relationship between 'address' and 'default' variable
ggplot(df1, aes(x= default,y=address)) +
  geom_point()+
  geom_smooth()


#check the relationship between 'income' and 'default' variable
ggplot(df1, aes(x= default,y=income)) +
  geom_point()+
  geom_smooth()

#check the relationship between 'debtinc' and 'default' variable
ggplot(df1, aes(x= default,y=debtinc)) +
  geom_point()+
  geom_smooth()


#check the relationship between 'creddebt' and 'default' variable
ggplot(df1, aes(x= default,y=creddebt)) +
  geom_point()+
  geom_smooth()

#check the relationship between 'othdebt' and 'default' variable
ggplot(df1, aes(x= default,y=othdebt)) +
  geom_point()+
  geom_smooth()

# it is showing that very less  correlation among numeric variables

#check the relationship between all numeric variable using pair plot

ggpairs(df1[,c('age', 'employ', 'address', 'income', 'debtinc', 'creddebt', 'othdebt','default')])



#Check the distribution of numerical data using histogram
hist1 = ggplot(data = df1, aes(x =age)) + ggtitle("Distribution of age") + geom_histogram(bins = 25)
hist2 = ggplot(data = df1, aes(x =employ))+ ggtitle("Distribution of employment") + geom_histogram(bins = 25)
hist3 = ggplot(data = df1, aes(x =address)) + ggtitle("Distribution of address") + geom_histogram(bins = 25)
hist4 = ggplot(data = df1, aes(x =income)) + ggtitle("Distribution of income") + geom_histogram(bins = 25)
hist5 = ggplot(data = df1, aes(x =debtinc)) + ggtitle("Distribution of Individual's debt payment ") + geom_histogram(bins = 25)
hist6 = ggplot(data = df1, aes(x =creddebt)) + ggtitle("Distribution of debt-to-credit ratio ") + geom_histogram(bins = 25)
hist7 = ggplot(data = df1, aes(x =othdebt)) + ggtitle("Distribution of Any other debts") + geom_histogram(bins = 25)

gridExtra::grid.arrange(hist1,hist2,hist3,hist4,hist5,hist6,hist7,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = df1,aes(x =age, y = default)) + ggtitle("Distribution of age") + geom_point() + xlab("age") + ylab("Default")
scat2 = ggplot(data = df1,aes(x =employ, y = default)) + ggtitle("Distribution of employment") + geom_point(color="red") + xlab("employment") + ylab("Default")
scat3 = ggplot(data = df1,aes(x =address, y = default)) + ggtitle("Distribution of address") + geom_point() + xlab("Feel address") + ylab("Default")
scat4 = ggplot(data = df1,aes(x =income, y = default)) + ggtitle("Distribution of income") + geom_point(color="red") + xlab("income") + ylab("Default")
scat5 = ggplot(data = df1,aes(x =debtinc, y = default)) + ggtitle("Distribution of Individual's debt payment") + geom_point(color="red") + xlab("Debt Payment") + ylab("Default")
scat6 = ggplot(data = df1,aes(x =creddebt, y = default)) + ggtitle("Distribution of debt-to-credit ratio ") + geom_point(color="red") + xlab("debt-to-credit ratio") + ylab("Default")
scat7 = ggplot(data = df1,aes(x =othdebt, y = default)) + ggtitle("Distribution of Any other debts") + geom_point(color="red") + xlab("other debts") + ylab("Default")

gridExtra::grid.arrange(scat1,scat2,scat3,scat4,scat5,scat6,scat7,ncol=2)


##################################Missing Values Analysis###############################################

sum(is.na(df1$default))
missing_val = data.frame(apply(df1, 2, function(x){sum(is.na(x))}))

missing_val$columns = row.names(missing_val)

row.names(missing_val) = NULL

names(missing_val)[1] = "missing_percentage"

missing_val$missing_percentage = (missing_val$missing_percentage/nrow(df1))*100.

missing_val = missing_val[order(-missing_val$missing_percentage),]

missing_val = missing_val[,c(2,1)]

write.csv(missing_val, "Mising_perc_R.csv", row.names = F)
df <-(df1)

# ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
#   geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
#   ggtitle("Missing data percentage (Train)") + theme_bw()


############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(df,is.numeric) #selecting only numeric

numeric_data = df[,numeric_index]

cnames = colnames(numeric_data)

cnames

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "default",group=1), data = subset(df))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="default")+
           ggtitle(paste("Box plot of loan default wrt",cnames[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,gn6,gn7,gn8,gn9,ncol=3)

#lets check the NA's
data.frame(apply(df,2,function(x){sum(is.na(x))}))

library(DMwR)

#Imputing with KNN
df = knnImputation(df,k=3)

# lets check the missing values
data.frame(apply(df,2,function(x){sum(is.na(x))}))

#All the missing values have been imputed

##################################Feature Selection################################################
## Correlation Plot 
corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Multi Collinearity Check
library(usdm)
vif(df[,-1])

vifcor(df[,-1], th = 0.9)

#Chi-squared Test of Independence
#Collecting Factorial data
factor_index = sapply(df,is.factor) # checking Fact Vars index (= true)
factor_index
factor_data = df[,factor_index] # Asssigning data to those indexes
head(factor_data)

#Checking dependencies of default' w.r.t all fact_data variables as,
for (i in 1) # as 'default' is TV we will exclude it
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$default,factor_data[,i])))
}
#Our Categorical Variable ed the value is 0.0497 which is also less than 0.05 
#So we have to reject our Null Hypothesis and consider our categorical variable also.
########################  Normality  check #################################
#Normalisation
cnames = c('age', 'employ', 'address', 'income', 'debtinc', 'creddebt', 'othdebt')

for(i in cnames){
  print(i)
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i] - min(df[,i])))
}



###################### separating the labeled and not labeled observations. 
##### last 150 observations are not labled. Those observations to be 
### predicted after the model building. we will predict those values after 
### chossing best model. 

df_train = df[1:700,1:9]
dim(df_train)
sum(is.na(df$default))
df_test  = df[701:850,1:8]
dim(df_test)

# This Function will take Actual y value and Predicted  values and it will give
# Output as  Accuracy , Precision , Recall etc
model_evaluation <- function(test_y,predicted_y) {
  
  table_matrix= table(test_y,predicted_y)
  print (confusionMatrix(table_matrix))
  precision=  table_matrix[4]/(  table_matrix[4] +  table_matrix[3])
  print(paste("Precision  is--" ,precision))
  recall  = table_matrix[4]/(  table_matrix[4] +  table_matrix[2])
  print(paste("recall  is--" ,recall))
  Accuracy = sum(diag(table_matrix)) / sum(table_matrix)
  print(paste("Accuracy   is--" ,Accuracy))
  
  FNR  = table_matrix[2]/(  table_matrix[4] +  table_matrix[2])
  print(paste("FNR     is--" ,FNR))
  F1_score = 2 * (precision * recall)/(precision+recall)
  print(paste("F1 score     is--",F1_score))
}

###################################Model Development#######################################
#Clean the environment
rmExcept("df")


library(caTools)

#Splitting into training and testing data
set.seed(123)
sample = sample.split(df_train, SplitRatio = 0.8)
sample
train = subset(df_train, sample==TRUE)
str(train)
test = subset(df_train, sample==FALSE)
str(test)

###############################Logistic Regression############################################################
logit_model = glm(default ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

##Evaluate the performance of classification model
ConfMatrix_lg = table(test$default, logit_Predictions)
confusionMatrix(ConfMatrix_lg)


model1 = glm(default~age,train, family = "binomial")
summary(model1)

model2 = glm(default~ed,train, family = "binomial")
summary(model2)

model3 = glm(default~employ,train, family = "binomial")
summary(model3)

model4 = glm(default~address,train, family = "binomial")
summary(model4)

model5 = glm(default~creddebt,train, family = "binomial")
summary(model5)

##model with high important variables

model6 = glm(default~creddebt+debtinc+address+employ,train, family = "binomial")
summary(model6)

res = predict(model6, test, type = "response")
range(res)

confusion_matric = table(Actualvalue=test$default, predictedvalue=res>0.5)

print(confusion_matric)

accuracy = (104+20)/(104+20+24+7)

test$default

print(accuracy)

# precision = 0.74
# recall = 0.45

####### threshold evaluation ################
### ROC CURVE ##########
######AUC####
library(ROCR)
pred_log = prediction(res,test$default)
acc = performance(pred_log,"acc")
plot(acc)

roc_curve = performance(pred_log, "tpr" , "fpr")
plot(roc_curve)
plot(roc_curve , colorize = T, print.cutoffs.at=seq(0.1,by=0.1))

###### using threshold value of 0.4 we can incraese the true positive rate 

confusion_matric = table(Actualvalue=test$default, predictedvalue=res>0.4)

print(confusion_matric)

############# Model Evaluation  #
model_evaluation(test$default,logit_Predictions)

#Accuracy = 0.80
#Precision = 0.74
#Recall = 0.45
#FNR = 0.54

############ AUC##############

auc = performance(pred_log, "auc")
auc

# AUC = 0.82

#############Precision recall curve ##############
library(PRROC)
PRC_curve = performance(pred_log, "prec" , "rec")
plot(PRC_curve, colorize = T)

############################## Decision tree#############################################
library(tree)
deci_model = tree(default~., data = train)
summary(deci_model)

### plotting 
plot(deci_model)
text(deci_model,pretty = 0)

#  plotting decision tree
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
par(cex= 0.8)
plot(deci_model)
text(deci_model)
rpart.plot(deci_model)

##Evaluate the performance of classification model

deci_pred = predict(deci_model, test, type = "class")

confusion_matric = table(test$default,deci_pred)
confusion_matric


print(confusion_matric)
confusionMatrix(confusion_matric)

############# Model Evaluation  #
model_evaluation(test$default,deci_pred)


#### cross validation 
cv.deci_model = cv.tree(deci_model, FUN = prune.misclass)
cv.deci_model
plot(cv.deci_model)

####pruning
prune.deci_model = prune.misclass(deci_model, best = 10)
plot(prune.deci_model)
text(prune.deci_model)

#### prediction of values again
deci_pred_1 = predict(prune.deci_model, test, type = "class")

Confusion_matrix_1= table(test$default, deci_pred_1)

print(Confusion_matrix_1)
confusionMatrix(Confusion_matrix_1)
############# Model Evaluation  #

model_evaluation(test$default,deci_pred_1)

# accuracy = 0.74 
# precision = 0.54 
# recall = 0.43 
# FNR= 0.56

class(deci_pred_num)
class(testdata_num)
deci_pred_num<- as.numeric(deci_pred_1)
testdata_num<- as.numeric(test$default)
#ROC Curve
library(ROCR)
data(ROCR.simple)
dt.pred <- prediction(deci_pred_num,testdata_num)
roc <- performance(dt.pred,"tpr","fpr")
plot(roc,
     colorize=T,
     main="ROC -Curve")   
abline(a=0,b=1)

#AUC curve
auc<- performance(dt.pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc
#AUC=0.6483

#legend(.6,.2,auc,title="AUC",cex=4)
#False Negative rate
#FNR = FN/FN+TP
#Precision recall curve
library(PRROC)
PRC_curve = performance(dt.pred, "prec" , "rec")
plot(PRC_curve, colorize = T)

#################################Random Forest##############################
RF_model = randomForest(default ~ ., train, importance = TRUE, ntree = 500)

#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-9])

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$default, RF_Predictions)
confusionMatrix(ConfMatrix_RF)


#False Negative rate
#FNR = FN/FN+TP

## tune mtry

tuneRF(train[,-9], train[,9],stepfactor = 0.5, 
       plot = TRUE , ntreeTry = 1000, 
       trace = TRUE , 
       improve = 0.05)

rf1 = randomForest(default~.,data = train, ntree = 1000, mtry = 2)

rf1

# predict 

rf_pred1 = predict(rf1,test)
confusion_matric1 = table(Actualvalue=test$default, predictedvalue=rf_pred1)

print(confusion_matric1)

# no. of nodes for the trees
hist(treesize(rf1),main = " no. of nodes for the trees", col = "green")

# variable importance
varImpPlot(rf1,
           sort = T,
           main = "variable importance")

importance(rf1)
varUsed(rf1)

### Considering only max meandecreaseGini
### considering debtinc, employ, creddebt, othdeb, income.
### build model 
rf_final = randomForest(default~debtinc+employ+creddebt+othdebt+income ,
                        data = train,
                        ntree = 1000, mtry = 2)
rf_final

# prediction 
rf_pred_final = predict(rf_final,test)

confusion_matric_f = table(Actualvalue=test$default, predictedvalue=rf_pred_final)

print(confusion_matric_f)
confusionMatrix(confusion_matric_f)

class(test$default)
class(rf_pred_final)

############# Model Evaluation  #
model_evaluation(test$default,rf_pred_final)

#Accuracy = 0.73
#Precision = 0.55
#Recall = 0.34
#FNR = 0.42

class(rf_pred_final_num)
class(rf_testdata_num)
rf_pred_final_num<- as.numeric(deci_pred_1)
rf_testdata_num<- as.numeric(test$default)

library(ROCR)
data(ROCR.simple)
pred <- prediction(rf_pred_final_num,rf_testdata_num)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#AUC curve
auc<- performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<- round(auc,4)
#legend(.6,.2,auc,title="AUC",cex=4)
#False Negative rate
#FNR = FN/FN+TP

#Precision recall curve
library(PRROC)
PRC_curve = performance(pred, "prec" , "rec")
plot(PRC_curve, colorize = T)
#################################Gradient Boosting Classifier##############################
library(gbm)
library(caret) 

# train a model using our training data
mod_gbm = gbm(default ~.,
              data = train,
              distribution = "multinomial",
              cv.folds = 10,
              shrinkage = .01,
              n.minobsinnode = 10,
              n.trees = 200)

print(mod_gbm)

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)

# randomize data
random_index <- sample(1:nrow(train), nrow(train))
random_train <- train[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  mod_gbm.tune = gbm(default ~.,
                data = train,
                distribution = "multinomial",
                cv.folds = 10,
                n.trees = 200,
                interaction.depth = hyper_grid$interaction.depth[i],
                shrinkage = hyper_grid$shrinkage[i],
                n.minobsinnode = hyper_grid$n.minobsinnode[i],
                bag.fraction = hyper_grid$bag.fraction[i],
                train.fraction = .75,
                n.cores = NULL, # will use all cores by default
                verbose = FALSE
  )
  
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(mod_gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(mod_gbm.tune$valid.error))
}

hyper_grid
  dplyr::arrange(min_RMSE)
  head(10)

# modify hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
## [1] 81

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  mod_gbm.tune = gbm(default ~.,
                     data = train,
                     distribution = "multinomial",
                     cv.folds = 10,
                     n.trees = 400,
                     interaction.depth = hyper_grid$interaction.depth[i],
                     shrinkage = hyper_grid$shrinkage[i],
                     n.minobsinnode = hyper_grid$n.minobsinnode[i],
                     bag.fraction = hyper_grid$bag.fraction[i],
                     train.fraction = .75,
                     n.cores = NULL, # will use all cores by default
                     verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(mod_gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(mod_gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

# for reproducibility
set.seed(123)

# train GBM model
mod_gbm.fit.final = gbm(default ~.,
                        data = train,
                        distribution = "multinomial",
                        cv.folds = 10,
                        n.trees = 76,
                        interaction.depth = 5,
                        shrinkage = 0.05,
                        n.minobsinnode = 5,
                        bag.fraction = .80, 
                        train.fraction = 1,
                        n.cores = NULL, # will use all cores by default
                        verbose = FALSE
)

print(mod_gbm.fit.final)



# generate predictions for our held-out testing data

pred = predict.gbm(object = mod_gbm,
                   newdata = test,
                   n.trees = 76,
                   type = "response")
#The predicted result is not easy-readable data so we'll get class names with the highest prediction value.

labels = colnames(pred)[apply(pred, 1, which.max)]
result = data.frame(test$default, labels)


print(result)

#confusion matrix
cm = confusionMatrix(test$default, as.factor(labels))
print(cm)


############# Model Evaluation  #
model_evaluation(test$default, as.factor(labels))
#Accuracy = 0.70
#Precision = 0.45
#Recall = 0.11
#FNR = 0.18

str(labels)
set.seed(55555) 
pred_cont <- as.numeric(labels)
str(pred_cont)

library(ROCR)
# Use ROCR package to plot ROC Curve
gb.pred <- prediction(pred_cont,test$default)
gb.perf <- performance(gb.pred, "tpr", "fpr")

plot(gb.perf,
     avg="threshold",
     colorize=TRUE,
     lwd=1,
     main="ROC Curve w/ Thresholds",
     print.cutoffs.at=seq(0, 1, by=0.05),
     text.adj=c(-0.5, 0.5),
     text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")

#AUC curve
auc<- performance(gb.pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<- round(auc,4)
legend(.6,.2,auc,title="AUC",cex=4)
auc
#AUC=0.5298

#Precision recall curve
library(PRROC)
PRC_curve = performance(gb.pred, "prec" , "rec")
plot(PRC_curve, colorize = T)




#################################XG Boosting Classifier##############################
library(xgboost) # for xgboost
library(readr)
library(stringr)
library(caret)
library(car)

# put our testing & training data into two seperates Dmatrixs objects

df_matrix <- data.matrix(df)

# get the numb 80/20 training test split
data <- round(length(df) * .8)

# training data
train_data <- df_matrix[1:data,]
train_labels <- df_matrix[1:data]

# testing data
test_data <- df_matrix[-(1:data),]
test_labels <-df_matrix[-(1:data),ncol=1]


#Convert the cleaned dataframe to a dmatrix

dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic")
# generate predictions for our held-out testing data
pred_XG <- predict(model, dtest)


# get & print the classification error
err <- mean(as.numeric(pred_XG > 0.5) != test_labels)
print(paste("test-error=", err))

test_labels_1 <-df[-(1:data)]
length(test_labels_1$default)
length((pred_XG))

test_data_1=as.data.frame(as.table(test_data))
length(test_data_1$default)

mode(test_data) = "numeric"
test_data_2=data.frame(test_data)
length(test_data_2$default)
str(pred>0.5)
pred_fac<-as.factor(pred_XG)
testdata_fac<-as.factor(test_data_2$default)
class(pred_fac)

confusion_matric_f = table(testdata_fac,pred_fac)


print(confusion_matric_f)
confusionMatrix(confusion_matric_f)

############# Model Evaluation  #
model_evaluation(testdata_fac,pred_fac)

#Accuracy = 0.78
#Precision = 0.62
#Recall = 0.55
#FNR = 0.52

library(ROCR)

# Use ROCR package to plot ROC Curve
xgb.pred <- prediction(pred_XG, test_data_2$default)
xgb.perf <- performance(xgb.pred, "tpr", "fpr")

plot(xgb.perf,
     avg="threshold",
     colorize=TRUE,
     lwd=1,
     main="ROC Curve w/ Thresholds",
     print.cutoffs.at=seq(0, 1, by=0.05),
     text.adj=c(-0.5, 0.5),
     text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")

#AUC curve
auc<- performance(xgb.pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<- round(auc,4)
legend(.6,.2,auc,title="AUC",cex=4)
auc
#AUC=0.4693

#Precision recall curve
library(PRROC)
PRC_curve = performance(xgb.pred, "prec" , "rec")
plot(PRC_curve, colorize = T)

#### we can not decide the perfomance of model only based on the accuracy
# we need to have a good trade off between precision and recall.
# logistic model has 80.0 % accuracy with good trade-off b/t prec and recall.

##### Conclusion ======= logistic model is the best suited model on this dataset. 
#### predicting for the test data.
res_1 = predict(model6,df_test, type = "response")
range(res_1)
savehistory(file = "final_code.Rhistory")
##########extacting predicted values output from Logistic Regression model######################
results <- data.frame(test, pred_def = logit_Predictions)

write.csv(results, file = 'LG output R .csv', row.names = FALSE, quote=FALSE)

