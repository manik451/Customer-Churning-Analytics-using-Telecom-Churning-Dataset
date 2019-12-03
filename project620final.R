churn_df<- read.csv("C://Users//HareeshManisha//Desktop//620 docs//telecom.csv", header = TRUE)
summary(churn_df)
head(churn_df)
dim(churn_df)

#area code is not of quality data beacuse there are only 3 area codes for all the states which is not correct. 
#So we are dropping it

churn_df <- churn_df[,-3] 

str(churn_df)

#converted logical data type for Churn to numeric data type
#churn_df$Churn <- as.factor(as.numeric(churn_df$Churn))

churn_df$Churn <- as.numeric(churn_df$Churn)


ggplot(churn_df, aes(x= Churn,  group=Total.day.charge)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day") +
  facet_grid(~Total.day.charge) +
  scale_y_continuous(labels = scales::percent)










#==============================================================================


#heat map for numerical variables
names(churn_df)
numerical.var <- c(2,5:19)

library(gplots)
heatmap.2(cor(churn_df[, numerical.var]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(churn_df[, numerical.var]),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))
#no multicolliearity observed apart from mins and charge for day,eve,night,intl

#####################################################################################

#scatter plots:
par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
plot(churn_df$Total.day.charge ~ churn_df$Total.day.minutes, ylab = "day.charge", xlab = "day.minutes",
     col = ifelse(churn_df$Churn ==TRUE, "red", "blue"))


par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
plot(churn_df$Total.eve.charge ~ churn_df$Total.eve.minutes, ylab = "eve.charge", xlab = "eve.minutes",
     col = ifelse(churn_df$Churn ==TRUE, "blue", "green"))


par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
plot(churn_df$Total.intl.charge ~ churn_df$Total.intl.minutes, ylab = "intl.charge", xlab = "intl.minutes",
     col = ifelse(churn_df$Churn ==TRUE, "orange", "blue"))


par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
plot(churn_df$Total.night.charge ~ churn_df$Total.night.minutes, ylab = "night.charge", xlab = "night.minutes",
     col = ifelse(churn_df$Churn ==TRUE, "blue", "green"))

####################################################################

#need below  bar plots:
#xaxis= churn and yaxis=Avg day charge 

data.for.plotc<- aggregate(churn_df$Total.day.charge,by = list(churn_df$Churn),FUN=mean)
summary(data.for.plotc)
names(data.for.plotc) <- c("Churn", "MeanTotaldaycharge") #Naming the coloumns
summary(data.for.plotc)
barplot(data.for.plotc[,2],  names.arg = data.for.plotc[,1], 
        xlab = "Churn", ylab = "Avg. Total day charge")

#xaxis= churn and yaxis=Avg customer service calls

data.for.plot2<- aggregate(churn_df$Customer.service.calls,by = list(churn_df$Churn),FUN=mean)
summary(data.for.plot2)
names(data.for.plot2) <- c("Churn", "Avg CustomerServiceCalls") #Naming the coloumns
summary(data.for.plot2)
barplot(data.for.plot2[,2],  names.arg = data.for.plot2[,1], 
        xlab = "Churn", ylab = "Avg CustomerServiceCalls")

#xaxis= churn and yaxis=Avg voice mail messages 

library(data.table)
m_data <- setDT(churn_df)[, mean(Number.vmail.messages), by=Churn]
m_data[, barplot(V1, names=Churn, main="BARPLOT", xlab="Churn", ylab="Avg Vmail messages")]


########################################################################

#pivot for categorical variables
names(churn_df)
library(reshape)
mlt <- melt(churn_df, id=c(1,3,4), measure=c("Churn"))
head(mlt, 10)
tail(mlt, 10)
summary(mlt)

# copy top 5 from r output and show it in slide
#cat var STATE:
cast(mlt, State ~ variable, mean)#we can notice that avg churn is high in the states of NJ TX CA MD(maryland) SC

#compare the avg mean churns for separately for intl and vmail. Then show both the vars together for churn in sllide
#Cat var INTL plan:
cast(mlt, International.plan ~ variable, mean)#having an intl plan seems to be increasing the avg churn

cast(mlt, International.plan ~ variable, length)#having an intl plan seems to be driving churn

#cat var vmail plan
cast(mlt, Voice.mail.plan ~ variable, mean)#having a vmail plan seems to be reducing the avg churn

cast(mlt, Voice.mail.plan ~ variable, length)#having a vmail plan seems to be reducing the avg churn

#CAT var INTKL plan and vmail plan
cast(mlt, International.plan + Voice.mail.plan ~ variable, mean)
#since having an intl plan is driving churn along with not having a voice mail plan, we notice that customers with these combination are the most likeliest to churn

#counts of churn and non churn:
cast(mlt, International.plan + Voice.mail.plan ~ variable, length)

#==============================================================================
par(mfcol = c(1, 4))
boxplot(churn_df$Account.length, ylab = "Account Length")
boxplot(churn_df$Number.vmail.messages, ylab = "#Voice mail messages", cex.lab=1.7)
boxplot(churn_df$Total.day.minutes, ylab = "Day minutes", cex.lab=1.7)
boxplot(churn_df$Total.day.calls, ylab = "Day Calls", cex.lab=1.7)
boxplot(churn_df$Total.day.charge, ylab = "Day Charge", cex.lab=1.7)
boxplot(churn_df$Total.eve.minutes, ylab = "Eve mins", cex.lab=1.7)








# regular boxplots
boxplot(churn_df$Account.length, ylab = "Account Length")
boxplot(churn_df$Number.vmail.messages, ylab = "#Voice mail messages")
boxplot(churn_df$Total.day.minutes, ylab = "Day minutes")
boxplot(churn_df$Total.day.calls, ylab = "Day Calls")
boxplot(churn_df$Total.day.charge, ylab = "Day Charge")

boxplot(churn_df$Total.eve.minutes, ylab = "Eve mins")
boxplot(churn_df$Total.eve.calls, ylab = "Eve calls")
boxplot(churn_df$Total.eve.charge, ylab = "Eve charge")

boxplot(churn_df$Total.night.minutes, ylab = "Night mins")
boxplot(churn_df$Total.night.calls, ylab = "night calls")
boxplot(churn_df$Total.night.charge, ylab = "night charge")

boxplot(churn_df$Total.intl.minutes, ylab = "intl mins")
boxplot(churn_df$Total.intl.calls, ylab = "intl calls")
boxplot(churn_df$Total.intl.charge, ylab = "intl charge")
boxplot(churn_df$Customer.service.calls, ylab = "Cust Service Calls")


#boxplots with respect to Churn
par(mfcol = c(1, 3))
boxplot(churn_df$Account.length ~ churn_df$Churn, xlab = "churn", ylab = "Acc length")
boxplot(churn_df$Number.vmail.messages ~ churn_df$Churn, xlab = "churn", ylab = "#voicemail msgs")
boxplot(churn_df$Customer.service.calls ~ churn_df$Churn, xlab = "churn", ylab = "Cust service calls")

#box plot for minutes
par(mfcol = c(1, 5))
boxplot(churn_df$Total.day.minutes ~ churn_df$Churn, xlab = "churn", ylab = "day.minutes")
boxplot(churn_df$Total.eve.minutes ~ churn_df$Churn, xlab = "churn", ylab = "eve.minutes")
boxplot(churn_df$Total.night.minutes ~ churn_df$Churn, xlab = "churn", ylab = "night.minutes")
boxplot(churn_df$Total.intl.minutes ~ churn_df$Churn, xlab = "churn", ylab = "intl.minutes")

#box plot for calls
par(mfcol = c(1, 5))
boxplot(churn_df$Total.day.calls ~ churn_df$Churn, xlab = "churn", ylab = "day.calls")
boxplot(churn_df$Total.eve.calls ~ churn_df$Churn, xlab = "churn", ylab = "eve.calls")
boxplot(churn_df$Total.night.calls ~ churn_df$Churn, xlab = "churn", ylab = "night.calls")
boxplot(churn_df$Total.intl.calls ~ churn_df$Churn, xlab = "churn", ylab = "intl.calls")

#box plot for charge
par(mfcol = c(1, 5))
boxplot(churn_df$Total.day.charge ~ churn_df$Churn, xlab = "churn", ylab = "day.charge")
boxplot(churn_df$Total.eve.charge ~ churn_df$Churn, xlab = "churn", ylab = "eve.charge")
boxplot(churn_df$Total.night.charge ~ churn_df$Churn, xlab = "churn", ylab = "night.charge")
boxplot(churn_df$Total.intl.charge ~ churn_df$Churn, xlab = "churn", ylab = "intl.charge")

#====================================================

#bar plot for cust service calls with buckets:
library(pROC)
library(C50)
library(ggplot2)
library(gridExtra)

exp4 <- ggplot(churn_df, aes(Customer.service.calls, fill = Churn)) + geom_bar(position = "fill") +
  labs(x = "Customer service calls", y = "Proportion") + theme(legend.position = "right") 
exp4



churnrate <- table(churn_df$Churn) / nrow(churn_df)
(churnrate)

#====================================================

#Model 1: Logistic model

churn_df <- read.csv("C://Users//HareeshManisha//Desktop//620 docs//telecom.csv", header = TRUE)
summary(churn_df)

churn_df <- churn_df[,-c(2,3)]
str(churn_df)
numerical.var <- churn_df[c(4:17)]

categoriacl.var <- churn_df[c(1,2,3,18)]

a<-scale(numerical.var) #Normalizes the Bedrooms column
head(a)

#install.packages("scales")
library(scales)
b<-rescale(a) #Normalizes the Bedrooms column
head(b)

churn_df <- cbind(categoriacl.var,b)


#converted logical data type for Churn to numeric data type
churn_df$Churn <- as.numeric(churn_df$Churn)



#PARTITIONING
set.seed(42)
train.rows_churn <- sample(rownames(churn_df), dim(churn_df)[1]*0.6)
valid.rows_churn <- sample(setdiff(rownames(churn_df), train.rows_churn), 
                           dim(churn_df)[1]*0.2)
test.rows_churn <- setdiff(rownames(churn_df), union(train.rows_churn, valid.rows_churn))
train.data_churn <- churn_df[train.rows_churn, ]
valid.data_churn <- churn_df[valid.rows_churn, ]
test.data_churn<- churn_df[test.rows_churn, ]

#LOGIT MODEL

logitechurn.reg <- glm(Churn ~ ., data = train.data_churn, family = "binomial") 
options(scipen=999)
summary(logitechurn.reg)

library(e1071)
library(caret)
library(forecast)
car.lm.step <- step(logitechurn.reg, direction = "both")
summary(car.lm.step) 
car.lm.step.pred <- predict(car.lm.step, valid.data_churn)
confusionMatrix(as.factor(ifelse(car.lm.step.pred > 0.003, 1, 0)), as.factor(valid.data_churn$Churn), positive = "1")



library(gains)
gain <- gains(valid.data_churn$Churn, car.lm.step.pred, groups=10)
gain


# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.data_churn$Churn))~c(0,gain$cume.obs), 
     xlab="# records", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.data_churn$Churn))~c(0, dim(valid.data_churn)[1]), lty=2)



######################################################################################



#Accuracy on test data

pred_testmodel_churn<- predict(logitebay.reg, test.data_churn)
confusionMatrix(as.factor(ifelse(pred_testmodel_churn > 0.003, 1, 0)), as.factor(test.data_churn$Churn), positive = "0")


#############################################################################################

#Model 2: Decision Trees

#DECISION TREES MODEL
#Default tree with all the predictors
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)

default.tree_churn<- rpart(Churn ~ ., data = train.data_churn, method = "class")
prp(default.tree_churn, type = 1, extra = 1, split.font = 1, varlen = -10) 

pred_validmodel_churn<- predict(default.tree_churn, valid.data_churn, type = "class")
confusionMatrix(pred_validmodel_churn, as.factor(valid.data_churn$Churn), positive = "0")

#on testing data
pred_testmodel_churn<- predict(default.tree_churn, test.data_churn, type = "class")
confusionMatrix(pred_testmodel_churn, as.factor(test.data_churn$Churn), positive = "0")


#Pruning:
#deeper.tree <- rpart(Churn ~ ., data = train.data_churn, cp=0, xval=5, method = "class")
#prp(deeper.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  
#printcp(deeper.tree)
#optimal_cp <- deeper.tree$cptable[which.min(deeper.tree$cptable[,"xerror"]),"CP"]
#optimal_cp
#ptree <- prune(class.tree.train.deeper, cp = optimal_cp) 
#prp(ptree, type = 1, extra = 1, split.font = 1, varlen = -10)  
#ptree.valid.prediction <- predict(ptree, valid.data_churn, type = "class")
#confusionMatrix(ptree.valid.prediction, as.factor(valid.data_churn$Churn))"

#rules of the tree
library(rattle)
asRules(default.tree_churn)










#################################################################################
#Model: 4 KNN
set.seed(42)
train.rows_churn <- sample(rownames(churn_df), dim(churn_df)[1]*0.6)
valid.rows_churn <- sample(setdiff(rownames(churn_df), train.rows_churn), 
                           dim(churn_df)[1]*0.2)
test.rows_churn <- setdiff(rownames(churn_df), union(train.rows_churn, valid.rows_churn))
train.data_churn <- churn_df[train.rows_churn, ]
valid.data_churn <- churn_df[valid.rows_churn, ]
test.data_churn<- churn_df[test.rows_churn, ]

train.norm.df <- train.data_churn
valid.norm.df <- valid.data_churn
test.norm.df <- test.data_churn
churn.norm.df <- churn_df

str(churn_df)
library(caret)

norm.values <-preProcess(train.data_churn[, c(1:18)], method=c("center", "scale"))

train.norm.df[, c(1:18)] <- predict(norm.values, train.data_churn[, c(1:18)])
valid.norm.df[, c(1:18)] <- predict(norm.values, valid.data_churn[, c(1:18)])
test.norm.df[, c(1:18)] <- predict(norm.values, test.data_churn[, c(1:18)])
churn.norm.df[, c(1:18)] <- predict(norm.values, churn_df[, c(1:18)])
class(train.norm.df)

# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable).
#install.packages("FNN")

library(caret)
library(e1071)
library(FNN)

#Since the dataset is small i am taking total iteration 10.
# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1,20,1), accuracy = rep(0, 20)) 

# compute knn for different k on validation.

for(i in 1:20) {
  knn.pred <- knn(train = train.norm.df[, c(1:18)], test = valid.norm.df[, c(1:18)], 
                  cl = train.norm.df[, 19], k = i) 
  
  accuracy.df[i, 2] <- confusionMatrix(table(knn.pred, valid.norm.df[, 19]))$overall[1]
}
accuracy.df

confusionMatrix(knn.pred, as.factor(test.norm.df[, 19]),positive="1" )









###############################################################################
#Model 3 neural nets

churn_df <- read.csv("C://Users//HareeshManisha//Desktop//620 docs//telecom.csv", header = TRUE)

str(churn_df)
selected.var <- c(2, 6:20)
numerical.var <- c(2, 6:20)
churn_df <- churn_df[, numerical.var]

#normalization data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
} # set normalize function
toyota.temp.norm <- as.data.frame(lapply(churn_df, normalize)) # normalize the data

# partitioning data
set.seed(1)
train.rows_churn <- sample(rownames(churn_df), dim(churn_df)[1]*0.6)

# sample 30% of the row IDs into the validation set, drawing only from records not already in the training set
# use setdiff() to find records not already in the training set
valid.rows_churn <- sample(setdiff(rownames(churn_df), train.rows_churn), 
                           dim(churn_df)[1]*0.2)

# assign the remaining 20% row IDs serve as test
test.rows_churn <- setdiff(rownames(churn_df), union(train.rows_churn, valid.rows_churn))

# create the 3 data frames by collecting all columns from the appropriate rows 
train.data_churn <- churn_df[train.rows_churn, ]

valid.data_churn <- churn_df[valid.rows_churn, ]
test.data_churn<- churn_df[test.rows_churn, ]


# build neural network and plot
nn <- neuralnet(Churn ~ ., data = train.data_churn, linear.output = F, hidden=2)
plot(nn,rep='best')

reg1 <- lm(Churn ~ Account.length+Number.vmail.messages+Total.day.minutes+Total.day.calls +Total.day.charge +
             Total.eve.minutes+Total.eve.calls+Total.eve.charge +Total.night.minutes+Total.night.calls+
             Total.night.charge+Total.intl.minutes+Total.intl.calls+Customer.service.calls, train.data_churn)
str(churn_df)
library(forecast)
pred_reg3 <- predict(reg1, newdata = valid.data_churn)
accuracy(pred_reg3, valid.data_churn$Churn)



#Neural nets (method 2, with 0.5 threshold)


dim(churn_df)
train.data_churn[,1]<- as.numeric(train.data_churn[,1])
train.data_churn[,2]<- as.numeric(train.data_churn[,2])
train.data_churn[,3]<- as.numeric(train.data_churn[,3])
valid.data_churn[,1]<-as.numeric(valid.data_churn[,1])
valid.data_churn[,2]<-as.numeric(valid.data_churn[,2])
valid.data_churn[,3]<-as.numeric(valid.data_churn[,3])

#Normalizing all the predictors and outcome in the dataset
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#normalizing training and validation sets after partitioning separatley
train.data_normc<- as.data.frame(lapply(train.data_churn, normalize))
valid.data_normc<- as.data.frame(lapply(valid.data_churn, normalize))
dim(train.data_churn)

#library(caret)
#install.packages("neuralnet")
library(neuralnet)
options(expressions = 5e5)
getwd()

nn_churn <- neuralnet(Churn ~ ., data = train.data_normc, linear.output = F, hidden = 18)
head(nn_churn)

# display weights
nn_churn$weights
plot(nn_churn,rep='best')

model3 <- compute(nn_churn, valid.data_normc)
model3.class <-ifelse(model3$net.result > .5, 1, 0)
confusionMatrix(as.factor(model3.class),as.factor(valid.data_normc$Churn),positive="1")


model_test <- compute(nn_churn, valid.data_normc)
model3.class <-ifelse(model3$net.result > .5, 1, 0)
confusionMatrix(as.factor(model3.class),as.factor(valid.data_normc$Churn),positive="1")

#######################################################################################








