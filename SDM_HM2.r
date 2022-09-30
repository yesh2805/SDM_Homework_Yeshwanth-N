library("ggplot2")#Installing Necessary Packages

#install.packages("ISLR2")

library("ISLR2")

library("dplyr")

mycollege <- College

head(mycollege)

corrcoll <- College
corrcoll$Private <- as.numeric(corrcoll$Private) 

#install.packages("corrplot")
library(corrplot)

mat <- cor(corrcoll)

#Correlation Plot for the whole Data
corrplot(mat,order="AOE",method = "color", addCoeff.col = "gray")

plot(mycollege)

colnames(mycollege)

#Histogram For Private COllege or Not!
#hist(as.numeric(mycollege$Private), main = "Private College Yes/No-- Histogram",breaks = 4, col = "orange", xlab = "Private Yes/NO")
#boxplot(mycollege$Private,horizontal=TRUE)$out

ggplot(mycollege,aes(factor(Private))) + 
    geom_bar(aes(fill = factor(Private))) + 
    labs(x="Private College or Not") +
    scale_fill_discrete(name = "Private College or Not")


#Find out Outlier for EACH COLOUMN- Application Count and Applying Transformations(log)
par(mfrow =c(2,2))
hist(mycollege$Apps,main = "Application Count -- Histogram",col = "red", xlab = "Applications Count")
boxplot(mycollege$Apps,main = "BoxPlot-Application Count", xlab = "Applications Count",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Application Count
mycollege = mycollege[-which(mycollege$Apps>20000),]
hist(mycollege$Apps,main = "After Outliers Removal ",col = "green", xlab = "Applications Count")
mycollege$Apps = log(mycollege$Apps)
hist(mycollege$Apps,main = "Applying Log Tranformation ",col = "blue", xlab = "Applications Count")

#Find out Outlier for EACH COLOUMN- Enroll
par(mfrow =c(2,2))
hist(mycollege$Enroll,main = "Enroll Count - Histogram",col = "red", xlab = "Enrollment Count")
boxplot(mycollege$Enroll,main = "BoxPlot - Enrollment ", xlab = "Enrollment",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Enrollment Count
mycollege = mycollege[-which(mycollege$Enroll>5000),]
hist(mycollege$Enroll,main = "After Outliers Removal ",col = "green", xlab = "Enrollment")
mycollege$Enroll = log(mycollege$Enroll)
hist(mycollege$Enroll,main = "Applying Log Tranformation ",col = "blue", xlab = "Enrollment")

#Find out Outlier for EACH COLOUMN- Accept
par(mfrow =c(2,2))
hist(mycollege$Accept,main = "Acceptance - Histogram",col = "red", xlab = "Acceptance")
boxplot(mycollege$Accept,main = "Acceptance - BoxPlot", xlab = "Acceptance",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Enrollment Count
mycollege = mycollege[-which(mycollege$Accept>12000),]
hist(mycollege$Accept,main = "After Outliers Removal ",col = "green", xlab = "Acceptance")
mycollege$Accept = log(mycollege$Accept)
hist(mycollege$Accept,main = "Applying Log Tranformation ",col = "blue", xlab = "Acceptance")

#Find out Outlier for EACH COLOUMN- Top10perc
par(mfrow =c(2,2))
hist(mycollege$Top10perc,main = "Top10perc - Histogram",col = "red", xlab = "Top10perc")
boxplot(mycollege$Top10perc,main = "Top10perc - BoxPlot", xlab = "Top10perc",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Enrollment Count
mycollege = mycollege[-which(mycollege$Top10perc>90),]
hist(mycollege$Top10perc,main = "After Outliers Removal ",col = "green", xlab = "Top10perc")
mycollege$Top10perc = log(mycollege$Top10perc)
hist(mycollege$Top10perc,main = "Applying Log Tranformation ",col = "blue", xlab = "Top10perc")

#Find out Outlier for EACH COLOUMN- Top25perc Tranformations NOT NEEDED HERE!!
par(mfrow =c(2,1))
hist(mycollege$Top25perc,main = "Top25perc - Histogram",col = "red", xlab = "Top25perc")
boxplot(mycollege$Top25perc,main = "Top25perc - BoxPlot", xlab = "Top25perc",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Enrollment Count
#mycollege = mycollege[-which(mycollege$Top25perc>90),]
#hist(mycollege$Top25perc,main = "After Outliers Removal ",col = "green", xlab = "Top25perc")
#mycollege$Top25perc = log(mycollege$Top10perc)
#hist(mycollege$Top25perc,main = "Applying Log Tranformation ",col = "blue", xlab = "Top25perc")

#Find out Outlier for EACH COLOUMN- F.Undergrad and Applying Log Tranformation
par(mfrow =c(2,2))
hist(mycollege$F.Undergrad,main = "F.Undergrad - Histogram",col = "red", xlab = "F.Undergrad")
boxplot(mycollege$F.Undergrad,main = "F.Undergrad - BoxPlot", xlab = "F.Undergrad",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- F.Undergrad Count
mycollege = mycollege[-which(mycollege$F.Undergrad>20000),]
hist(mycollege$F.Undergrad,main = "After Outliers Removal ",col = "green", xlab = "F.Undergrad")
mycollege$F.Undergrad = log(mycollege$F.Undergrad)
hist(mycollege$F.Undergrad,main = "Applying Log Tranformation ",col = "blue", xlab = "F.Undergrad")

#Find out Outlier for EACH COLOUMN- P.Undergrad and Applying Log Tranformation
par(mfrow =c(2,2))
hist(mycollege$P.Undergrad,main = "P.Undergrad - Histogram",col = "red", xlab = "P.Undergrad")
boxplot(mycollege$P.Undergrad,main = "P.Undergrad - BoxPlot", xlab = "P.Undergrad",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- F.Undergrad Count
mycollege = mycollege[-which(mycollege$P.Undergrad>10000),]
hist(mycollege$P.Undergrad,main = "After Outliers Removal ",col = "green", xlab = "P.Undergrad")
mycollege$P.Undergrad = log(mycollege$P.Undergrad)
hist(mycollege$P.Undergrad,main = "Applying Log Tranformation ",col = "blue", xlab = "P.Undergrad")

#Find out Outlier for EACH COLOUMN- Outstate and Transformation Not Needed
par(mfrow =c(2,2))
hist(mycollege$Outstate,main = "Outstate - Histogram",breaks = 50, col = "red", xlab = "Outstate")
boxplot(mycollege$Outstate,main = "Outstate - BoxPlot", xlab = "Outstate",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- F.Undergrad Count
mycollege = mycollege[-which(mycollege$Outstate>20000),]
hist(mycollege$Outstate,main = "After Outliers Removal ",breaks = 50, col = "green", xlab = "Outstate")
#mycollege$Outstate = log(mycollege$Outstate)
#hist(mycollege$Outstate,main = "Applying Log Tranformation ",col = "blue", xlab = "Outstate")

#Find out Outlier for EACH COLOUMN- Room.Board and Transformation Not Needed
par(mfrow =c(2,2))
hist(mycollege$Room.Board,main = "Room.Board - Histogram",breaks = 20, col = "red", xlab = "Room.Board")
boxplot(mycollege$Room.Board,main = "Room.Board - BoxPlot", xlab = "Room.Board",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Room.Board Count
mycollege = mycollege[-which(mycollege$Room.Board>8000),]
hist(mycollege$Room.Board,main = "After Outliers Removal ",breaks = 50, col = "green", xlab = "Room.Board")
#mycollege$Outstate = log(mycollege$Outstate)
#hist(mycollege$Outstate,main = "Applying Log Tranformation ",col = "blue", xlab = "Outstate")

#Find out Outlier for EACH COLOUMN- Books and Transformation Not Needed
par(mfrow =c(2,2))
hist(mycollege$Books,main = "Books - Histogram",breaks = 20, col = "red", xlab = "Books")
boxplot(mycollege$Books,main = "Books - BoxPlot", xlab = "Books",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Books Count
mycollege = mycollege[-which(mycollege$Books>1100),]
hist(mycollege$Books,main = "After Outliers Removal ",breaks = 20, col = "green", xlab = "Books")
mycollege$Books = log(mycollege$Books)
hist(mycollege$Books,main = "Applying Log Tranformation ",col = "blue", xlab = "Books")

#Find out Outlier for EACH COLOUMN- Personal and Applying Log Transformation
par(mfrow =c(2,2))
hist(mycollege$Personal,main = "Personal - Histogram",breaks = 20, col = "red", xlab = "Personal")
boxplot(mycollege$Personal,main = "Personal - BoxPlot", xlab = "Personal",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Books Count
mycollege = mycollege[-which(mycollege$Personal>4000),]
hist(mycollege$Personal,main = "After Outliers Removal ",breaks = 20, col = "green", xlab = "Personal")
mycollege$Personal = log(mycollege$Personal)
hist(mycollege$Personal,main = "Applying Log Tranformation ",col = "blue", xlab = "Personal")

#Find out Outlier for EACH COLOUMN- Terminal and Applying Log Transformation
par(mfrow =c(2,2))
hist(mycollege$Terminal,main = "Terminal - Histogram",breaks = 20, col = "red", xlab = "Terminal")
boxplot(mycollege$Terminal,main = "Terminal - BoxPlot", xlab = "Terminal",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Terminal Count
mycollege = mycollege[-which(mycollege$Terminal<40),]
hist(mycollege$Terminal,main = "After Outliers Removal ",breaks = 20, col = "green", xlab = "Terminal")
mycollege$Terminal = log(mycollege$Terminal)
hist(mycollege$Terminal,main = "Applying Log Tranformation ",col = "blue", xlab = "Terminal")

#Find out Outlier for EACH COLOUMN- PhD and No Outliers and Transformation.
par(mfrow =c(2,2))
hist(mycollege$PhD,main = "PhD - Histogram",breaks = 20, col = "red", xlab = "PhD")
boxplot(mycollege$PhD,main = "PhD - BoxPlot", xlab = "PhD",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- PhD Count
mycollege = mycollege[-which(mycollege$PhD<25),]
hist(mycollege$PhD,main = "After Outliers Removal ",breaks = 20, col = "green", xlab = "PhD")
mycollege$PhD = sqrt(mycollege$PhD)
hist(mycollege$PhD,main = "Applying sqrt Tranformation ",col = "blue", xlab = "PhD")

#Find out Outlier for EACH COLOUMN- S.F.Ratio and No  Transformation.
par(mfrow =c(2,2))
hist(mycollege$S.F.Ratio,main = "S.F.Ratio - Histogram",breaks = 20, col = "red", xlab = "S.F.Ratio")
boxplot(mycollege$S.F.Ratio,main = "S.F.Ratio - BoxPlot", xlab = "S.F.Ratio",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- S.F.Ratio Count
mycollege = mycollege[-which(mycollege$S.F.Ratio>30),]
hist(mycollege$S.F.Ratio,main = "After Outliers Removal ",breaks = 20, col = "green", xlab = "S.F.Ratio")
mycollege$S.F.Ratio = log(mycollege$S.F.Ratio)
hist(mycollege$S.F.Ratio,main = "Applying Log Tranformation ",col = "blue", xlab = "S.F.Ratio")

#Find out Outlier for EACH COLOUMN- perc.alumni and apply log
par(mfrow =c(2,2))
hist(mycollege$perc.alumni,main = "perc.alumni - Histogram",breaks = 20, col = "red", xlab = "perc.alumni")
boxplot(mycollege$perc.alumni,main = "perc.alumni - BoxPlot", xlab = "perc.alumni",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- perc.alumni Count
mycollege = mycollege[-which(mycollege$perc.alumni>60),]
hist(mycollege$perc.alumni,main = "After Outliers Removal ",breaks = 20, col = "green", xlab = "perc.alumni")
#mycollege$perc.alumni = log(mycollege$perc.alumni)
#hist(mycollege$perc.alumni,main = "Applying Log Tranformation ",col = "blue", xlab = "perc.alumni")

#Find out Outlier for EACH COLOUMN- Expend and apply log
par(mfrow =c(2,2))
hist(mycollege$Expend,main = "Expend - Histogram",breaks = 20, col = "red", xlab = "Expend")
boxplot(mycollege$Expend,main = "Expend - BoxPlot", xlab = "Expend",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Expend Count
mycollege = mycollege[-which(mycollege$Expend>30000),]
hist(mycollege$Expend,main = "After Outliers Removal ",breaks = 20, col = "green", xlab = "Expend")
mycollege$Expend = log(mycollege$Expend)
hist(mycollege$Expend,main = "Applying Log Tranformation ",col = "blue", xlab = "Expend")

colnames(mycollege)

#Find out Outlier for EACH COLOUMN- Grad.Rate NO TRANFORMATION NEEDED
par(mfrow =c(2,2))
hist(mycollege$Grad.Rate,main = "Grad.Rate - Histogram",col = "red", xlab = "Grad.Rate")
boxplot(mycollege$Grad.Rate,main = "Grad.Rate - BoxPlot", xlab = "Grad.Rate",horizontal=TRUE)$out
#Removing Outliers for EACH COLOUMN- Grad.Rate Count
#mycollege = mycollege[-which(mycollege$Grad.Rate>110),]
mycollege = mycollege[-which(mycollege$Grad.Rate<20),]
hist(mycollege$Grad.Rate,main = "After Outliers Removal",col = "green", xlab = "Grad.Rate")
mycollege$Grad.Rate = sqrt(mycollege$Grad.Rate)
hist(mycollege$Grad.Rate,main = "Applying sqrt Tranformation ",col = "blue", xlab = "Grad.Rate")

library(dplyr)

library(ISLR)


library(lattice)

library(caret)

splits <- createDataPartition(mycollege$Apps, p = 0.75, list = FALSE)

train_set <- mycollege[splits,]
test_set <- mycollege[-splits,]
print("Rows count in training set:")
nrow(train_set)
print("Rows count in testing set:")
nrow(test_set)
#train_set


sum(is.infinite(train_set$Grad.Rate))#checking if there are any infinite values after Log transformation.

lin_model <- lm(Apps~., data = train_set)

model_prediction <- predict(lin_model, test_set)

MSE <- mean((model_prediction-test_set$Apps)**2)
cat("Mean Squared Error          -", MSE ,"\n")

cat("Accuracy of Linear Model    -", (100 * summary(lin_model)$r.squared),"%")

#lin_info <- postResample(model_prediction, test_set$Apps)
#lin_info

Accuracy1 <- (100 * summary(lin_model)$r.squared)

plot(lin_model)

#Load the Data
X <- model.matrix(Apps~.,data = mycollege[,-1])
head(X)


X <- model.matrix(Apps~.,data = mycollege[,-1])
Y <- mycollege$Apps

library(glmnet)

ridge.mod = glmnet(X, Y, alpha=0)
names(ridge.mod)

set.seed(45)
train <- sample(1:nrow(X), round(nrow(X)/2))

cv.out <- cv.glmnet(X[train,], Y[train], alpha = 0)
plot(cv.out)




names(cv.out)
bestlam <- cv.out$lambda.min
bestlam


ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")

ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X[-train,], type = "response")



ridge.mod$lambda[50]
coef(ridge.mod)[,50]

y_hat <- ridge.pred2
y_true <- Y[-train]
test_error <- mean((y_hat - y_true)**2)  #test_error
test_error

test.avg <-  mean(y_true)
p2_ridge_accuracy <-  1 - (test_error /mean((Y[-train]- test.avg)^2))

Accuracy2 <- (p2_ridge_accuracy * 100)

lasso.mod <- glmnet(X[train,], Y[train], alpha = 1)
#x11()
plot(lasso.mod)




# LEts look at coefficients....

lasso.mod$lambda[70]
coef(lasso.mod)[,70]

lasso.mod$lambda[50]
coef(lasso.mod)[,50]

lasso.mod$lambda[20]
coef(lasso.mod)[,20]


# Best Lambda
cv.out = cv.glmnet(X[train,], Y[train], alpha = 1)
bestlam = cv.out$lambda.min

lasso.pred <- predict(lasso.mod, s = bestlam, type = "coefficients")

lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = X[-train,], type = "response")

y_hat_lasso <- lasso.pred2
y_true <- Y[-train]

test_error_lasso <- mean((y_hat_lasso-y_true)^2) #26.33054
test_error_lasso



test.avg1 <-  mean(y_true)
p2_lasso_accuracy <-  1 - (test_error_lasso /mean((Y[-train]- test.avg1)^2))

Accuracy3 <- (p2_lasso_accuracy * 100)

rep.data <- data.frame(
   model_name = c ("Linear", "Ridge", "Lasso"), 
   Test_error = c(MSE,test_error,test_error_lasso),
   mod_accuracy = c(Accuracy1, Accuracy2, Accuracy3))

print(rep.data)
   

library(leaps)

train_prob2 <- read.table(file.path(getwd(), "ticdata2000.txt"))
test_wot <- read.table(file.path(getwd(), "ticeval2000.txt"))
train_wot <- read.table(file.path(getwd(), "tictgts2000.txt"))

dim(train_prob2)
dim(test_wot)
dim(train_wot)

head(train_prob2)
head(test_wot)
head(train_wot)

tot_test <- cbind(test_wot,train_wot)
head(tot_test,)

names(tot_test)[86] <- "V86"

head(tot_test)
dim(tot_test)

total_dt <- rbind(train_prob2,tot_test)
head(total_dt)


ggplot(total_dt,aes(factor(V86))) + 
    geom_bar(aes(fill = factor(V86))) + 
    labs(x="Caravan_Total") +
    scale_fill_discrete(name = "Caravan_Total")

ggplot(total_dt,aes(factor(V4))) + 
    geom_bar(aes(fill = factor(V86))) + 
    geom_text(stat='count', aes(label=..count..), vjust=0) + 
    labs(x="Age Group") +
    scale_fill_discrete(name = "Caravan") + 
    ggtitle("Group distribution by Age who  opt for Policy") +
    theme(plot.title = element_text(hjust = 0.7))

#Age Group 2 count
nrow(total_dt[total_dt$V4 == 2 & total_dt$V86 == 1,])

#Age Group 3 count
nrow(total_dt[total_dt$V4 == 3 & total_dt$V86 == 1,])

# Age Group 4 count
nrow(total_dt[total_dt$V4 == 4 & total_dt$V86 == 1,])

Age_grp <- unique(total_dt$V4)

age_grp_ratio <- data.frame('age_grp' = NULL, 'ratio' = NULL)

for (i in 1:length(Age_grp))   
{
     # total in each group
    total_cust <- nrow(total_dt[total_dt$V4 == Age_grp[i],])
    
    # customer who brought
    Purchased <- nrow(total_dt[total_dt$V4 == Age_grp[i] & total_dt$V86 == 1,])
    
    age_grp_ratio[i, "age_grp"] <- Age_grp[i]
    age_grp_ratio[i, "ratio"] <- round((Purchased / total_cust) * 100 , 3)
}

age_grp_ratio[order(age_grp_ratio$ratio), ]

ggplot(age_grp_ratio,aes(x = factor(age_grp), y = ratio)) + 
    geom_bar(stat = "identity",aes(fill = factor(age_grp))) + 
    geom_text(aes(label = ratio), vjust = 0) +
    labs(x="Age Grouping of customers", y = "Ratio (%)") +
    scale_fill_discrete(name = "Grouping of Ages") +
    ggtitle("Using Age Group basis policy purchase ration of Caravan") + 
    theme(plot.title = element_text(hjust = 0.5))

# Perform subset selection on the tic data
regfit.full1 <- regsubsets(V86~., data = total_dt, nbest = 1, nvmax = 85, method = "forward")
my_sum1 <- summary(regfit.full1)
names(my_sum1)

my_sum1$rss

par(mfrow = c(2,2))
plot(my_sum1$cp, xlab = "No. of variables", ylab = "Cp", type = "l",main = paste("Min Error at",which.min(my_sum1$cp), "Variables"))
points(which.min(my_sum1$cp), my_sum1$cp[which.min(my_sum1$cp)], col = "steelblue", cex = 2, pch = 20)

plot(my_sum1$bic, xlab = "No. of variables", ylab = "BIC", type = "l",main = paste("Min Error at",which.min(my_sum1$bic), "Variables"))
points(which.min(my_sum1$bic), my_sum1$bic[which.min(my_sum1$bic)], col = "darkgoldenrod", cex = 2, pch = 20)

plot(my_sum1$rss, xlab = "No. of variables", ylab = "RSS", type = "l", main = paste("Min Error at",which.min(my_sum1$rss), "Variables"))
points(which.min(my_sum1$rss), my_sum1$rss[which.min(my_sum1$rss)], col = "cyan", cex = 2, pch = 20)
     
     
plot(my_sum1$adjr2, xlab = "No. of variables", ylab = "Adjusted Rsq", type = "l",,main = paste("Max AdjR2",which.max(my_sum1$adjr2), "Variables"))
points(which.max(my_sum1$adjr2), my_sum1$adjr2[which.max(my_sum1$adjr2)], col = "magenta", cex = 2, pch = 20)


######################################################
# Look at subset selection using test/training data
######################################################
predict.regsubsets = function(object, newdata, id){
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
    }

y_true_train = train_prob2$V86
y_true_test = tot_test$V86

# create objects to store error
train_err_store <- matrix(rep(NA, 85))
test_err_store <- matrix(rep(NA, 85))
regfit.fwd <- regsubsets(V86~., data = total_dt, nbest = 1, nvmax = 85, method = "forward") # perform subset selection
for (i in 1:85){
    # make the predictions
    y_hat_train = predict(regfit.fwd, newdata = train_prob2, id = i)
    y_hat_test = predict(regfit.fwd, newdata = tot_test, id = i)
    
    # compare the prediction with the true
    train_err_store[i] = (1/length(y_true_train))*sum((y_true_train-y_hat_train)^2)
    test_err_store[i] = (1/length(y_true_test))*sum((y_true_test-y_hat_test)^2)

}
#train_err_store
forward_err <- min(test_err_store)


plot(train_err_store, col = "blue", type = "b", xlab = "No. of variables", ylab = "MSE",ylim = c(0.0520,0.0551))
lines(test_err_store, col = "red", type = "b")
which(test_err_store == min(test_err_store))


y_true_train = train_prob2$V86
y_true_test = tot_test$V86

# create objects to store error
train_err_store <- matrix(rep(NA, 85))
test_err_store <- matrix(rep(NA, 85))
regfit.fwd <- regsubsets(V86~., data = total_dt, nbest = 1, nvmax = 85, method = "backward") # perform subset selection
for (i in 1:85){
    # make the predictions
    y_hat_train = predict(regfit.fwd, newdata = train_prob2, id = i)
    y_hat_test = predict(regfit.fwd, newdata = tot_test, id = i)
    
    # compare the prediction with the true
    train_err_store[i] = (1/length(y_true_train))*sum((y_true_train-y_hat_train)^2)
    test_err_store[i] = (1/length(y_true_test))*sum((y_true_test-y_hat_test)^2)

}
backward_err <- min(test_err_store)

plot(train_err_store, col = "blue", type = "b", xlab = "No. of variables", ylab = "MSE", ylim = c(0.0520,0.0551))
lines(test_err_store, col = "red", type = "b")
which(test_err_store == min(test_err_store))


head(tot_test)

X <- model.matrix(V86~.,data = tot_test)
Y <-tot_test$V86

library(glmnet)

ridge.mod = glmnet(X, Y, alpha=0)
names(ridge.mod)

set.seed(45)
train <- sample(1:nrow(X), round(nrow(X)/2))

cv.out <- cv.glmnet(X[train,], Y[train], alpha = 0)
plot(cv.out)


names(cv.out)
bestlam <- cv.out$lambda.min
bestlam


ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")

ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X[-train,], type = "response")


y_hat <- ridge.pred2
y_true <- Y[-train]
test_error_ridge <- mean((y_hat - y_true)^2)  #test_error
test_error_ridge

lasso.mod <- glmnet(X[train,], Y[train], alpha = 1)
#x11()
plot(lasso.mod)


# Best Lambda
cv.out = cv.glmnet(X[train,], Y[train], alpha = 1)
bestlam = cv.out$lambda.min

lasso.pred <- predict(lasso.mod, s = bestlam, type = "coefficients")

lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = X[-train,], type = "response")

y_hat_lasso <- lasso.pred2
y_true <- Y[-train]

test_error_lasso <- mean((y_hat_lasso-y_true)^2) #26.33054
test_error_lasso

ticr.data <- data.frame(
   model_name = c ("Forward","Backward","Ridge", "Lasso"), 
   Test_error = c(forward_err,backward_err,test_error_ridge,test_error_lasso))

print(ticr.data)

train_set1 <- read.table(file.path(getwd(), "zip.train.gz"))
test_set1  <- read.table(file.path(getwd(), "zip.test.gz"))

colnames(train_set1)

## Filtering to 7’s and 9’s and desired variables.
train <- train_set1[train_set1[,1] %in% c(7, 9),]
test <- test_set1[test_set1[,1] %in% c(7, 9),]

dim(train)
dim(test)

#Linear Regression
lin_model <- lm(V1~., data = train)

model_pred <- predict(lin_model, test)

MeanSqE <- mean((model_pred - test$V1)**2)
MeanSqE

library(class)

# Running K-nearest neighbors.
library(class)
b <- c(1,3,5,7,9,11,13,15)
for(k in 1:length(b))
{
    pred.vals.knn <- knn(train[,1:257], test[,1:257], train[,1], b[k])
    error.rate.knn <- mean(pred.vals.knn!=test[,1])
    print(paste("For k =", k ,"->","Error","->", error.rate.knn) )
}
