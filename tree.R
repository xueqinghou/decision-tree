################# 1 ################
# profit = 5200-5200fnr-8700fpr


################# 2 ################
bank = read.csv("~/Desktop/BA/homework/bank.csv")
str(bank)
str(bank)
set.seed(644)
train = sample(1:nrow(bank),0.66667*nrow(bank)) 
b.train = bank[train,]  
b.test = bank[-train,]   

################# 3 ################
# grow tree 
library(rpart)
library(caret)
fit = rpart(y ~ ., # formula
            data=b.train, # dataframe used
            method="class",  #classification
            control=rpart.control(minsplit=10,cp=0))
#  num of cross validation for gini estimation
# stop splitting if node has 10 or fewer obs

# plot tree 
plot(fit, uniform=TRUE, # space out the tree evenly
     branch=0.5, # make elbow type branches
     compress=F, # make it shorter vertically
     main="Classification Tree for Bank Prediction",   # title
     margin=0.1) # leave space so it all fits
text(fit,  use.n=TRUE, # show numbers for each class
     all=TRUE, # show data for internal nodes as well
     fancy=F, # draw ovals and boxes
     pretty=T, # , # show split details
     cex=1.0) # compress fonts to 60%)  
printcp(fit)
plotcp(fit)
bestcp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
#best cp = 0.0231454

fit.small = rpart(y ~ ., 
                   data=b.train,
                   control=rpart.control(minsplit=10, cp=0.0231454))
nodes = nrow(fit.small$frame)
plot(fit.small, uniform=T, branch=0.5, compress=T,
     main="Tree with best cp, (19 nodes)", margin=0.05)
text(fit.small,  splits=T, all=F, use.n=T, 
     pretty=T, fancy=F, cex=1.2)
#b.the pruned tree have 19 nodes.

# extract the vector of predicted values for bank for every row
b.pred = predict(fit.small, b.train, type="class")
# extract the actual value of ira for every row
b.actual = b.train$y

# now make a contingency table of actual versus predicted
# this is called the confusion matrix
# for RESUBSTITUTION
library(caret)
confusion.matrixtrain = table(b.pred,b.actual)

#now let us use the hold out data in b.test
# as above, figure out the confusion matrix
btest.pred = predict(fit.small, b.test, type="class")
btest.actual = b.test$y
confusion.matrixtest= table(btest.pred,btest.actual)

# first let us look at the profit from default cutoff of 50%
cm = confusionMatrix(confusion.matrixtest, positive = 'yes')
cm

# let us look up values in the cm object
#alpha
alpha = (1-cm$byClass["Specificity"][[1]])
#beta
beta = (1-cm$byClass["Sensitivity"][[1]])

#d.alpha = 0.0287226, beta = 0.6521739

profit = 5200-5200*beta-8700*alpha

#e. profit = 1558.809

################# 4 ################
b.train.no = b.train[b.train$y == 'no',]
b.train.yes = b.train[b.train$y == 'yes',]

set.seed(234)
b.train.nosub = sample(1:nrow(b.train.no),nrow(b.train.yes)) 
b.train.nosub = b.train.no[b.train.nosub,]  
b.bal = rbind(b.train.nosub,b.train.yes)
str(b.bal)
################# 5 ################

# grow tree 
set.seed(234)
fit2 = rpart(y ~ ., # formula
            data=b.bal, # dataframe used
            method="class",  #classification
            control=rpart.control(minsplit=10,cp=0))
#  num of cross validation for gini estimation
# stop splitting if node has 10 or fewer obs

# plot tree 
plot(fit2, uniform=TRUE, # space out the tree evenly
     branch=0.5, # make elbow type branches
     compress=F, # make it shorter vertically
     main="Classification Tree for Bank Prediction",   # title
     margin=0.1) # leave space so it all fits
text(fit2,  use.n=TRUE, # show numbers for each class
     all=TRUE, # show data for internal nodes as well
     fancy=F, # draw ovals and boxes
     pretty=T, # , # show split details
     cex=1.0) # compress fonts to 60%)
printcp(fit2)
plotcp(fit2)
bestcp2 = fit2$cptable[which.min(fit2$cptable[,"xerror"]),"CP"]
#best cp =0.0148368
set.seed(234)
fit.small2 = rpart(y ~ ., 
                  data=b.bal,
                  control=rpart.control( minsplit=10,cp=0.0148368))
nodes2 = nrow(fit.small2$frame)
plot(fit.small2, uniform=T, branch=0.5, compress=T,
     main="Tree with best cp, (17 nodes)", margin=0.05)
text(fit.small2,  splits=T, all=F, use.n=T, 
     pretty=T, fancy=F, cex=1.2)
#b.the pruned tree have 17 nodes.

# extract the vector of predicted values for bank for every row
b.pred2 = predict(fit.small2, b.bal, type="class")
# extract the actual value of ira for every row
b.actual2 = b.bal$y

# now make a contingency table of actual versus predicted
# this is called the confusion matrix
# for RESUBSTITUTION
confusion.matrixbal = table(b.pred2,b.actual2)

#now let us use the hold out data in b.test
# as above, figure out the confusion matrix
btest.pred2 = predict(fit.small2, b.test, type="class")
btest.actual2 = b.test$y
confusion.matrixtest2 = table(btest.pred2,btest.actual2)

# first let us look at the profit from default cutoff of 50%
cm2 = confusionMatrix(confusion.matrixtest2, positive = 'yes')
cm2
# let us look up values in the cm object
#alpha
alpha2 = (1-cm2$byClass["Specificity"][[1]])
#beta
beta2 = (1-cm2$byClass["Sensitivity"][[1]])

#d.alpha2 = 0.2690854, beta2 = 0.125

profit2 = 5200-5200*beta2-8700*alpha2
#e. profit2 = 2208.957

################# 6 ################
library(ROCR)
##  Start by predicting prob instead of class
b.pred.prob = as.data.frame(predict(fit.small2, b.bal, type="prob"))
head(b.pred.prob)
# now we will use the ROCR package
# first step is compute the score object
b.pred.score = # first of two steps - compute the score
  prediction(b.pred.prob[,2],  # the predicted P[Yes]
             b.bal$y) # the actual class

# next step is to compute the performance object for the curve we want
b.pred.perf = performance(b.pred.score, "tpr", "fpr")

plot(b.pred.perf, 
     colorize=T, # colorize to show cutoff values
     lwd=4) # make the line 4 times thicker than default
abline(0,1)  # draw a line with intercept=0, slope = 1
abline(h=1) #draw a horizontal line at y = 1
abline(v=0) #draw a vertical line at x = 0

# General evaluation of ROC of classifiers
# area under the curve (AUC)
# note that the performance object is an S4 object
# it has slots not columns.  Pick slots using the @ operator rather than $
performance(b.pred.score, "auc")@y.values  # 0.871796

# now we will determine the optimal cutoff
b.cost = performance(b.pred.score, measure="cost", 
                         cost.fn=5200, cost.fp=8700)
plot(b.cost)
#seems to be minimized around 0.7
# we can find this more precisely
cutoff.best = b.cost@x.values[[1]][which.min(b.cost@y.values[[1]])]
cutoff.best  #0.6792453

# meaning if Prob[yes] <= custoff.best assign to NO else YES
# Let us make predictions using this cutoff rate for the bal set
# create a vector of predictions based on optimal cutoff value
b.pred.bal = predict(fit.small2, b.test, type="prob")
head(b.pred.bal)

b.pred.bal.cutoff = 
  ifelse(b.pred.bal[,2] > cutoff.best,'yes','no') 

# now let us find the profit for the bal data set using the optimal cutoff
#make the confusion matrix using table()
confusion.matrixtest3 = table(b.pred.bal.cutoff,b.test$y)
cm3 = confusionMatrix(confusion.matrixtest3, positive='yes')
cm3


#e. profit3 = 1770.093

#alpha
alpha3 = (1-cm3$byClass["Specificity"][[1]])
#beta
beta3 = (1-cm3$byClass["Sensitivity"][[1]])
#d.alpha = 0.2350718, beta = 0.2663043
profit3 = 5200-5200*beta3-8700*alpha3

#e. profit3 = 1770.093

################# 7 ################
# the expected profit company will get from the models:
# b.bal with cost minimizing cutoff > b.bal with default cutoff
# so the b.bal with cost minimizing cutoff is better.
