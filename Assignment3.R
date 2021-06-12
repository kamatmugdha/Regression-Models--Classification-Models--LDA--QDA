library(class)    # for KNN
library(ISLR)     # for data
library(MASS)     # for LDA

head(Weekly)
summary(Weekly)

cor(Weekly[, -9])

pairs(Weekly)

glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)

glm.probs = predict(glm.fit, type="response")
glm.preds = rep("Down", 1089)
glm.preds[glm.probs > 0.5] = "Up"
table(glm.preds, Weekly$Direction)

plot(glm.probs, col= ifelse(Weekly$Direction=="Down", "red","green"), pch=16)
abline(h = 0.5, lwd= 3)

training.data = Weekly[Weekly$Year<2009,]
test.data = Weekly[Weekly$Year>2008,]
simpglm = glm(Direction~Lag2, data= training.data, family = "binomial")
summary(simpglm)

testprobs = predict(simpglm, type="response", newdata = test.data)
testdirs = Weekly$Direction[Weekly$Year>2008]
plot(testprobs, col= ifelse(Weekly$Direction[Weekly$Year>2008]=="Down", "red","green"), pch=16)
abline(h = 0.5, lwd= 3)

testpreds = rep("Down", 104)
testpreds[testprobs>0.5] = "Up"
#mean(probs)
table(testpreds, testdirs)

lda.fit = lda(Direction~Lag2, data= training.data)
lda.fit

lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
table(lda.class, test.data$Direction)

qda.fit = qda(Direction~Lag2, data= training.data)
qda.fit

qda.pred = predict(qda.fit, newdata=test.data, type="response")
qda.class = qda.pred$class
table(qda.class, test.data$Direction)

set.seed(1)
train.X = cbind(training.data$Lag2)
test.X = cbind(test.data$Lag2)
train.Y = cbind(training.data$Direction)
knn.pred = knn(train.X, test.X, train.Y, k=1)
table(knn.pred, test.data$Direction)

knn3.pred = knn(train.X, test.X, train.Y, k=3)
table(knn3.pred, test.data$Direction)

qda.fit2 = qda(Direction~Lag1 + Lag2 + Lag4, data= training.data)
qda.fit2

qda.pred2 = predict(qda.fit2, newdata=test.data, type="response")
qda.class2 = qda.pred2$class
table(qda.class2, test.data$Direction)

lda.fit2 = lda(Direction~Lag1 + Lag2 + Lag4, data= training.data)
lda.fit2

lda.pred2 = predict(lda.fit2, newdata=test.data, type="response")
lda.class2 = lda.pred2$class
table(lda.class2, test.data$Direction)


auto = Auto
auto$mpg01 = rep(0, length(auto$mpg))
auto$mpg01[auto$mpg>median(auto$mpg)] = 1
head(auto)

median(auto$mpg)
cor(auto[, -9])

par(mfrow=c(2,2))
plot(auto$year, auto$acceleration, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)
plot(auto$year, auto$weight, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)
plot(auto$year, auto$horsepower, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)
plot(auto$year, auto$displacement, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)

train <- (auto$year %% 2 == 0)
auto.train <- auto[train, ]
auto.test <- auto[!train, ]



lda.fit2 = lda(mpg01~displacement+weight+cylinders+year, data=auto ,subset=train)
lda.fit2
pred.lda2 = predict(lda.fit2, newdata = auto.test, type="response")$class
table(pred.lda2, auto.test$mpg01)



qda.fit2 = qda(mpg01~displacement+weight+cylinders+year, data=auto ,subset=train)
qda.fit2
pred.qda2 = predict(qda.fit2, newdata = auto.test, type="response")$class
table(pred.qda2, auto.test$mpg01)
mean(pred.qda2 != auto.test$mpg01)

logisticreg = glm(mpg01~displacement+weight+cylinders+year, family="binomial", data=auto)
summary(logisticreg)

log_probs = predict(logisticreg, auto.test, type="response")
log_preds = rep(0, dim(auto.test)[1])
log_preds[log_probs>0.5]=1            
table(log_preds,auto.test$mpg01 )
mean(log_preds != auto.test$mpg01)

par(mfrow=c(1,1))
plot(log_probs, col= ifelse(auto.test$mpg01==0, "red", "blue"), pch = 16)
abline(h=0.5, lwd=3)


train_auto_X <- cbind(auto$cylinders, 
                      auto$displacement, 
                      auto$horsepower,
                      auto$weight
                      )[train,]

test_auto_X <- cbind(
  auto$cylinders, 
  auto$displacement, 
  auto$horsepower,
  auto$weight
)[!train,]

set.seed(1)
knn.pred2 = knn(train_auto_X, test_auto_X, auto.train$mpg01, k=1)
tab = table(knn.pred2, auto.test$mpg01)
error.rate = round((sum(tab[1,2], tab[2,1])/dim(auto.test)[1])*100, 2)
paste("The error rate is of", error.rate, "%")


knn.pred2 = knn(train_auto_X, test_auto_X, auto.train$mpg01, k=10)
tab = table(knn.pred2, auto.test$mpg01)
error.rate = round((sum(tab[1,2], tab[2,1])/dim(auto.test)[1])*100, 2)
paste("The error rate is of", error.rate, "%")

knn.pred2 = knn(train_auto_X, test_auto_X, auto.train$mpg01, k=20)
tab = table(knn.pred2, auto.test$mpg01)
error.rate = round((sum(tab[1,2], tab[2,1])/dim(auto.test)[1])*100, 2)
paste("The error rate is of", error.rate, "%")

knn.pred2 = knn(train_auto_X, test_auto_X, auto.train$mpg01, k=100)
tab = table(knn.pred2, auto.test$mpg01)
error.rate = round((sum(tab[1,2], tab[2,1])/dim(auto.test)[1])*100, 2)
paste("The error rate is of", error.rate, "%")

library(tree)
set.seed(15)
tr <- sample(1:nrow(OJ), 800)
oj.tr <- OJ[tr,]
oj.te <- OJ[-tr,]
tree.oj <- tree(Purchase ~., oj.tr)
summary(tree.oj)

tree.oj
plot(tree.oj,type="uniform")
text(tree.oj, pretty=0)

oj.pred <-  predict(tree.oj, oj.te, type="class")
table(oj.te$Purchase,oj.pred)
mean( oj.te$Purchase == oj.pred)
set.seed(50)
cv.oj <- cv.tree(tree.oj, FUN = prune.tree)
cv.oj

plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree size", ylab = "Cross Validation Error")
oj.pruned <-  prune.tree(tree.oj, best=6)
summary(tree.oj)
summary(oj.pruned)

set.seed(1)
train <- 1:1000
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train <- Caravan[train, ]
Caravan.test <- Caravan[-train, ]

set.seed(1)
library(gbm)
boost.caravan <- gbm(Purchase ~ ., data = Caravan.train, distribution = "gaussian", n.trees = 1000, shrinkage = 0.01)
summary(boost.caravan)

probs.test <- predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
pred.test <- ifelse(probs.test > 0.2, 1, 0)
table(Caravan.test$Purchase, pred.test)

logit.caravan <- glm(Purchase ~ ., data = Caravan.train, family = "binomial")
probs.test2 <- predict(logit.caravan, Caravan.test, type = "response")
pred.test2 <- ifelse(probs.test > 0.2, 1, 0)
table(Caravan.test$Purchase, pred.test2)
