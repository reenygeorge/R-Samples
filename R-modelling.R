#MODELLING CODE

#Model fitting- logistic regression
logisticFit <- function(interactions,train){
  
  test <- (-train)
  trainData <- interactions[train,]
  testData <- interactions[test,]
  
  modelFit <- glm(residing ~ ., family=binomial(link='logit'), data=trainData, x=TRUE)
  save(modelFit,file="LogisticModel.RData")
  print(summary(modelFit))
  print(vif(modelFit))
  coefficients <- coef(modelFit)[-1]
  write.csv(coefficients, logisticCoefFile)
  
  #Test the model's performance
  testProb <- predict.glm(modelFit,type="response",newdata=testData)
  testPred <- rep(0,length(testProb))
  testPred[testProb > 0.7] <- 1
  confusionMatrix <- table(pred=testPred,true=testData$residing)
  print(confusionMatrix)
  accuracy <- mean(testPred == testData$residing)
  print(accuracy)
  
  #evaluate crossvalidation error
  cv.error.10 <- cv.glm(trainData, modelFit,K=10)
  print(cv.error.10$delta)
  
  designMat <- model.matrix(residing ~ .,data=interactions)[,-1]
  weightVector <- generateOddratioWeights(coefficients,designMat)
  #weightVector <- generateOddratioWeights(coefficients,modelFit$x[,-1])
  return(weightVector)
}

#lasso regression
lassoRegressionFit <- function(interactions,train) {
  
  test <- (-train)
  trainData <- interactions[train,]
  testData <- interactions[test,]
  
  #Create the design matrix
  x_train <- model.matrix(residing ~ .,data=trainData)[,-1]
  y_train <- trainData$residing
  #create a training and test set
  
  lassoMod <- glmnet(x_train,y_train,alpha=1,family="binomial")
  plot(lassoMod)
  #print(lassoMod)
  
  set.seed(1)
  cv.out <- cv.glmnet(x_train,y_train,alpha=1)
  plot(cv.out)
  bestlam <- cv.out$lambda.1se
  print("bestlam")
  print(bestlam)
  lasso.coef <- coef(cv.out,s=bestlam)
  print(lasso.coef[lasso.coef!=0])
  lasso.coef <- as.vector(as.matrix(lasso.coef))
  save(lasso.coef,file="LassoCoef.RData")
  write.csv(lasso.coef, lassoCoefFile)
  
  #Test the model performance
  x_test <- model.matrix(residing ~ .,data=testData)[,-1]
  y_test <- testData$residing
  testProb <- predict(cv.out,newx = x_test,s=bestlam,type="response")
  testPred <- rep(0,length(testProb))
  testPred[testProb > 0.7] <- 1
  confusionMatrix <- table(pred=testPred,true=testData$residing)
  print(confusionMatrix)
  accuracy <- mean(testPred == testData$residing)
  print(accuracy)
  
  designMat <- model.matrix(residing ~ .,data=interactions)[,-1]
  weightVector <- generateOddratioWeights(lasso.coef[-1],designMat)
  return(weightVector)
  
}