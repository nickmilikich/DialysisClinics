############################
# Importation and formatting
############################

setwd("~/Google\ Drive\ (nmilikic@nd.edu)/Fall\ 2019/PSY\ 60122\ Machine\ Learning\ for\ Social\ &\ Behavioral\ Research/Final\ Project/")
dial = read.csv(file = "DialysisCareQualityData.csv", header = TRUE, skip = 1, sep = ",", na.strings = c("","-"))
dim(dial)
dial = na.omit(dial)
dim(dial)
str(dial)
dial$Network = as.factor(dial$Network)
dial$TotalStations = as.integer(dial$TotalStations)
dial$ZipCode = as.factor(dial$ZipCode)

dial.complete = dial
dial = dial[,c(3,4,5,6,7,8,9,10,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,48,49,50,51,52,53,55,56,57,58,59)]

###############################
# First iteration - cutoff of 1
###############################

dial$SRR = as.factor(dial$SRR > 1)

####################
# Variable Selection
####################

# Lasso, 1SE rule
library(glmnet)
dial.mat = data.matrix(dial)
dial.lasso.cv = cv.glmnet(dial.mat[,-26], dial.mat[,26], family = "binomial", alpha = 1)
plot(dial.lasso.cv)
round(coef(dial.lasso.cv, dial.lasso.cv$lambda.1se), 5)

# Elastic net, 1SE rule
dial.enet.cv = cv.glmnet(dial.mat[,-26], dial.mat[,26], family = "binomial", alpha = 0.5)
plot(dial.enet.cv)
round(coef(dial.enet.cv, dial.enet.cv$lambda.1se), 5)

# Using variables selected from lasso and elastic net
# The full data matrix is still stored in dial.complete
dial = dial[,c("SRR","PctgBlack","Network","AvgSerumPhosphorous","PctgFistula","PctgCatheterOnly90","TotalPatients","PctgMedicare","PctgNonMedicare","ProfitStatus","ChainOwner","PctgFluVaccine","AvgHemoglobin","PctgHemoglobin10","PctgESAPrescribed","AvgKtV","PctgKtV18","PctgKtVOther","PctgPoorEnglish","HospitalAffiliation","Urbanicity2")]

##########################
# Decision Tree Algorithms
##########################

# CART
library(partykit)
library(caret)
dial.rpart = train(SRR ~ ., dial, method = "rpart", tuneLength = 30)
plot(as.party(dial.rpart$finalModel))
sum(dial$SRR == predict(dial.rpart, dial[,-1])) / length(dial$SRR)

# Conditional Inference Tree
dial.ctree = train(SRR ~ ., dial, method = "ctree", tuneLength = 3)
plot(dial.ctree$finalModel)
sum(dial$SRR == predict(dial.ctree, dial[,-1])) / length(dial$SRR)

# CART Random Forest
dial.rf = train(SRR ~ ., dial, method = "rf", tuneLength = 1, trControl = trainControl(method = "cv", number = 10), importance = T)
dial.rf$results
sum(dial$SRR == predict(dial.rf, dial[,-1])) / length(dial$SRR)
varImp(dial.rf)$importance[order(varImp(dial.rf)$importance[,2], decreasing = T),]

# Boosting
library(grf)
library(dismo)
dial.num = dial
dial.num$SRR = as.numeric(dial.num$SRR) - 1
dial.gbm = gbm.step(dial.num, 2:21, 1, tree.complexity = 3, family = "bernoulli")
summary(dial.gbm)
inter = gbm.interactions(dial.gbm)
inter$interactions
inter$rank.list

#####################################
# Second iteration - cutoff of 1.2072
#####################################

dial.it2 = dial.complete[,c(3,4,5,6,7,8,9,10,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,48,49,50,51,52,53,55,56,57,58,59)]
dial.it2$SRR = as.factor(dial.it2$SRR > 1.2072)

####################
# Variable Selection
####################

# Lasso, 1SE rule
dial.it2.mat = data.matrix(dial.it2)
dial.it2.lasso.cv = cv.glmnet(dial.it2.mat[,-26], dial.it2.mat[,26], family = "binomial", alpha = 1)
plot(dial.it2.lasso.cv)
round(coef(dial.it2.lasso.cv, dial.it2.lasso.cv$lambda.1se), 5)

# Elastic net, 1SE rule
dial.it2.enet.cv = cv.glmnet(dial.it2.mat[,-26], dial.it2.mat[,26], family = "binomial", alpha = 0.5)
plot(dial.it2.enet.cv)
round(coef(dial.it2.enet.cv, dial.it2.enet.cv$lambda.1se), 5)

# Using variables selected from lasso and elastic net
# The full data matrix is still stored in dial.it2.complete
dial.it2 = dial.it2[,c("SRR","PctgBlack","Network","PctgFistula","PctgMedicare","PctgNonMedicare","TotalStaff","PctgFluVaccine","AvgHemoglobin","PctgESAPrescribed","AvgKtV","PctgKtV18","PctgKtVOther")]

##########################
# Decision Tree Algorithms
##########################

# CART
dial.it2.rpart = train(SRR ~ ., dial.it2, method = "rpart", tuneLength = 30)
plot(as.party(dial.it2.rpart$finalModel))
sum(dial.it2$SRR == predict(dial.it2.rpart, dial.it2[,-1])) / length(dial.it2$SRR)

# Conditional Inference Tree
dial.it2.ctree = train(SRR ~ ., dial.it2, method = "ctree", tuneLength = 3)
plot(dial.it2.ctree$finalModel)
sum(dial.it2$SRR == predict(dial.it2.ctree, dial.it2[,-1])) / length(dial.it2$SRR)

# CART Random Forest
dial.it2.rf = train(SRR ~ ., dial.it2, method = "rf", tuneLength = 1, trControl = trainControl(method = "cv", number = 10), importance = T)
dial.it2.rf$results
sum(dial.it2$SRR == predict(dial.it2.rf, dial.it2[,-1])) / length(dial.it2$SRR)
varImp(dial.it2.rf)$importance[order(varImp(dial.it2.rf)$importance[,2], decreasing = T),]

# Boosting
dial.it2.num = dial.it2
dial.it2.num$SRR = as.numeric(dial.it2.num$SRR) - 1
dial.it2.gbm = gbm.step(dial.it2.num, 2:12, 1, tree.complexity = 3, family = "bernoulli")
summary(dial.it2.gbm)
inter = gbm.interactions(dial.it2.gbm)
inter$interactions
inter$rank.list

######################
# Principal Components
######################

dial.pc = princomp(data.matrix(dial.complete[,c(3,4,5,6,7,8,9,10,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,48,49,50,51,52,53,55,56,57,58,59)]))
summary(dial.pc)
plot(dial.pc, type = "lines")
loadings(dial.pc)











