set.seed(542)
library(readr)
train <- readr::read_csv("c:/users/whuang67/downloads/train.csv")
test <- readr::read_csv("c:/users/whuang67/downloads/test.csv")

## Test###############################################
train1 <- train[1:10000,]
str(train[, 100:132])

levels.col <- base::array(NA, dim = 117)
levels.col.logis <- base::array(NA, dim = 117)
for (i in 1:117){
  levels.col[i] <- base::length(base::table(train[, i]))
  if (levels.col[i] <= 20){
    levels.col.logis[i] <- TRUE
  } else {
    levels.col.logis[i] <- FALSE
  }
}


length(base::c(base::as.logical(levels.col.logis), base::as.logical(rep(TRUE, 15))))
train1 <- train[, base::c(base::as.logical(levels.col.logis),
                          base::as.logical(rep(TRUE, 15)))]

library(Matrix)
train.Predictors <- base::data.frame(
  base::as.matrix(
    Matrix::sparse.model.matrix(loss ~ .-1, data=train1)[, -1]
  )
)
###########################################333333
library(Matrix)
train.Predictors <- base::data.frame(
  base::as.matrix(
    Matrix::sparse.model.matrix(loss ~ .-1, data=train)[, c(-1,-2)]
  )
)
train.Response <- base::data.frame(train[, 132])
names(train.Response) <- "loss"

library(MASS)
library(glmnet)

FirstAttempt <- glmnet::cv.glmnet(
  x = base::as.matrix(train.Predictors),
  y = base::as.matrix(train.Response),
  alpha = 1,
  family = "gaussian",
  nfolds = 10
)
FirstAttempt$lambda.min
FirstAttempt$lambda.1se
plot(FirstAttempt)

FirstAttempt1 <- glmnet::glmnet(
  x = base::as.matrix(train.Predictors),
  y = base::as.matrix(train.Response),
  alpha = 1,
  family = "gaussian",
  lambda = FirstAttempt$lambda.1se,
  standardize = FALSE
)
FirstAttempt2 <- train(
  x = base::as.matrix(train.Predictors),
  y = base::as.matrix(train.Response),
  method = "glmnet",
  trControl = train::trainControl(method = "cv", number = 5)
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 450, by = 50))
)
library(caret)
?trainControl
FirstAttempt1$beta


train.Predictors2 <- train.Predictors[, as.logical(FirstAttempt1$beta)]

par(mfrow=c(1,1))
LinearAttempt <- stats::lm(log(loss) ~.,
                           data = base::data.frame(train.Predictors2,
                                                   train.Response))
summary(LinearAttempt)
LinearPred <- stats::predict(LinearAttempt,
                             data = base::data.frame(train.Predictors2,
                                                     train.Response))
sum(abs(exp(LinearPred)-train.Response))/nrow(train.Response)

plot(FirstAttempt)

WholeData.train <- data.frame(train.Predictors, train.Response)
library(caret)
trainControl.lasso <- trainControl(method = "cv", number = 5)
grid.lasso <- expand.grid(alpha = 1, lambda = seq(0, 450, by = 50))

glmnetFit1 <- train(
  loss ~ .,
  data = WholeData.train,
  trControl = trainControl.lasso,
  method = "glmnet",
  tuneGrid = grid.lasso,
  verbose = FALSE
)


glmnet::plot.glmnet(SecondAttempt)

Coefficients <- data.frame(as.matrix(FirstAttempt$beta))


SecondAttempt <- step(
  lm(loss ~., data = WholeData.train),
  direction = "both",
  trace = 0
)

