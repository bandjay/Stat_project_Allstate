#####  Script for Data exploration  ##############
## Explore numeric features , find out principal components and correlation analysis, 
     # check for feasible transformations w.r.to response.
## Explore categorical variables , check for chi-square independence, 
     # anova model for vars with few levels, vars with many levels may require special attention.
     # try merging several vars and see if it makes sense.
##  outlier and influence diagnostics .
## try different feature intercations such as x1+x2  ,x1/x2, x1*x2 w.r.to response.
## reserve 10% of data for validation, dont fit any model for this data.
## Try both bootstrap and CV for regularization.
##  Training dataset has 180 k rows approx...better use 'readr' package for loading data it is faster
## for exploration 'dplyr' package is faster with large data.
##  Apart from CARET we can use H20 package which has nice models that can do distributed modelling(faster)


## Data loading
library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(caret)
train <- read_csv("C:/Users/Jay/Desktop/542 project/train.csv/train.csv")
test <- read_csv("C:/Users/Jay/Desktop/542 project/test.csv/test.csv")
dim(train)
dim(test)


## Missing values plot
library(Amelia)
missmap(train, col=c("black", "grey"), legend=TRUE) # no missing data

## correlation plot between numeric vars
numeric_data<- train[,118:131]
dim(numeric_data)
library(corrplot)
M <- cor(numeric_data)
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F")) 
corrplot.mixed(M, lower="circle",upper="number",col=c("green","blue"))

## PCA for numeric data
pr.comp=prcomp(numeric_data,center = TRUE,scale. = TRUE)
pr.var=pr.comp$sdev^2
pve=pr.var/sum(pr.var)
pve
summary(pr.comp)
ggplot()+geom_line(aes(x=(1:length(pve)),y=pve),size=1,col="green")

# PCA seems useful just keep only top 3-4 components instead of 14 vars

# boxplot for loss /resposne
summary(train$loss)
boxplot(train$loss)

# there are some outleirs to be taken care
#ggplot()+geom_boxplot(aes(train$loss))

### Chi square test for independence
library(MASS)
tr_vars=names(train)[2:117]
attach(train)
for (i in 1:length(tr_vars))
  {
   for (j in 1:length(tr_vars))
    {
     if (i!=j)
     {
       x=t(train[,tr_vars[i]])
       y=t(train[,tr_vars[j]])
       tbl = table(x,y) 
       tbl 
       test_res=chisq.test(tbl)
       if (test_res$p.value>0.05){
         cat(tr_vars[i],tr_vars[j]," are independent")
         cat("\n")
       }
   }
   }
  } 




