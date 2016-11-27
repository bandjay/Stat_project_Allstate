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
train <- read_csv("C:/Users/Jay/Desktop/542 project/train.csv")
test <- read_csv("C:/Users/Jay/Desktop/542 project/test.csv")
dim(train)
dim(test)
summary(train)
str(train[,1:131])

#train=train[1:6000,]

## Missing values plot
library(Amelia)
missmap(train, col=c("red", "blue"), legend=TRUE) # no missing data

## correlation plot between numeric vars
numeric_data<- train[,118:131]
dim(numeric_data)
library(corrplot)
M <- cor(numeric_data)
#col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
#                           "cyan", "#007FFF", "blue","#00007F")) 
corrplot.mixed(M, lower="circle",upper="number",col=c("green","blue"))

## PCA for numeric data
summary(numeric_data)
pr.comp=prcomp(numeric_data,center = TRUE,scale. = TRUE)
pr.var=pr.comp$sdev^2
pve=pr.var/sum(pr.var)
pve
summary(pr.comp)
pve_cum=pve
for (i in 1:length(pve)){
pve_cum[i+1]=pve_cum[i]+pve_cum[i+1]
}
pve_cum=na.omit(pve_cum)
number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot()+geom_line(aes(x=(1:length(pve)),y=pve),size=1,col="green")+
  geom_line(aes(x=(1:length(pve)),y=pve_cum),col="magenta",size=1.25,linetype=4)+
  xlab("Number of principal components")+ylab("Percentage variance explained")+
  ggtitle("PCA plot")+scale_x_continuous(breaks=number_ticks(15))
# PCA seems useful just keep only top 3-4 components instead of 14 vars

# boxplot for loss /resposne
summary(train$loss)
boxplot(train$loss)
# histogram for loss
ggplot(data=train, aes(train$loss)) + geom_histogram(col="#f04546")+
  xlab("Number of observations")+ylab("loss value")+ggtitle("Histogram for loss")
# making loss as normal
ggplot(data=train, aes(log(train$loss))) + geom_histogram(fill="#f04546")+
  xlab("Log Loss Value")+ylab("Number of observations")+ggtitle("Histogram for loss")
#+coord_flip()

train$loss=log(train$loss)

# there are some outleirs to be taken care
#ggplot()+geom_boxplot(aes(train$loss))


##### printing levels in the cat vars if two vars has same levels display msg
library(MASS)
tr_vars=names(train)[2:117]
attach(train)
for (i in 1:length(tr_vars))
{
  cat("cat variable",i,"has",length(table(train[,i+1])),"levels" )
  cat("\n")
} 

### Chi square test for independence
library(MASS)
#train=train[1:1000,]
tr_vars=names(train)[2:73]
attach(train)
varlist=rep(0,500)
for (i in 1:length(tr_vars))
{
  for (j in 1:length(tr_vars))
  {
    if (i!=j)
    {
      x=t(train[,tr_vars[1]])
      y=t(train[,tr_vars[2]])
      tbl = table(x,y) 
      tbl 
      test_res=chisq.test(tbl)
      if (test_res$p.value<0.05){
        varlist=tr_vars[j]
        cat(tr_vars[i],tr_vars[j]," are dependent")
        cat("\n")
      }
    }
  }
} 


# R tsne
library(Rtsne)
train1=read.csv("C:/Users/Jay/Desktop/542 project/train.csv")
dim(train1)
#train_sample=train1[1:1000,]
set.seed(426) # for reproducibility
tsne <- Rtsne(train1, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
# visualizing

tsne_data <- tsne$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(1:3))


plot(tsne$Y, t='n', main="BarnesHutSNE")
text(tsne$Y, labels=colnames(train1))


# K modes clustering
library(klaR)
kmodes(train_sample, 3, iter.max = 10, weighted = FALSE)


# clusters of var
library(ClustOfVar)
tree <- hclustvar(X.quanti = NULL, X.quali =train1[,2:116])
plot(tree)


# gower distance for clustering
library(cluster)
library(Rtsne)
#train_mat=train1[,2:116]
gower_dist <- daisy(train1[1:10000,2:116],
                    metric = "gower")
#summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

#train1[
#  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
#        arr.ind = TRUE)[1, ], ]

pam_fit <- pam(train1[1:10000,2:116], diss = TRUE, k = 3)

#pam_results <- college_clean %>%
#  dplyr::select(-name) %>%
#  mutate(cluster = pam_fit$clustering) %>%
#  group_by(cluster) %>%
#  do(the_summary = summary(.))
#pam_results$the_summary



tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))+ggtitle("Clustering based on Categorical Vars")
                    