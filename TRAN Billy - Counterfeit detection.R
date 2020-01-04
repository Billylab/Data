# Counterfeit detection

###################################################


### Part 1 - Introduction ----

### 1.1 Installing and loading packages ====
if (!require(RCurl)) install.packages('RCurl')
if (!require(forecast)) install.packages('forecast')
if (!require(reshape)) install.packages('reshape')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(FactoMineR)) install.packages('FactoMineR')
if (!require(factoextra)) install.packages('factoextra')
if (!require(dendextend)) install.packages('dendextend')
if (!require(plyr)) install.packages('plyr')
if (!require(dplyr)) install.packages('dplyr')
if (!require(corrplot)) install.packages('corrplot')
if (!require(rpart)) install.packages('rpart')
if (!require(rpart.plot)) install.packages('rpart.plot')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(rpart.plot)) install.packages('rpart.plot')
if (!require(caret)) install.packages('data.table')
if (!require(caret)) install.packages('caret')
if (!require(leaps)) install.packages('leaps')
if (!require(MASS)) install.packages('MASS')
if (!require(grid)) install.packages('grid')
library(RCurl)
library(forecast)
library(reshape)
library(ggplot2)
library(ggpubr)
library(FactoMineR)
library(factoextra)
library(dendextend)
library(plyr)
library(dplyr)
library(corrplot)
library(rpart.plot)
library(dplyr)
library(tidyverse)
library(data.table)
library(caret)
library(leaps)
library(MASS)
library(grid)


### 1.2 Loading data ====

#### Getting data from my Github
URL_bills <- getURL("https://raw.githubusercontent.com/Billylab/Data/master/billets.csv")
URL_bills_to_predict <- getURL("https://raw.githubusercontent.com/Billylab/Data/master/example.csv")
bills <- read.csv(text = URL_bills, header=TRUE, sep=",",na.strings=c(""))
bills_to_predict <- read.csv(text = URL_bills_to_predict, header=TRUE, sep=",",na.strings=c("")) 
head(bills)


### 1.3 Data description ====
fake_bills <- subset(bills, is_genuine == "False")
real_bills <- subset(bills, is_genuine == "True")
cat(dim(fake_bills)[1],"are fake bills.\n") 
cat(dim(real_bills)[1],"are real bills.")

# There is 170 bills in total. Each bill is described by a state (real : TRUE & fake : FALSE) and 6 geometric characteristics lengths : diagonal, height_left, height_right, margin_low, margin_up, length
# My aim is to create a counterfeit detection algorithm according to this dataset.
  


###################################################

### Part 2 - Analysis ----


### 2.1 Data cleaning ====
# We work with real data: we must take into account the fact that some data may be missing, outliers or atypical. We must clean our dataset before our analyzes.
### 2.1.1 Data Structure ####
  
str(bills)

### 2.1.2 Missing data ####

# Here are the number of missing data per variable.
sapply(bills, function(x) sum(is.na(x)))

### 2.1.3 Outliers and unusual data ####
  
# Here is a summary of the data :
summary(bills)

# multiplot : Plot multiple graphics
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) { 
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols 
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot 
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
    } }
}

b1 <- ggplot(bills, aes(x="", y=diagonal)) + geom_boxplot(fill="gray")+ labs(x="diagonal", y = "Valeur")+ theme_classic()
b2 <- ggplot(bills, aes(x="", y=height_left)) + geom_boxplot(fill="gray")+ labs(x="height_left", y = "Valeur")+ theme_classic()
b3 <- ggplot(bills, aes(x="", y=height_right)) + geom_boxplot(fill="gray")+ labs(x="height_right", y = "Valeur")+ theme_classic()
b4 <- ggplot(bills, aes(x="", y=margin_up)) + geom_boxplot(fill="gray")+labs(x="margin_up", y = "Valeur")+ theme_classic()
b5 <- ggplot(bills, aes(x="", y=margin_low)) + geom_boxplot(fill="gray")+labs(x="margin_low", y = "Valeur")+ theme_classic()
b6 <- ggplot(bills, aes(x="", y=length)) + geom_boxplot(fill="gray")+labs(x="length", y = "Valeur")+ theme_classic()

multiplot(b1,b2,b3,b4,b5,b6, cols=3)

### 2.1.4 Categorical variable ####
levels(bills$is_genuine)



### 2.2 Data visualization ====
# Calculation of average variables and by category true or false ticket
mu_diagonal <- ddply(bills, "is_genuine", summarise, grp.mean=mean(diagonal))
mu_height_left <- ddply(bills, "is_genuine", summarise, grp.mean=mean(height_left))
mu_height_right <- ddply(bills, "is_genuine", summarise, grp.mean=mean(height_right))
mu_margin_up <- ddply(bills, "is_genuine", summarise, grp.mean=mean(margin_up))
mu_margin_low <- ddply(bills, "is_genuine", summarise, grp.mean=mean(margin_low))
mu_length <- ddply(bills, "is_genuine", summarise, grp.mean=mean(length))

p1 <- ggplot(bills, aes(x=diagonal, fill=is_genuine, color=is_genuine)) +
  geom_histogram(position="identity", alpha=0.3, bins=30) + geom_vline(data=mu_diagonal, aes(xintercept=grp.mean, color=is_genuine),linetype="dashed") + geom_density(alpha=0.4)
p2 <- ggplot(bills, aes(x=height_left, fill=is_genuine, color=is_genuine)) +
  geom_histogram(position="identity", alpha=0.3, bins=30) + geom_vline(data=mu_height_left, aes(xintercept=grp.mean, color= is_genuine),linetype="dashed") + geom_density(alpha=0.4)
p3 <- ggplot(bills, aes(x=height_right, fill=is_genuine, color=is_genuine)) +
  geom_histogram(position="identity", alpha=0.3, bins=30) + geom_vline(data=mu_height_right, aes(xintercept=grp.mean, color=is_genuine),linetype="dashed") + geom_density(alpha=0.4)
p4 <- ggplot(bills, aes(x=margin_up, fill=is_genuine, color=is_genuine)) +
  geom_histogram(position="identity", alpha=0.3, bins=30) + geom_vline(data=mu_margin_up, aes(xintercept=grp.mean, color=is_genuine),linetype="dashed") + geom_density(alpha=0.4)
p5 <- ggplot(bills, aes(x=margin_low, fill=is_genuine, color=is_genuine)) +
  geom_histogram(position ="identity", alpha=0.3, bins=30) + geom_vline(data=mu_margin_low, aes(xintercept=grp.mean, color=is_genuine),linetype="dashed") + geom_density(alpha=0.4)
p6 <- ggplot(bills, aes(x=length, fill=is_genuine, color=is_genuine)) +
  geom_histogram(position="identity", alpha=0.3, bins=30) + geom_vline(data=mu_length, aes(xintercept=grp.mean, color=is_genuine ),linetype="dashed") + geom_density(alpha=0.4)

# Plotting : Figure
multiplot(p1,p2,p3,p4,p5,p6, cols=2)

### 2.3 Data exploration ====
### 2.3.1 Correlation Matrix ####

#To observe the possible correlations between varibales, the correlation matrix between the variables is represented.
mcor <- cor(bills[,c(2:7)])
corrplot(mcor, type="upper", tl.col="black", tl.srt=30,title="Figure 3 - Correlation matrix", mar=c(0,0,3,0))
# In the first part, we describe our data, our bills and the correlations which could exist between the geometrical characteristics of the ticket. We have noticed from the histograms that what distinguishes the most the counterfeits from the real bills are the lower margin, the upper margin, and the height.

### 2.3.2 Principal Component Analysis ####

res.pca <- PCA(bills, quali.sup = 1, scale.unit = TRUE, graph=FALSE)

# Scree Plot Eigenvalues : Percentage of variances explained by each principal axis
eig.val <- get_eigenvalue(res.pca)
figure6 <- fviz_screeplot(res.pca,addlabels=TRUE,ylim=c(0,50),linecolor='red') + 
  labs(title = "Figure 4 - Scree Plot Eigenvalues", x = "Principal component", y = "% of explained variance") + 
  theme(plot.title = element_text(size=20, face="bold",hjust = 0.5),
        axis.title.x = element_text(color="black", size=14, face="bold",hjust = 0.5),
        axis.title.y = element_text(color="black", size=14, face="bold",vjust = 0.5))
plot(figure6)

#Quality of representation of the variables on the factorial axes
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE,addCoef.col = "black",
         title="Figure 5 - Quality of representation of the variables", 
         mar=c(0,0,1,0))

# Contributions of variables to PC1
contrib1 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 6)
contrib2 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 6)
multiplot(contrib1,contrib2, cols=1)

# Correlation Circle
figure7 <- fviz_pca_var(res.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07" ),repel = TRUE) + 
  labs(title = "Figure 7 - Correlation Circle") +
  theme(plot.title = element_text(color="#3876C2", size=20, face="bold",hjust = 0.5),
        axis.title.x = element_text(color="black", size=14, face="bold",hjust = 0.5),
        axis.title.y = element_text(color="black", size=14, face="bold",vjust = 0.5))
plot(figure7)

# Representation of individuals
figure10a <- fviz_pca_biplot(res.pca, 
                             select.ind = list(cos2 = 0.8, contrib=50),
                             axes = c(1,2),addEllipses =TRUE, 
                             col.ind = bills$is_genuine ,
                             color_palette=c("#00AFBB","#FC4E07")) + 
  labs(title = "Figure 8 - Individuals and additional variables: Axis 1 and 2") + 
  theme(plot.title = element_text(color="#3876C2", size=14, face="bold",hjust = 0.5),
        axis.title.x = element_text(color="black", size=14 , face="bold",hjust = 0.5),
        axis.title.y = element_text(color="black", size=14, face="bold",vjust = 0.5))
plot(figure10a)


###################################################

### Part 3 - Modeling results and model performance ----

### 3.1 Preparing cross-validation ====


# Cross-validation relies on random data separation.
# To use random processes but to make them reproducible
# in time or on another machine / system, use the set.seed () function
set.seed(123)
# Implementation of a 5-part cross-validation
folds <- 5
# To stratify the parts on the variable is_genuine
# list: TRUE - The result is in list form
# returnTrain: TRUE - The returned values are the indexes 
#corresponding to the lines used for the training game
cvIndex <- createFolds(factor(bills$is_genuine), folds, list=T, returnTrain = T)
# index: At each iteration, each list is a vector of integers 
#corresponding to the lines used for the training.
# method: cv: Cross Validation
# number: Number of parts of the sample
train.control <- trainControl(index = cvIndex, method = "cv", number = folds)
# trainX : Contains the geometric variables that will be used to estimate is_genuine 
#in the training dataset
trainX <- bills[, -1]
trainY <- bills$is_genuine

### 3.2 Modeling : Decision tree ====
dtree_fit <- train(trainX, trainY, method = "rpart", trControl=train.control, tuneLength = 5, parms=list(split='information'))
print(dtree_fit)

# Let be : alpha = accuracy et RMSE = root mean squared error. 
# In a case of a binary problem, these scores can be calculated according to true positives (TP), true negatives (TN), 
# false positives (FP) and false negatives (FN). The total size of the dataset is : Omega=TP+TN+FP+FN. Otherwise :
# alpha=(TP+TN)/Omega
# RMSE=sqrt((FP+FN)/Omega 
# So : alpha+RMSE^2=1

RSME_mean_decision_tree = ((1-0.947^2)+(1-0.941^2)*3+(1-0.712^2))/5
cat('With the decision tree, the average RSME for cross-validation is',
round(RSME_mean_decision_tree,2),'.')


rpart.plot(dtree_fit$finalModel, type=3, fallen.leaves = TRUE, cex=1.3) # Plot : Figure

### 3.3 Modeling : Logistic regression ====
bills <- transform(bills, is_genuine = as.character(is_genuine)) # 1 : real bills.
bills$is_genuine[bills$is_genuine=='True']<-1 # 0 : fale bills.
bills$is_genuine[bills$is_genuine=='False']<-0
bills <- transform(bills, is_genuine = as.numeric(is_genuine))
head(bills)

# trainX : Contains the geometric variables that will be used to estimate is_genuine 
# in the train dataset
trainX <- bills[, -1]
trainY <- bills$is_genuine
# The geometric variables present in billetsY will be used to estimate is_genuine.
m_lr <- train(trainX, trainY, method="glmStepAIC", direction = 'both', trControl=train.control)
print(m_lr)
print(m_lr$resample)
# With logistic regression, the mean RSME for cross-validation is 0.18.
# Since the mean RSME for cross-validation of logistic regression is lower than that of the decision tree, 
# the logistic regression model is considered to be more efficient. It will be used on the entire dataset for the prediction.

### 3.4 Prediction ====

# Here are the bills that I am going to classisfy.
bills_to_predict

# For the prediction, now that the optimal model has been determined, I carry out a logistic regression on all the data, 
#with the logit parameter so that the probabilities are between 0 and 1. Our program will be able to make a prediction on 
# a bill, that is to say to determine if it is a real or a fake bill. For each bill, the classification algorithm will give 
# the probability that the bill is true. If this probability is greater than or equal to 0.5, the bill will be considered true. 
# Otherwise, it will be considered as a fake.
model <- glm(is_genuine ~ margin_low + margin_up + length, data = bills, family=binomial(link="logit"))
fitted.results <- predict(model,newdata=bills_to_predict,type='response')
print(fitted.results)
print("The results above are the probability for each bill that it is true.")

fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results[fitted.results==1] <- 'True'
fitted.results[fitted.results==0] <- 'False'
# is_genuine is added to the dataframe bills_to_predict
bills_to_predict$is_genuine <- fitted.results
bills_to_predict <- transform(bills_to_predict, is_genuine = as.factor(is_genuine))
bills_to_predict

# Display on the correlation circle
# An id column (format: factor) is created.
bills$id <- rownames(bills)
bills <- transform(bills, id = as.factor(id))
# We collect the real bills and bills to predict in the same table.
data <- merge(bills, bills_to_predict, all = T, sort = FALSE)
res.pca <- PCA(data[,c(1:7)], quali.sup = 1, ind.sup = 171:175, scale.unit = TRUE, graph=FALSE)

# The bills to be predicted are displayed on the factorial plane
p <- fviz_pca_ind(res.pca, geom.ind = "point",pointsize = 1,habillage=1,addEllipses = TRUE)
p <- fviz_add(p, res.pca$ind.sup$coord) + labs(title = "Figure 13 - Predicted bills") +
  theme(plot.title = element_text(color="#3876C2", size=20, face="bold",hjust = 0.5),
        axis.title.x = element_text (color="black", size=14, face="bold",hjust = 0.5),
        axis.title.y = element_text(color="black", size=14, face="bold",vjust = 0.5))
p



###################################################

### Part 4 - Conclusion ----

# From a set of bills (characterized by geometrical lengths and a status - real or fake), my aim was to make an algorithm in order to predict if a bill is real or not.

# First, I had to clean all the dataset (missing data, outliers, unusual data...) so that I can see which one of the geometrical characteristics have the most influence on the bill status. It would seem, then, that what distinguishes the true from the counterfeit bills are :
  # The top margin of the bills
  # The lower margin of the bills
  # The length of the bills
# Then by using a principal component analysis, I was able to confirm my initial hypothesis. The variables that contribute the most to the categorization of the bills are the length of the bill, its lower margin and its top margin.
# Finally, I compared 2 models in order to predict a bill status : decision tree and logistic regression. Both models was using at least length and margin low (and margin up for logistic regression). With cross-validation, I showed that the logistic regression performs better (lower RMSE). So I used logistic regression in order to predict the status of new bills.

# Once the automatic learning algorithm has been trained on a first set of data, one should evaluate it on a second set of data in order to verify that the model does not over-learn.
# Finally, the model need to be deployed in production to make predictions, and use the new input data to re-train and improve its model.




