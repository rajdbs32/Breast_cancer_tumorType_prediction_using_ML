class(Breast_cancer_data)
df=Breast_cancer_data
df
View(df)
install.packages("caret")
install.packages("caTools")
install.packages("e1071")
install.packages("rstatix")
install.packages("randomForest")
library(caTools)
library(caret)
library(e1071)
library(randomForest)
library(nnet)
anyNA(df)
boxplot(df)

#generation of Characteristic plot 
ggpairs(df[, -6], aes(color=as.factor(df$diagnosis), alpha=0.75), lower=list(continuous="smooth"))+ 
  theme_bw()+
  labs(title="Cell Nuclei Characteristics")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=14))

#conversion of outcome variable to factor
df$X6 = as.factor(df$X6) 

#reduntant 
sapply(df,class)

#splitting ds into test and training sets
set.seed(12345)
intrain<-createDataPartition(y=df$X6, p=0.8, list=FALSE) #splitting on the basis of outcome variable and splitratio=0.8
training<-df[intrain,] #trainset with outcomevariable
testing<-df[-intrain,] #testset without outcome variable

#10-fold cross validation
trctrl<-trainControl(method = "repeatedcv", number = 10, repeats=3)
#SVM Linear
svm_linear<-train(X6 ~., data=training, method="svmLinear", trControl=trctrl, preProcess=c("center", "scale"), tuneLength=10)
svm_linear
testing_predict<-predict(svm_linear, newdata = testing)
testing_predict<-predict(svm_linear, newdata = testing)
confusionMatrix(table(testing_predict,testing$X6))
cm_svm <-confusionMatrix(table(testing_predict,testing$X6))
cm_svm

#Random Forest
forest<- randomForest(training$X6~., data=training)
forest_predict<-predict(forest, newdata = testing)
confusionMatrix(table(forest_predict, testing$X6))
cm_forest <- confusionMatrix(table(forest_predict, testing$X6))
cm_forest

#Naive BAyes
nb=naiveBayes(training$X6~., data=training) 
y_predict<-predict(nb, newdata = testing)
confusionMatrix(table(y_predict,testing$X6))
cm_nb <-confusionMatrix(table(y_predict,testing$X6))
cm_nb

#neural network
neural <-nnet(X6~.,training, size=6, trace=FALSE, matrix=1000 )
ps<- predict(neural,testing, type="class")
cm<-table(ps, testing$X6)
cm
plot(cm)
