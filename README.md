---
title: 'STAT 432 Project Proposal??Analysis of the Spam Email Database '
output:
  word_document:
    toc: yes
  html_document:
    theme: readable
    toc: yes
  pdf_document:
    toc: yes
---
**Group Name**??Chaoyue the Lucky Fish

**Group Member Information**

- Bingkun Luo: bluo5

- Xinran Wang: xwang258

- Junchu Zhang: jzhng156

# Introduction and Literature Review

## [Data source information](http://archive.ics.uci.edu/ml/datasets/Spambase)

## A Comprehensive Introduction of Data

This dataset is about the categorization of spam emails, which is a problem faced by all of us. The ??spam?? concept includes advertisements, make money fast schemes, chain letters, pornography, etc.. Spam email is defined by three characteristics, anonymity(unknown sender), mass mailing(large number of recipients) and unsolicited(not requested by recipients) . Not only will dealing with spam emails become a waste of time, but some service providers also spend huge amount of money on spam. It??s necessary for data scientists to develop an efficient classification of spam emails. In this way, we found this data set from the UCI Machine Learning Repository about the classification of spam email. The collection of the spam emails in this data set was from the author??s postmaster and individuals who had filed spam, while the collection of non-spam email was from field work and personal emails. There are some specific words or symbols that are typical in spam email and some typical in non-spam email, while the others does not have a significant direction to spam or non-spam. We will use the machine learning methods we learned this semester to analyze this dataset, find out the words and symbols that influence the classification of spam email the most, and compare the different classification methods. 

First, we input the data and change the variable names of each columns for future analysis. 

```{r echo=FALSE}
my_data = spam = read.table("spambase.data",sep = ",")
colnames(my_data) <- (c("word_freq_make","word_freq_address","word_freq_all","word_freq_3d", "word_freq_our","word_freq_over","word_freq_remove","word_freq_internet ","word_freq_order", "word_freq_mail", "word_freq_receive","word_freq_will", "word_freq_people", "word_freq_report", "word_freq_addresses","word_freq_free", "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit","word_freq_your","word_freq_font","word_freq_000","word_freq_money","word_freq_hp","word_freq_hpl","word_freq_george","word_freq_650","word_freq_lab","word_freq_labs","word_freq_telnet","word_freq_857","word_freq_data","word_freq_415","word_freq_85","word_freq_technology","word_freq_1999","word_freq_parts","word_freq_pm","word_freq_direct","word_freq_cs","word_freq_meeting","word_freq_original","word_freq_project","word_freq_re","word_freq_edu","word_freq_table","word_freq_conference","char_freq_;","char_freq_( ","char_freq_[","char_freq_!","char_freq_$","char_freq_#","capital_run_length_average","capital_run_length_longest","capital_run_length_total","spam"))
head(my_data)
```

We didnt't see any missing values in this dataset. So we continue with the dataset and explain the dependent and the independent variables.

There is one nominal {0,1} class attribute of type spam variable, which also known as outcome in the UCI dataset. The value of ??0?? means the email is categorized as non-spam email, while ??1?? denotes the email is considered as spam email.

The other 57 variables in the dataset are classified as independent variables, which are inputs. All independent variables are continuous variables, and are divided into several segments. The first 48 continuous real [0,100] attributes of type word_freq_WORD = percentage of words in the e-mail that match WORD. The calculation is 100 * (number of times the WORD appears in the email) / total number of words in email. A ??word?? in this case is any string of alphanumeric characters bounded by non-alphanumeric characters or end-of-string. The following 6 continuous real [0,100] attributes of type char_freq_CHAR = percentage of characters in the email that match CHAR. The calculation is 100 * (number of CHAR occurences) / total characters in e-mail. There is 1 continuous real [1,??] attribute of type capital_run_length_average = average length of uninterrupted sequences of capital letters, 1 continuous integer [1,??] attribute of type capital_run_length_longest = length of longest uninterrupted sequence of capital letters, 1 continuous integer [1,??] attribute of type capital_run_length_total = sum of length of uninterrupted sequences of capital letters = total number of capital letters in the email.


## Literature Review

We found relevant analyzes of the data set from [the following resources](https://www.researchgate.net/publication/269651895_Spam_Mail_Detection_through_Data_Mining_-_A_Comparative_Performance_Analysis):

The research group used the same dataset as we do. They wanted to find the best filter of the spam/non-spam email with the highest accuracy rate.They first classified the full dataset using different methods and found that Random Forest provides the best accuracy rate. Then they used the Best-First Feature selection algorithm to select a subset of the features of the original dataset and did the classification again. They found that tree like classifiers(Random trees and Random Forest) works well in spam email detection and accuracy rate improved incredibly when they first applied feature selection algorithm into the entire process.  


# Summary Statistics and Data Visualization

We separate our dataset into the training set, ??train??, and the testing set, ??test??. We choose to split the size of the training set and the testing set by 3:1.

```{r}
library(caTools)
set.seed(5)
sample = sample.split(my_data,SplitRatio = 0.75)
train =subset(my_data,sample ==TRUE) 
test =subset(my_data, sample==FALSE)
```

We can take a look at the overall summary statistics of the dataset. Because the dataset has 57 independent variables and 1 dependent variable. For now, we would like to show the summary statistic of the independent variables. Due to the space limit, we will only print out the result for the first input to have a general idea of how the dataset looks like. 

```{r echo=FALSE}
summary.list = function(x)list(
 Mean=mean(x),
 Median=median(x),
 Max.Min=range(x),
 Variance=var(x),
 Std.Dev=sd(x),
 Coeff.Variation.Prcnt=sd(x)/mean(x)*100,
 Std.Error=sd(x)/sqrt(length(x[!is.na(x)])),
 Quantile=quantile(x)
)

summary.list(my_data[,1])
```

After we look into the x variables, our group would also like to examine the dependent variable, spam. This is a binomial factor, in which the value ??1?? represents spam email, and the value of ??0?? represents non-spam email.

```{r echo=FALSE}
summary.list(my_data[, 58])
```

Since the spam email is classified as a binary value. We could directly view that in the original dataset, the percentage of the spam email is 0.394, which is the mean value. The percentage of non-spam email will be 0.606, which can be calculated by 1- 0.394.

After we have a general view of both the independent and dependent variables. We would like to have some visualization of the dataset. First of all, we will perform the Principal Component Analysis (PCA). The plot is shown below:


```{r echo=FALSE}
spam_pc <- prcomp(my_data[, 1:57], center = TRUE,scale. = TRUE)
plot(spam_pc, type = "l")

```

By using the elbow method, we find that the first two principle components explain most of the variances. So, we will use the first two PC directions to plot the variables and have a clear visualization of the dataset.

```{r echo=FALSE}
 library(ggplot2)
    ggplot(data = data.frame(spam_pc$x), aes(x=PC1, y=PC2)) + 
        geom_point(color=c("orange", "blue")[as.factor(my_data$spam)], size = 2)
```

Although the full linear regression model gives us an R square larger than 0.5, our group chooses not to use it because the dependent variable, which is spam variable is binomial. Therefore, the assumptions of linear regression are violated, and the predicted value is meaningless. Due to the special case of the spam variable, our group decides to use the logistic regression to run the full model, instead of the linear regression.

```{r echo=FALSE}
binomial = glm(spam~., family=binomial(link = logit), data = train)
summary(binomial)
```

From the summary of binomial regression, we can see that at least some variables are significant due to their p-values are less than 0.05. 

Moreover, we can use the testing set to check how the model is performing and use the confusion matrix to compare the result to the original spam/non-spam classification. In this case, we set 0.5 as the cutoff point. If the predicted value is larger than 0.5, we categorize it into the ??spam?? category. If the predicted value is smaller than 0.5, we categorize it into the ??non-spam?? category.


```{r echo=FALSE}
binomial_predict = predict(binomial, newdata = test[, 1:57], type = "response")
cm_binomial = table(predicted = binomial_predict > 0.5, actual = test$spam)
cm_binomial
```

After utilizing the testing set, our group also calculate the misclassification error of binomial. 

```{r echo=FALSE}
(47+51)/(670+47+51+421)
```

The misclassification error is 0.0824, which implies that in the logistic regression, 91.76% of the testing sample is correctly classified.

```{r echo=FALSE}
par(mfrow=c(2,2))
plot(binomial)
```

We also check the plot of the binomial regression and see that there are no apparent outliers or influential points. 

# Proposed Analysis

Our group performs the statistics summary for the independent and dependent variable first to have a general view of the dataset. Then our group chooses several methods to plot the dataset and have a data visualization, which have been explained in the previous section.

Our group decides to utilize the testing set to check how the model is performing and use the confusion matrix to compare the result to the original spam/non-spam classification. We first use the binomial regression to fit the independent and dependent variable in the spam dataset due to the reason that the dependent variable is a binary value. 

Next, our group will try some other classification methods with the full model and calculate the misclassification error. Since the spam/non-spam column as our ??output?? variable, all the methods we use should be supervised learning methods. After the classification, our group would like to compare the misclassification error and the accuracy rate and use it to compare the effectiveness of different models.

1.	Naive Bayes

Naive bayes is a binary classifier for the dependent variable, and the words appeared in the email, which are the inputs, is considered as independent.  We can use Naive Bayes and calculate the misclassification error and the accuracy rate for this method.


```{r echo=FALSE}
train$spam = as.factor(train$spam)
test$spam = as.factor(test$spam)
```


```{r echo=FALSE}
library(e1071)
library(caret)
naive = naiveBayes(spam~., data = train)
nb_predict = predict(naive, test[,!names(test) %in% "Spam"])
confusionMatrix(nb_predict,test$spam)
```

From the summary, we could see that the accuracy rate is 0.7023, which implies that the misclassification error is 0.2977, which can be calculates as 1-0.7023. The results show that 70.23% of the testing sample is correctly classified in the Naive Bayes method. Compare these statistics with the one in the logistic regression, we would see that the accuracy rate is much lower for the Naive Bayes method, and we would like to conclude that the Naive Bayes method is not as effectiveness as the logistic regression. 

2.	SVM

The third method our group would like to try is the SVM method. The aim of SVM aim is to find a best linear function for classification that maximizes the margin. Our group will calculate the misclassification error and the accuracy rate to see whether the SVM method is effective.


```{r echo=FALSE}
svm = svm(spam~., data = train)
svm_predict = predict(svm, test[,-58])
confusionMatrix(svm_predict,test$spam)
```

From the above table, the accuracy rate for the SVM model is 0.9344 and the misclassification error is 0.0656, which is 1-0.9344. This data means that 93.44% of the testing sample is correctly classified in the SVM method. By comparing the results with the Naive Bayes and the logistic regression, we find that the SVM method performs better in classifying the full dataset.

3.	Random Forest

The last method we are going to talk about is the Random Forest. The object of the Random Forest is to average multiple deep decision trees, trained on various parts of the same training set. We will increase some bias and loss some interpretability, but we expect to boost our performance for the final model. Our group will also calculate the misclassification error and the accuracy rate to measure the effectiveness.


```{r echo=FALSE}
library(randomForest)
set.seed(10)
rf=randomForest(train[,-58], as.factor(train[,58]), mtry = 7, nodesize = 1,importance=TRUE)
n=nrow(test)
```

```{r echo=FALSE}
forest_predict = predict(rf, test[,-58])
confusionMatrix(forest_predict,test$spam)
```

By looking at the result from the above table, the accuracy rate for the Random Forest method is 0.9554 and the misclassification rate is 0.0446, which is calculates by 1-0.9554. The outcome indicates that 95.54% of the testing sample is correctly classified in Random Forest method. The Random Forest method generates the best outcome by comparing the data from the previous methods in the full dataset. 

```{r echo=FALSE}
varImpPlot(rf, main='Variable Importance Plot: Full Model')
```

We draw the variable importance plot for the Random Forest of the full model. The importance of the variables in classifying are listed from top to buttom in the variance importance plot. We find that "char_freq_!" is the most important variable in classifying based on both the MeanDecreaseAccuracy and the MeanDecreaseGini. 


After analyzed the full dataset with different methods. Our group plan to eliminate some variables based on model selection methods because we find that some of the variables are not significant in the logistic regression. So, our group would like to try some reduced models and test the confusion matrix and accuracy rate of the previous models to see whether we could get a better result. First, we would like to use the ??stepwised selection?? based on minimum AIC values to generate our reduced model and find out the significant variables.

```{r include=FALSE}
reduced = step(glm(spam~., family=binomial(link = logit), data = train), direction = "both")
```
```{r}
summary(reduced)$coefficient
```

After running the stepwise function, we find out that there are 44 variable left in this reduced model based on minimum AIC value. For those significant inputs, our group decide to analyze some of the independent variables and how they contribute to the construction of spam filter. 

The collection of the spam-emails is from the postmaster or the individuals who have classified the email as spam. From the significant independent variables, our group has selected several words for spam filter and provide the reason for each of the choices.

- Our

According to the generalized linear model, ??Our?? is considered as a high potential attribute for spam mail, as its p value is 2.77e-05 (near 0). It is a popular word that is detached among the 4601 correspondence, and we classify it as spam words because lots of the commercial like to include this word in the advertisement emails. For example, ??Here is our best-seller products??, ??Our lowest price of the season on XX?? and so on.  

- Free

According to the generalized linear model, ??Free?? is considered as a high potential attribute for spam mail, as its p value is 3.67e-10l. ??Free?? by its definition ??not costing or charging anything?? is considered as a advertising gimmick under most situation. For instance, ??free trial??, ??free sample?? and ??free shipping??. There is also some special case for it appears to be a delivery of e-journal, including heading like ??O.J. Simpson could be set free from Nevada prison Monday??. But such outrageous news is quite rare, we would instead conclude that ??free?? is a iconic spam word among the 4601 correspondence.

- Money

In the introduction of the Spambase, we acknowledge that make money fast schemes is one of the popular junk mail circulated across the internet. It is an obvious trap aiming at the credulous people, as ??money?? is often appears in the multilevel marketing email.
 
- 000 & $

Very alike the word ??Money??, ??000?? and  ??$?? also are related with large amount of financial crime involving gambling and lottery fraud. For example, ??...won $1,000,000??. Moreover, in the table above they each has a p value of 3.75e-04 and 8.45e-12.

Next, we would like to offer a list of non-spam words, and the collection of the non-spam emails is from the field work or the personal emails. Our group will give the analysis of why each of the word are contributed to the non-spam filter. 

- George

George is a widespread masculine name for both the given name or surname. It is popular as the given name because of the spread veneration of the Christian military saint Saint George. Also, George serves as a surname of Irish, English, Welsh, South Indian Christian, Middle Eastern Christian, French, or Native American origin. If the email contains the word ??George??, it is more likely that people send the email to a specific person named ??George?? for the personal matters. Therefore, our group classify the word ??George?? as the non-spam word. 

- 650

According to the generalized linear model, ??650?? is considered as a high potential attribute for non-spam mail, as its p value is 8.51e-03. People often refer the word ??650?? as the area code. In the situation where the email contains ??650??, it is very likely that the transmitters want to send the announcement to the receivers only within a certain area. Therefore, we classify ??650?? as non-spam filter.

- Project

The word ??Project?? may refer to the individual or group project in the school work or may refer to the certain tasks/programs the business company are undertaking. Since the work ??Project?? is often viewed as a sequence of tasks to be executed, the marketing department seldom use this work in their advertisement email. So, we would like to use this word as non-spam filter. 

- Re

??Re?? in the subject line of the email implies the ??response?? or ??reply??. If the email holds ??Re??, it is more likely to be a reply email and have a small chance to become the spam email. So, our group place the word ??Re?? into the non-spam filter. In addition, ??Re?? has a p value of 2.33e-07, which is very significant. 

- Edu

??Edu?? is considered as a high potential attribute for non-spam mail as its p value is 3.89e-06, according to the generalized linear model. Only certain type of people could have the email address ends with ??edu??, like the college administration office, professors, students, and so on. If people receive the email from the ??edu?? address, it is probable that the professors or college faculty member would like to make an announcement or send out the newsletter, which contains the useful information for you. Or even your classmates would like to communicate with you for some group projects or course concerns. Therefore, our group labels ??Edu?? as non-spam filter. 

```{r}
significant = c(1,2,4,5,6,7,8,9,12,15,16,17,19,20,21,23,24,25,26,27,28,29,33,34,35,36,38,39,41,42,43,44,45,46,47,48,49,52,53,54,55,56,57,58)
```

From the summary, we re-fine the significant variables based on AIC value, and we create a new dataset for those. Then, we would like to generate a reduced train and test dataset only for the variables that are selected from the stepwise selection.

```{r echo=FALSE}
train_reduced =train[,significant]
test_reduced =test[,significant]

```

In this way, we can use the methods we utilized before to do the classification again and compare the result with the previous analysis for each of the methods. We would like to start with Na??ve Bayes method first. 

1.	Naive Bayes


```{r echo=FALSE}
naive_reduced = naiveBayes(spam~., data = train_reduced)
nb_predict_reduced <- predict(naive_reduced, test_reduced[,!names(test_reduced) %in% "Spam"])
confusionMatrix(nb_predict_reduced,test_reduced$spam)
```
The summary table indicates that the accuracy rate is 0.709 and the misclassification error is 0.291, which is 1-0.709 for the Naive Bayes method. There is 70.9% of the testing sample is correctly classified in Naive Bayes method. Compared to the accuracy rate of 0.7023 in the full model, there is a slightly increase in accuracy for this reduced model.

2.	SVM


```{r echo=FALSE}
svm_reduced = svm(spam~., data = train_reduced)
svm_predict_reduced = predict(svm_reduced, test_reduced[,-44])
confusionMatrix(svm_predict_reduced,test_reduced$spam)
```

The accuracy rate shown in the above table is 0.9327 and the misclassification error is 0.0673, which can be calculated by 1-0.9327 in the SVM method. The results show that 93.27% of the testing sample is correctly classified. Compared to the 0.9344 accuracy of the full model, there is an insignificant decrease in accuracy rate for the SVM method for the reduced model. However, the SVM method performs better in classification than Naive Bayes method in both the full and the reduced model.

3.	Random Forest


```{r echo=FALSE}
set.seed(10)
rf_reduced=randomForest(train_reduced[,-44], as.factor(train_reduced[,44]), mtry = 7, nodesize = 1,importance=TRUE)
```

```{r echo=FALSE}
forest_predict_reduced = predict(rf_reduced, test_reduced[,-44])
confusionMatrix(forest_predict_reduced,test_reduced$spam)
```

By looking at the result from the above table, the accuracy rate for the Random Forest method is 0. 9546 and the misclassification rate is 0. 0454, which is calculates by 1-0. 9546. There is 95.46% of the testing sample is correctly classified in the reduced model of Random Forest method. Compared to the 0.9537 accuracy rate of the full model, there is a slightly increase for the reduced model. We could also conclude that the accuracy rate in Random Forest method performs the best outcome both in full and the reduced model than thee Na??ve Bayes method and SVM method. 


We have found that Random Forest may be a good method for classification of spam/non-spam email. So, we would like to take a close look at the Random Forest method to see whether we could further improve the effectiveness of the prediction. Therefore, our group plans to reduce the variables based on the p-value of the logistic regression, instead of the ??stepwise selection?? method. First, we will include the variables which the p-value is less than 0.05 in binomial regression, then we will calculate the misclassification error and the accuracy rate to see whether the new reduced model is more effective.  

```{r}
significant2 = c(5,6,7,8,16,17,19,21,23,24,25,27,28,33,34,36,42,44,45,46,49,52,53,54,55,56,57,58)
```

```{r echo=FALSE}
train_reduced_2 =train[,significant2]
test_reduced_2 =test[,significant2]
```

```{r echo=FALSE}
set.seed(10)
rf_reduced_2=randomForest(train_reduced_2[,-28], as.factor(train_reduced_2[,28]), mtry = 7, nodesize = 1,importance=TRUE)
```

```{r echo=FALSE}
forest_predict_reduced_2 = predict(rf_reduced_2, test_reduced_2[,-28])
confusionMatrix(forest_predict_reduced_2,test_reduced_2$spam)
```

From the table above, we could see that the accuracy rate for the second reduced model is 0.9537, and the misclassification error is 0.0462, which can be calculated by 1-0. 9537. The results show that 95.37% of the testing sample is correctly classified. Compared to the 0.9546 accuracy rate of the previous reduced model of the Random Forest method, the accuracy rate of the second reduced model has slightly decreased to 0.9537.


For curious, our group would like to reduce the full model again to the third reduced model. In this time, we will use the variables with p -values less than 0.001 in binomial regression to see is there any improvement of the accuracy rate for the Random Forest method. 


```{r}
significant3 = c(5,7,16,21,23,25,27,34,45,46,52,53,58)
```

```{r echo=FALSE}
train_reduced_3 =train[,significant3]
test_reduced_3 =test[,significant3]
```

```{r echo=FALSE}
set.seed(10)
rf_reduced_3=randomForest(train_reduced_3[,-13], as.factor(train_reduced_3[,13]), mtry = 7, nodesize = 1,importance=TRUE)
```

```{r echo=FALSE}
forest_predict_reduced_3 = predict(rf_reduced_3, test_reduced_3[,-13])
confusionMatrix(forest_predict_reduced_3,test_reduced_3$spam)
```

From the table above, we could see that the accuracy rate for the third reduced model is 0.931, and the misclassification error is 0.069, which can be calculated by 1-0. 931. The results show that 93.1% of the testing sample is correctly classified. Compared to the 0.9537 accuracy rate of the second reduced model and 0.9546 accuracy rate of the first reduced model of the Random Forest method, the accuracy rate of the third reduced model with variables?? p values less than 0.001 has slightly decreased to 0.931. Therefore, we would like to conclude that the reduce the models which based on significance of the variables did not give us a more accurate Random Forest classification.


# Conclusion and Discussion

## Summarize Scientific Findings

In summary, our group uses one full model and one reduced model from the "stepwise selection" to perform different classification methods such as Naive Bayes, SVM and Random Forest. Based on the accuracy rate and the misclassification rate of each method, we find that in both the full model and the reduced model, Random Forest method has the highest accuracy rate and the lowest misclassification rate than SVM and Naive Bayes. Also, in both models, SVM performs much better than Naive Bayes based on accuracy rate and misclassification rate. The accuracy rate of the Random Forest can reach 95%, which is considerably effective in spam/non-spam classification. 

Our group also compares the classification results before and after we reduced the variables. We find that the full model and the reduced model based on "stepwise selection" do not have significant difference in accuracy rate of the Naive Bayes, the SVM and the Random Forest method. Since we do a "stepwise selection" to obtain the reduced model, we use the model with the minimum AIC. So this reduced model is considered better than the full model with larger AIC. We can also use the other model selection criterias such as Mallow's Cp and BIC to do the model selection.

Besides, our group tries to increase the accuracy rate of the Random Forest method. We reduce the model further based on the p-value of the logistic regression. The second reduced model contains all variables with p-value less than 0.05 and the third reduced model contains all variables with p-value less than 0.001. We find that when we reduce more variables, the accuracy rate of the Random Forest decreases. Based on our analysis, we still consider the first reduced model based on the "stepwise selection" as the best model that gives us the best classification result. 


## Potential Pitfalls and Improvements

The Spambase has its limitation as the data was collected during 90s, which is not sufficient for analyzing the spam base for nowadays email. Today the genre of email could be specify into Main, Social media, subscribe promotion and Spam. Also email is not only considered as a popular contact tools, but also the essential identification for your account on the website. The detection of spam would need to drop some of the words out of the spam list such as personal pronoun. Though they appear to be significant in our analysis, they all identify as non spam. Instead of adding those interference, we would contain more meaningful non spam words, such as more names of weekdays, which indicates specific schedule. Also phrase could be a better form to detect spam.  For instance, the difference between ??CS?? and ??CS go?? . Although these two words all contain ??CS?? word, ??CS go?? should be identified as promotion rather than the ??CS?? on the university course listing.


Another pitfall for our project is there are some of the methods which have been mentioned in the literature review, but our group member could not perform the dataset in the same way as the authors did in the their project. Machine learning is a deep topic, and there are much more room for us to learn beyond the course STAT 432. Therefore, our group believes that there is a better way to perform the reduced model with some high-level feature selection algorithm after we gain more knowledge in the machine learning field in the future. 





