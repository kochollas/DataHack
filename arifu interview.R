# Arifu challenge 
# Code by Mike Kochola

#Assignment 1

# pulling data from google sheet
library(googlesheets)
library(dplyr)
gs_auth(new_user = TRUE)
test<-gs_key("1jhayIizlUyf_kjSGMNyy8pP9A_WbasZSj6IIdNN57dE")
housing<-gs_read(test, ws=1)

x=data.frame(unique(housing$learner_id))
nrow(housing)
trainings<-table(housing$program_code)
trainings<-data.frame(trainings)
top3<-trainings%>%arrange(Freq)
#interactions
attach(housing)
m=xtabs(~variation_code+learner_id, data=housing)
m=data.frame(m)
top_int<-m%>%arrange(Freq)
#more than 100 interactions
more100int<-top_int%>%filter(Freq>100)
#less than 100 inter
less100int<-top_int%>%filter(Freq<=100)
# distribution of inter count
hist(less100int$Freq)
freq1<-less100int%>%filter(Freq>0)
hist(freq1$Freq)
# day of week
data1<-data.frame(table(housing$created_at))
data1<-data1%>%arrange(Freq)

#QUESTION 2

fert_data<-read.csv("/media/mikes/F/Data_Science03-11-2018/interviews/chal2.csv")
table(fert_data$variation_code)
# test of proportions on the significance of thumb being a popular training
res <- prop.test(x = c(3013, 3718), n = c(6731, 6731), alternative = "less")
#n=6731

The thumps is more popular at 55.2% backed with significance level test using test of proportion with chisq.test().
having a p value less than 0.05
proportion of Thumbs fert training is significantly higher than Narrative training.

#QUESTION 3.
credit<-read.csv("/media/mikes/F/Data_Science03-11-2018/interviews/creditdata.csv")
# exploratory data analysis
str(credit)
table(credit$job) # skilled employed leading
table(credit$checking_balance) # 1-200  dm leading
table(credit$months_loan_duration) # too many indepe factors
table(credit$credit_history) # leading category repaid
table(credit$purpose) # leading takes loan for radio and tv
hist(credit$amount) # Majority take less than 10000
table(credit$savings_balance) # less 100DM leading
table(credit$employment_length) # 1-4 years
table(credit$installment_rate)
table(credit$personal_status)
table(credit$job)

# split training and test sets
n=1000
training = 700
test = 300
training_id<-data.frame(sample(c(1:1000),700,replace = F))
names(training_id)=c("observation_id")
train<-merge(credit,training_id, by ="observation_id")

#modelling using logistic regression
#make response variable dichotomous using a loop
for( i in 1: nrow(credit)){
  if(credit[i,18]==2){
    credit[i,18]<-0
  }
  
  
}
# on the train data
for( i in 1: nrow(train)){
  if(train[i,18]==2){
    train[i,18]<-0
  }
  
  
}




attach(train)

mod1<-glm(formula = default ~ ., family = binomial(link = "logit"), data = train)
summary(mod1)

# Evaluating on test data
test<-data.frame(sample(c(1:1000),300,replace = F))
names(test)=c("observation_id")
test<-merge(credit,test, by ="observation_id")
#Prediction and evaluation
pred<-round(predict(mod1,test, type="response"))

table(pred)
table(test$default)
model prediction of bad loan :85% accurate

