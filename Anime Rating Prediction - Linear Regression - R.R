#1. Importing Dataset
CLV = read.csv('C:/Users/LENEVO/Documents/1/Stats + R/Final R Project/Final R Project IVY/Fn-UseC_-Marketing-Customer-Value-Analysis.csv',na.strings = c(""," ","NA","NULL"))

#Dimemsion of the data
dim(CLV)
#There are 9134 rows and 22 columns in the data

#Checking the structure and summary of the dataset
summary(CLV)
str(CLV)

#Categorical Columns
categoryvar = c('State','Response','Coverage','Education','EmploymentStatus','Gender'
                ,'Location.Code','Policy.Type','Type.of.Open.Complaints','Type.of.Policies','Policy',
                'Renew.offer.Type','Sales.Channel','Vehicle.Class','Vehicle.Size','Effective.To.Date')

#Conversion of Continuous columns into categorical columns
for(fcol in categoryvar){
  CLV[, c(fcol)] = as.factor(CLV[, c(fcol)])
}

#Continous Columns
Continousvar = c('Customer.Lifetime.Value','Income','Monthly.Premium.Auto',
                 'Months.Since.Last.Claim','Months.Since.Policy.Inception',
                 'Total.Claim.Amount')

#Checking Missing values in the Dataset
colSums(is.na(CLV))
#There are no missing values in the Dataset

###---Univariate Analysis of the Data---###

# Traetment of outliers
par(mfrow=c(1,1))
# Customer Lifetime Value
boxplot(CLV$Customer.Lifetime.Value, horizontal = T)
quantiles2 = quantile(CLV$Customer.Lifetime.Value, c(0.95,0.96,0.963,0.97,0.98,0.99,0.995,0.997,0.998,0.999,0.9997))
quantiles2

final_quantile = quantile(CLV$Customer.Lifetime.Value, 0.9997)
CLV$Customer.Lifetime.Value = ifelse(CLV$Customer.Lifetime.Value>final_quantile, final_quantile,
                                     CLV$Customer.Lifetime.Value)
max(CLV$Customer.Lifetime.Value)

# Monthly Premium Auto
boxplot(CLV$Monthly.Premium.Auto, horizontal = T)

quantiles = quantile(CLV$Monthly.Premium.Auto, c(0.95,0.96,0.963,0.97,0.98,0.99,0.995,0.997,0.998))

quantiles_final = quantile(CLV$Monthly.Premium.Auto,0.995)
quantiles_final
max(CLV$Monthly.Premium.Auto)

CLV$Monthly.Premium.Auto = ifelse(CLV$Monthly.Premium.Auto >quantiles_final, quantiles_final,
                                  CLV$Monthly.Premium.Auto)

#Months Since Last Claim
boxplot(CLV$Months.Since.Last.Claim, horizontal = T)
# There are no outliers in this column

# Months Since Policy Inception
boxplot(CLV$Months.Since.Policy.Inception, horizontal = T)
# There are no outliers in this column

# Income : There are no outliers in this column
boxplot(CLV$Income, horizontal = T)

# Total Claim Amount
boxplot(CLV$Total.Claim.Amount, horizontal = T)

quantiles1 = quantile(CLV$Total.Claim.Amount, c(0.95,0.96,0.963,0.97,0.98,0.99,0.995,0.997,0.998))
quantiles1

quants_final = quantile(CLV$Total.Claim.Amount, 0.998)
quants_final

CLV$Total.Claim.Amount = ifelse(CLV$Total.Claim.Amount > quants_final, quants_final, CLV$Total.Claim.Amount)
max(CLV$Total.Claim.Amount)



# Division of screen into 6 parts
par(mfrow=c(2,3))

library('RColorBrewer')

# Histogram of Continous Columns
for(histogram in Continousvar)
{
  hist(CLV[ , c(histogram)], main=paste('Histogram of:',histogram), col=brewer.pal(8,'Paired'))
}

#Divison of plot screen into 16 parts
par(mfrow=c(2,7))

# Barplot of Categorical Columns
for(bar in categoryvar)
{
  barplot(table(CLV[,c(bar)]), main = paste('Barplot of:',bar), col=brewer.pal(8,'Paired'))
}

##--- Bivariate Analysis ---###
#For Bivarite analysis our target variable CLV is a continous variable
# For continous vs continous variables scatter plot is plotted

par(mfrow=c(1,1))

#Scatter Plot for Continous vs Continous variables
plot(CLV[,c(Continousvar)], col='blue')

#The corelation of the predictor variables with the target variable is analysed through 1st row
#Through Scatter Plot we analyse that there is no corelation between target variables and the
#predictor variables.

# For Continous vs Categorical Variables Box Plot is Plotted

par(mfrow=c(2,7))

for(boxgraph in categoryvar)
{
  boxplot(Customer.Lifetime.Value~CLV[,c(boxgraph)], data=CLV, main=paste('Box Plot of:',boxgraph),
           col=brewer.pal(8,'Paired'))
}

###--- Probability tests for the prediction of good variables and bad variables for the final ML dataset---###

#Correlation test to analyze the relation between continuous vs continuous variables
#Continous Columns
Continousvar = c('Customer.Lifetime.Value','Income','Monthly.Premium.Auto',
                 'Months.Since.Last.Claim','Months.Since.Policy.Inception',
                 'Total.Claim.Amount')

corrData = cor(CLV[,c(Continousvar)], use='complete.obs')
corrData

#Good variables to be considered would be for which absolute correlation with 
#customer.lifetime.value >0.5
abs(corrData['Customer.Lifetime.Value',])>0.5
 # There are no good continous predictor variables

#ANOVA test to analyze the relation between continuous vs categorical variables

#Categorical Columns
categoryvar = c('State','Response','Coverage','Education','EmploymentStatus','Gender'
                ,'Location.Code','Policy.Type','Type.of.Open.Complaints','Type.of.Policies','Policy',
                'Renew.offer.Type','Sales.Channel','Vehicle.Class','Vehicle.Size','Effective.To.Date')

for (ATEST in categoryvar) {
  AnovaSUmmary = summary(aov(Customer.Lifetime.Value~CLV[,c(ATEST)], data=CLV))
  print(ATEST)
  print(AnovaSUmmary)
}

#In ANOVA test ,  categorical predictor variables whose p value is less than 0.05,
#for those predictor variables we reject the null hypothesis and those predictor variables
#are highly corelated with the target variable

# Good Categorical Predictor Variables are: Coverage, Education, EmploymentStatus,
# Type.of.Open.Complaints, Type.of.Policies

# Predictor Variables
PredictorVariableNames = c('Coverage', 'Education', 'EmploymentStatus',
                           'Type.of.Open.Complaints', 'Type.of.Policies')

###--- Creating Final Model for ML ---###
InputData = CLV
TargetVariableName = 'Customer.Lifetime.Value'

TargetVariable = InputData[,TargetVariableName]
PredictorVariable = InputData[,PredictorVariableNames]

MLData = data.frame(TargetVariable,PredictorVariable)

####-----Sampling | Splitting of data into 70% train and 30% Test dataset----####

sampleSummary = sample(1:nrow(MLData), size = 0.7*nrow(MLData))
length(sampleSummary)
DataForTrain = MLData[sampleSummary,]
DataForTest = MLData[-sampleSummary,]

## Applying Linear Regression on the training dataset
Lm_Model = lm(TargetVariable~., data=DataForTrain)
summary(Lm_Model)

# The P value of Education, Type of open complaints, Employment status is greater than 0.5
# hence the intercept of coefficient of these variables is equal to zero
# We remove these variables from the LR Model

Model1 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
               I(Education=='College')+I(Education=='Doctor')+I(Education=='High School or Below')
               +I(Education=='Master')+I(EmploymentStatus=='Employed')+
                 I(EmploymentStatus=='Medical Leave')+I(EmploymentStatus=='Retired')+
                 I(EmploymentStatus=='Unemployed')+I(Type.of.Open.Complaints==1)+
                 I(Type.of.Open.Complaints==2)+I(Type.of.Open.Complaints==3)+
                 I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
               I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
                 I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model1)

Model2 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')+I(Education=='Doctor')+I(Education=='High School or Below')
            +I(Education=='Master')+I(EmploymentStatus=='Employed')+
              I(EmploymentStatus=='Medical Leave')+I(EmploymentStatus=='Retired')+
              I(EmploymentStatus=='Unemployed')+I(Type.of.Open.Complaints==1)+
              I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model2)

Model3 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')+I(Education=='Doctor')+I(Education=='High School or Below')
            +I(Education=='Master')+I(EmploymentStatus=='Employed')+
              I(EmploymentStatus=='Medical Leave')+I(EmploymentStatus=='Retired')+
              I(EmploymentStatus=='Unemployed')+I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model3)

Model4 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')+I(Education=='Doctor')+I(Education=='High School or Below')
            +I(Education=='Master')+I(EmploymentStatus=='Employed')+
              I(EmploymentStatus=='Medical Leave')+I(EmploymentStatus=='Retired')+I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model4)

Model5 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')+I(Education=='Doctor')+I(Education=='High School or Below')
            +I(Education=='Master')+I(EmploymentStatus=='Employed')+
              I(EmploymentStatus=='Medical Leave')+I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model5)

Model6 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')+I(Education=='Doctor')+I(Education=='High School or Below')
            +I(Education=='Master')+I(EmploymentStatus=='Employed')
              +I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model6)

Model7 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')+I(Education=='Doctor')+I(Education=='High School or Below')
            +I(EmploymentStatus=='Employed')
            +I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model7)

Model8 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')+I(Education=='Doctor')
            +I(EmploymentStatus=='Employed')
            +I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model8)


Model8 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')+
              I(Education=='College')
            +I(EmploymentStatus=='Employed')
            +I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model8)

Model9 = lm(TargetVariable~I(Coverage=='Extended')+ I(Coverage=='Premium')
                      +I(EmploymentStatus=='Employed')
            +I(Type.of.Open.Complaints==3)+I(Type.of.Open.Complaints==4)+I(Type.of.Policies==2)+I(Type.of.Policies==3)+
              I(Type.of.Policies==4)+I(Type.of.Policies==5)+I(Type.of.Policies==6)+I(Type.of.Policies==7)+
              I(Type.of.Policies==8)+I(Type.of.Policies==9), data=DataForTrain)
summary(Model9)

## Predictons of the linear model
#1. The customer life time value of a customer having extended coverage is 1641 units more
#   than a customer having basic coverage.
#2. The customer life time value of a customer having premium coverage is 3693 units more
#   than a customer having basic coverage.
#3. The Customer Life time value of an Employed customer is 697 units more than a disabled customer
#4. The customer Life Time value of a customer having 3rd open complaints is 1264 units less than
#   the customer having 0 complaints
#5. The customer Life Time value of a customer having 4th open complaints is 1281 units less than
#   the customer having 0 complaints
#6. The customer Life time value of a customer having 2nd policy is 12240 units more than a customer
#   having 1st policy
#7. The customer Life time value of a customer having 3rd policy is 3442 units more than a customer
#   having 1st policy
#8. The customer Life time value of a customer having 4th policy is 3444 units more than a customer
#   having 1st policy
#9. The customer Life time value of a customer having 5th policy is 3611 units more than a customer
#   having 1st policy
#10. The customer Life time value of a customer having 6th policy is 3305 units more than a customer
#   having 1st policy
#11. The customer Life time value of a customer having 7th policy is 3715 units more than a customer
#   having 1st policy
#12. The customer Life time value of a customer having 8th policy is 3578 units more than a customer
#   having 1st policy
#13. The customer Life time value of a customer having 9th policy is 3487 units more than a customer
#   having 1st policy

###--Checking Accuracy of model on Test Data Set--###
head(DataForTest)
DataForTest$Pred_Lm = predict(Model9,DataForTest)

# Calculating the Absolute Percentage Error for each prediction
DataForTest$LM_APE= 100 *(abs(DataForTest$TargetVariable-DataForTest$Pred_Lm)/DataForTest$TargetVariable)
head(DataForTest)

##for final accuracy: we take the mean/median of all the errors and subtract it from 100

MeanAPE=mean(DataForTest$LM_APE)
MedianAPE=median(DataForTest$LM_APE)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

# Final Summary Model of LR Model
# R Square : 0.507
# Adjusted R Square : 0.506
# F statistic: 504.6
# Residual Standard Error: 4857
# Mean Accuracy: 69
# Median Accuracy: 75


#####-----Decision Tree Model-----#####

install.packages("party")
library(party)

ctree_model = ctree(TargetVariable~., data=DataForTrain)
par(mfrow=c(1,1))
plot(ctree_model)

###-- Checking the accuracy of decision tree model on test data--###
DataForTest$Pred_CTREE=as.numeric(predict(ctree_model, DataForTest))
head(DataForTest)

DataForTest$CTREE_APE= 100 *(abs(DataForTest$TargetVariable-DataForTest$Pred_CTREE)/DataForTest$TargetVariable)
head(DataForTest)

print(paste('### Mean Accuracy of Decision tree Model is: ', 100 - mean(DataForTest$CTREE_APE)))
print(paste('### Median Accuracy of Decision tree Model is: ', 100 - median(DataForTest$CTREE_APE)))

# Final Summary of Decision Tree
# Mean Accuracy: 70
# Median Accuracy: 75

###---MODEL COMPARISON---###
#The mean and median accuracy of decision tree is greater than linear regression model
#hence decision tree model would be a good fit to analyse the customer life time value.

# multicollinearity test
install.packages("car")
library(car)
vif(Model9)

#BP Test
install.packages("lmtest")
library(lmtest)
bptest(Model9)



