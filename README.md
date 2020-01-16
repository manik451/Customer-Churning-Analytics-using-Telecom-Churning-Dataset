Analyzed Customer Churning using Telecom Churning Dataset (R)

Experimental Data used for this model was Customer churning dataset available on Kaggle. Total we have 3333 records/ observations and 20 variables

What is Churn?
(In general): Loss/attrition of a customer for a company
(In Telecom): Customer attrition mainly owing to a either a grievance/or upgradation to a competitoraa
For the dataset: The dependent variable that is a result of multiple independent entities in telecom business
For data mining: subject of classification with a wider aim to analyse or suggest how Churn should be minimized
For Business: Shift the focus to relevant (read rules) to the right variables (entities or plans ) to achieve least churn

Data Prepocessing

Data type for Churn : Converted from Logical class to Numerical.
Dataset has no missing values.
Dropped Area.code - Data is adding no value to outcome variable and is of low quality (i.e., there are 3 same area codes for all the states)
Dropped Account.Length - Account length itself is dependent on outcome variable Churn, which signifies the customer’s lifetime with the network, cannot make an impact on the outcome (churn) variable.
Data Manipulation- All Numerical variables have been normalized on the detection of outliers. 


Build Three models :

1: Logistic Regression

Goal: To classify records or predict a new records class or to compute the propensity of a new record. In this case, predicting churn as 0 or 1.It is a parametric approach, dataset has 17 variables which needs to be reviewed for influence on outcome variable churn.
A stepwise search algorithm has been applied on dataset and compared with logistic model with all predictors.
To analyze how accurate our model is performing, we tried to find out using accuracy matrix also tried to show it visually using lift chart.

2: Decision Tree

Goal: The output of decision trees, the set of rules, provide highly effective structure to understand and reduce Churn. 
They also help you to form a balanced picture of the risks and rewards associated with each possible course of action.
In the original dataset, ratio of rows with Churn = 1 is ~14%.

3: Neural Network

In neural networks, it is common practice to check prediction metrics on the train data too as well as test data.


Knowledge and Actions:
We recommend Decision trees for their easy interpretability, reduction of cost complexity and availability of rules and risks involved. 
The rules of decision trees can be helpful in making plans for minimizing the churn while logistic helps the company understand the probabilities of churn based on numerical and categorical values. 
Neural networks helped us understand more about model and data evaluation.


Recommendations:
We recommend Decision Tree algorithm for the problem of Churn because it is a data driven model, i.e., more the data; better the results

