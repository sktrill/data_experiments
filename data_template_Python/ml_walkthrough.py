# -*- coding: utf-8 -*-
"""
===============================================================
REFERENCE - MACHINE LEARNING PROJECT EXAMPLE
===============================================================
- Swapnil Kotecha

General ML workflow:    
1. Data cleaning and formatting
2. Exploratory data analysis
3. Feature engineering and selection/extraction
4. Establish a baseline and compare several machine learning models 
on a performance metric
5. Perform hyperparameter tuning on the best model to optimize it for the problem
6. Evaluate the best model on the testing set
7. Interpret the model results to the extent possible
8. Draw conclusions and document work

The objective is to use the energy data to build a model that can 
predict the Energy Star Score of a building and interpret the results 
to find the factors which influence the score.

Good example: https://towardsdatascience.com/a-complete-machine-learning-walk-through-in-python-part-one-c62152f39420

"""

import pandas as pd
import numpy as np

# No warnings about setting value on copy of slice
pd.options.mode.chained_assignment = None

# Display up to 60 columns of a dataframe
pd.set_option('display.max_columns', 60)

# Matplotlib visualization
import matplotlib.pyplot as plt
%matplotlib inline

# Set default font size
plt.rcParams['font.size'] = 24

# Internal ipython tool for setting figure size
from IPython.core.pylabtools import figsize

# Seaborn for visualization
import seaborn as sns
sns.set(font_scale = 2)

# Splitting data into training and testing
from sklearn.model_selection import train_test_split

# Imputing missing values and scaling values
from sklearn.preprocessing import Imputer, MinMaxScaler

# Machine Learning Models
from sklearn.linear_model import LinearRegression, RANSACRegressor
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor

# Hyperparameter tuning
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV

from sklearn import tree

# LIME for explaining predictions
import lime
import lime.lime_tabular

#-------------------------------------
# 1. Data cleaning and formatting
#-------------------------------------

# read data into a dataframe
data = pd.read_csv('data/Energy_and_Water_Data_Disclosure_for_Local_Law_84_2017__Data_for_Calendar_Year_2016_.csv')

# Display top of dataframe
data.head()

# See the column data types and non-missing values
data.info()

# Replace all occurrences of Not Available with numpy not a number
data = data.replace({'Not Available': np.nan})

# Iterate through the columns
for col in list(data.columns):
    # Select columns that should be numeric
    if ('ft²' in col or 'kBtu' in col or 'Metric Tons CO2e' in col or 'kWh' in 
        col or 'therms' in col or 'gal' in col or 'Score' in col):
        # Convert the data type to float
        data[col] = data[col].astype(float)


# Statistics for each column
data.describe()

# Function to calculate missing values by column
def missing_values_table(df):
        # Total missing values
        mis_val = df.isnull().sum()
        
        # Percentage of missing values
        mis_val_percent = 100 * df.isnull().sum() / len(df)
        
        # Make a table with the results
        mis_val_table = pd.concat([mis_val, mis_val_percent], axis=1)
        
        # Rename the columns
        mis_val_table_ren_columns = mis_val_table.rename(
        columns = {0 : 'Missing Values', 1 : '% of Total Values'})
        
        # Sort the table by percentage of missing descending
        mis_val_table_ren_columns = mis_val_table_ren_columns[
            mis_val_table_ren_columns.iloc[:,1] != 0].sort_values(
        '% of Total Values', ascending=False).round(1)
        
        # Print some summary information
        print ("Your selected dataframe has " + str(df.shape[1]) + " columns.\n"      
            "There are " + str(mis_val_table_ren_columns.shape[0]) +
              " columns that have missing values.")
        
        # Return the dataframe with missing information
        return mis_val_table_ren_columns

missing_values_table(data)

# Get the columns with > 50% missing
missing_df = missing_values_table(data);
missing_columns = list(missing_df[missing_df['% of Total Values'] > 50].index)
print('We will remove %d columns.' % len(missing_columns))

# Drop the columns
data = data.drop(columns = missing_columns)


#-------------------------------------
# 2. Exploratory data analysis
#-------------------------------------
# Exploratory Data Analysis (EDA) is an open-ended process where we make plots 
# and calculate statistics in order to explore our data. The purpose is to to 
# find anomalies, patterns, trends, or relationships. These may be interesting 
# by themselves (for example finding a correlation between two variables) or 
# they can be used to inform modeling decisions such as which features to use. 
# In short, the goal of EDA is to determine what our data can tell us! EDA 
# generally starts out with a high-level overview, and then narrows in to 
# specific parts of the dataset once as we find interesting areas to examine.

figsize(8, 8)

# Rename the score 
data = data.rename(columns = {'ENERGY STAR Score': 'score'})

# Histogram of the Energy Star Score
plt.style.use('fivethirtyeight')
plt.hist(data['score'].dropna(), bins = 100, edgecolor = 'k');
plt.xlabel('Score'); plt.ylabel('Number of Buildings'); 
plt.title('Energy Star Score Distribution');

# Histogram Plot of Site EUI
figsize(8, 8)
plt.hist(data['Site EUI (kBtu/ft²)'].dropna(), bins = 20, edgecolor = 'black');
plt.xlabel('Site EUI'); 
plt.ylabel('Count'); plt.title('Site EUI Distribution');


# explore statistics
data['Site EUI (kBtu/ft²)'].describe()
data['Site EUI (kBtu/ft²)'].dropna().sort_values().tail(10)

# found outlier
data.loc[data['Site EUI (kBtu/ft²)'] == 869265, :]

# definition of extreme outlier
# On the low end, an extreme outlier is below  First Quartile -3 * Interquartile Range
# On the high end, an extreme outlier is above Third Quartile + 3 * Interquartile Range
# Calculate first and third quartile
first_quartile = data['Site EUI (kBtu/ft²)'].describe()['25%']
third_quartile = data['Site EUI (kBtu/ft²)'].describe()['75%']

# Interquartile range
iqr = third_quartile - first_quartile

# Remove outliers
data = data[(data['Site EUI (kBtu/ft²)'] > (first_quartile - 3 * iqr)) &
            (data['Site EUI (kBtu/ft²)'] < (third_quartile + 3 * iqr))]

# Histogram Plot of Site EUI
figsize(8, 8)
plt.hist(data['Site EUI (kBtu/ft²)'].dropna(), bins = 20, edgecolor = 'black');
plt.xlabel('Site EUI'); 
plt.ylabel('Count'); plt.title('Site EUI Distribution');
# This plot looks a little less suspicious and is close to normally distributed 
# with a long tail on the right side (it has a positive skew).


# look for relationships:
# examine catergorical variables with density plots

# Create a list of buildings with more than 100 measurements
types = data.dropna(subset=['score'])
types = types['Largest Property Use Type'].value_counts()
types = list(types[types.values > 100].index)

# Plot of distribution of scores for building categories
figsize(12, 10)

# Plot each building
for b_type in types:
    # Select the building type
    subset = data[data['Largest Property Use Type'] == b_type]
    
    # Density plot of Energy Star scores
    sns.kdeplot(subset['score'].dropna(),
               label = b_type, shade = False, alpha = 0.8);

# label the plot
plt.xlabel('Energy Star Score', size = 20); plt.ylabel('Density', size = 20); 
plt.title('Density Plot of Energy Star Scores by Building Type', size = 28);
# this graph tells us that we should include the property type because this 
# information can be useful for determining the score. As building type is a 
# categorical variable, it will have to be one-hot encoded before we can feed 
# it into a machine learning model

# Create a list of boroughs with more than 100 observations
boroughs = data.dropna(subset=['score'])
boroughs = boroughs['Borough'].value_counts()
boroughs = list(boroughs[boroughs.values > 100].index)

# Plot of distribution of scores for boroughs
figsize(12, 10)

# Plot each borough distribution of scores
for borough in boroughs:
    # Select the building type
    subset = data[data['Borough'] == borough]
    
    # Density plot of Energy Star scores
    sns.kdeplot(subset['score'].dropna(),
               label = borough);    
                
# label the plot
plt.xlabel('Energy Star Score', size = 20); plt.ylabel('Density', size = 20); 
plt.title('Density Plot of Energy Star Scores by Borough', size = 28);
# The borough of the building does not seem to make as significant a difference
# in the distribution of the score as does the building type. Nonetheless, 
# it might make sense to include the borough as a categorical variable


# correlations between features and target
# Find all correlations and sort 
correlations_data = data.corr()['score'].sort_values()

# Print the most negative correlations
print(correlations_data.head(15), '\n')

# Print the most positive correlations
print(correlations_data.tail(15))

# headmap with top correlations
columns = correlations_data.head().index
sns.heatmap(data[columns].corr(), annot = True)
plt.show()

# headmap with bottom correlations
columns = correlations_data.tail().index
sns.heatmap(data[columns].corr(), annot = True)
plt.show()


# In the following code, we take log and square root transformations of the 
# numerical variables, one-hot encode the two selected categorical variables 
# (building type and borough), calculate the correlations between all of the 
# features and the score, and display the top 15 most positive and top 15 most 
# negative correlations.
# Select the numeric columns
numeric_subset = data.select_dtypes('number')

# Create columns with square root and log of numeric columns
for col in numeric_subset.columns:
    # Skip the Energy Star Score column
    if col == 'score':
        next
    else:
        numeric_subset['sqrt_' + col] = np.sqrt(numeric_subset[col])
        numeric_subset['log_' + col] = np.log(numeric_subset[col])

# Select the categorical columns
categorical_subset = data[['Borough', 'Largest Property Use Type']]

# One hot encode
categorical_subset = pd.get_dummies(categorical_subset)

# Join the two dataframes using concat
# Make sure to use axis = 1 to perform a column bind
features = pd.concat([numeric_subset, categorical_subset], axis = 1)

# Drop buildings without an energy star score
features = features.dropna(subset = ['score'])

# Find correlations with the score 
correlations = features.corr()['score'].dropna().sort_values()

# Display most negative correlations
correlations.head(15)

# Display most positive correlations
correlations.tail(15)

# After transforming the features, the strongest relationships are still those 
# related to Energy Use Intensity (EUI). The log and square root transformations 
# do not seem the have resulted in any stronger relationships. There are no 
# strong positive linear relationships although we do see that a building type 
# of office (Largest Property Use Type_Office) is slightly positively correlated 
# with the score. This variable is a one-hot encoded representation of the
# categorical variables for building type.

figsize(12, 10)

# Extract the building types
features['Largest Property Use Type'] = data.dropna(subset = ['score'])['Largest Property Use Type']

# Limit to building types with more than 100 observations (from previous code)
features = features[features['Largest Property Use Type'].isin(types)]

# Use seaborn to plot a scatterplot of Score vs Log Source EUI
sns.lmplot('Site EUI (kBtu/ft²)', 'score', 
          hue = 'Largest Property Use Type', data = features,
          scatter_kws = {'alpha': 0.8, 's': 60}, fit_reg = False,
          size = 12, aspect = 1.2);

# Plot labeling
plt.xlabel("Site EUI", size = 28)
plt.ylabel('Energy Star Score', size = 28)
plt.title('Energy Star Score vs Site EUI', size = 36);
# There is a clear negative relationship between the Site EUI and the score. 
# The relationship is not perfectly linear (it looks with a correlation 
# coefficient of -0.7), but it does look like this feature will be important 
# for predicting the score of a building.

figsize(12, 10)
# Extract the columns to  plot
plot_data = features[['score', 'Site EUI (kBtu/ft²)', 
                      'Weather Normalized Source EUI (kBtu/ft²)', 
                      'log_Total GHG Emissions (Metric Tons CO2e)']]

# Replace the inf with nan
plot_data = plot_data.replace({np.inf: np.nan, -np.inf: np.nan})

# Rename columns 
plot_data = plot_data.rename(columns = {'Site EUI (kBtu/ft²)': 'Site EUI', 
                                        'Weather Normalized Source EUI (kBtu/ft²)': 'Weather Norm EUI',
                                        'log_Total GHG Emissions (Metric Tons CO2e)': 'log GHG Emissions'})

# Drop na values
plot_data = plot_data.dropna()

# Function to calculate correlation coefficient between two columns
def corr_func(x, y, **kwargs):
    r = np.corrcoef(x, y)[0][1]
    ax = plt.gca()
    ax.annotate("r = {:.2f}".format(r),
                xy=(.2, .8), xycoords=ax.transAxes,
                size = 20)

# Create the pairgrid object
grid = sns.PairGrid(data = plot_data, size = 3)

# Upper is a scatter plot
grid.map_upper(plt.scatter, color = 'red', alpha = 0.6)

# Diagonal is a histogram
grid.map_diag(plt.hist, color = 'red', edgecolor = 'black')

# Bottom is correlation and density plot
grid.map_lower(corr_func);
grid.map_lower(sns.kdeplot, cmap = plt.cm.Reds)

# Title for entire plot
plt.suptitle('Pairs Plot of Energy Data', size = 36, y = 1.02);

# Now that we have explored the trends and relationships within the data, 
# we can work on engineering a set of features for our models. 
# We can use the results of the EDA to inform this feature engineering. 
# In particular, we learned the following from EDA which can help us in 
# engineering/selecting features:
#       1. The score distribution varies by building type and to a lesser 
#       extent by borough. Although we will focus on numerical features, 
#       we should also include these two categorical features in the model.
#       2. Taking the log transformation of features does not result in 
#       significant increases in the linear correlations between features 
#       and the score


#-------------------------------------
# 3. Feature engineering and selection
#-------------------------------------

# Feature Engineering: The process of taking raw data and extracting or creating 
# new features that allow a machine learning model to learn a mapping beween 
# these features and the target. This might mean taking transformations of 
# variables, such as we did with the log and square root, or one-hot encoding 
# categorical variables so they can be used in a model. Generally, I think of 
# feature engineering as adding additional features derived from the raw data.

# Feature Selection: The process of choosing the most relevant features in your
# data. "Most relevant" can depend on many factors, but it might be something 
# as simple as the highest correlation with the target, or the features with 
# the most variance. In feature selection, we remove features that do not 
# help our model learn the relationship between features and the target. 
# This can help the model generalize better to new data and results in a
# more interpretable model. Generally, I think of feature selection as 
# subtracting features so we are left with only those that are most important.

# Feature engineering and selection are iterative processes that will usually 
# require several attempts to get right. Often we will use the results of modeling,
# such as the feature importances from a random forest, to go back and redo 
# feature selection, or we might later discover relationships that 
# necessitate creating new variables. 

# Feature engineering and selection often has the highest returns on time invested 
# in a machine learning problem. It can take quite a while to get right, 
# but is often more important than the exact algorithm and hyperparameters used 
# for the model

# In this project, we will take the following steps for feature engineering:
#     1. Select only the numerical variables and two categorical variables (borough and property use type)
#     2. Add in the log transformation of the numerical variables
#     3. One-hot encode the categorical variables
#
# For feature selection, we will do the following:
#     1. Remove collinear features

# Copy the original data
features = data.copy()

# Select the numeric columns
numeric_subset = data.select_dtypes('number')

# Create columns with log of numeric columns
for col in numeric_subset.columns:
    # Skip the Energy Star Score column
    if col == 'score':
        next
    else:
        numeric_subset['log_' + col] = np.log(numeric_subset[col])
        
# Select the categorical columns
categorical_subset = data[['Borough', 'Largest Property Use Type']]

# One hot encode
categorical_subset = pd.get_dummies(categorical_subset)

# Join the two dataframes using concat
# Make sure to use axis = 1 to perform a column bind
features = pd.concat([numeric_subset, categorical_subset], axis = 1)

features.shape
#(11319, 110) - 109 features, 1 column is the score

# remove collinear features
plot_data = data[['Weather Normalized Site EUI (kBtu/ft²)', 'Site EUI (kBtu/ft²)']].dropna()

plt.plot(plot_data['Site EUI (kBtu/ft²)'], plot_data['Weather Normalized Site EUI (kBtu/ft²)'], 'bo')
plt.xlabel('Site EUI'); plt.ylabel('Weather Norm EUI')
plt.title('Weather Norm EUI vs Site EUI, R = %0.4f' % np.corrcoef(data[['Weather Normalized Site EUI (kBtu/ft²)', 'Site EUI (kBtu/ft²)']].dropna(), rowvar=False)[0][1]);

# While variables in a dataset are usually correlated to a small degree, 
# highly collinear variables can be redundant in the sense that we only need 
# to retain one of the features to give our model the necessary information.
# Removing collinear features is a method to reduce model complexity by 
# decreasing the number of features and can help to increase model generalization. 
# It can also help us to interpret the model because we only have to worry about 
# a single variable, such as EUI, rather than how both EUI and weather normalized 
# EUI affect the score.

def remove_collinear_features(x, threshold):
    '''
    Objective:
        Remove collinear features in a dataframe with a correlation coefficient
        greater than the threshold. Removing collinear features can help a model
        to generalize and improves the interpretability of the model.
        
    Inputs: 
        threshold: any features with correlations greater than this value are removed
    
    Output: 
        dataframe that contains only the non-highly-collinear features
    '''
    
    # Dont want to remove correlations between Energy Star Score
    y = x['score']
    x = x.drop(columns = ['score'])
    
    # Calculate the correlation matrix
    corr_matrix = x.corr()
    iters = range(len(corr_matrix.columns) - 1)
    drop_cols = []

    # Iterate through the correlation matrix and compare correlations
    for i in iters:
        for j in range(i):
            item = corr_matrix.iloc[j:(j+1), (i+1):(i+2)]
            col = item.columns
            row = item.index
            val = abs(item.values)
            
            # If correlation exceeds the threshold
            if val >= threshold:
                # Print the correlated features and the correlation value
                # print(col.values[0], "|", row.values[0], "|", round(val[0][0], 2))
                drop_cols.append(col.values[0])

    # Drop one of each pair of correlated columns
    drops = set(drop_cols)
    x = x.drop(columns = drops)
    x = x.drop(columns = ['Weather Normalized Site EUI (kBtu/ft²)', 
                          'Water Use (All Water Sources) (kgal)',
                          'log_Water Use (All Water Sources) (kgal)',
                          'Largest Property Use Type - Gross Floor Area (ft²)'])
    
    # Add the score back in to the data
    x['score'] = y
               
    return x

# Remove the collinear features above a specified correlation coefficient 
features = remove_collinear_features(features, 0.6);

# Remove any columns with all na values
features  = features.dropna(axis=1, how = 'all')
features.shape
# (11319, 65) - reduced to 64 features

# While a large number of features may be problematic for models such as 
# linear regression, models such as the random forest perform implicit 
# feature selection and automatically determine which features are important 
# during training

# There are plenty of more methods for feature selection. Some popular methods 
# include principal components analysis (PCA) which transforms the features 
# into a reduced number of dimensions that preserve the greatest variance, 
# or independent components analysis (ICA) which aims to find the independent 
# sources in a set of features. However, while these methods are effective at 
# reducing the number of features, they create new features that have no 
# physical meaning and hence make interpreting a model nearly impossible.


#-------------------------------------
# 4. Establish a baseline and compare several machine learning models 
#    on a performance metric
#-------------------------------------

# split into training and testing sets

# In machine learning, we always need to separate our features into two sets:

# 1. Training set which we provide to our model during training along with the 
# answers so it can learn a mapping between the features and the target

# 2. Testing set which we use to evaluate the mapping learned by the model. 
# The model has never seen the answers on the testing set, but instead, must 
# make predictions using only the features. As we know the true answers for 
# the test set, we can then compare the test predictions to the true test 
# targets to get an estimate of how well our model will perform when deployed 
# in the real world.

# For our problem, we will first extract all the buildings without an 
# Energy Star Score (we don't know the true answer for these buildings so 
# they will not be helpful for training or testing). Then, we will split 
# the buildings with an Energy Star Score into a testing set of 30% 
# of the buildings, and a training set of 70% of the buildings.

# Extract the buildings with no score and the buildings with a score
no_score = features[features['score'].isna()]
score = features[features['score'].notnull()]

print(no_score.shape)
print(score.shape)

# Separate out the features and targets
features = score.drop(columns='score')
targets = pd.DataFrame(score['score'])

# Replace the inf and -inf with nan (required for later imputation)
features = features.replace({np.inf: np.nan, -np.inf: np.nan})

# Split into 70% training and 30% testing set
X, X_test, y, y_test = train_test_split(features, targets, test_size = 0.3, random_state = 42)

print(X.shape)
print(X_test.shape)
print(y.shape)
print(y_test.shape)


# establish a baseline

# If the models we build cannot outperform a naive guess then we might have to 
# admit that machine learning is not suited for this problem. This could be 
# because we are not using the right models, because we need more data, 
# or because there is a simpler solution that does not require machine learning.

# For a regression task, a good naive baseline is to predict the median value 
# of the target on the training set for all examples on the test set. This is 
# simple to implement and sets a relatively low bar for our models: if they 
# cannot do better than guessing the median value, then we will need to 
# rethink our approach.

# In this case, because we doing regression, the mean absolute error is
# an appropriate metric. This is also interpretable because it represents 
# the average amount our estimate is off by in the same units as the target value

# Function to calculate mean absolute error
def mae(y_true, y_pred):
    return np.mean(abs(y_true - y_pred))

# Now we can make the median guess and evaluate it on the test set
baseline_guess = np.median(y)

print('The baseline guess is a score of %0.2f' % baseline_guess)
print("Baseline Performance on the test set: MAE = %0.4f" % mae(y_test, baseline_guess))
# The baseline guess is a score of 66.00
# Baseline Performance on the test set: MAE = 24.5164

# This shows our average estimate on the test set is off by about 25 points. 
# The scores are between 1 and 100 so this means the average error from a naive
# method if about 25%. The naive method of guessing the median training value
# provides us a low baseline for our models to beat!

# Overfitting may occur if too many features are used and do not help explain
# the test data - we say that the model has 'high variance'. Similarly 
# underfitting may occur if the model does not explain the train data - we say
# that the model has 'high bias'

# variance - measures consistency (or variability) of model prediction (sensitive to randomness)
# bias - measures error in predictions, and indicates systemic error (not due to randomness)

# regularization - is a useful method to handle collinearilty (amongst features), 
# filter out noise and eventually prevent overfitting. Regularlization works by
# introducing additional information (bias) to penalize extreme parameter weights. 
# Most common is L2-regularization. For regularization to work all numerical data
# must be normalized / standardized

# Save the no scores, training, and testing data
no_score.to_csv('data/no_score.csv', index = False)
X.to_csv('data/training_features.csv', index = False)
X_test.to_csv('data/testing_features.csv', index = False)
y.to_csv('data/training_labels.csv', index = False)
y_test.to_csv('data/testing_labels.csv', index = False)



# Read in data into dataframes 
train_features = pd.read_csv('data/training_features.csv')
test_features = pd.read_csv('data/testing_features.csv')
train_labels = pd.read_csv('data/training_labels.csv')
test_labels = pd.read_csv('data/testing_labels.csv')

# Display sizes of data
print('Training Feature Size: ', train_features.shape)
print('Testing Feature Size:  ', test_features.shape)
print('Training Labels Size:  ', train_labels.shape)
print('Testing Labels Size:   ', test_labels.shape)

# more complex methods for imputation (filling missing values): 
# http://www.stat.columbia.edu/~gelman/arm/missing.pdf
# https://www.tandfonline.com/doi/full/10.1080/1743727X.2014.979146

# Create an imputer object with a median filling strategy
imputer = Imputer(strategy='median')

# Train on the training features
imputer.fit(train_features)

# Transform both training data and testing data
X = imputer.transform(train_features)
X_test = imputer.transform(test_features)

print('Missing values in training features: ', np.sum(np.isnan(X)))
print('Missing values in testing features:  ', np.sum(np.isnan(X_test)))

# Make sure all values are finite
print(np.where(~np.isfinite(X)))
print(np.where(~np.isfinite(X_test)))


# scale features

# This is necessary because features are in different units, and we want to 
# normalize the features so the units do not affect the algorithm. Linear 
# Regression and Random Forest do not require feature scaling, but other 
# methods, such as support vector machines and k nearest neighbors, do 
# require it because they take into account the Euclidean distance between 
# observations. For this reason, it is a best practice to scale features 
# when we are comparing multiple algorithms.

# There are two ways to scale features:

#     1. For each value, subtract the mean of the feature and divide by the 
#     standard deviation of the feature. This is known as standardization and 
#     results in each feature having a mean of 0 and a standard deviation of 1

#     2. For each value, subtract the minimum value of the feature and divide 
#     by the maximum minus the minimum for the feature (the range). This assures 
#     that all the values for a feature are between 0 and 1 and is called 
#     scaling to a range or normalization

# As with imputation, when we train the scaling object, we want to use only 
# the training set. When we transform features, we will transform both the 
# training set and the testing set

# Create the scaler object with a range of 0-1
scaler = MinMaxScaler(feature_range=(0, 1))

# Fit on the training data
scaler.fit(X)

# Transform both the training and testing data
X = scaler.transform(X)
X_test = scaler.transform(X_test)

# Convert y to one-dimensional array (vector)
y = np.array(train_labels).reshape((-1, ))
y_test = np.array(test_labels).reshape((-1, ))


# models to evaluate (scikit-learn):
# 1. Linear Regression
# 2. Support Vector Machine Regression
# 3. Random Forest Regression
# 4. Gradient Boosting Regression
# 5. K-Nearest Neighbors Regression

# To compare the models, we are going to be mostly using the Scikit-Learn 
# defaults for the model hyperparameters. Generally these will perform 
# decently, but should be optimized before actually using a model. 
# At first, we just want to determine the baseline performance of each model, 
# and then we can select the best performing model for further optimization 
# using hyperparameter tuning. Remember that the default hyperparameters 
# will get a model up and running, but nearly always should be adjusted 
# using some sort of search to find the best settings for your problem!

# Takes in a model, trains the model, and evaluates the model on the test set
def fit_and_evaluate(model):
    
    # Train the model
    model.fit(X, y)
    
    # Make predictions and evalute
    model_pred = model.predict(X_test)
    model_mae = mae(y_test, model_pred) # mae, user-defined, to calc mean abs error
    
    # Return the performance metric
    return model_mae

lr = LinearRegression()
lr_mae = fit_and_evaluate(lr)
print('Linear Regression Performance on the test set: MAE = %0.4f' % lr_mae)
# more linear regression
model = lr
model.fit(X, y)
model.coef_
model.intercept_

rr = RANSACRegressor()
rr_mae = fit_and_evaluate(rr)
print('Robust Regression Performance on the test set: MAE = %0.4f' % rr_mae)

svm = SVR(C = 1000, gamma = 0.1)
svm_mae = fit_and_evaluate(svm)
print('Support Vector Machine Regression Performance on the test set: MAE = %0.4f' % svm_mae)

random_forest = RandomForestRegressor(random_state=60)
random_forest_mae = fit_and_evaluate(random_forest)
print('Random Forest Regression Performance on the test set: MAE = %0.4f' % random_forest_mae)

gradient_boosted = GradientBoostingRegressor(random_state=60)
gradient_boosted_mae = fit_and_evaluate(gradient_boosted)
print('Gradient Boosted Regression Performance on the test set: MAE = %0.4f' % gradient_boosted_mae)

knn = KNeighborsRegressor(n_neighbors=10)
knn_mae = fit_and_evaluate(knn)
print('K-Nearest Neighbors Regression Performance on the test set: MAE = %0.4f' % knn_mae)


plt.style.use('fivethirtyeight')
figsize(8, 6)

# Dataframe to hold the results
model_comparison = pd.DataFrame({'model': ['Linear Regression', 'Robust Regression', 
                                           'Support Vector Machine',
                                           'Random Forest', 'Gradient Boosted',
                                            'K-Nearest Neighbors'],
                                 'mae': [lr_mae, rr_mae, svm_mae, random_forest_mae, 
                                         gradient_boosted_mae, knn_mae]})

# Horizontal bar chart of test mae
model_comparison.sort_values('mae', ascending = False).plot(x = 'model', y = 'mae', kind = 'barh',
                                                           color = 'red', edgecolor = 'black')

# Plot formatting
plt.ylabel(''); plt.yticks(size = 14); plt.xlabel('Mean Absolute Error'); plt.xticks(size = 14)
plt.title('Model Comparison on Test MAE', size = 20);

# Depending on the run (the exact results change slighty each time), the gradient 
# boosting regressor performs the best followed by the random forest. I have 
# to admit that this is not the most fair comparison because we are using 
# mostly the default hyperparameters. Especially with the Support Vector 
# Regressor, the hyperparameters have a significant influence on performance. 
# (the random forest and gradient boosting methods are great for starting out
# because the performance is less dependent on the model settings). 
# Nonetheless, from these results, we can conclude that machine learning 
# is applicable because all the models significantly outperform the baseline!

# From here, I am going to concentrate on optimizing the best model using 
# hyperparamter tuning. Given the results here, I will concentrate on using 
# the GradientBoostingRegressor. 


#-------------------------------------
# 5. Perform hyperparameter tuning on the best model to optimize
#    it for the problem
#-------------------------------------

# Hyperparameters vs Model parameters:

# Model hyperparameters: are best thought of as settings for a machine learning 
# algorithm that are tuned by the data scientist before training. Examples 
# would be the number of trees in the random forest, or the number of neighbors 
# used in K Nearest Neighbors Regression.

# Model parameters: are what the model learns during training, such as the 
# weights in the linear regression.

# Tuning the model hyperparameters controls the balance of under vs over fitting
# in a model.

# We can try to correct for under-fitting by making a more complex
# model, such as using more trees in a random forest or more layers in a deep
# neural network. A model that underfits has high bias, and occurs when our
# model does not have enough capacity (degrees of freedom) to learn the
# relationship between the features and the target.

# We can try to correct for overfitting by limiting the complexity of the
# model and applying regularization (introducing noise). This might mean
# decreasing the degree of a polynomial regression, or adding dropout layers
# to a deep neural network. A model that overfits has high variance and
# in effect has memorized the training set. Both underfitting and overfitting
# lead to poor generalization performance on the test set.

# We can choose the best hyperparameters for a model through:
#     1. random search and 
#     2. cross validation

# 1. Random search refers to the method in which we choose hyperparameters to 
#    evaluate: we define a range of options, and then randomly select 
#    combinations to try. This is in contrast to grid search which evaluates
#    every single combination we specify. Generally, random search is better 
#    when we have limited knowledge of the best model hyperparameters and we 
#    can use random search to narrow down the options and then use grid search 
#    with a more limited range of options.

# 2. Cross validation is the method used to assess the performance of the
#    hyperparameters. Rather than splitting the training set up into separate 
#    training and validation sets which reduces the amount of training data 
#    we can use, we use K-Fold Cross Validation. This means dividing the 
#    training data into K folds, and then going through an iterative process 
#    where we first train on K-1 of the folds and then evaluate performance 
#    on the kth fold. We repeat this process K times so eventually we will 
#    have tested on every example in the training data with the key that 
#    each iteration we are testing on data that we did not train on. At the 
#    end of K-fold cross validation, we take the average error on each of 
#    the K iterations as the final performance measure and then train the 
#    model on all the training data at once. The performance we record is 
#    then used to compare different combinations of hyperparameters.

# Here we will implement random search with cross validation to select the 
# optimal hyperparameters for the gradient boosting regressor. We first define 
# a grid then peform an iterative process of: randomly sample a set of 
# hyperparameters from the grid, evaluate the hyperparameters using 4-fold 
# cross-validation, and then select the hyperparameters with the best performance.

# The entire process of performing random search with cross validation is:
#    1. Set up a grid of hyperparameters to evaluate
#    2. Randomly sample a combination of hyperparameters
#    3. Create a model with the selected combination
#    4. Evaluate the model using K-fold cross validation
#    5. Decide which hyperparameters worked the best

# Of course we don't actually do this iteration ourselves, we let Scikit-Learn 
# and RandomizedSearchCV do the process for us!

# Loss function to be optimized
loss = ['ls', 'lad', 'huber']

# Number of trees used in the boosting process
n_estimators = [100, 500, 900, 1100, 1500]

# Maximum depth of each tree
max_depth = [2, 3, 5, 10, 15]

# Minimum number of samples per leaf
min_samples_leaf = [1, 2, 4, 6, 8]

# Minimum number of samples to split a node
min_samples_split = [2, 4, 6, 10]

# Maximum number of features to consider for making splits
max_features = ['auto', 'sqrt', 'log2', None]

# Define the grid of hyperparameters to search
# We selected 6 different hyperparameters to tune in the gradient boosting regressor.
hyperparameter_grid = {'loss': loss,
                       'n_estimators': n_estimators,
                       'max_depth': max_depth,
                       'min_samples_leaf': min_samples_leaf,
                       'min_samples_split': min_samples_split,
                       'max_features': max_features}

# In the code below, we create the Randomized Search Object passing in the 
# following parameters:
#    estimator: the model
#    param_distributions: the distribution of parameters we defined
#    cv: the number of folds to use for k-fold cross validation
#    n_iter: the number of different combinations to try
#    scoring: which metric to use when evaluating candidates
#    n_jobs: number of cores to run in parallel (-1 will use all available)
#    verbose: how much information to display (1 displays a limited amount)
#    return_train_score: return the training score for each cross-validation fold
#    random_state: fixes the random number generator used so we get the same results every run

# The Randomized Search Object is trained the same way as any other 
# scikit-learn model. After training, we can compare all the different
# hyperparameter combinations and find the best performing one.

# Bagging vs Boosting
# The Gradient Boosted Regression model is an ensemble method, meaning that 
# it is built out of many weak learners, in this case individual decision trees. 
# While a bagging algorithm such as random forest trains the weak learners 
# in parallel and has them vote to make a prediction, a boosting method like 
# Gradient Boosting, trains the learners in sequence, with each learner 
# “concentrating” on the mistakes made by the previous ones.

# Create the model to use for hyperparameter tuning
model = GradientBoostingRegressor(random_state = 42)

# Set up the random search with 4-fold cross validation
random_cv = RandomizedSearchCV(estimator=model,
                               param_distributions=hyperparameter_grid,
                               cv=4, n_iter=25, 
                               scoring = 'neg_mean_absolute_error',
                               n_jobs = -1, verbose = 1, 
                               return_train_score = True,
                               random_state=42)

# Fit on the training data
random_cv.fit(X, y)

# Scikit-learn uses the negative mean absolute error for evaluation because
# it wants a metric to maximize. Therefore, a better score will be closer to 0. 
# We can get the results of the randomized search into a dataframe, and 
# sort the values by performance

# Get all of the cv results and sort by the test performance
random_results = pd.DataFrame(random_cv.cv_results_).sort_values('mean_test_score', ascending = False)

random_results.head(10)

random_cv.best_estimator_

# The best gradient boosted model has the following hyperparameters:
#    loss = lad
#    n_estimators = 500
#    max_depth = 5
#    min_samples_leaf = 6
#    min_samples_split = 6
#    max_features = None (This means that max_features = n_features according to the docs)

# Using random search is a good method to narrow down the possible hyperparameters
# to try. Initially, we had no idea which combination would work the best, but
# this at least narrows down the range of options.

# We could use the random search results to inform a grid search by creating
# a grid with hyperparameters close to those that worked best during the
# randomized search. However, rather than evaluating all of these settings
# again, I will focus on a single one, the number of trees in the forest
# (n_estimators). By varying only one hyperparameter, we can directly observe
# how it affects performance. In the case of the number of trees, we would
# expect to see a significant affect on the amount of under vs overfitting.

# Here we will use grid search with a grid that only has the n_estimators
# hyperparameter. We will evaluate a range of trees then plot the training
# and testing performance to get an idea of what increasing the number of trees
# does for our model. We will fix the other hyperparameters at the best values
# returned from random search to isolate the number of trees effect.

# Create a range of trees to evaluate
trees_grid = {'n_estimators': [100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800]}

model = GradientBoostingRegressor(loss = 'lad', max_depth = 5,
                                  min_samples_leaf = 6,
                                  min_samples_split = 6,
                                  max_features = None,
                                  random_state = 42)

# Grid Search Object using the trees range and the random forest model
grid_search = GridSearchCV(estimator = model, param_grid=trees_grid, cv = 4, 
                           scoring = 'neg_mean_absolute_error', verbose = 1,
                           n_jobs = -1, return_train_score = True)

# Fit the grid search
grid_search.fit(X, y)

# Get the results into a dataframe
results = pd.DataFrame(grid_search.cv_results_)

# Plot the training and testing error vs number of trees
figsize(8, 8)
plt.style.use('fivethirtyeight')
plt.plot(results['param_n_estimators'], -1 * results['mean_test_score'], label = 'Testing Error')
plt.plot(results['param_n_estimators'], -1 * results['mean_train_score'], label = 'Training Error')
plt.xlabel('Number of Trees'); plt.ylabel('Mean Abosolute Error'); plt.legend();
plt.title('Performance vs Number of Trees');

results.sort_values('mean_test_score', ascending = False).head(5)

# From this plot, it's pretty clear that our model is overfitting! The training 
# error is significantly lower than the testing error, which shows that the model 
# is learning the training data very well but then is not able to generalize to 
# the test data as well. Moveover, as the number of trees increases, the amount 
# of overfitting increases. Both the test and training error decrease as the 
# number of trees increase but the training error decreases more rapidly.
#
# There will always be a difference between the training error and testing error 
# (the training error is always lower) but if there is a significant difference, 
# we want to try and reduce overfitting, either by getting more training data
# or reducing the complexity of the model through hyperparameter tuning
# or regularization. For the gradient boosting regressor, some options 
# include reducing the number of trees, reducing the max depth of each tree,
# and increasing the minimum number of samples in a leaf node. 
#
# Based on the cross validation results, the best model using 800 trees
# and achieves a cross validation error under 9. This indicates that the
# average cross-validation estimate of the Energy Star Score is within
# 9 points of the true answer!


#-------------------------------------
# 6. Evaluate the best model on the testing set
#-------------------------------------

# Default model - for comparison
default_model = GradientBoostingRegressor(random_state = 42)

# Select the best model
final_model = grid_search.best_estimator_

final_model

%%timeit -n 1 -r 5
default_model.fit(X, y)

%%timeit -n 1 -r 5
final_model.fit(X, y)

default_pred = default_model.predict(X_test)
final_pred = final_model.predict(X_test)

print('Default model performance on the test set: MAE = %0.4f.' % mae(y_test, default_pred))
print('Final model performance on the test set:   MAE = %0.4f.' % mae(y_test, final_pred))

# The final model does out-perform the baseline model by about 10%, but at 
# the cost of significantly increased running time (it's about 12 times slower 
# on my machine). 

# Machine learning is often a field of tradeoffs: bias vs variance, 
# acccuracy vs interpretability, accuracy vs running time, and the final 
# decision of which model to use depends on the situation. Here, the increase 
# in run time is not an impediment, because while the relative difference is 
# large, the absolute magnitude of the training time is not significant. 
# In a different situation, the balance might not be the same so we would need 
# to consider what we are optimizing for and the limitations we have to work with.

# As a general rule, proper feature engineering will have a much larger impact
# on model performance than even the most extensive hyperparameter tuning. 
# It’s the law of diminishing returns applied to machine learning: feature 
# engineering gets you most of the way there, and hyperparameter tuning 
# generally only provides a small benefit.
                                              
figsize(8, 8)

# Density plot of the final predictions and the test values
sns.kdeplot(final_pred, label = 'Predictions')
sns.kdeplot(y_test, label = 'Values')

# Label the plot
plt.xlabel('Energy Star Score'); plt.ylabel('Density');
plt.title('Test Values and Predictions');

# It appears the model might be less accurate at predicting the extreme values 
# and instead predicts values closer to the median.

# Another diagnostic plot is a histogram of the residuals. Ideally, we would 
# hope that the residuals are normally distributed, meaning that the model 
# is wrong the same amount in both directions (high and low).

figsize = (6, 6)

# Calculate the residuals 
residuals = final_pred - y_test

# Plot the residuals in a histogram
plt.hist(residuals, color = 'red', bins = 20,
         edgecolor = 'black')
plt.xlabel('Error'); plt.ylabel('Count')
plt.title('Distribution of Residuals');

# The residuals are close to normally disributed, with a few noticeable 
# outliers on the low end. These indicate errors where the model estimate
# was far below that of the true value.



#-------------------------------------
# 7. Interpret the model results to the extent possible
#-------------------------------------

# Read in data into dataframes
train_features = pd.read_csv('data/training_features.csv')
test_features = pd.read_csv('data/testing_features.csv')
train_labels = pd.read_csv('data/training_labels.csv')
test_labels = pd.read_csv('data/testing_labels.csv')

# Create an imputer object with a median filling strategy
imputer = Imputer(strategy='median')

# Train on the training features
imputer.fit(train_features)

# Transform both training data and testing data
X = imputer.transform(train_features)
X_test = imputer.transform(test_features)

# Sklearn wants the labels as one-dimensional vectors
y = np.array(train_labels).reshape((-1,))
y_test = np.array(test_labels).reshape((-1,))

model = GradientBoostingRegressor(loss='lad', max_depth=5, max_features=None,
                                  min_samples_leaf=6, min_samples_split=6, 
                                  n_estimators=800, random_state=42)

model.fit(X, y)

#  Make predictions on the test set
model_pred = model.predict(X_test)

print('Final Model Performance on the test set: MAE = %0.4f' % mae(y_test, model_pred))


# We will explore several ways to interpret our model:
#     1. Feature importances
#     2. Locally Interpretable Model-agnostic Explainer (LIME)
#     3. Examining a single decision tree in the ensemble


# 1. Feature importances
# ------------------------

# One of the basic ways we can interpret an ensemble of decision trees is 
# through what are known as the feature importances. These can be interpreted 
# as the variables which are most predictive of the target. We can use the 
# relative values to compare the features and determine which are most 
# relevant to our problem.
# good discussion: 
# https://stackoverflow.com/questions/15810339/how-are-feature-importances-in-randomforestclassifier-determined


# Extract the feature importances into a dataframe# Extra 
feature_results = pd.DataFrame({'feature': list(train_features.columns), 
                                'importance': model.feature_importances_})

# Show the top 10 most important
feature_results = feature_results.sort_values('importance', ascending = False).reset_index(drop=True)

feature_results.head(10)

figsize = (12, 10)
plt.style.use('fivethirtyeight')

# Plot the 10 most important features in a horizontal bar chart
feature_results.loc[:9, :].plot(x = 'feature', y = 'importance', 
                                 edgecolor = 'k',
                                 kind='barh', color = 'blue');
plt.xlabel('Relative Importance', size = 20); plt.ylabel('')
plt.title('Feature Importances from Random Forest', size = 30);

# The Site Energy Use Intensity, Site EUI (kBtu/ft²), and the Weather 
# Normalized Site Electricity Intensity, Weather Normalized Site Electricity 
# Intensity (kWh/ft²) are the two most important features by quite a large margin.

# Use Feature Importances for Feature Selection

# Given that not every feature is important for finding the score, what would 
# happen if we used a simpler model, such as a linear regression, with the 
# subset of most important features from the random forest? The linear 
# regression did outperform the baseline, but it did not perform well compared 
# to the model complex models. Let's try using only the 10 most important 
# features in the linear regression to see if performance is improved. We 
# can also limit to these features and re-evaluate the random forest.

# Extract the names of the most important features
most_important_features = feature_results['feature'][:10]

# Find the index that corresponds to each feature name
indices = [list(train_features.columns).index(x) for x in most_important_features]

# Keep only the most important features
X_reduced = X[:, indices]
X_test_reduced = X_test[:, indices]

print('Most important training features shape: ', X_reduced.shape)
print('Most important testing  features shape: ', X_test_reduced.shape)
 
lr = LinearRegression()

# Fit on full set of features
lr.fit(X, y)
lr_full_pred = lr.predict(X_test)

# Fit on reduced set of features
lr.fit(X_reduced, y)
lr_reduced_pred = lr.predict(X_test_reduced)

# Display results
print('Linear Regression Full Results: MAE =    %0.4f.' % mae(y_test, lr_full_pred))
print('Linear Regression Reduced Results: MAE = %0.4f.' % mae(y_test, lr_reduced_pred))

# Well, reducing the features did not improve the linear regression results! 
# It turns out that the extra information in the features with low importance 
# do actually improve performance.
# Let's look at using the reduced set of features in the
# gradient boosted regressor.

# Create the model with the same hyperparamters
model_reduced = GradientBoostingRegressor(loss='lad', max_depth=5, max_features=None,
                                  min_samples_leaf=6, min_samples_split=6, 
                                  n_estimators=800, random_state=42)

# Fit and test on the reduced set of features
model_reduced.fit(X_reduced, y)
model_reduced_pred = model_reduced.predict(X_test_reduced)

print('Gradient Boosted Reduced Results: MAE = %0.4f' % mae(y_test, model_reduced_pred))

# The model results are slightly worse with the reduced set of features and 
# we will keep all of the features for the final model. The desire to reduce 
# the number of features is because we are always looking to build the most 
# parsimonious model: that is, the simplest model with adequate performance. 
# A model that uses fewer features will be faster to train and generally 
# easier to interpret. In this case, keeping all of the features is not a major 
# concern because the training time is not significant and we can still make 
# interpretations with many features.


# 2. Locally Interpretable Model-agnostic Explanations (LIME)
# ------------------------------------------------------------

# LIME shows how a machine learning model thinks by approximating the 
# region around a prediction with a linear model.

# Find the residuals# Find t 
residuals = abs(model_reduced_pred - y_test)
    
# Exact the worst and best prediction
wrong = X_test_reduced[np.argmax(residuals), :]
right = X_test_reduced[np.argmin(residuals), :]

# Create a lime explainer object
explainer = lime.lime_tabular.LimeTabularExplainer(training_data = X_reduced, 
                                                   mode = 'regression',
                                                   training_labels = y,
                                                   feature_names = list(most_important_features))

# Display the predicted and true value for the wrong instance
print('Prediction: %0.4f' % model_reduced.predict(wrong.reshape(1, -1)))
print('Actual Value: %0.4f' % y_test[np.argmax(residuals)])


# Explanation for wrong prediction
wrong_exp = explainer.explain_instance(data_row = wrong, 
                                       predict_fn = model_reduced.predict)

# Plot the prediction explaination
wrong_exp.as_pyplot_figure();
plt.title('Explanation of Prediction', size = 28);
plt.xlabel('Effect on Prediction', size = 22);

# In this example, our gradient boosted model predicted a score of 12.86 and 
# the actual value was 100.

# The plot from LIME is showing us the contribution to the final prediction
# from each of the features for the example. We can see that the Site EUI
# significantly decreased the prediction because it was above 95.50. 
# The Weather Normalized Site Electricity Intensity on the other hand, 
# increased the prediction because it was lower than 3.80.

# We can interpret this as saying that our model thought the Energy Star Score 
# would be much lower than it actually was because the Site EUI was high. 
# However, in this case, the score was 100 despite the high value of the EUI. 
# While this significant mistake (off by 88 points!) might initially have been
# confusing, now we can see that in reality, the model was reasoning through
# the problem and just arrived at the incorrect value! A human going over the
# same process probably would have arrived at the same conclusion (if they
# had the patience to go through all the data).


# Now we can go through the same process with a prediction the model got correct.
# Display the predicted and true value for the correct instance
print('Prediction: %0.4f' % model_reduced.predict(right.reshape(1, -1)))
print('Actual Value: %0.4f' % y_test[np.argmin(residuals)])

# Explanation for correct prediction
right_exp = explainer.explain_instance(right, model_reduced.predict, num_features=10)
right_exp.as_pyplot_figure();
plt.title('Explanation of Prediction', size = 28);
plt.xlabel('Effect on Prediction', size = 22);

# The correct value for this case was 100 which our gradient boosted model got right on!

# The plot from LIME again shows the contribution to the prediciton of each of
# feature variables for the example. For instance, because the Site EUI was
# less than 62.70, that contributed significantly to a higher estimate of the
# score. Likewise, the year built being less than 1927 also positively contributed
# to the final prediction.

# Observing break down plots like these allow us to get an idea of how the
# model makes a prediction. This is probably most valuable for cases where the
# model is off by a large amount as we can inspect the errors and perhaps
# engineer better features or adjust the hyperparameters of the model
# to improve predictions for next time. The examples where the model is off
# the most could also be interesting edge cases to look at manually. The model
# drastically underestimated the Energy Star Score for the first building
# because of the elevated Site EUI. We might therefore want to ask why the
# building has such a high Energy Star Score even though it has such a high EUI. 

# A process such as this where we try to work with the machine learning algorithm
# to gain understanding of a problem seems much better than simply letting
# the model make predictions and completely trusting them! Although
# LIME is not perfect, it represents a step in the right direction towards
# explaining machine learning models.


# 3. Examining a Single Decision Tree
# ----------------------------------
# Extract a single tree
single_tree = model_reduced.estimators_[105][0]

tree.export_graphviz(single_tree, out_file = 'images/tree.dot',
                     rounded = True, 
                     feature_names = most_important_features,
                     filled = True)

single_tree

# Convert to a png from the command line
# This requires the graphviz visualization library (https://www.graphviz.org/)
# !dot -Tpng images/tree.dot -o images/tree.png

# limit the max depth to improve readability 
tree.export_graphviz(single_tree, out_file = 'images/tree_small.dot',
                     rounded = True, feature_names = most_important_features,
                     filled = True, max_depth = 3)


#-------------------------------------
# 8. Draw conclusions and document work
#-------------------------------------

