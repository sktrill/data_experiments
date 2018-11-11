# -*- coding: utf-8 -*-
"""
===============================================================
REFERENCE - MACHINE LEARNING CODEBOOK
===============================================================
- Swapnil Kotecha

ML Workflow
---------------------------
1. Data cleaning and formatting
        - missing values
        - column splits
        - untidy data
        - outliers
        - duplicates
        - data type
        - anonymize sensitive data

2. Exploratory data analysis
        - summary stats, mean, variance, kurtosis, skew, 
        - domain outliers, boxplot
        - histograms, get a sense of distributions, pmf / pdf, cdf
        - correlations (auto correlation for time series)
        - confirm domain relationships
        - understand natural resolution of dataset
        - check class imbalance

3. Feature engineering and selection/extraction
        - dimensionality reduction (if reqd):
            - pca, ica (pca is svd of covariance matrix)
            - applying dimensioninality reduction method allows for a check on which
                features are most important (best explain the variance)
            - rfe (recursive feature eliminiation): uses model accuracy to 
                identify which attributes (and combination of attributes) 
                contribute the most to predicting the target attribute
        - feature selection:
            - use Lasso regression to determine feature importance (for LR)
            - fill in missing values (zero, mean, median etc) or drop observations
            - remove outliers
        - feature engineering:
            - create aggregrates / composites
            - decompose categorical, date/time data
            - add transformations (log, sqrt etc)
        - feature scaling:
            - standardize / normalize features

4. Establish a baseline and compare several machine learning models 
on a performance metric
        - split data into train and test sets 
            - stratify = y if classes are imbalanced
            - curse of dimensionality, more dimensions = higher risk of overfitting
        - encode categorical and/or text data:
            - use getdummies (pandas) or OneHotEncoder, LabelEncoder (scikit)
        - regularization: 
            - may want to use regularized terms during training (and non-regularized
                during for the test set)
                - for regression:
                    - Lasso - L1 norm, good to find features
                    - Ridge - L2 norm, good when not too many outliers
                    - Elastic Net
                - for trees:
                    - max depth - reduce depth to prevent overfitting
                    - others: min_samples_leaf, max_features, n_estimators etc.
            - early stopping: stops when error on validation set starts increasing
                hence prevents overfitting
        - evaluation metrics:
            - prediction/regression: mean sqd error (RMSE), mean absolute error (MAE)
                - RMSE (L2 norm - not good it too many outliers)
            - classification: accuracy, logloss, area under ROC, percision
                recall, F1-score, also percision-recall curves and ROC curves
                - Error rate = (FP + FN) / (FP + FN + TP + TN)
                - Accuracy = (TP + TN) / (FP + FN + TP + TN)
                - Sensitivity (true positive rate / recall) = TP / (TP + FN) = TP / P
                - Specificity (true negative rate) = TN / (TN + FP) = TN / N
                - Percision = TP / (TP + FP)
                - F1 = harmonic mean of percision and recall
                - ROC curve plots True Positive Rate vs False Positive Rate
            - tips:
                - perfect classifier will have AUC ROC = 1, random clsfr. = 0.5
                - use PR curve (over ROC) when positive class is rare or when you
                    care more about the false positives than the false negatives
                - for multiclass classification use confusion matrix
                - if dataset has imbalanced classes - look for early retrieval area
                    to analyse the performance with small false positive rate. 
                    Imbalanced classes are better analyzed via percision-recall curves
                - 
        - compare models on evaluation metrics and pick 2-3 most promising

5. Perform hyperparameter tuning on the best model using cross-validation
        - use randomized search to find ballpark set of hyperparameters and then
          use grid search to fine tune one or two hyperparameters within the set
        - k-fold cross validate all tuning to reduce training data variance
        - nested cross validation folds are useful for large data sets where
          computational performance is important (use k-fold to pick model
          and then a 2-fold, validation and training fold, to tune picked model)
        - stratified k-fold cv is useful for classification as it retains class
          proportions through all k cross validation folds
        - calculate confusion matrix (see false positives and true negatives)        
        
6. Evaluate the best model on the testing set
        
7. Interpret the model results to the extent possible
        1. feature importances
        2. Locally Interpretable Model-agnostic Explainer (LIME)
        3. examine a single decision tree in the ensemble


Common Learning Methods
---------------------------
    Supervised:
        - kNN
        - Linear Regression
        - Logistic Regression
        - SVM
        - Decision Trees and Random Forests
        - Neural Networks
    
    Unsupervised:
        - Clustering: 
            - K Means
            - Hierarchical Cluster Analysi
            - Expectation Maximization
        - Visualization and dimensionality reduction
            - PCA
            - Kernal PCA
            - t-distribued Stochastic Neighbour Embedding (t-SNE)
        - Association rule learning
            - Apriori
            - Eclat
    
    Also: Deep Belief Networks, Restrited Boltzmann Machines, RNN, CNN, LTSM    
    + Reinforcement learning
        

Data Engineering
---------------------------
    Data Modelling:
    - data tables should be normalized (have simpler schemas, 
        standardized data and carry less redundancy) - but smaller
        tables means tracking data relations, more joins nad more ETL pipelines
    - querying denomalized tables (aka wide table) is easier to query
        but data processing is slower and ETL pipelines difficult to maintain
    - trade-off: Star Schema
    - the star schema organized table in a star-like pattern, with a fact 
        table at the center, surrounded by dim tables
    - fact table: point in time transactional data - each row is a transaction
        e.g. bookings, reservations, cancellations
    - dimension table: slowly changing attributes and can be hierarchal
        e.g. users, listings, markets
    - design focuses on building normalized tables: fact and dimension tables, 
        when needed denormalized tables can be built from smaller normalized tables

    Data Partitioning: 
    - more effective technique to improve query performance
    - data partitioning breaks data into independent, self contained chunks
    - data from the same chunk share partition keys
    - common partition key is  'datestamp' - common analytic ref, ETLs often
        run daily, backfilling also based on datestamp
    - data backfilling: to compute metrics and dimenions in the past
        e.g. Hive allows for dynamic partitions
    - caching also helps speed up queries (esp. in-memory like Redis, Memcache)
    - the most straightforward strategy is least recently used. LRU works 
        by evicting less commonly used data in preference of more frequently 
        used data, and is almost always an appropriate caching strategy
    
    ETL:
    - Airflow uses DAGs (Directed Acyclic Graphs), DAGs are made up of Operators
    - DAG defines the sequence and schedule of operations, 
        the Operators define discrete tasks to take place within this sequence
    - DAGs describe how to run a data pipeline, operators describe 
        what to do in a data pipeline, and come in 3 categories:
        1. sensors: waits for a certain time, external file or upstream source
        2. operators: triggers action e.g. execute py function, Hive query
        3. transfers: move data from one location to another
    - ETL best practices: 
        1. partition data
        2. load data incrementally - only add what's new, use partition key
        3. enforce idempotency - pipeline should be built so that the same 
            query, when run against the same business logic and time 
            range, returns the same result
        4. parameterize workflow - use Jinja
        5. add data checks early and often - on staging table
        6. build alerts and monitoring system
        7. configuration as code
        8. create workflow frameworks (evenutally)
    
    SQL:
    - filter early and often
    - index most used columns
    - use 'explain'
    - reduce joins, subqueries, aggregrate, conditions on 'having' etc.


"""
# ---------------------------------
# COMMON EXPRESSIONS
# ---------------------------------

# For EDA
# ---------------------------------

# commons
head(), dtypes, describe(), corr(), shape, info()
groupby(col), 
apply(fcn)

# value_counts() method on Pandas series
preg.birthord.value_counts().sort_index()

# include null values
preg.birthord.value_counts(dropna = False, sort = True)

# count just the nulls
preg.birthord.isnull().sum()

# calculate sample with replacement
sample = np.random.choice(df, 100, replace = "True")

# isinstance can take tuples
a = 4
b = 4.5
c = 's'
print(isinstance(a,(int, float)))
print(isinstance(b,(int, float)))
print(isinstance(c,(int, float)))

# isiterable - check if unknown object is iterable
a = [1,2,4,5]
b = 5
c = 'a string'

def isiterable(obj):
        try:
            iter(obj)
            return True
        except TypeError: # not iterable
            return False

print(isiterable(a))
print(isiterable(b))
print(isiterable(c))

# get variable info
b = [i for i in range(0, 10, 2)]
b?

# get function info
def add_two(a, b):
    """
    adds two numbers
    """
    return a + b
add_two?

# get function info - with function code
add_two??

# time it
%timeit add_two(1, 2)

# turn auto magic on/off - for ipython
%automagic

# activate interactive debugger
%debug?

# for matplotlib - to plot inline
%matplotlib inline

import matplotlib.pyplot as plt
import numpy as np
plt.plot(np.random.randn(50).cumsum())

# to set figsize globally
plt.rc('figure', figsize=(10, 6))
np.set_printoptions(precision=4, suppress=True)
plt.plot(np.random.randn(50).cumsum())

# generate some random data in a 2 x 3 matrix
data = np.random.randn(2, 3)
data

# creating an np array
data1 = [6, 7.5, 8, 0, 1]
arr1 = np.array(data1)
arr1

# some common methods
arr1.ndim
arr1.shape
arr1.dtype

# create arrays of zeros
np.zeros(10)
np.zeros((2,3))
np.ones(5)

# like range but array based
np.arange(10)

# transpose of matrix
data
data.T
data.transpose()

# inner/dot product
np.dot(data, data.T)

# common np functions
arr = np.arange(10)
arr
np.sqrt(arr)
np.exp(arr)

x = np.random.randn(7)
y = np.random.randn(7)
np.maximum(x, y)
np.minimum(x, y)

# conditional logic in ndarrays
xarr = np.array([1.1, 1.2, 1.3, 1.4, 1.5])
yarr = np.array([2.1, 2.2, 2.3, 2.4, 2.5])
cond = np.array([True, False, True, True, False])
result = [(x if c else y)
          for x, y, c in zip(xarr, yarr, cond)]
result
# or equivalently, 
result = np.where(cond, xarr, yarr)
result

# common statistical methods
arr = np.random.randn(5, 4)
arr
arr.mean()
np.mean(arr)
arr.sum()

arr.mean(axis=1) # column wise
arr.sum(axis=0) # row wise

arr.cumsum()
arr.cumprod()
arr.std()
arr.var()

# .any, .all for boolean
bools = np.array([False, False, True, False])
bools.any() # check if any true
bools.all() # check if all true

# get unique values
names = np.array(['Bob', 'Joe', 'Will', 'Bob', 'Will', 'Joe', 'Joe'])
np.unique(names)
# or equivalently, 
sorted(set(names))

# get rid of duplicates, drop na, fill missing values
df = pd.read_csv('...')
df.drop_duplicates()
df.dropna()
df.column.fillna(df.column.mean())
df.column.notnull().all() # column does not contain any missing values

