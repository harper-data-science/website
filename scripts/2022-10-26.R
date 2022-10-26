## HEADER ####
## who: Ed
## what: HARUG {mlr3} intro
## when: 2022-10-26

## CONTENTS ####
## 00 Setup
## 01 Tasks

## 00 Setup ####
# Ed work PC
setwd(r'(D:\Dropbox\git-harper-data-science\website\scripts)')

# install.packages("mlr3")
# install.packages("mlr3viz")
# install.packages("mlr3proba")
install.packages("mlr3learners")
install.packages("mlr3extralearners")
install.packages("mlr3proba")  
install.packages("mlr3cluster")

library(mlr3)
library(mlr3viz)
library(mlr3proba)
requireNamespace("mlr3proba", quietly = TRUE)

library(mlr3learners)       # load recommended learners provided by mlr3learners package
library(mlr3extralearners)  # this loads further less-well-supported learners
library(mlr3proba)          # this loads some survival and density estimation learners
library(mlr3cluster)        # this loads some learners for clustering




## 01 Tasks ####

# Tasks "contain" data and other info in mlr3
# e.g. TaskClassif, TaskRegr

### 1.1 Data ####
data("mtcars", package = "datasets")
data = mtcars[, 1:3]
str(data)


### 1.2 create task ####
task_mtcars = as_task_regr(data, target = "mpg", id = "cars")
print(task_mtcars)

# viz summary task
mlr3viz::autoplot(task_mtcars, type = 'pairs')

### 1.3  Built-in example Tasks
mlr_tasks
as.data.table(mlr_tasks)


# example penguins 
task_penguins = tsk("penguins")
print(task_penguins)
mlr3viz::autoplot(task_penguins, type = 'pairs')


### 1.3 Task descriptives ####
task_mtcars$nrow
task_mtcars$feature_names #nb feature terminology
task_mtcars$target_names

# The data contained in a task can be accessed through $data()
task_mtcars$data() # footer nice touch!

# show summary of entire data
summary(as.data.table(task_mtcars))

### 1.4 mutators ####
# these manupulate task objects with min syntax
task_penguins_small = tsk("penguins")
task_penguins_small$select(c("body_mass", "flipper_length")) # keep only these features
task_penguins_small$filter(2:4) # keep only these rows
task_penguins_small$data()

### 1.5 Plotting
library("mlr3viz")

# get the pima indians task
task = tsk("pima")

# subset task to only use the 3 first features
task$select(head(task$feature_names, 3))

# default plot: class frequencies
autoplot(task)

# pairs plot (requires package GGally)
autoplot(task, type = "pairs")

# duo plot (requires package GGally)
autoplot(task, type = "duo")

## 2 Learners ####

# Objects of class Learner provide a unified interface 
# to many popular machine learning algorithms in R. 
# They consist of methods to train and predict a model 
# for a Task and provide meta-information about the 
# learners, such as the hyperparameters (which control 
# the behavior of the learner) you can set.

## Test :: Train paradigm

# classif.featureless: Simple baseline classification learner. 
# The default is to always predict the label that is most 
# frequent in the training set. While this is not very useful 
# by itself, it can be used as a “fallback learner” to 
# make predictions in case another, more sophisticated, 
# learner failed for some reason.
# 
# 
# regr.featureless: Simple baseline regression learner. 
# The default is to always predict the mean of the target 
# in training set. Similar to mlr_learners_classif.featureless, 
# it makes for a good “fallback learner”

# There are a lot of learners
mlr_learners

# get a learner
learner = lrn("classif.rpart")
learner
learner$param_set

# accessing values
learner$param_set$values
learner$param_set$values$cp <- 0.01
learner$param_set$values

### 2.1 load a learner ####
task = tsk("pima")
learner = lrn("classif.rpart")

### 2.2 Train a learner ####
learner$model # none yet

# Now we fit the classification tree using the 
# training set of the task by calling the $train() 
# method of Learner:

learner$train(task)
learner$model
plot(learner$model)

### 2.3 Predicting
pima_new = data.table::fread("
age, glucose, insulin, mass, pedigree, pregnant, pressure, triceps
24,  145,     306,     41.7, 0.5,      3,        52,       36
47,  133,     NA,      23.3, 0.2,      7,        83,       28
")
pima_new

prediction = learner$predict_newdata(pima_new)
prediction

# with truth column
pima_new_known = cbind(pima_new, diabetes = factor("pos", levels = c("pos", "neg")))
prediction = learner$predict_newdata(pima_new_known)
prediction


### 2.3 prediction type ####

task_pima_new = as_task_classif(pima_new_known, target = "diabetes")
prediction = learner$predict(task_pima_new)
prediction


learner$predict_type = "prob"

# re-fit the model
learner$train(task)

# rebuild prediction object
prediction = learner$predict(task_pima_new)
prediction

### 2.4 Confusion matrix
prediction <- learner$predict(task)
prediction$confusion

### 2.4 plotting predictions
task = tsk("penguins")
learner = lrn("classif.rpart", predict_type = "prob")
learner$train(task)
prediction = learner$predict(task)

library("mlr3viz")
autoplot(prediction)

## test
task_iris = as_task_classif(iris, target = "Species")

learner = lrn("classif.rpart", predict_type = "prob")
learner$train(task_iris)
prediction = learner$predict(task_iris)

library("mlr3viz")
autoplot(prediction)
