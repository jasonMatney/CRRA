rm(list=ls())
library(data.table)
library(dplyr)
library(readbulk)
library(keras)
library(dataPreparation)
library(tidyverse)
library(BBmisc)
# install_keras()

dsn="C:\\Users\\jmatney\\Documents\\GitHub\\CRRA"

#################
## Read in data files and merge
################
# setwd(dsn)
# raw_data <- read_bulk(directory = "data\\hybrid")
# dim(raw_data)
# df <- raw_data[ !names(raw_data) %in% c("Index","File")]

#### Load merged data
df <- as.data.frame(fread(paste0(dsn, "\\data\\hybrid\\CRRA_data.csv")))

################
# remove -9999 # 
################

# df[df == -9999] <- NA
# final <- df[complete.cases(df), ]
# dim(final)
# df <- df[which(df$Dep_100yr_Depth_ft != -9999), ]


###############################################
## Make test data frame to run model quickly ##
###############################################
 ### delete this when running full model! ###
df <- df[1:10000,]
dim(df)


###############
## Normalize ##
###############
df_scale <- df

# remove coords from scaling
df_scale <- df_scale[ , -which(names(df_scale) %in% c("x","y"))]

# get numeric values
ind <- !sapply(df_scale, is.integer)

# Normalize
df_scale[ind] <- lapply(df_scale[ind], normalize)

head(df_scale)

# ................

######################
## TRAIN TEST SPLIT ##
######################


# Random sample indexes
train_index <- sample(1:nrow(df_scale), 0.8 * nrow(df_scale))
test_index <- setdiff(1:nrow(df_scale), train_index)

train_data <- df_scale[train_index, ] 
#train_data <- dplyr::select(train_data, -c("x","y")) 

test_data <- df_scale[test_index, ]
#test_data <- dplyr::select(test_data, -c("x","y")) 


########################
y <- "Dep_100yr_Depth_ft"

# Build X_train, y_train, X_test, y_test
x_train <- as.matrix(dplyr::select(train_data, -c(y)))
y_train <- as.matrix(dplyr::select(train_data, c(y)))

x_test <- as.matrix(dplyr::select(test_data, -c(y)))
y_test <- as.matrix(dplyr::select(test_data, c(y)))

#############
### KERAS ###
#############

######################
## Create the model ##
######################
build_model <- function() {
  
  # add dropout layers
  # add early stopping 
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",input_shape = dim(x_train)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mae",
    optimizer = optimizer_rmsprop(),
model %>% summary()

##################### list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
## Train the model ##
#####################

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 50

# Fit the model and store training stats
history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(print_dot_callback)
)

summary(model)
plot(history)

model %>% evaluate(x_test, y_test)

############################
# Un-Normalize and Predict #
############################

#--------------------------