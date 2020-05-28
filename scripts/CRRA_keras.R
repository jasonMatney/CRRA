rm(list=ls())
library(data.table)
library(dplyr)
library(readbulk)
library(keras)
library(dataPreparation)
library(tidyverse)
# install_keras()

dsn="C:\\Users\\jmatney\\Documents\\GitHub\\CRRA"
setwd(dsn)
# raw_data <- read_bulk(directory = "data\\hybrid")
# dim(raw_data)
# df <- raw_data[ !names(raw_data) %in% c("Index","File")]

df <- as.data.frame(fread(paste0(dsn, "\\data\\hybrid\\CRRA_data.csv")))
# remove -9999 
df <- df[which(df$Dep_100yr_Depth_ft != -9999), ]

# Random sample indexes
train_index <- sample(1:nrow(df), 0.8 * nrow(df))
test_index <- setdiff(1:nrow(df), train_index)

train_data <- df[train_index, ] 
train_data <- dplyr::select(train_data, -c("x","y")) 

test_data <- df[test_index, ]
test_data <- dplyr::select(test_data, -c("x","y")) 



###############
## Normalize ##
###############

# ................

######################
## TRAIN TEST SPLIT ##
######################

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
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(x_train)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()

#####################
## Train the model ##
#####################

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 500

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