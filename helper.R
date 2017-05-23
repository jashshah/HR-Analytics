train_test_split <- function(DataFrame, DepVar, Split, seed){
  library(caTools)
  set.seed(seed)
  ind <- sample.split(Y = DataFrame[,DepVar], SplitRatio = Split)
  train <- DataFrame[ind,]
  test <- DataFrame[!ind,]
  return(list(train = train, test = test))
  print("The training and testing datasets have been created.")
}

num_NAs <- function(x){
  sapply(x, function(y) sum(is.na(y)))
}

min_max_scaling <- function(train, test){
  
  min_vals <- sapply(train, min)
  range1 <- sapply(train, function(x) diff(range(x)))
  
  # scale the training data
  
  train_scaled <- data.frame(matrix(nrow = nrow(train), ncol = ncol(train)))
  
  for(i in seq_len(ncol(train))){
    column <- (train[,i] - min_vals[i])/range1[i]
    train_scaled[i] <- column
  }
  
  colnames(train_scaled) <- colnames(train)
  
  # scale the testing data using the min and range of the train data
  
  test_scaled <- data.frame(matrix(nrow = nrow(test), ncol = ncol(test)))
  
  for(i in seq_len(ncol(test))){
    column <- (test[,i] - min_vals[i])/range1[i]
    test_scaled[i] <- column
  }
  
  colnames(test_scaled) <- colnames(test)
  
  return(list(train = train_scaled, test = test_scaled))
}

checkInstallLoad <- function(libName) {
  if(!require(libName, character.only=TRUE)) {
    install.packages(libName)
    require(libName, character.only=TRUE)
  }
}