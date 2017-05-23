source('helper.R')
source('libraries.R')

# Data from a company called Access Healthcare

mydata <- read_excel('data/HR Data.xlsx', na = "NA") %>%
  mutate(DOL = as.factor(if_else(DOL == "No", "0", if_else(DOL == "Yes", "1", DOL)))) %>%
  select(-Functionality) %>%
  as.data.frame()

functionality_df <- read.csv('data/functionality_split.csv', stringsAsFactors = FALSE)

mydata <- base::merge(mydata, functionality_df)

# prop.table(table(mydata$DOL, useNA = 'always')) # 30:70 split between Yes:No

dat <- train_test_split(mydata, DepVar = 'DOL', Split = 0.7, seed = 1)
train <- dat$train
test <- dat$test

X_train <- select(train, -DOL)
Y_train <- select(train, DOL)

X_test <- select(test, -DOL)
Y_test <- select(test, DOL)

rm(mydata, dat, train_test_split, functionality_df)
