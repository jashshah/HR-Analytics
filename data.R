source('helper.R')
source('libraries.R')

mydata <- read_excel('data/HR Data.xlsx', na = "NA") %>%
  mutate(quit = as.factor(if_else(Absconding == "Resigned" | 
                                          Absconding == "Absconding", "1", "0"))) %>%
  select(-Functionality) %>%
  as.data.frame()

functionality_df <- read.csv('data/functionality_split.csv', stringsAsFactors = FALSE)

variance_hike <- read_excel('data/MLPredictionTillMarch28_V1.xlsx') %>%
  select(Empcode, Rating, `Variance %`)

mydata <- base::merge(mydata, functionality_df)
mydata <- base::merge(mydata, variance_hike)

prop.table(table(mydata$quit, useNA = 'always')) # 30:70 split between Yes:No

mydata <- mydata %>% filter(!(is.na(Rating) & is.na(`Variance %`)))

dat <- train_test_split(mydata, DepVar = 'quit', Split = 0.7, seed = 1)
train <- dat$train
test <- dat$test

X_train <- select(train, -quit)
Y_train <- select(train, quit)

X_test <- select(test, -quit)
Y_test <- select(test, quit)

rm(mydata, dat, train_test_split, functionality_df, variance_hike)
