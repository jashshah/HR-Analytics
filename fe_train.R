source('data.R')

x_train <- X_train %>%
  select(-Empcode, -Name, -rec_id, -NT_UserName, -UserId, -ExitInitiatedOn, -Absconding, -PrevCompanyName, -L1Supervisor, -L2Supervisor, -Designation, -HighestDegree, -Major, -Client)  %>%
  rename(productivity = `Productivity%`, quality = `Quality%`)

y_train <- Y_train

# Convert DOJ and DOB to dates and find the age of joining (in days)

x_train$DOJ <- ymd(x_train$DOJ)
x_train$DOB <- ymd(x_train$DOB)
x_train$age_at_joining <- as.numeric(difftime(time1 = x_train$DOJ, time2 = x_train$DOB))

# Fill NA values in Per_Pincode by values in Pre_Pincode
x_train$Per_Pincode[is.na(x_train$Per_Pincode)] <- x_train$Pre_Pincode[is.na(x_train$Per_Pincode)]

# Create a new variable whether Pre_Pincode and Per_Pincode are same

x_train$Pre_Pincode <- as.character(x_train$Pre_Pincode)
x_train$Per_Pincode <- as.character(x_train$Per_Pincode)

x_train <- x_train %>%
  mutate(pin_code_diff = if_else(Pre_Pincode == Per_Pincode, 0, 1))
  
x_train <- x_train %>%
  select(-DOJ, -DOB, -Pre_Pincode, -Per_Pincode, -MLAvailed)

# Convert values for PrevCompExp less than 0 to 0 and log transform it after adding 1

x_train$PrevCompanyExp[x_train$PrevCompanyExp < 0] <- 0
x_train$PrevCompanyExp <- log(x_train$PrevCompanyExp + 1)

# Create three new variables - whether both productivity and quality are NA, only productivity is NA or only quality is NA

x_train <- x_train %>%
  mutate(prod_qual_both = if_else(is.na(productivity) & is.na(quality) | 
                                    productivity == 0 & is.na(quality), 1, 0), 
         prod_only = if_else(is.na(productivity) & !is.na(quality), 1, 0), 
         qual_only = if_else(!is.na(productivity) & is.na(quality), 1, 0))

# Replace NA in productivity and quality by the median

median_productivity <- median(x_train$productivity, na.rm = TRUE)
median_quality <- median(x_train$quality, na.rm = TRUE)

x_train$productivity[is.na(x_train$productivity)] <- median_productivity
x_train$quality[is.na(x_train$quality)] <- median_quality

# x_train <- x_train %>%
#   mutate(main_func = if_else(main_func == 'Operation', 'Operations', main_func)) %>%
#   mutate(main_func = if_else(main_func == 'Shared service' | main_func == 'Shared Service', 'Shared Services', main_func))

x_train <- x_train %>%
  select(-main_func, -sub_func)

# Create a new variable whether there are more than zero dependents

x_train <- x_train %>%
  mutate(dep_zero = ifelse(Dependents == 0, 1, 0)) %>%
  select(-Dependents)


