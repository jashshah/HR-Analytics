source('data.R')
source('fe_train.R')

x_test <- X_test %>%
  select(-Empcode, -Name, -rec_id, -NT_UserName, -UserId, -ExitInitiatedOn, -Absconding, -PrevCompanyName, -L1Supervisor, -L2Supervisor, -Designation, -HighestDegree, -Major, -Client)  %>%
  rename(productivity = `Productivity%`, quality = `Quality%`)

y_test <- Y_test

x_test$DOJ <- ymd(x_test$DOJ)
x_test$DOB <- ymd(x_test$DOB)
x_test$age_at_joining <- as.numeric(difftime(time1 = x_test$DOJ, time2 = x_test$DOB))

x_test$Per_Pincode[is.na(x_test$Per_Pincode)] <- x_test$Pre_Pincode[is.na(x_test$Per_Pincode)]

x_test$Pre_Pincode <- as.character(x_test$Pre_Pincode)
x_test$Per_Pincode <- as.character(x_test$Per_Pincode)
x_test <- x_test %>%
  mutate(pin_code_diff = if_else(Pre_Pincode == Per_Pincode, 0, 1))

x_test <- x_test %>%
  select(-DOJ, -DOB, -Pre_Pincode, -Per_Pincode, -MLAvailed)

x_test$PrevCompanyExp[x_test$PrevCompanyExp < 0] <- 0
x_test$PrevCompanyExp <- log(x_test$PrevCompanyExp + 1)

x_test <- x_test %>%
  mutate(prod_qual_both = if_else(is.na(productivity) & is.na(quality) | 
                                    productivity == 0 & is.na(quality), 1, 0), 
         prod_only = if_else(is.na(productivity) & !is.na(quality), 1, 0), 
         qual_only = if_else(!is.na(productivity) & is.na(quality), 1, 0))

x_test$productivity[is.na(x_test$productivity)] <- median_productivity
x_test$quality[is.na(x_test$quality)] <- median_quality

# x_test <- x_test %>%
#   mutate(main_func = if_else(main_func == 'Operation', 'Operations', main_func)) %>%
#   mutate(main_func = if_else(main_func == 'Shared service' | main_func == 'Shared Service', 'Shared Services', main_func))

x_test <- x_test %>%
  select(-main_func, -sub_func)

x_test <- x_test %>%
  mutate(dep_zero = ifelse(Dependents == 0, 1, 0)) %>%
  select(-Dependents)


