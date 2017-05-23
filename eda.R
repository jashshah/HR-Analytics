source('data.R')

summary(train$`Productivity%`)

# ggplot(train, aes(x = log(productivity))) + geom_histogram()

# ggplot(train, aes(x = quality)) + geom_histogram()

ggplot(train, aes(x = NoOfSickLeaveAvailed)) + geom_histogram()

train %>%
  filter(Gender == "F" & MLAvailed != 0) %>%
  ggplot() + geom_histogram(aes(x = MLAvailed, fill = DOL))

table(train$MLAvailed)

prop <- mean(as.numeric(as.character(train$DOL)))

ggplot(train, aes(Client, fill = DOL)) + 
  geom_bar(position = 'fill') + 
  theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = prop)

ggplot(train, aes(Client, DOL)) + geom_jitter() + theme(axis.text.x = element_text(angle = 90))

ggplot(train, aes(Client)) + geom_bar() + facet_grid(.~DOL) + theme(axis.text.x = element_text(angle = 90))

ggplot(train, aes(Client, fill = DOL)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

ggplot(train, aes(reorder(Client, Client, function(x) -length(x)), fill = DOL)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

fe_train <- data.frame(x_train, y_train)

ggplot(fe_train, aes(prod_qual_both, fill = DOL)) + geom_bar(position = 'fill')

ggplot(fe_train, aes(prod_only, fill = DOL)) + geom_bar()

ggplot(fe_train, aes(qual_only, fill = DOL)) + geom_bar(position = 'fill')

ggplot(fe_train, aes(main_func, fill = DOL)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

ggplot(train, aes(sub_func, fill = DOL)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) # + scale_x_discrete(limits = c('Clinical'))

ggplot(train, aes(sub_func, fill = DOL)) + geom_bar(position = 'fill') + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = prop)


ggplot(train, aes(Dependents, fill = DOL)) + geom_bar(position = 'fill')

ggplot(x_train, aes(log(productivity + 1))) + geom_histogram()
