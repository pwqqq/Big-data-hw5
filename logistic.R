library(readr)
library(dplyr)

Predict_NoShow_Train <- read_csv("~/Dropbox/Predict_NoShow_Train.csv")

df = Predict_NoShow_Train

df$Gender_n = ifelse(df$Gender=='M',1,0)

df$DateAppointmentWasMade = as.Date(df$DateAppointmentWasMade,'%Y-%m-%d')
df$DateOfAppointment = as.Date(df$DateOfAppointment,'%Y-%m-%d')

df$year_of_app_made = as.numeric(format(df$DateAppointmentWasMade, "%Y"))
df$month_of_app_made = as.numeric(format(df$DateAppointmentWasMade, "%m"))

df$year_of_app = as.numeric(format(df$DateOfAppointment, "%Y"))
df$month_of_app = as.factor(format(df$DateOfAppointment, "%m"))
df$status = ifelse(df$Status  == 'Show-Up',0,1)

train_chr_dt = df %>%
  select(-Status) %>%
  select(-ID) %>%
  select(-DateAppointmentWasMade) %>%
  select(-DateOfAppointment) %>%
  select(-Gender) %>%
  select(-DayOfTheWeek)

train_chr = train_chr_dt %>%
  select(-year_of_app, -year_of_app_made)# -month_of_app, -month_of_app_made)

day_week = model.matrix(~DayOfTheWeek-1, df)

train = cbind(train_chr,day_week)
train_numeric = data.frame(apply(train,2,as.numeric))

# prepare train and test data
data_set_size = floor(nrow(train_numeric)*.6)

indexes = sample(1:nrow(train_numeric), size = data_set_size)

train.s = train_numeric[indexes,]
test.s = train_numeric[-indexes,]

mod_logi = glm(status~.,data = train.s, family = 'binomial')
pred.glm = predict(mod_logi,data.frame(test.s[,-14]),type = 'response')

prediction_glm <- as.numeric(pred.glm > 0.5)

logloss_glm = -sum( test.s[,14] %*% log(pred.glm) + (1 - test.s[,14]) %*% log(1 - pred.glm) ) /length(test.s[,14])

