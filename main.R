library(tidyverse)

companies <- read.csv("companies.csv")
payments <- read.csv("payments.csv")
physicians <- read.csv("physicians.csv")
previously_released_payments <- read.csv("previously_released_payments.csv")

train <- physicians[physicians$set == 'train', ]
test <- physicians[physicians$set == 'test', ]

train_pay <- merge(x = train, y = payments, by.x = "id", by.y = "Physician_ID", all=TRUE)

glimpse(train)
summary(train)

summary(train_pay)


paymentsTrain <- subset(payments, Physician_ID %in% train$id )
paymentsTest <- subset(payments, Physician_ID %in% test$id )

paymentsTrainYes <- subset(paymentsTrain, Ownership_Indicator == 'Yes')
paymentsTrainNo <- subset(paymentsTrain, Ownership_Indicator == 'No')

trainPhysiciansYes <- subset(trainPhysicians, id %in% paymentsTrainYes$Physician_ID)
trainPhysiciansNo <- subset(trainPhysicians, id %in% paymentsTrainYes$Physician_ID)
