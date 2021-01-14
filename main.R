library(tidyverse)

companies <- read.csv("companies.csv")
payments <- read.csv("payments.csv")
physicians <- read.csv("physicians.csv")
previously_released_payments <- read.csv("previously_released_payments.csv")

#A subset containig only Physicians Data that should be used for training
trainPhysicians <- physicians[physicians$set == 'train', ]
#A subset containig only Physicians Data that should be used for testing
testPhysicians <- physicians[physicians$set == 'test', ]

train_pay <- merge(x = train, y = payments, by.x = "id", by.y = "Physician_ID", all=TRUE)

glimpse(train)
summary(train)

summary(train_pay)

#A subset containing of only Payments Data that should be used for training
paymentsTrain <- subset(payments, Physician_ID %in% trainPhysicians$id )
#A subset containing of only Payments Data that should be used for testing
paymentsTest <- subset(payments, Physician_ID %in% testPhysicians$id )

#A subset containing of only Payments Data that should be used for training AND the Ownership Indicator is YES
paymentsTrainYes <- subset(paymentsTrain, Ownership_Indicator == 'Yes')
#A subset containing of only Payments Data that should be used for training AND the Ownership Indicator is NO
paymentsTrainNo <- subset(paymentsTrain, Ownership_Indicator == 'No')

#A subset containing of only Physicians Data that should be used for training AND the Ownership Indicator is YES 
trainPhysiciansYes <- subset(trainPhysicians, id %in% paymentsTrainYes$Physician_ID)
#A subset containing of only Physicians Data that should be used for training AND the Ownership Indicator is NO -- PROBABLY NOT USABLE - NOT IMPORTANT
trainPhysiciansNo <- subset(trainPhysicians, id %in% paymentsTrainNo$Physician_ID)
