library(tidyverse)

set.seed(2021)
companies <- read.csv("companies.csv")
payments <- read.csv("payments.csv")
physicians <- read.csv("physicians.csv")
previously_released_payments <- read.csv("previously_released_payments.csv")

train <- physicians[physicians$set == 'train', ]
test <- physicians[physicians$set == 'test', ]
train_pay <- merge(x = train, y = payments, by.x = "id", by.y = "Physician_ID", all=TRUE)

#### PHYS NOT FROM US #####
train[train$Country != 'UNITED STATES' & is.na(train$State), ]

companies[is.na(companies$Company_ID)]
NROW(companies[companies$State == 'NY', ])
NROW(companies[companies$State != 'NY', ])
subset(companies, State == 'NY')

#### NULL PAYEMENTS WITH PRODUCT #####
NROW(subset(payments, is.na(Product_Type_1)))

NROW(payments[is.na(payments$City_of_Travel), ])
NROW(subset(payments, is.na(City_of_Travel)))

NROW(subset(payments, !is.na(City_of_Travel)))

NROW(subset(payments, Nature_of_Payment_or_Transfer_of_Value == 'Travel and Lodging'))
subset(payments, !is.na(Contextual_Information) & Ownership_Indicator == 'No')[, c('Contextual_Information', 'Ownership_Indicator')]

physicians$License_State_2
NROW(subset(physicians, is.na(physicians$License_State_1)))
NROW(subset(physicians, is.na(physicians$License_State_2)))
NROW(subset(physicians, is.na(physicians$License_State_3)))
NROW(subset(physicians, is.na(physicians$License_State_4)))
NROW(subset(physicians, is.na(physicians$License_State_5)))

glimpse(train)
summary(train)
NROW(payments)
summary(payments)

summary(train_pay)
