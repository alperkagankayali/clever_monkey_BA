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
