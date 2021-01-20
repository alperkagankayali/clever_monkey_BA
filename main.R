library(tidyverse)

companies <- read.csv("companies.csv")
payments <- read.csv("payments.csv")
physicians <- read.csv("physicians.csv")
previously_released_payments <- read.csv("previously_released_payments.csv")

#A subset containig only Physicians Data that should be used for training
trainPhysicians <- physicians[physicians$set == 'train', ]
#A subset containig only Physicians Data that should be used for testing
testPhysicians <- physicians[physicians$set == 'test', ]

train_pay <- merge(x = trainPhysicians, y = payments, by.x = "id", by.y = "Physician_ID") # not all=true, it adds non-matched values at the end.
test_pay <- merge(x = testPhysicians, y = payments, by.x = "id", by.y = "Physician_ID")
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
trainPhysiciansNo <- subset(trainPhysicians, id %in% paymentsTrainNo$Physician_ID) # also not in trainPhysiciansYes should be.

#Check if there is a significant difference in staates -> is not
table(trainPhysiciansYes$State)/NROW(trainPhysiciansYes)
table(trainPhysiciansNo$State)/NROW(trainPhysiciansNo)
table(trainPhysicians$State)/NROW(trainPhysicians)


#Check if there is a significant difference in cities -> is not
table(trainPhysiciansYes$City)/NROW(trainPhysiciansYes)
table(trainPhysiciansNo$City)/NROW(trainPhysiciansNo)
table(trainPhysicians$City)/NROW(trainPhysicians)

#Check Countries -> All out of the USA but only 0.02 from United States Minor Outlying Island. So not significant
table(trainPhysiciansYes$Country)/NROW(trainPhysiciansYes)
table(trainPhysiciansNo$Country)/NROW(trainPhysiciansNo)
NROW(trainPhysiciansYes)

#Check Primary Specialities Neurology -> Very likely its getting postive  23% (positive) vs 2.34% (negativ) if you are a Neurology Physisician there is a likelyhood of 95 % chance it is positive
# Urology it is roughly 9 % vs 2.62 %
# Ophthalmology 7.6 % vs 2.96
table(trainPhysiciansYes$Primary_Specialty)

table(trainPhysiciansNo$Primary_Specialty)
count(trainPhysiciansYes,)
setDT(trainPhysiciansYes)[,.N, by=Primary_Specialty]

library(data.table)
orderdPYPS <- setDT(trainPhysiciansYes)[, .N, by = Primary_Specialty][order(-N)]
orderdPYPSFrequency <-orderdPYPS
orderdPYPSFrequency$N <- as.numeric(orderdPYPS$N)/ NROW(trainPhysiciansYes)
orderdPYPSFrequency

orderdPNPS <- setDT(trainPhysiciansNo)[, .N, by = Primary_Specialty][order(-N)]
orderdPNPSFrequency <-orderdPNPS
orderdPNPSFrequency$N <- as.numeric(orderdPNPS$N)/ NROW(trainPhysiciansNo)
orderdPNPSFrequency

orderdPAPS <- setDT(trainPhysicians)[, .N, by = Primary_Specialty][order(-N)]

#ProductCategories -> Neurology
ProductCatAll <- setDT(paymentsTrain)[, .N, by = Product_Category_1][order(-N)]
ProductCatYes1 <- setDT(paymentsTrainYes)[, .N, by = Product_Category_1][order(-N)]
ProductCatYes2 <- setDT(paymentsTrainYes)[, .N, by = Product_Category_2][order(-N)]
ProductCatYes3 <- setDT(paymentsTrainYes)[, .N, by = Product_Category_3][order(-N)]

#ProductCategories -> Also filter -> 64406-008-01, 64406-006-02, 64406-011-01
ProductCodeAll1 <- setDT(paymentsTrain)[, .N, by = Product_Code_1][order(-N)]
ProductCodeYes1 <- setDT(paymentsTrainYes)[, .N, by = Product_Code_1][order(-N)]
ProductCodeYes2 <- setDT(paymentsTrainYes)[, .N, by = Product_Code_2][order(-N)]
ProductCodeYes3 <- setDT(paymentsTrainYes)[, .N, by = Product_Code_3][order(-N)]

#ProductTypes -> Biological
ProductTypeAll1 <- setDT(paymentsTrain)[, .N, by = Product_Type_1][order(-N)]
ProductTypeYes1 <- setDT(paymentsTrainYes)[, .N, by = Product_Type_1][order(-N)]
ProductTypeYes2 <- setDT(paymentsTrainYes)[, .N, by = Product_Type_2][order(-N)]
ProductTypeYes3 <- setDT(paymentsTrainYes)[, .N, by = Product_Type_3][order(-N)]

#States
PhysisicianStateAll1 <- setDT(trainPhysicians)[, .N, by = State][order(-N)]
PhysisicianStateYes1 <- setDT(trainPhysiciansYes)[, .N, by = State][order(-N)]

CompanyStateAll1 <- setDT(companies)[, .N, by = State][order(-N)]
CompanyStateYes1 <- setDT(trainPhysiciansYes)[, .N, by = State][order(-N)]

#City
PhysisicianCityAll1 <- setDT(trainPhysicians)[, .N, by = City][order(-N)]
PhysisicianCityYes1 <- setDT(trainPhysiciansYes)[, .N, by = City][order(-N)]
#ZIP
PhysisicianZIPAll1 <- setDT(trainPhysicians)[, .N, by = Zipcode][order(-N)]
PhysisicianZIPYes1 <- setDT(trainPhysiciansYes)[, .N, by = Zipcode][order(-N)]
#Licence State
PhysisicianLicenseStateAll1 <- setDT(trainPhysicians)[, .N, by = License_State_1][order(-N)]
PhysisicianLicenseStateYes1 <- setDT(trainPhysiciansYes)[, .N, by = License_State_1][order(-N)]

PhysisicianLicenseStateAll1Freque <-PhysisicianLicenseStateAll1
PhysisicianLicenseStateAll1Freque$N <- as.numeric(PhysisicianLicenseStateAll1Freque$N)/ NROW(trainPhysicians)
PhysisicianLicenseStateYes1Freque <-PhysisicianLicenseStateYes1
PhysisicianLicenseStateYes1Freque$N <- as.numeric(PhysisicianLicenseStateYes1Freque$N)/ NROW(trainPhysiciansYes)


NROW(paymentsTrainYes)
