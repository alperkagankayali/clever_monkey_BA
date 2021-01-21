library(tidyverse)
library(ggplot2)

set.seed(2021)
companies <- read.csv("companies.csv")
payments <- read.csv("payments.csv")
physicians <- read.csv("physicians.csv")
previously_released_payments <- read.csv("previously_released_payments.csv")

dim(physicians)
dim(payments)
trainPhysicians <- physicians[physicians$set == 'train', ]
trainPayments <- merge(x = trainPhysicians, y = payments_exp, by.x = "id", by.y = "Physician_ID")
finalTableTrain <- merge(x = trainPayments, y = companies, 
                         by.x = "Company_ID", by.y = "Company_ID")

companies$State[is.na(companies$State)] <- "Other"
#### Graphs #####
## State
ggplot(trainPayments, aes(x=State, fill = factor(Ownership_Indicator))) + 
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 2))
summary(trainPayments[c("Ownership_Indicator", "State")])

ggplot(finalTableTrain, aes(x=State.y, fill = factor(Ownership_Indicator))) + 
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 2))

ggplot(finalTableTrain, aes(x=Country.y, fill = factor(Ownership_Indicator))) + 
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 45))

ggplot(finalTableTrain, aes(x=Country.y)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45))

##### PHYSICIAN TABLE #####
physicians_exp <- physicians

physicians_exp %>% as_tibble()
### CITY COL
# This col has too many factors and therefore either removed or catagorized more.
NROW(unique(physicians_exp$City))
physicians_exp <- select(physicians_exp, -c("City"))

### LICENSE STATE COL
# instead of having 5 cols, we are going to have one col (license state)
# and multiple rows.
physicians_exp <- physicians_exp %>% 
  pivot_longer(c('License_State_1', 'License_State_2', 'License_State_3', 
                 'License_State_4', 'License_State_5'), 
               values_to ='License_State', names_repair = "unique")
physicians_exp <- subset(physicians_exp, !is.na(physicians_exp$License_State), select = -c(name))
dim(physicians_exp)

physicians_exp <- physicians_exp %>% 
  mutate(License_State_Dir = case_when(
    License_State == "CT" | License_State == "ME" | 
      License_State == "MA" | License_State == "NH"| 
      License_State == "RI" |  License_State == "VT" ~ "NE_1",
    License_State == "NY" | License_State == "NJ" | License_State == "PA" ~ "NE_2",
    License_State == "WI" | License_State == "MI" | License_State == "IL" | License_State == "IN"| 
      License_State == "OH" ~ "MW_1",
    License_State == "ND" | License_State == "MN" | License_State == "SD" | 
      License_State == "IA"| License_State == "NE" | 
      License_State == "KS" | License_State == "MO" ~ "MW_2",
    License_State == "DE" | License_State == "FL" | 
      License_State == "GA" | License_State == "MD"| 
      License_State == "NC" | License_State == "SC"|
      License_State == "VA" | License_State == "WV"|
      License_State == "DC" | License_State == "PR" ~ "S_1",
    License_State == "AL" | License_State == "KY" | 
      License_State == "TN" | License_State == "MS" ~ "S_2",
    License_State == "AR" | License_State == "TX" | 
      License_State == "OK" | License_State == "LA" ~ "S_3",
    License_State == "AZ" | License_State == "CO"| 
      License_State == "ID" | License_State == "MT"|
      License_State == "NV" | License_State == "NM"|
      License_State == "UT" | License_State == "WY" ~ "W_1",
    License_State == "AK" | License_State == "CA" | 
      License_State == "HI" | License_State == "OR" |
      License_State == "WA" ~ "W_2",
    TRUE ~ "Other"
  )) %>%
  select(-c(License_State))
physicians_exp$License_State_Dir <- as.factor(physicians_exp$License_State_Dir)

### PS COL
physicians_exp$Primary_Specialty <- as.character(physicians_exp$Primary_Specialty)
test_p <- physicians_exp
test_p["primarySpecialty_count"] <- NA
for (row in 1: NROW(test_p)){
  split <- unlist(strsplit(test_p$Primary_Specialty[row], '\\|'))
  test_p$primarySpecialty_count[row] <- NROW(split)
}
unique(test_p$primarySpecialty_count)
## there are max 3 specialities, therefore we can create 3 cols to hold them.

physicians_exp[, "Primary_Specialty_1"] <- as.character(NA)
physicians_exp[, "Primary_Specialty_2"] <- as.character(NA)
physicians_exp[, "Primary_Specialty_3"] <- as.character(NA)
for (row in 1: NROW(physicians_exp)){
  split <- unlist(strsplit(physicians_exp$Primary_Specialty[row], '\\|'))
  for (i in 1:length(split)){
    col_name <- sprintf("Primary_Specialty_%d", i)
    physicians_exp[row, col_name] <- split[i]
  }
}
physicians_exp <- subset(physicians_exp, select =-c(Primary_Specialty))
physicians_exp$Primary_Specialty_1 <- as.factor(physicians_exp$Primary_Specialty_1)
physicians_exp$Primary_Specialty_2 <- as.factor(physicians_exp$Primary_Specialty_2)
physicians_exp$Primary_Specialty_3 <- as.factor(physicians_exp$Primary_Specialty_3)
glimpse(physicians_exp)
summary(physicians_exp)
levels(physicians_exp$Primary_Specialty_1) #5
levels(physicians_exp$Primary_Specialty_2) #35
levels(physicians_exp$Primary_Specialty_3) # 117
# 4952 NA's in Primary_Specialty_3 and weird values (consider removing this col)
# also this col has around 150 117.
# consider leaving Primary_Specialty_1 and Primary_Specialty_2 only or just 1.

## POSSIBLIE SOLUTION: LONGER PIVOT
## PROBLEM: if a physician have multiple license states and multiple specs.
#, it will be seen as if he is specialized in this field in this license state,
# not specialized in this field in general. (?)
physicians_primary_spec_exp <- physicians_exp %>% 
  pivot_longer(c('Primary_Specialty_3', 'Primary_Specialty_2', 'Primary_Specialty_1'), 
               values_to ='Primary_Specialty', names_repair = "unique")
physicians_primary_spec_exp <- subset(physicians_exp, !is.na(physicians_exp$Primary_Specialty), select = -c(name))
dim(physicians_primary_spec_exp)
physicians_primary_spec_exp$Primary_Specialty <- as.factor(physicians_exp$Primary_Specialty)
levels(physicians_primary_spec_exp$Primary_Specialty) # 154 levels

## SOLUTION 2:
# REMOVING Primary_Specialty_2 and Primary_Specialty_3.
# PRO: LESS SPECIFIC VALUES.
# CON: LESS VALUES IN GENERAL.
physicians_primary_spec_1 <- subset(physicians_exp, select = -c(Primary_Specialty_2, Primary_Specialty_3))
dim(physicians_primary_spec_1)
physicians_primary_spec_1$Primary_Specialty <- as.factor(physicians_exp$Primary_Specialty_1)
physicians_primary_spec_1 <- select(physicians_primary_spec_1, -c("Primary_Specialty_1"))
levels(physicians_primary_spec_1$Primary_Specialty) # 5 levels

physicians_exp <- physicians_primary_spec_1


## ZIP CODE COL (?)
# could be removed.
physicians_exp <- select(physicians_exp, -c("Zipcode"))

## PROVINCE COL
physicians_exp <- subset(physicians_exp, select = -c(Province))

# STATE
physicians_exp <- physicians_exp %>% 
  mutate(State_Dir = case_when(
    State == "CT" | State == "ME" | 
      State == "MA" | State == "NH"| 
      State == "RI" |  State == "VT" ~ "NE_1",
    State == "NY" | State == "NJ" | State == "PA" ~ "NE_2",
    State == "WI" | State == "MI" | State == "IL" | State == "IN"| 
      State == "OH" ~ "MW_1",
    State == "ND" | State == "MN" | State == "SD" | 
      State == "IA"| State == "NE" | 
      State == "KS" | State == "MO" ~ "MW_2",
    State == "DE" | State == "FL" | 
      State == "GA" | State == "MD"| 
      State == "NC" | State == "SC"|
      State == "VA" | State == "WV"|
      State == "DC" | State == "PR" ~ "S_1",
    State == "AL" | State == "KY" | 
      State == "TN" | State == "MS" ~ "S_2",
    State == "AR" | State == "TX" | 
      State == "OK" | State == "LA" ~ "S_3",
    State == "AZ" | State == "CO"| 
      State == "ID" | State == "MT"|
      State == "NV" | State == "NM"|
      State == "UT" | State == "WY" ~ "W_1",
    State == "AK" | State == "CA" | 
      State == "HI" | State == "OR" |
      State == "WA" ~ "W_2",
    TRUE ~ "Other"
  )) %>%
  select(-c(State))
physicians_exp$State_Dir <- as.factor(physicians_exp$State_Dir)

## COUNTRY COL
# all from US > remove
physicians_exp <- subset(physicians_exp, select = -c(Country))

physicians_ML <- physicians_exp %>% select(-contains("Name"))

summary(physicians_ML)
## 2 NA State, 2 "UNITED STATES MINOR OUTLYING ISLANDS" in Country.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### PAYEMENTS TABLE #####

payments_exp <- payments %>% as_tibble()
## DATE
## TODO: TO BE REVISITED ##

## NUMBER OF PAYEMENTS
subset(payments_exp, Number_of_Payments == 3)[order(payments_exp$Company_ID), ]
## IS THERE A TRANS ID? 
## ?

## Form_of_Payment_or_Transfer_of_Value
# TODO: FIX
unique(payments_exp$Form_of_Payment_or_Transfer_of_Value)
form_of_pay_col <- payments_exp$Form_of_Payment_or_Transfer_of_Value
summary(form_of_pay_col)
levels(form_of_pay_col)[
  levels(form_of_pay_col)=="Stock, stock option, or any other ownership interest"
  | levels(form_of_pay_col)=="Any other ownership interest"
  | levels(form_of_pay_col)=="Stock"
  | levels(form_of_pay_col)=="Stock option"
  | levels(form_of_pay_col)=="Stock, stock option, or any other ownership interest"
  | levels(form_of_pay_col)=="Dividend, profit or other return on investment"] <- "Other ownership interest"
unique(form_of_pay_col)
summary(form_of_pay_col)
NROW(subset(payments_exp, is.na(form_of_pay_col)))

## Nature_of_Payment_or_Transfer_of_Value
nature_of_pay_travel <- subset(payments_exp, Nature_of_Payment_or_Transfer_of_Value == 
              'Travel and Lodging')
NROW(nature_of_pay_travel)
# Since there are only 75179 rows with Travel and Lodging (very low number),
# the cols (City_of_Travel, State_of_Travel and Country_of_Travel)
# will be removed
payments_exp <- select(payments_exp, -c("City_of_Travel", "State_of_Travel", "Country_of_Travel"))

## Ownership_Indicator
unique(payments_exp$Ownership_Indicator)
payments_exp$Ownership_Indicator <- as.numeric(
  if_else(payments_exp$Ownership_Indicator == 'Yes', 1, 0))

## Third_Party_Recipient
third_part_rec <- payments_exp$Third_Party_Recipient
unique(third_part_rec)
summary(third_part_rec)
##Entity             Individual No Third Party Payment 
##14742                   4800                1382708 
# (?)

## Charity
unique(payments_exp$Charity)
na_charity <- subset(payments_exp, is.na(payments_exp$Charity))
NROW(na_charity)
na_charity
yes_charity <- subset(payments_exp, payments_exp$Charity == 'Yes')
no_charity <- subset(payments_exp, payments_exp$Charity == 'No')
yes_charity
NROW(yes_charity) ## 27
NROW(no_charity) ## 767502
## 26 out of 27 yeses has Entity in Third_Party_Recipient
## while this is could be a good corr., I don't think we can use the col
# due to the low number of not null observations and also the huge of No's.
payments_exp <- select(payments_exp, -c("Charity"))
# should it be kept (?)

## Third_Party_Covered
# lots of nulls and included in Third_Party_Recipient
payments_exp <- select(payments_exp, -c("Third_Party_Covered"))

## Contextual Info. 
# hard to get any data from it, therefore removed.
payments_exp <- select(payments_exp, -c("Contextual_Information"))

## PRODUCT

# Names and codes will be removed.
payments_exp <- payments_exp %>% select(-contains("Name"))
payments_exp <- payments_exp %>% select(-contains("Code"))

# pivot product type and category to rows.
NROW(subset(payments_exp, is.na(Product_Type_1)))/NROW(payments_exp) # 0.06%
NROW(subset(payments_exp, is.na(Product_Type_2)))/NROW(payments_exp) # 78%
NROW(subset(payments_exp, is.na(Product_Type_3)))/NROW(payments_exp) # 92%

NROW(subset(payments_exp, is.na(Product_Category_1)))/NROW(payments_exp) # 41%
NROW(subset(payments_exp, is.na(Product_Category_2)))/NROW(payments_exp) # 87%
NROW(subset(payments_exp, is.na(Product_Category_3)))/NROW(payments_exp) # 95%

payments_exp <- payments_exp %>%
  pivot_longer(c('Product_Type_1', 'Product_Type_2', 'Product_Type_3'), 
             values_to ='Product_Type', names_repair = "unique")
payments_exp <- subset(payments_exp, !is.na(payments_exp$Product_Type), select = -c(name))
#payments_exp <- payments_exp %>%
#  pivot_longer(c('Product_Category_1', 'Product_Category_2', 'Product_Category_3'), 
#               values_to ='Product_Category', names_repair = "unique")
#payments_exp <- subset(payments_exp, !is.na(payments_exp$Product_Type), select = -c(name))

# (?) how to proceed with categories?
# naive solution: removing all the three columns (?)
payments_exp <- select(payments_exp, -c("Product_Category_1", 
                                        "Product_Category_2",
                                       "Product_Category_3"))

# solution 2: removing Product_Category_2 and Product_Category_3 and keeping 
# only Product_Category_1. Also, removing rows with null values of category
# risk: removing 38% of the data.

# solution 3: impute.
# hard: since there are 1678 values.
unique(payments_exp$Product_Category_1)
summary(payments_exp$Product_Category_1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### COMPANIES TABLE #####
## State
# null if country not US (?)
# naive solution
companies_exp <- companies
companies_exp <- companies_exp %>% 
  mutate(Company_State_Dir = case_when(
    State == "CT" | State == "ME" | 
      State == "MA" | State == "NH"| 
      State == "RI" |  State == "VT" ~ "NE_1",
    State == "NY" | State == "NJ" | State == "PA" ~ "NE_2",
    State == "WI" | State == "MI" | State == "IL" | State == "IN"| 
      State == "OH" ~ "MW_1",
    State == "ND" | State == "MN" | State == "SD" | 
      State == "IA"| State == "NE" | 
      State == "KS" | State == "MO" ~ "MW_2",
    State == "DE" | State == "FL" | 
      State == "GA" | State == "MD"| 
      State == "NC" | State == "SC"|
      State == "VA" | State == "WV"|
      State == "DC" | State == "PR" ~ "S_1",
    State == "AL" | State == "KY" | 
      State == "TN" | State == "MS" ~ "S_2",
    State == "AR" | State == "TX" | 
      State == "OK" | State == "LA" ~ "S_3",
    State == "AZ" | State == "CO"| 
      State == "ID" | State == "MT"|
      State == "NV" | State == "NM"|
      State == "UT" | State == "WY" ~ "W_1",
    State == "AK" | State == "CA" | 
      State == "HI" | State == "OR" |
      State == "WA" ~ "W_2"
  ))
companies_exp$Company_State_Dir <- as.factor(physicians_exp$Company_State_Dir)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### JOIN ####

summary(companies_exp)
summary(payments_exp)
summary(physicians_ML)

trainPhysicians <- physicians_ML[physicians_ML$set == 'train', ]
testPhysicians <- physicians_ML[physicians_ML$set == 'test', ]
## all = TRUE removed to not join missing rows from both tables with NA (Full
# Outer Join).
trainPayments <- merge(x = trainPhysicians, y = payments_exp, by.x = "id", by.y = "Physician_ID")
dim(trainPayments) # 5 mio.?
testPayments <- merge(x = testPhysicians, y = payments_exp, by.x = "id", by.y = "Physician_ID")
dim(testPayments)
summary(trainPayments)

finalTableTrain <- merge(x = trainPayments, y = companies_exp, 
                         by.x = "Company_ID", by.y = "Company_ID")
finalTableTest <- merge(x = testPayments, y = companies_exp, 
                         by.x = "Company_ID", by.y = "Company_ID")

finalTableTrain <- select(finalTableTrain, -c("set"))
finalTableTest <- select(finalTableTest, -c("set"))
summary(finalTableTrain)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### CLASSIFICATION ####
#install.packages("tree")
library(tree)

tree.pay = tree(Ownership_Indicator~., data=finalTableTrain)

#### OTHER ANALYSIS ####
#### PHYS NOT FROM US 
trainPhysicians[trainPhysicians$Country != 'UNITED STATES' & is.na(trainPhysicians$State), ]

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
