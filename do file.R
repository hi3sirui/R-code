install.packages("labelled")
install.packages("Publish")
install.packages("gtsummary")
install.packages("flextable")
install.packages("huxtable")
install.packages("skimr")
install.packages("markdown")
install.packages("ftExtra")

getwd()
list.files()

#load .csv in R, titled "v1", has to be backlash "/"
v1 <- read.csv("L:/Auditdata/Students/Lexi/v1 csv.csv")

install.packages("dplyr")
library(dplyr)

#Renaming variables
names(v1)
v1 <- v1 %>%
  rename(WaistCircum_2021=waist_k,
         WeightStatement_prior13_2021=weight_statements_a_k,
         WeightStatement_13to19_2021=weight_statements_b_k,
         WeightStatement_19to25_2021=weight_statements_c_k,
         WeightStatement_over25_2021=weight_statements_d_k,
         WeightStatement_prior13_2024=weight_statements_a_k_v2,
         WeightStatement_13to19_2024=weight_statements_b_k_v2,
         WeightStatement_19to25_2024=weight_statements_c_k_v2,
         WeightStatement_over25_2024=weight_statements_d_k_v2
  )
v1 <- v1 %>%
  rename(Age_2021=cpr_alder,
         Age_2024=age,
         Weight_2021=weight_k,
         Weight_2024=weight_k_v2,
         Height_2021=height_k,
         Height_2024=height_k_v2,
  )

#Making age an integer
v1 <- v1 %>%
  mutate(Age_2021=trunc(Age_2021),Age_2024=trunc(Age_2024))

#Creating the B#MI variable, and if either height or weight is NA then BMI is also NA
v1 <- v1 %>%
  mutate(
    BMI_2021 = if_else(
      !is.na(Weight_2021) & !is.na(Height_2021) &
        Weight_2021 > 0 & Height_2021 > 0,
      Weight_2021 / (Height_2021 / 100)^2,
      NA_real_
    )
  )
v1 <- v1 %>%
  mutate(
    BMI_2024 = if_else(
      !is.na(Weight_2024) & !is.na(Height_2024) &
        Weight_2024 > 0 & Height_2024 > 0,
      Weight_2024 / (Height_2024 / 100)^2,
      NA_real_
    )
  )

#total number of NA fields. 2021: 33838; 2024: 42137
sum(is.na(v1$Age_2021)) 
sum(is.na(v1$Age_2024))

#number of NA fields ONLY in 2021: 9717
sum(is.na(v1$Age_2021) & !is.na(v1$Age_2024))
#number of NA fields ONLY in 2024:18016
sum(!is.na(v1$Age_2021) & is.na(v1$Age_2024))
#number of rows with a NA field in 2021 OR 2024: 27733
sum(xor(is.na(v1$Age_2021), is.na(v1$Age_2024)))


#sanity check✅
# sum(
#   (!is.na(v1$Age_2021) & !is.na(v1$Age_2024)) +
#     ( is.na(v1$Age_2021) & !is.na(v1$Age_2024)) +
#     (!is.na(v1$Age_2021) &  is.na(v1$Age_2024)) +
#     ( is.na(v1$Age_2021) &  is.na(v1$Age_2024))
# )

#Populate the reference table with only one age missing
age_missing_exactly_one <- v1 %>%
  filter(xor(is.na(Age_2021), is.na(Age_2024))) %>%
  select(ipnr, Age_2021, Age_2024)
View(age_missing_exactly_one)

write.csv(
  age_missing_exactly_one,
  file = "age_missing_exactly_one.csv",
  row.names = FALSE
)

#Flagging the missing fields
v1 <- v1 %>%
  mutate(
    Age_2021_imputed = if_else(
      is.na(Age_2021) & !is.na(Age_2024),
      1L, 0L
    ),
    Age_2024_imputed = if_else(
      !is.na(Age_2021) & is.na(Age_2024),
      1L, 0L
    )
  )
table(v1$Age_2021_imputed)#9717 missing in 2021
table(v1$Age_2024_imputed)#18016 missing in 2024

#Calculating the missing ages
v1 <- v1 %>%
  mutate(
    Age_2021 = if_else(
      is.na(Age_2021) & !is.na(Age_2024),
      Age_2024 - 3,
      Age_2021
    ),
    Age_2024 = if_else(
      !is.na(Age_2021) & is.na(Age_2024),
      Age_2021 + 3,
      Age_2024
    )
  )
#sanity check on only 1 age is missing, n=0
#sum(xor(is.na(v1$Age_2021), is.na(v1$Age_2024)))
v1 %>%
  filter(!is.na(Age_2021) & !is.na(Age_2024)) %>%
  mutate(gap = Age_2024 - Age_2021) %>%
  count(gap)

v1 <- v1 %>%
  mutate(
    age_gap_flag = if_else(
      !is.na(Age_2021) & !is.na(Age_2024) & (Age_2024 - Age_2021 != 3),
      1L, 0L
    )
  )

table(v1$age_gap_flag)





