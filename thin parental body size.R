library(dplyr)
View(test)

table(test$physique_mom_k)
table(test$physique_dad_k)

test <- test %>%
  mutate(
    mom_3cat = factor(case_when(
       physique_mom_k >= 1 & physique_mom_k <= 3 ~ "obese",
    physique_mom_k >= 4 & physique_mom_k <=6 ~ "reference",
    physique_mom_k >=7 ~ "underweight",
    TRUE ~ NA_character_
    ),
   levels = c("reference", "obese", "underweight")
  ),
    
    dad_3cat = factor(case_when(
      physique_dad_k >= 1 & physique_dad_k <= 3 ~ "obese",
      physique_dad_k >= 4 & physique_dad_k <=6 ~ "reference",
      physique_dad_k >=7 ~ "underweight",
      TRUE ~ NA_character_
    ),
    levels = c("reference", "obese", "underweight")
  ))
  
table(test$mom_3cat, useNA = "always")
table(test$dad_3cat, useNA = "always")
