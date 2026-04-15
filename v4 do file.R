library(dplyr)

ds <- read.csv("L:/Auditdata/Students/Lexi/Data_Lexi_v4.csv")
View(ds)

colnames(ds)

#PREP----
##renaming vars----
ds <- ds %>%
  rename(LS21 = quality_of_life_a_k,
         H21 = height_k,
         W21 = weight_k,
         waist21 = waist_k,
         WS_a21 = weight_statements_a_k,
         WS_b21 = weight_statements_b_k,
         WS_c21 = weight_statements_c_k,
         WS_d21 = weight_statements_d_k,
         WCT_21 = weight_change_k,
         momPhys_21 = physique_mom_k,
         dadPhys21 = physique_dad_k,
         age_2024 = age,
         LS24 = qol,
         phyHealth_24 = phy_health_v2,
         menHealth_24 = men_health_v2,
         W24 = weight_k_v2,
         H24 = height_k_v2,
         WS_a24 = weight_statements_a_k_v2,
         WS_b24 = weight_statements_b_k_v2,
         WS_c24 = weight_statements_c_k_v2,
         WS_d24 = weight_statements_a_k_v2,
         WCT_24 = weight_change_k,
         momPhys_24 = physique_mom_k_v2,
         dadPhys24 = physique_dad_k_v2,
         age_2021 = cpr_alder,
         speUd_21  = work_b_k1,
         diplUd_21 = work_b_k2,
         dayWork_21 = work_schedule_a_m_chc,
         nightWork_21 = work_schedule_b_m_chc,
         eveWork_21 = work_schedule_c_m_chc,
         dayHrs_21 = day_hours_m_chc,
         nightHrs_21 = evening_hours_m_chc,
         eveHrs_21 = night_hours_m_chc,
         mixedHrs_21 = shift_hours_m_chc,
         nightFreq_24 = eve,
         eveFreq_24 = night,
         weekendFreq_24 = weekend,
         dayWork_24 = day_hours_k_v2,
         nightWork_24 = evening_hours_k_v2,
         eveWork_24 = night_hours_k_v2,
         mixedWork_24 = shift_hours_k_v2,
         weekendWork_24 = weekend_k_v2,
         LGBTID = lbgt,
         famInh_24 = inheritage_icd_v2,
         obeInh_24 = inheritage_icd_v3___5
  )

##treat age----
ds <- ds %>%
  mutate(age_2021 = trunc(age_2021),
         age_2024 = trunc(age_2024))

ds <- ds %>%
  mutate(
    age_2021_imputed = if_else(
      is.na(age_2021) & !is.na(age_2024),
      age_2024-3,
      age_2021
    )
  )

ds <- ds %>%
  mutate(
    age_2021_imputed=trunc(age_2021_imputed)
  )

ds <- ds %>%
  mutate(
    age_2024_imputed = if_else(
      !is.na(age_2021) & is.na(age_2024),
      age_2021+3,
      age_2024
    )
  )

ds <- ds %>%
  mutate(
    age_flag = if_else(
      is.na(age_2021) & !is.na(age_2024),
      "missing 2021",
      if_else(
        !is.na(age_2021) & is.na(age_2024),
        "missing 2024",
        NA_character_
      )
    )
  )

##age 25+----
ds <- ds %>%
  filter(age_2021_imputed >= 25)

# Full distribution of LGBTID
table(ds$LGBTID, useNA = "always")

# Cross tabulation with sex
table(ds$cpr_sex, ds$LGBTID, useNA = "always")

##making height to integer ----
v3 <- v3 %>%
  mutate(height_2021 = trunc(height_2021),
         height_2024 = trunc(height_2024))

##making weight to integer ----
v3 <- v3 %>%
  mutate(weight_2021 = trunc(weight_2021),
         weight_2024 = trunc(weight_2024))
