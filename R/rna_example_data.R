rm(list = ls())
library(tidyverse) 
library(fastDummies)
library(foreign)
library(naniar) 
client = T
remove = T
method_normlization = c("voom","voom_nodesign","hg", "voom_nodesign_quantile")#,"hg_ours")
method = method_normlization[3]

source("/Volumes/Share/projects/DE/R/init_packages.R")
source("/Volumes/Share/projects/DE/R/init_data.R")

source("/Volumes/Share/projects/DE/R/config.R") 


 predictors = c("high_lowbirth","obesew1or2","obesew3","obesew4","obesew5")

 #outcome = "Alzheimers_up_mRNA" # for disease signatures
 outcome = "Colorectal_ud_mRNA" # for single genes

controls = c("sex_interv", "re", "Plate", "AvgCorrelogram100",
             "age_w1orw2",
             #"bingedrink", "currentsmoke",
             "W5REGION","cpreterm",
             "pregnant_biow5", "illness_4wks_biow5", "illness_2wks_biow5",
             "smoking_biow5", "kit_biow5", "tube_biow5",  "FastHrs",  
             "travel_biow5", "BirthY"
) 
phen=waves %>% mutate(blood = AID %in% dat$AID) %>% filter(blood==TRUE) 

# phen=phen %>%  mutate(time_biow5 = case_when( (hour_biow5==6 | hour_biow5==7) ~ "6_7",
#                                               (hour_biow5==8 | hour_biow5==9) ~ "8_9",
#                                               (hour_biow5==10 | hour_biow5==11) ~ "10_11",
#                                               (hour_biow5==12 | hour_biow5==13) ~ "12_13",
#                                               (hour_biow5==14 | hour_biow5==15) ~ "14_15",
#                                               (hour_biow5==16 | hour_biow5==17) ~ "16_17",
#                                               (hour_biow5==18 | hour_biow5==19 | hour_biow5==20) ~ "18_19_20"))


#remove people who already have certain diseases by setting NA to their corresponding diseases signature value

temp  = phen %>%  left_join(outme, by = "AID") %>% mutate_at(.vars = c("diabetes_mRNA"), .funs =funs(ifelse(diabetes == 1, NA, . ))) %>% 
  mutate_at(.vars = c("CVD_mRNA"), .funs = funs(ifelse(heartatk == 1, NA, . ))) %>% 
  mutate_at(.vars = c("Asthma_mRNA"), .funs = funs(ifelse(H5ID6F == 1| H5ID6FM == 1, NA, . ))) %>% 
  mutate_at(.vars = c("Hypertension_mRNA"), .funs = funs(ifelse(H5ID6C == 1 |H5ID6CM == 1, NA, . ))) %>% 
  mutate_at(.vars = c("Aortic_Aneurysm_ud_mRNA"), .funs = funs(ifelse(H5ID6Q == 1 |H5ID6QM == 1, NA, . ))) %>% 
  mutate_at(.vars = c("Melanoma_mRNA"), .funs = funs(ifelse(H5ID6A == 1 |H5ID6AM == 1, NA, . )))


keep = temp %>% dplyr:: select(controls, predictors, outcome)%>% complete.cases



toy_example = temp %>% dplyr::select(
                                                predictors, sex_interv_m, re_2, re_3, re_4, re_5, Plate_Year1Plate02, Plate_Year1Plate03, Plate_Year1Plate04, 
                                                Plate_Year1Plate05, Plate_Year1Plate06, Plate_Year1Plate07, Plate_Year1Plate08, 
                                                Plate_Year1Plate09, Plate_Year1Plate10, Plate_Year1Plate11, Plate_Year1Plate12,
                                                AvgCorrelogram100, age_w1orw2, age_w3, age_w4, age_w5,
                                                #bingedrink, currentsmoke, 
                                                W5REGION_MW, W5REGION_S, W5REGION_W,cpreterm,
                                                pregnant_biow5, 
                                                illness_4wks_biow5, illness_2wks_biow5, smoking_biow5, kit_biow5, tube_biow5,
                                                FastHrs,travel_biow5,BirthY,
                                                outcome)#, FAMST5_2, FAMST5_3, FAMST5_4, FAMST5_5, 
#matches("months_biow5_"), matches("time_biow5_")
#) %>% dplyr::select (-months_biow5_1, -time_biow5_6_7, -months_biow5_NA, -time_biow5_NA)
toy_example = toy_example %>% mutate(BirthY= as.numeric(BirthY) - 1974) %>% 
  mutate(BirthY2= BirthY^2, BirthY3= BirthY^3) %>%
  select(-outcome,outcome)

toy_example = toy_example[keep,]

toy_example = toy_example %>% 
  dplyr:: filter(age_w1orw2<19, age_w3>18 & age_w3<25, age_w4>24) %>%
  select(-age_w3, -age_w4, -age_w5)

oldnames = toy_example %>% colnames()

newnames = c(paste("w", 1:length(predictors), sep = ""),paste("c", 1:(dim(toy_example)[2] - length(predictors) - 1), sep = ""),"y")

toy_example =  toy_example %>% rename_at(vars(oldnames), ~ newnames) 

toy_example %>% saveRDS(str_c("/Volumes/xu/epi_try/R/range_real_data/toy_example_obese",outcome) %>% str_c("birthY.rds"))
