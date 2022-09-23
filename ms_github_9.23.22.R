library(dplyr)
library(haven)
library(pecan)
library(ggplot2)
library(psych)
library(janitor)
library(MBESS)
library(emmeans)
library(ggpubr)

setwd("/Users/phoebelam/Google Drive/0_datasets/mega/datasets/0_merged/00_current/")
setwd("/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebe.h.lam@gmail.com/My Drive/0_datasets/mega/datasets/0_merged/00_current")
mega <- readRDS("mega.rds")
# saveRDS(mega, "mega.rds")

mega %>% select(., stimgc_bin_v1, stimgc_bin_long, wbalo, gender_r, bmi, age, cur.obses_mega_dis, lsi.chr.mean_nohome) %>%
  rename(phenotype_t1 = stimgc_bin_v1,
         phenotype_t2 = stimgc_bin_long,
         race = wbalo,
         female = gender_r,
         disadv = cur.obses_mega_dis,
         stress = lsi.chr.mean_nohome) %>%
  mutate(disadv_z = scale(disadv),
         stress_z = scale(stress),
         black_dc = case_when(race == "2.black"~1,
                              TRUE~0),
         asian_dc = case_when(race == "3.asian"~1,
                              TRUE~0),
         latinx_dc = case_when(race=="4.latinx"~1,
                               TRUE~0),
         poc_dc = case_when(race=="5.other"~1,
                            TRUE~0)) -> dat

# main effects: disadvantage and stress
glm(phenotype_t1~ race + female + bmi + age + disadv_z, data=git, family = "binomial")
glm(phenotype_t1~ race + female + bmi + age + stress_z, data=dat, family = "binomial")
glm(phenotype_t2~ race + female + bmi + age + disadv_z, data=dat, family = "binomial")
glm(phenotype_t2~ race + female + bmi + age + stress_z, data=dat, family = "binomial")
glm(phenotype_t2~ race + female + bmi + age + phenotype_t1 + disadv_z, data=dat, family = "binomial")
glm(phenotype_t2~ race + female + bmi + age + phenotype_t1 + stress_z, data=dat, family = "binomial")

# indirect effects of disadvantage -> stress -> phenotype
process (data = dat, y = "phenotype_t1", x = "disadv_z", m = "stress_z", 
         cov = c("age", "female", "bmi", "black_dc", "asian_dc", "latinx_dc", "poc_dc"),
         model = 4, total = 1, conf=95,
         boot = 30000, seed=12345)

process (data = dat, y = "phenotype_t2", x = "disadv_z", m = "stress_z", 
         cov = c("age", "female", "bmi", "black_dc", "asian_dc", "latinx_dc", "poc_dc"),
         model = 4, total = 1, conf=95,
         boot = 30000, seed=12345)

process (data = dat, y = "phenotype_t2", x = "disadv_z", m = "stress_z", 
         cov = c("phenotype_t1", "age", "female", "bmi", "black_dc", "asian_dc", "latinx_dc", "poc_dc"),
         model = 4, total = 1, conf=95,
         boot = 30000, seed=12345)

# age-moderated indirect effects of disadvantage -> stress -> phenotype
process (data = dat, y = "phenotype_t1", x = "disadv_z", m = "stress_z", w = "age",
         cov = c("age", "female", "bmi", "black_dc", "asian_dc", "latinx_dc", "poc_dc"),
         model = 15, total = 1, conf=95,
         boot = 30000, seed=12345)

process (data = dat, y = "phenotype_t2", x = "disadv_z", m = "stress_z", w = "age",
         cov = c("age", "female", "bmi", "black_dc", "asian_dc", "latinx_dc", "poc_dc"),
         model = 15, total = 1, conf=95,
         boot = 30000, seed=12345)

process (data = dat, y = "phenotype_t2", x = "disadv_z", m = "stress_z", w = "age",
         cov = c("phenotype_t1", "age", "female", "bmi", "black_dc", "asian_dc", "latinx_dc", "poc_dc"),
         model = 15, total = 1, conf=95,
         boot = 30000, seed=12345)




