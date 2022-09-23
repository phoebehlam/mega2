# main effects: disadvantage and stress
glm(phenotype_t1~ race + female + bmi + age + disadv_z, data=dat, family = "binomial")
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




