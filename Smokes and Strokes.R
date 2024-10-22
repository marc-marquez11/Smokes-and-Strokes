library("DescTools")
library("vcd")

#c
stroke <- read.csv(
  "C:/Users/MMM/Desktop/colleg stuff/Fall 2023/cat data anal/the strokes.csv")
stroke <- stroke[stroke$bmi != "N/A", ] #Missing/ambiguous info removed
stroke <- stroke[stroke$smoking_status != "Unknown", ]
stroke <- stroke[stroke$gender != "Other", ]
stroke$bmi <- as.numeric(stroke$bmi)
colnames(stroke)

#stroke: response
#gender, hypertension, heart dis: binom
#smokingstat: multinom
#age, gluclvl, bmi: contin


#d)
#Gender and Stroke Status
(gstr.table <- table(stroke$gender, stroke$stroke, 
      dnn = c("Gender", "Stroke")))
round(proportions(gstr.table, 1), 4)
mosaic(gstr.table, labeling = labeling_values)
fisher.test(gstr.table)

#Hypertension and Stroke Status
(hypstr.table <- table(stroke$hypertension, stroke$stroke, 
      dnn = c("Hypertension", "Stroke")))
round(proportions(hypstr.table, 1), 4)
mosaic(hypstr.table, labeling = labeling_values)
fisher.test(hypstr.table)

#Heart Disease and Stroke Status
(hdstr.table <- table(stroke$heart_disease, stroke$stroke, 
      dnn = c("Heart Disease", "Stroke")))
round(proportions(hdstr.table, 1), 4)
mosaic(hdstr.table, labeling = labeling_values)
fisher.test(hdstr.table)

#Smoking Status and Stroke Status
(smstr.table <- table(stroke$smoking_status, stroke$stroke, 
      dnn = c("Smoking Status", "Stroke")))
round(proportions(smstr.table, 1), 4)
mosaic(smstr.table, labeling = labeling_values)
chisq.test(smstr.table)
GTest(smstr.table)

#Age and Stroke Status
astr.fit <- glm(stroke~age, family = binomial, data = stroke)
summary(astr.fit)
drop1(astr.fit, test = "LRT")

#BMI and Stroke Status
bmistr.fit <- glm(stroke~bmi, family = binomial, data = stroke)
summary(bmistr.fit)
drop1(bmistr.fit, test = "LRT")

#Avg Glucose Level and Stroke Statuss
aglstr.fit <- glm(stroke~avg_glucose_level, family = binomial, data = stroke)
summary(aglstr.fit)
drop1(aglstr.fit, test = "LRT")


#e)
#The Model
fit.2 <- glm(
  stroke~age + hypertension + heart_disease + 
    avg_glucose_level, family = binomial, data = stroke)

#Residual Plots and Info
plot(rstandard(fit.2)~stroke$stroke, 
     xlab = "Stroke Status", ylab = "Standard Deviance Residuals",
     xlim = c(0,1))
sum(rstandard(fit.2) >= 2)
sum(rstandard(fit.2) >= 0)
sum(rstandard(fit.2) <= 0)
sum(rstandard(fit.2) <= -1)
sum(rstandard(fit.2) <= -2)


#f)

#Model 1: Main Effects
me.fit <- glm(
  stroke~gender + age + hypertension + heart_disease + smoking_status + 
  avg_glucose_level + bmi, family = binomial, data = stroke)
summary(me.fit)
drop1(me.fit, test = "LRT")

#Model 2: Drop Gender and BMI
fit.1 <- glm(
  stroke~age + hypertension + heart_disease + smoking_status + 
    avg_glucose_level, family = binomial, data = stroke)
summary(fit.1)
drop1(fit.1, test = "LRT") 

#Model 3: Drop Smoking Status
fit.2 <- glm(
  stroke~age + hypertension + heart_disease + 
    avg_glucose_level, family = binomial, data = stroke)
summary(fit.2)
drop1(fit.2, test = "LRT")

#Automated Stepwise
fit.null <- glm(stroke~1, family = binomial, data = stroke)
fit.max <- glm(stroke~(age + hypertension + heart_disease + 
                         avg_glucose_level) ^ 2,
               family = binomial, data = stroke)
step(fit.null, scope = list( 
  upper=~((age + hypertension + heart_disease + 
             avg_glucose_level) ^ 2), lower=~1), direction = "both")
step(fit.null, scope = list( 
  upper=~((age + hypertension + heart_disease + 
             avg_glucose_level) ^ 2), lower=~1), direction = "forward")
step(fit.max, scope = list( 
  upper=~((age + hypertension + heart_disease + 
      avg_glucose_level) ^ 2), lower=~1), direction = "backward")
#All 3 yield the same model

#Model 4: Add AGL*Heart Disease and Hypertension*Heart Disease
fit.3 <- glm(formula = stroke ~ age + avg_glucose_level + hypertension + 
      heart_disease + avg_glucose_level:heart_disease + 
      hypertension:heart_disease, 
    family = binomial, data = stroke)
summary(fit.3) #heart disease becomes insignificant
drop1(fit.3, test = "LRT")

#Model 5: Add AGL*Heart Disease
fit.3.1 <- glm(
  stroke~age + hypertension + heart_disease + 
    avg_glucose_level + avg_glucose_level:heart_disease,
  family = binomial, data = stroke)
summary(fit.3.1) #heart disease still insignificant
drop1(fit.3.1, test = "LRT")
#Check if it's better than fit 2
anova(fit.2, fit.3, test="LRT")

