#Read Data
library(readxl)
vets <- read_excel("D:/ITS/DATA AKADEMIK/SEMESTER 6/ANALISIS SURVIVAL/vets.xlsx")
vets

#Distribution Test on Survival Time
Uji_Distribusi <- ks.test(vets$time,"pexp",rate=(1/mean(vets$time)))
Uji_Distribusi

#Open Library
library(survival)
pct=0:137/137
survival=1-pct

#Kaplan-Meier Curves
library(ggplot2)
library(survminer)
KM_Treatment=survfit(Surv(time,status)~trt,data=vets)
ggsurvplot(KM_Treatment,data=veteran,surv.median.line = "hv", pval = TRUE)
KM_Cell_Type_1=survfit(Surv(time,status)~cell_type_1,data=vets)
ggsurvplot(KM_Cell_Type_1,data=vets,surv.median.line = "hv", pval = TRUE)
KM_Cell_Type_2=survfit(Surv(time,status)~cell_type_2,data=vets)
ggsurvplot(KM_Cell_Type_2,data=vets,surv.median.line = "hv", pval = TRUE)
KM_Cell_Type_3=survfit(Surv(time,status)~cell_type_3,data=vets)
ggsurvplot(KM_Cell_Type_3,data=vets,surv.median.line = "hv", pval = TRUE)
KM_Cell_Type_4=survfit(Surv(time,status)~cell_type_4,data=vets)
ggsurvplot(KM_Cell_Type_4,data=vets,surv.median.line = "hv", pval = TRUE)
KM_Prior_Therapy=survfit(Surv(time,status)~prior_therapy,data=vets)
ggsurvplot(KM_Prior_Therapy,data=vets, pval = TRUE)

#Log Rank Test
survdiff(formula=Surv(time,status)~trt,data=vets)
survdiff(formula=Surv(time,status)~cell_type_1,data=vets)
survdiff(formula=Surv(time,status)~cell_type_2,data=vets)
survdiff(formula=Surv(time,status)~cell_type_3,data=vets)
survdiff(formula=Surv(time,status)~cell_type_4,data=vets)
survdiff(formula=Surv(time,status)~prior_therapy,data=vets)

#Parametric Survival Model
model1 = survreg(Surv(time,status)~trt+cell_type_1+cell_type_2+cell_type_3+cell_type_4+performance+disease_duration+age+prior_therapy,data=vets,dist="exponential")
summary(model1)
model2 = survreg(Surv(time,status)~trt+cell_type_1+cell_type_2+cell_type_3+cell_type_4+performance+age+prior_therapy,data=vets,dist="exponential")
summary(model2)
model3 = survreg(Surv(time,status)~trt+cell_type_1+cell_type_2+cell_type_3+cell_type_4+performance+age,data=vets,dist="exponential")
summary(model3)
model4 = survreg(Surv(time,status)~trt+cell_type_1+cell_type_2+cell_type_3+cell_type_4+performance,data=vets,dist="exponential")
summary(model4)
model5 = survreg(Surv(time,status)~cell_type_1+cell_type_2+cell_type_3+cell_type_4+performance,data=vets,dist="exponential")
summary(model5)
model6 = survreg(Surv(time,status)~cell_type_2+cell_type_3+cell_type_4+performance,data=vets,dist="exponential")
summary(model6)
model7 = survreg(Surv(time,status)~cell_type_2+cell_type_3+performance,data=vets,dist="exponential")
summary(model7)

#Best Parametric Survival Model Selection
AIC = AIC(model1,model2,model3,model4,model5,model6,model7)
AIC