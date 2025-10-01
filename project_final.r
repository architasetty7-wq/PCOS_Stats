pcos_data = read.csv("PCOS_extended_dataset.csv")

pcos_data$PCOS..Y.N. = as.factor(pcos_data$PCOS..Y.N.)
pcos_data$Weight.gain.Y.N. = as.factor(pcos_data$Weight.gain.Y.N.)
pcos_data$Fast.food..Y.N. = as.factor(pcos_data$Fast.food..Y.N.)
pcos_data$Reg.Exercise.Y.N. = as.factor(pcos_data$Reg.Exercise.Y.N.)

str(pcos_data)

#data summary

hist(pcos_data$BMI)
hist(pcos_data$FSH.LH)
hist(pcos_data$BP._Systolic..mmHg.)
hist(pcos_data$BP._Diastolic..mmHg.)
hist(pcos_data$RBS.mg.dl.)
hist(pcos_data$PRG.ng.mL.)

z_scores <- scale(pcos_data$FSH.LH)
pcos_data = pcos_data[abs(z_scores) < 3,]

z_scores <- scale(pcos_data$BP._Systolic..mmHg.)
pcos_data = pcos_data[abs(z_scores) < 3,]

z_scores <- scale(pcos_data$BP._Diastolic..mmHg.)
pcos_data = pcos_data[abs(z_scores) < 3,]

z_scores <- scale(pcos_data$RBS.mg.dl.)
pcos_data = pcos_data[abs(z_scores) < 3,]

z_scores <- scale(pcos_data$PRG.ng.mL.)
pcos_data = pcos_data[abs(z_scores) < 3,]

hist(pcos_data$BMI)
hist(pcos_data$FSH.LH)
hist(pcos_data$BP._Systolic..mmHg.)
hist(pcos_data$BP._Diastolic..mmHg.)
hist(pcos_data$RBS.mg.dl.)
hist(pcos_data$PRG.ng.mL.)

#bar graphs for the qualitative data
PCOS_data = c(sum(pcos_data$PCOS..Y.N.),
              length(pcos_data$PCOS..Y.N.) - sum(pcos_data$PCOS..Y.N.))
#this assigns a label to each value
names(PCOS_data) = c("PCOS","Normal")
#this builds the plot with labels
barplot(PCOS_data, 
        main = "Frequency of PCOS", 
        xlab = "PCOS or Normal", 
        ylab = "Frequency", 
        col = c("green","red"))

Excercise_data = c(sum(pcos_data$Reg.Exercise.Y.N.),
                   length(pcos_data$Reg.Exercise.Y.N.) - sum(pcos_data$Reg.Exercise.Y.N.))
#this assigns a label to each value
names(Excercise_data) = c("Regularly Exercised","Did Not Regularly Exercise")
#this builds the plot with labels
barplot(Excercise_data, 
        main = "Frequency of Regular Exercise", 
        xlab = "Regularly Exercised or Did Not Regularly Exercise", 
        ylab = "Frequency", 
        col = c("green","red"))

Fast_food_data = c(sum(pcos_data$Fast.food..Y.N.),
                   length(pcos_data$Fast.food..Y.N.) - sum(pcos_data$Fast.food..Y.N.))
#this assigns a label to each value
names(Fast_food_data) = c("Fast Food Eater","Not")
#this builds the plot with labels
barplot(Fast_food_data, 
        main = "Frequency of Eating Fast Food", 
        xlab = "Fast Food Eater or Not", 
        ylab = "Frequency", 
        col = c("green","red"))

weight_data = c(sum(pcos_data$Weight.gain.Y.N.),
                   length(pcos_data$Weight.gain.Y.N.) - sum(pcos_data$Weight.gain.Y.N.))
#this assigns a label to each value
names(weight_data) = c("Weight Gain","Not")
#this builds the plot with labels
barplot(weight_data, 
        main = "Frequency of Weight Gain", 
        xlab = "Weight Gain or Not", 
        ylab = "Frequency", 
        col = c("green","red"))


##############################################################################
#assumptions
#multi-colinearity
library(corrplot)
#now lets check for multicollinearity using corplot
predictors = pcos_data[,c("BMI","FSH.LH","BP._Systolic..mmHg.",
                          "BP._Diastolic..mmHg.","PRG.ng.mL.","RBS.mg.dl.")]
# Calculate the correlation matrix of the predictors
cor_matrix = cor(predictors)
# Print correlation matrix for review
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Predictors for Predicting BMI", mar = c(0,0,1,0))
################################################################################

logit_model <- glm(PCOS..Y.N.~ Weight.gain.Y.N.+Fast.food..Y.N.+Reg.Exercise.Y.N.+FSH.LH+BP._Systolic..mmHg.+BP._Diastolic..mmHg.+RBS.mg.dl.+PRG.ng.mL., 
             data = pcos_data, family = binomial())

summary(logit_model)

logit_model2 <- glm(PCOS..Y.N.~ Weight.gain.Y.N.+Fast.food..Y.N.+Reg.Exercise.Y.N.+BP._Systolic..mmHg.+PRG.ng.mL., 
                 data = pcos_data, family = binomial())

summary(logit_model2)

pcos_data$probabilities <- predict(logit_model, newdata = pcos_data, type = "response")
str(pcos_data)

library(ggplot2)
ggplot(pcos_data, aes(x=BP._Systolic..mmHg., y=probabilities, color=PCOS..Y.N.)) +
  geom_point() + theme_classic()

ggplot(pcos_data, aes(x=PRG.ng.mL., y=probabilities, color=PCOS..Y.N.)) +
  geom_point() + xlim(0, 7.5)+theme_classic()
##############################################################################
pcos_data = read.csv("PCOS_extended_dataset.csv")
#get the values for each combination
#PCOS False Weight gain False
pf_wf = nrow(pcos_data[pcos_data$PCOS..Y.N.==0 & pcos_data$Weight.gain.Y.N.==0,])
#PCOS True Weight gain False
pt_wf = nrow(pcos_data[pcos_data$PCOS..Y.N.==1 & pcos_data$Weight.gain.Y.N.==0,])
#PCOS False Weight gain True
pf_wt = nrow(pcos_data[pcos_data$PCOS..Y.N.==0 & pcos_data$Weight.gain.Y.N.==1,])
#PCOS True Weight gain True
pt_wt = nrow(pcos_data[pcos_data$PCOS..Y.N.==1 & pcos_data$Weight.gain.Y.N.==1,])

#generates the contingency table
contingency_table1 <- matrix(c(pf_wf,pt_wf,pf_wt,pt_wt),
                            nrow = 2,  # Number of rows
                            byrow = TRUE,  # Fill by rows
                            dimnames = list(
                              weight_gain = c("False", "True"),
                              PCOS_status = c("False","True")
                            ))
print(contingency_table1)

chi_test1 <- chisq.test(contingency_table1)
print(chi_test1)
##############################################################################

#get the values for each combination
#PCOS False Weight gain False
pf_ff = nrow(pcos_data[pcos_data$PCOS..Y.N.==0 & pcos_data$Fast.food..Y.N. ==0,])
#PCOS True Weight gain False
pt_ff = nrow(pcos_data[pcos_data$PCOS..Y.N.==1 & pcos_data$Fast.food..Y.N.==0,])
#PCOS False Weight gain True
pf_ft = nrow(pcos_data[pcos_data$PCOS..Y.N.==0 & pcos_data$Fast.food..Y.N.==1,])
#PCOS True Weight gain True
pt_ft = nrow(pcos_data[pcos_data$PCOS..Y.N.==1 & pcos_data$Fast.food..Y.N.==1,])

#generates the contingency table
contingency_table2 <- matrix(c(pf_ff,pt_ff,pf_ft,pt_ft),
                             nrow = 2,  # Number of rows
                             byrow = TRUE,  # Fill by rows
                             dimnames = list(
                               Fast_food = c("False", "True"),
                               PCOS_status = c("False","True")
                             ))
print(contingency_table2)

chi_test2 <- chisq.test(contingency_table2)
print(chi_test2)

################################################################################


#get the values for each combination
#PCOS False Weight gain False
pf_rf = nrow(pcos_data[pcos_data$PCOS..Y.N.==0 & pcos_data$Reg.Exercise.Y.N. ==0,])
#PCOS True Weight gain False
pt_rf = nrow(pcos_data[pcos_data$PCOS..Y.N.==1 & pcos_data$Reg.Exercise.Y.N.==0,])
#PCOS False Weight gain True
pf_rt = nrow(pcos_data[pcos_data$PCOS..Y.N.==0 & pcos_data$Reg.Exercise.Y.N.==1,])
#PCOS True Weight gain True
pt_rt = nrow(pcos_data[pcos_data$PCOS..Y.N.==1 & pcos_data$Reg.Exercise.Y.N.==1,])

#generates the contingency table
contingency_table2 <- matrix(c(pf_rf,pt_rf,pf_rt,pt_rt),
                             nrow = 2,  # Number of rows
                             byrow = TRUE,  # Fill by rows
                             dimnames = list(
                               Reg_exercise = c("False", "True"),
                               PCOS_status = c("False","True")
                             ))
print(contingency_table2)

chi_test2 <- chisq.test(contingency_table2)
print(chi_test2)




