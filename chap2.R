
library(ggplot2)
library(GGally)
#Exercice 14
####################################################################################################################
vitesse  = read.csv('C:/Users/LaurenceAMBASSA/Desktop/CLIENT 1/chapitre 2/Données de vitesse initiale ada.csv', sep=';')
vitesse$Area = as.factor(vitesse$Area)
boxplot(Velocity~Area, data = vitesse)

ggplot(vitesse, aes(Area, Velocity)) + geom_boxplot(notch = FALSE, fill = "cornflowerblue",
                                                    alpha = .7) + labs(title = 'distribution of the surface of the discharge hole as a function of the initial speed')
fit = aov(Velocity~Area, data = vitesse)
summary(fit)

#bonferroni method
library(multcomp)
fit = aov(Velocity~Area, data = vitesse)
doelev = as.integer(levels(vitesse$Velocity))
k = rbind(contrMat(table(vitesse$Area), 'Tukey'))
kht = glht(fit, linfct = mcp(Area = k))
summary(kht, test = adjusted('bonferroni'))
summary(kht)

#tukey method
TukeyHSD(fit, ordered = FALSE, conf.level = 0.95)

####################################################################################################################

#exercice 1
####################################################################################################################

pulp = read.csv('C:/Users/LaurenceAMBASSA/Desktop/CLIENT 1/chapitre 2/purple.csv', sep = ';')
pulp$categories = factor(pulp$categories)
row_mean = apply(pulp, MARGIN = 1, FUN = mean)
ggplot(pulp, aes(categories, values)) + geom_boxplot(notch = FALSE, fill = "cornflowerblue",
                                                    alpha = .7) + labs(title = 'Reflectance Data, Pulp Experiment')

fit_pulp = aov(values~categories, data = pulp)
summary(fit_pulp)

#intervalle à 95% pour le test de bonferroni
pairwise.t.test(pulp$values, pulp$categories, p.adjust = 'bonferroni')

#Tukey 99% 
TukeyHSD(fit_pulp, ordered = FALSE, conf.level = 0.99)

#bonferonni 99%
library(multcomp)
fit_pulp = aov(values~categories, data = pulp)
kht = glht(fit_pulp, linfct = 4)
summary(kht, adjusted('bonferroni'))



### fit ANCOVA model to data
data("litter")
amod <- aov(weight ~ dose + gesttime + number, data = litter)
### define matrix of linear hypotheses for `dose'
doselev <- as.integer(levels(litter$dose))
K <- rbind(contrMat(table(litter$dose), "Tukey"),
           otrend = c(-1.5, -0.5, 0.5, 1.5),
           atrend = doselev - mean(doselev),
           ltrend = log(1:4) - mean(log(1:4)))
### set up multiple comparison object
Kht <- glht(amod, linfct = mcp(dose = K), alternative = "less")
### cf. Westfall (1997, Table 2)
summary(Kht, test = univariate())
summary(Kht, test = adjusted("bonferroni"))
summary(Kht, test = adjusted("Shaffer"))
summary(Kht, test = adjusted("Westfall"))
summary(Kht, test = adjusted("single-step"))



pulp_b = aov(values~categories, data = pulp)
doelev = as.integer(levels(pulp$values))
k = rbind(contrMat(table(pulp$categories), 'Tukey'))
kht = glht(pulp_b, linfct = mcp(categories = k))
summary(kht, test = adjusted('bonferroni'))
summary(kht)

####################################################################################################################

#exercice 11
####################################################################################################################
#calcul de la p_value au niveau 0.01
p_value = 2*pf(8.98, df1 = 3, df2 = 26, lower.tail = FALSE)

#perform multiple comparisons of the four treatments at the 0.01 level
yA = 66.10
yB = 65.75
yC = 62.63
yD = 63.85
sigma = 2.39
nA = 7
nB = 8
nC = 9
nD = 6

t_AB = (yA-yB)/(sigma*(1/nA+1/nB))
t_AC = (yA-yC)/(sigma*(1/nA+1/nC))
t_AD = (yA-yD)/(sigma*(1/nA+1/nD))
t_BC = (yB-yC)/(sigma*(1/nB+1/nC))
t_BD = (yB-yD)/(sigma*(1/nB+1/nD))
t_CD = (yD-yC)/(sigma*(1/nC+1/nD))

t_AB
t_AC
t_AD
t_BC
t_BD
t_CD

####################################################################################################################

#exercice 12
####################################################################################################################

mandrel = read.csv("C:/Users/LaurenceAMBASSA/Desktop/CLIENT 1/chapitre 2/mandrel experience.csv", sep=";")
mandrel$Treatments = factor(mandrel$Treatments)
fit = aov(values~Treatments, data = mandrel)
summary(fit)

#Residuals analysis
plot(residuals(fit))
abline(h=c(-2, 0, 2), lty=c(2, 1, 2), col=c(1, 2, 1))

library(ggplot2)
library(ggpubr)
#Create a QQ plot of residuals
ggqqplot(residuals(fit))

#shapiro yest
shapiro.test(residuals(fit))

#perform multiple comparisons tukey of 3 treatments
TukeyHSD(fit, ordered = FALSE, conf.level = 0.95)

####################################################################################################################

#exercice 13
####################################################################################################################
strenght = read.csv("C:/Users/LaurenceAMBASSA/Desktop/CLIENT 1/chapitre 2/strenght data.csv", sep=";")
strenght$laser.power = factor(strenght$laser.power)
#Comparison Tukey method
fit = aov(values~laser.power, data = strenght)
summary(fit)
TukeyHSD(fit, ordered = FALSE, conf.level = 0.95)

####################################################################################################################

#exercice 15
####################################################################################################################
sprayer = read.csv2("C:/Users/LaurenceAMBASSA/Desktop/CLIENT 1/chapitre 2/Airsprayer.csv", sep=",")


####################################################################################################################

#exercice 16
####################################################################################################################

machines = read.csv("C:/Users/LaurenceAMBASSA/Desktop/CLIENT 1/chapitre 2/packing machines.csv", sep=";")
machines$treatments..machines. = factor(machines$treatments..machines.)
fit = aov(actual.weight~treatments..machines., data = machines)
summary(fit)


#exercice 17
####################################################################################################################
blood = read.csv("C:/Users/LaurenceAMBASSA/Desktop/CLIENT 1/chapitre 2/Blood Pressure Data.csv", header=T, sep=";")
str(blood)
blood$devices = factor(blood$devices)
blood$Doctors = factor(blood$Doctors)
model1 = lm(values1~devices, data = blood)
model2 = lm(values2~Doctors, data = blood)
anova(model1)
anova(model2)
anova(lm(values1~devices+Doctors*values2, data = blood))

















































