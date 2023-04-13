#importation des donn?es
data = read.csv('Ericksen.csv', sep = ';')
head(data)
str(data)
#diagramme de dispersion
pairs(data[c('Minority', 'Crime', 'Poverty', 'Language', 'High.school', 'Housing', 'Conventional', 'Undercount')])

#anlayse de la regression
modele = lm(Undercount~Minority+Crime+Poverty+Language+High.school+Housing+Conventional, data = data)
summary(modele)
#les predicteurs qui semblent etre les plus important sont ceux avec un param?tre significativement different de zero
#au risque de 95%. parmis ces predicteurs nous avons Minority, Conventional, Crime, Poverty et Language.

summary(modele)$coefficients
#intervales de confiance
confint(modele)
#prevision d'une valeur ulterieure
predict(modele, data.frame(Minority=10, Crime=50, Poverty=20, Language=1, High.school=30, Housing=20, Conventional=50),
                           interval='prediction')

predict(modele, data.frame(Minority=10, Crime=50, Poverty=20, Language=1, High.school=30, Housing=20, Conventional=50),
                           interval='confidence')

#analyse des residus
residus = rstudent(modele)
val_prdict = predict(modele)
#fraphique des vaeurs predictes et des residus 
plot(val_prdict, residus, xlab = "predictives values", ylab = "residuals")
abline(h=c(-2, 0, 2), lty=c(2, 1, 2), col=c(1, 2, 1))

#test de normalit? des residus
shapiro.test(residus)
hist(residus)

full = lm(Undercount~Minority+Crime+Poverty+Language+High.school+Housing+Conventional, data = data)
null = lm(Undercount~1, data = data)
back = step(full, direction = 'backward')
formulas(back)

forw = step(null, scope = list(lower = null, upper = full), direction = 'forward', trace = 1)

#comparaison des modeles
modele0 = lm(Undercount~Minority+Crime+Poverty+Language+Conventional, data = data)
summary(modele0)
modele1 = lm(Undercount~Minority+Crime+Poverty+Language+Conventional+High.school, data = data)
summary(modele1)

anova(modele0, modele1)


#Exercice 14: consommation d'essence
data0 = read.csv("données sur la consommation d'essence.csv",
                 sep = ';')
#diagramme de dispersion
pairs(data0)
str(data0)
cor(data0)
#apr?s observation du diagramme de dispersion, il est difficile de deceler une relation lineaire entre les cocvariables
#et la variable y affectant la consommation d'essence

#anamlyse de la regression
modele2 = lm(Y~X_1+X_2+X_3+X_4, data = data0)
summary(modele2)

summary(modele2)$coefficients

#utilisation de diff?rentes methodes pour la selection du meilleur mod?le
full = lm(Y~X_1+X_2+X_3+X_4, data = data0)
null = lm(Y~1, data = data0)

back = step(full, direction = 'backward')
forw = step(null, scope = list(lower = null, upper = full), direction = 'forward', trace = 1)
forw
back

#analyse des residus
residus = rstudent(modele2)
val_predict = predict(modele2)
plot(val_predict, residus, xlab = "predictives values", ylab = "residuals")
abline(h=c(-2, 0, 2), lty=c(2, 1, 2), col=c(1, 2, 1))

#test de normalit? des residus
shapiro.test(residus)
hist(residus)








