###############MODELOS PARAM?TRICOS#################

dialise<-read.csv("R/dialise.csv", header=T)
names(dialise)

require(survival)

x<-with(dialise, Surv(tempo,status))

diaweib<-survreg(x~idade, data=dialise, dist="weibull")
summary(diaweib)

congenita.simples<-survreg(x~idade + congenita, data=dialise, dist="weibull")
congenita.strata<-survreg(x~idade + strata(congenita), data=dialise, dist="weibull")
congenita.inter<-survreg(x~idade * strata(congenita), data=dialise, dist="weibull")

summary(congenita.simples)
summary(congenita.strata)
summary(congenita.inter)

#Par?metro de Forma

#Simples
1/congenita.simples$scale

#Estratificado
1/congenita.strata$scale

#Estratificado com intera??o
1/congenita.inter$scale

#Riscos relativos

#Simples
exp(-congenita.simples$coeff[-1])^(1/congenita.simples$scale)

#Estratificado para congenita=0 (sim)
exp(-congenita.strata$coeff[-1])^(1/congenita.strata$scale[1])

#Estratificado para congenita=1 (n?o)
exp(-congenita.strata$coeff[-1])^(1/congenita.strata$scale[2])

#Estratificado com intera??o para cong?nita=0 (sim)
exp(-sum(congenita.inter$coeff[2:3]))^(1/congenita.inter$scale[1])

#Estratificado com intera??o para cong?nita=1 (n?o)
exp(-sum(congenita.inter$coeff[2:3]))^(1/congenita.inter$scale[2])
