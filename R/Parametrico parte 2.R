#id=identificação
#ini=data do diagnostico da Aids (em dias)
#fim=data do obito
#tempo=dias de sobrevivencia do diagnostico ate o obito
#status= 0(censura);1(obito)
#sexo=F(feminino); M(masculino)
# escola=0(sem escolaridade);1(ensino fundamental); 2(ensino medio); 3(ensino superior)
# idade=idade na data do diagnostico (20 a 68 anos)
# risco=0(homosexual masculino);1(usuario de drogas injetaveis); 2(transfusao); 3(contato sexual
#com HIV +); 5(hetero com multiplos parceiros); 6(dois fatores de risco)
#acompan=acompanhamento: 0=ambulatorial/hospital-dia, 1=internação posterior, 2=internação imediata
#obito: S=obito; N=nao obito; I=ignorado
#anotrat: ano do início do tratamento (1990 a 2000), 9=sem tratamento
#tratam terapia antiretroviral: 0=nenhum, 1=mono, 2=combinada, 3=potente
#doenca: de apresentacao: 1=pcp; 2=pcp pulmonar; 3=pcp disseminada; 4=toxoplasmose; 5=sarcoma; 7=outra doeca
#8=candidiase; 9=duas doencas; 10=herpes, 99=definido por cd4
#propcp=profilaxia para pneumocistis: 0=sem profilaxia; 2=primaria; 3=secundaria, 4=ambas


hiv<-read.csv("ipec.csv", header=T, sep=";")
names(hiv)

library(survival)

x<-Surv(hiv$tempo,hiv$status)

#Distribuição exponencial

mod.exp<-survreg(x~idade+sexo+tratam, data=hiv, dist = "exponential")
summary(mod.exp)

exp(-6.11597) #cálculo do alfa

exp(-mod.exp$coeff[-1]) #cálculo do Risco Relativo


#Distribuição Weibull

mod.wei<-survreg(x~idade+sexo+tratam, data=hiv, dist = "weibull")
summary(mod.wei)

exp(- 6.06842) #cálculo do alfa
exp(-0.14185) #cálculo de gama (forma)

exp(-mod.wei$coeff[-1])^(1/mod.wei$scale) #cálculo do Risco Relativo


#ANOVA

anova(mod.exp, mod.wei)

#Medida da Qualidad do Ajuste (Cálculo da Deviance)

#Exponencial

mod.exp.dev<-sum(resid(mod.exp, type="deviance")^2)
mod.exp.dev
gl<-193-3-1
gl
1-pchisq(mod.exp.dev,gl)

#Weibull

mod.wei.dev<-sum(resid(mod.wei, type="deviance")^2)
mod.wei.dev
gl<-193-3-1
gl
1-pchisq(mod.wei.dev,gl)


# Análise gráfica (contruir KM estratificado)

KME.sexo<-survfit(x~hiv$sexo,data=hiv)
plot (KME.sexo, conf.int=F, fun="cumhaz", xlab="Dias", ylab="Risco Acumulado", 
      mark.time=F, lty=c(1,2), col = 1:2)

legend (x="topleft", legend=c("Fem", "Masc"), 
        lty = c(1,2),col = 1:2, title="Sexo")

#Exemplo banco Diálise

plot (KME.diabetes, conf.int=F, fun="cumhaz", xlab="Dias", ylab="Risco Acumulado", 
      mark.time=F, lty=c(1,2), col = 1:2)

legend (x="topleft", legend=c("sem diabetes", "com diabetes"), 
        lty = c(1,2),col = 1:2, title="Diabetes")

#Análise de Resíduo

#Exponencial

res.ldcase.exp<-residuals (mod.exp, type="ldcase")
plot(res.ldcase.exp, main = "Vetor de Parâmetros", xlab="Índice")

res.ldresp.exp<-residuals (mod.exp, type="ldresp")
plot(res.ldresp.exp, main = "Valores Preditos", xlab="Índice")

res.ldshape.exp<-residuals (mod.exp, type="ldshape")
plot(res.ldshape.exp, main = "Parâmetros de Forma", xlab="Índice")

identify(res.ldcase.exp)
identify(res.ldresp.exp)
identify(res.ldshape.exp)

names(hiv)
hiv[c(9,10,49,82, 182), c(4,5,6,8,13)]

#Weibull

res.ldcase.wei<-residuals (mod.wei, type="ldcase")
plot(res.ldcase.wei, main = "Vetor de Parâmetros", xlab="Índice")

res.ldresp.wei<-residuals (mod.wei, type="ldresp")
plot(res.ldresp.wei, main = "Valores Preditos", xlab="Índice")

res.ldshape.wei<-residuals (mod.wei, type="ldshape")
plot(res.ldshape.wei, main = "Parâmetros de Forma", xlab="Índice")

identify(res.ldcase.wei)
identify(res.ldresp.wei)
identify(res.ldshape.wei)

#Retirando o 82

modelo2<-(survreg(x~idade+sexo+tratam, data=hiv, dist = "weibull", subset =-82))
summary(modelo2)

res.ldcase.modelo2<-residuals (modelo2, type="ldcase")
plot(res.ldcase.modelo2, main = "Vetor de Parâmetros", xlab="Índice")

res.ldresp.modelo2<-residuals (modelo2, type="ldresp")
plot(res.ldresp.modelo2, main = "Valores Preditos", xlab="Índice")

res.ldshape.modelo2<-residuals (modelo2, type="ldshape")
plot(res.ldshape.modelo2, main = "Parâmetros de Forma", xlab="Índice")

identify(res.ldcase.modelo2)
identify(res.ldresp.modelo2)
identify(res.ldshape.modelo2)         
