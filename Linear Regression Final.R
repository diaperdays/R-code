

#########################################################
#           Linear Regression Final                     #
#########################################################


rm(list=ls()) #clean up

setwd("~/Dropbox/EvalShareJustin/Data")
skills <- read.csv("skillstx.csv", header = TRUE)

skills$MOTIV <- (skills$motiv1 + skills$motiv2 + skills$motiv3)/3
skills$SKILLS <- (skills$skills1 + skills$skills2 + skills$skills3 + skills$skills4 + skills$skills5)/5

motivvars <-skills[c("motiv1","motiv2","motiv3")]
corr.test(motivvars)

skillsvars <-skills[c("skills1","skills2","skills3", "skills4", "skills5")]
corr.test(skillsvars)

alpha(motivvars, keys=NULL, title="alpha for motiv items")
alpha(skillsvars, keys=NULL, title="alpha for skills items")

SKILLS <- (skillsvars$skills1 + skillsvars$skills2 + skillsvars$skills3 + skillsvars$skills4 +skillsvars$skills5)/5
describe(SKILLS)

MOTIV <- (motivvars$motiv1 + motivvars$motiv2 + motivvars$motiv3)/3
describe(MOTIV)

perform<-(skills$perform)
describe(perform)

df<-data.frame(skills)
df$skills1<-NULL
df$skills2<-NULL
df$skills3<-NULL
df$skills4<-NULL
df$skills5<-NULL
df$motiv1<-NULL
df$motiv2<-NULL
df$motiv3<-NULL

treatment <- subset(df, tx==1)
control <- subset(df, tx==0)

library(car)
scatterplotMatrix(~perform+SKILLS+MOTIV, diagonal=c("histogram"), data=treatment, reg.line=lm, plot.points=TRUE, smooth=FALSE, main="Scatterplot Matrix")
scatterplotMatrix(~perform+SKILLS+MOTIV, diagonal=c("histogram"), data=control, reg.line=lm, plot.points=TRUE, smooth=FALSE, main="Scatterplot Matrix")

lm1 <-lm(SKILLS~tx + MOTIV + tx*MOTIV, data=df)
summary(lm1)
Anova(lm1)

lm2 <-lm(perform~tx + MOTIV + tx*MOTIV, data=df)
summary(lm2)
Anova(lm2)

mean(df$MOTIV)

df$MOTIV4.129 <-df$MOTIV-4.129

mix1 <-lm(perform ~ tx + MOTIV4.129,data=df)
summary(mix1)

mix2 <-lm(perform ~ tx + MOTIV4.129 + tx*MOTIV4.129 + MOTIV4.129, data=df)
summary(mix2)

mix3 <-lm(SKILLS~ tx + MOTIV4.129 + tx*MOTIV4.129 + MOTIV4.129, data=df)
summary(mix3)

MOTIV4.5<-subset(df,MOTIV>4.5)

mix4 <-lm(perform~tx, data=MOTIV4.5)
summary(mix4)
anova(mix4)
confint(mix4, level=.95)

mix5 <-lm(SKILLS~tx, data=MOTIV4.5)
summary(mix5)
anova(mix5)
confint(mix5, level=.95)

mix6 <-lm(perform~SKILLS +tx, data=MOTIV4.5)
summary(mix6)
anova(mix6)
confint(mix6, level=.95)

model.xy <- lm(perform ~ tx, data=MOTIV4.5)
summary(model.xy)

model.xm <- lm(SKILLS~tx, data=MOTIV4.5)
summary(model.xm)

model.xmy <- lm(perform~SKILLS+tx, data=MOTIV4.5)
summary(model.xmy)
library(boot)
coefficients(model.xm)
coefficients(model.xmy)
path.a <-coefficients(model.xm)["tx"]
path.b <-coefficients (model.xmy)["SKILLS"]
se.path.a <-sqrt(vcov(model.xm)["tx","tx"])
se.path.b <-sqrt(vcov(model.xmy)["SKILLS","SKILLS"])
path.a
path.b
se.path.a
se.path.b
ab<-path.a*path.b
ab

Sobel <- function (a, b, sa, sb, conf) {
  sab <- sqrt(a*a*sb*sb + b*b*sa*sa)
  Sobelz <- a*b/sab
  pvalue <- 2*pnorm(abs(Sobelz), lower.tail=FALSE)
  alphaL <- (1-conf)/2
  alphaU <- 1- alphaL
  lvalue <- a*b+qnorm(alphaL)*sab
  uvalue <- a*b+qnorm(alphaU)*sab
  list(sab=sab,Sobelz=Sobelz,lower=lvalue,upper=uvalue,pvalue=pvalue)
}
Sobel(path.a, path.b, se.path.a, se.path.b, conf=.95)

#bootstrap
boot.med <- function(data, indices){
  data <-data[indices,]  
  
  reg1 <- lm(SKILLS~tx, data=data)
  reg2 <- lm(perform~SKILLS+tx, data=data)
  
  a <- coefficients(reg1)["tx"]	#the a path coefficient
  b <- coefficients(reg2)["SKILLS"]	#the b path coefficient
  a*b						#returns the product of a*b
}
medboot<-boot(data=MOTIV4.5, statistic = boot.med, R=5000)	
medconfint <- boot.ci(medboot, index=1,conf=(.95), type=c("bca"))
print (medconfint)

