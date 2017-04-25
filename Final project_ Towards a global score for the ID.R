setwd("C:/Users/Vector/Desktop/Proyecto")
library(foreign)
depresion <- read.spss("Depresion_PICDI.sav", to.data.frame=TRUE)


sexo <- depresion$SEXO
library(gdata)
sex <- drop.levels(sexo)#funcion que elimina niveles no usados en un factor(No aplicable)
rm(sexo)
age <- depresion$EDAD
suicide <- depresion$SUICIDIO
centre <- depresion$CENTRO
academic <- depresion$ESTUDIOS
IDnum <- depresion$NDI35
sample <- depresion$Clin_Control

descriptives <- data.frame(sex, age, suicide, centre, academic,IDnum, sample)
rm(sex, age, suicide, centre, academic,IDnum, sample)
##
descriptives$study <- "A"

###########

library(pscl)
library(lmtest)
library(ggplot2)
library(MASS)
library(alr3)
library(plyr)

bdi <- depresion$BDI
pdi <- depresion$PDI35
data <- as.data.frame(bdi)
data$pdi <- pdi
data$picdi <- depresion$PICDI35
data$idi <- depresion$IDI35
data$icdi <- depresion$ICDI35
data$num <- depresion$NDI35
data$perfil <- depresion$PERFIL
data$age <- depresion$EDAD
data$pol <- depresion$POLARIZACIÓN
data$ind <- depresion$INDEFINICION
data$sui <- depresion$Cat_sui
data$sex <- depresion$SEXO
data$civ <- depresion$ESTADO.CIVIL
data$est <- depresion$ESTUDIOS
data$lab <- depresion$SIT.LABORAL
data$lpdi <- log1p(pdi)
data$sample <- depresion$Clin_Control
data$pcd <- depresion$PCD
data$eeag <- depresion$EEAG
data$numdc <- depresion$NUMERO.CD
data$disc <- depresion$DIST.YO_IDEAL
data$self_ideal <- depresion$YO_IDEAL
data$self_others <- depresion$YO_OTROS
data$ideal_others <- depresion$IDEAL_OTROS
data$subject <- depresion$CODIGO


data$cons.style <- revalue(data$perfil, c("Ninguno"="None", "Positividad"="Positivity",
                                          "Negatividad"="Negativity", "Resentimiento"="Resentment",
                                          "Superioridad"="Superiority", "Aislamiento"="Isolation",
                                          "Baja Autoestima"="Low Self-Esteem"))

data$cons.style[data$cons.style=="Low Self-Esteem"] <- "None" #Quitar low self esteem
data$cons.style <- droplevels(data$cons.style) #y borrar "low self esteem"


# Recode all NAs. Pero si hago esto se fastidian las asunciones
data[c(180, 188,189, 193, 195, 196, 199,226, 227, 228, 229,
       230, 252, 258, 261), "cons.style"] <- "None"

data[c(194, 198, 237,240, 241, 242), "cons.style"] <- "Positivity"

data[c(192, 197, 201, 259, 262, 239), "cons.style"] <- "Superiority"

data[238, "cons.style"] <- "Resentment"

#los que ya estaban codificados, estaban siguiendo un procedimiento antiguo. Es una medida que no vale

##########

#GRÁFICOS DESCRIPTIVOS

##############

#grid

library(OpenRepGrid)
bertin(feixas2004, color=c("darkblue", "lightblue"), ylim= c(0.05, .85), cex.elements=.5,
       cex.constructs=.54, cex.text=.4, margins=c(0.045, 1, 1))

#density PDI

library(ggplot2)
library(grid)
library(gridExtra)

p1 <-ggplot(depresion, aes(x=PDI35), colour="black") +
  geom_density(colour="black", fill="lightgreen")
  

p2 <- ggplot(depresion, aes(x=IDI35), colour="black") +
  geom_density(colour="black", fill="lightgreen")

p3 <- ggplot(depresion, aes(x=ICDI35), color="black") +
  geom_density(colour="black", fill="lightgreen")

p4 <- ggplot(depresion, aes(x=PICDI35), color="black") +
  geom_density(colour="black", fill="lightgreen")

grid.arrange(p1, p2, p3, p4, ncol = 2, main = "Distribution of the global scores in the sample")

#sex

library(ggplot2)
library(plyr)

descriptives$sex <- revalue(descriptives$sex, c("Hombre"="Male", "Mujer"="Female"))

ggplot(descriptives, aes(x=study,fill=sex)) + 
  geom_bar(colour="black")+
  guides(fill=guide_legend(reverse=T))#+
geom_text(aes(x=study), colour="white")


#clin-control, no en el texto

ce <- ddply(descriptives, "sample", transform,
            percent_weight = Weight / sum(Weight)*100)

ggplot(descriptives, aes(x=sample)) +
  geom_bar(fill="blue")
  
#distancia yo-ideal

data$sample <- revalue(data$sample, c("clinica"="Clinic", "control"="Control"))

summary(data$disc)

ggplot(data, aes(x=disc, fill=sample)) +
  geom_histogram(aes(y = ..density..),color="black", bg="yellow", fill=sample,alpha=1)+
  geom_density(alpha=.35)+
  xlab("Self - Ideal Discrepancy")+
  ylab("Density")

ggplot(data, aes(x=disc))+
  geom_histogram()
  

ggplot(data, aes(x=sample, y=disc))+
  geom_boxplot()


#indices
#PDI
ggplot(data, aes(x=pdi, fill=sample)) +
  geom_histogram(aes(y = ..density..),color="black", bg="yellow", fill=sample, size=.2,
                 binwidth=.8)+
  geom_density(alpha=.5)+
  #ylim(0,1)+
  xlim(0, 15)
#PICDI
ggplot(data, aes(x=picdi, fill=sample)) +
  geom_histogram(aes(y = ..density..),color="black", bg="yellow", fill=sample,alpha=.5, size=.2)+
  geom_density(alpha=.5)



#polarización

summary(data$pol)

ggplot(data, aes(x=pol, y=bdi, colour=sample, shape=sample)) +
  geom_point()
  


##############################

#RESULTS

##############################

############ROC

library(pROC)

rPDI <- roc(Clin_Control ~ PDI35, depresion)
rIDI <- roc(Clin_Control ~ IDI35, depresion)
rICDI <- roc(Clin_Control ~ ICDI35, depresion)
rPICDI <- roc(Clin_Control ~ PICDI35, depresion)
rnum <- roc(Clin_Control ~ NDI35, depresion)
rbdi <- roc(Clin_Control ~ BDI, depresion)


#CURVES

plot(rIDI, print.thres=F, col="grey",main="Comparison of the indexes with number of IDs",
     xlab="1 - Specificity", xaxp =c(1, 0, 1))
plot(rPDI, print.thres=F,add=T, col="red")
plot(rICDI, print.thres=F, add=T, col="lightskyblue1")
plot(rPICDI, print.thres=F, add=T, col="orange")
plot(rnum, add=T, col="green")
#plot(rbdi, add=T, col="yellow")
plot(rinter, add=T, col="black")
legend("bottomright", legend = c("IDI", "PDI", "ICDI", "PICDI", "Number of IDs"), col = c("grey", "red","lightskyblue1", "orange", "green"),lwd = 2)

#AUC's confint

aucpdi <- ci.auc(rPDI)
aucpicdi <- ci.auc(rPICDI)
aucnum <- ci.auc(rnum)
aucicdi <- ci.auc(rICDI)
aucidi <- ci.auc(rIDI)
aucs <- data.frame(aucpdi, aucpicdi, aucicdi,aucnum,aucidi)
aucs <- t(aucs)
dfauc <- as.data.frame(aucs)
rownames(dfauc) <- c("PDI", "PICDI", "ICDI", "Number of IDs", "IDI")
colnames(dfauc) <- c("Lower bound CI", "Estimate", "Higher bound CI")
dfauc <- round(dfauc, digits=3)
dfauc

#ROC tests, AUCs comparisons following DeLong's method

a1 <-roc.test(rIDI,rICDI)#IDI
a2 <-  roc.test(rIDI, rPICDI)
a3 <-  roc.test(rIDI,rPDI)
a4 <-  roc.test(rIDI,rnum)

a5 <-roc.test(rnum, rICDI)
a6 <-  roc.test(rnum,rPICDI)
a7 <-  roc.test(rnum, rPDI)#num

a8 <-roc.test(rICDI, rPDI)#ICDI
a9 <-  roc.test(rICDI,rPICDI)

a10 <-roc.test(rPDI, rPICDI)#PDI

tests <- data.frame(a1$statistic,a2$statistic,a3$statistic,a4$statistic, a5$statistic, a6$statistic,
           a7$statistic, a8$statistic, a9$statistic, a10$statistic)

tests <- t(tests)
tests <- as.data.frame(tests)

colnames(tests) <- c("Z-score")
rownames(tests) <- c("IDI-ICDI", "IDI-PICDI", "IDI-PDI", "IDI-num", "num-ICDI", "num-PICDI", 
                     "num-PDI", "ICDI-PDI", "ICDI-PICDI", "PDI-PICDI")

x <- as.numeric(c(a1$p.value, a2$p.value, a3$p.value, a4$p.value, a5$p.value, a6$p.value,
                    a7$p.value, a8$p.value, a9$p.value, a10$p.value))

tests$p.value <- x

tests <- round(tests, digits=4)

rm(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,x)

tests

#Cutting points---specificities-sensitivities

#PDI

PDI.sens <- rPDI$sensitivities
PDI.spec <- rPDI$specificities
PDI.thre <- rPDI$thresholds
roc.PDI <- data.frame(PDI.thre, PDI.sens, PDI.spec)
rm(PDI.sens, PDI.spec, PDI.thre)

summary(depresion$PDI35)
round(roc.PDI[c(114:111, 101:98, 90:88, 43:40), ], digits=3)

#PICDI

PICDI.sens <- rPICDI$sensitivities
PICDI.spec <- rPICDI$specificities
PICDI.thre <- rPICDI$thresholds
roc.PICDI <- data.frame(PICDI.thre, PICDI.sens, PICDI.spec)
rm(PICDI.sens, PICDI.thre, PICDI.spec)

roc.PICDI

round(roc.PICDI[c(140:137, 124:123, 95:93, 53:50), ], digits=3)

#ggplot(depresion, aes(x=PDI35), color="black") +
#  geom_bar(binwidth=1.5, fill="orange", color="black")

#PICDI

PICDI.sens <- rPICDI$sensitivities
PICDI.spec <- rPICDI$specificities
PICDI.thre <- rPICDI$thresholds
roc.PICDI <- data.frame(PICDI.thre, PICDI.sens, PICDI.spec)
  
summary(depresion$PICDI35)
round(roc.PICDI, digits=3)


################################################BDI


ggplot(data, aes(x=bdi, fill=sample)) +
  geom_histogram(color="black", position="identity", alpha=.6, binwidth=3)+
  scale_fill_brewer(palette="Dark2")





#Model 1

#Model 1. PDI
#I tried to include proportion of dilemmatic constructs. the model is still good. No effects of PDC

b1 <- lm(bdi ~ pdi + pol + disc + pdi*pol, data=data)

plot(b1)
anova(b1) 
st1 <- studres(b1)
shapiro.test(st1)

b2 <- lm(bdi ~ pdi + pol + disc, data=data)

plot(b2)

summary(b2)

library(relaimpo)

rela <- calc.relimp(b2, rela=F)
round(rela$lmg, digits=3)


vif(b1)

stud.res.b2 <- studres(b2)


stud.res.b2 <- as.numeric(stud.res.b2)
b2.graph <- as.data.frame (stud.res.b2)

b2.graph$stand.res.b2 <- stdres(b2) #standardized residuals
b2.graph$stand.pred.b2 <- scale(b2$fitted.values)
b2.graph$subject <- data$subject

#Scatterplot for homocedasticity

bptest(b2)

ggplot(b2.graph, aes(x=stand.pred.b2, y=stand.res.b2))+
  geom_point(size=2)+
  xlab("Regression Standardized Predicted Value")+
  ylab("Regression Standardized Residual")+
  theme_bw()+
  ggtitle("Scatterplot Std Residuals vs. Std Fitted values")+
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic"))
  


#Histogram for normality
ggplot(b2.graph, aes(x=stud.res.b2, y=..density..)) +
  geom_histogram(colour="black", fill="red") +
  geom_line(stat="density", size=1) +
  xlab("Regression Studentized Residuals") +
  ylab("Frequency") +
  theme_bw() +
  ggtitle("Histogram of Standardized Residuals") +
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#annotate("text", x=4.35, y=1.5, label="Studentized Breusch-Pagan test",
# fontface="italic", size=6) +
#annotate("text", x=4.35, y=1.2, label="p.value = 0.04",
# fontface="italic", size=6)
#annotate("text", x=1, y=0.6, label="Shapiro-Wilk test p-value < 0.001",
        # fontface="italic", size=6)

shapiro.test(stud.res.b2)


qqnorm(stud.res.b2, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(stud.res.b2)



plot(b2)

#remove outlier. I think it is not neccesary
noOut <- data[- 49, ]

b3 <- lm(bdi ~ pdi + pol + disc + pdi*pol, data=noOut)#this model with the interaction is better

plot(b3)
anova(b3)

st33 <- studres(b3)
shapiro.test(st33)


bonferroni:
  0.05/4

summary(b3)

rela <- calc.relimp(b3, rela=F)
round(rela$lmg, digits=3)


#plots of assumptions...data no outlier. the shapiro wilk does not get better

stud.res.b3 <- studres(b3)

shapiro.test(stud.res.b3)

ggplot(b3.graph, aes(x=stud.res.b2, y=..density..)) +
  geom_histogram(colour="black", fill="red") +
  geom_line(stat="density", size=1) +
  xlab("Regression Studentized Residuals") +
  ylab("Frequency") +
  theme_bw() +
  ggtitle("Histogram of Standardized Residuals") +
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


qqnorm(stud.res.b3, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(stud.res.b3)


bptest(b3)

b3.graph$stand.res.b3 <- stdres(b3) #standardized residuals
b3.graph$stand.pred.b3 <- scale(b3$fitted.values)

ggplot(b2.graph, aes(x=stand.pred.b2, y=stand.res.b2))+
  geom_point(size=2)+
  xlab("Regression Standardized Predicted Value")+
  ylab("Regression Standardized Residual")+
  theme_bw()+
  ggtitle("Scatterplot Std Residuals vs. Std Fitted values")+
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic"))

plot(b2)


##With PICDI

b45 <- lm(bdi ~ picdi + pol + disc + picdi*pol, data=data)

anova(b45)

b4 <- lm(bdi ~ picdi + disc + picdi*pol, data=data)
anova(b4)

summary(b4)

rela <- calc.relimp(b4, rela=F)
round(rela$lmg, digits=3)

plot(b4)

stud.res.b4 <- studres(b4)

stud.res.b4 <- as.numeric(stud.res.b4)
b4.graph <- as.data.frame (stud.res.b4)

b4.graph$stand.res.b4 <- stdres(b4) #standardized residuals
b4.graph$stand.pred.b4 <- scale(b4$fitted.values)
b4.graph$subject <- data$subject

#Homocesdascity

bptest(b4)

ggplot(b4.graph, aes(x=stand.pred.b4, y=stand.res.b4))+
  geom_point(size=2)+
  xlab("Regression Standardized Predicted Value")+
  ylab("Regression Standardized Residual")+
  theme_bw()+
  ggtitle("Scatterplot Std Residuals vs. Std Fitted values")+
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic"))
 



#Normality



ggplot(b4.graph, aes(x=stud.res.b4, y=..density..)) +
  geom_histogram(colour="black", fill="red") +
  geom_line(stat="density", size=1) +
  xlab("Regression Studentized Residuals") +
  ylab("Frequency") +
  theme_bw() +
  ggtitle("Histogram of Standardized Residuals") +
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#annotate("text", x=4.35, y=1.5, label="Studentized Breusch-Pagan test",
# fontface="italic", size=6) +
#annotate("text", x=4.35, y=1.2, label="p.value = 0.04",
# fontface="italic", size=6)
#annotate("text", x=1, y=0.6, label="Shapiro-Wilk test p-value < 0.001",
# fontface="italic", size=6)

shapiro.test(stud.res.b4)

qqnorm(stud.res.b4, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(stud.res.b4)
#annotate("text", x=1, y=0.6, label="Shapiro-Wilk test p-value < 0.001",
# fontface="italic", size=6)


#same outlier

noOut <- data[- 49, ]

b5 <- lm(bdi ~ picdi + pol + disc + picdi*pol, data=noOut)#this model with the interaction is better

plot(b5)
anova(b5)

bptest(b5)

st5 <-studres(b5)
shapiro.test(st5)


bonferroni:
  0.05/4

summary(b5)



#compare the two models

anova(b2, b4)
rela <- calc.relimp(b2, rela=F)
rela2 <- calc.relimp(b4, rela=F)

anova(rela$lmg, rela2$lmg)

rela$lmg
rela2$lmg

######################################
#EEAG


ggplot(data, aes(x=eeag))+
  geom_bar(binwidth=3, color="black", fill="lightgreen")

summary(data$eeag)


e1 <- lm(eeag ~ pdi + disc + pol + pdi*pol, data=data)#this model with the interaction is better

summary(e1)
anova(e1)

e2 <- lm(eeag ~ pdi + disc, data=data)

anova(e2)
summary(e2)


relai <- calc.relimp(e2, rela=F)
round(relai$lmg, digits=3)


plot(e2) #problems with 99?


stud.res.e2 <- studres(e2)

stud.res.e2 <- as.numeric(stud.res.e2)
e2.graph <- as.data.frame (stud.res.e2)

e2.graph$stand.res.e2 <- stdres(e2) #standardized residuals
e2.graph$stand.pred.e2 <- scale(e2$fitted.values)
e2.graph$subject <- data$subject



ggplot(e2.graph, aes(x=stud.res.e2, y=..density..)) +
  geom_histogram(colour="black", fill="red") +
  geom_line(stat="density", size=1) +
  xlab("Regression Studentized Residuals") +
  ylab("Frequency") +
  theme_bw() +
  ggtitle("Histogram of Standardized Residuals") +
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

shapiro.test(stud.res.e2)

qqnorm(stud.res.e2, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles
       Shapiro - Wilk test <.05", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(stud.res.e2)

bptest(e2)

ggplot(e2.graph, aes(x=stand.pred.e2, y=stand.res.e2))+
  geom_point(size=2)+
  xlab("Regression Standardized Predicted Value")+
  ylab("Regression Standardized Residual")+
  theme_bw()+
  ggtitle("Scatterplot Std Residuals vs. Std Fitted values")+
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic"))
  

###################without outliers

myData <- data[-c(99),]


e3 <- lm(eeag ~ pdi + disc, data=myData)

anova(e3)
summary(e3)

vif(e3)

relas <- calc.relimp(e3, rela=F)
round(relas$lmg, digits=3)



#Plots of assumptions
sres.e3 <- studres(e3)
shapiro.test(sres.e3)

qqnorm(sres.e3, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles
       Shapiro - Wilk test = 0.480", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(sres.e3)



e3.graph <- as.data.frame (sres.e3)

e3.graph$stand.res.e3 <- stdres(e3) #standardized residuals
e3.graph$stand.pred.e3 <- scale(e3$fitted.values)
e3.graph$subject <- data$subject


ggplot(e3.graph, aes(x=stand.pred.e3, y=stand.res.e3))+
  geom_point(size=2)+
  xlab("Regression Standardized Predicted Value")+
  ylab("Regression Standardized Residual")+
  theme_bw()+
  ggtitle("Scatterplot Std Residuals vs. Std Fitted values")+
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic"))

### PICDI

e4 <- lm(eeag ~ picdi + disc + pol + picdi*pol, data=myData)#this model with the interaction is better

anova(e4)
summary(e4)


e45 <- lm(eeag ~ picdi + disc, data=data)

e5 <- lm(eeag ~ picdi + disc, data=myData)

anova(e5)
summary(e5)

relas <- calc.relimp(e5, rela=F)
round(relas$lmg, digits=3)

bptest(e5)

#Assumptions

sres.e5 <- studres(e5)

shapiro.test(sres.e5)

qqnorm(sres.e5, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles
       Shapiro - Wilk test = 0.215", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(sres.e5)

plot(e5)



stud.res.e5 <- studres(e5)

stud.res.e5 <- as.numeric(stud.res.e5)
e5.graph <- as.data.frame (stud.res.e5)

e5.graph$stand.res.e5 <- stdres(e5) #standardized residuals
e5.graph$stand.pred.e5 <- scale(e5$fitted.values)
e5.graph$subject <- data$subject

#Homocesdascity

bptest(e5)

ggplot(e5.graph, aes(x=stand.pred.e5, y=stand.res.e5))+
  geom_point(size=2)+
  xlab("Regression Standardized Predicted Value")+
  ylab("Regression Standardized Residual")+
  theme_bw()+
  ggtitle("Scatterplot Std Residuals vs. Std Fitted values")+
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic"))




#Normality



ggplot(b4.graph, aes(x=stud.res.b4, y=..density..)) +
  geom_histogram(colour="black", fill="red") +
  geom_line(stat="density", size=1) +
  xlab("Regression Studentized Residuals") +
  ylab("Frequency") +
  theme_bw() +
  ggtitle("Histogram of Standardized Residuals") +
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#annotate("text", x=4.35, y=1.5, label="Studentized Breusch-Pagan test",
# fontface="italic", size=6) +
#annotate("text", x=4.35, y=1.2, label="p.value = 0.04",
# fontface="italic", size=6)
#annotate("text", x=1, y=0.6, label="Shapiro-Wilk test p-value < 0.001",
# fontface="italic", size=6)

shapiro.test(stud.res.b4)

qqnorm(stud.res.b4, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(stud.res.b4)
#annotate("text", x=1, y=0.6, label="Shapiro-Wilk test p-value < 0.001",
# fontface="italic", size=6)















































##without outlier

myData <- data[-c(99),]


e6 <- lm(eeag ~ pdi + disc, data=myData)

anova(e6)
summary(e6)

bptest(e6)

sres.e6 <- studres(e6)
shapiro.test(sres.e6)

vif(e6)

library(relaimpo)
q <- calc.relimp(e6, rela=F)






e2 <- lm(eeag ~ pdi + perfil + pol + ind + pdi*pol, data=data)

anova(e2)
bptest(e2)

sres.e2 <- studres(e2)
shapiro.test(sres.e2)
layout(matrix(c(1,2,3,4),2,2))
plot(e1)

summary(e2)

vif(e1)

library(relaimpo)
calc.relimp(b2, rela=F)


#####E3 EEAG with PICDI without outlier

myData <- data[-c(99),]

e3 <- lm(eeag ~ picdi + pol + picdi*pol + ind, data=myData)

anova(e3)
bptest(e3)

sres.e3 <- studres(e3)
shapiro.test(sres.e3)
layout(matrix(c(1,2,3,4),2,2))
plot(e1)

summary(e3)

vif(e1)

library(relaimpo)
calc.relimp(b2, rela=F)


##Assumptions

stud.res.e1 <- studres(e1)
stud.res.e1 <- as.numeric(stud.res.e1)
e1.graph <- as.data.frame (stud.res.e1)

shapiro.test(stud.res.e1)

ggplot(e1.graph, aes(x=stud.res.e1, y=..density..)) +
  geom_histogram(colour="black", fill="red") +
  geom_line(stat="density", size=1) +
  xlab("Regression Studentized Residuals") +
  ylab("Frequency") +
  theme_bw() +
  ggtitle("Histogram of Standardized Residuals") +
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


qqnorm(stud.res.e1)
qqline(stud.res.e1)

#would be useful to label the outlier (49)

plot(e1)#check the leverage.plot

#remove the 99, why remove it?

summary(data[99, ])

myData <- data[-c(99),]

summary(data$eeag[99])
summary(data$eeag)

summary(data$pdi[99])
summary(data$pdi)

summary(data$pol[99])
summary(data$pol)

#A very low score in EEAG, and a very high score in both pdi and polarization.


#########


e2 <- lm(eeag ~ pdi + perfil + pol + ind + age + pdi*pol, data=data)
anova(e2)

plot(e2)

summary(e2)

sres.e2 <- studres(e2)

shapiro.test(sres.e2)

bptest(e2)

#### remove not useful predictors

e3 <- lm(eeag ~ pdi + perfil + pol, data=myData)#it does improve significantly

sres.e3 <- studres(e3)

shapiro.test(sres.e3)

bptest(e3)

summary(e3)

library(relaimpo)
calc.relimp(e3, rela=F)

stud.res.e3 <- studres(e3)
stud.res.e3 <- as.numeric(stud.res.e3)
e3.graph <- as.data.frame (stud.res.e3)

##Non normal. plot

shapiro.test(stud.res.e3)

ggplot(e3.graph, aes(x=stud.res.e3, y=..density..)) +
  geom_histogram(colour="black", fill="red") +
  geom_line(stat="density", size=1) +
  xlab("Regression Studentized Residuals") +
  ylab("Frequency") +
  theme_bw() +
  ggtitle("Histogram of Standardized Residuals") +
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x=-1, y=0.6, label="Shapiro-Wilk test p-value = 0.1842",
           fontface="italic", size=6)

qqnorm(stud.res.e3)
qqline(stud.res.e3)


bptest(e3)

e3.graph$stand.res.e3 <- stdres(e3) #standardized residuals
e3.graph$stand.pred.e3 <- scale(e3$fitted.values)

ggplot(e3.graph, aes(x=stand.pred.e3, y=stand.res.e3))+
  geom_point(size=2)+
  xlab("Regression Standardized Predicted Value")+
  ylab("Regression Standardized Residual")+
  theme_bw()+
  ggtitle("Scatterplot Std Residuals vs. Std Fitted values")+
  theme(plot.title = element_text(size=rel(1.2), face="bold.italic"))+
  annotate("text", x=1.4, y=2.3, label="Studentized Breusch-Pagan test",
           fontface="italic", size=5) +
  annotate("text", x=1.4, y=2, label="p.value = 0.6619",
           fontface="italic", size=5)

