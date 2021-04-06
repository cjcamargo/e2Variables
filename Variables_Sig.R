#Archivo automático para selección de VOP Sig.


 #Analisis de Multicolinealidad 
## Revisar variables eliminadas por multicolinealidad el analista debe incluir alguna que considere necesaria


datamodel_0i <- Plantilla.VOP_DeltaE_CSV[,-1]
nvif = 120 #se recomienda un número mayor a 10 y menor a 500
direccion = "both" #"both" or "backward" or "forward"

#BloqueMulticolinealidad
{
library(corrplot)
library(mctest)
library(dplyr)
 
CorA<-cor(datamodel_0i) 
corrplot(CorA, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.001, tl.col = 'black',
         order = "hclust", diag = FALSE)
fit1 <-lm(datamodel_0i[,1] ~.,datamodel_0i[,-1])
MCD<-imcdiag(fit1)
MCDdf<-as.data.frame( MCD$idiags)
median(MCDdf$VIF)
HighMC <-  as.data.frame(filter(MCDdf, MCDdf$VIF > nvif)) # max 96% R2 MC (recomendado 10  /90%)
nombresdel <- rownames(HighMC)
datamodel_0F <- datamodel_0i[, !(names(datamodel_0i) %in% nombresdel)]
}

datamodel_0F <- PCAtraining_1
#Seleccion del mejor modelo-------

if (ncol(datamodel_0F)>3) {

  library(BAS)
  library(tidyverse)
  library(ggplot2)
  library(dbplyr)
  library(MASS)
  library(tsoutliers)
  
#Stepwise-----------------------------
{
ModelStep <- stepAIC(lm(datamodel_0F[,1]~.,data=datamodel_0F[,-1]),direction=direccion)

DataStep<-ModelStep$model


FitStep<-lm(DataStep[,1]~.,data=DataStep[,-1])

SumStep<-summary(FitStep)
TblStad <- as.data.frame( SumStep$coefficients)

DataElim <- as.data.frame(filter(TblStad, TblStad[,4] > 0.1))
nombresVar <- rownames(DataElim)
datamodel_0F1 <- DataStep[, !(names(DataStep) %in% nombresVar)]
}
  
  

#BAYES----------------
if (ncol(datamodel_0F1)<21){

Regbas1 <- bas.lm(datamodel_0F1[,1] ~ .,
                  data = datamodel_0F1[,-1],
                  method = "MCMC",
                  prior = "ZS-null",
                  modelprior = uniform())


summary(Regbas1)
image(Regbas1,rotate = FALSE)
SumBasF<-as.data.frame( summary(Regbas1))



keep_columnsF <- SumBasF %>% 
  rownames_to_column()%>% 
  filter(str_detect(rowname, "BF"))%>% 
  summarise_if(is.numeric, sum) %>% 
  select_if(~.x ==1) %>% 
  names()


SumBas1F <- as.data.frame( SumBasF[, (names(SumBasF) %in% keep_columnsF)])
NombreVarF <- as.vector( row.names(SumBasF))
BF1C <- cbind(NombreVarF,SumBas1F)

ModelElimF <- as.data.frame(filter(BF1C, BF1C[,2] == 0))
nameElimF <- as.character( ModelElimF[,1])
datamodel1 <- datamodel_0F1[, !(names(datamodel_0F1) %in% nameElimF)]



#OLS DELTA E

if(ncol(datamodel1)>2) {
  fit12 <-lm(datamodel1[,1] ~., data= datamodel1[,-1])
} else {
  fit12 <-lm(datamodel1[,1] ~datamodel1[,-1])
}





write.csv2(datamodel1, "C:/Users/Carlos Camargo/Desktop/significativase2.csv")
summary(fit12)
} else { print("Tiene mas de 21 variables de entrada a Bayes se recomienda aplicar Stepwise con diferente dirección nuevamente ó realizar análisis manual") }
} else {
  
  print("No existen suficientes variables")
}

#probar modelo completo
summary(fit1)


