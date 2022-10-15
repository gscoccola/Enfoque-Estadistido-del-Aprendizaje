
rm( list=ls() )  #remove all objects
gc()   

require("data.table")
library(tidyverse)
library(modelr)
library(corrplot)
library(moderndive)
library(lattice)
library(reshape2)
library(ggpubr)
theme_set(theme_pubr())

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\PC\\Documents\\eea2022_tp1_scoccola_gabriel")  #Establezco el Working Directory

#cargo el dataset
DT  <- fread("./encuesta_salud_train.csv")

#¿Cómo es la correlación entre las variables numéricas? Utilice y analice en detalle algún gráfico que sirva
#para sacar conclusiones sobre la asociación de variables realizando apertura por género. En particular, ¿cómo
#es la correlación entre la variable a explicar (peso) y el resto de las variables numéricas?
  
DT_fem=DT[genero=="Femenino"]
DT_mas=DT[genero=="Masculino"]

cor_fem=cor(DT_fem[, c("edad", "altura", "peso", "dias_consumo_comida_rapida",
                       "consumo_diario_alcohol", "dias_actividad_fisica_semanal"), with=FALSE])

melted_cormat1 <- melt(cor_fem)

p1 <- ggplot(data = melted_cormat1, aes(x=Var1, y=Var2, fill=value)) +  geom_tile()


cor_mas=cor(DT_mas[, c("edad", "altura", "peso", "dias_consumo_comida_rapida",
                       "consumo_diario_alcohol", "dias_actividad_fisica_semanal"), with=FALSE])

melted_cormat2 <- melt(cor_mas)

p2 <-ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


figure <- ggarrange(p1, p2,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1,  common.legend = TRUE, legend = "bottom")
figure

#score_model <- lm(DT$altura ~ DT$peso, data = DT)

ggplot(DT, aes(x = altura, y = peso, color = genero)) +
  geom_point() +
  labs(x = "altura", y = "peso", color = "genero")





as.data.frame(table(DT$frecuencia_hambre_mensual))

h0=DT[frecuencia_hambre_mensual=="Nunca"]
h1=DT[frecuencia_hambre_mensual=="Rara vez"]
h2=DT[frecuencia_hambre_mensual=="Algunas veces"]
h3=DT[frecuencia_hambre_mensual=="Casi siempre"]
h4=DT[frecuencia_hambre_mensual=="Siempre"]


p0=ggplot(h0, aes(x=h0$consumo_semanal_comida_grasa, fill=genero)) +
  geom_bar(position="dodge")
p1=ggplot(h1, aes(x=h1$consumo_semanal_comida_grasa, fill=genero)) +
  geom_bar(position="dodge")
p2=ggplot(h2, aes(x=h2$consumo_semanal_comida_grasa, fill=genero)) +
  geom_bar(position="dodge")
p3=ggplot(h3, aes(x=h3$consumo_semanal_comida_grasa, fill=genero)) +
  geom_bar(position="dodge")
p4=ggplot(h4, aes(x=h4$consumo_semanal_comida_grasa, fill=genero)) +
  geom_bar(position="dodge")

p5=ggplot(h0, aes(x=h0$consumo_semanal_verdura, fill=genero)) +
  geom_bar(position="dodge")
p6=ggplot(h1, aes(x=h1$consumo_semanal_verdura, fill=genero)) +
  geom_bar(position="dodge")
p7=ggplot(h2, aes(x=h2$consumo_semanal_verdura, fill=genero)) +
  geom_bar(position="dodge")
p8=ggplot(h3, aes(x=h3$consumo_semanal_verdura, fill=genero)) +
  geom_bar(position="dodge")
p9=ggplot(h4, aes(x=h4$consumo_semanal_verdura, fill=genero)) +
  geom_bar(position="dodge")

figure <- ggarrange(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9,
                    labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
                    ncol = 5, nrow = 2,
                    common.legend = TRUE, legend = "bottom")
figure
