

as.data.frame(table(DT_test$c_gaseosa))




#variable sobre si hay datos perdidos

c('ID','edad', 'genero', 'n_educ', 'altura', 'peso', 'f_hambre', 'c_rapida', 'e_alcohol', 'c_alcohol', 'a_fisica', 'c_frutas', 'c_verdura', 'c_gaseosa', 'c_snacks', 'c_grasa')

DT[, dato_perdido := ( f_hambre == "Dato perdido")+ ( c_rapida == "Dato perdido") + ( e_alcohol == "Dato perdido") + ( c_alcohol == "Dato perdido") +( a_fisica == "Dato perdido") + ( c_frutas == "Dato perdido") +( c_verdura == "Dato perdido") + ( c_gaseosa == "Dato perdido") + ( c_snacks == "Dato perdido") + ( c_grasa == "Dato perdido")]
DT$dato_perdido <- as.factor(DT$dato_perdido)

DT$dato_perdido <- relevel(DT$dato_perdido, ref="0")


DT$dato_perdido <- fct_collapse(DT$dato_perdido, "si" = c("1","2","3","4"))

as.data.frame(table(DT$dato_perdido))

#Ajustamos un modelo lineal múltiple
modelo_sc_r <- lm(peso ~ altura*genero + edad + dato_perdido, data = DT)

#Resumen del modelo
summary(modelo_sc_r)





#f hambre 


#Defino a la variable como factor
DT$f_hambre <- as.factor(DT$f_hambre)

#Redefino el baseline
DT$f_hambre <- relevel(DT$f_hambre, ref="Nunca")

levels(DT$f_hambre)

#Ajustamos un modelo lineal múltiple
modelo_sc_r <- lm(peso ~ altura*genero + edad + f_hambre , data = DT)

#Resumen del modelo
summary(modelo_sc_r)






##tomo alcohol o no 

`
```{r}
#Defino a la variable como factor
DT$e_alcohol <- as.factor(DT$e_alcohol)

#Redefino el baseline
DT$e_alcohol <- relevel(DT$e_alcohol, ref="Nunca tomA(C) alcohol mA!s que unos pocos sorbos")

#Hago una copia del Dataset para preservar los datos originales
DT_copy <- DT

#En la copia, junto estos 3 niveles
DT_copy$e_alcohol <- fct_collapse(DT_copy$e_alcohol, "tomo" = c("12 o 13 aA+/-os","10 o 11 aA+/-os", "8 o 9 aA+/-os", "7 aA+/-os o menos", "14 o 15 aA+/-os", "16 o 17 aA+/-os", "18 aA+/-os o mA!s"))

#Ajustamos un modelo lineal múltiple
modelo_sc_r <- lm(peso ~ altura*genero + edad + n_gaseosa + e_alcohol , data = DT_copy)

#Resumen del modelo
summary(modelo_sc_r)


levels(DT_copy$c_gaseosa)
