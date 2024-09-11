
# ELIMINA LA NOTACION CIENTIFICA

options(scipen=999) 

# CAMBIA EL NOMBRE DEL TITULO

PD <- rename(PD, "ID"="IBS")

# COLOCA EL MARCADOR DE LA CUENTA COLOCA LA CARTERA Y LA FAMILIA

risk_1 <- left_join(RIESGO, CUENTA, by = "CUENTA_CONTABLE") %>% 
  left_join(CARTERA, by = "SUB_PRODUCTO") %>%
  left_join(FAMILIA, by = "SUB_PRODUCTO")

# ELIMINA LOS PRODUCTOS QUE NO SE ESTUDIARA

risk_str <- filter(risk_1, MAR_CUEN == "0")

# SELECCCIONAR LAS VARIABLES DE LA PD A UTILIZAR

PD <- select(PD, ID, PD_DF, INCU)

# CREAR EL SALDO EAD

risk_ead <- mutate(risk_str, EAD=SALDO_ACTUAL+LINEA_CREDITO_DISPONIBLE)

# REALIZAR LA TABLA DINAMICA POR CARTERA SOLO POR SALDO Y POR CARTERA  

car_sal <- dcast(risk_str, ID+NOMBRE_CLIENTE~CARTERA, sum, value.var="SALDO_ACTUAL")

# SE PUEDE MEJORAR / COLOCA SALDOS CEROS A LAS CARTERAS INACTIVAS

car_sal$MANUFACTURA <- 0
car_sal$VEHICULO <- 0
#car_sal$MICROCREDITO <- 0
car_sal$HIPOTECARIO <- 0
car_sal$TURISMO <- 0

# REALIZAR LA UNION DE LA PROBABILIDAD, LOS MAYORES DEUDORES Y LAS GARANTIAS AJUSTADAS

per <- left_join(car_sal, PD, by = "ID") %>%
  left_join(MAYORES, by = "ID") %>%
  left_join(GARANTIA, by = "ID")

# COLOCAR CERO A LOS VALORES QUE SALEN EN NA

per$C_1_HC<- ifelse(is.na(per$C_1_HC),0,per$C_1_HC)
per$MAYORES<- ifelse(is.na(per$MAYORES),0,per$MAYORES)

# IDENTIFICA LAS CARTERAS ACTIVAS 

cartera_activa <- select(risk_str, CARTERA)
cartera_activa <- unique(cartera_activa)

# TODAS LAS CARTERAS QUE SE TENIAN 

cartera_total <- data.frame("CARTERA"=c("COMERCIAL","AGRICOLAS","COMERCIO EXTERIOR","MANUFACTURA","MICROCREDITOS","RRHH","TURISMO","VEHICULO"))

# BUSCAR LAS CARTERAS QUE NO ESTAN ACTIVAS 

cartera_inact <- anti_join(cartera_total, cartera_activa, by="CARTERA")

# SE PUEDE MEJORAR / SE TRASPONE 

cartera_rest <- as.data.frame(t(cartera_inact[-2])) 

#cartera_rest <- select(-1,)

#cl <- count(cartera_inact)
#ct <- cl$n
#c1 <- 1

# CALCULO REALIZA LA SUMA DE LOS SALDOS CALCULA EL MAXIMO DE SALDO POR CARTERA

per1 <- per %>% mutate(TOTAL = rowSums(per[,3:10]),E1=TOTAL*(1+PD_DF)-C_1_HC,E=pmax(E1,0),
                       MAX_TOTAL = pmax(AGRICOLAS,COMERCIAL,RRHH,HIPOTECARIO,TURISMO,MICROCREDITOS,VEHICULO),
                       CARTERA_MAXIMA = apply(.[,3:10], 1, function(x) names(x)[which.max(x)]))

# COLOCA EL LGD

LGDB <- LGD$BASE
LGDP <- LGD$PREFERIDA

# CALCULO LA LGD PREVIA

per2 <- mutate(per1, LGD = ifelse(MAYORES==0, LGDB*(E/TOTAL), LGDP*(E/TOTAL)) ,CONSTRUCCION=0)

# CALCULO DE LGD AGRCILA, TURISMO CONSTRUCCION

per3 <- mutate(per2, LGD_AGRI=ifelse(per2$AGRICOLAS>0 & per2$PD>0.5,0.1,per2$LGD),
               LGD_TUR=ifelse(per2$TURISMO>0 & per2$PD>0.65,0.1,
                              ifelse(per2$TURISMO>0 & per2$PD>0.5,0.05,per2$LGD)),
               LGD_CONS=ifelse(per2$CONSTRUCCION>0 & per2$PD>0.65,0.20,
                               ifelse(per2$CONSTRUCCION>0 & per2$PD>0.50,0.10,
                                      ifelse(per2$CONSTRUCCION>0 & per2$PD>0.25,0.05,
                                             ifelse(per2$CONSTRUCCION>0 & per2$PD>0.00,0.01,per2$LGD)))),
               LGD_MAX=pmax(LGD_CONS,LGD_TUR,LGD_AGRI,LGD))

# TRAE LAS TASAS DE RECUPERACION Y CASTIGO

per4 <- left_join(per3, TASA1, by = "CARTERA_MAXIMA")

# CREAR LAS TASAS FINAL y LA LGD DEFINITIVA Y LAS PERDIDAS Y PERDIDAS ESPERADAS

per6 <- mutate(per4, TASA_FINAL=ifelse(PD_DF<0.78, per4$TASA_ACTIVA, per4$TASA_CASTIGOS),
               LGD_DEF=((LGD_MAX-(LGD_MAX*TASA_FINAL))/100),
               PERDIDAS=LGD_DEF*PD_DF*TOTAL,
               NO_ESP=(sqrt(PD_DF*(1-PD_DF))*LGD_DEF*TOTAL),
               NO_ESPA=NO_ESP*2.33
               
)

# ORDENA LAS PERDIDAS

per6 <- arrange(per6, desc(TOTAL))

# GENERA EL ARCHIVO DE LAS PERDIDAS

write.xlsx(per6, "PERDIDAS_ESPERADAS.xlsx")
