library(data.table)
library(lubridate)
library(dplyr)
library(ranger)
library(pROC)

##################LIMPIEZA DE DATOS - var rta####################
var_rta= fread("DT19_Datos_Var_Rpta_train.csv",key = 'id')
var_rta = var_rta[, id := as.factor(id)]
var_rta= var_rta %>% select(id,var_rpta,segmento)
##################LIMPIEZA DE DATOS - keys####################
datos_tr= fread("DT19_Datos_transaccionales_train.csv", key = 'id')
datos_tr = datos_tr[, id := as.factor(id)]

datos_fecha= datos_tr %>% select(id,fecha_trxn)
datos_tr = datos_tr %>% select(id,cdgrpta,canal,disposit,vlrtran)
datos_tr$cdgrpta = hutils::if_else(datos_tr$cdgrpta==0,1,0)

##################LIMPIEZA DE DATOS - keys####################
# keys= fread("DT19_maestro_cdgtrn_cdgrpta.csv")
# keys= keys %>% select(canal,clasif_trxn,clasif_cod_rpta,grupo_modifcado) %>% filter(clasif_cod_rpta != "None")
##############################################
####
#mer_data=merge.data.table(datos_tr,var_rta, by="id")
mer_data= datos_tr[var_rta, on='id']
#mer_data= mer_data %>% select(id:vlrtran,var_rpta:segmento)
####
mer_data = mer_data[, canal := as.factor(canal)]
mer_data = mer_data[, segmento := as.factor(segmento)]
mer_data = mer_data[, var_rpta := as.factor(var_rpta)]
mer_data = mer_data[, disposit := as.factor(disposit)]

######
table(mer_data$var_rpta, mer_data$canal)
#Canal 4, Canal 7, Canal 3 no aportan
table(mer_data$var_rpta, mer_data$segmento)
#
table(mer_data$var_rpta, mer_data$disposit)
#Disposit 7, 9, 2, 1, 6, 8
###Cortamos las filas duplicadas
mer_data = unique(mer_data)
mer_data = mer_data[, cdgrpta := as.factor(cdgrpta)]
###############TRATAMIENTO DATOS REPETIDOS
mer_data1= mer_data %>% select(id, canal,vlrtran)  %>% dcast(...~canal, value.var = 'vlrtran',
                                                            fun.aggregate= sum)

mer_data2= mer_data %>% select(id,disposit) %>% group_by(id,disposit) %>% summarise(Cont=n()) %>%
  ungroup() %>%reshape2::dcast(...~disposit, value.var = "Cont", fun.aggregate=median)  %>% 
  mutate_all(list(~tidyr::replace_na(.,0)))


mer_data3 = mer_data %>% select(id,cdgrpta) %>%  group_by(id,cdgrpta) %>%
summarise(Cont=n()) %>%ungroup() %>% reshape2::dcast(...~cdgrpta, value.var = "Cont", fun.aggregate=sum)
colnames(mer_data3) = c('id','No_exit',"Exit")

mer_data4= mer_data %>% select(id, disposit,vlrtran)  %>% dcast(...~disposit, value.var = 'vlrtran',
                                                             fun.aggregate= sum)
###############################
datos_fecha= datos_fecha %>% group_by(id) %>% summarise(count= n()) %>% ungroup()
###############################

mer_data = mer_data %>% select(var_rpta,id, segmento) %>% distinct() %>% 
  merge.data.table(mer_data1, by="id") %>%
  merge.data.table(mer_data2, by="id") %>%
  merge.data.table(mer_data3, by="id") %>%
  merge.data.table(datos_fecha, by= 'id') %>%
  merge.data.table(mer_data4, by= 'id')
mer_data = as.data.frame(mer_data)

# mer_data2 = mer_data %>% select(id,CANAL_1_0:CANAL_8_1) %>% mutate(Suma=rowSums(.[,2:17])) %>%
#   select(id,Suma)

#mer_data[,c(4:11)]= lapply(mer_data[,c(4:11)], function(x) ifelse(x==0 , 0, log(x)))

#####
# mer_data =mer_data  %>% mutate(var_rpta1= if_else(Suma == 0 & var_rpta =='1','1','0'),
#                              var_rpta2 =if_else(var_rpta=='1' & var_rpta1 =='1','0',
#                                                 if_else(var_rpta=='0' & var_rpta1=='0','0',
#                                                         if_else(var_rpta=='1'& var_rpta1=='0','1','0')))) %>%
#   mutate(var_rpta=var_rpta2) #%>% select(id:canal_n)

# mer_data$var_rpta = as.factor(mer_data$var_rpta)
############DATOS JUGUETE##############

#dcast(toy, id + canal + var_rpta+segmento ~ disposit, value.var = "vlrtran",fun.aggregate = sum)

############DATOS JUGUETE 2############
# toy= mer_data[mer_data$id =="8"]
# toy1= toy %>% select(id,canal) %>% group_by(canal) %>% summarise('canal_n'= n());toy1
# toy %>% select(-canal) %>%group_by(id,segmento,var_rpta) %>% summarise_all(list(sum)) %>%
#   left_join(toy1, by='id')
#######################################

##########RANDOM FOREST######################
#https://stats.stackexchange.com/questions/290886/how-to-improve-rare-event-binary-classification-performance
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3602667/
#https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/JRip
#https://cran.r-project.org/web/packages/RWeka/RWeka.pdf
#DownSample

#write.csv2(mer_data,"mer_data.csv")
mer_data = fread("mer_data.csv", dec = ',')
mer_data$id = factor(mer_data$id)
mer_data$var_rpta = factor(mer_data$var_rpta)
mer_data$segmento = factor(mer_data$segmento)
mer_data= mer_data %>%select(-V1)
##################1 vs 0 = 25%
set.seed(2019)
muestra=sample(c(1:nrow(mer_data[mer_data$var_rpta==0,])),nrow(mer_data[mer_data$var_rpta==1,])*1,replace = T)
mer_data_bal_20= data.frame(rbind(mer_data[mer_data$var_rpta==1,], 
                                  mer_data[mer_data$var_rpta==0,][muestra,]))

##################1 vs 0 = 20%


##################1 vs 0 = 33%

##################1 vs 0 = 50%


###-------------------BOSQUE ALEATORIO Standar Mtry----------------------
modelo1= ranger(var_rpta~.,data= mer_data_bal_20%>% select(-id, -CANAL_3,
                                                           -CANAL_4,-CANAL_7,
                                                           -DISPOSIT_1,
                                                           -DISPOSIT_2,
                                                           -DISPOSIT_6,
                                                           -DISPOSIT_7,
                                                           -DISPOSIT_8,
                                                           -DISPOSIT_9),probability = T, seed = 2019,
                num.trees = 1500)

Pred=as.data.frame(predict(modelo1, data = mer_data)$predictions)
Pred$Unos= if_else(Pred$`1`<=0.5,0,1)
estudio = data.frame(Pred$Unos, mer_data$var_rpta)

table(estudio)
saveRDS(modelo1,"modelo1.RDS")
###-------------------XGBOOST---------------------------------------------
library(xgboost)

modelo2= xgboost(data= as.matrix(mer_data_bal_20 %>% select(-X,-id,-var_rpta)),
                 label = mer_data_bal_20$var_rpta, nrounds = 100)
Pred=as.data.frame(predict(modelo2, newdata =as.matrix(mer_data %>%select(-X,-id,-var_rpta))))
Pred[,2]= if_else(Pred[,1]<=0.5,0,1)
estudio = data.frame(Pred[,2], mer_data$var_rpta)

table(estudio)
saveRDS(modelo2,"modelo2.RDS")
###-------------------BOSQUE ALEATORIO Random Forest mtry=15----------------------

modelo3= ranger(var_rpta~.,data= mer_data%>% select(-id, -CANAL_3,
                                                    -CANAL_4,
                                                    -CANAL_7,
                                                    -DISPOSIT_1,
                                                    -DISPOSIT_2,
                                                    -DISPOSIT_6,
                                                    -DISPOSIT_7,
                                                    -DISPOSIT_8,
                                                    -DISPOSIT_9),
                probability = T, seed = 2019, 
                mtry =9, class.weights = c(0.5,1))

Pred=as.data.frame(predict(modelo3, data = mer_data)$predictions)
Pred$Unos= if_else(Pred$`1`<=0.5,0,1)
estudio = data.frame(Pred$Unos, mer_data$var_rpta)

table(estudio)
saveRDS(modelo2,"modelo2.RDS")

###-------------------BOSQUE ALEATORIO Random Forest class.weight---------------------
modelo4= ranger(var_rpta~.,data= mer_data%>% select(-id),probability = T, seed = 2019,
                class.weights = c(1,0.2))

Pred=as.data.frame(predict(modelo4, data = mer_data)$predictions)
Pred$Unos= if_else(Pred$`1`<=0.5,0,1)
estudio = data.frame(Pred$Unos, mer_data$var_rpta)

table(estudio)
saveRDS(modelo4,"modelo4.RDS")

###-------------------BOSQUE ALEATORIO Random Forest class.weight---------------------


################################################################################################
################################################################################################
################################################################################################

##################LIMPIEZA DE DATOS - keys####################

##################LIMPIEZA DE DATOS - var rta####################
var_rta_t= fread("DT19_IDs_predict.csv",key = 'id')
var_rta_t = var_rta_t[, id := as.factor(id)]
var_rta_t= var_rta_t %>% select(id,segmento)
##################LIMPIEZA DE DATOS - keys####################
datos_tr_t= fread("DT19_Datos_transaccionales_predict.csv",key = 'id')
datos_tr_t = datos_tr_t[, id := as.factor(id)]
datos_fecha_t= datos_tr_t %>% select(id,fecha_trxn)
datos_tr_t = datos_tr_t %>% select(id,cdgrpta,canal,disposit,vlrtran)
datos_tr_t$cdgrpta = hutils::if_else(datos_tr_t$cdgrpta==0,1,0)
##################LIMPIEZA DE DATOS - keys####################

mer_data_t=merge.data.table(datos_tr_t,var_rta_t, by="id")
#mer_data= mer_data %>% select(id:vlrtran,var_rpta:segmento)
####
mer_data_t$id= factor(mer_data_t$id)
mer_data_t$canal = factor(mer_data_t$canal)
mer_data_t$segmento = factor(mer_data_t$segmento)
mer_data_t$cdgrpta= factor(mer_data_t$cdgrpta)
mer_data_t$disposit = factor(mer_data_t$disposit)
###Cortamos las filas duplicadas
mer_data_t = unique(mer_data_t)
###############TRATAMIENTO DATOS REPETIDOS
mer_data1_t= mer_data_t %>% select(id, canal,vlrtran)  %>% dcast(...~canal, value.var = 'vlrtran',
                                                             fun.aggregate= sum)

mer_data2_t= mer_data_t %>% select(id,disposit) %>% group_by(id,disposit) %>% summarise(Cont=n()) %>%
  ungroup() %>%reshape2::dcast(...~disposit, value.var = "Cont", fun.aggregate=median)  %>% 
  mutate_all(list(~tidyr::replace_na(.,0)))


mer_data3_t = mer_data_t %>% select(id,cdgrpta) %>%  group_by(id,cdgrpta) %>%
  summarise(Cont=n()) %>%ungroup() %>% reshape2::dcast(...~cdgrpta, value.var = "Cont", fun.aggregate=sum)
colnames(mer_data3_t) = c('id','No_exit',"Exit")

mer_data4_t= mer_data_t %>% select(id, disposit,vlrtran)  %>% dcast(...~disposit, value.var = 'vlrtran',
                                                                fun.aggregate= sum)

####
datos_fecha_t= datos_fecha_t %>% group_by(id) %>% summarise(count= n()) %>% ungroup()
###


mer_data_t = mer_data_t %>% select(id, segmento) %>% distinct() %>% 
  merge.data.table(mer_data1_t, by="id") %>%
  merge.data.table(mer_data2_t, by="id") %>%
  merge.data.table(mer_data3_t, by="id") %>%
  merge.data.table(datos_fecha_t, by = 'id')
# mer_data2 = mer_data %>% select(id,CANAL_1_0:CANAL_8_1) %>% mutate(Suma=rowSums(.[,2:17])) %>%
#   select(id,Suma)

mer_data_t[,c(3:10)]= lapply(mer_data_t[,c(3:10)], function(x) ifelse(x==0 , 0, log(x)))
mer_data_t = as.data.frame(mer_data_t)

write.csv2(mer_data_t,"mer_data_t.csv",row.names = FALSE)

mer_data_t= mer_data_t %>% merge.data.table(mer_data4_t, by= 'id')
##############################Predicciones###########################3
mer_data_t = read.csv2("mer_data_t.csv")
mer_data_t$segmento = factor(mer_data_t$segmento)

Pred_t=as.data.frame(predict(modelo1, data = mer_data_t %>% select(-id))$predictions)
#Pred=as.data.frame(predict(modelo1, newdata =as.matrix(mer_data_t %>% select(-id))))
####Submission 1

submission1= data.frame(id=mer_data_t$id,probabilidad=Pred_t[,1]) %>% mutate(probabilidad= (1-probabilidad))

write.csv(submission1,"C:/Users/chidalgo/Downloads/Works/Dataton_Bancolombia/prediccion/DT19_IDs_predict.csv", 
          row.names = FALSE)  


#########Submission 2

submission2= data.frame(id=mer_data_t$id,probabilidad=Pred_t$`1`)

write.csv(submission2,"C:/Users/chidalgo/Downloads/Works/Dataton_Bancolombia/prediccion/DT19_IDs_predict.csv", 
          row.names = FALSE)  

#########Submision 3

submission3= data.frame(id=mer_data_t$id,probabilidad=Pred_t$`1`)

write.csv(submission3,"C:/Users/chidalgo/Downloads/Works/Dataton_Bancolombia/prediccion/DT19_IDs_predict.csv", 
          row.names = FALSE)  


#########Submision 4

submission4= data.frame(id=mer_data_t$id,probabilidad=Pred_t$`1`)

write.csv(submission3,"C:/Users/chidalgo/Downloads/Works/Dataton_Bancolombia/prediccion/DT19_IDs_predict.csv", 
          row.names = FALSE)  


#####Ensemble submission

submission5= submission1 %>% left_join(submission2,by="id") %>% left_join(submission2,by="id") %>%
  left_join(submission3,by="id") %>%  left_join(submission4,by="id") %>%
  group_by(id) %>% mutate(probabilidad= (probabilidad.x + probabilidad.y + 
                                           probabilidad.x.x + probabilidad.y.y)/4) %>% 
  select(id,probabilidad)


write.csv(submission4,"C:/Users/chidalgo/Downloads/Works/Dataton_Bancolombia/prediccion/DT19_IDs_predict.csv", 
          row.names = FALSE)  
