##-----------------------------------------
##  SIMULACION CLIENTES
##  V2
##-----------------------------------------
setwd("C://Users/Julian Ricardo/Desktop/Hackathon")
##-----------------------------------------
##  LIBRERIAS

library(dplyr)
library(tidyr)
library(EnvStats)
library(Hmisc)
library(gdata)
library(stats)
library(ggplot2)
library(lubridate)
library(StatMeasures)
library(Hmisc)
library(sos)
library(rtrim)
library(stringr)
library(tidyr)
library(dplyr)
library(xgboost)
library(pROC)

##-----------------------------------------
##  LABELS
##-----------------------------------------
df<-read.csv('C:\\Users\\Julian Ricardo\\Downloads\\Clientes.csv',header=TRUE,sep=",")
df_perfiles<-read.csv('C:\\Users\\Julian Ricardo\\Desktop\\Hackathon\\perfiles.csv',header=TRUE,sep=",")
work<-read.csv('C:\\Users\\Julian Ricardo\\Desktop\\Hackathon\\work.csv',header=TRUE,sep=",")
home<-read.csv('C:\\Users\\Julian Ricardo\\Desktop\\Hackathon\\home.csv',header=TRUE,sep=",")

source("C:\\Users\\Julian Ricardo\\Desktop\\Hackathon\\Funciones.txt")

set.seed(25)
n=10000 ## Numero de clientes


##-----------------------------------------
## PORCENTAJES POR PERFIL
##-----------------------------------------
perfiles<-c('auto_alto',
            'auto_medio',
            'hipo_alto',
            'hipo_medio',
            'viajes',
            'restaurante',
            'vestuario',
            'licores',
            'acc_auto')

#porcentajes=rep(n/perfiles,perfiles) # SI QUEREMOS QUE TODO LOS PERFILES TENGA EL MISMO NUMERO DE CLIENTES

por_perfiles=rgamma(9,9,0.5)
por_perfiles=por_perfiles/sum(por_perfiles)

df.perfiles<-data.frame(perfiles,por_perfiles,clientes=round(n*por_perfiles,0))


##-----------------------------------------
## ID & tipo
##-----------------------------------------
df[1:n,]<-rep(NA,ncol(df))

## ID
df$id<-1:n

## Tipo
tipo<-c('app',
        'no_app',
        'pre_aprob',
        'prospecto',
        'no_cliente')

por_tipo<-c(0.6,0.25,0.1,0.05,0.05)

por_tipo_df<-data.frame(tipo,
                    por_tipo_fin=sort(cumsum(sort(por_tipo)),decreasing = TRUE),
                    por_tipo_ini=c(sort(cumsum(sort(por_tipo)),decreasing = TRUE)[-1],0),
                    clientes=round(n*por_tipo,0))


df.tipo<-crossing(data.frame(id=df$id,prob=runif(n,0,1)),
          por_tipo_df) %>% 
  filter(prob>=por_tipo_ini&prob<por_tipo_fin) %>% 
  select(id,
         tipo)

df<-df %>% left_join(df.tipo,
                 by='id') %>% 
  mutate(tipo.x=tipo.y,
         tipo.y=NULL) %>% 
  rename(tipo=tipo.x) %>% 
  mutate(app=ifelse(tipo=="app",1,0),
         partner=ifelse(tipo=="no_cliente",1,0))


##-----------------------------------------
## PERFILES
##-----------------------------------------
df_perfiles<-df_perfiles %>% 
  gather(producto,valor,2:10)



a=1
for(i in df.perfiles$perfiles){
  df_perfiles_res<-df_perfiles[df_perfiles$producto==i,]
  df<-simular_producto(df,df.perfiles,a,
                   n_per=df.perfiles[a,"clientes"],
                   par.edad=as.numeric(df_perfiles_res[5,"valor"]),
                   par.sexo=as.numeric(df_perfiles_res[6,"valor"]),
                   par.ing=as.numeric(df_perfiles_res[7,"valor"]),
                   par.est=as.numeric(df_perfiles_res[8,"valor"]),
                   par.edu=as.numeric(df_perfiles_res[9:12,"valor"]),
                   par.nse=as.numeric(df_perfiles_res[13:18,"valor"]),
                   par.sector=as.numeric(df_perfiles_res[19,"valor"]),
                   par.saldo=as.numeric(df_perfiles_res[20:21,"valor"]),
                   par.vehiculo=as.numeric(df_perfiles_res[22,"valor"]),
                   par.pre_vehiculo=as.numeric(df_perfiles_res[23,"valor"]),
                   par.hipo=as.numeric(df_perfiles_res[24,"valor"]),
                   par.per_hipo=as.numeric(df_perfiles_res[25,"valor"]),
                   par.linv=as.numeric(df_perfiles_res[26,"valor"]),
                   par.pre_linv=as.numeric(df_perfiles_res[27,"valor"]),
                   par.tc=as.numeric(df_perfiles_res[28:34,"valor"]),
                   par.pre_tc=as.numeric(df_perfiles_res[35,"valor"]),
                   par.endeud=as.numeric(df_perfiles_res[36:37,"valor"]),
                   par.no_vehi=as.numeric(df_perfiles_res[38,"valor"]),
                   par.no_hipo=as.numeric(df_perfiles_res[39,"valor"]),
                   par.no_tc=as.numeric(df_perfiles_res[40,"valor"]),
                   par.score=as.numeric(df_perfiles_res[41:42,"valor"]),
                   prob.viajes=as.numeric(df_perfiles_res[43,"valor"]),
                   prop.viajes=as.numeric(df_perfiles_res[44,"valor"]),
                   prop.rest=as.numeric(df_perfiles_res[45,"valor"]),
                   prop.vest=as.numeric(df_perfiles_res[46,"valor"]),
                   prop.lico=as.numeric(df_perfiles_res[47,"valor"]),
                   prop.acc=as.numeric(df_perfiles_res[48,"valor"]),
                   prob.carro.1=as.numeric(df_perfiles_res[49,"valor"]))

  a=a+1
}

summary(df)


##-----------------------------------------
##  HIPO 
##-----------------------------------------

hipo_corte<-quantile(df[df$desemb_hipoteca>0,"desemb_hipoteca"],0.6)
df$hipo<-ifelse(df$desemb_hipoteca==0,0,
       ifelse(df$desemb_hipoteca<hipo_corte,"hipo_medio","hipo_alta")
       )


##-----------------------------------------
##  AUTO   
##-----------------------------------------

auto_corte<-quantile(df[df$desemb_auto>0,"desemb_auto"],0.6)
df$auto<-ifelse(df$desemb_auto==0,0,
                ifelse(df$desemb_auto<auto_corte,"auto_medio","auto_alta")
)



##-----------------------------------------
## XGBOOST TC
##-----------------------------------------

train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$tc<-ifelse(df$prod_tc==0,0,1)

df_train <- df[df$id%in%train,c(3:8,11,13,16:21,24:29)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,16:21,24:29)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("tc")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"tc"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.12,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.5,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_tc <- xgb.train(param, dtrain, nrounds = 100, watchlist)

df$tc_pred<-predict(bst_tc,as.matrix(df[,c(3:8,11,13,16:21,24:29)]))

df %>% 
  filter(tc==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(tc_pred))



##-----------------------------------------
## XGBOOST AUTO GAMA ALTA
##-----------------------------------------


train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$auto_alta<-ifelse(df$auto=="auto_alta",1,0)

df_train <- df[df$id%in%train,c(3:8,11,13,18:21,24:25,27:32)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,18:21,24:25,27:32)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("auto_alta")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"auto_alta"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_auto_alta <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_auto_alta)
#View(importance_matrix)

df$auto_alta_pred<-predict(bst_auto_alta,as.matrix(df[,c(3:8,11,13,18:21,24:25,27:32)]))

df %>% 
  filter(auto_alta==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(auto_alta_pred))




##-----------------------------------------
## XGBOOST AUTO GAMMA MEDIO
##-----------------------------------------


train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$auto_medio<-ifelse(df$auto=="auto_medio",1,0)

df_train <- df[df$id%in%train,c(3:8,11,13,18:21,24:25,27:32)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,18:21,24:25,27:32)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("auto_medio")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"auto_medio"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_auto_medio <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_auto_medio)
#View(importance_matrix)

df$auto_medio_pred<-predict(bst_auto_medio,as.matrix(df[,c(3:8,11,13,18:21,24:25,27:32)]))

df %>% 
  filter(auto_medio==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(auto_medio_pred))




##-----------------------------------------
## XGBOOST LICORES
##-----------------------------------------

train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$licores_rta<-ifelse(df$licores>0,1,0)


df_train <- df[df$id%in%train,c(3:8,11,13,16:21,24:29)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,16:21,24:29)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("licores_rta")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"licores_rta"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_licores_rta <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_licores_rta)
#View(importance_matrix)

df$licores_rta_pred<-predict(bst_licores_rta,as.matrix(df[,c(3:8,11,13,16:21,24:29)]))

df %>% 
  filter(licores_rta==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(licores_rta_pred)) %>% 
  pull(perfil) %>% 
  table()




##-----------------------------------------
## XGBOOST RESTAURANTES
##-----------------------------------------

train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$restaurante_rta<-ifelse(df$restaurante>0,1,0)


df_train <- df[df$id%in%train,c(3:8,11,13,16:21,24:29)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,16:21,24:29)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("restaurante_rta")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"restaurante_rta"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_restaurante_rta <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_restaurante_rta)
#View(importance_matrix)

df$restaurante_rta_pred<-predict(bst_restaurante_rta,as.matrix(df[,c(3:8,11,13,16:21,24:29)]))

df %>% 
  filter(restaurante_rta==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(restaurante_rta_pred)) %>% 
  pull(perfil) %>% 
  table()



##-----------------------------------------
## XGBOOST ACCESORIOS CARRO
##-----------------------------------------

train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$accesorios_rta<-ifelse(df$accesorios_auto>0,1,0)


df_train <- df[df$id%in%train,c(3:8,11,13,16:21,24:29)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,16:21,24:29)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("accesorios_rta")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"accesorios_rta"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_accesorios_rta <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_accesorios_rta)
View(importance_matrix)

df$accesorios_rta_pred<-predict(bst_accesorios_rta,as.matrix(df[,c(3:8,11,13,16:21,24:29)]))

df %>% 
  filter(accesorios_rta==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(accesorios_rta_pred)) %>% 
  pull(perfil) %>% 
  table()


##-----------------------------------------
## XGBOOST VESTUARIO
##-----------------------------------------

train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$vestuario_rta<-ifelse(df$vestuario>0,1,0)

names(df_train)
df_train <- df[df$id%in%train,c(3:8,11,13,18:21,24:25,27:32)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,18:21,24:25,27:32)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("vestuario_rta")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"vestuario_rta"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_vestuario_rta <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_vestuario_rta)
#View(importance_matrix)

df$vestuario_rta_pred<-predict(bst_vestuario_rta,as.matrix(df[,c(3:8,11,13,18:21,24:25,27:32)]))

df %>% 
  filter(vestuario_rta==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(vestuario_rta_pred)) %>% 
  pull(perfil) %>% 
  table()

##-----------------------------------------
## XGBOOST HIPOTECARIO ALTO
##-----------------------------------------

train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$hipo_alta<-ifelse(df$hipo=="hipo_alta",1,0)

df_train <- df[df$id%in%train,c(3:8,11,13,16,17, 20:21,24:25,26,27, 29:33)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,16,17, 20:21,24:25,26,27, 29:33)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("hipo_alta")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"hipo_alta"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_hipo_alta <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_hipo_alta)
#View(importance_matrix)

df$hipo_alta_pred<-predict(bst_hipo_alta,as.matrix(df[,c(3:8,11,13,16,17, 20:21,24:25,26,27, 29:33)]))

df %>% 
  filter(hipo_alta==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(hipo_alta_pred))

##-----------------------------------------
## XGBOOST HIPOTECARIO MEDIO
##-----------------------------------------

train<-sample(df[df$tipo%in%c("app","no_app"),"id"],length(df[df$tipo%in%c("app","no_app"),"id"]*0.8))
df$hipo_medio<-ifelse(df$hipo=="hipo_medio",1,0)

df_train <- df[df$id%in%train,c(3:8,11,13,16,17, 20:21,24:25,26,27, 29:33)]
df_test <- df[!(df$id%in%train),c(3:8,11,13,16,17, 20:21,24:25,26,27, 29:33)]

dtrain <- xgb.DMatrix(data.matrix(df_train),
                      label = df[df$id%in%train,c("hipo_medio")])
dtest <- xgb.DMatrix(data.matrix(df_test),
                     label = df[!(df$id%in%train),"hipo_medio"])

watchlist <- list(train = dtrain, eval = dtest)

param <- list(eta = 0.08,
              max_depth = 2, 
              nround=300, 
              subsample = 0.2,
              colsample_bytree = 0.4,
              set.seed = 100,
              objective = "binary:logistic",
              nthread = 3, 
              eval_metric = "auc")

bst_hipo_medio <- xgb.train(param, dtrain, nrounds = 100, watchlist)

importance_matrix <- xgb.importance(model = bst_hipo_medio)
#View(importance_matrix)

df$hipo_medio_pred<-predict(bst_hipo_medio,as.matrix(df[,c(3:8,11,13,16,17, 20:21,24:25,26,27, 29:33)]))

df %>% 
  filter(hipo_medio==0,
         tipo%in%c('app','no_app')) %>% 
  arrange(desc(hipo_medio_pred))

##-----------------------------------------
## AGREGAR 
##-----------------------------------------

df<-df %>% 
  left_join(home,by="id") %>% 
  rename(lon_vivienda=x,
         lat_vivienda=y) %>% 
  left_join(work,by='id') %>% 
  rename(lon_trabajo=x,
          lat_trabajo=y) %>% 
  mutate(ubi_trabajo=NULL,
         ubi_vivienda=NULL)


write.csv(df, "db_clientes_perfil.csv", row.names = F)



