# ESTIMATIVA DE FROTA NECESS�RIA POR LINHA
# PRIMEIRO PASSO: DEFINI��O DO TOTAL DE FROTA NECESS�RIA CONSIDERANDO A DEMANDA ATUAL

## *DADOS NECESS�RIOS PARA A DEFINI��O DE DEMANDA
## - INDICE DE ROTATIVIDADE = 1
## - INDICE DE GRATUIDADE = MAIOR REGISTRADO AT� ENT�O
## - INDICE DE TRANSBORDO = MAIOR REGISTRADO AT� ENT�O

## *DADOS NECESS�RIOS PARA A DEFINI��O DE VIAGENS NECESS�RIAS
## - PASSAGEIRO POR TRECHO CR�TICO
## - PAR�METROS M�NIMOS CONTRATUAIS (INTERVALO MAXIMO PERMITIDO)

#CARREGANDO BIBLIOTECAS

library(dplyr)
library(stringr)
library(plyr)

#CARREGANDO ARQUIVOS PARA DEFINI��O DE DEMANDA

Indices<-read.csv(file.choose(),sep=";")
MCO<-read.csv(file.choose(),sep=";")
Viag_Especificadas<-read.csv(file.choose(),sep=";")
setwd(choose.dir())
myfiles = list.files(path=getwd(),pattern="*.csv",full.names=TRUE)
FROTA_BH03 = ldply(myfiles, read.csv, sep=";")
rm(myfiles)

#TRABALHANDO COM OS MAIORES INDICES DE TRANSBORDO E GRATUIDADE

colnames(Indices)<-c("COD_LINH","NUM_SUBL","NUM_PONT_CTRL_ORIG","HOR_INIC","TIP_DIA","HOR_FINA","DAT_PESQ","INC_GRAT","INC_ROTA","TOT_TRBD","FAT_INTE_GRAT","FAT_INTE_ROTA","FAT_INTE_TRBD","INC_TRBD")
Indices$INC_GRAT<-gsub(",",".",Indices$INC_GRAT)
Indices$INC_GRAT<-as.numeric(Indices$INC_GRAT)
Indices$INC_TRBD<-gsub(",",".",Indices$INC_TRBD)
Indices$INC_TRBD<-as.numeric(Indices$INC_TRBD)

for(j in 1:nrow(Indices)){
  if(is.na(Indices$INC_TRBD[j])){
    Indices$INC_TRBD[j]<-1
  }else if(Indices$INC_TRBD[j]==0){
    Indices$INC_TRBD[j]<-1
  }
}

Max_Indices<-Indices %>% dplyr::select(COD_LINH,INC_GRAT,INC_TRBD) %>% dplyr::group_by(COD_LINH) %>% dplyr::summarise(max_IGr=max(INC_GRAT),max_ITr=max(INC_TRBD))
rm(j,Indices)

#RETIRANDO VIAGENS OCIOSAS E LIMPANDO ARQUIVO
MCO<-filter(MCO,Viagem!="Oci.")
MCO<-MCO %>% select(Código.Externo.Linha,Num.Terminal,Numero.Ve�.culo,Data.Hora.In�.cio.Opera�.�.o,Data.Hora.Final.Opera�.�.o,Passageiros)
colnames(MCO)<-c("COD_LINH","COD_PC","N�mero.Veiculo","HORA_INICIO","HORA_FIM","PASS_PAG")
MCO<-filter(MCO,COD_PC %in% c("1","2"))
#DETERMINANDO FAIXA E TEMPO DE VIAGEM
MCO$FX<-str_sub(MCO$HORA_INICIO,start=-10,end=-9)
MCO$TV<-((as.numeric(str_sub(MCO$HORA_FIM,start=9,end=10))*1440) + (as.numeric(str_sub(MCO$HORA_FIM,start=-10,end=-9))*60)+(as.numeric(str_sub(MCO$HORA_FIM,start=-7,end=-6))))-(((as.numeric(str_sub(MCO$HORA_INICIO,start=9,end=10))*1440) +as.numeric(str_sub(MCO$HORA_INICIO,start=-10,end=-9))*60)+(as.numeric(str_sub(MCO$HORA_INICIO,start=-7,end=-6))))
MCO<-filter(MCO,TV>5,PASS_PAG>0)
#JUNTANDO INDICES DE TRANSBORDO E GRATUIDADE
MCO<-merge(MCO,Max_Indices,all.x=T)
MCO[is.na(MCO)]<-1
#DEFININDO DEMANDA TRANSPORTADA
MCO$DEMANDA<-ceiling(MCO$PASS_PAG*MCO$max_ITr*MCO$max_IGr)
FROTA<-FROTA_BH03 %>% select(N�mero.Ve�culo,Padr�o.Ve�culo,N�mero.de.Assentos,�rea.�til)
MCO<-merge(MCO,FROTA,by.x="N�mero.Veiculo",by.y="N�mero.Ve�culo",all.x=T)
#TIRANDO ESTACOES
MCO<-filter(MCO,COD_LINH %in% Viag_Especificadas$LINHA)
#INDICE DE ROTATIVIDADE =1
MCO$IR<-1
MCO$PTC<-MCO$DEMANDA/MCO$IR

#CAPACIDADE M�XIMA PERMITIDA PELO DECRETO 17362/2020

for(j in 1:nrow(MCO)){
  if(is.na(MCO$Padr�o.Ve�culo[j])){
    MCO$Max_Decreto[j]<-MCO$DEMANDA[j]
  }else if(MCO$Padr�o.Ve�culo[j]=="10"){
    MCO$Max_Decreto[j]<-MCO$N�mero.de.Assentos[j]+5
  }else if(MCO$Padr�o.Ve�culo[j]=="84"){
    MCO$Max_Decreto[j]<-MCO$N�mero.de.Assentos[j]+20
  }else{
    MCO$Max_Decreto[j]<-MCO$N�mero.de.Assentos[j]+10
  }
}

#VIAGENS NECESS�RIAS

VIAG_NECESSARIAS<-MCO %>% dplyr::select(COD_LINH,COD_PC,FX,TV,PASS_PAG,max_IGr,max_ITr,DEMANDA,PTC,Max_Decreto) %>% dplyr::group_by(COD_LINH,COD_PC,FX) %>% dplyr::summarise(Dem_MCO=(sum(PASS_PAG))/5,max_IGr=max(max_IGr),max_ITr=max(max_ITr),Dem_mean=(sum(DEMANDA))/5,PTC_mean=(sum(PTC))/5,Capacidade=mean(Max_Decreto),TC=mean(TV))
VIAG_NECESSARIAS$Viag<-ceiling(VIAG_NECESSARIAS$PTC_mean/VIAG_NECESSARIAS$Capacidade)

#SALVANDO ARQUIVO CSV

write.csv2(VIAG_NECESSARIAS,"VIAGN.csv",row.names=F)
