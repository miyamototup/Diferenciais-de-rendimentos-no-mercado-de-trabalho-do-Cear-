
###Conversão txt para um BD SQL
#library(RSQLite)
#setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
#ce=dbConnect(SQLite(),dbname='RAISCEARA')


#setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Dados')
#em=list.files(pattern ='')


#for (i in 1:length(em)){
#  c=read.csv(paste('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Dados/',em[[i]],sep="")
#             ,skip=0,sep=';',dec=',',header=T,fileEncoding = "WINDOWS-1252")
#  j=em[[i]]
#  dbWriteTable(conn=ce,name=j,value=c,append=F)
#}


#dbListTables(ce)

#####
library(RSQLite)
setwd('/Arquivos/Documentos/Pesquisa/RAIS-Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')
dbListTables(ce)

dbListFields(ce,'CE2000')
#Vamos utilizar as variáveis:

uf=c("CE2000","CE2001","CE2002","CE2003","CE2004","CE2005",
     "CE2006","CE2007","CE2008","CE2009","CE2010","CE2011","CE2012","CE2013",
     "CE2014")

#Forma de ser obter raiscept utilizando a função emp_form desenvolvida para
#o conjunto de funções de manuseio da rais
#source("rais.R")
#emprego_formal=emp_form(uf,ce)

for (i in 1:length(uf)){
  k=uf[[i]]
  raisce=dbSendQuery(ce,paste("select `Município` from",k))
  raisce=fetch(raisce,n=-1)
  raisce=as.data.frame(table(raisce$Município))
  raisce$Ano=1999+i
  colnames(raisce)=c('Codigo','Empregos Formais','Ano')
  lraiscept[[i]]=raisce
  }

raisce2000=lraiscept[[1]]
raisce2014=lraiscept[[15]]
raiscept=do.call(rbind.data.frame,lraiscept)
#Salitre apresntou uma taxa de crescimento anual de 22% a.a VOu dar uma olhada na série
#salitre=rbind(raisce2000[raisce2000$Codigo==231195,],raisce2001[raisce2001$Codigo==231195,],raisce2002[raisce2002$Codigo==231195,],
#              raisce2003[raisce2003$Codigo==231195,],raisce2004[raisce2004$Codigo==231195,],raisce2005[raisce2005$Codigo==231195,],
#              raisce2006[raisce2006$Codigo==231195,],raisce2007[raisce2007$Codigo==231195,],raisce2008[raisce2008$Codigo==231195,],
#              raisce2009[raisce2009$Codigo==231195,],raisce2010[raisce2010$Codigo==231195,],raisce2011[raisce2011$Codigo==231195,],
#              raisce2012[raisce2012$Codigo==231195,],raisce2013[raisce2013$Codigo==231195,],raisce2014[raisce2014$Codigo==231195,])
#salitre=salitre[,c(3,2)]
#plot(salitre)

rm(list= ls()[!(ls() %in% c('raiscept','raisce2000','raisce2014'))]) 
gc()

raiscept$logEF=log(raiscept$`Empregos Formais`)
raiscept=raiscept[,c(1,3,4)]

raiscept2=list()
raiscept=split(raiscept,paste(raiscept$Codigo))

for (i in 1:length(raiscept)){
  cept=lm(raiscept[[i]]$logEF~raiscept[[i]]$Ano,data=raiscept[[i]])
  slope=as.data.frame((exp(cept$coefficients[2]))-1)
  cept2=as.data.frame(cbind(as.character(raiscept[[i]]$Codigo[1]),slope))
  raiscept2[[i]]=cept2
}

raiscept=do.call(rbind.data.frame,raiscept2)
colnames(raiscept)=c('Codigo','TEF')


###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/municipios_2010/municipios_2010.shp')
#Para adicionar contornos no mapa final preciso adicionar o shape file dos estados
estados=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/estados_2010/estados_2010.shp")


#Selecionar apenas os municípios que pertencem ao estado de CE
cemunicipios=municipios
cemunicipios=cemunicipios[cemunicipios@data$uf=='CE',]
cemunicipios@data$ordem=1:dim(cemunicipios@data)[1]

#Selecionar apenas o shape do estado de CE
ceestado=estados
ceestado=ceestado[ceestado@data$sigla=='CE',]

#Compatibilizar codigos das bases ibge e rais
munrais=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/Codigo - RAIS.csv')
munrais=munrais[order(munrais$Municipio),]
munrais=munrais[order(munrais$Municipio),]

#municípios brasil 
munbrasil=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/lat_long_municipiosbrasil.csv',
                   fileEncoding = 'WINDOWS-1252')
munbrasil=munbrasil[munbrasil$UF=='CEARÁ',c(1,2)]
munbrasil=munbrasil[order(munbrasil$mun),]

munrais=cbind(munrais,munbrasil)
#Seu animal!!! Não precisa compatibilizar por nomes. O codigo da rais é igual ao codigo do IBGE menos 
# o último dígito. Sò retirar o último dígito do ibge e mandar dar merge
munrais=munrais[,c(1,3,2)]
colnames(munrais)=c('CodigoR','CodigoI','Municipio')

#Mapa da distribuição dos postos de trabalho em 2000
#raisce2000$perc2000=raisce2000$`Empregos Formais`/sum(raisce2000$`Empregos Formais`)
#Por enquanto não vou usar distribuição percentual porque a maioria dos municípios só vai ter valor inteiro tres casas depois da vírgula

raisce2000=merge(raisce2000,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
cemunicipios2000=cemunicipios
cemunicipios2000@data=merge(cemunicipios2000@data,raisce2000,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2000@data$cor[cemunicipios2000@data$`Empregos Formais`<=1000]='yellow'
cemunicipios2000@data$cor[cemunicipios2000@data$`Empregos Formais`>1000&cemunicipios2000@data$`Empregos Formais`<=5000]='khaki'
cemunicipios2000@data$cor[cemunicipios2000@data$`Empregos Formais`>5000&cemunicipios2000@data$`Empregos Formais`<=10000]='gold2'
cemunicipios2000@data$cor[cemunicipios2000@data$`Empregos Formais`>10000&cemunicipios2000@data$`Empregos Formais`<=100000]='goldenrod'
cemunicipios2000@data$cor[cemunicipios2000@data$`Empregos Formais`>=550000]='goldenrod4'
cemunicipios2000@data=cemunicipios2000@data[order(cemunicipios2000@data$ordem),]
plot(cemunicipios2000,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2000@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Rotina')
source(compassRose(-40.8,-7))

#Mapa de 2014
raisce2014=merge(raisce2014,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
cemunicipios2014=cemunicipios
cemunicipios2014@data=merge(cemunicipios2014@data,raisce2014,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2014@data$cor[cemunicipios2014@data$`Empregos Formais`<=1000]='yellow'
cemunicipios2014@data$cor[cemunicipios2014@data$`Empregos Formais`>1000&cemunicipios2014@data$`Empregos Formais`<=5000]='khaki'
cemunicipios2014@data$cor[cemunicipios2014@data$`Empregos Formais`>5000&cemunicipios2014@data$`Empregos Formais`<=10000]='gold2'
cemunicipios2014@data$cor[cemunicipios2014@data$`Empregos Formais`>10000&cemunicipios2014@data$`Empregos Formais`<=100000]='goldenrod'
cemunicipios2014@data$cor[cemunicipios2014@data$`Empregos Formais`>=550000]='goldenrod4'
cemunicipios2014@data=cemunicipios2014@data[order(cemunicipios2014@data$ordem),]
plot(cemunicipios2014,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2014@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Rotina')
source(compassRose(-40.8,-7))



#Taxa de crescimento
raiscept=merge(raiscept,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
ceraisceptmun=cemunicipios
ceraisceptmun@data=merge(ceraisceptmun@data,raiscept,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
ceraisceptmun@data$cor[ceraisceptmun@data$TEF<=0]='tomato'
ceraisceptmun@data$cor[ceraisceptmun@data$TEF>0&ceraisceptmun@data$TEF<=0.10]='yellow'
ceraisceptmun@data$cor[ceraisceptmun@data$TEF>0.10&ceraisceptmun@data$TEF<=0.15]='khaki'
ceraisceptmun@data$cor[ceraisceptmun@data$TEF>0.15&ceraisceptmun@data$TEF<=0.20]='gold2'
ceraisceptmun@data$cor[ceraisceptmun@data$TEF>0.20]='goldenrod'
ceraisceptmun@data=ceraisceptmun@data[order(ceraisceptmun@data$ordem),]
plot(ceraisceptmun,border=T,lwd=.1,axes=F,las=1,col=ceraisceptmun@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("tomato","yellow","khaki","gold2","goldenrod"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Rotina')
source(compassRose(-40.8,-7))



rm(list=ls())

##########################
#Mapas sobre participação de ocupados com ensino superior em relação aos ocupados totais por município
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')
dbListTables(ce)
dbListFields(ce,'CE2000')

uf=c("CE2000","CE2001","CE2002","CE2003","CE2004","CE2005",
     "CE2006","CE2007","CE2008","CE2009","CE2010","CE2011","CE2012","CE2013",
     "CE2014")

lraiscept=list()
#Observação
#Até 2005 = `Grau.Instrução.2005.1985`
#Depois (i=7) =Escolaridade.após.2005
for (i in 7:length(uf)){
  k=uf[[i]]
  raisce=dbSendQuery(ce,paste("select `Município`,`Escolaridade.após.2005` from",k))
  raisce=fetch(raisce,n=-1)
  raisce=as.data.frame(table(raisce$Município,raisce$Escolaridade.após.2005))
  r=aggregate(raisce[,c(3)],by=list(Municipio=raisce$Var1),sum,na.rm=T)
  r1=raisce[raisce$Var2==1,]
  r2=raisce[raisce$Var2==9,]
  raisce=merge(r,r1,by.x=c('Municipio'),by.y=c('Var1'))
  raisce=merge(raisce,r2,by.x=c('Municipio'),by.y=c('Var1'))
  colnames(raisce)=c('Municipio','TotalEduc','Analf','TotalAnalf','Superior','TotalSuper')
  raisce$Percanalf=raisce$TotalAnalf/raisce$TotalEduc
  raisce$Percsuper=raisce$TotalSuper/raisce$TotalEduc
  raisce$Ano=1999+i
  lraiscept[[i]]=raisce
  
}  

raisce2000=lraiscept[[1]]
raisce2014=lraiscept[[15]]
raiscept=do.call(rbind.data.frame,lraiscept)
raiscepsuperior=raiscept

rm(list= ls()[!(ls() %in% c('raiscept','raisce2000','raisce2014','raiscepsuperior'))]) 
gc()

raiscept$logPercanalf=log(raiscept$Percanalf)
raiscept=raiscept[,c(1,9,10)]
raiscept=raiscept[!is.infinite(raiscept$logPercanalf),]


raiscept2=list()
raiscept=split(raiscept,paste(raiscept$Municipio))


for (i in 1:length(raiscept)){
  cept=lm(raiscept[[i]]$logPercanalf~raiscept[[i]]$Ano,data=raiscept[[i]])
  slope=as.data.frame((exp(cept$coefficients[2]))-1)
  cept2=as.data.frame(cbind(as.character(raiscept[[i]]$Municipio[1]),slope))
  raiscept2[[i]]=cept2
}

raiscept=do.call(rbind.data.frame,raiscept2)
colnames(raiscept)=c('Codigo','TPANALF')



###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/municipios_2010/municipios_2010.shp")
#Para adicionar contornos no mapa final preciso adicionar o shape file dos estados
estados=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/estados_2010/estados_2010.shp")


#Selecionar apenas os municípios que pertencem ao estado de CE
cemunicipios=municipios
cemunicipios=cemunicipios[cemunicipios@data$uf=='CE',]
cemunicipios@data$ordem=1:dim(cemunicipios@data)[1]

#Selecionar apenas o shape do estado de CE
ceestado=estados
ceestado=ceestado[ceestado@data$sigla=='CE',]

#Compatibilizar codigos das bases ibge e rais
munrais=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/Codigo - RAIS.csv')
munrais=munrais[order(munrais$Municipio),]
munrais=munrais[order(munrais$Municipio),]

#municípios brasil 
munbrasil=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/lat_long_municipiosbrasil.csv',
                   fileEncoding = 'WINDOWS-1252')
munbrasil=munbrasil[munbrasil$UF=='CEARÁ',c(1,2)]
munbrasil=munbrasil[order(munbrasil$mun),]

munrais=cbind(munrais,munbrasil)
#Seu animal!!! Não precisa compatibilizar por nomes. O codigo da rais é igual ao codigo do IBGE menos 
# o último dígito. Sò retirar o último dígito do ibge e mandar dar merge
munrais=munrais[,c(1,3,2)]
colnames(munrais)=c('CodigoR','CodigoI','Municipio')

#Mapas dos postos ocupados por analfabetos
raisce2000=merge(raisce2000,munrais,by.x=c('Municipio'),by.y=c('CodigoR'),all.x=T,all.y=F)
raisce2000=raisce2000[,c(1:10)]
cemunicipios2000=cemunicipios
cemunicipios2000@data=merge(cemunicipios2000@data,raisce2000,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2000@data$cor[cemunicipios2000@data$Percanalf<=0.05]='yellow'
cemunicipios2000@data$cor[cemunicipios2000@data$Percanalf>0.05&cemunicipios2000@data$Percanalf<=0.10]='khaki'
cemunicipios2000@data$cor[cemunicipios2000@data$Percanalf>0.10&cemunicipios2000@data$Percanalf<=0.20]='gold2'
cemunicipios2000@data$cor[cemunicipios2000@data$Percanalf>0.20&cemunicipios2000@data$Percanalf<=0.50]='goldenrod'
cemunicipios2000@data$cor[cemunicipios2000@data$Percanalf>=0.5]='goldenrod4'
cemunicipios2000@data=cemunicipios2000@data[order(cemunicipios2000@data$ordem),]
plot(cemunicipios2000,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2000@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))



#Mapa de 2014
raisce2014=merge(raisce2014,munrais,by.x=c('Municipio'),by.y=c('CodigoR'),all.x=T,all.y=F)
raisce2014=raisce2014[,c(1:10)]
cemunicipios2014=cemunicipios
cemunicipios2014@data=merge(cemunicipios2014@data,raisce2014,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2014@data$cor[cemunicipios2014@data$Percanalf<=0.05]='yellow'
cemunicipios2014@data$cor[cemunicipios2014@data$Percanalf>0.05&cemunicipios2014@data$Percanalf<=0.10]='khaki'
cemunicipios2014@data$cor[cemunicipios2014@data$Percanalf>0.10&cemunicipios2014@data$Percanalf<=0.20]='gold2'
cemunicipios2014@data$cor[cemunicipios2014@data$Percanalf>0.20&cemunicipios2014@data$Percanalf<=0.50]='goldenrod'
cemunicipios2014@data$cor[cemunicipios2014@data$Percanalf>=0.50]='goldenrod4'
cemunicipios2014@data=cemunicipios2014@data[order(cemunicipios2014@data$ordem),]
plot(cemunicipios2014,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2014@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))

#Taxa de crescimento
raiscept=merge(raiscept,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
ceraisceptmun=cemunicipios
ceraisceptmun@data=merge(ceraisceptmun@data,raiscept,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
ceraisceptmun@data$cor[ceraisceptmun@data$TPANALF>=0]='yellow'
ceraisceptmun@data$cor[ceraisceptmun@data$TPANALF<0&ceraisceptmun@data$TPANALF>=-0.10]='tomato'
ceraisceptmun@data$cor[ceraisceptmun@data$TPANALF<(-0.10)&ceraisceptmun@data$TPANALF>=-0.20]='red'
ceraisceptmun@data$cor[ceraisceptmun@data$TPANALF<(-0.20)&ceraisceptmun@data$TPANALF>=-0.40]='red3'
ceraisceptmun@data$cor[ceraisceptmun@data$TPANALF<(-0.40)]='tomato4'
ceraisceptmun@data=ceraisceptmun@data[order(ceraisceptmun@data$ordem),]
plot(ceraisceptmun,border=T,lwd=.1,axes=F,las=1,col=ceraisceptmun@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","tomato","red","red3","tomato4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))


##################################
#Mapas sobre ocupados no ensino superior em 2000
cemunicipios2000@data$cor[cemunicipios2000@data$Percsuper<=0.05]='yellow'
cemunicipios2000@data$cor[cemunicipios2000@data$Percsuper>0.05&cemunicipios2000@data$Percsuper<=0.10]='khaki'
cemunicipios2000@data$cor[cemunicipios2000@data$Percsuper>0.10&cemunicipios2000@data$Percsuper<=0.15]='gold2'
cemunicipios2000@data$cor[cemunicipios2000@data$Percsuper>0.15&cemunicipios2000@data$Percsuper<=0.20]='goldenrod'
cemunicipios2000@data$cor[cemunicipios2000@data$Percsuper>=0.20]='goldenrod4'
cemunicipios2000@data=cemunicipios2000@data[order(cemunicipios2000@data$ordem),]
plot(cemunicipios2000,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2000@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))

#Mapa de 2014
cemunicipios2014@data$cor[cemunicipios2014@data$Percsuper<=0.05]='yellow'
cemunicipios2014@data$cor[cemunicipios2014@data$Percsuper>0.05&cemunicipios2014@data$Percsuper<=0.10]='khaki'
cemunicipios2014@data$cor[cemunicipios2014@data$Percsuper>0.10&cemunicipios2014@data$Percsuper<=0.15]='gold2'
cemunicipios2014@data$cor[cemunicipios2014@data$Percsuper>0.15&cemunicipios2014@data$Percsuper<=0.20]='goldenrod'
cemunicipios2014@data$cor[cemunicipios2014@data$Percsuper>=0.20]='goldenrod4'
cemunicipios2014@data=cemunicipios2014@data[order(cemunicipios2014@data$ordem),]
plot(cemunicipios2014,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2014@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))

raiscept=raiscepsuperior
raiscept$logPercsuper=log(raiscept$Percsuper)
raiscept=raiscept[,c(1,9,10)]
raiscept=raiscept[!is.infinite(raiscept$logPercsuper),]


raiscept2=list()
raiscept=split(raiscept,paste(raiscept$Municipio))


for (i in 1:length(raiscept)){
  cept=lm(raiscept[[i]]$logPercsuper ~raiscept[[i]]$Ano,data=raiscept[[i]])
  slope=as.data.frame((exp(cept$coefficients[2]))-1)
  cept2=as.data.frame(cbind(as.character(raiscept[[i]]$Municipio[1]),slope))
  raiscept2[[i]]=cept2
}

raiscept=do.call(rbind.data.frame,raiscept2)
colnames(raiscept)=c('Codigo','TPSUPER')


#Taxa de crescimento
raiscept=merge(raiscept,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
ceraisceptmun=cemunicipios
ceraisceptmun@data=merge(ceraisceptmun@data,raiscept,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
ceraisceptmun@data$cor[ceraisceptmun@data$TPSUPER<=0]='tomato'
ceraisceptmun@data$cor[ceraisceptmun@data$TPSUPER>0&ceraisceptmun@data$TPSUPER<=0.10]='yellow'
ceraisceptmun@data$cor[ceraisceptmun@data$TPSUPER>0.10&ceraisceptmun@data$TPSUPER<=0.20]='khaki'
ceraisceptmun@data$cor[ceraisceptmun@data$TPSUPER>0.20&ceraisceptmun@data$TPSUPER<=0.40]='gold2'
ceraisceptmun@data$cor[ceraisceptmun@data$TPSUPER>0.40]='goldenrod'
ceraisceptmun@data=ceraisceptmun@data[order(ceraisceptmun@data$ordem),]
plot(ceraisceptmun,border=T,lwd=.1,axes=F,las=1,col=ceraisceptmun@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("tomato","yellow","khaki","gold2","goldenrod"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))

rm(list=ls())


#codigos 06,07,08
###### Mapa sobre participação de ocupados com mais de três anos no posto de trabalho sobre os ocupados totais


library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')
dbListFields(ce,'CE2000')

uf=c("CE2000","CE2001","CE2002","CE2003","CE2004","CE2005",
"CE2006","CE2007","CE2008","CE2009","CE2010","CE2011","CE2012","CE2013",
"CE2014")

lraiscept=list()
for (i in 1:length(uf)){
  k=uf[[i]]
  raisce=dbSendQuery(ce,paste("select `Município`, `Faixa.Tempo.Emprego` from",k))
  raisce=fetch(raisce,n=-1)
  raisce=as.data.frame(table(raisce$Município,raisce$Faixa.Tempo.Emprego))
  raisce=raisce[raisce$Var2!='{ñ',]
  r=aggregate(raisce[,c(3)],by=list(Municipio=raisce$Var1),sum,na.rm=T)
  r1=raisce[raisce$Var2=='06'|raisce$Var2==6,]
  r2=raisce[raisce$Var2=='07'|raisce$Var2==7,]
  r3=raisce[raisce$Var2=='08'|raisce$Var2==8,]
  raisce=merge(r,r1,by.x=c('Municipio'),by.y=c('Var1'))
  raisce=merge(raisce,r2,by.x=c('Municipio'),by.y=c('Var1'))
  raisce=merge(raisce,r3,by.x=c('Municipio'),by.y=c('Var1'))
  colnames(raisce)=c('Municipio','Total de Postos','TO6','TOT06','TO7','TOT07','TO8','TOT08')
  raisce$TOT060708=raisce$TOT06+raisce$TOT07+raisce$TOT08
  raisce$PERCTOT060708=raisce$TOT060708/raisce$`Total de Postos`
  raisce$Ano=1999+i
lraiscept[[i]]=raisce  
}

raisce2000=lraiscept[[1]]
raisce2014=lraiscept[[15]]
raiscept=do.call(rbind.data.frame,lraiscept)

rm(list= ls()[!(ls() %in% c('raiscept','raisce2000','raisce2014'))]) 
gc()


raiscept$logrotatividade=log(raiscept$PERCTOT060708)
raiscept=raiscept[,c(1,11,12)]
raiscept=raiscept[!is.infinite(raiscept$logrotatividade),]


raiscept2=list()
raiscept=split(raiscept,paste(raiscept$Municipio))


for (i in 1:length(raiscept)){
  cept=lm(raiscept[[i]]$logrotatividade~raiscept[[i]]$Ano,data=raiscept[[i]])
  slope=as.data.frame((exp(cept$coefficients[2]))-1)
  cept2=as.data.frame(cbind(as.character(raiscept[[i]]$Municipio[1]),slope))
  raiscept2[[i]]=cept2
}

raiscept=do.call(rbind.data.frame,raiscept2)
colnames(raiscept)=c('Codigo','TPROTATIVIDADE')

###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/municipios_2010/municipios_2010.shp")
#Para adicionar contornos no mapa final preciso adicionar o shape file dos estados
estados=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/estados_2010/estados_2010.shp")


#Selecionar apenas os municípios que pertencem ao estado de CE
cemunicipios=municipios
cemunicipios=cemunicipios[cemunicipios@data$uf=='CE',]
cemunicipios@data$ordem=1:dim(cemunicipios@data)[1]

#Selecionar apenas o shape do estado de CE
ceestado=estados
ceestado=ceestado[ceestado@data$sigla=='CE',]

#Compatibilizar codigos das bases ibge e rais
munrais=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/Codigo - RAIS.csv')
munrais=munrais[order(munrais$Municipio),]
munrais=munrais[order(munrais$Municipio),]

#municípios brasil 
munbrasil=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/lat_long_municipiosbrasil.csv',
                   fileEncoding = 'WINDOWS-1252')
munbrasil=munbrasil[munbrasil$UF=='CEARÁ',c(1,2)]
munbrasil=munbrasil[order(munbrasil$mun),]

munrais=cbind(munrais,munbrasil)
#Seu animal!!! Não precisa compatibilizar por nomes. O codigo da rais é igual ao codigo do IBGE menos 
# o último dígito. Sò retirar o último dígito do ibge e mandar dar merge
munrais=munrais[,c(1,3,2)]
colnames(munrais)=c('CodigoR','CodigoI','Municipio')

raisce2000=merge(raisce2000,munrais,by.x=c('Municipio'),by.y=c('CodigoR'),all.x=T,all.y=F)
raisce2000=raisce2000[,c(1:12)]
cemunicipios2000=cemunicipios
cemunicipios2000@data=merge(cemunicipios2000@data,raisce2000,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2000@data$cor[cemunicipios2000@data$PERCTOT060708<=0.20]='yellow'
cemunicipios2000@data$cor[cemunicipios2000@data$PERCTOT060708>0.20&cemunicipios2000@data$PERCTOT060708<=0.40]='khaki'
cemunicipios2000@data$cor[cemunicipios2000@data$PERCTOT060708>0.40&cemunicipios2000@data$PERCTOT060708<=0.60]='gold2'
cemunicipios2000@data$cor[cemunicipios2000@data$PERCTOT060708>0.60&cemunicipios2000@data$PERCTOT060708<=0.80]='goldenrod'
cemunicipios2000@data$cor[cemunicipios2000@data$PERCTOT060708>0.8]='goldenrod4'
cemunicipios2000@data=cemunicipios2000@data[order(cemunicipios2000@data$ordem),]
plot(cemunicipios2000,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2000@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))


#Mapa de 2014
raisce2014=merge(raisce2014,munrais,by.x=c('Municipio'),by.y=c('CodigoR'),all.x=T,all.y=F)
raisce2014=raisce2014[,c(1:12)]
cemunicipios2014=cemunicipios
cemunicipios2014@data=merge(cemunicipios2014@data,raisce2014,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2014@data$cor[cemunicipios2014@data$PERCTOT060708<=0.20]='yellow'
cemunicipios2014@data$cor[cemunicipios2014@data$PERCTOT060708>0.20&cemunicipios2014@data$PERCTOT060708<=0.40]='khaki'
cemunicipios2014@data$cor[cemunicipios2014@data$PERCTOT060708>0.40&cemunicipios2014@data$PERCTOT060708<=0.60]='gold2'
cemunicipios2014@data$cor[cemunicipios2014@data$PERCTOT060708>0.60&cemunicipios2014@data$PERCTOT060708<=0.80]='goldenrod'
cemunicipios2014@data$cor[cemunicipios2014@data$PERCTOT060708>0.80]='goldenrod4'
cemunicipios2014@data=cemunicipios2014@data[order(cemunicipios2014@data$ordem),]
plot(cemunicipios2014,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2014@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))

#Taxa de crescimento
raiscept=merge(raiscept,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
ceraisceptmun=cemunicipios
ceraisceptmun@data=merge(ceraisceptmun@data,raiscept,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
ceraisceptmun@data$cor[ceraisceptmun@data$TPROTATIVIDADE>=0]='yellow'
ceraisceptmun@data$cor[ceraisceptmun@data$TPROTATIVIDADE<0&ceraisceptmun@data$TPROTATIVIDADE>=-0.015]='tomato'
ceraisceptmun@data$cor[ceraisceptmun@data$TPROTATIVIDADE<(-0.015)&ceraisceptmun@data$TPROTATIVIDADE>=-0.020]='red'
ceraisceptmun@data$cor[ceraisceptmun@data$TPROTATIVIDADE<(-0.02)&ceraisceptmun@data$TPROTATIVIDADE>=-0.04]='red3'
ceraisceptmun@data$cor[ceraisceptmun@data$TPROTATIVIDADE<(-0.04)]='tomato4'
ceraisceptmun@data=ceraisceptmun@data[order(ceraisceptmun@data$ordem),]
plot(ceraisceptmun,border=T,lwd=.1,axes=F,las=1,col=ceraisceptmun@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","tomato","red","red3","tomato4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))


rm(list=ls())

#Mapas de renda 
###
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')
dbListFields(ce,'CE2010')

uf=c("CE2000","CE2001","CE2002","CE2003","CE2004","CE2005",
     "CE2006","CE2007","CE2008","CE2009","CE2010","CE2011","CE2012","CE2013",
     "CE2014")

lraiscept=list()
for (i in 1:length(uf)){
  k=uf[[i]]
raisce=dbSendQuery(ce,paste("select `Município`,`Vl.Remun.Dezembro.Nom` from",k))
raisce=fetch(raisce,n=-1)
#Excluiu-se os indíduos com valor de renda igual a zero
raisce=raisce[raisce$Vl.Remun.Dezembro.Nom!=0,]
numocupmun=as.data.frame(table(raisce$Município))
rendamun=aggregate(raisce[,c(2)],by=list(Muncipio=raisce$Município),sum,na.rm=T)
raisce=merge(numocupmun,rendamun,by.x=c('Var1'),by.y=c('Muncipio'))
colnames(raisce)=c('Codigo','NumPost','Rendmun')
#Criando variável renda média por trabalhador ocupado
raisce$rendmedtrab=raisce$Rendmun/raisce$NumPost
raisce$Ano=1999+i
lraiscept[[i]]=raisce
}


raiscept=do.call(rbind.data.frame,lraiscept)
#Trazer o data frame raiscep a valor presente e depois separar 2000 e 2014



#Inflação
#Dividir por indivíduo e trazer os valores para 2016 ou 2015 corrigindo pela inflação
#Tratamento do IPCA
ipca=read.csv("file:///home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/IPCA/IPCA.csv",header=T)
ipca=ipca[c(21:36),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 2005. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2016. Multiplico o valor monetário po esse número
ipca[17,]=NA
ipca$V3[17]=1
ipca$V3[16]=ipca$V1[16]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}

ipca=ipca[,c(1,5)]


raiscept=merge(raiscept,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)
raiscept$rendmedtrabreal=raiscept$rendmedtrab*raiscept$V3

raisce2000=raiscept[raiscept$Ano==2000,]
raisce2014=raiscept[raiscept$Ano==2014,]

rm(list= ls()[!(ls() %in% c('raiscept','raisce2000','raisce2014'))]) 
gc()


raiscept$logrendapcpreal=log(raiscept$rendmedtrabreal)
raiscept=raiscept[,c(2,1,8)]
raiscept=raiscept[!is.infinite(raiscept$logrendapcpreal),]


raiscept2=list()
raiscept=split(raiscept,paste(raiscept$Codigo))


for (i in 1:length(raiscept)){
  cept=lm(raiscept[[i]]$logrendapcpreal~raiscept[[i]]$Ano,data=raiscept[[i]])
  slope=as.data.frame((exp(cept$coefficients[2]))-1)
  cept2=as.data.frame(cbind(as.character(raiscept[[i]]$Codigo[1]),slope))
  raiscept2[[i]]=cept2
}

raiscept=do.call(rbind.data.frame,raiscept2)
colnames(raiscept)=c('Codigo','TPRENDAREAL')

rm(list= ls()[!(ls() %in% c('raiscept','raisce2000','raisce2014'))]) 
gc()


###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/municipios_2010/municipios_2010.shp")
#Para adicionar contornos no mapa final preciso adicionar o shape file dos estados
estados=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/estados_2010/estados_2010.shp")


#Selecionar apenas os municípios que pertencem ao estado de CE
cemunicipios=municipios
cemunicipios=cemunicipios[cemunicipios@data$uf=='CE',]
cemunicipios@data$ordem=1:dim(cemunicipios@data)[1]

#Selecionar apenas o shape do estado de CE
ceestado=estados
ceestado=ceestado[ceestado@data$sigla=='CE',]

#Compatibilizar codigos das bases ibge e rais
munrais=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/Codigo - RAIS.csv')
munrais=munrais[order(munrais$Municipio),]
munrais=munrais[order(munrais$Municipio),]

#municípios brasil 
munbrasil=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/lat_long_municipiosbrasil.csv',
                   fileEncoding = 'WINDOWS-1252')
munbrasil=munbrasil[munbrasil$UF=='CEARÁ',c(1,2)]
munbrasil=munbrasil[order(munbrasil$mun),]

munrais=cbind(munrais,munbrasil)
#Seu animal!!! Não precisa compatibilizar por nomes. O codigo da rais é igual ao codigo do IBGE menos 
# o último dígito. Sò retirar o último dígito do ibge e mandar dar merge
munrais=munrais[,c(1,3,2)]
colnames(munrais)=c('CodigoR','CodigoI','Municipio')

#Mapas dos postos ocupados por analfabetos
raisce2000=merge(raisce2000,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
raisce2000=raisce2000[,c(1:8)]
cemunicipios2000=cemunicipios
cemunicipios2000@data=merge(cemunicipios2000@data,raisce2000,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2000@data$cor[cemunicipios2000@data$rendmedtrabreal<=500]='yellow'
cemunicipios2000@data$cor[cemunicipios2000@data$rendmedtrabreal>500&cemunicipios2000@data$rendmedtrabreal<=1000]='khaki'
cemunicipios2000@data$cor[cemunicipios2000@data$rendmedtrabreal>1000&cemunicipios2000@data$rendmedtrabreal<=1500]='gold2'
cemunicipios2000@data$cor[cemunicipios2000@data$rendmedtrabreal>1500&cemunicipios2000@data$rendmedtrabreal<=2000]='goldenrod'
cemunicipios2000@data$cor[cemunicipios2000@data$rendmedtrabreal>2000]='goldenrod4'
cemunicipios2000@data=cemunicipios2000@data[order(cemunicipios2000@data$ordem),]
plot(cemunicipios2000,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2000@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))



#Mapa de 2014
raisce2014=merge(raisce2014,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
raisce2014=raisce2014[,c(1:8)]
cemunicipios2014=cemunicipios
cemunicipios2014@data=merge(cemunicipios2014@data,raisce2014,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2014@data$cor[cemunicipios2014@data$rendmedtrabreal<=500]='yellow'
cemunicipios2014@data$cor[cemunicipios2014@data$rendmedtrabreal>500&cemunicipios2014@data$rendmedtrabreal<=1000]='khaki'
cemunicipios2014@data$cor[cemunicipios2014@data$rendmedtrabreal>1000&cemunicipios2014@data$rendmedtrabreal<=1500]='gold2'
cemunicipios2014@data$cor[cemunicipios2014@data$rendmedtrabreal>1500&cemunicipios2014@data$rendmedtrabreal<=2000]='goldenrod'
cemunicipios2014@data$cor[cemunicipios2014@data$rendmedtrabreal>2000]='goldenrod4'
cemunicipios2014@data=cemunicipios2014@data[order(cemunicipios2014@data$ordem),]
plot(cemunicipios2014,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2014@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))


#Taxa de crescimento
raiscept=merge(raiscept,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
ceraisceptmun=cemunicipios
ceraisceptmun@data=merge(ceraisceptmun@data,raiscept,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
ceraisceptmun@data$cor[ceraisceptmun@data$TPRENDAREAL<=0]='tomato'
ceraisceptmun@data$cor[ceraisceptmun@data$TPRENDAREAL>0&ceraisceptmun@data$TPRENDAREAL<=0.05]='yellow'
ceraisceptmun@data$cor[ceraisceptmun@data$TPRENDAREAL>0.05&ceraisceptmun@data$TPRENDAREAL<=0.08]='khaki'
ceraisceptmun@data$cor[ceraisceptmun@data$TPRENDAREAL>0.08&ceraisceptmun@data$TPRENDAREAL<=0.10]='gold2'
ceraisceptmun@data$cor[ceraisceptmun@data$TPRENDAREAL>0.10]='goldenrod'
ceraisceptmun@data=ceraisceptmun@data[order(ceraisceptmun@data$ordem),]
plot(ceraisceptmun,border=T,lwd=.1,axes=F,las=1,col=ceraisceptmun@data$cor)
#legenda=as.character(c("Inferior a 0%","0% a 5%","5% a 8%","8% a 10%", "Superior a 10%"))
legenda=as.character(c("1","2","3","4", "5"))

cores=as.character(c("tomato","yellow","khaki","gold2","goldenrod"))
#legend(x=-38,y=-3, legenda, fill=cores, bty="o", title="Renda", cex=0.4 )
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)

#Adicionando escala ao mapa
library(maps)
#map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="#0000ffff", metric = FALSE, ratio=FALSE, relwidth=0.08)
#map.scale(x=-38.5, y=-7.6,relwidth=0.07,metric=T,ratio=F,cex=0.5)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)

#Rosa dos ventos
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))


rm(list=ls())

###Vou criar um mapa de estabelecimentos, como o Abel pediu. MAs para isso não vou utilizar o banco de dados
#RAISCEARA. vou utilizar um banco de dados a nivel de Brasil da RAIS com estabelecimentos

library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/EstabelecimentosBR/BDSQL')
estabece=dbConnect(SQLite(),dbname='ESTABRAISBR')
dbListTables(estabece)

dbListFields(estabece,"BRESTAB2000")

bresatb=c("BRESTAB2000","BRESTAB2001","BRESTAB2002","BRESTAB2003","BRESTAB2004","BRESTAB2005",
          "BRESTAB2006","BRESTAB2007","BRESTAB2008","BRESTAB2009","BRESTAB2010","BRESTAB2011",
          "BRESTAB2012","BRESTAB2013","BRESTAB2014")
#A partir de i=3 ou 2002 MUNICIPIO = Município

lraiscept=list()
for (i in 3:length(bresatb)){
k=bresatb[[i]]
estab=dbSendQuery(estabece,paste("select `Município`from", k, "where `Município`>=230000 AND `Município`<240000"))
estab=fetch(estab,n=-1)
estab=as.data.frame(table(estab$Município))
colnames(estab)=c('Codigo','NESTAB')
estab$ANO=1999+i
lraiscept[[i]]=estab
}

raisce2000=lraiscept[[1]]
raisce2014=lraiscept[[15]]
raiscept=do.call(rbind.data.frame,lraiscept)


raiscept$logestab=log(raiscept$NESTAB)
raiscept=raiscept[,c(1,3,4)]
raiscept=raiscept[!is.infinite(raiscept$logestab),]


raiscept2=list()
raiscept=split(raiscept,paste(raiscept$Codigo))


for (i in 1:length(raiscept)){
  cept=lm(raiscept[[i]]$logestab~raiscept[[i]]$ANO,data=raiscept[[i]])
  slope=as.data.frame((exp(cept$coefficients[2]))-1)
  cept2=as.data.frame(cbind(as.character(raiscept[[i]]$Codigo[1]),slope))
  raiscept2[[i]]=cept2
}

raiscept=do.call(rbind.data.frame,raiscept2)
colnames(raiscept)=c('Codigo','TPESTAB')

rm(list= ls()[!(ls() %in% c('raiscept','raisce2000','raisce2014'))]) 
gc()


###Tratar os shapefiles para trabalhar apenas com São Paulo
library(maptools)
#Malha de municípios
municipios=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/municipios_2010/municipios_2010.shp")
#Para adicionar contornos no mapa final preciso adicionar o shape file dos estados
estados=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/estados_2010/estados_2010.shp")


#Selecionar apenas os municípios que pertencem ao estado de CE
cemunicipios=municipios
cemunicipios=cemunicipios[cemunicipios@data$uf=='CE',]
cemunicipios@data$ordem=1:dim(cemunicipios@data)[1]

#Selecionar apenas o shape do estado de CE
ceestado=estados
ceestado=ceestado[ceestado@data$sigla=='CE',]

#Compatibilizar codigos das bases ibge e rais
munrais=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/Codigo - RAIS.csv')
munrais=munrais[order(munrais$Municipio),]
munrais=munrais[order(munrais$Municipio),]

#municípios brasil 
munbrasil=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/lat_long_municipiosbrasil.csv',
                   fileEncoding = 'WINDOWS-1252')
munbrasil=munbrasil[munbrasil$UF=='CEARÁ',c(1,2)]
munbrasil=munbrasil[order(munbrasil$mun),]

munrais=cbind(munrais,munbrasil)
#Seu animal!!! Não precisa compatibilizar por nomes. O codigo da rais é igual ao codigo do IBGE menos 
# o último dígito. Sò retirar o último dígito do ibge e mandar dar merge
munrais=munrais[,c(1,3,2)]
colnames(munrais)=c('CodigoR','CodigoI','Municipio')

#Mapa da distribuição dos estabelecimentos em 2000

raisce2000=merge(raisce2000,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
cemunicipios2000=cemunicipios
cemunicipios2000@data=merge(cemunicipios2000@data,raisce2000,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2000@data$cor[cemunicipios2000@data$NESTAB<=100]='yellow'
cemunicipios2000@data$cor[cemunicipios2000@data$NESTAB>100&cemunicipios2000@data$NESTAB<=200]='khaki'
cemunicipios2000@data$cor[cemunicipios2000@data$NESTAB>200&cemunicipios2000@data$NESTAB<=300]='gold2'
cemunicipios2000@data$cor[cemunicipios2000@data$NESTAB>300&cemunicipios2000@data$NESTAB<=500]='goldenrod'
cemunicipios2000@data$cor[cemunicipios2000@data$NESTAB>=500]='goldenrod4'
cemunicipios2000@data=cemunicipios2000@data[order(cemunicipios2000@data$ordem),]
plot(cemunicipios2000,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2000@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))

#Mapa de 2014
raisce2014=merge(raisce2014,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
cemunicipios2014=cemunicipios
cemunicipios2014@data=merge(cemunicipios2014@data,raisce2014,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2014@data$cor[cemunicipios2014@data$NESTAB<=100]='yellow'
cemunicipios2014@data$cor[cemunicipios2014@data$NESTAB>100&cemunicipios2014@data$NESTAB<=200]='khaki'
cemunicipios2014@data$cor[cemunicipios2014@data$NESTAB>200&cemunicipios2014@data$NESTAB<=300]='gold2'
cemunicipios2014@data$cor[cemunicipios2014@data$NESTAB>300&cemunicipios2014@data$NESTAB<=500]='goldenrod'
cemunicipios2014@data$cor[cemunicipios2014@data$NESTAB>=500]='goldenrod4'
cemunicipios2014@data=cemunicipios2014@data[order(cemunicipios2014@data$ordem),]
plot(cemunicipios2014,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2014@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("yellow","khaki","gold2","goldenrod","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))



#Taxa de crescimento
raiscept=merge(raiscept,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
ceraisceptmun=cemunicipios
ceraisceptmun@data=merge(ceraisceptmun@data,raiscept,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
ceraisceptmun@data$cor[ceraisceptmun@data$TPESTAB<=0]='tomato'
ceraisceptmun@data$cor[ceraisceptmun@data$TPESTAB>0&ceraisceptmun@data$TPESTAB<=0.03]='yellow'
ceraisceptmun@data$cor[ceraisceptmun@data$TPESTAB>0.03&ceraisceptmun@data$TPESTAB<=0.05]='khaki'
ceraisceptmun@data$cor[ceraisceptmun@data$TPESTAB>0.05&ceraisceptmun@data$TPESTAB<=0.08]='gold2'
ceraisceptmun@data$cor[ceraisceptmun@data$TPESTAB>0.08]='goldenrod'
ceraisceptmun@data=ceraisceptmun@data[order(ceraisceptmun@data$ordem),]
plot(ceraisceptmun,border=T,lwd=.1,axes=F,las=1,col=ceraisceptmun@data$cor)
legenda=as.character(c("1","2","3","4", "5"))
cores=as.character(c("tomato","yellow","khaki","gold2","goldenrod"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Rotina')
source(compassRose(-40.8,-7))

rm(list=ls())
##################################################
#Regressões
###
library(RSQLite)
library(ff)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')
dbListFields(ce,'CE2006')

uf=c("CE2000","CE2001","CE2002","CE2003","CE2004","CE2005",
     "CE2006","CE2007","CE2008","CE2009","CE2010","CE2011","CE2012","CE2013",
     "CE2014")

#Inserir meso regiões
meso=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/mesorregiões do ce.csv')
meso=meso[order(meso$Mun),]
#Compatibilizar codigos das bases ibge e rais
munrais=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Shapes/Codigo - RAIS.csv')
munrais=munrais[order(munrais$Municipio),]
meso=cbind(meso,munrais)
meso$Meso=as.factor(meso$Meso)
meso$codmeso=unclass(meso$Meso)
meso=meso[,c(3,5)]
meso$codmeso=as.numeric(meso$codmeso)

#Inflação
#Dividir por indivíduo e trazer os valores para 2016 ou 2015 corrigindo pela inflação
#Tratamento do IPCA
ipca=read.csv("file:///home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/IPCA/IPCA.csv",header=T)
ipca=ipca[c(21:36),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 2005. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2016. Multiplico o valor monetário po esse número
ipca[17,]=NA
ipca$V3[17]=1
ipca$V3[16]=ipca$V1[16]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}

ipca=ipca[,c(1,5)]

#Em 2006 a variável Grau.Instrução.2005.1985 muda de nome para Escolaridade.após.2005
#Tenho que garantir que isso não atrapalhe o cbind da lista criada
lpooled=list()
for (i in 7:length(uf)){
  k=uf[[i]]
cepooled=dbSendQuery(ce,paste("select `Município`, `Vl.Remun.Média.Nom`, `Faixa.Tempo.Emprego`, `Escolaridade.após.2005`, `Idade`, `Sexo.Trabalhador` from",k))
cepooled=fetch(cepooled,n=-1)
cepooled=cepooled[cepooled$Vl.Remun.Média.Nom>0,]
cepooled=cepooled[cepooled$Faixa.Tempo.Emprego>0,]
cepooled=cepooled[cepooled$Escolaridade.após.2005>0,]
cepooled=cepooled[cepooled$Idade>0,]
cepooled$Ano=1999+i
colnames(cepooled)[4]='Escolaridade'
#Adicionando meso
cepooled=merge(cepooled,meso,by.x=c('Município'),by.y=c('Codigo'))
#Inflação, salário real e log
cepooled=merge(cepooled,ipca,by.x=c('Ano'),by.y=c('Data'),all.x=T,all.y=F)
cepooled$salario=cepooled$Vl.Remun.Média.Nom*cepooled$V3
cepooled$logsalario=log(cepooled$salario)
#Idade ao quadrado
cepooled$qdidade=cepooled$Idade*cepooled$Idade


lpooled[[i]]=cepooled

}

lpooled=do.call(rbind.data.frame,lpooled)
lpooled=lpooled[,c(1,2,4,5,6,7,8,11,12)]
rm(list= ls()[!(ls() %in% c('lpooled'))]) 
gc()

#PAdronizando as categorias
#Na categoria faixa.tempo.emprego vou exluir a categoria 99( não classificado),
#depois vou converter todas as categorias para factor, compatibilizá-las e criar as binárias
lpooled=lpooled[lpooled$Faixa.Tempo.Emprego!='99',]
lpooled$tempoemprego[lpooled$Faixa.Tempo.Emprego=='01'|lpooled$Faixa.Tempo.Emprego=='1'|lpooled$Faixa.Tempo.Emprego=='02'|lpooled$Faixa.Tempo.Emprego=='2'|lpooled$Faixa.Tempo.Emprego=='03'|lpooled$Faixa.Tempo.Emprego=='3']=1
lpooled$tempoemprego[lpooled$Faixa.Tempo.Emprego=='04'|lpooled$Faixa.Tempo.Emprego=='4'|lpooled$Faixa.Tempo.Emprego=='05'|lpooled$Faixa.Tempo.Emprego=='5']=2
lpooled$tempoemprego[lpooled$Faixa.Tempo.Emprego=='06'|lpooled$Faixa.Tempo.Emprego=='6']=6
lpooled$tempoemprego[lpooled$Faixa.Tempo.Emprego=='07'|lpooled$Faixa.Tempo.Emprego=='7']=7
lpooled$tempoemprego[lpooled$Faixa.Tempo.Emprego=='08'|lpooled$Faixa.Tempo.Emprego=='8']=8
lpooled$tempoemprego=as.character(lpooled$tempoemprego)
lpooled=cbind(lpooled,model.matrix(~lpooled$tempoemprego-1))
lpooled=lpooled[,c(1,2,4:9,11:15)]
gc()

#Escolaridade
lpooled$tempoescola[lpooled$Escolaridade==1]=1
lpooled$tempoescola[lpooled$Escolaridade==2|lpooled$Escolaridade==3]=2
lpooled$tempoescola[lpooled$Escolaridade==4|lpooled$Escolaridade==5]=3
lpooled$tempoescola[lpooled$Escolaridade==6]=6
lpooled$tempoescola[lpooled$Escolaridade==7]=7
lpooled$tempoescola[lpooled$Escolaridade==8]=8
lpooled$tempoescola[lpooled$Escolaridade==9]=9
lpooled$tempoescola[lpooled$Escolaridade==10|lpooled$Escolaridade==11]=10
lpooled$tempoescola=as.character(lpooled$tempoescola)
lpooled=cbind(lpooled,model.matrix(~lpooled$tempoescola-1))
lpooled=lpooled[,c(1,2,4,5:13,15:22)]
gc()

#sexo
lpooled$Sexo.Trabalhador=as.character(lpooled$Sexo.Trabalhador)
lpooled=cbind(lpooled,model.matrix(~lpooled$Sexo.Trabalhador-1))
lpooled=lpooled[,c(1:3,5:22)]
gc()


#mesorregião
lpooled$codmeso=as.character(lpooled$codmeso)
lpooled=cbind(lpooled,model.matrix(~lpooled$codmeso-1))
lpooled=lpooled[,c(1:3,5:28)]
gc()


#criando binária na unha para tentar não estourar memória
lpooled$bin2000=0
lpooled$bin2000[lpooled$Ano==2000]=1

lpooled$bin2001=0
lpooled$bin2001[lpooled$Ano==2001]=1

lpooled$bin2002=0
lpooled$bin2002[lpooled$Ano==2002]=1

lpooled$bin2003=0
lpooled$bin2003[lpooled$Ano==2003]=1

lpooled$bin2004=0
lpooled$bin2004[lpooled$Ano==2004]=1

lpooled$bin2005=0
lpooled$bin2005[lpooled$Ano==2005]=1

lpooled$bin2006=0
lpooled$bin2006[lpooled$Ano==2006]=1

lpooled$bin2007=0
lpooled$bin2007[lpooled$Ano==2007]=1

lpooled$bin2008=0
lpooled$bin2008[lpooled$Ano==2008]=1

lpooled$bin2009=0
lpooled$bin2009[lpooled$Ano==2009]=1

lpooled$bin2010=0
lpooled$bin2010[lpooled$Ano==2010]=1

lpooled$bin2011=0
lpooled$bin2011[lpooled$Ano==2011]=1

lpooled$bin2012=0
lpooled$bin2012[lpooled$Ano==2012]=1

lpooled$bin2013=0
lpooled$bin2013[lpooled$Ano==2013]=1

lpooled$bin2014=0
lpooled$bin2014[lpooled$Ano==2014]=1


gc()


library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')

dbWriteTable(conn=ce,name='BASEMODELO',value=lpooled,append=F)


###############################################################
#### Regressão

#Ajustes para o anos 2000
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')

dbListTables(ce)
dbListFields(ce,'BASEMODELO')

############################################
basereg2000=dbSendQuery(ce,"select *from BASEMODELO where `Ano`=2000")
basereg2000=fetch(basereg2000,n=-1)
basereg2000=basereg2000[,c(1:27)]
#Excluindo a binária para região metropolitana de fortaleza lpooled$codmeso7
basereg2000=basereg2000[,c(1:26)]
#Exluindo binária para menos de uma ano de emprego lpooled$tempoemprego1
basereg2000=basereg2000[,c(1:5,7:26)]
#Omitindo a binária para analfabetos lpooled$tempoescola1
basereg2000=basereg2000[,c(1:9,11:25)]
#Omitindo a binária para sexo feminino lpooled$Sexo.Trabalhador2
basereg2000=basereg2000[,c(1:17,19:24)]

#Definindo variáveis para MQO e para a regressão quantílica
y=cbind(basereg2000$logsalario)
#ACho que essa formulação funciona
#x=cbind(as.matrix(basereg2000[,c(3,5,17:22)]))

x=cbind(as.matrix(basereg2000[,c(3,5:9,11:23)]))

MQO#
mqo2000=lm(y~x,data=basereg2000)
summary(mqo2000)

library(quantreg)

q252000=rq(y~x,data=basereg2000,tau=0.25)
summary(q252000)

q502000=rq(y~x,data=basereg2000,tau=0.50)
summary(q502000)

q752000=rq(y~x,data=basereg2000,tau=0.75)
summary(q752000)

###############################################################
#### Regressão

#Ajustes para o anos 2007
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')

dbListTables(ce)
dbListFields(ce,'BASEMODELO')

############################################
basereg2007=dbSendQuery(ce,"select *from BASEMODELO where `Ano`=2007")
basereg2007=fetch(basereg2007,n=-1)
basereg2007=basereg2007[,c(1:27)]
#Excluindo a binária para região metropolitana de fortaleza lpooled$codmeso7
basereg2007=basereg2007[,c(1:26)]
#Exluindo binária para menos de uma ano de emprego lpooled$tempoemprego1
basereg2007=basereg2007[,c(1:5,7:26)]
#Omitindo a binária para analfabetos lpooled$tempoescola1
basereg2007=basereg2007[,c(1:9,11:25)]
#Omitindo a binária para sexo feminino lpooled$Sexo.Trabalhador2
basereg2007=basereg2007[,c(1:17,19:24)]

#Definindo variáveis para MQO e para a regressão quantílica
y=cbind(basereg2007$logsalario)
x=cbind(as.matrix(basereg2007[,c(3,5:23)]))

#MQO#
mqo2007=lm(y~x,data=basereg2007)
summary(mqo2007)

library(quantreg)

q252007=rq(y~x,data=basereg2007,tau=0.25)
summary(q252007)

q502007=rq(y~x,data=basereg2007,tau=0.50)
summary(q502007)

q752007=rq(y~x,data=basereg2007,tau=0.75)
summary(q752007)


###############################################################
#### Regressão

#Ajustes para o anos 2014
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=dbConnect(SQLite(),dbname='RAISCEARA')

dbListTables(ce)
dbListFields(ce,'BASEMODELO')

############################################
basereg2014=dbSendQuery(ce,"select *from BASEMODELO where `Ano`=2014")
basereg2014=fetch(basereg2014,n=-1)
basereg2014=basereg2014[,c(1:27)]
#Excluindo a binária para região metropolitana de fortaleza lpooled$codmeso7
basereg2014=basereg2014[,c(1:26)]
#Exluindo binária para menos de uma ano de emprego lpooled$tempoemprego1
basereg2014=basereg2014[,c(1:5,7:26)]
#Omitindo a binária para analfabetos lpooled$tempoescola1
basereg2014=basereg2014[,c(1:9,11:25)]
#Omitindo a binária para sexo feminino lpooled$Sexo.Trabalhador2
basereg2014=basereg2014[,c(1:17,19:24)]

#Definindo variáveis para MQO e para a regressão quantílica
y=cbind(basereg2014$logsalario)
x=cbind(as.matrix(basereg2014[,c(3,5:23)]))

#MQO#
mqo2014=lm(y~x,data=basereg2014)
summary(mqo2014)

library(quantreg)

q252014=rq(y~x,data=basereg2014,tau=0.25)
summary(q252014)

q502014=rq(y~x,data=basereg2014,tau=0.50)
summary(q502014)

q752014=rq(y~x,data=basereg2014,tau=0.75)
summary(q752014)


#################################################
#Regressão com dados empilhados
#Ajustes para o anos 2007
library(ff)
library(RSQLite)
library(ffbase)
library(ETLUtils)
library(biglm)



#setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
#ce=dbConnect(SQLite(),dbname='RAISCEARA')

#dbListTables(ce)
#dbListFields(ce,'BASEMODELO')

#basemodelo=as.ffdf(dbGetQuery(ce, "select *from BASEMODELO"))

setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL')
ce=read.odbc.ffdf(query = "select *from BASEMODELO",odbcConnect.args = list(dsn="RAISCEARA"))
#https://www.rdocumentation.org/packages/ETLUtils/versions/1.3/topics/read.odbc.ffdf

ce=read.dbi.ffdf(query = "select *from BASEMODELO",dbConnect.args = list(drv=dbDriver('SQLite'), dbname='/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara/Base SQL/RAISCEARA'),VERBOSE = T,next.rows = 5000000)
#https://www.rdocumentation.org/packages/ETLUtils/versions/1.3/topics/read.odbc.ffdf


setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS - Ceara')
basemodelo=read.csv.ffdf(file="BASEMODELO",header=T)

basemodelo[,c(3,5,7:10,12:19,21:26,29:42)]

GLM=bigglm.ffdf(logsalario ~ Idade + qdidade + lpooled.tempoemprego2 + lpooled.tempoemprego6 +
  lpooled.tempoemprego7 + lpooled.tempoemprego8 + lpooled.tempoescola10 + lpooled.tempoescola2 +
    lpooled.tempoescola3 + lpooled.tempoescola6 + lpooled.tempoescola7 + lpooled.tempoescola8 + lpooled.tempoescola9+
    lpooled.Sexo.Trabalhador1 + lpooled.codmeso1 + lpooled.codmeso2 + lpooled.codmeso3 + lpooled.codmeso4 + lpooled.codmeso5+
  lpooled.codmeso6 + bin2001 + bin2002 + bin2003 + bin2004 + bin2005 + bin2006 + bin2007 + bin2008 + bin2009 + bin2010 + bin2011+
    bin2012 + bin2013 + bin2014,data=basemodelo)

summary(GLM)



#http://www.bnosac.be/index.php/blog/25-bigglm-on-your-big-data-set-in-open-source-r-it-just-works-similar-as-in-sas


#############################################################################
#############################################################################
#############################################################################
###Correção para a REN - Revista Econômica do Nordeste
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Base SQL')
data=dbConnect(SQLite(),dbname='RAISCEARA')
dbListTables(data)
dbListFields(data,'CE2000')

#Mapa de participação dos municípios no total da população ocupada

###Tratar os shapefiles para trabalhar apenas com Ceará
library(maptools)
#Malha de municípios
municipios=readShapeSpatial('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/municipios_2010/municipios_2010.shp')
#Para adicionar contornos no mapa final preciso adicionar o shape file dos estados
estados=readShapeSpatial("/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/estados_2010/estados_2010.shp")


#Selecionar apenas os municípios que pertencem ao estado de CE
cemunicipios=municipios
cemunicipios=cemunicipios[cemunicipios@data$uf=='CE',]
cemunicipios@data$ordem=1:dim(cemunicipios@data)[1]

#Selecionar apenas o shape do estado de CE
ceestado=estados
ceestado=ceestado[ceestado@data$sigla=='CE',]

#Compatibilizar codigos das bases ibge e rais
munrais=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/Codigo - RAIS.csv')
munrais=munrais[order(munrais$Municipio),]
munrais=munrais[order(munrais$Municipio),]

#municípios brasil 
munbrasil=read.csv('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Shapes/lat_long_municipiosbrasil.csv',
                   fileEncoding = 'WINDOWS-1252')
munbrasil=munbrasil[munbrasil$UF=='CEARÁ',c(1,2)]
munbrasil=munbrasil[order(munbrasil$mun),]

munrais=cbind(munrais,munbrasil)
#Seu animal!!! Não precisa compatibilizar por nomes. O codigo da rais é igual ao codigo do IBGE menos 
# o último dígito. Sò retirar o último dígito do ibge e mandar dar merge
munrais=munrais[,c(1,3,2)]
colnames(munrais)=c('CodigoR','CodigoI','Municipio')


#2000
r2000=as.data.frame(table(dbGetQuery(data,'select `Município` from CE2000')))
colnames(r2000)=c('Codigo',"EF")
r2000$EFP=r2000$EF/sum(r2000$EF,na.rm=T)
#Qual é o percentil 75%
format(quantile(r2000$EFP),scientific = F)
r2000$cor[r2000$EFP<=0.001474322066]='gold2'
r2000$cor[r2000$EFP>0.001474322066]='goldenrod4'
r2000=merge(r2000,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
cemunicipios2000=cemunicipios
cemunicipios2000@data=merge(cemunicipios2000@data,r2000,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2000@data=cemunicipios2000@data[order(cemunicipios2000@data$ordem),]
plot(cemunicipios2000,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2000@data$cor)
legenda=as.character(c("1","2"))
cores=as.character(c("gold2","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Rotina')
source(compassRose(-40.8,-7))


#2014
r2014=as.data.frame(table(dbGetQuery(data,'select `Município` from CE2014')))
colnames(r2014)=c('Codigo',"EF")
r2014$EFP=r2014$EF/sum(r2014$EF,na.rm=T)
#Qual é o percentil 75%
format(quantile(r2014$EFP),scientific = F)
r2014$cor[r2014$EFP<=0.0018906520]='gold2'
r2014$cor[r2014$EFP>0.0018906520]='goldenrod4'
r2014=merge(r2014,munrais,by.x=c('Codigo'),by.y=c('CodigoR'),all.x=T,all.y=F)
cemunicipios2014=cemunicipios
cemunicipios2014@data=merge(cemunicipios2014@data,r2014,by.x=c('codigo_ibg'),by.y=c('CodigoI'))
cemunicipios2014@data=cemunicipios2014@data[order(cemunicipios2014@data$ordem),]
plot(cemunicipios2014,border=T,lwd=.1,axes=F,las=1,col=cemunicipios2014@data$cor)
legenda=as.character(c("1","2"))
cores=as.character(c("gold2","goldenrod4"))
legend(x=-38.5,y=-6, legenda, fill=cores, bty="n",cex=0.9)
library(maps)
map.scale(x=-41.25, y=-7.5,relwidth=0.06,metric=T,ratio=F,cex=0.7)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Rotina')
source(compassRose(-40.8,-7))


###############
##Hora de montar as tabelas com as proporções
rm(list=ls())
library(RSQLite)
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Base SQL')
data=dbConnect(SQLite(),dbname='RAISCEARA')

base=dbGetQuery(data,'Select *from BASEMODELO where `ANO`=2000 OR `ANO`=2007 OR `ANO`=2014')
#Criar variável que represente a população total
base$poptotal=1
#Criar variáveis para as faixas de salário
base$salario=exp(base$logsalario)
#binárias de salário
base$ate1sm=0
base$ate1sm[base$salario<=880]=1
base$uma2sm=0
base$uma2sm[base$salario>880&base$salario<=1760]=1
base$doisa3sm=0
base$doisa3sm[base$salario>1760&base$salario<=2640]=1
base$tresa5sm=0
base$tresa5sm[base$salario>2640&base$salario<=4400]=1
base$cincoa10sm=0
base$cincoa10sm[base$salario>4400&base$salario<=8800]=1
base$mais10sm=0
base$mais10sm[base$salario>8800]=1

base=aggregate(base[,c(6:27,43,45:50)],by=list(Ano=base$Ano),sum,na.rm=T)
base=as.data.frame(t(base))
base$A2000=base$V1/base[24,1]
base$A2007=base$V2/base[24,2]
base$A2014=base$V3/base[24,3]
base=round(base,4)
 
setwd('/home/bmiyamoto/Documentos/Pesquisa/RAIS-Ceara/Correções para  REN')
write.csv(base,file='Tabpreliminar.csv',row.names=T)



