  install.packages('OpenImageR')
library(OpenImageR) #instalar

#lendo imagem TUDO
tudo=readImage("entornos utfpr 2005.jpg")
imageShow(tudo)
dim(tudo) #linhas x colunas x matrix(R, G, B)
tudo2 = tudo

#lendo imagem verde
verde=readImage('Florestas.jpg')
imageShow(verde)

#lendo imagem ruim
residencias=readImage('Residências.jpg')
imageShow(residencias)

#lendo imagem fundo
terrenos=readImage('Terrenos.jpg')
imageShow(terrenos)

#(R, G, B)
c(verde[,,1])
length(c(verde[,,1]))
mverde=cbind(c(verde[,,1]),
             c(verde[,,2]),
             c(verde[,,3]))

mterrenos=cbind(c(terrenos[,,1]),
                c(terrenos[,,2]),
                c(terrenos[,,3]))

mresidencias=cbind(c(residencias[,,1]),
                   c(residencias[,,2]),
                   c(residencias[,,3]))
dim(mverde)
dim(mterrenos)
dim(mresidencias)

mverde=mverde[sample(1:2831938,10000),]

mterrenos=mterrenos[sample(1:651984,10000),]

mresidencias=mresidencias[sample(1:421176,5000),]

#arrumando dados (0=terrenos, 1=verde, 2=residencias)
cbind(mterrenos,0)
cbind(mverde,1)
cbind(mresidencias,2)

dados=rbind(cbind(mterrenos,0),cbind(mverde,1),cbind(mresidencias,2))
head(dados)
colnames(dados)=c("R","G","B",'Y')
head(dados)
dim(dados)

install.packages("randomForest")
library(randomForest)
modelo=randomForest(as.factor(Y)~R+G+B,data=dados)
print(modelo)
importance(modelo)

#predicao imagem tudo
mtudo=cbind(c(tudo[,,1]),
            c(tudo[,,2]),
            c(tudo[,,3]))

head(mtudo)
colnames(mtudo)=c('R',"G","B")
head(mtudo) #(0=terrenos, 1=verde, 2=residencias)

pred=predict(modelo,newdata = mtudo)
table(pred)
table(as.numeric(pred)) #transforma numero

pred=as.numeric(pred)-1 #voltar ao original
table(pred)

ncol(tudo[,,3]) #quantas colunas original
mpred=matrix(pred,ncol=1441)

imageShow(mpred)
imageShow(tudo)

#(0=terrenos, 1=verde, 2=residencias)

tudo2=tudo
tudo2[,,1][pred==0]=0
tudo2[,,2][pred==0]=0
tudo2[,,3][pred==0]=0

tudo2[,,1][pred==1]=0
tudo2[,,2][pred==1]=0
tudo2[,,3][pred==1]=0

tudo2[,,1][pred==2]=1
tudo2[,,2][pred==2]=0
tudo2[,,3][pred==2]=1
imageShow(tudo2)
