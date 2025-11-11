firstindex=function(x,a){
  x=sort(x)
  for (i in seq(1,length(x))){
    if (x[i]==a){
      i
      break
    }
  }
  if (i==length(x)){
    print("Este valor que has insertado no se encuentra en el vector")
  }
  
  i
}

skew=function(x){
   m3 <- sum((x-mean(x))^3)/length(x)
   s3 <- sd(x)^3
   m3/s3
 }

kurtosis <- function(x){
  m4 <- sum((x-mean(x))^4)/length(x)
  s4 <- var(x)^2
  m4/s4
}

medarm=function(x){
  length(x)/sum(1/x)
}

medgeo=function(x){
  if (prod(x)>0) {
    prod(x)^(1/length(x))
  }
  else {
    if (length(x)%%2==1){
      -prod(abs(x))^(1/length(x))
    }
    else {
      NaN
    }
  }
}

medgeo2=function(x){
  if (prod(x)<=0){
    print("Cuidado, hay numeros negativos o iguales a cero ")
  }
  else{
    exp(sum(log(x))/length(x))
  }
}

medqua=function(x){
  sign(sum(x))*sqrt(sum(x*x)/length(x))
}


a=airquality
days=as.numeric(a$Day);
temps=as.numeric((a$Temp-32)*5/9+273.15) #Pasamos las T a Kelvin
winds=as.numeric(a$Wind*1.60934)#Pasamos las v a km/h
t=seq(1:length(days))#Definimos un vector con todos los días que se midió
source("firstindex.R") #Una función que me diga donde está el primer término de un vector igual a un valor a
o=firstindex(a$Month,5); p=firstindex(a$Month,6); q=firstindex(a$Month,7); r=firstindex(a$Month,8); s=firstindex(a$Month,9)
par(mfrow=c(2,2))
plot(t,a$Ozone,pch=18,col="red", main="Concentración de Ozono",xlab="Días", ylab="Ozono(ppb)")
lines(t[o]*rep(1,length(a$Ozone)),a$Ozone,lty=3,col="grey"); lines(t[p]*rep(1,length(a$Ozone)),a$Ozone,lty=3,col="grey")
lines(t[q]*rep(1,length(a$Ozone)),a$Ozone,lty=3,col="grey"); lines(t[r]*rep(1,length(a$Ozone)),a$Ozone,lty=3,col="grey")
lines(t[s]*rep(1,length(a$Ozone)),a$Ozone,lty=3,col="grey")
legend(x="topright",legend=c("Ozono","Meses"),pch=18, lty=3, col=c("red","grey"),cex=0.56,bty="n")
plot(t,a$Solar.R,pch=15, col="green",main="Concentración de radianza solar",xlab="Días",ylab="Lum (lang)")
lines(t[o]*rep(1,length(a$Solar.R)),a$Solar.R,lty=3,col="grey"); lines(t[p]*rep(1,length(a$Solar.R)),a$Solar.R,lty=3,col="grey")
lines(t[q]*rep(1,length(a$Solar.R)),a$Solar.R,lty=3,col="grey"); lines(t[r]*rep(1,length(a$Solar.R)),a$Solar.R,lty=3,col="grey")
lines(t[s]*rep(1,length(a$Solar.R)),a$Solar.R,lty=3,col="grey")
legend(x="topright",legend=c("Radianza","Meses"),pch=15, lty=3, col=c("green","grey"),cex=0.56,bty="n")
plot(t,winds,pch=16, col="blue",main="Velocidad del viento",xlab="Días", ylab="vel (km/h)")
lines(t[o]*rep(1,length(a$Wind)),winds,lty=3,col="grey"); lines(t[p]*rep(1,length(a$Wind)),winds,lty=3,col="grey")
lines(t[q]*rep(1,length(a$Wind)),winds,lty=3,col="grey"); lines(t[r]*rep(1,length(a$Wind)),winds,lty=3,col="grey")
lines(t[s]*rep(1,length(a$Wind)),winds,lty=3,col="grey")
legend(x="topright",legend=c("Velocidad","Meses"),pch=16, lty=3, col=c("blue","grey"),cex=0.56,bty="n")
plot(t,temps,pch=17, col="yellow",main="Temperatura del aire", xlab="Días", ylab="T(K)")
lines(t[o]*rep(1,length(a$Temp)),temps,lty=3,col="grey"); lines(t[p]*rep(1,length(a$Temp)),temps,lty=3,col="grey")
lines(t[q]*rep(1,length(a$Temp)),temps,lty=3,col="grey"); lines(t[r]*rep(1,length(a$Temp)),temps,lty=3,col="grey")
lines(t[s]*rep(1,length(a$Temp)),temps,lty=3,col="grey")
legend(x="topright",legend=c("Temperatura","Meses"),pch=17, lty=3, col=c("blue","grey"),cex=0.56,bty="n")

#dev.off()
n_bins=as.integer((length(t))^(1/2)); print(n_bins)
par(mfrow=c(2,2))
hist(a$Ozone,breaks=n_bins,col="red", main="Histograma concentración de ozono",xlab="Concentraciones de Ozono (pbb)")
hist(a$Solar.R,breaks=n_bins,col="blue", main="Histograma de la radianza",xlab="Radianza en el aire (lang)")
hist(winds,breaks=n_bins, col="green", main="Histograma velocidad del viento",xlab="Velocidad del aire (km/h)")
hist(temps,breaks=n_bins,col="yellow", main="Histograma de temperaturas", xlab="Temperaturas (K)")

#Las funciones se definen en las diapositivas.
source("medarm.R")
source("medgeo.R")
source("medqua.R")
source("mimoda.R")
source("medgeo2.R")

Rad=a$Solar.R[!is.na(a$Solar.R)]
medarmSR=medarm(Rad)
medgeoSR=medgeo(Rad)
medquaSR=medqua(Rad)
medariSR=mean(Rad)
medianSR=median(Rad)
print(medarm(Rad)); print(medgeo(Rad)); print(medqua(Rad)); print(mean(Rad)); print(median(Rad))

medgeoSR=medgeo2(Rad)
print(medarmSR); print(medgeoSR); print(medquaSR);print(medariSR); print(medianSR)
#Ahora, en efecto, funciona :)

x=sample(1:length(Rad),5) #Los indices de los datos que cambiaremos. Los 
#cambiaremos por un término aleatorio dado por una distribución uniforme, entre el minimo y máximo de la medida.
Rad2=Rad
for (i in 1:length(Rad)){
  for (j in 1:length(x)){
    if (i==x[j]){
      Rad2[i]=runif(1,min=min(Rad),max=max(Rad))
        }
    }
}

fluctuaciones=c((mean(Rad2)-mean(Rad))/mean(Rad),(medarm(Rad2)-medarm(Rad))/medarm(Rad), 
                (medgeo2(Rad2)-medgeo2(Rad))/medgeo2(Rad),(medqua(Rad2)-medqua(Rad))/medqua(Rad),
                (median(Rad2)-median(Rad))/median(Rad))
print(fluctuaciones)


rankQ=IQR(winds); desv=sd(winds); madwinds=mad(winds,constant=1)
#Para saber el valor menos robusto, repetimos el proceso con el mismo Rad2
x=sample(1:length(winds),5) #Los indices de los datos que cambiaremos. Los 
winds2=winds
for (i in 1:length(winds)){
  for (j in 1:length(x)){
    if (i==x[j]){
      winds2[i]=runif(1,min(winds),max(winds))
    }
  }
}
fluctuaciones2=c((IQR(winds2)-IQR(winds))/IQR(winds),(sd(winds2)-sd(winds))/sd(winds),(mad(winds2,constant=1)
                 -mad(winds,constant=1))/mad(winds,constant=1))
print(fluctuaciones2)

#Hay que repetir todo el proceso con todas las variables. Los NA nos fastidian,
#pero sólo falta quitarlos en ozone
Ozone=a$Ozone[!is.na(a$Ozone)]
#Calculando ahora todos los coeficientes
rankQwinds=IQR(winds); desvwinds=sd(winds); madwinds=mad(winds,constant=1); pearwinds=sd(winds)/abs(mean(winds))
rankQozone=IQR(Ozone); desvOzone=sd(Ozone); madOzone=mad(Ozone,constant=1); pearozone=sd(Ozone)/abs(mean(Ozone))
rankQtemps=IQR(temps); desvtemps=sd(temps); madtemps=mad(temps,constant=1); peartemps=sd(temps)/abs(mean(temps))
rankQRad=IQR(Rad); desvRad=sd(Rad); madRad=mad(Rad,constant=1); pearRad=sd(Rad)/abs(mean(Rad))
ranks=c(rankQozone,rankQRad,rankQtemps,rankQwinds); 
names=c("ozone","Rad","temps","winds")
desvs=c(desvOzone,desvRad,desvtemps,desvwinds)
mads=c(madOzone,madRad,madtemps,madwinds)
Coeff_Pearson=c(pearozone,pearRad,peartemps,pearwinds)
table=data.frame(names,ranks,desvs,mads,Coeff_Pearson)
print(table)

#Usaremos las funciones que ya vienen definidas en las presentaciones
set.seed(111)
source("coeffisher.R"); source("kurtosis.R")
tf=skew(temps); tkurt=kurtosis(temps)
print(tf); print(tkurt)

x=seq(min(temps), max(temps),length=1000)
distgaus=dnorm(x, 300, sd=sd(temps))
h1=hist(temps,breaks=n_bins, prob=TRUE, col="pink", main="Histograma de temperaturas",xlab="Temperatuas (K)")
lines(h1$mids,h1$density,col="blue",lwd=3)
lines(x, distgaus, col = "red", lwd = 2)
legend(x="topright",legend=c("Temperatura","Polinomio de frecuencias","Distribución Gaussiana centrada"), lty=c(2,1,1), col=c("pink","blue","red"),cex=0.5,bty="n")

ozono=sort(Ozone)
plot(ecdf(ozono),verticals=TRUE,lwd=2,xlab="Concentración de ozono (ppb)",ylab="Función de densidad acumulada")
lines(seq(1,length(ozono)),0.5*rep(1,length(ozono)),lty=2)
lines(30*rep(1,length(ozono)),cumsum(ozono)/max(cumsum(ozono)),lty=2)

median(ozono)

table2=data.frame(a$Ozone,a$Solar.R,a$Wind,a$Temp)
pairs(table2, main="Concentration of meteorological variables")


