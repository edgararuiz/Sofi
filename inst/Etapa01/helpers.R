#______________________________________________________________
#Etapa 1
#_____________________________________________________________
#####
Ordenar<-function(IDm,CausaD)
{
  IDm <- as.integer(IDm)
  CausaD <- as.character(CausaD)
  CapA<-DefCap(cau=CausaD)
  CapAut<-as.integer(CapA[[1]])
  codcap<-as.data.frame(cbind(IDm,CausaD,CapAut))
  cat("~~~ Ordenando Datos                               ~~~ \n")
  codcapor<-codcap[order(CausaD),]
  Id<-1:dim(codcapor)[1]
  codord<-cbind(codcapor,Id)
  Tem<-codord[codord$CapAut==1,]
  PN<-c(1,dim(Tem)[1])
  for (i in 2:20){
    Tem<-codord[codord$CapAut==i,]
    PN<-rbind(PN,c(i,dim(Tem)[1]))}
  list(codord,PN)
}


OptFact<-function(V1,Ns,n)
  {
  cat("~~~ Inicia etapa de muestra                       ~~~ \n")
  t<-0;pr<-0
  while (pr==0){t<-t+1
                if (Ns[t]>0){
                  NT<-c(1:Ns[t])
                  Capit<-as.integer(rep(V1[t],Ns[t]))
                  FactorExp<-rep(Ns[t]/n[t],Ns[t])
                  EnMuestra<-as.integer(srswor(n[t],Ns[t]))
                  MuestraGr<-data.frame(NT,Capit,FactorExp,EnMuestra)
                  cat(t," ")
                  Ac<-Ns[t]
                  pr<-1}}
  for (i in (t+1):20) {Ac[i]<-Ac[i-1]+Ns[i]}
  for (i in (t+1):20)
    {
      if (Ns[i]>0){
        NT<-c((Ac[i-1]+1):Ac[i])
        N<-Ns[i];m<-n[i]
        Capit<-as.integer(rep(V1[i],N))
        FactorExp<-rep(N/m,N)
        EnMuestra<-as.integer(srswor(m,N))
        Mu<-data.frame(NT,Capit,FactorExp,EnMuestra)
        MuestraGr<-rbind(MuestraGr,Mu)
        cat(i," ")}
    }
  cat("~~ Fin ~~~ \n")
  return(MuestraGr)
  }

optn<-function(N,p,E)
    {
      z<-1.96
      S<-z^2*p*(1-p)*N
      I<-E^2*(N-1)+z^2*p*(1-p)
      n<-as.integer(S/I)
      return(n)
    }

#####
#_____________________________________________________________
#Etapa 2
#_______________________________________________________________
#####
Revic<-function(CAUSADEF,RECODCBD,RECODCBD2){
dimn<-length(CAUSADEF)
dimn
Valor3<-rep(0,dimn)
Valor4<-rep(0,dimn)
Bien3<-rep(0,dimn)
Bien<-rep(0,dimn)
Rev<-rep(0,dimn)
Caus<-as.character(CAUSADEF)
Rec1<-as.character(RECODCBD)
Rec2<-as.character(RECODCBD2)

for (i in 1:dimn)
{
  Ca<-substr(Caus[i],1,3)
  R1<-substr(Rec1[i],1,3)
  R2<-substr(Rec2[i],1,3)
  if (Ca==R1 & R1==R2) {Valor3[i]<-1;Bien3[i]<-1}
  else if (Ca==R1 & R1!=R2) {Valor3[i]<-2}
  else if (Ca==R2 & R1!=R2) {Valor3[i]<-3}
  else if (Ca!=R1 & R1==R2) {Valor3[i]<-4}
  else if (Ca!=R1 & R1!=R2) {Valor3[i]<-5}
  else Valor[i]<-6
}

for (i in 1:dimn)
{
  Ca<-substr(Caus[i],1,4)
  if (substr(Caus[i],4,4)=="X") {Ca<-substr(Caus[i],1,3)}
  R1<-substr(Rec1[i],1,4)
  if (substr(Rec1[i],4,4)=="X") {Ca<-substr(Rec1[i],1,3)}
  R2<-substr(Rec2[i],1,4)
  if (substr(Rec2[i],4,4)=="X") {Ca<-substr(Rec2[i],1,3)}
  if (Ca==R1 & R1==R2) {Valor4[i]<-1;Bien[i]<-1}
  else if (Ca==R1 & R1!=R2) {Valor4[i]<-2;Rev[i]<-1}
  else if (Ca==R2 & R1!=R2) {Valor4[i]<-3;Rev[i]<-1}
  else if (Ca!=R1 & R1==R2) {Valor4[i]<-4}
  else if (Ca!=R1 & R1!=R2) {Valor4[i]<-5;Rev[i]<-1}
  else Valor[i]<-6
}

Valor3<-as.integer(Valor3)
Valor4<-as.integer(Valor4)
Bien3<-as.integer(Bien3)
Bien<-as.integer(Bien)
Rev<-as.integer(Rev)
Cap1<-DefCap(CAUSADEF)
Cap<-DefCap(RECODCBD)
CapAut<-as.integer(Cap1[[1]])
ManualD<-as.integer(Cap[[1]])
Etapa4<-cbind(Caus,Rec1,Rec2,CapAut,ManualD,Valor3,Valor4,Bien3,Bien,Rev)
#Etapa4<-cbind(CapAut,ManualD,Valor3,Valor4,Bien3,Bien,Rev)
return(Etapa4)
}


Frecu<-function(TabFrec){
  d<-dim(TabFrec)
  PosDif<-matrix(NA, d[1], 35)
  for (i in 1:d[1]){
    PosDif[i,1]<-i
    PosDif[i,2]<-sum(TabFrec[i,])
    p<-3
    for (j in 1:d[2]) {
      if (TabFrec[i,j]!=0) {p<-p+1
                            PosDif[i,p]<-j}
    }
    PosDif[i,3]<-p-3
  }
  ColFin<-max(PosDif[,3])+2
  TabFrec2<-matrix(NA, d[1]*2, ColFin)
  rown<-rownames(TabFrec)
  coln<-colnames(TabFrec)
  TabFrec2[1,1]<-rown[1]
  for (j in 1:PosDif[1,3]){
  TabFrec2[1,j+1]<-coln[PosDif[1,j+3]]
  TabFrec2[2,j+1]<-TabFrec[1,PosDif[1,j+3]]
  TabFrec2[1,ColFin]<-PosDif[1,2]
  TabFrec2[2,ColFin]<-PosDif[1,2]
  }
  for (i in 2:d[1]){
  TabFrec2[i+(i-1),1]<-rown[i]
  for (j in 1:PosDif[i,3]){
    TabFrec2[i+(i-1),j+1]<-coln[PosDif[i,j+3]]
    TabFrec2[i+(i-1)+1,j+1]<-TabFrec[i,PosDif[i,j+3]]
    }
  TabFrec2[i+(i-1),ColFin]<-PosDif[i,2]
  TabFrec2[i+(i-1)+1,ColFin]<-PosDif[i,2]
  }
return(TabFrec2)
}

InterVal<-function(CapAutBien,Pob,Error){
  #INTERVALOS DE CONFIANZA
  Muestra<-rep(0,21)
  Bien<-Muestra;BienT<-Muestra;Poblacion<-Muestra;P<-Muestra;Pn<-Muestra;FactorExp<-Muestra
  
  for (i in 1:20)
  {TemCap<-CapAutBien[CapAutBien[,1]==i,]
   Muestra[i]<-nrow(TemCap)
   Bien[i]<-sum(TemCap[,2])
   FactorExp[i]<-Pob[i]/Muestra[i]
   Pn[i]<-(Bien[i]/Muestra[i])*100
   BienT[i]<-sum(TemCap[,2])*FactorExp[i]
   Poblacion[i]<-Pob[i]
   P[i]<-(BienT[i]/Pob[i])*100
  }

  i<-21
  Muestra[i]<-nrow(CapAutBien)
  Bien[i]<-sum(CapAutBien[,2])
  Pn[i]<-(Bien[i]/Muestra[i])*100
  Poblacion[i]<-sum(Pob)
  BienT[i]<-sum(BienT,na.rm = T)
  P[i]<-(BienT[i]/Poblacion[i])*100
  Cap<-c(1:21)
  
  psup<-(qbeta(1-Error/2,Bien+.5,Muestra-Bien+.5))*100
  pinf<-(qbeta(Error/2,Bien+.5,Muestra-Bien+.5))*100
  psup2<-P+(psup-Pn)
  pinf2<-P-(Pn-pinf)
  pinf3<-P-(1-(Muestra/Poblacion))^(1/2)*(P-pinf2)
  psup3<-P+(1-(Muestra/Poblacion))^(1/2)*(psup2-P)
  Int.Conf<-cbind(Cap,Poblacion,BienT,Muestra,Bien,P,pinf3,psup3)
  Int.Conf<-Int.Conf[!is.na(Int.Conf[,3]),]
  return(Int.Conf)
}