#______________________________________________________________
#Etapa 1
#_____________________________________________________________

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
#_____________________________________________________________
#Etapa 2
#_______________________________________________________________

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
ManualDoble<-as.integer(Cap[[1]])
Etapa4<-cbind(Caus,Rec1,Rec2,CapAut,ManualDoble,Valor3,Valor4,Bien3,Bien,Rev)
return(Etapa4)
}
