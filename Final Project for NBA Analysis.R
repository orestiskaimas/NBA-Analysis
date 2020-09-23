#1#eisagogi dedomenon
nba=read.csv("C:\\NBA2016-data-for-final-project.csv",header=T,sep=";")
summary(nba)
dim(nba)
#2#afairesi_NA
a=0;
for (i in 1:528){
if(sum(is.na(nba[i,]))>0)
a=rbind(a,i)
}
a
nba=nba[-c(47,80,135),]
dim(nba)
#3#evresi gia kathe omada pontwn
levels(nba$Tm)
k=1;
sumponton=rep(0,length(levels(nba$Tm)))
for(j in levels(nba$Tm)){
for (i in 1:525){
if (nba$Tm[i]==j) sumponton[k]=sumponton[k]+ nba$PTS[i]
}
k=k+1;
}
#sinolika apotelesmata ponton
cbind(levels(nba$Tm),sumponton)

#4#arithmitikos mesos
#prepei na vro posa paixnidia epaixe i kathe omada

k=1
maxagones=rep(0,length(levels(nba$Tm)));
for(j in levels(nba$Tm)){
maxagones[k]=max(nba$G[which(nba$Tm==j)])
k=k+1;
}

mesos=sumponton/maxagones

#sinolika apotelesmata meson ponton
cbind(levels(nba$Tm),mesos)

#5#sintelestis metavlitotitas
temp=0
k=1
sintelestis=rep(0,length(levels(nba$Tm)))
for(j in levels(nba$Tm)){
sintelestis[k]=sd(nba$PTS[which(nba$Tm==j)])/mean(nba$PTS[which(nba$Tm==j)])
k=k+1;
}
#sinolika apotelesmata ana omada
cbind(levels(nba$Tm),sintelestis)

#6# 5prwtes omades me vasi tin simetoxi paixton
temp=0
k=1
simetoxes=rep(0,length(levels(nba$Tm)))
for(j in levels(nba$Tm)){
simetoxes[k]=sum(nba$G[which(nba$Tm==j)])
k=k+1;
}
teliko=as.data.frame(cbind(levels(nba$Tm),simetoxes))

t1=as.factor(teliko$V1[order(teliko$simetoxes,decreasing = T)])
t2=teliko$simetoxes[order(teliko$simetoxes,decreasing = T)]
##i teliki katataxi me fthinousa seira
data.frame(omada=t1,simetoxes=t2)

#oi 5
data.frame(omada=t1,simetoxes=t2)[1:5,]

#7 grafimata

plot(sumponton,type='h',xaxt = "n",main='Sinolo pontwn ana omada',ylab='Points',xlab='Team')
axis(1, at=1:30, labels = FALSE)
text(1:30, par("usr")[3] - 0.2, labels = levels(nba$Tm), srt = 45, pos = 1, xpd = TRUE)



plot(mesos,type='h',xaxt = "n",main="mesos oros pontwn",ylab='point',xlab='team')
axis(1, at=1:30, labels = FALSE)
text(1:30, par("usr")[3] - 0.2, labels = levels(nba$Tm), srt = 45, pos = 1, xpd = TRUE)


plot(sintelestis,type='h',xaxt = "n",main="CV gia kathe omada", ylab='CV',xlab='team')
axis(1, at=1:30, labels = FALSE)
text(1:30, par("usr")[3] - 0.2, labels = levels(nba$Tm), srt = 45, pos = 1, xpd = TRUE)



#8# ypologismos GIni
library(reldist)
k=1
gini_p=rep(0,length(levels(nba$Tm)))
for(j in levels(nba$Tm)){
gini_p[k]=gini(nba$PTS[which(nba$Tm==j)])
k=k+1;
}
#Gini ana POINTS
cbind(levels(nba$Tm),gini_p)


k=1
gini_m=rep(0,length(levels(nba$Tm)))
for(j in levels(nba$Tm)){
gini_m[k]=gini(nba$MP[which(nba$Tm==j)])
k=k+1;
}
#Gini ana lepta
cbind(levels(nba$Tm),gini_m)

#9
##posoi paixtes kato apo 5 lepta
sum(nba$MP/nba$G<=5)
which(nba$MP/nba$G<=5)
k=1
gini_p2=rep(0,length(levels(nba$Tm)))
for(j in levels(nba$Tm)){
gini_p2[k]=gini(nba$PTS[which((nba$Tm==j) & nba$MP/nba$G>5)])
k=k+1;
}
#Gini ana POINTS
cbind(levels(nba$Tm),gini_p,gini_p2)
##diafora stis dyo periptwseis
gini_p2-gini_p


k=1
gini_m2=rep(0,length(levels(nba$Tm)))
for(j in levels(nba$Tm)){
gini_m2[k]=gini(nba$MP[which((nba$Tm==j) & nba$MP/nba$G>5)])
k=k+1;
}
#Gini ana POINTS
cbind(levels(nba$Tm),gini_m,gini_m2)
gini_m2-gini_m

#den yparxei simantiki diafora afou genika itan mono 27 ta atoma me afti tin idiotita

#10# histogramms
par(mfrow=c(1,3))
hist(nba$X3P[nba$X3PA>0]/nba$X3PA[nba$X3PA>0],main='pososta tripontwn',xlab='three points')
hist(nba$X2P[nba$X2PA>0]/nba$X2PA[nba$X2PA>0],main='pososta dipontwn',xlab='2 points')
hist(nba$FT[nba$FT>0]/nba$FTA[nba$FT>0],main='pososta eleftherwn volwn',xlab='FT')


#11#Grafima gia tous kalyterous sta 2ponta kai 3ponta me proipothesi na min eixan soutarei ipervolika
graphics.off()
temp=which(nba$X2PA>0 &(nba$X3PA<=20) & nba$MP/nba$G>5)
temp2=which(nba$X3PA>0 &(nba$X3PA<=20) & nba$MP/nba$G>5)
temp3=sort(nba$X3P[temp2]/nba$X3PA[temp2],decreasing = T)[10]
kalyteroi=which((nba$X3PA >0) &(nba$X3PA<=20) & (nba$MP/nba$G >5) & (nba$X3P/nba$X3PA>=temp3))
nba$Player[kalyteroi]


plot(temp,nba$X2P[temp]/nba$X2PA[temp],main='pososta dipontwn',xlab='',ylab='percent')
text( y=nba$X2P[kalyteroi]/nba$X2PA[kalyteroi],  x=kalyteroi ,labels = nba$Player[kalyteroi])

plot(temp2,nba$X3P[temp2]/nba$X3PA[temp2],main='pososta tripontwn',xlab='',ylab='percent')
text( y=nba$X3P[kalyteroi]/nba$X3PA[kalyteroi],  x=kalyteroi ,labels = nba$Player[kalyteroi])
#12#5 kalyteroi point guard me vasi ta rebound
rebound_5tou=sort(nba$TRB[which(nba$Pos=='PG')],decreasing=T)[5]
###einai oi
nba$Player[which(nba$Pos=='PG' & nba$TRB >=rebound_5tou)]
#13## me ta parakatw statistika
nba[which(nba$Pos=='PG' & nba$TRB >=rebound_5tou),]
#14## barplot

onomata=levels(nba$Tm)[order(sumponton,decreasing = F)]

barplot(sort(sumponton),horiz = T, names.arg=onomata,las=2,cex.names=0.8)
barplot(c(rep(0,15),mean(sumponton),rep(0,15)),  horiz = T, col='red', add=T,xaxt="n")

#15#Plithos me score panw apo 1000 pontous
sum(nba$PTS>=1000)
#16#t-eleghos kata poso oi Power Guerd exoun paromoia sixnotita pontwn

t.test(nba$PTS[nba$Pos=='PG'],nba$PTS[nba$Pos=='PF'])
##pvalue= 0.2838
##ara den  aporripto tin isotita twn meson, ara den yparxei diafora gia tis dyo katigories
#17##to grammiko modelo eleghei tin grammiki sxesi metaxi mia metavlitis me alles
##edw tha elegxoume ta pososta tripontwn se sxesime ta pososta 2pontwn
##theloume i katanomi pou akolouthei i metavliti pou elghoume na akolouthei kanoniki katanomi
y=NULL
x=NULL
for (i in 1:length(nba$X3P)){
if (nba$X3PA[i]>0){
y[i]=nba$X3P[i]/nba$X3PA[i]}else y[i]=0
if (nba$X2PA[i]>0){
x[i]=nba$X2P[i]/nba$X2PA[i]}else x[i]=0
}
hist(y)
qqnorm(y)
qqline(y)

##oson afora tin ypothesi kanonikotitas,den fainetai na ikanopoieitai
plot(x,y,main='scatterplot',ylab='3pt',xlab='2pt')
#oson afora tin grammikotita den vlepoume kati xekatharo
summary(lm(y~x))
##kata arxas na poume oti to modelo prosarmozei poly kaka afou Multiple R-squared:  0.03544
##i metavliti 2points einai statistika simantiki afou to pvalue tis einai<0.05
##ara paizei rolo kai pio sigekrimena gia kathe 1% exoume meiwsi 0.26073% gia ta triponta

#18

boxplot(nba$Age,main='katanomi ilikias paixtwn')
boxplot(nba$MP,main='katanomi xronou simmetoxis')
summary(nba$ORB)
boxplot(nba$DRB[nba$ORB<20],nba$DRB[(nba$ORB>=20) & (nba$ORB<30)],nba$DRB[nba$ORB>=30],main='Sxesi amintikwn rebound me epithetika se auxousa kaitigoriopoiisi')
##paratirw oti oso pio pola epithetika toso afxanoun kai ta amintika
summary(nba$STL)
boxplot(nba$BLK[nba$STL<26],nba$BLK[(nba$STL>=26) & (nba$STL<50)],nba$BLK[nba$STL>=50],main='Sxesi klepsimata me tapes se auxousa kaitigoriopoiisi')
##yparxei kapoia dinamiki oxi toso isxyrh vevaia
plot(nba$GS,nba$PTS,main='Paixtes pou xekinisan-se sxesi me tous pontous pou vazoun',ylab='Points',xlab='games started')
##profanwsoipaixtes pou xekinanperimenoume na exoun kai perissoterous pontous afou kai paizoun perisotero endexomenos
#alla kai mallon einai oi kalyteroi kata ton proponiti


#####oi lezantes kathoti kai ta sxolia  perieixan greeklish, kathoti ypirxe thema stin kodikopoiisi####