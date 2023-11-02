setwd("C:/Users/Free/Meu Drive (alexandrehack@gmail.com)/11.R CORE TEAM/1_BANCO_DE_SCRIPTS/TRIFATORIAL_ARACA")
dad<- read.table("araca_quebra.txt", header = TRUE)

#analise exploratoria dos dados 
media<-mean(dad$ger);media
ger<-dad$ger #criar objeto 
sd(ger); #variancia 
sqrt(var(ger)) #desvio padrao 
cv<-sqrt(var(ger))/media*100; cv #coeficiente de variacao 
range (ger) #amplitude total 

#ger
library(ExpDes.pt)
fat3.dic(dad$frio, dad$solucao, dad$tempo, dad$ger, quali = c(TRUE, TRUE, TRUE), mcomp = "duncan",
         fac.names = c("frio", "solucao", "tempo"), sigT = 0.05, sigF = 0.05)

hist(ger)
qqnorm(ger)
qqline(ger)
attach(dad)
boxplot(ger ~ frio*solucao*tempo)
plot(frio,ger)

library("MASS")
boxcox(ger~frio*solucao*tempo, data=dad, plotit=T)
boxcox(ger ~ frio*solucao*tempo, data=dad, lam=seq(1, 3, 1/10))

ger_t<- (dad$ger^(2.4) - 1)/2.4
dad["ger_t"]<-ger_t #criou-se a coluna ger_t

fat3.dic(dad$frio, dad$solucao, dad$tempo, dad$ger_t, quali = c(TRUE, TRUE, TRUE), mcomp = "duncan",
         fac.names = c("frio", "solucao", "tempo"), sigT = 0.05, sigF = 0.05)

# testando transformações diversas 
ger_r<- sqrt(dad$ger) ##transformado por raiz de x 
ger_rx<- sqrt(dad$ger)+sqrt(dad$ger+1) ##transformado por raiz de x + raiz de x+1
ger_r1<- sqrt(dad$ger+1) ##transformado por raiz de x+1 
ger_l<- log(dad$ger+1) ##transformado por log x+1
ger_a<- asin(sqrt(dad$ger/100))*(180/pi) ##transformado por arco seno raiz x/100
ger_10<- log10(dad$ger) ##transformado por log 10
ger_arc<- asin(dad$ger) ##transformado por arco seno 
dad["ger_arc"]<-ger_arc
# obs: nao aceitou nenhum tipo de transformacao, mais perto foi lambda 2,4

#IVG
fat3.dic(dad$frio, dad$solucao, dad$tempo, dad$ivg, quali = c(FALSE, TRUE, TRUE), mcomp = "duncan",
         fac.names = c("frio", "solucao", "tempo"), sigT = 0.05, sigF = 0.05) # resíduos nao normais

boxcox(ivg~frio*solucao*tempo, data=dad, plotit=T)
boxcox(ivg ~ frio*solucao*tempo, data=dad, lam=seq(0, 1, 1/10))

##testando tranformacoes de variaveis diversas 
ivg_t <- (dad$ivg^(0.8) - 1)/0.8 # boxcox lambda=0.8
#ivg_r<- sqrt(dad$ivg) ##transformado por raiz de x 
#ivg_rx<- sqrt(dad$ivg)+sqrt(dad$ivg+1) ##transformado por raiz de x + raiz de x+1
#ivg_r1<- sqrt(dad$ivg+1) ##transformado por raiz de x+1 
#ivg_l<- log(dad$ivg+1) ##transformado por log x+1
#ivg_a<- asin(sqrt(dad$ivg/100))*(180/pi) ##transformado por arco seno raiz x/100
#ivg_10<- log10(dad$ivg) ##transformado por log 10
#$ivg_arc<- asin(dad$ivg) ##transformado por arco seno 

dad["ivg_t"]<-ivg_t # utilizou-se esta para o IVG porem ainda não atendeu
fat3.dic(dad$frio, dad$solucao, dad$tempo, dad$ivg_t, quali = c(FALSE, TRUE, TRUE), mcomp = "duncan",
         fac.names = c("frio", "solucao", "tempo"), sigT = 0.05, sigF = 0.05)

##grafico
library(ggplot2)
library(dplyr)

### 1 parte da 1 interacao 
### separar os niveis dos fatores para interacao solucao x frio 
nenhuma<-filter(dad, solucao=="nada")
H2O<- filter(dad, solucao=="H2O")
hipo_5<- filter(dad, solucao=="5")
GA3_100<- filter(dad, solucao=="100")
GA3_300<- filter(dad, solucao=="300")

## 2 parte
dnenhuma<-tapply (nenhuma$ivg, nenhuma$frio, mean) #reconhecer o eixo x e y (do nivel nenhuma)
d1nenhuma<-data.frame(ivg=dnenhuma,frio=c(0,5,10,15,20)) #colocados de forma manual

dH2O<-tapply (H2O$ivg, H2O$frio, mean) #reconhecer o eixo x e y (do nivel nenhuma)
d1H2O<-data.frame(ivg=dH2O,frio=c(0,5,10,15,20)) #colocados de forma manual

dhipo_5<-tapply (hipo_5$ivg, hipo_5$frio, mean) #reconhecer o eixo x e y (do nivel hipo_5)
d1hipo_5<-data.frame(ivg=dhipo_5,frio=c(0,5,10,15,20)) #colocados de forma manual

dGA3_100<-tapply (GA3_100$ivg, GA3_100$frio, mean) #reconhecer o eixo x e y (do nivel GA3 100)
d1GA3_100<-data.frame(ivg=dGA3_100,frio=c(0,5,10,15,20)) #colocados de forma manual

dGA3_300<-tapply (GA3_300$ivg, GA3_300$frio, mean) #reconhecer o eixo x e y (do nivel hGA3 300)
d1GA3_300<-data.frame(ivg=dGA3_300,frio=c(0,5,10,15,20)) #colocados de forma manual

### 3 parte
##do H2O
ggplot(data=dad)+
      geom_point(data= d1H2O, mapping=aes(x=frio, y=ivg),size=2.5)+
      geom_smooth(data= d1H2O, mapping = aes (x=frio, y= ivg),
              method= "lm",
              formula=y~poly(x,2),
              se= F, color= "black")+
  
    labs (x="stratification period (days)", y="germination rate index (IVG)") + #colocar o titulo do eixo x 
  
    geom_text(data=d1H2O,aes(x=max(frio), y=min(ivg), label= "y= 4,3632 x^2 - 0,2545x - 0,0066 R^2: 0,99"),
            hjust=1.2, size=4.5)+ 
    ggtitle("H2O")+
            theme_classic()
            #ggsave(filename= "h2o_en.jpg", device= "jpg", dpi= 300, scale=1)

#do nenhuma
ggplot(data=dad)+
geom_point(data= d1nenhuma, mapping=aes(x=frio, y=ivg),size=2.5)+
  geom_smooth(data= d1nenhuma, mapping = aes (x=frio, y= ivg),
              method= "lm",
              formula=y~poly(x,2),
              se= F, color= "black")+
  
  labs (x="stratification period (days)", y="germination rate index (IVG)") + #colocar o titulo do eixo x 
  
  geom_text(data=d1nenhuma,aes(x=max(frio), y=min(ivg), label= "y= 3,6667 x? - 0,2372 x - 0,0085 r?: 0,81"),
            hjust=1.2, size=4.5)+
   ggtitle("Nenhuma")+
  theme_classic()
#ggsave(filename= "nenhuma_en.jpg", device= "jpg", dpi= 300, scale=1)

#do hipo_5
 ggplot(data=dad)+
  geom_point(data= d1hipo_5, mapping=aes(x=frio, y=ivg),size=2)+
  geom_smooth(data= d1hipo_5, mapping = aes (x=frio, y= ivg),
              method= "lm",
              formula=y~x, #linear
              se= FALSE, color= "black")+
  
  labs (x="Dias de estratifica??o", y="?ndice de velocidade de germina??o(IVG)", y="iVG") + #colocar o titulo do eixo x 
  
  geom_text(data=d1hipo_5,aes(x=max(frio), y=min(ivg), label= "y= 3,896 x - 0,1052 r?: 0,98"),
            hjust=1.2, size=4.5)+ 
   ggtitle("NaClO 5%")+
  theme_classic()
#ggsave(filename= "hipoclo_pt.jpg", device= "jpg", dpi= 300, scale=1)
  
#GA3 100
 ggplot(data=dad)+
  geom_point(data= d1GA3_100, mapping=aes(x=frio, y=ivg),size=2)+
    geom_smooth(data= d1GA3_100, mapping = aes (x=frio, y= ivg),
              method= "lm",
              formula=y~x, #linear
              se= FALSE, color= "black")+
  
  labs (x="Dias de estratifica??o", y="?ndice de velocidade de germina??o (IVG)") + #colocar o titulo do eixo x 
  
  geom_text(data=d1GA3_100,aes(x=max(frio), y=min(ivg), label= "y= 2,86 x + 0,0451 r?: 0,85"),
            hjust=1.2, size=4.5)+ 
   ggtitle("GA3 100 mg L-1")+
  theme_classic()
#ggsave(filename= "ga3_100_pt.jpg", device= "jpg", dpi= 300, scale=1)
  
#GA3 300 # nao significativo 
ggplot(data=dad)+
geom_hline(yintercept = mean(d1GA3_300$ivg))+
  geom_point(data=d1GA3_300, mapping = aes(x=frio, y=ivg), size=2)+
  labs (x="Dias de estratifica??o", y="?ndice de velocidade de germina??o (IVG)") + 
  geom_text(data=d1GA3_100,aes(x=max(frio), y=min(ivg), label= "y= N?o significativo "),
            hjust=1.2, size=4.5)+ 
  ggtitle("GA3 300 mg L-1")+
 theme_classic()
#ggsave(filename= "ga3_300_pt.jpg", device= "jpg", dpi= 300, scale=1)

## 1° parte da 2 interacao 
### separar os niveis dos fatores para interacao tempo x frio 

h6<-filter(dad, tempo=="6")
h12<- filter(dad, tempo=="12")
h24<- filter(dad, tempo=="24")

## 2 parte
dh6<-tapply (h6$ivg, h6$frio, mean) #reconhecer o eixo x e y (do nivel nenhuma)
d1h6<-data.frame(ivg=dh6,frio=c(0,5,10,15,20)) #colocados de forma manual

dh12<-tapply (h12$ivg, h12$frio, mean) #reconhecer o eixo x e y (do nivel nenhuma)
d1h12<-data.frame(ivg=dh12,frio=c(0,5,10,15,20)) #colocados de forma manual

dh24<-tapply (h24$ivg, h24$frio, mean) #reconhecer o eixo x e y (do nivel hipo_5)
d1h24<-data.frame(ivg=dh24,frio=c(0,5,10,15,20)) #colocados de forma manual

#do 6 horas
ggplot(data=dad)+
  geom_point(data= d1h6, mapping=aes(x=frio, y=ivg),size=2)+
  geom_smooth(data= d1h6, mapping = aes (x=frio, y= ivg),
              method= "lm",
              formula=y~poly(x,2),
              se= FALSE, color= "black")+
  
  labs (x="stratification period (days)", y="germination rate index (IVG)", y="iVG") + #colocar o titulo do eixo x 
  
  geom_text(data=d1h6,aes(x=max(frio), y=min(ivg), label= "y= 3,528 x? - 0,0988 x + 0,0027 r?: 0,91"),
            hjust=1.2, size=4.5)+ 
  ggtitle("6 horas")+
  theme_classic()
#ggsave(filename= "6h_en.jpg", device= "jpg", dpi= 300, scale=1)

#de 12 horas
ggplot(data=dad)+
  geom_point(data= d1h12, mapping=aes(x=frio, y=ivg),size=2)+
  geom_smooth(data= d1h12, mapping = aes (x=frio, y= ivg),
              method= "lm",
              formula=y~x, #linear
              se= FALSE, color= "black")+
  
  labs (x="Per?odo de estratifica??o", y="?ndice de velocidade de germina??o (IVG)") + #colocar o titulo do eixo x 
  
  geom_text(data=d1h12,aes(x=max(frio), y=min(ivg), label= "y= 3,322 x + 0,0481 r?: 0,84"),
            hjust=1.2, size=4.5)+ 
  ggtitle("12 horas")+
  theme_classic()
# ggsave(filename= "12h_pt.jpg", device= "jpg", dpi= 300, scale=1)

#do 24 horas
ggplot(data=dad)+
  geom_point(data= d1h24, mapping=aes(x=frio, y=ivg),size=2)+
  geom_smooth(data= d1h24, mapping = aes (x=frio, y= ivg),
              method= "lm",
              formula=y~poly(x,2),
              se= FALSE, color= "black")+
  
  labs (x="Per?odo de estratifica??o", y="?ndice de velocidade de germina??o (IVG)", y="iVG") + #colocar o titulo do eixo x 
  
  geom_text(data=d1h24,aes(x=max(frio), y=min(ivg), label= "y= 3,552 x? - 0,111 x + 0,0031 r?: 0,84"),
            hjust=1.2, size=4.5)+ 
  ggtitle("24 horas")+
  theme_classic()
# ggsave(filename= "24h_pt.jpg", device= "jpg", dpi= 300, scale=1)

#TM
fat3.dic(dad$frio, dad$solucao, dad$tempo, dad$tm, quali = c(F, TRUE, TRUE), mcomp = "duncan",
         fac.names = c("frio", "solucao", "tempo"), sigT = 0.05, sigF = 0.05)

##explorando dados 

media<-mean(dad$tm);media
tm<-dad$tm #criar objeto 
sd(tm); #variancia 
sqrt(var(tm)) #desvio padrao 
cv<-sqrt(var(tm))/media*100; cv #coeficiente de variacao 
range (tm) #amplitude total 
hist(tm)
qqnorm(tm)
qqline(tm)

boxplot(tm ~ frio*solucao*tempo)

## grafico do fator frio 

frio <- filter(dad,frio)

dfrio<-tapply (dad$tm, dad$frio, mean) #reconhecer o eixo x e y (do fator frio)
d1frio<-data.frame(tm=dfrio,frio=c(0,5,10,15,20)) #colocados de forma manual


ggplot(data=dad)+
  geom_point(data= d1frio, mapping=aes(x=frio, y=tm),size=2)+
  geom_smooth(data= d1frio, mapping = aes (x=frio, y= tm),
              method= "lm",
              formula=y~x,
              se= F, color= "black")+
  
  labs (x="Per?odo de estratifica??o", y="Tempo m?dio de germina??o (TMG)", size= 3) + #colocar o titulo do eixo x 
  
  geom_text(data=d1frio,aes(x=max(frio), y=min(tm), label= "y= 20,47 x? +0,0644 r?: 0.51"),
            hjust=1.2, size=4.5)+ 
  theme_classic()
# ggsave(filename= "tmg_frio_pt.jpg", device= "jpg", dpi= 300, scale=1)
