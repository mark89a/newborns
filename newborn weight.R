#1) 
#IMPORTAZIONE DATI  
neonati=read.csv("neonati.csv", sep = ",", stringsAsFactors = T) 
nrow(neonati)
attach(neonati)
summary(neonati)
library(moments)
library(dplyr)
library(ggplot2)
library(utils)
library(tidyverse)
library(car)
library(lmtest)
library(plotly)
library(rgl)
#------------------------------------------------------------------------------------------------------------------

#dopo aver fatto l'analisi dei residui, rileviamo che ci sono outliers rilevanti nella riga 1551, 
#torniamo qui dunque per eliminare questa riga

righe_da_eliminare=c(1551)
neonati <- neonati %>% filter(!(row_number() %in% righe_da_eliminare))




#Ho rilevato due anomali nell'età della madre, dove vengono indicate 0 e 1 anno come
#età della madre, così ho rimosso quelle informazioni

righe_anomale <- which(neonati$Anni.madre <2)
neonati_clean <- subset(neonati, !rownames(neonati) %in% righe_anomale)
neonati <- neonati_clean

attach(neonati_clean)
summary(neonati)
nrow(neonati)


#----------------------------------------------------------------------------------------------------------

#2)
#DESCRIZIONE DATASET
#Il dataset contiene 10 variabili: 
#L'età della madre dei neonati del campione, variabile quantitativa discreta
#Il numero di gravidanze, anch'essa variabile quantitativa discreta
#Madre fumatrice oppure no. Si tratta di una variabile dummy a cui al SI viene attribuito il valore di 1
#e al NO il valore di 0.
fumatrici=sum(neonati$Fumatrici)
n=length(neonati$Fumatrici)
non_fumatrici=n-fumatrici
#Le madri fumatrici risultano essere 104, le non fumatrici sono 2394
#Il periodo di gestazione in settimane, variabile quantitativa discreta
#Il peso del neonato, variabile quantitativa continua
#La lunghezza, variabile quantitativa continua
#Il diametro del cranio, variabile quantitiva continua
#il tipo di parto è una variabile qualitativa su scala nominale, con modalità Naturale o Cesareo
table(Tipo.parto)
percentuale_ces=728/n
percentuale_ces
percentuale_nat=1772/n
#con table di Tipo.parto vediamo che nel campione, 728 donne hanno praticato il parto cesareo (29.14%)
#mentre 1770 hanno fatto il parto naturale (70.93%)
#La variabile ospedale riguarda gli ospedali in cui sono avvenuti i parti
ospedale=table(Ospedale)
mean(ospedale)
#Table di Ospedale ci dice che i parti sono avvenuti in 3 ospedali con una media di 833 nascite ad ospedale
#Il sesso indica chiaramente il sesso dei neonati
sesso=table(Sesso)
sesso
#Nel campione ci sono 1255 femmine e 1243 maschi
#Lo studio è finalizzato a verificare se le variabili registrate possano influenzarsi l'una con l'altra,
#ad esempio se il fatto che la madre sia fumatrice, possa influenzare altre variabili, come il periodo di
#gestazione, il peso, le dimensioni o addirittura il sesso del neonato. Stessa considerazione si può fare
#per gli anni della madre o per il numero di gravidanze, ipotizzando che questi fattori possano influenzare
#le caratteristiche del neonato. Inoltre lo studio vuole creare un modello di regressione che contenga le
#variabili significative ed influenti in grado di fare previsioni sulle caratteristiche del nascituro.

#----------------------------------------------------------------------------------------------------------------------------

#3) ANALISI DESCRITTIVA
#PESO
View(table(Peso))
skewness(Peso)
kurtosis(Peso)
plot(density(Peso))

interruzioni_peso=c(800, 1600, 2400, 3200, 4000, max(Peso) )
etichette_peso=c( "tra 0.8 kg e 1.6 kg", 
                  "tra 1.6 kg e 2.4 kg", "tra 2.4 kg e 3.2 kg",
                  "tra 3.2 kg e 4 kg", " + di 4 kg")

peso_cl=cut(Peso, breaks = interruzioni_peso, labels = etichette_peso)
table(peso_cl)

library(ggplot2)
ggplot(data=neonati_clean)+
  geom_bar(aes(x=peso_cl),
           col="black",
           fill="lightblue")+
  labs(title = "Distribuzione del peso dei neonati",
       x="Classi di peso",
       y="Numero di osservazioni rilevate")
 
#dopo aver diviso la variabile peso in classi, vediamo che la classe modale è quella
#che tra i 3.2 e i 4 kg con 1277 osservazioni, mentre la classe con meno osserevazioni
#è quella dei neonati tra 0.8 e 1.6 kg.
#La media del peso è pari a 3284, la mediana è 3300, così come la moda.
#la distribuzione del peso risulta leggermente asimmetrica negativa (media<mediana=moda) )
#e leptocurtica.           

#LUNGHEZZA
View(table(Lunghezza))
skewness(Lunghezza)
kurtosis(Lunghezza)
plot(density(Lunghezza))

interruzioni_lunghezza=c(300, 350, 400, 450, 500, max(Peso) )
etichette_lunghezza=c( "tra 300 mm e 350 mm", 
                  "tra 350 mm e 400 mm", "tra 400 mm e 450 mm",
                  " tra 450 mm e 500 mm", "+ di 500 mm")

lunghezza_cl=cut(Lunghezza, breaks = interruzioni_lunghezza, labels = etichette_lunghezza)
table(lunghezza_cl)

library(ggplot2)
ggplot(data=neonati)+
  geom_bar(aes(x=lunghezza_cl),
           col="black",
           fill="lightblue")+
  labs(title = "Distribuzione della lunghezza dei neonati",
       x="Classi di lunghezza",
       y="Numero di osservazioni rilevate")  
  
#anche la lunghezza è asimmetrica negativa e leptocurtica. La media è 494.7 mm, la mediana
#è 500 mm e la moda è anche 500 mm.
#la classe con il maggior numero di osservazioni rilevate è infatti quella dei
#neonati con lunghezza compresa tra i 450 e i 500 mm, con 1465 osservazioni, 
#anche se si rilevano numerose osservazioni nella classe con lunghezza maggiore
#di 500 mm

#DIAMETRO CRANIO
View(table(Cranio))
skewness(Cranio)
kurtosis(Cranio)
plot(density(Cranio))

interruzioni_cranio=c(200, 250, 300, 350, 400)
etichette_cranio=c( "tra 200 mm e 250 mm", 
                       "tra 250 mm e 300 mm", "tra 300 mm e 350 mm",
                       " tra 350 mm e 400 mm")

cranio_cl=cut(Cranio, breaks = interruzioni_cranio, labels = etichette_cranio)
table(cranio_cl)


ggplot(data=neonati)+
  geom_bar(aes(x=cranio_cl),
           col="black",
           fill="lightblue")+
  labs(title = "Distribuzione del diametro del cranio dei neonati",
       x="Classi di diametro",
       y="Numero di osservazioni rilevate")  

#per il diametro del cranio la distribuzione è piuttosto simmetrica, media, mediana
#e moda sono uguali, ossia 340 mm.
#La classe modale è quella che comprende i bambini con un cranio tra 300 e 350 mm

#Analisi grafica del peso in relazione agli anni della madre e al tipo di parto


ggplot(data = neonati) +
  geom_point(aes(x = Peso, y = Anni.madre, color = Tipo.parto)) +
  labs(title = "Relazione Anni Madre - Peso - Tipo Parto",
       x = "Peso", y = "Anni madre") +
  scale_color_manual(values = c("Nat" = "blue", "Ces" = "red"),
                     labels = c("Parto Naturale", "Cesareo"))



#A prima vista dallo scatterplot che relaziona gli anni della madre con il peso del neonato
#non sembra esserci una relazione consistente tra gli anni della madre e il peso del
#bambino, a prescindere dall'età della madre, il peso non sembra risentirne
#particolarmente. 
modello_peso_anni.madre <- lm(Peso~Anni.madre, neonati)
risultato_Anova=Anova(modello_peso_anni.madre)
print(risultato_Anova)
#anche il test Anova conferma che non c'è relazione significativa tra peso del neonato e anni della madre
#in quanto il test Anova che relaziona peso e anni della madre restituisce un p-value di 0.234

#Analisi grafica del peso in relazione alle settimane di gestazione e al tipo di parto
ggplot(neonati, aes(x = factor(Gestazione), y = Peso, fill=Tipo.parto)) +
  geom_boxplot()+
  xlab("Settimane di gestazione") +
  ylab("Peso dei neonati in grammi") +
  ggtitle("Relazione tra peso dei neonati, settimane di gestazione")

modello_peso_gestazione <- lm(Peso~Gestazione, neonati)
risultato_Anova_gest=Anova(modello_peso_gestazione)
print(risultato_Anova_gest)
#mettendo in correlazione grafica le settimane di gestaione con il peso vediamo 
#come al crescere delle settimane di gestazione, il peso del neonato cresce, il peso
#mediano è sempre maggiore al crescere delle settimane.Non sembrano esserci sostanziali
#differenze di peso tra parti naturali e cesarei. I parti prematuri avvengono tutti
#naturalmente fino alle 30 settimane
#A conferma dell'analisi grafica che stabilisce la relazione tra settimane di gestazione e anni
#della madre, c'è anche il test Anova che restituisce un p-value molto piccolo (2.2e-16), il che ci 
#induce ad accettare l'ipotesi per cui il peso del neonato vari a seconda delle settimane di gestazione

#-----------------------------------------------------------------------------------------------------------------
#4)
#IPOTESI DI UGUAGLIANZA TRA MEDIA CAMPIONARIA E DELLA POPOLAZIONE 
shapiro.test(Peso)
t.test(Peso, mu=3284)     #p-value=0.986 accettiamo l'ipotesi nulla di uguaglianza tra medie
t.test(Lunghezza, mu=494) #p-value=0.186 accettiamo l'ipotesi nulla di uguaglianza tra medie

#Non avendo la media del peso e della lunghezza della popolazione, usiamo la media
#campionaria


#--------------------------------------------------------------------------------------------------------------------------
#5)
#DIFFERENZE TRA I DUE SESSI
ggplot(neonati, aes(x = factor(Sesso), y = Peso)) +
  geom_boxplot()+
  xlab("Sesso") +
  ylab("Peso dei neonati in grammi") +
  ggtitle("Relazione tra peso dei neonati e sesso")

pesi_maschi <- subset(neonati, Sesso == "M")$Peso
media_pesi_maschi <- mean(pesi_maschi)  #3408.49
sd_pesi_maschi = sd(pesi_maschi)        #493.9
var(pesi_maschi)

pesi_femmine=subset(neonati, Sesso=="F")$Peso
media_pesi_femmine=mean(pesi_femmine)          #3161.06
sd_pesi_femmine=sd(pesi_femmine)               #526.51
var((pesi_femmine))

t.test(Peso ~ Sesso, data = neonati, var.equal = F)   #p-value=2.2e-16

#facendo il t.test per la lunghezza tra maschi e femmine, il p-value risulta essere pari a 2.2e-16,
#un valore molto basso che ci fa rifiutare l'ipotesi nulla di uguaglianza tra medie. Pertanto possiamo
#dire che le medie dei pesi di maschi e femmine sono significativamente diverse

t.test(Lunghezza~Sesso, data = neonati, var.equal=F)     #p-value=2.2e-16
ggplot(neonati, aes(x = factor(Sesso), y = Lunghezza)) +
  geom_boxplot()+
  xlab("Sesso") +
  ylab("Lunghezza dei neonati in mm") +
  ggtitle("Relazione tra Lunghezza dei neonati e sesso")
#anche per la lunghezza dei neonati il p-value del test t risulta essere molto piccolo, quindi le medie
#per della lunghezza per maschi e femmine sono significatimente diverse

t.test(Cranio~Sesso, data = neonati, var.equal=F)       #p-value=1.414e-13
 ggplot(neonati, aes(x = factor(Sesso), y = Cranio)) +
  geom_boxplot()+
  xlab("Sesso") +
  ylab("diametro del cranio in mm") +
  ggtitle("Relazione tra diametro del cranio dei neonati e sesso")

#Anche le medie del diametro dei crani di maschi e femmine risultano essere significativamente diverse

 
#--------------------------------------------------------------------------------------------------------------------

#6) 
#TABELLA E GRAFICO DI FREQUENZE NATI CON PARTO CESAREO E NATURALE PER OSPEDALE
tabella_frequenze_parto <- table(neonati$Tipo.parto, neonati$Ospedale)

View(tabella_frequenze_parto)

distr_freq_tipo.parto=as.data.frame(tabella_frequenze_parto)
colnames(distr_freq_tipo.parto) <- c("Tipo.parto", "Ospedale", "Frequenze")



#tabella pivot con tidyverse per aggregare meglio i dati
tabella_tipo_parto <- distr_freq_tipo.parto %>%
  pivot_wider(names_from = Ospedale, values_from = Frequenze)

rownames(tabella_tipo_parto) <- c("Naturale", "Cesareo")

#grafico a barre affiancate delle freq.assolute di parti cesarei e naturali per ospedale

x11()
ggplot(data=distr_freq_tipo.parto) +
  geom_bar(aes(x = Ospedale, y=Frequenze, fill=Tipo.parto),
           stat = "identity", position = "dodge") +
           labs(x = "Ospedale", y = "Frequenze", fill = "Tipo di Parto") +
  scale_y_continuous(breaks = seq(0, 2500, 100))+
  theme(panel.spacing = unit(0.3, "cm"),
        legend.position = c(0.1, 0.85),
        legend.direction = "vertical") 

#verifica dell'ipotesi di maggior numero di parti cesarei in alcuni ospedali
tabella_cesarei <- table(neonati$Tipo.parto=="Ces", neonati$Ospedale)

 chisq.test(tabella_cesarei)
#Pearson's Chi-squared test
#data:  tabella_cesarei
#X-squared = 1.083, df = 2, p-value = 0.5819
 
# In questo caso, avendo un p-value di 0.5819, che è superiore al consueto livello 
#di significatività di 0.05, non possiamo rifiutare l'ipotesi nulla. 
#Ciò suggerisce che non vi sono differenze significative nella proporzione di parti 
#cesarei tra gli ospedali analizzati, quindi non si può dire che si facciano più parti
 #cesarei in alcuni ospedali

#---------------------------------------------------------------------------------------------------------

#ANALISI MULTIDIMENSIONALE
 
#1)
#Indagine sulle variabili in relazione alla variabile risposta peso 
x11()
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor,...)
{
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)

  txt <- paste0(prefix, format(c(r, 0.123456789), digits = digits)[1])
  
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.8, 0.8, txt, cex = 1.5, cex.labels=1.5)
 }
 
 pairs(neonati, lower.panel = panel.cor, upper.panel = panel.smooth, cex.labels=1.5)
 

#dalla griglia che mostra i coefficienti di correlazione tra tutte le variabili, vediamo che:
 #- Gli anni della madre non hanno praticamente alcuna correlazione con nessuna delle variabili
 #  quantitative presenti.
 
 #- Anche il numero di gravidanze no ha praticamente alcuna correlazione con le altre variabili
 
 #- Le settimane di gestazione hanno una correlazione positiva con lunghezza, peso e diametro 
 #  del cranio del neonato 
 
 #Il peso, oltre che con le settimane di gestazione, ha una correlazione molto positiva con la
 #lunghezze e il diametro del cranio

 #Alla fine dell'analisi quindi possiamo dire che le variabili che presentano tutte un forte coefficiente
 #di correlazione sono settimane di gestazione, peso, lunghezza e diametro del cranio, tutte le altre 
 #variabili quantitative non sembrano essere correlate tra loro
 
 
 #Analizziamo ora le variabili qualitative:

#1 - SESSO
par(mfrow=c(1,2)) 
boxplot(Peso)
boxplot(Peso~Sesso)
 
t.test(Peso~Sesso)   #p-value=2.2e-16
t.test(Lunghezza~Sesso) #p-value=2.2e-16
t.test(Cranio~Sesso)    #p-value=1.414e-13
t.test(Anni.madre~Sesso) #p-value=0.636
t.test(Gestazione~Sesso) #p-value=2.096e-11
t.test(Fumatrici~Sesso)  #p-value=0.51
t.test(N.gravidanze~Sesso)  #p-value=0.21
#la variabile sesso mostra un p-value molto basso in relazione a tutte le altre variabili, tranne che con
#gli anni della madre, con la madre fumatrice o no e con il numero di gravidanze, con cui non sembra
#esserci un legame.
#quindi se raffrontata alle variabili quantitative, le differenze tra maschi e 
#femmine sembrano essere rilevanti, per cui è bene inserire questa variabile nel modello.



#-TIPO DI PARTO
t.test(Peso~Tipo.parto)
t.test(Lunghezza~Tipo.parto)
t.test(Cranio~Tipo.parto) 

#il tipo di parto restituisce un p-value sempre alto con le variabili quantitative, tranne con la Lunghezza
#ma è molto al limite, ma ad ogni modo non sembra essere una variabili così rilevante in relazione
#alle altre.
 
ggplot(neonati, aes(x = factor(Tipo.parto), y = Peso)) +
  geom_boxplot()+
  xlab("Tipo di parto") +
  ylab("peso in g") +
  ggtitle("Relazione tra tipo di parto e peso")

ggplot(neonati, aes(x = factor(Tipo.parto), y = Lunghezza)) +
  geom_boxplot()+
  xlab("Tipo di parto") +
  ylab("Lunghezza in mm") +
  ggtitle("Relazione tra tipo di parto e lunghezza")


#2)
#Creazione del modello con tutte le variabili

mod1=lm(Peso~. , data=neonati)
summary(mod1)
#dal summary del modello che pone come variabile risposta il peso e come variabili esplicative tutte 
#le altre variabili a disposizione, vediamo che le variabili che risultano essere molto significative
#sono le settimane di gestazione, la lunghezza del neonato, il diametro del cranio ed il sesso.
#Anche il numero di gravidanze  e il tipo di parto sembrano avere una significatività, anche se minore.
#I beta delle variabili più significative ci dicono che:

#Per ogni mm in più di lunghezza, il peso aumenta di 10.29 g
#Per ogni mm in più di diametro del cranio, il peso aumenta di 10.47 g
#Per ogni settimana in più di gestazione, il peso cresce di 32.57 g
#SessoM con beta di 77.57 indica che nei maschi si rileva un peso di 77.57 g maggiore rispetto alle
#femmine.
#per ogni gravidanza in più il peso aumenta di 11.38 g
#Il peso dei neonati da parto naturale sembra essere maggiore di 29.6 g rispetto a quelli nati da
#parto cesareo, anche se dall'analisi precedente non risulta esserci una correlazione così forte tra
#tipo di parto e peso.

#L'R quadro aggiustato del modello è pari a 0.7278, una variabilità spiegata del 73%, che non è male
#ma il modello può essere migliorato.



#3)
#Ricerca del modello migliore

#Procediamo passo passo:

mod2=update(mod1, ~.-Ospedale)
summary(mod2)
#nel modello 2 la variabile "fumatrici" continua ad essere poco significativa, ma per il resto non 
#è cambiato molto

anova(mod1, mod2) #il test anova ha un p-value di 0.1036, il che ci dice che l'informazione che 
                  #abbiamo rimosso, ossia ospedale, potrebbe apportare una informazione importante
BIC(mod1, mod2)   #tuttavia il BIC risulta inferiore nel modello 2, il che ci porterebbe a preferirlo
AIC(mod1, mod2)   #l'AIC invece è inferiore nel modello 1
car::vif(mod2)    #tutti i VIF sono al di sotto di 5, quindi non c'è problema di multicollinearità


mod3=update(mod2, ~.-Fumatrici)
summary(mod3)
anova(mod2,mod3)        #il p-value è di 0.25, non c'è 
BIC(mod1, mod2, mod3)   #il BIC più basso è del modello 3
AIC(mod1, mod2, mod3)   #l'AIC continua ad essere più basso nel modello 1 e uguale negli altri due
#l'R quadro rimane invariato 
car::vif(mod3)          #anche qui i vif sono tutti al di sontto di 5

mod4=update(mod3, ~.-Anni.madre)
summary(mod4)
anova(mod3, mod4)   #test anova ha un p-value di 0.44, la variabile anni della madre dunque potrebbe
                    #non aggiungere un'informazione rilevante
BIC(mod1,mod2,mod3,mod4)  #il BIC del modello 4 è il più basso
AIC(mod1,mod2,mod3,mod4)  #anche il AIC del modello 4 si abbassa rispetto al 2 e al 3
vif(mod4)                 #vif tutti sotto 5


mod5=update(mod4,~.-Tipo.parto)
summary(mod5)
anova(mod4, mod5)
BIC(mod1,mod2,mod3,mod4,mod5)
AIC(mod1,mod2,mod3,mod4,mod5)
vif(mod5)

#il modello prescelto è il numero 5
#mod5=Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + Sesso)
#L'output rappresenta i risultati della regressione lineare adattata al Modello 5.
#La colonna "Estimate" fornisce le stime dei coefficienti di regressione per ogni variabile predittiva 
#nel modello. Ad esempio, l'intercetta è stimata a -6681.7251, il coefficiente di "N.gravidanze" è stimato
#a 12.4554, il coefficiente di "Gestazione" è stimato a 32.3827, il coefficiente di "Lunghezza" è stimato
#a 10.2455, il coefficiente di "Cranio" è stimato a 10.5410 e il coefficiente di "SessoM" è stimato a 77.9807.
#La colonna "Std. Error" rappresenta l'errore standard delle stime dei coefficienti, che misura la loro 
#precisione.
#La colonna "t value" indica il valore t calcolato per testare l'ipotesi che il coefficiente sia uguale 
#a zero.
#La colonna "Pr(>|t|)" fornisce il p-value associato al test t, che misura la significatività statistica 
#del coefficiente.
#I coefficienti delle variabili "N.gravidanze", "Gestazione", "Lunghezza", "Cranio" e "SessoM" sono tutti 
#statisticamente significativi, poiché i loro p-value sono inferiori a 0.05.
#Il "Residual standard error" rappresenta l'errore residuo standard, che fornisce una stima della 
#deviazione standard dei residui.
#Il "Multiple R-squared" rappresenta la proporzione di varianza totale della variabile di risposta 
#spiegata dal modello. In questo caso è 0.727, il che indica che il modello spiega circa il 72.7% della 
#variazione nella variabile di risposta.
#L' "Adjusted R-squared" tiene conto del numero di predittori nel modello e fornisce una misura di 
#adattamento del modello considerando anche la complessità del modello.
#Infine, il "F-statistic" è il valore F calcolato per testare l'ipotesi nulla che tutti i coefficienti 
#di regressione siano nulli. Il p-value associato al F-statistic è molto piccolo (p-value < 2.2e-16),
#il che indica che il modello nel suo complesso è statisticamente significativo.
#In sintesi, i risultati indicano che il Modello 5 è statisticamente significativo e spiega in modo 
#significativo la variazione nella variabile di risposta "Peso" in base alle variabili predittive 
#incluse nel modello.


#vediamo ora di trovare il modello migliore col pacchetto MASS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#utilizziamo il pacchetto MASS per ricercare il modello col metodo stepwise
stepwise.mod=MASS::stepAIC(mod1,
                           direction = "both",
                           k=2)

summary(stepwise.mod)
anova( mod5, stepwise.mod)
BIC(mod1,mod2,mod3,mod4,mod5,stepwise.mod)  #il modello 5 ha il BIC più basso
AIC(mod1,mod2,mod3,mod4,mod5, stepwise.mod)  #il modello stepwise.mod ha l'AIC più basso

par(mfrow=c(2,2))

plot(stepwise.mod)

lev_stepwise=hatvalues(stepwise.mod)
plot(lev_stepwise)
p_stepwise=sum(lev_stepwise)
n=nrow(neonati)
soglia_stepwise=2*p_stepwise/n
abline(h=soglia_stepwise, col="red")

lev[lev_stepwise>soglia_stepwise]

plot(rstudent(stepwise.mod))
abline(h=c(-2,2), col="red")
outlierTest(stepwise.mod)
vif(stepwise.mod)
#IL METODO stepwise di MASS ci restituisce questo modello:
#PESO=N.GRAVIDANZE+GESTAZIONE+LUNGHEZZA+CRANIO+TIPO PARTO+OSPEDALE+SESSO
#il BIC più basso continua ad essere quello del modello 5, mentre il modello stepwise.mod ha l'AIC 
#migliore.
#i vif sono anche qui tutti sotto la soglia di 5.
#nel test anova il p-value il p-value è molto basso (0.001405), il che suggerisce che 
#l'aggiunta delle variabili predittive nel Modello 2 è statisticamente significativa e migliora 
#significativamente la capacità del modello di spiegare la variazione nella variabile di risposta.

#IL MODELLO MIGLIORE PER IL TEST ANOVA SEMBRA ESSERE QUELLO INDIVIDUATO DAL PACCHETTO MASS, MA
#ESSO AGGIUNGE DUE VARIABILI COME IL TIPO DI PARTO E L'OSPEDALE, CHE AVEVAMO VISTO NON AVERE UN LEGAME
#COSI' FORTE CON IL PESO, INOLTRE NON POSSEDIAMO QUESTE INFORMAZIONI AL FINE DELLA NOSTRA PREVISIONE ED
#ESSENDO DELLE VARIABILI NON QUANTITATIVE, NON LE AGGIUNGEREI NEL MODELLO, TORNANDO A PREFERIRE IL MODELLO 5
#--------------------------------------------------------------------------------------------------------


#4)
#Presenza di interazioni ed effetti non lineari

interazioni=lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + Sesso + Gestazione*Lunghezza, data = neonati)
summary(interazioni)
#Aggiungendo l'interazione tra Gestazione e Lunghezza, quest'ultima perde di 
#significatività, quindi questa interazione non va aggiunta al modello.

interazione2=lm(Peso~N.gravidanze+Gestazione+Lunghezza+Cranio+Sesso+Cranio*Lunghezza, data = neonati)
summary(interazione2)
#Anche aggiungendo l'interazione tra Lunghezza e Cranio la Lunghezza perde di 
#significatività

#Si possono fare anche analisi grafiche tra la variabile risposta Peso e le variabili
#indipendenti del modello
par(mfrow=c(2,3))
plot(neonati$Gestazione, neonati$Peso, xlab = "Gestazione", ylab = "Peso", main = "Grafico di dispersione Gestazione-Peso")
plot(neonati$Lunghezza, neonati$Peso, xlab = "Lunghezza", ylab = "Peso", main = "Grafico di dispersione Lunghezza-Peso")
plot(neonati$N.gravidanze, neonati$Peso, xlab = "N.gravidanze", ylab = "Peso", main = "Grafico di dispersione N.gravidanze-Peso")
plot(neonati$Sesso, neonati$Peso, xlab = "Sesso", ylab = "Peso", main = "Grafico di dispersione Sesso-Peso")
plot(neonati$Cranio, neonati$Peso, xlab = "Cranio", ylab = "Peso", main = "Grafico di dispersione Cranio-Peso")

#Dai grafici che relazionano le variabili indipendenti con la variabile risposta, sembrano 
#esserci dei pattern regolari, quindi non sembrano esserci effetti non lineari.

modello_interazioni<- lm(Peso ~ N.gravidanze + poly(Gestazione, 2) + poly(Lunghezza, 3) + Cranio + Sesso + Gestazione:Lunghezza, data = neonati)
summary(modello_interazioni)






#-------------------------------------------------------------------------------------------------------------------
#5)
#Analisi dei residui

par(mfrow=c(2,2))

plot(mod5)
#Dal grafico del modello vediamo che nel primo grafico i punti si disperdono causualmente attorno
#alla media di zero. Nel secondo grafico i residui seguono la bisettrice quindi sono distribuiti
#normalmente. Il terzo grafico non segue un pattern preciso, quindi tutti i requisiti per i residui
#sembrano essere rispettati.
#Nell'ultimo grafico in cui vengono mostrati i potenziali valori influenti, solo un'osservazione si
#trova nell'area compresa tra 0.5 e 1 della distanza di Cook

#valori leverage
lev=hatvalues(mod5)
plot(lev)
p=sum(lev)
n=nrow(neonati)
soglia=2*p/n
abline(h=soglia, col="red")

lev[lev>soglia]

#valori outliers
plot(rstudent(mod5))
abline(h=c(-2,2), col="red")
outlierTest(mod5)
#ci sono tre valori outliers, e sono le registrazioni numero 1551, 155 e 1306

#distanza di cook
cook=cooks.distance(mod4)
plot(cook)
max(cook)
#l'unico valore anomalo è riportato dall'osservazione numero 1551, che se allontana dalla distanza
#di Cook, quindi torniamo all'inizio e rimuoviamo questa riga dai dati

bptest(mod5)      #p-value di 2.2e-16, quindi c'è omoschedasticità (0.045 dopo la rimozione outliers)
dwtest(mod5)      #il p-value è 0.1209, non c'è un problema di autocorrelazione
shapiro.test(residuals(mod5))  #il p-value è 2.2e-16, quindi non c'è normalità dei residui
plot(density(residuals(mod5)))
qqnorm(residuals(mod5))
qqline(residuals(mod5))
plot(density(residuals(mod5)))
mean(residuals(mod5))
#dai grafici dei residui, essi sembrano distribuirsi normalmente, sia dalla densità dei residui, che hanno
#una media praticamente di zero (1.56415e-14). Anche dal Q-Q plot essi sembrano seguire la bisettrice
#e dunque una distribuzione normale.
#Dopo aver rimosso la riga 1551 che conteneva outliers, il test di omoschedasticità rifiuta sempre l'ipotesi
#nulla, ma lo fa con un valore del p-value poco al di sotto della soglia del 5%. L'autocorrelazione è assente
#e nonostante il test Shapiro rifiuto l'ipotesi di normalità, l'analisi grafica sembra suggerire la normalità
#della distribuzione dei residui, con media vicinissima allo zero


#--------------------------------------------------------------------------------------------------------------------------

#6)
#Bontà del modello
summary(mod5)

#L'R quadro aggiustato del modello prescelto è pari a 0.736, dunque una variabilità spiegata del 73.6%.
#Dopo aver rimosso la riga presentante valori outliers, l'analisi dei residui ha un bptest con p-value
#pari a 0.045, quindi molto vicino alla soglia del 5%, e più accettabile rispetto al dataframe 
#contenente la riga con outliers che supera la distanza di Cook. Non c'è autocorrelazione e nonostante lo
#Shapiro test rifiuti l'ipotesi di normalità dei residui, le rappresentazioni grafiche sembrano tutte
#dimostrare normalità dei residui.
#l'R-quadrato aggiustato rappresenta la proporzione di varianza nel peso dei neonati spiegata dal modello.
#In questo caso, l'R-quadrato è 0.7372, il che indica che il modello spiega circa il 73.72% della variazione 
#nel peso dei neonati.
#Il test di significatività mostra che tutte le variabili nel modello sono significative in quanto hanno 
#valori p inferiori al livello di significatività (0.05). 
#Questo suggerisce che tutte le variabili considerate sono statisticamente significative per spiegare 
#il peso dei neonati.
#Il p-value del modello è molto piccolo, quindi nel complesso il modello è significativo
#--------------------------------------------------------------------------------------------------------------------------

#7)
#Previsione del peso

#Prevediamo il peso di una neonata con la madre alla terza gravidanza alla settimana 39
#Non avendo a disposizione i dati delle ecografie riguardanti la lunghezza e il diametro del cranio
#che sono variabili presenti nel modello di regressione, essendo essi valori mancanti a causa della mancata
#registrazione da parte dell'ecografia di queste misure, calcoliamo una media campionaria di esse per le 
#femmine alla settimana 39 di gestazione con la madre alla terza gravidanza, così da inserire questi valori
#nel modello
lunghezza_femmine_sett_39 <- subset(neonati, Gestazione == 39 & Sesso == "F" & N.gravidanze==3)$Lunghezza
media_lunghezza_femmine_sett_39=mean(lunghezza_femmine_sett_39)

cranio_femmine_sett_39<- subset(neonati, Gestazione ==39 & Sesso =="F" & N.gravidanze==3)$Cranio
media_cranio_femmine_sett_39=mean(cranio_femmine_sett_39)


previsione=predict(mod5, newdata = data.frame(N.gravidanze=3, 
                   Lunghezza=media_lunghezza_femmine_sett_39, 
                   Cranio=media_cranio_femmine_sett_39, 
                   Gestazione=39, Sesso="F"))

previsione    #3225.44

#il modello di regressione che abbiamo scelto, tolti i valori outliers, ci indica che la neonata 
#avente queste caratteristiche peserà 3225.44 grammi
#----------------------------------------------------------------------------------------------------------------

#8)
#Grafici


library(scatterplot3d)
scatterplot3d(x=Cranio, y=Gestazione, z=Peso, 
              color =  ifelse(Sesso == "M", "blue", "red"), size = 2,
              type = "h", main = "Grafico 3D del peso del bambino", 
              xlab = "Cranio", ylab = "Gestazione", 
              zlab = "Peso")
legend("bottomright", legend = c("Maschio", "Femmina"), 
fill = c("blue", "red"), bty = "n")


