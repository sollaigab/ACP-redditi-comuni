#PROGETTO DATA MINING
#OBIETTIVO: individuare le componenti principali e interpretarle
############
############

### Nella nostra analisi si utilizzerà i seguenti pacchetti
# Se necessario installare i seguenti pacchetti
# install.packages("dplyr")
# install.packages("gridExtra")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("GGally")
# install.packages("ggrepel")
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(GGally)
library(ggrepel)

### 1)PRIMA FASE:caricare dati in R e preparare matrice dati con le variabili di interesse
redditi <- read.csv("https://raw.githubusercontent.com/sollaigab/ACP-redditi-comuni/main/Redditi_comuni.csv", header=TRUE, sep=";", quote="", na.strings = "")
summary(redditi)

### Con read.table carico in R il dataset in formato csv
### Con header=TRUE indico che la prima riga del dataset si riferisce all'intestazione e non va quindi considerata come dato
### Sep indica il separatore dei caratteri che risulta essere il ;
### Quote fornisce indicazioni sui caratteri speciali che si devono considerare
### Na.strings indica il vettore di caratteri o stringhe che deve essere interpretato come missing value
### Non indicando nessun vettore evito ad esempio che la sigla della provincia di Napoli (ovvero NA) sia interpretata come un missing value
### Si può infatti verificare che non sussistono missing value in corrispondenza della variabile Provincia
sum(is.na(redditi$Sigla.Provincia))
redditi$Sigla.Provincia <- as.factor(redditi$Sigla.Provincia)

### Il file contiene dati sui redditi comunali italiani
### Il sito di riferimento è:
### http://www1.finanze.gov.it/finanze3/analisi_stat/index.php?search_class[0]=cCOMUNE&opendata=yes
### Il contenuto informativo del dataset può essere riassunto come segue:
### 1)variabili descrittive= nome del comune, della provincia di appartenenza,ecc
### 2)variabile demografica=numero di contribuenti
### 3)variabili economiche=suddivise in 
### 3.1)elementi del reddito complessivo=somma dei redditi lordi (es. fabbricati, lavoro dipendente, pensione,
### lavoro autonomo etc..)
### 3.2)reddito imponibile=(reddito complessivo - deduzioni)
### 3.3)Imposta Irpef netta=Imposta Irpef lorda - detrazioni
### Considerando che l'imposta Irpef lorda è reddito imponibile*aliquote Irpef applicate per scaglioni
### 3.4)Gli 8 scaglioni
### Inoltre per ogni variabile economica si riporta l'ammontare in euro e la frequenza
### Quando riportano la denominazione "Ammontare in euro", si riferisce al reddito in valore assoluto 
### Quando riportano la denominazione "Frequenza" si riferisce al numero contribuenti che dichiarano quel tipo di reddito
### Il dataset contiene 51 variabili e 7979 unità
dim(redditi)

### Creiamo la variabile Macroregioni con cui collochiamo ogni comune ad una Macroregione geografica
### Eliminiamo inoltre la riga 7979 che non contiene informazioni
redditi <- redditi[-7979,-51]
names(table(redditi$Regione))
regioni.nord <- c("Liguria","Lombardia","Piemonte", "Valle d'Aosta","Emilia Romagna", "Friuli Venezia Giulia", "Trentino Alto Adige", "Veneto")
regioni.centro <- c("Lazio","Marche","Toscana","Umbria")
regioni.mezzogiorno <- c("Abruzzo","Basilicata","Calabria","Campania","Molise","Puglia","Sardegna","Sicilia")
length(regioni.nord)+length(regioni.centro)+length(regioni.mezzogiorno)

# viene aggiunta ora una nuova variabile al dataframe redditi
Macroregione <- vector(length = 7978)
redditi$Macroregione <- Macroregione
redditi$Macroregione <- ifelse(redditi$Regione%in%regioni.mezzogiorno, "Sud", redditi$Macroregione)
redditi$Macroregione <- ifelse(redditi$Regione%in%regioni.centro, "Centro", redditi$Macroregione)
redditi$Macroregione <- ifelse(redditi$Regione%in%regioni.nord, "Nord", redditi$Macroregione)
redditi$Macroregione <- as.factor(redditi$Macroregione)
redditi$Macroregione
head(redditi)

### Rinominiamo le Regioni in modo da renderle più leggibili nei grafici
redditi$Regione<-as.character(redditi$Regione)
i <- which(redditi$Regione == "Lombardia")
redditi$Regione[i] <- "LOM"
i <- which(redditi$Regione == "Veneto")
redditi$Regione[i] <- "VEN"
i <- which(redditi$Regione == "Friuli Venezia Giulia")
redditi$Regione[i] <- "FVG"
i <- which(redditi$Regione == "Emilia Romagna")
redditi$Regione[i] <- "EMR"
i <- which(redditi$Regione == "Toscana")
redditi$Regione[i] <- "TOS"
i <- which(redditi$Regione == "Campania")
redditi$Regione[i] <- "CAM"
i <- which(redditi$Regione == "Calabria")
redditi$Regione[i] <- "CAL"
i <- which(redditi$Regione == "Sardegna")
redditi$Regione[i] <- "SAR"
i <- which(redditi$Regione == "Lazio")
redditi$Regione[i] <- "LAZ"
i <- which(redditi$Regione == "Marche")
redditi$Regione[i] <- "MAR"
i <- which(redditi$Regione == "Umbria")
redditi$Regione[i] <- "UMB"
i <- which(redditi$Regione == "Piemonte")
redditi$Regione[i] <- "PIE"
i <- which(redditi$Regione == "Abruzzo")
redditi$Regione[i] <- "ABR"
i <- which(redditi$Regione == "Basilicata")
redditi$Regione[i] <- "BAS"
i <- which(redditi$Regione == "Liguria")
redditi$Regione[i] <- "LIG"
i <- which(redditi$Regione == "Trentino Alto Adige")
redditi$Regione[i] <- "TAD"
i <- which(redditi$Regione == "Valle d'Aosta")
redditi$Regione[i] <- "VDA"
i <- which(redditi$Regione == "Puglia")
redditi$Regione[i] <- "PUG"
i <- which(redditi$Regione == "Molise")
redditi$Regione[i] <- "MOL"
i <- which(redditi$Regione == "Sicilia")
redditi$Regione[i] <- "SIC"
redditi$Regione <- as.factor(redditi$Regione)
levels(redditi$Regione)

#si crea una funzione che riceve in input il dataset e restituisce come output la somma dei missing value (NA) per ogni variabile
mv <- function(data){
  vett <- vector("numeric", length = 51)
  for(i in 1:length(data)){
    vett[i] <- sum(is.na(data[,i]))
  }
  return(vett)
}
redd <- mv(redditi)
redd
i <- which(redd > 800)
head(redditi[,i])
i

### Si eliminano poi tutte le variabili che hanno più di 800 missing value
### Si ha così un dataset di 39 variabili e 7978 unità
redditi <- redditi[,-i]
summary(redditi)

### Per rendere più agevoli le procedure per la manipolazione del dataset si crea l'oggetto redditi.t che contiene il dataset redditi ma in formato tibble
### Con questo formato, richiamando redditi.t, si potranno visionare le prime dieci righe del dataframe e le prime sette colonne
redditi.t <- as_tibble(redditi)
redditi.t

### Si creano, col comando select(), due dataframe che hanno come variabili solo quelle che hanno come denominazione, rispettivamente, Ammontare in euro e Frequenza
### Effettuando la divisione di questi due dataframe si ottiene tabella.redditi.medie che riporta i valori medi per ogni singola variabile economica
tabella.redditi.ammontare <- redditi.t %>% select(ends_with("Ammontare.in.euro"))
tabella.redditi.frequenza <- redditi.t %>% select(ends_with("Frequenza"))
tabella.redditi.medie <- tabella.redditi.ammontare/tabella.redditi.frequenza
colnames(redditi.t)

### In seguito, si crea redditi.medie aggiungendo, col comando mutate(), le variabili non economiche, raggruppate in tabella.nominativi
tabella.nominativi <- redditi.t %>% select(contains("Anno.di.imposta")|contains("Codice.catastale")|contains("Codice.Istat.Comune")|contains("Denominazione.Comune")|contains("Sigla.Provincia")|contains("Regione")|contains("Codice.Istat.Regione")|contains("Numero.contribuenti") | contains("Macroregione"))
redditi.medie <- tabella.nominativi %>% mutate(tabella.redditi.medie)
summary(redditi.medie)
dim(redditi.medie)

### Dopo le modifiche effettuate, il dataframe di riferimento ha 24 variabili per 7978 unità
### Si conduce ora un'analisi interpretativa sulle variabili quantitative che si considerano superflue ai fini dell'analisi 
dimnames(redditi.medie)

### Si eliminano i dati contabili come codice catastale e codice Istat Comune
### Si eliminano gli addizionali, gli scaglioni e i bonus
### Gli addizionali e bonus si eliminano perchè si riferiscono a notazioni tecniche dal punto di vista contabile risultando, quindi, di più difficile interpretazione
### Gli scaglioni invece si eliminano perché, ponendo il rapporto ammontare/frequenza, forniscono informazioni sulla distribuzione della ricchezza limitatamente alla singola classe
redditi.medie.acp <- redditi.medie[,-c(1,2,3,7,17,19,20,21,22,23,24)]
dimnames(redditi.medie.acp)

### Si assegnano nomi più interpretativi e di facile utilizzo alle variabili del nuovo dataframe
colnames(redditi.medie.acp)[c(6:13)] <- c("Reddito.fabbricati", "Reddito.dipendente", "Reddito.pensione", "Reddito.imprenditore",
                                          "Reddito.partecipazione", "Reddito.imponibile", "Imposta.netta","Reddito.addizionale")
names(redditi.medie.acp)

### Dopo le modifiche effettuate, il dataframe di riferimento ha 13 variabili per 7978 unità
dim(redditi.medie.acp)
summary(redditi.medie.acp) 


### 2)SECONDA FASE: analisi esplorativa dei dati. Si è svolta direttamente su shiny.

##############################################
### 3)TERZA FASE:utilizzando le funzioni del package dplyr, si definisce la sottomatrice di dati da analizzare e si presenta una breve analisi esplorativa del dataframe
###si crea un campione casuale di 500 osservazioni
nc <- 500
set.seed(619)
n <- sample_n(redditi.medie.acp, size = nc)
str(n)
summary(n)

###nel caso dal campione risultavano missing value, era necessario apportare delle misure per evitare problematiche nella creazione dei grafici
## per la fase 7 si offre la possibilità di effettuare un campionamento casuale infinite volte
#per questo occorre che non ci siano missing value e si dispone in questo modo
dim(redditi.medie.acp)

mv.comuni <- function(data){
  vett <- vector("numeric", length = 7978)
  for(r in 1:7978){
    vett[r] <- sum(is.na(data[r,]))
  }
  return(vett)
}
redd.comuni <- mv.comuni(redditi.medie.acp)
redd.comuni
r <- which(redd.comuni != 0)

redditi.sen.mi <- redditi.medie.acp[-r,]
summary(redditi.sen.mi$Denominazione.Comune)
# un altro fattore che incide su shiny è che nome di alcuni comuni sono uguali, quindi vanno cancellati i doppioni
redditi.senzamissing <- redditi.sen.mi[-c(1063,1762,1763,3441,5765,6190,7176),]
dim(redditi.senzamissing)



#In questo modo siamo passati da 7978 a 7686 comuni

###4) QUARTA FASE: effettuare l'analisi in componenti principali e selezione numero CP
n <- n[complete.cases(n),]
str(n)
campione.media.misura <- n[,-c(1, 2, 3, 4,5)]
rownames(campione.media.misura) <- n$Denominazione.Comune
head(campione.media.misura)
dim(campione.media.misura)
sum(is.na(campione.media.misura))
colnames(campione.media.misura) <- c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz")
ggpairs(campione.media.misura, axisLabels = "none")
correlazione.campione.media <- cor(campione.media.misura)
correlazione.campione.media
pca <- princomp(campione.media.misura, scores = TRUE, cor = TRUE)
summary(pca)

###calcolo autovalori e varianza spiegata, scindere il summary(pca) in vari data.frame per shiny e altri calcoli
autovalori <- pca$sdev^2
var.spieg <- autovalori/sum(autovalori)
data.autovalori<-data.frame(autovalori)
var.cum<-cumsum(var.spieg)
data.varianza<-data.frame(var.spieg,var.cum)

### 4.2) Si discute sulla scelta del numero ottimale di componenti da considerare.
###il 1° criterio prevede di considerare un numero di CP tale da tener conto d'una percentuale q sufficientemente elevata della varianza totale 
data.varianza

###il 2° criterio prevede la costruzione di un grafico chiamato scree plot(grafico della falda)
#esso riporta il valore degli autovalori sull'asse delle ordinate e il numero d'ordine della CP sull'asse delle ascisse
plot(pca, type = "l")


###il 3° criterio, definito il criterio di Kaiser prevede invece che si calcola la media delle varianza, ovvero l'autovalore medio
#Poi si estraggono le prime K componenti la cui varianza supera tale media
autovalore.medio<-(sum(autovalori))/8


### 4.3) loadings e quota varianza di ogni singola variabile
load <- pca$loadings
load<-print(load, cutoff=0)
data.loading<-matrix(load[1:64],nrow=8, ncol= 8,dimnames=list(c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz"),c("C1","C2","C3","C4","C5","C6","C7","C8")))


###si crea la funzione var.func
var.func <- function(loadings,comp.sdev){
  loadings*comp.sdev
}
sdev.pca <- pca$sdev
var.coord <- t(apply(load, 1, var.func, sdev.pca))
quote.varianza <- var.coord^2
sum(quote.varianza[1,])

###5) QUINTA FASE: presentare e commentare i risultati ottenuti dall'analisi in componenti principali, mediante grafici e tabelle.
#le assunzioni proposte vengono a questo punto esposte attraverso grafici
#Grafico degli individui con clustering
redditi.pca <- cbind(campione.media.misura, pca$scores[,1:3])
colnames(redditi.pca)
head(redditi.pca)
ggplot(redditi.pca, aes(Comp.1, Comp.2, fill = n$Macroregione)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(legend.title = element_blank())

#Grafico delle correlazioni variabili
cerchio.cor.redditi <- cor(campione.media.misura,pca$scores)
cerchio.cor.redditi<- data.frame(cerchio.cor.redditi)
rownames(cerchio.cor.redditi) <- c("RFA","RDI","RP","RIM","RAS","RTAX","TAX","RAD") #se si mette la legenda si può eliminare questa riga
ggplot(cerchio.cor.redditi, aes(Comp.1, Comp.2)) +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[1,1], yend = cerchio.cor.redditi[1,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="magenta") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[2,1], yend = cerchio.cor.redditi[2,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="chartreuse4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[3,1], yend = cerchio.cor.redditi[3,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="firebrick2") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[4,1], yend = cerchio.cor.redditi[4,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="goldenrod") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[5,1], yend = cerchio.cor.redditi[5,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="springgreen4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[6,1], yend = cerchio.cor.redditi[6,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="slateblue1") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[7,1], yend = cerchio.cor.redditi[7,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="coral") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[8,1], yend = cerchio.cor.redditi[8,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="dodgerblue2") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) 



###biplot 
redditi.pca
ggplot(redditi.pca, aes(Comp.1, Comp.2, fill = n$Macroregione)) +
  geom_point(shape = 21) +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[1,1], yend = cerchio.cor.redditi[1,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="magenta") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[2,1], yend = cerchio.cor.redditi[2,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="chartreuse4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[3,1], yend = cerchio.cor.redditi[3,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="firebrick2") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[4,1], yend = cerchio.cor.redditi[4,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="goldenrod") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[5,1], yend = cerchio.cor.redditi[5,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="springgreen4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[6,1], yend = cerchio.cor.redditi[6,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="slateblue1") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[7,1], yend = cerchio.cor.redditi[7,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="coral") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi[8,1], yend = cerchio.cor.redditi[8,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="dodgerblue2") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-1,1)) +
  scale_y_continuous(limits = c(-1,1)) +
  theme(legend.title = element_blank())

### Calcolo indici bontà
pca.scores <- pca$scores

coseno.quadrato <- apply(pca.scores^2, 1, FUN=function(x){x/sum(x)})

coseno.quadrato.t <- t(coseno.quadrato)

apply(coseno.quadrato.t, 1, FUN=sum)

indici.coseno<-round(coseno.quadrato.t*100, digits=2)

colore <- ifelse(coseno.quadrato.t[,1]>0.1, "red", "black")		
colore <- ifelse(coseno.quadrato.t[,2]>=0.1, "green", colore )	
colore <- ifelse((coseno.quadrato.t[,1]<0.1 & coseno.quadrato.t[,2]<0.1), "orange", colore)	

plot(pca$scores[,1:2], type="n")
points(pca$scores[,1],pca$scores[,2], col = colore, cex=.6)
abline(h=0,v=0)



# Calcolo dei contributi assoluti alla  variabilità degli assi
contributi.assoluti <- apply(pca.scores^2, 2, FUN=function(x){x/sum(x)})
# per verificare l'esattezza dei risultati, ricordiamo che la somma per colonne della matrice dei contributi assoluti dà sempre 1
apply(contributi.assoluti, 2, FUN=sum)

indici.contributi<-round(contributi.assoluti*100, digits = 2)

##FASE 7: ACP per differenti campionioni (si è svolta su shiny)

#FASE 8: ACP per Regioni

#si crea un apposito dataset per le Regioni
redditi.regioni.t <- redditi.t %>%
  group_by(Regione) %>%
  summarise_at(vars(c(7:37)), sum, na.rm = TRUE)
redditi.regioni.t <- redditi.regioni.t[, -2]

regioni.nord.sigle <- c("LIG","LOM","PIE", "VDA","EMR", "FVG", "TAD", "VEN")
regioni.centro.sigle <- c("LAZ","MAR","TOS","UMB")
regioni.mezzogiorno.sigle <- c("ABR","BAS","CAL","CAM","MOL","PUG","SAR","SIC")

tabella.redditi.ammontare.regioni <- redditi.regioni.t %>% select(ends_with("Ammontare.in.euro"))
tabella.redditi.frequenza.regioni <- redditi.regioni.t %>% select(ends_with("Frequenza"))
tabella.redditi.medie.regioni <- tabella.redditi.ammontare.regioni/tabella.redditi.frequenza.regioni
colnames(redditi.regioni.t)


Macroregione1 <- vector(length = 20)
redditi.regioni.t$Macroregione1 <- Macroregione1
redditi.regioni.t$Macroregione1 <- ifelse(redditi.regioni.t$Regione%in%regioni.mezzogiorno.sigle, "Sud", redditi.regioni.t$Macroregione1)
redditi.regioni.t$Macroregione1 <- ifelse(redditi.regioni.t$Regione%in%regioni.centro.sigle, "Centro", redditi.regioni.t$Macroregione1)
redditi.regioni.t$Macroregione1 <- ifelse(redditi.regioni.t$Regione%in%regioni.nord.sigle, "Nord", redditi.regioni.t$Macroregione1)
redditi.regioni.t$Macroregione1 <- as.factor(redditi.regioni.t$Macroregione1)
redditi.regioni.t$Macroregione1
levels(redditi.regioni.t)
head(redditi.regioni.t)

tabella.nominativi.regioni <- redditi.regioni.t %>% select(contains("Regione") | contains("Macroregione"))
redditi.medie.regioni <- tabella.nominativi.regioni %>% mutate(tabella.redditi.medie.regioni)
summary(redditi.medie.regioni)
dim(redditi.medie.regioni)

dimnames(redditi.medie.regioni)

redditi.medie.acp.regioni <- redditi.medie.regioni[, -c(10,12:17)]
dimnames(redditi.medie.acp.regioni)

colnames(redditi.medie.acp.regioni)[c(3:10)] <- c("Reddito.fabbricati", "Reddito.dipendente", "Reddito.pensione", "Reddito.imprenditore",
                                                  "Reddito.partecipazione", "Reddito.imponibile", "Imposta.netta","Reddito.addizionale")
names(redditi.medie.acp.regioni)

dim(redditi.medie.acp.regioni)
summary(redditi.medie.acp.regioni) 

redditi.medie.acp.regioni$Regione <- as.factor(redditi.medie.acp.regioni$Regione)
redditi.medie.acp.regioni$Macroregione1 <- as.factor(redditi.medie.acp.regioni$Macroregione1)

summary(redditi.medie.acp.regioni)

campione.media.misura.regioni <- redditi.medie.acp.regioni[,-c(1, 2)]
rownames(campione.media.misura.regioni) <- redditi.medie.acp.regioni$Regione
head(campione.media.misura.regioni)
dim(campione.media.misura.regioni)
sum(is.na(campione.media.misura.regioni))

#si effetua l'ACP sul dataset appositamente creato
pca.regioni <- princomp(campione.media.misura.regioni, scores = TRUE, cor = TRUE)
summary(pca.regioni)

autovalori.regioni <- pca.regioni$sdev^2
var.spieg.regioni <- autovalori.regioni/sum(autovalori.regioni)
data.autovalori.regioni<-data.frame(autovalori.regioni)
var.cum.regioni<-cumsum(var.spieg.regioni)
data.varianza.regioni<-data.frame(var.spieg.regioni,var.cum.regioni)

data.varianza.regioni

plot(pca.regioni, type = "l")

autovalore.medio.regioni<-(sum(autovalori.regioni))/8
autovalore.medio.regioni

load.regioni <- pca.regioni$loadings
load.regioni<-print(load.regioni, cutoff=0)
data.loading.regioni<-matrix(load.regioni[1:72],nrow=8, ncol= 8, 
                             dimnames = list(c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz"),
                                             c("C1","C2","C3","C4","C5","C6","C7","C8")))

var.func <- function(loadings,comp.sdev){
  loadings*comp.sdev
}
sdev.pca.regioni <- pca.regioni$sdev
var.coord.regioni <- t(apply(load.regioni, 1, var.func, sdev.pca.regioni))
quote.varianza.regioni <- var.coord.regioni^2
sum(quote.varianza.regioni[1,])

redditi.pca.regioni <- cbind(campione.media.misura.regioni, pca.regioni$scores[,1:3])
colnames(redditi.pca.regioni)
head(redditi.pca.regioni)

#si ottiene il biplot
ggplot(redditi.pca.regioni, aes(Comp.1, Comp.2, fill = redditi.medie.acp.regioni$Macroregione1)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(legend.title = element_blank())


cerchio.cor.redditi.regioni <- cor(campione.media.misura.regioni,pca.regioni$scores)
cerchio.cor.redditi.regioni<- data.frame(cerchio.cor.redditi.regioni)
rownames(cerchio.cor.redditi.regioni) <- c("RFA","RDI","RP","RIM","RAS","RTAX","TAX","RAD")
ggplot(cerchio.cor.redditi.regioni, aes(Comp.1, Comp.2)) +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[1,1], yend = cerchio.cor.redditi.regioni[1,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="magenta") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[2,1], yend = cerchio.cor.redditi.regioni[2,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="chartreuse4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[3,1], yend = cerchio.cor.redditi.regioni[3,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="firebrick2") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[4,1], yend = cerchio.cor.redditi.regioni[4,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="goldenrod") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[5,1], yend = cerchio.cor.redditi.regioni[5,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="springgreen4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[6,1], yend = cerchio.cor.redditi.regioni[6,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="slateblue1") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[7,1], yend = cerchio.cor.redditi.regioni[7,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="coral") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[8,1], yend = cerchio.cor.redditi.regioni[8,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="dodgerblue2") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

redditi.pca.regioni
ggplot(redditi.pca.regioni, aes(Comp.1, Comp.2, fill = redditi.medie.acp.regioni$Macroregione1)) +
  geom_label(label = rownames(redditi.pca.regioni), color = "gray1")+
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[1,1], yend = cerchio.cor.redditi.regioni[1,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="magenta") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[2,1], yend = cerchio.cor.redditi.regioni[2,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="chartreuse4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[3,1], yend = cerchio.cor.redditi.regioni[3,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="firebrick2") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[4,1], yend = cerchio.cor.redditi.regioni[4,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="goldenrod") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[5,1], yend = cerchio.cor.redditi.regioni[5,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="springgreen4") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[6,1], yend = cerchio.cor.redditi.regioni[6,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="slateblue1") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[7,1], yend = cerchio.cor.redditi.regioni[7,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="coral") +
  geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.redditi.regioni[8,1], yend = cerchio.cor.redditi.regioni[8,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="dodgerblue2") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(legend.title = element_blank())

#indici bontà regioni

pca.scores.regioni <- pca.regioni$scores

coseno.quadrato.regioni <- apply(pca.scores.regioni^2, 1, FUN=function(x){x/sum(x)})

coseno.quadrato.t.regioni <- t(coseno.quadrato.regioni)

apply(coseno.quadrato.t.regioni, 1, FUN=sum)

indici.coseno.regioni<-round(coseno.quadrato.t.regioni*100, digits=2)

colore <- ifelse(coseno.quadrato.t.regioni[,1]>0.1, "red", "black")		
colore <- ifelse(coseno.quadrato.t.regioni[,2]>=0.1, "green", colore )	
colore <- ifelse((coseno.quadrato.t.regioni[,1]<0.1 & coseno.quadrato.t.regioni[,2]<0.1), "orange", colore)	

plot(pca.regioni$scores[,1:2], type="n")
text(pca.regioni$scores[,1],pca.regioni$scores[,2],labels= rownames(redditi.pca.regioni),col = colore, cex=.6)
abline(h=0,v=0)



# Calcolo dei contributi assoluti alla  variabilità degli assi
contributi.assoluti <- apply(pca.scores^2, 2, FUN=function(x){x/sum(x)})
# per verificare l'esattezza dei risultati, ricordiamo che la somma per colonne della matrice dei contributi assoluti dà sempre 1
apply(contributi.assoluti, 2, FUN=sum)

indici.contributi<-round(contributi.assoluti*100, digits = 2)



