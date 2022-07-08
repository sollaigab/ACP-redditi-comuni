library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("superhero"),
                tags$style(
                    HTML("th { color: #ded9d9 }"),
                    HTML("label { color: #ded9d9 } "),
                    HTML("select {color: #000000} ")),
                tabsetPanel(
                    tabPanel("INTRO",
                             titlePanel(span("Laboratorio di Statistica e Data Mining",style="font-family:Impact; color:red")),
                             sidebarLayout(
                                 sidebarPanel(
                                     h2(span("Progetto:ACP",style="color:red")),
                                     p("Il progetto  consiste nell'analisi del dataset",span("redditi_comuni.csv", style = "color:red"),"finalizzata alla individuazione delle componenti principali."),
                                     br(),
                                     br(),
                                     h3(span("Gruppo",style="color:red")),
                                     p(strong(em("-Stefano Apicella"))),
                                     p(strong(em("-Simone Manzolillo"))),
                                     p(strong(em("-Gabriele Sollai")))
                                 ),
                                 mainPanel(
                                     h1(span("Dataset",style="color:red")),
                                     p("Il dataset", span("redditi_comuni.csv",style="color:red"),"fornisce l'elenco dei comuni italiani e per ogni comune sono riportate le informazioni sulle principali variabili Irpef"),
                                     p("Per informazioni aggiuntive  si riporta il sito del",
                                       a("MEF.", 
                                         href = "http://www1.finanze.gov.it/finanze3/analisi_stat/index.php?search_class[0]=cCOMUNE&opendata=yes")),
                                     br(),
                                     h2(span("Fasi del progetto",style="color:red")),
                                     p("1) Caricamento del dataset e preparazione della matrice dati;"),
                                     p("2) Analisi esplorativa del dataframe;"),
                                     p("3) Definizione della sottomatrice dei dati da analizzare;"),
                                     p("4) Analisi in componenti principali (ACP);"),
                                     p("5) Biplot."),
                                     p("6) Indice coseno quadrato"),
                                     p("7) ACP per comuni casuali"),
                                     p("8) ACP per regioni")
                                 )
                             )
                    ),
                    tabPanel("1 UPLOAD DEL DATASET",
                             sidebarLayout(
                                 sidebarPanel(
                                     h2(span("Caricamento Dataset",style="color:red")),
                                     p("Nella prima fase del progetto si carica il dataset",span("redditi_comuni.csv", style = "color:red"),"attraverso il comando:"),
                                     code('redditi <- read.csv("https://raw.githubusercontent.com/sollaigab/ACP-redditi-comuni/main/Redditi_comuni.csv", header=TRUE, sep=";", quote="", na.strings = "")')
                                 ),
                                 mainPanel(
                                     h1(span("Data cleaning",style="color:red")),
                                     p("Il dataset", 
                                       span("redditi",style="color:red"),"deve essere sottoposto a un processo di",strong("rilevamento, correzzione e rimozione"), "di record errati, imprecisi e irrilevanti."),
                                     p("La matrice dei dati di partenza si riferisce a",strong("7979 Comuni"), "rispetto ai quali sono state raccolte informazioni riferite a",strong("51 variabili"),"(riportate tra le colonne)"),
                                     p("Di seguito si riportano le modifiche che sono state apportate:"),
                                     br(),
                                     p("1) eliminazione della", strong("riga 7979"), "che non contiene informazioni"),
                                     p(code("redditi <- redditi[-7979,]")),
                                     p("2) aggiunta della variabile",span("Macroregione", style = "color:red")," con cui collochiamo ogni Comune ad una Macroregione"),
                                     p(code('regioni.nord <- names(table(redditi$Regione))[c(5,6,9,13,18,20,21,8)]')),
                                     p(code('regioni.centro <- names(table(redditi$Regione))[c(7,10,17,19)]')),
                                     p(code('regioni.mezzogiorno <- names(table(redditi$Regione))[c(1,2,3,4,11,14,15,16)]')),
                                     p(code('redditi$Macroregione <- ifelse(redditi$Regione%in%regioni.mezzogiorno, "Sud", NA)')),
                                     p(code('redditi$Macroregione <- ifelse(redditi$Regione%in%regioni.centro, "Centro", redditi$Macroregione)')),
                                     p(code('redditi$Macroregione <- ifelse(redditi$Regione%in%regioni.nord, "Nord", redditi$Macroregione)')),
                                     p("3) rinomina delle Regioni"),
                                     p("4) eliminazione delle variabili che non sono di interesse"),
                                     p("5) eliminazione delle variabili che superano gli",strong("800 missing value")),
                                     p("6) divisione delle variabili che hanno come denominazione",span("Ammontare in euro", style = "color:red"),"con quelle che hanno come denominazione", span("Frequenza.", style = "color:red"),"In questo modo si riportano i valori medi per ogni singola variabile economica."),
                                     p('-',span("Ammontare in euro",style= "color:red"), "si riferisce al reddito in valore assoluto"), 
                                     p('-',span("Frequenza", style= "color:red"), "si riferisce al numero contribuenti che dichiarano quel tipo di reddito"),
                                     p("7) rinomina delle variabili rimanenti"),
                                     br(),
                                     br(),
                                     p("Si riporta di seguito il dataframe finale denominato",span("redditi.medie.acp",style= "color:red"),"che ha",strong("13 variabili"), "e", strong("7978 rilevazioni:")),
                                     br(),
                                     br(),
                                     DT::dataTableOutput("table")
                                 )
                             )
                    ),
                    tabPanel("2.1 DATA VISUALIZATION",
                             titlePanel(span("Analisi esplorativa",style="font-family:Impact")),
                             sidebarLayout(
                                 sidebarPanel(
                                     h2(span("Dplyr",style="color:red")),
                                     p("Per effettuare l'analisi esplorativa del dataframe sono state utilizzate funzioni del package",span("dplyr", style = "color:red"),"Nello specifico ci siamo avvalsi di:"),
                                     p("1)",code('as_tibble("redditi")'),"per trasformare il dataframe in formato tibble"),
                                     p("2)",code("rename()"), "per rinominare alcune variabili"),
                                     p("3)",code("select()"),"per creare alcuni dataframe con specifiche variabili selezionate dal dataframe originale "),
                                     p("4)",code("mutate()"),"per aggiungere nuove variabili a un dataframe conservando quelle originali " ),
                                     p("5)",code("transmute()"),"per aggiungere nuove variabili ed eliminare quelle esistenti"),
                                     p("6)",code("summarise"),"che permette di creare righe riassuntive effettuando specifiche operazioni"),
                                     p("7)",code("arrange"), "per ordinare le righe"),
                                     p("8)",code("filter"),"per creare un dataframe mantenendo le righe che soddisfano certe condizioni"),
                                     br(),
                                     h2(span("Boxplot:",style="color:red")),
                                     selectInput("dati.irpef", "Boxplot singolo:", 
                                                 choices=colnames(redditi.medie.acp)[5:13]),
                                     helpText("non sono stati considerati i valori anomali"),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     selectInput("variabile", "Boxplot regioni confrontati:", 
                                                 choices=colnames(redditi.medie.acp)[5:13]),
                                     br()
                                 ),
                                 mainPanel( 
                                     h1(span("Boxplot",style="color:red")),
                                     p("Il boxplot risulta utile per comprendere se la distribuzione risulta simmetrica o asimmetrica e per confrontare la forma delle distribuzioni."),
                                     p("In particolare, permette di rappresentare sullo stesso grafico cinque tra le misure di posizione maggiormente utilizzate, ossia:il valore minimo, il primo quantile, la mediana, il terzo quantile e il valore massimo di ogni variabile."),
                                     p("Di seguito, risulta possibile selezionare il boxplot della variabile di interesse del dataset nella sezione",strong("Boxplot singolo.")),
                                     plotOutput("r.boxplot"),
                                     br(),
                                     p("Risulta utile anche effettuare una comparazione tra i boxplot. Di seguito, nella sezione",strong("Boxplot regioni"), "si seleziona la variabile di interesse rispetto alla quale si effettua una duplice comparazione:"),
                                     p("1)nel primo grafico tra le 20 Regioni distinte per Macroregioni"),
                                     p("2) nel secondo grafico invece si comparano solo le tre Macroregioni"),
                                     br(),
                                     plotOutput("a.boxplot"),
                                     br(),
                                     plotOutput("m.boxplot"),
                                     br()
                                 )
                             )
                    ),
                    tabPanel("2.2 DATA VISUALIZATION",
                             titlePanel(span("Analisi esplorativa",style="font-family:Impact")),
                             sidebarLayout(
                                 sidebarPanel(
                                     h2(span("Barplot:",style="color:red")),
                                     selectInput("vari", "Barplot Regioni:", 
                                                 choices=colnames(redditi.medie.acp)[5:13]),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     h2(span("Scatter Plot:",style="color:red")),
                                     br(),
                                     bootstrapPage(
                                         helpText("Risulta impossibile selezionare un valore inferiore al numero di osservazioni del dataset, che risulta essere pari a 7978."),
                                         br(),
                                         helpText("Risulta comunque opportuno un valore intorno a 500."),
                                         br(),
                                         helpText("Un valore eccessivamente alto di osservazioni rende difficile o addirittura impossibile l'interpretazione del grafico."),
                                         br(),
                                         helpText("Un valore eccessivamente basso di osservazioni rende il grafico non rappresentativo del dataset di riferimento"),
                                         br(),
                                         numericInput('nc', 'Dimensione del campione', nc),
                                         selectInput("reg", "Scatter Plot: variabile y:", 
                                                     choices=colnames(redditi.medie.acp)[c(5,6,7,8,9,10,12,13)])
                                     )
                                 ),
                                 mainPanel( 
                                     h1(span("Barplot",style="color:red")),
                                     p("In questa sezione si illustra l'analisi condotta con i barplot."),
                                     p("In particolare, il grafico che segue descrive sull'asse delle ascisse le singole Regioni, distinte in diversi colori a seconda della Macroregione di appartenenza."),
                                     p("Sull'asse delle ordinate invece sono riportati i valori della varibile selezionata."),
                                     plotOutput("r.barplot"),
                                     br(),
                                     br(),
                                     h1(span("Scatter Plot",style="color:red")),
                                     p("In questa sezione si illustra l'analisi condotta con gli scatter plot, o diagrammi a dispersione."),
                                     p("In particolare, il grafico che segue descrive due variabili su un piano cartesiano."),
                                     p("Abbiamo deciso di porre come variabile fissa ",span("Reddito.imponibile",style= "color:red"),", mentre le altre variabili sono selezionabili e riportate sull'asse delle ordinate"),
                                     p("Ogni osservazione viene distinta in base alla ",span("Macroregione",style="color:red"),"di appartenenza"),
                                     p("Dato che riportare 7978 osservazioni in un grafico di questo tipo, renderebbe difficile o impossibile l'interpretazione, abbiamo deciso di costruire lo scatterplot basandoci su un campione estratto casualmente dal dataset"),
                                     p("Risulta possibile selezionare la dimensione del campione, tenendo presente che non deve essere eccessivamente alta o bassa. E in ogni caso non risulta possibile sforare i 7978."),
                                     br(),
                                     bootstrapPage(
                                         plotOutput('regressione'),
                                         br(),
                                         br(),
                                         br(),
                                         br()
                                     )
                                 )
                             )
                    ),
                    tabPanel("3 INIZIALIZZAZIONE ACP",
                             titlePanel(span("Definizione e analisi della sottomatrice",style="font-family:Impact")),
                             sidebarLayout(
                                 sidebarPanel(
                                     h2(span("Fase 3: Definizione della sottomatrice dei dati",style="color:red")),
                                     br(),
                                     p("1) Si sostituiscono i missing value con la media della colonna di riferimento. Il dataset ottenuto viene definito ",span("redditi.finale",style="color:red")),
                                     br(),
                                     p("2) Si estrae un campione casuale. Per effettuare l'analisi abbiamo imposto un seme per rendere coerente ogni calcolo con uno specifico campione"),
                                     p("La procedura applicata risulta la seguente:"),
                                     p(code("set.seed(619)")),
                                     br(),
                                     p(code("campione.media <- sample_n(redditi.finale, size = 500)")),
                                     br(),
                                     p(span("campione.media",style="color:red")," risulta il campione  del dataset ",span("redditi.finale",style="color:red")," riferito a uno specifico seme."),
                                     p("Nel seguente progetto saranno illustrate analisi e considerazioni su tale campione, con la opzione di poter modificare il campione e la sua dimensione per ogni grafico")
                                 ),
                                 mainPanel( 
                                     h1(span("Fase 4: Analisi per componenti principali: introduzione",style="color:red")),
                                     br(),
                                     p("Per effettuare l'analisi del dataset si riscontra il problema dell'elevato numero di variabili."),
                                     p("Infatti, nonostante abbiamo estromesso molte variabili per variegati motivi, rimangono ancora 13 variabili che risultano comunque un numero elevato."),
                                     p("Per questo motivo si ci chiede se risulta possibile rappresentare le osservazioni in uno spazio di dimensione ridotta rispetto a quello originale, senza provocare una perdita eccessiva di informazioni."),
                                     p("La somma delle varianze delle ", strong(em("p variabili originali")), " che rappresenta la varianza totale, misura l'informazione conenuta nel dataset."),
                                     p("L'Analisi per componenti principali, o ", strong(em("ACP")),", trasforma l'insieme delle variabili originali in variabili incorrelate."),
                                     p("Le nuove variabili incorrelate sono dette", strong(em("componenti principali."))),
                                     p("L'insieme completo delle componenti principali spiega la varianza totale del dataset"),
                                     p("Gli obbiettivi sono: "),
                                     br(),
                                     p("1) Sostituire le ",strong(em("p variabili correlate")), "con ", strong(em("k variabili incorrelate")), "con ", strong(em("(k << p) ")), "preservando comunque la maggior parte dell'informazionecontenuta nel dataset."),
                                     br(),
                                     p("2) Agevolare l'interpretazione dei dati, identificando qualche caratteristica particolare nelle prime componenti principali."),
                                     br(),
                                     br(),
                                     h3(span("Le componenti principali", style="color:red")),
                                     br(),
                                     p("Le componenti principali sono definite come quelle combinazioni lineari ortogonali delle variabili originarie che spiegano in proporzioni progressivamente minori la varianza totale."),
                                     p("La prima componente principale risulta la combinazione lineare che spiega la quota massema di varianza delle variabili originarie "),
                                     p("La seconda componente principale risulta la combinazione lineare, incorrelata con la prima componente principale, che spiega la quota massima di varianza che rimane avendo estratto la prima componente principale."),
                                     p("Stessa logica si applica alle altre componenti"),
                                     br(),
                                     br()
                                 )
                             )
                    ),
                    tabPanel("4.1 ACP",
                             pageWithSidebar(
                                 headerPanel(span("Analisi per componenti principali",style="font-family:Impact")),
                                 sidebarPanel(
                                     p("Clicca il bottone per caricare il grafico"),
                                     actionButton("goButton", "Correlazioni", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                     br(),
                                     p("Col comando: "),
                                     p(code("pca <- princomp(campione.media.misura, scores = TRUE, cor = TRUE)")),
                                     p("Si effettua l'analisi per componenti principali"),
                                     br(),
                                     p("Per ogni componente risulta possibile visionare i corrispondenti autovalori."),
                                     p(code("autovalori <- pca$sdev^2")),
                                     br(),
                                     h3(span("Analisi autovalori", style="color:red")),
                                     p("Uno dei criteri adottatti per il numero di CP da selezionare risulta quello di conservare tutte le CP con autovalori maggiori di 1."),
                                     p("La spiegazione risulta essere che l'autovalore di una CP corrisponde alla sua varianza e, operando su variabili standardizzate, queste hanno varianze unitarie"),
                                     p("Quindi si decide di mantenere le CP che spiegano una quota di varianza totale maggiore di quella di una singola variabile"),
                                     br(),  
                                     DT::dataTableOutput("table.auto")
                                 ),
                                 mainPanel( 
                                     h1(span("Fase 4.1: Analisi per componenti principali: procedura",style="color:red")),
                                     br(),
                                     p("L'ACP risulta applicabile solo a una matrice di misura, ossia una matrice che possiede solo valori quantitativi."),
                                     p("Si esclude dall'analisi la variabile ",span("numero.contribuenti",style="color:red")," dato che ha natura demografica, mentre invece la nostra analisi si concentra sulla ricerca di CP in merito a variabili economiche. Inoltre attraverso il rapporto ammontare/frequenza che abbiamo eseguito, le variabili economiche non sono influenzate dal numero di contribuenti."),
                                     p("Si procede quindi col seguente comando:"),
                                     br(),
                                     p(code("ggpairs(campione.media.misura, axisLabels = 'none')")),
                                     br(),
                                     plotOutput("ncorr")
                                 )
                             )
                    ),
                    tabPanel("4.2 ACP",
                             pageWithSidebar(
                                 headerPanel(span("Analisi per componenti principali",style="font-family:Impact")),
                                 sidebarPanel(
                                     h3(span("Criterio n.1 (Criterio di Kaiser)",style="color:red")),
                                     br(),
                                     p("Si calcola la media delle varianza, ovvero ",strong(em("l'autovalore medio.")),"e si estraggono le prime K componenti la cui varianza supera tale media"),
                                     br(),
                                     p(code("(sum(autovalori))/8")),
                                     br(),
                                     h2(span("Conclusioni su numero CP",style="color:red")),
                                     h3(span("Caso 1",style="color:red")),
                                     p(strong(em("L'autovalore medio ")),"risulta 1 quindi in questo caso sono sufficienti le prime due CP"),
                                     h3(span("Caso 2",style="color:red")),
                                     p("Con la prima CP si spiega il 71.7% della varianza totale."),
                                     p("Con la seconda CP si spiega il 10.7% della varianza totale."),
                                     p("Con le prime due CP si spiega l'82% della varianza totale che riteniamo sufficiente per descrivere 8 variabili"),
                                     p("Abbiamo comunque fornito un'interpretazione anche della terza CP, e in questo modo la varianza totale sale al 90%"),
                                     h3(span("Caso 3",style="color:red")),
                                     p("Nel nostro caso si ha una brusca variazione tra il primo e secondo CP"),
                                     p("Dal secondo al terzo la variazione risulta molto bassa"),
                                     p("Dal terzo in poi la variazione risulta graduale"),
                                     p("Quindi i primi due CP sono sufficienti a descrivere le otto variabili")),
                                 mainPanel( 
                                     h1(span("Fase 4.2: Analisi per componenti principali: numero CP da scegliere",style="color:red")),
                                     br(),
                                     p("La scelta del numero minimo di componenti principali risulta una delle parti fondamentali nell'ACP."),
                                     p("Il numero scelto k deve permettere una descrizione valida ma anche sintetica della matrice dei dati originaria."),
                                     p("Si possono quindi adottare vari criteri"),
                                     br(),
                                     h3(span("Criterio n.2", style="color:red")),
                                     br(),
                                     p("Si considera un numero di CP tale da tener conto d'una percentuale sufficientemente elevata della varianza totale"),
                                     p("Col codice ", code("var.spieg <- autovalori/sum(autovalori)"), " si dividono i singoli autovalori per la somma totale."),
                                     p("In questo modo risulta possibile ottenere la seguente tabella in cui si possono visionare la percentuale di varianza spiegata da ogni CP e la varianza cumulata."),
                                     br(),
                                     DT::dataTableOutput("table.var"),
                                     br(),
                                     h3(span("Criterio n.3", style="color:red")),
                                     br(),
                                     p("Si costruisce un grafico chiamato ", strong(em("scree plot")), "o", strong(em("grafico della falda."))),
                                     p("Esso riporta il valore degli autovalori sull'asse delle ordinate e il numero d'ordine della CP sull'asse delle ascisse"),
                                     p("Gli autovalori sono in ordine decrescente, quindi il grafico risulta una ",strong(em("spezzata discendente."))),
                                     p("Se K componenti sono importanti e le restanti P-K trascurabili, tra K e K+1 si manifesta una brusca variazione della pendenza, definita ",strong(em("gomito"))),
                                     p("Il gomito segnala che K risulta essere il numero opportuno di CP da conservare"),
                                     br(),
                                     plotOutput("grafico.falda"),
                                     br()
                                 )
                             )
                    ),
                    tabPanel("4.3 ACP",
                             pageWithSidebar(
                                 headerPanel(span("Analisi per componenti principali",style="font-family:Impact")),
                                 sidebarPanel(
                                     h2(span("Interpretazione varianza variabili",style="color:red")),
                                     p(strong(em("La prima CP ")),"spiega oltre il 90% della varianza delle ",span("variabili economiche generali", style = "color:red"),"."),
                                     p("Inoltre descrive oltre il 70% della varianza di ",span("reddito lavoro dipendente e da pensione", style = "color:red"),"."),
                                     p("Mentre invece descrive meno del 55% della varianza di ",span("reddito medio dell'imprenditore e da partecipazioni", style = "color:red"),"."),
                                     p("Quindi la prima CP si conferma come ",strong(em("reddito generale della classe medio-povera.", style = "color:red"))),
                                     p(strong(em("La seconda CP ")), "descrive oltre il 60% della variabile ",span("reddito medio da fabbricati", style = "color:red")," e il 14% di ",span("reddito medio dell'imprenditore", style = "color:red"),"."),
                                     p("Quindi la seconda CP si conferma come variabile ",strong(em("investimenti in immobili della classe medio-ricca", style = "color:red")),"."),
                                     p("La variabile ",span("reddito medio da partecipazione", style = "color:red")," risulta quella descritta meno dalle prime due CP."),
                                     p("Ma se estendiamo l'analisi alla terza CP si nota che descrive il 30% di questa variabile."),
                                     p(strong(em("La terza CP ")), " sembra riferirsi al reddito generato dagli strumenti finanziari, ma essa confluisce anche l'8% di reddito di spettanza dell'imprenditore e il 9% di reddito da fabbricati."),
                                     p("Quindi alla terza CP confluisce il reddito della ",strong(em("classe estremamente ricca", style = "color:red"))," d'Italia,imprenditori che hanno investimenti sia in strumenti finanziari che in immobili")
                                 ),
                                 mainPanel( 
                                     h1(span("Fase 4.3: Analisi per componenti principali: interpretazioni",style="color:red")),
                                     h2(span("Varianza variabili",style="color:red")),
                                     p("A fini interpretativi si valuta per ogni CP anche la", strong(em("quota di varianza"))," spiegata per ogni singola variabile"),
                                     p("Il codice utilizzato risulta essere:"),
                                     p(code("var.func <- function(loadings,comp.sdev){loadings*comp.sdev}")),
                                     p(code("sdev.pca <- pca$sdev")),
                                     p(code("var.coord <- t(apply(load, 1, var.func, sdev.pca)")),
                                     p(code("quote.varianza <- var.coord^2")),
                                     br(),
                                     DT::dataTableOutput("table.quote.var")
                                 )
                             )
                    ),
                    tabPanel("5 INTERPRETAZIONE E BIPLOT",
                             pageWithSidebar(
                                 headerPanel(span("Commento risultati",style="font-family:Impact")),
                                 sidebarPanel(
                                     h2(span("Codici scores",style="color:red")),
                                     p(code("redditi.pca <- cbind(campione.media.misura, pca$scores[,1:3])")),
                                     p(code("ggplot(redditi.pca, aes(Comp.1, Comp.2, fill = n$Macroregione)) +")),
                                     p(code("stat_ellipse(geom = 'polygon', col = 'black', alpha = 0.5) +")),
                                     p(code("geom_point(shape = 21) +")),
                                     p(code("geom_hline(yintercept = 0) +")),
                                     p(code("geom_vline(xintercept = 0) +")),
                                     p(code("theme(legend.title = element_blank())")),
                                     br(),
                                     h2(span("Vettori variabili",style="color:red")),
                                     br(),
                                     h3(span("Interpretazione grafico variabili",style="color:red")),
                                     p(strong(em("parte superiore = ")),"mercato immobiliare"),
                                     p(strong(em("parte inferiore = ")),"mercato mobiliare"),
                                     p(strong(em("parte destra = ")),"comuni ricchi"),
                                     p(strong(em("parte sinistra = ")),"comuni poveri"),
                                     p(strong(em("quadrante 1 (in alto  a destra) = ")),"comuni ricchi con elevato reddito nel mercato immobiliare"),
                                     p(strong(em("quadrante 2 (in alto a sinistra) = ")),"comuni poveri con elevato reddito nel mercato immobiliare"),
                                     p(strong(em("quadrante 3 (in basso a sinistra) = ")),"comuni poveri con elevato reddito da impresa e mercato mobiliare"),
                                     p(strong(em("quadrante 4 (in basso a destra) = ")),"comuni poveri con elevato reddito da impresa e mercato mobiliare"),
                                     p(strong(em("centro = ")),"maggioranza dei casi, comuni con reddito medio omologato alla maggioranza dei comuni"),
                                     h3(span("Etichette",style="color:red")),
                                     p(strong(em("RFA = ", style="color:magenta")),"redditi per fabbricati"),
                                     p(strong(em("RDI = ", style="color:#458b00")),"redditi dipendenti"),
                                     p(strong(em("RP = ", style="color:	#ee2c2c")),"redditi pensioni"),
                                     p(strong(em("RIM = ", style="color:goldenrod")),"redditi imprenditori"),
                                     p(strong(em("RAS = ", style="color:#008b45")),"redditi asset"),
                                     p(strong(em("RTAX = ", style="color:#836fff")),"redditi imponibili"),
                                     p(strong(em("TAX = ", style="color:coral")),"imposta"),
                                     p(strong(em("RAD = ", style="color:#1c86ee")),"redditi addizionali"),
                                     br(),
                                     h2(span("Interpretazione biplot",style="color:red")),
                                     br(),
                                     p("L'interpretazione delle posizioni di scores e vettori col biplot fornisce un'informazione addizionale che non risulta possibile ottenere dall'analisi disgiunta dei due grafici, dato che in questo modo si mettono in luce relazioni tra scores e variabili"),
                                     p(strong(em("punti vicini al centroide = ")),"Comuni con valori delle p variabili prossimi alle rispettive medie"),
                                     p(strong(em("punti lontani dal centroide e vicino a uno degli assi = ")),"Comuni con un punteggio di quella CP alto"),
                                     p(strong(em("punti lontani dal centroide e vicino a un vettore = ")),"Comuni con valore di quella variabile maggiore della media"),
                                     p(strong(em("ATTENZIONE:")), "Risulta possibile effettuare uno zoom in un'area indicata del grafico:"),
                                     p("1) Si seleziona un'area da zoommare con il cursore e appare una mano"),
                                     p("2) Con doppio click si effettua lo zoom"),
                                     p("3) Per tornare al grafico originale basta cliccare due volte, senza selezionare un'ulteriore area altrimenti si effettua un secondo zoom")
                                 ),
                                 mainPanel( 
                                     h1(span("Fase 5: Interpretazione e Biplot",style="color:red")),
                                     p("Il Biplot indica una tecnica grafica di rappresentazione in un piano cartesiano in sovrapposizione di variabili e scores."),
                                     p("Il prefisso 'bi' nella denominazione di biplot infatti si riferisce a questa caratteristica simultanea."),
                                     br(),
                                     h2(span("Grafico scores",style="color:red")),
                                     p("Il primo grafico che compone il biplot risulta essere il diagramma di dispersione degli ",strong(em("scores")), " delle prime due CP."),
                                     plotOutput("grafico.individui"),
                                     br(),
                                     h2(span("Grafico variabili",style="color:red")),
                                     p("Il secondo grafico che compone il biplot risulta essere il grafico dei punti variabili, ossia i ", strong(em("p vettori"), " indicanti i coefficienti di correlazione tra ognuna delle prime due CP e le variabili indicati nella seguente tabella:")),
                                     DT::dataTableOutput("table.cor.comp"),
                                     br(),
                                     p("I vettori sono sempre contenuti nel cerchio di raggio unitario, con centro nell'origine."),
                                     p("La lunghezza di un vettore coincide con la quota di varianza della variabile spiegata dalle prime due CP"),
                                     p("Se il vettore corrispondente ad una variabile si avvicina al valore unitario, questo indica che la variabile in esame risulta riprodotta quasi perfettamente nel piano cartesiano dalle prime due CP"),
                                     p("L'angolo tra ciascun vettore ed ognuno dei due assi segnala la correlazione tra variabili e CP."),
                                     p("Quindi se l'angolo risulta di 90 gradi, la variabile e la CP sono incorrelate."),
                                     plotOutput("grafico.corr.variabili"),
                                     br(),
                                     h2(span("Grafico biplot",style="color:red")),
                                     p("Sovrapponendo il diagramma di dispersione degli scores e il grafico delle correlazioni variabili-CP si ottiene il ",strong(em("biplot")),", illustrato qui di seguito:"),
                                     plotOutput("grafico.biplot", height = 300,
                                                dblclick = "grafico.biplot_dblclick",
                                                brush = brushOpts(
                                                    id = "grafico.biplot_brush",
                                                    resetOnNew = TRUE
                                                )
                                     ),
                                     br(),
                                     br(),
                                 )
                             )
                    ),
                    tabPanel("6 INDICI DI COSENO QUADRATO",
                             pageWithSidebar(
                                 headerPanel(span("Valutazione scores",style="font-family:Impact")),
                                 sidebarPanel(
                                     h2(span("Codici quadrato coseno",style="color:red")),
                                     p(code("pca.scores <- pca$scores")),
                                     p(code("coseno.quadrato <- apply(pca.scores^2, 1, FUN=function(x){x/sum(x)})")),
                                     p(code("coseno.quadrato.t <- t(coseno.quadrato)")),
                                     p(code("indici.coseno<-round(coseno.quadrato.t*100, digits=2)")),
                                     helpText("La funzione",strong(em(" apply ")),"permette di applicare la funzione FUN alle righe (=2) o alle colonne (=1) di una matrice."),
                                     h2(span("Codici contributi agli assi",style="color:red")),
                                     p(code("contributi.assoluti <- apply(pca.scores^2, 2, FUN=function(x){x/sum(x)})")),
                                     p(code("indici.contributi<-round(contributi.assoluti*100, digits = 2)")),
                                     helpText("Anche in questo caso risulta possibile effettuare un controllo con: ", code("apply(contributi.assoluti, 2, FUN=sum)"), ", dato che la somma per colonne della matrice dei contributi assoluti risulta sempre 1.")
                                 ),
                                 mainPanel( 
                                     h1(span("Indici",style="color:red")),
                                     h2(span("Calcolo quadrato del coseno",style="color:red")),
                                     p("Per effettuare un'analisi della rappresentanza del biplot, si effettua il quadrato degli scores."),
                                     p("I valori bassi indicano che quel punto non risulta rappresentato in modo idoneo sul rispettivo asse."),
                                     p("Tali valori sono riportati in percentuale nella tabella seguente."),
                                     DT::dataTableOutput("table.coseno.quadrato"),
                                     p("Per verificare l'esattezza dei risultati, ricordiamo che la somma per righe della matrice dei coseni risulta sempre 1."),
                                     p("Quindi basta effettuare il seguente comando e controllare che i risultati siano tutti 1."),
                                     p(code("apply(coseno.quadrato.t, 1, FUN=sum)")),
                                     br(),
                                     p("Sul grafico degli individui, possiamo anche colorare di rosso i punti mal rappresentati sull'asse 1, di verde quelli mal rappresentati sull'asse 2 e di arancione quelli mal rappresentati su entrambi gli assi 1 e 2"),
                                     p("In questo caso consideriamo come mal rappresentati i punti con indice coseno quadro minore di 0.1."),
                                     plotOutput("rappresentazione"),
                                     br(),
                                     h2(span("Contributi alla varianza degli assi",style="color:red")),
                                     p(" Risulta possibile anche visualizzare i contributi assoluti in percentuale."),
                                     p("I valori alti indicano che quel punto contribuisce in misura maggiore degli altri alla ricostruzione della varianza sul rispettivo asse"),
                                     DT::dataTableOutput("table.contributi"),
                                     br()
                                 )
                             )
                    ),
                    tabPanel("7 ACP PER COMUNI CASUALI",
                             pageWithSidebar(
                                 headerPanel(span("Grafici sintetici",style="font-family:Impact")),
                                 sidebarPanel(
                                     h2(span("Campione casuale",style="color:red")),
                                     p("Di seguito si riporta una barra attraverso la quale risulta possibile effettuare la medesima analisi ACP per differenti campioni casuali con diversa grandezza del campione analizzato."),
                                     p("Risulta possibile infatti visionare nella sezione Grafici: "),
                                     p(strong("- lo screeplot:")," per la selezione del numero di CP"),
                                     p(strong("- il biplot:")," per l'analisi CP"),
                                     p(strong("- grafico coseno quadrato:"), "per verificare la mal rappresentazione."),
                                     p("Nella sezione Tabelle risulta possibile visionare invece:"),
                                     p(strong("- la varianza spiegata:"), "come secondo criterio di selezione CP."),
                                     p(strong("- la quota di varianza:"), "per l'interpretazione dell'ACP."),
                                     br(),
                                     sliderInput("numero","Numero di osservazioni:",
                                                 value = 500,
                                                 min= 100,
                                                 max= 7686),
                                 ),
                                 mainPanel( 
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Grafici", plotOutput("plotcasuale"),br(), plotOutput("biplotcasuale", height = 300,
                                                                                                                dblclick = "biplotcasuale_dblclick",
                                                                                                                brush = brushOpts(
                                                                                                                    id = "biplotcasuale_brush",
                                                                                                                    resetOnNew = TRUE)),br(),plotOutput("qua.cas"),br()),
                                                 tabPanel("Tabelle", DT::dataTableOutput("varianzacasuale"),br(),DT::dataTableOutput("quotevarianzacasuale"),br())
                                     )
                                 )
                             )
                    ),
                    tabPanel("8 ACP SU REGIONI",
                             pageWithSidebar(
                                 headerPanel(span("ACP SINTETICO",style="font-family:Impact")),
                                 sidebarPanel(
                                     h2(span("Dataset regionale",style="color:red")),
                                     p("Per effettuare l'ACP sulle Regioni abbiamo utilizzato le funzioni del pacchetto",strong(" dplyr")," per sommare le righe dei Comuni con la medesima Regione."),
                                     br(),
                                     p(code("redditi.regioni.t <- redditi.t %>%
                                          group_by(Regione) %>%
                                       summarise_at(vars(c(7:37)), sum, na.rm = TRUE)")),
                                     br(),
                                     p("Nell'analisi si illustrano gli stessi grafici e tabelle della fase 7, ma in questo caso non si analizza un campione ma l'intero dataset."),
                                     h3(span("Etichette",style="color:red")),
                                     p(strong(em("RFA = ", style="color:magenta")),"redditi per fabbricati"),
                                     p(strong(em("RDI = ", style="color:#458b00")),"redditi dipendenti"),
                                     p(strong(em("RP = ", style="color:	#ee2c2c")),"redditi pensioni"),
                                     p(strong(em("RIM = ", style="color:goldenrod")),"redditi imprenditori"),
                                     p(strong(em("RAS = ", style="color:#008b45")),"redditi asset"),
                                     p(strong(em("RTAX = ", style="color:#836fff")),"redditi imponibili"),
                                     p(strong(em("TAX = ", style="color:coral")),"imposta"),
                                     p(strong(em("RAD = ", style="color:#1c86ee")),"redditi addizionali"),
                                     br(),
                                     h3(span("Grafici e tabelle",style="color:red")),
                                     p("Nella sezione Grafici abbiamo: "),
                                     p(strong("- lo screeplot:")," per la selezione del numero di CP"),
                                     p(strong("- il biplot:")," per l'analisi CP"),
                                     p(strong("- grafico coseno quadrato:"), "per verificare la mal rappresentazione."),
                                     p("Nella sezione Tabelle risulta abbiamo invece:"),
                                     p(strong("- la varianza spiegata:"), "come secondo criterio di selezione CP."),
                                     p(strong("- la quota di varianza:"), "per l'interpretazione dell'ACP."),
                                     
                                 ),
                                 mainPanel( 
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Grafici", plotOutput("plotregioni"),br(), plotOutput("biplotregioni"),br(), plotOutput("qua.cas.regioni"),br()),
                                                 tabPanel("Tabelle", DT::dataTableOutput("varianzaregioni"),br(),DT::dataTableOutput("quotevarianzaregioni"),br())
                                     )
                                 )
                             )
                    )
                )
)


# Define server logic ----
server <- function(input, output) {
    data.intacc <- reactive({      #creata funzione per la fase 2.1 per creare dataset di Regione, Macroregione, variabile considerata
        if (input$variabile == "Numero.contribuenti") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Numero.contribuenti")
        } else if (input$variabile == "Reddito.fabbricati") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.fabbricati")
        } else if (input$variabile =="Reddito.dipendente" ) {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.dipendente")
        }else if (input$variabile == "Reddito.pensione") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.pensione")
        } else if (input$variabile == "Reddito.imprenditore") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.imprenditore")
        }else if (input$variabile == "Reddito.partecipazione") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.partecipazione")
        } else if (input$variabile == "Reddito.imponibile") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.imponibile")
        }else if (input$variabile == "Imposta.netta") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Imposta.netta")
        } else if (input$variabile == "Reddito.addizionale") {
            micro <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.addizionale")
        }
        return(micro)
    })
    data.int <- reactive({      #creata funzione per la fase 2.2 per creare dataset di Regione, Macroregione, variabile considerata
        if (input$vari == "Numero.contribuenti") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Numero.contribuenti")
        } else if (input$vari == "Reddito.fabbricati") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.fabbricati")
        } else if (input$vari =="Reddito.dipendente" ) {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.dipendente")
        }else if (input$vari == "Reddito.pensione") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.pensione")
        } else if (input$vari == "Reddito.imprenditore") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.imprenditore")
        }else if (input$vari == "Reddito.partecipazione") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.partecipazione")
        } else if (input$vari == "Reddito.imponibile") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.imponibile")
        }else if (input$vari == "Imposta.netta") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Imposta.netta")
        } else if (input$vari == "Reddito.addizionale") {
            micr <- redditi.medie.acp %>% select("Regione"|"Macroregione"|"Reddito.addizionale")
        }
        return(micr)
    })
    data.pro<- function(nc){ #creo funzione che restituisce campione con dimensione nc che seleziona l'utente
        dt<-sample_n(redditi.medie.acp, size = input$nc)
        return(dt)
    }
    data.regre <- reactive({      #creata funzione per la fase 2.2 per creare dataset di Regione, Macroregione, variabile considerata
        if (input$reg == "Numero.contribuenti") {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Numero.contribuenti"|"Reddito.imponibile")
        } else if (input$reg == "Reddito.fabbricati") {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Reddito.fabbricati"|"Reddito.imponibile")
        } else if (input$reg =="Reddito.dipendente" ) {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Reddito.dipendente"|"Reddito.imponibile")
        }else if (input$reg == "Reddito.pensione") {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Reddito.pensione"|"Reddito.imponibile")
        } else if (input$reg == "Reddito.imprenditore") {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Reddito.imprenditore"|"Reddito.imponibile")
        }else if (input$reg == "Reddito.partecipazione") {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Reddito.partecipazione"|"Reddito.imponibile")
        } else if (input$reg == "Imposta.netta") {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Imposta.netta"|"Reddito.imponibile")
        } else if (input$reg == "Reddito.addizionale") {
            cr <- data.pro() %>% select("Regione"|"Macroregione"|"Reddito.addizionale"|"Reddito.imponibile")
        }
        return(cr)
    })
    bcorr<-eventReactive(input$goButton,{    #4.1
        ggpairs(campione.media.misura, axisLabels = "none")
    })
    output$table <- DT::renderDataTable(DT::datatable({
        data <-redditi.medie.acp
    }))
    output$r.boxplot <- renderPlot({
        boxplot(redditi.medie.acp[,input$dati.irpef],outline=FALSE, 
                main=input$dati.irpef
        )
    })#######################
    output$a.boxplot <- renderPlot({
        ggplot(data = data.intacc(), 
               if (input$variabile == "Numero.contribuenti"){
                   aes(x=Regione,y=Numero.contribuenti, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.fabbricati"){
                   aes(x=Regione, y=Reddito.fabbricati, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.dipendente"){
                   aes(x=Regione, y=Reddito.dipendente, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.pensione"){ 
                   aes(x=Regione, y=Reddito.pensione, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.imprenditore"){
                   aes(x=Regione, y=Reddito.imprenditore, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.partecipazione"){
                   aes(x=Regione, y=Reddito.partecipazione, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.imponibile"){
                   aes(x=Regione, y=Reddito.imponibile, fill= Macroregione)
               }
               else if (input$variabile == "Imposta.netta"){
                   aes(x=Regione, y=Imposta.netta, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.addizionale"){
                   aes(x=Regione, y=Reddito.addizionale, fill= Macroregione)
               })+
            geom_boxplot(outlier.colour = "red", outlier.shape = 21, colour = "black", notch = TRUE) +
            if (input$variabile == "Numero.contribuenti"){
                ylim(0,25000)
            }
        else if (input$variabile == "Reddito.fabbricati"){
            ylim(0,2500)
        }
        else if (input$variabile == "Reddito.dipendente"){
            ylim(1000,30000)
        }
        else if (input$variabile == "Reddito.pensione"){ 
            ylim(1000,30000)
        }
        else if (input$variabile == "Reddito.imprenditore"){
            ylim(1000,30000)
        }
        else if (input$variabile == "Reddito.partecipazione"){
            ylim(1000,30000)
        }
        else if (input$variabile == "Reddito.imponibile"){
            ylim(1000,30000)
        }
        else if (input$variabile == "Imposta.netta"){
            ylim(0,10000)
        }
        else if (input$variabile == "Reddito.addizionale"){
            ylim(1000,30000)
        }
    })
    output$m.boxplot <- renderPlot({
        ggplot(data = data.intacc(), 
               if (input$variabile == "Numero.contribuenti"){
                   aes(x=Macroregione,y=Numero.contribuenti, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.fabbricati"){
                   aes(x=Macroregione, y=Reddito.fabbricati, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.dipendente"){
                   aes(x=Macroregione, y=Reddito.dipendente, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.pensione"){ 
                   aes(x=Macroregione, y=Reddito.pensione, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.imprenditore"){
                   aes(x=Macroregione, y=Reddito.imprenditore, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.partecipazione"){
                   aes(x=Macroregione, y=Reddito.partecipazione, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.imponibile"){
                   aes(x=Macroregione, y=Reddito.imponibile, fill= Macroregione)
               }
               else if (input$variabile == "Imposta.netta"){
                   aes(x=Macroregione, y=Imposta.netta, fill= Macroregione)
               }
               else if (input$variabile == "Reddito.addizionale"){
                   aes(x=Macroregione, y=Reddito.addizionale, fill= Macroregione)
               })+
            geom_boxplot(outlier.colour = "red", outlier.shape = 21, colour = "black", notch = TRUE) +
            if (input$variabile == "Numero.contribuenti"){
                ylim(0,25000)
            }
        else if (input$variabile == "Reddito.fabbricati"){
            ylim(0,2500)
        }
        else if (input$variabile == "Reddito.dipendente"){
            ylim(1000,30000)
        }
        else if (input$variabile == "Reddito.pensione"){ 
            ylim(1000,30000)
        }
        else if (input$variabile == "Reddito.imprenditore"){
            ylim(1000,30000)
        }
        else if (input$variabile == "Reddito.partecipazione"){
            ylim(1000,30000)
        }
        else if (input$variabile == "Imposta.netta"){
            ylim(0,10000)
        }
        else if (input$variabile == "Reddito.addizionale"){
            ylim(1000,30000)
        }
    })
    output$r.barplot <- renderPlot({
        ggplot(data = data.int(), 
               if (input$vari == "Numero.contribuenti"){
                   aes(x=Regione,y=Numero.contribuenti, fill= Macroregione)
               }
               else if (input$vari == "Reddito.fabbricati"){
                   aes(x=Regione, y=Reddito.fabbricati, fill= Macroregione)
               }
               else if (input$vari == "Reddito.dipendente"){
                   aes(x=Regione, y=Reddito.dipendente, fill= Macroregione)
               }
               else if (input$vari == "Reddito.pensione"){ 
                   aes(x=Regione, y=Reddito.pensione, fill= Macroregione)
               }
               else if (input$vari == "Reddito.imprenditore"){
                   aes(x=Regione, y=Reddito.imprenditore, fill= Macroregione)
               }
               else if (input$vari== "Reddito.partecipazione"){
                   aes(x=Regione, y=Reddito.partecipazione, fill= Macroregione)
               }
               else if (input$vari == "Reddito.imponibile"){
                   aes(x=Regione, y=Reddito.imponibile, fill= Macroregione)
               }
               else if (input$vari == "Imposta.netta"){
                   aes(x=Regione, y=Imposta.netta, fill= Macroregione)
               }
               else if (input$vari == "Reddito.addizionale"){
                   aes(x=Regione, y=Reddito.addizionale, fill= Macroregione)
               })+
            geom_bar(stat = "summary", col = "black")+
            if (input$vari == "Numero.contribuenti"){
                ylim(0,5000)
            }
        else if (input$vari == "Reddito.fabbricati"){
            ylim(0,2500)
        }
        else if (input$vari == "Reddito.dipendente"){
            ylim(0,30000)
        }
        else if (input$vari == "Reddito.pensione"){ 
            ylim(0,30000)
        }
        else if (input$vari == "Reddito.imprenditore"){
            ylim(0,30000)
        }
        else if (input$vari == "Reddito.partecipazione"){
            ylim(0,30000)
        }
        else if (input$vari == "Reddito.imponibile"){
            ylim(0,30000)
        }
        else if (input$vari == "Imposta.netta"){
            ylim(0,10000)
        }
        else if (input$vari == "Reddito.addizionale"){
            ylim(0,30000)
        }
    })
    output$regressione <- renderPlot({
        ggplot(data=data.regre(), 
               if (input$reg == "Numero.contribuenti"){
                   aes(x=Reddito.imponibile,y=Numero.contribuenti, col= Macroregione)
               }
               else if (input$reg == "Reddito.fabbricati"){
                   aes(x=Reddito.imponibile, y=Reddito.fabbricati, col= Macroregione)
               }
               else if (input$reg == "Reddito.dipendente"){
                   aes(x=Reddito.imponibile, y=Reddito.dipendente, col= Macroregione)
               }
               else if (input$reg == "Reddito.pensione"){ 
                   aes(x=Reddito.imponibile, y=Reddito.pensione, col= Macroregione)
               }
               else if (input$reg == "Reddito.imprenditore"){
                   aes(x=Reddito.imponibile, y=Reddito.imprenditore, col= Macroregione)
               }
               else if (input$reg== "Reddito.partecipazione"){
                   aes(x=Reddito.imponibile, y=Reddito.partecipazione, col= Macroregione)
               }
               else if (input$reg == "Imposta.netta"){
                   aes(x=Reddito.imponibile, y=Imposta.netta, col= Macroregione)
               }
               else if (input$reg == "Reddito.addizionale"){
                   aes(x=Reddito.imponibile, y=Reddito.addizionale, col= Macroregione)
               }) +
            geom_point(size = 3) +
            if (input$reg == "Numero.contribuenti"){
                ylim(0,5000)
            }
        else if (input$reg == "Reddito.fabbricati"){
            ylim(0,2500)
        }
        else if (input$reg == "Reddito.dipendente"){
            ylim(0,30000)
        }
        else if (input$reg == "Reddito.pensione"){ 
            ylim(0,30000)
        }
        else if (input$reg == "Reddito.imprenditore"){
            ylim(0,30000)
        }
        else if (input$reg == "Reddito.partecipazione"){
            ylim(0,30000)
        }
        else if (input$reg == "Imposta.netta"){
            ylim(0,10000)
        }
        else if (input$reg == "Reddito.addizionale"){
            ylim(0,30000)
        }
    })
    output$ncorr <- renderPlot({   #4.1
        bcorr()                   
    })
    output$table.auto <- DT::renderDataTable(DT::datatable({   #4.1
        data.auto <-data.autovalori    
    }), options = list(dom = 'ft'))
    output$table.var <- DT::renderDataTable(DT::datatable({   #4.2
        data.var <-data.varianza   
    }, options = list(dom = 'ft')))
    output$table.quote.var <- DT::renderDataTable(DT::datatable({   #4.3
        data.quote.var <-quote.varianza  
    }), options = list(dom = 'ft'))
    output$grafico.falda<- renderPlot({    #4.2
        plot(pca, type = "l")
    })
    output$grafico.individui<- renderPlot({
        redditi.pca <- cbind(campione.media.misura, pca$scores[,1:3])
        ggplot(redditi.pca, aes(Comp.1, Comp.2, fill = n$Macroregione)) +
            stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
            geom_point(shape = 21) +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 0) +
            theme(legend.title = element_blank())
    })
    output$table.cor.comp <- DT::renderDataTable(DT::datatable({   #4.3
        data.cor.comp <-cor(campione.media.misura, pca$scores[, 1:3])
    }), options = list(dom = 'ft'))
    output$grafico.corr.variabili<- renderPlot({
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
    })
    ranges <- reactiveValues(x = NULL, y = NULL)  #5
    output$grafico.biplot<- renderPlot({              
        ggplot(redditi.pca, aes(Comp.1, Comp.2, fill = n$Macroregione)) +
            geom_point(shape = 21,size = 3) +
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
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme(legend.title = element_blank())
    })
    observeEvent(input$grafico.biplot_dblclick, {
        brush <- input$grafico.biplot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    output$table.coseno.quadrato <- DT::renderDataTable(DT::datatable({   #6
        data.cos.quad <- indici.coseno
    }), options = list(dom = 'ft'))
    output$rappresentazione<-renderPlot({
        plot(pca$scores[,1:2], type="n")
        points(pca$scores[,1],pca$scores[,2], col = colore, cex=.6)
        abline(h=0,v=0)
    })
    output$table.contributi <- DT::renderDataTable(DT::datatable({   #6
        data.cos.contributi <- indici.contributi
    }), options = list(dom = 'ft'))
    output$plotcasuale<- renderPlot({
        numero<- input$numero
        camp <- sample_n(redditi.senzamissing, size = numero)
        camp <- camp[complete.cases(camp),]
        camp.med <- camp[,-c(1, 2, 3, 4,5)]
        rownames(camp.med) <- camp$Denominazione.Comune
        colnames(camp.med) <- c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz")
        cor.camp.med <- cor(camp.med)
        pca.cas <- princomp(camp.med, scores = TRUE, cor = TRUE)
        plot(pca.cas, type = "l")
    })
    output$qua.cas<- renderPlot({
        numero<- input$numero
        camp <- sample_n(redditi.senzamissing, size = numero)
        camp <- camp[complete.cases(camp),]
        camp.med <- camp[,-c(1, 2, 3, 4,5)]
        rownames(camp.med) <- camp$Denominazione.Comune
        colnames(camp.med) <- c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz")
        pca.cas <- princomp(camp.med, scores = TRUE, cor = TRUE)
        pca.scores.cas <- pca.cas$scores
        coseno.quadrato.cas <- apply(pca.scores.cas^2, 1, FUN=function(x){x/sum(x)})
        coseno.quadrato.t.cas <- t(coseno.quadrato.cas)
        colore.cas <- ifelse(coseno.quadrato.t.cas[,1]>0.1, "red", "black")		
        colore.cas <- ifelse(coseno.quadrato.t.cas[,2]>=0.1, "green", colore )	
        colore.cas <- ifelse((coseno.quadrato.t.cas[,1]<0.1 & coseno.quadrato.t.cas[,2]<0.1), "orange", colore.cas)	
        plot(pca.cas$scores[,1:2], type="n")
        points(pca.cas$scores[,1],pca.cas$scores[,2], col = colore.cas, cex=.6)
        abline(h=0,v=0)
    })
    ranges.casuale <- reactiveValues(x = NULL, y = NULL) 
    output$biplotcasuale<- renderPlot({
        numero<- input$numero
        camp <- sample_n(redditi.senzamissing, size = numero)
        camp <- camp[complete.cases(camp),]
        camp.med <- camp[,-c(1, 2, 3, 4,5)]
        rownames(camp.med) <- camp$Denominazione.Comune
        colnames(camp.med) <- c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz")
        pca.cas <- princomp(camp.med, scores = TRUE, cor = TRUE)
        redditi.pca.casuale <- cbind(camp.med, pca.cas$scores[,1:3])
        cerchio.cor.camp <- cor(camp.med,pca.cas$scores)
        cerchio.cor.camp<- data.frame(cerchio.cor.camp)
        ggplot(redditi.pca.casuale, aes(Comp.1, Comp.2, fill = camp$Macroregione)) +
            geom_point(shape = 21,size = 3) +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[1,1], yend = cerchio.cor.camp[1,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="magenta") +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[2,1], yend = cerchio.cor.camp[2,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="chartreuse4") +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[3,1], yend = cerchio.cor.camp[3,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="firebrick2") +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[4,1], yend = cerchio.cor.camp[4,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="goldenrod") +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[5,1], yend = cerchio.cor.camp[5,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="springgreen4") +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[6,1], yend = cerchio.cor.camp[6,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="slateblue1") +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[7,1], yend = cerchio.cor.camp[7,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="coral") +
            geom_segment(aes(x = 0, y = 0, xend = cerchio.cor.camp[8,1], yend = cerchio.cor.camp[8,2]), size = 1, arrow = arrow(length = unit(0.3,"cm")), color ="dodgerblue2") +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 0) +
            scale_x_continuous(limits = c(-1,1)) +
            scale_y_continuous(limits = c(-1,1)) +
            coord_cartesian(xlim = ranges.casuale$x, ylim = ranges$y, expand = FALSE) +
            theme(legend.title = element_blank())
    })
    observeEvent(input$biplotcasuale_dblclick, {
        brush <- input$biplotcasuale_brush
        if (!is.null(brush)) {
            ranges.casuale$x <- c(brush$xmin, brush$xmax)
            ranges.casuale$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges.casuale$x <- NULL
            ranges.casuale$y <- NULL
        }
    })
    dcasuale.var<- reactive({ 
        numero<- input$numero
        camp <- sample_n(redditi.senzamissing, size = numero)
        camp <- camp[complete.cases(camp),]
        camp.med <- camp[,-c(1, 2, 3, 4,5)]
        rownames(camp.med) <- camp$Denominazione.Comune
        colnames(camp.med) <- c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz")
        pca.cas <- princomp(camp.med, scores = TRUE, cor = TRUE)
        autovalori.cas <- pca.cas$sdev^2
        var.spieg.cas <- autovalori.cas/sum(autovalori.cas)
        data.autovalori.cas<-data.frame(autovalori.cas)
        var.cum.cas<-cumsum(var.spieg.cas)
        data.varianza.cas<-data.frame(var.spieg.cas,var.cum.cas) })
    output$varianzacasuale<-DT::renderDataTable(DT::datatable({
        dcasuale.var()  
    }, options = list(dom = 'ft')))
    dcasuale.quote.var<- reactive({ 
        numero<- input$numero
        camp <- sample_n(redditi.senzamissing, size = numero)
        camp <- camp[complete.cases(camp),]
        camp.med <- camp[,-c(1, 2, 3, 4,5)]
        rownames(camp.med) <- camp$Denominazione.Comune
        colnames(camp.med) <- c("r.fabbric","r.dipen","r.pensio","r.impren","r.asset","r.taxable","tax","r.addiz")
        pca.cas <- princomp(camp.med, scores = TRUE, cor = TRUE)
        var.func <- function(loadings,comp.sdev){
            loadings*comp.sdev
        }
        sdev.pca.cas <- pca.cas$sdev
        load.cas <- pca.cas$loadings
        load.cas<-print(load, cutoff=0)
        var.coord.cas <- t(apply(load.cas, 1, var.func, sdev.pca.cas))
        quote.varianza.cas <- var.coord.cas^2
    })
    output$quotevarianzacasuale<-DT::renderDataTable(DT::datatable({
        dcasuale.quote.var()  
    }, options = list(dom = 'ft')))
    output$plotregioni<- renderPlot({      #8
        plot(pca.regioni, type = "l")
    })
    output$biplotregioni<- renderPlot({
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
    })
    output$qua.cas.regioni<- renderPlot({      #8
        plot(pca.regioni$scores[,1:2], type="n")
        text(pca.regioni$scores[,1],pca.regioni$scores[,2],labels= rownames(redditi.pca.regioni),col = colore, cex=.6)
        abline(h=0,v=0)
    })
    output$varianzaregioni<-DT::renderDataTable(DT::datatable({
        da.var.reg<-data.varianza.regioni  
    }, options = list(dom = 'ft')))
    output$quotevarianzaregioni<-DT::renderDataTable(DT::datatable({
        data.quote.regioni<- quote.varianza.regioni  
    }, options = list(dom = 'ft')))
}

# Run the app ----
shinyApp(ui = ui, server = server)