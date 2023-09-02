# Iniziamo importando il set di dati e le librerie che utilizzeremo

library(moments)
library(dplyr)
library(ggplot2)
library(gghalves)
library(ggpubr)
library(GGally)
library(ggdist)
library(car)
library(lmtest)
library(scatterplot3d)

data <- read.csv("neonati.csv", stringsAsFactors = T)
data$Fumatrici <- as.factor(data$Fumatrici)
head(data,10)

#Il dataset contiene dati su 2500 neonati raccolti in 3 diversi ospedali. Ci sono 10 variabili per ogni osservazione:
  
#- `Anni.madre`: l'età della madre (variabile quantitativa continua);
#- `N.gravidanze`: il numero di gravidanze che la madre ha già affrontato (variabile quantitativa discreta); 
#- `Fumatrici`: è 0 se la madre non fuma, altrimenti è 1 (variabile qualitativa su scala nominale); 
#- `Gestazione`: numero di settimane di gestazione (variabile quantitativa continua);
#- `Peso`: peso del bambino, in g (variabile quantitativa continua);
#- `Lunghezza`: lunghezza del bambino, in mm (variabile quantitativa continua); 
#- `Cranio`: diametro del cranio del bambino, in mm (variabile quantitativa continua);
#- `Tipo.parto`: tipo di parto, naturale o cesareo (variabile qualitativa su scala nominale);
#- `Ospedale`: ospedale, 1, 2 o 3 (variabile qualitativa su scala nominale);
#- `Sesso`: sesso del bambino, Maschio o Femmina (variabile qualitativa su scala nominale).

#Il progetto mira a prevedere il peso del bambino, date tutte le altre variabili. Pertanto, studieremo come 
#le variabili influenzano il peso e vedremo quali di esse giocano un ruolo rilevante nella sua determinazione. 
#Per raggiungere questo obiettivo, utilizziamo la **regressione lineare multipla**.

#Prima di tutto, riassumiamo rapidamente ogni colonna del set di dati mediante la funzione `summary()`:
summary(data)

#Vediamo che il valore minimo della variabile `Anni.madre` è 0, quindi controlliamo:
filter(data, Anni.madre<10)

#Questi valori sono impossibili per l'età della madre. Tuttavia, il resto delle informazioni contenute nelle 
#righe con `Anni.madre` uguale a 0 o 1 sembra plausibile. Pertanto, scegliamo di mantenerle, modificando il 
#valore di `Anni.madre`.Siamo portati a scegliere la mediana perchè è una misura più robusta meno sensibile 
#agli outlier. Pertanto, scegliamo di sostituire i valori anomali di `Anni.madre` con la mediana, ovvero 28:

data$Anni.madre <- replace(data$Anni.madre,data$Anni.madre<2,median(data$Anni.madre))

#Verifichiamo se il peso e la lunghezza del neonato medio sono comparabili con le medie della popolazione. 
#Prima di farlo, vediamo se il sesso del neonato è statisticamente significativo per quanto riguarda il peso 
#e la lunghezza. A tal fine, eseguiamo un **two-sample t-test**:

two_cols <- c("pink","lightblue")
p_peso <- ggplot(data, aes(x = Sesso, y = Peso)) +
                 geom_half_boxplot(aes(color = Sesso), side = "l",
                    alpha = 0.8,
                    width = 0.5,
                    outlier.shape = 16,
                    outlier.size = 2,
                    linewidth = 0.75,
                    outlier.stroke = 0.75
                    ) +
                 geom_half_violin(aes(fill = Sesso), alpha = 0.5, side = "r") +  
                 scale_color_manual(values = two_cols) +
                 scale_y_continuous(breaks = seq(500, 5500, 500)) +
                 labs(x = "Sesso",
                 y = "Peso (g)",
                 title = "Peso del bambino in base al sesso") +
                 theme_minimal() +
                 theme(plot.title = element_text(size = 20, hjust = 0.5),
                       axis.text.x = element_text(size = 10),
                       axis.text.y = element_text(size = 10),
                       axis.title = element_text(size = 16))

p_lunghezza <- ggplot(data, aes(x=Sesso,y=Lunghezza))+
                      geom_half_boxplot(aes(color = Sesso), side = "l",
                                  alpha=0.8,
                                  width = 0.5,
                                  outlier.shape=16,
                                  outlier.size=2,
                                  linewidth = 0.75,
                                  outlier.stroke = 0.75
                                  )+
                      geom_half_violin(aes(fill = Sesso), alpha = 0.5, side = "r") +
                      scale_color_manual(values = two_cols)+
                      scale_y_continuous(breaks = seq(300,600,50))+
                      labs(x="Sesso",
                           y="Lunghezza (mm)",
                           title = "Lunghezza del bambino in base al sesso")+
                      theme_minimal()+
                      theme(plot.title = element_text(size = 20, hjust = 0.5),
                        axis.text.x = element_text(size = 10),
                        axis.text.y = element_text(size = 10),
                        axis.title = element_text(size = 16))

ggarrange(p_peso,p_lunghezza,nrow = 1)

t_peso <-t.test(data = data,
                Peso ~ Sesso,
                paired = F)
t_lunghezza <-t.test(data = data,
                     Lunghezza ~ Sesso,
                     paired = F)
t_peso$p.value;t_lunghezza$p.value

#In entrambi i casi, il p-value è molto piccolo, quindi rifiutiamo l'ipotesi nulla, 
#concludendo che la differenza tra i due valori medi è statisticamente significativa.

#Secondo [questo sito](https://www.babycenter.com/baby/baby-development/average-weight-and-growth-chart-for-babies-toddlers-and-beyo_10357633), 
#negli Stati Uniti il peso medio dei bambini alla nascita è di 3,2 kg per le femmine 
#e di 3,4 kg per i maschi, mentre la lunghezza media dei neonati è di 49,5 cm, con 
#le femmine che misurano 49,2 cm e i maschi che misurano 49,9 cm.<br> Vediamo ora 
#se la differenza tra le medie del campione e quelle della popolazione è statisticamente significativa,
#eseguendo 4 diversi t-test:

t_peso_F <- t.test(filter(data, Sesso=="F")["Peso"],
                   mu = 3200,
                   conf.level = 0.95,
                   alternative = "two.sided")

t_peso_M <- t.test(filter(data, Sesso=="M")["Peso"],
                   mu = 3400,
                   conf.level = 0.95,
                   alternative = "two.sided")

t_lunghezza_F <- t.test(filter(data, Sesso=="F")["Lunghezza"],
                        mu = 492,
                        conf.level = 0.95,
                        alternative = "two.sided")

t_lunghezza_M <- t.test(filter(data, Sesso=="M")["Lunghezza"],
                        mu = 499,
                        conf.level = 0.95,
                        alternative = "two.sided")

t_peso_F$p.value;t_peso_M$p.value;t_lunghezza_F$p.value;t_lunghezza_M$p.value

#I valori trovati su Internet sulle medie della popolazione non sono gli stessi 
#per ogni sito, e piccole variazioni nel peso e nella lunghezza media delle bambine 
#portano a un p-value molto più alto. Ad esempio, se consideriamo una media di 3150 g 
#e 491 mm per il peso e la lunghezza delle ragazze, rispettivamente, troveremo:

t_peso_F_3150 <- t.test(filter(data, Sesso=="F")["Peso"],
                        mu = 3150,
                        conf.level = 0.95,
                        alternative = "two.sided")
t_lunghezza_F <- t.test(filter(data, Sesso=="F")["Lunghezza"],
                        mu = 491,
                        conf.level = 0.95,
                        alternative = "two.sided")
t_peso_F_3150$p.value;t_lunghezza_F$p.value

#Possiamo notare che sono superiori al livello di significatività del 5%. Pertanto, 
#concludiamo che la non conoscenza del valore preciso del peso e della lunghezza medi 
#della popolazione non consente di trarre conclusioni con certezza. Tuttavia, possiamo 
#dire che il campione mostra la stessa tendenza della popolazione, per quanto riguarda 
#la dipendenza del peso e della lunghezza dal sesso.

#Possiamo anche studiare se il sesso influenza il numero di settimane di gestazione 
#e il diametro del cranio:

two_cols <- c("pink","lightblue")
p_peso <- ggplot(data, aes(x=Sesso,y=Gestazione))+
            geom_half_boxplot(aes(color = Sesso), side = "l",
                        alpha=0.8,
                        width = 0.5,
                        outlier.shape=16,
                        outlier.size=2,
                        linewidth = 0.75,
                        outlier.stroke = 0.75
                        )+
            geom_half_violin(aes(fill = Sesso), alpha = 0.5, side = "r") +
            scale_color_manual(values = two_cols)+
            scale_y_continuous(breaks = seq(25,45,5))+
            labs(x="Sesso",
                 y="Numero di settimane",
                 title = "Settimane di gestazione in base al sesso")+
            theme_minimal()+
            theme(plot.title = element_text(size = 20, hjust = 0.5),
                  axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 16))

p_lunghezza <- ggplot(data, aes(x=Sesso,y=Cranio))+
                  geom_half_boxplot(aes(color = Sesso), side = "l",
                              alpha=0.8,
                              width = 0.5,
                              outlier.shape=16,
                              outlier.size=2,
                              linewidth = 0.75,
                              outlier.stroke = 0.75
                              )+
                  geom_half_violin(aes(fill = Sesso), alpha = 0.5, side = "r") +
                  scale_color_manual(values = two_cols)+
                  scale_y_continuous(breaks=seq(230,410,20))+
                  labs(x="Sesso",
                       y="Diametro (mm)",
                       title = "Diametro del cranio in base al sesso")+
                  theme_minimal()+
                  theme(plot.title = element_text(size = 20, hjust = 0.5),
                        axis.text.x = element_text(size = 10),
                        axis.text.y = element_text(size = 10),
                        axis.title = element_text(size = 16))

ggarrange(p_peso,p_lunghezza,nrow = 1)

t.test(data = data,
       Gestazione ~ Sesso,
       paired = F)
t.test(data = data,
       Cranio ~ Sesso,
       paired = F)

#Entrambi i p-value sono molto piccoli, quindi concludiamo che la differenza tra i valori 
#medi del numero di settimane di gestazione, calcolati per ciascun sesso, è statisticamente 
#significativa. Lo stesso vale per il diametro del cranio.

#Possiamo anche studiare se esiste una correlazione tra il tipo di nascita e il sesso 
#del neonato. A questo proposito, poiché entrambe sono variabili categoriali, osserviamo
#la tabella di contingenza:

contingency_birth_sex <- table(data$Tipo.parto, data$Sesso)

ggballoonplot(data = as.data.frame(contingency_birth_sex),
              fill = "value",
              size.range = c(5, 15),
              label = TRUE) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
  guides(size = FALSE) +
  labs(x = "Tipo di Parto",
       y = "Sesso",
       title = "Sesso vs Tipo di Parto",
       fill = "Frequenza") +
  theme(plot.title = element_text(size = 26, hjust = 0.5),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.margin = margin(20, 20, 20, 20, "pt")) +
  annotate("text", x = 2.0, y = 2.5, label = "Dimensione delle bolle = Frequenza") +
  geom_hline(yintercept = 1:2 - 0.5, color = "white", size = 0.5) +
  geom_vline(xintercept = 1:2 - 0.5, color = "white", size = 0.5)

#Dal grafico si evince che non c'è correlazione tra le due variabili. 
#Ciò è confermato dal **test di indipendenza Chi-Square**:

chisq.test(contingency_birth_sex)

#Infatti, poiché il p-value è superiore a 0,05, non rifiutiamo l'ipotesi nulla di 
#indipendenza delle due variabili categoriali.

#L'ultima analisi che effettueremo è quella relativa alla correlazione tra il tipo 
#di nascita e l'ospedale. A tal fine, costruiamo nuovamente la tabella di contingenza ed 
#eseguiamo il test Chi-quadro di indipendenza:

contingency_birth_hospital <- table(data$Tipo.parto,data$Ospedale)
contingency_birth_hospital
ggballoonplot(data = as.data.frame(contingency_birth_hospital),
              fill = "value",
              size.range = c(2,12))+
  scale_fill_gradient()+
  labs(x="Tipo di parto",
       y="Ospedale",
       title = "Ospedale vs tipo di parto",
       fill = "Frequenza")+
  guides(size=F)+
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title.align = 0.5)

chisq.test(contingency_birth_hospital)

#Il p-value è di circa 0,58, quindi non rifiutiamo l'ipotesi nulla e concludiamo che
# la correlazione tra le due variabili non è statisticamente significativa.
#In altre parole non c'è una differenza significativa tra gli ospedali nella frequenza dei cesarei.

#Prima di iniziare la procedura di selezione delle caratteristiche, costruiamo 
#una matrice di grafici, composta da diagrammi per visualizzare correttamente la 
#correlazione di ogni combinazione di variabili del dataframe: 

data <- data %>%
  relocate(Peso,.before = Anni.madre) %>%
  relocate(Fumatrici,.before = Tipo.parto)

panel.corr <- function(x, y){
  
  par(usr = c(0, 1, 0, 1))
  
  if (length(unique(x))<=3 & length(unique(y))<=3){
    
    contingency_table <- table(y,x)
    test <- chisq.test(contingency_table)
    p.value <- test$p.value
    txt <- paste("Chi-quadro","p-value:", signif(p.value,3), sep="\n")
    text(0.5, 0.5, txt, cex = 2)
  }    
  else if (length(unique(x))==3 & length(unique(y))>3) {
    
    
    test <- pairwise.t.test(y, 
                            x,
                            paired = F,
                            pool.sd = T,
                            p.adjust.method = "none")
    osp_12 <- test$p.value[1,1]
    osp_13 <- test$p.value[2,1]
    osp_23 <- test$p.value[2,2]
    txt <- paste("Pairwise\n",
                 "p-values\n",
                 "1-2:", signif(osp_12,3), "\n",
                 "1-3:", signif(osp_13,3), "\n",
                 "2-3:", signif(osp_23,3), sep="")
    text(0.5, 0.5, txt, cex = 1.1)
    
  } 
  else if (length(unique(x))==2 & length(unique(y))>3) {
    
    test <- t.test(y ~ x,
                   paired = F)
    p.value <- test$p.value
    txt <- paste("t-test",
                 "p-value:",
                 signif(p.value,3),
                 sep="\n")
    text(0.5, 0.5, txt, cex = 2)
  } 
  else{
    
    r <- round(cor(x, y), digits=3)
    txt <- paste("Corr:", r, sep="\n")
    text(0.5, 0.5, txt, cex = 2)
    
  }
}

panel.scat <- function(x, y){
  if (length(unique(x))>3 & length(unique(y)) %in% c(2,3)){
    par(new=T)
    boxplot(x~y, 
            horizontal = T,
            xaxt="n",
            yaxt="n", 
            col = "lightcoral")
  }
  else if (length(unique(x))>3 & length(unique(y))>3) {
    points(x,y, pch = 20, cex = 1, col = "lightcoral")
  }
  else{
    if (length(unique(y))==3) {
      par(new=T)
      barplot(table(y,x)/max(table(y,x)),
              col=c("lightcoral","lightgreen","lightblue"),
              ylim=c(0,1.2),
              beside = T,
              axes=F,
              xaxt="n")
    }
    else{
      par(new=T)
      barplot(table(y,x)/max(table(y,x)),
              col=c("lightcoral","lightgreen"),
              ylim=c(0,1.2),
              beside = T,
              axes=F,
              xaxt="n")
    }
  }
}

big <- function(labels){
  text(mean(x),mean(y),"ciao")
}

pairs(data,
      upper.panel = panel.corr,
      lower.panel = panel.scat,
      cex.labels = 1.0,
      labels = c("Peso", "Anni\n Madre","Numero di\n gravidanze",
                 "Settimane di\n gestazione","Lunghezza","Diametro del\n Cranio",
                 "Fumatrici","Tipo di\n Parto","Ospedale","Sesso"))

#Abbiamo scelto di mostrare:
  
#- il coefficiente di correlazione lineare quando entrambe le variabili sono continue;
#- il p-value del two sample t-test  quando una variabile è continua e l'altra è dicotomica;
#- il p-value del t-test a coppie quando una variabile è continua e l'altra è categorica con più di 2 livelli;
#- il p-value del test Chi-quadro di indipendenza quando entrambe le variabili sono categoriche

#Dai valori della prima riga, si evince che la variabile `Peso` è altamente correlata con le variabili
#`Gestazione`, `Lunghezza` e `Cranio`. D'altra parte, osservando i p-value della stessa riga, si scopre 
#che il peso del neonato è fortemente influenzato anche dal sesso (come già visto in precedenza), 
#mentre non dipende in modo significativo dalle variabili `Fumatrici`, `Tipo.parto` e `Ospedale`.

#Poiché vogliamo fare una previsione sul peso di un neonato, cerchiamo un modello che descriva bene 
#come possiamo dedurlo dalle altre variabili. A questo proposito, partiamo dal modello che contiene tutte 
#le variabili:

mod1 <- lm(Peso ~ .,data = data)
summary(mod1)

#La variabile con il coefficiente corrispondente con il p-value più alto è `Anni.madre`, quindi 
#la eliminiamo dal modello: 

mod2 <- update(mod1,~.-Anni.madre)
summary(mod2)

#L'$R^2$ aggiustato è sostanzialmente lo stesso di prima, e questo fornisce un'ulteriore prova 
#che `Anni.madre` può essere eliminata dal modello. Procediamo con la selezione delle caratteristiche 
#eliminando la variabile `Ospedale`:

mod3 <- update(mod2,~.-Ospedale)
summary(mod3)

#Anche in questo caso, la minuscola diminuzione dell'$R^2$ aggiustato ci dice che `Ospedale` 
#non apporta alcun miglioramento reale al modello. A questo punto, eliminiamo anche la variabile 
#`Fumatrici` e vediamo cosa succede:

mod4 <- update(mod3,~.-Fumatrici)
summary(mod4)

#Nello stesso spirito di prima, proviamo a rimuovere anche la variabile `Tipo.parto` e 
#vediamo come cambia l'$R^2$ aggiustato:

mod5 <- update(mod4,~.-Tipo.parto)
summary(mod5)

#L'$R^2$ aggiustato rimane quasi invariato e tutti i coefficienti rimangono statisticamente significativi.
#Pertanto, la selezione delle variabili ci porta a identificare `mod5` come un buon candidato. 
#Lo stesso risultato si otterrebbe se si utilizzasse la funzione `stepAIC` del pacchetto `MASS`:

n <- nrow(data)
stepwise.mod <- MASS::stepAIC(mod1,
                              direction = "both",
                              k=log(n))
summary(stepwise.mod)

#dove `k=log(n)` fornisce il BIC, mentre `direction="both"` indica che la tecnica per la 
#feature selection è mista, ovvero una combinazione di backward e forward selection.

#Osservando i diagrammi di dispersione tra la variabile `Peso` e le variabili `Gestazione`, 
#`Lunghezza` e `Cranio`, sembrano esserci effetti non lineari. Ciò è evidente anche dalla 
#leggera forma a U nel grafico in alto a sinistra della figura seguente:

par(mfrow=c(2,2))
plot(mod5)

#che rappresenta i residui di `mod5` in funzione dei valori inseriti. Pertanto, ora proviamo 
#ad aggiungere i termini quadratici corrispondenti e vediamo se il nuovo modello fornisce un adattamento migliore: 

mod6 <- update(mod5,~.+I(Gestazione^2)+I(Lunghezza^2)+I(Cranio^2))
summary(mod6)

par(mfrow=c(2,2))
plot(mod6)

#L'aggiunta dei termini quadratici nei predittori porta a una diminuzione dei valori dei residui, 
#che ora presentano una forma più "orizzontale", se confrontati con quelli di `mod5`. Tuttavia, 
#a differenza di prima, la variabile `Cranio` sembra non essere più staticamente significativa. 
#Prima di eliminare il suo effetto principale, eliminiamo il suo effetto non lineare e vediamo cosa succede:


mod7 <- update(mod6,~.-I(Cranio^2))
summary(mod7)

#La variabile `Cranio` è tornata a essere statisticamente significativa, quindi la manteniamo.
#Inoltre, l'$R^2$ aggiustato è diminuito solo di poco, quindi siamo portati a preferire il `mod7` al `mod6`.
#Tuttavia, per decidere quale sia il modello migliore, esaminiamo i valori del BIC e dell'AIC di tutti i modelli costruiti:

BIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7)
AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7)

#Secondo l'AIC, il miglior modello è `mod6`, mentre il BIC ci porterebbe a scegliere `mod7`. 
#Questo accade perché il BIC pone una penalità più pesante sui modelli con molte variabili. 
#Poiché la differenza tra l'AIC di `mod6` e `mod7` è minuscola, se confrontata, per esempio, 
#alla differenza tra `mod5` e `mod6`, decidiamo di scegliere `mod7` come modello migliore, 
#preferendo un modello più semplice piuttosto che uno con più parametri.

#Prima di procedere, valutiamo anche la presenza di *multicollinearità** calcolando i 
#variance inflation factors (VIFs) di `mod7`

vif(mod7, type="predictor")

#Poiché tutti i VIF sono inferiori a 5, non si riscontrano livelli significativi di multicollinearità.

#Analizziamo i residui di `mod7`:

par(mfrow=c(2,2))
plot(mod7)

#dove le linee rosse sono volte a facilitare l'identificazione di una tendenza. 
#Verifichiamo se la loro media è compatibile con lo zero:

mean(residuals(mod7));sd(residuals(mod7))

#Possiamo quindi affermare che nel nostro caso i residui hanno media zero.
#Verifichiamo ora l'ipotesi di **omoschedasticità** mediante il **test di Breusch-Pagan**:

bptest(mod7)

#Il test ci porta a rifiutare l'ipotesi nulla, quindi concludiamo che i residui sono eteroschedastici,
#cioè non hanno una varianza costante.

#Eseguiamo ora il **test di Durbin-Watson** per verificare l'ipotesi di **indipendenza** dei residui:

dwtest(mod7)

#In questo caso, il p-value è superiore al livello di significatività del 5%, quindi non rifiutiamo
#l'ipotesi nulla che gli errori siano non correlati. Ciò è positivo, poiché gli errori standard dei 
#coefficienti di regressione stimati, utilizzati per calcolare la statistica t, si basano sull'ipotesi 
#di termini di errore non correlati.

#Infine, studiamo la presenza di **outlier** e/o **leverage point**: i primi sono punti per i quali
#$y_i$ è lontano dal valore previsto dal modello, mentre le osservazioni ad alta leva hanno un valore 
#insolito per $x_i$.<br> Per quantificare se un punto è problematico o meno, utilizziamo il grafico in 
#basso a destra della figura precedente. In particolare, vediamo che l'unico valore che ha una distanza di Cook 
#maggiore del valore critico di 1 è l'osservazione n. 1551:

cook <- cooks.distance(mod7)
ggplot()+
  geom_point(aes(x=1:length(cook),
                 y=cook,
                 colour = cook>1),
             size = 3)+
  geom_hline(aes(yintercept=c(0.5,1)),
             linetype=2,
             colour = "darkred")+
  geom_label(aes(x=2500,y=c(0.55,1.05),label=c(0.5,1)),
             size=5,
             fill="darkred",
             colour = "white")+
  scale_colour_manual(values = setNames(c("darkred","black"),c(T,F)))+
  labs(title = "Analisi dei residui: Cook's distance",
       x = "Index",
       y = "Cook's distance")+
  theme_minimal()+
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = "none")

#Pertanto, avendo una distanza di Cook molto grande, proviamo a rimuovere questo valore 
#ed eseguiamo nuovamente la regressione lineare multipla:

index <- match(max(cook),cook)
new_data <- data[-index,]
mod8 <- lm( Peso ~ N.gravidanze + Gestazione + 
             Lunghezza + Cranio + Sesso + I(Gestazione^2) + I(Lunghezza^2),
           data = new_data)
summary(mod8)

#La rimozione dell'osservazione provoca un aumento dell'$R^2$ aggiustato, ma anche 
#un sostanziale aumento del p-value del coefficiente associato a `I(Gestation.weeks^2)`, 
#anche se ancora statisticamente significativo. Proviamo a rimuoverla: 

mod9 <- update(mod8,~.-I(Gestazione^2))
summary(mod9)
BIC(mod7,mod8,mod9)

#In base al valore del BIC, siamo portati a scegliere `mod9` come miglior modello di 
#regressione lineare multipla. Consideriamo i suoi residui:

par(mfrow=c(2,2))
plot(mod9)

#Come in precedenza, non rifiutiamo l'ipotesi di non correlazione dei residui, 
#come si evince dal test di Durbin-Watson:

dwtest(mod9)

#Eseguiamo ora il test Breusch-Pagan:

bptest(mod7);bptest(mod9)

#Il p-value del test di Breush-Pagan è ora significativamente più alto rispetto a prima, ma sempre sotto lo 0.05%, 
#per cui non rifiutiamo l'ipotesi di omoschedasticità. 
#Pertanto, possiamo concludere che il `mod9` può essere utilizzato per fare una previsione affidabile 
#del peso di un neonato.

#Cerchiamo di semplificare il modello per visualizzare i dati, pur mantenendo gli ingredienti essenziali.
#A tal fine, dividiamo i dati in base al sesso del neonato. Quindi, scegliamo altre due variabili tra 
#i predittori del `mod9`, che sono `Gestazione`, `N.gravidanze`, `Lunghezza` e `Cranio`. 
#Ad esempio, consideriamo la coppia di variabili data da `Gestazione` e `Lunghezza`. Si ottiene quindi 
#il seguente diagramma di dispersione tridimensionale:

colors <- c("pink","lightblue")
colors <- colors[as.numeric(data$Sesso)]
s3d <- scatterplot3d(data$Peso~data$Gestazione+data$Lunghezza, 
                     color = colors,
                     pch = 16,
                     angle = 50,
                     main = "3D Scatter plot:Relazione tra Gestazione, Lunghezza e Peso del Bambino",
                     xlab = "Gestazione",
                     ylab = "Lunghezza (mm)",
                     zlab = "Peso del bambino (g)",
                     grid = T,
                     box = F)
legend("topleft", legend = levels(data$Sesso),
       col =  c("pink","lightblue"), 
       pch = 16,
       inset = +0.03,
       xpd = TRUE,
       horiz = FALSE,
       bty="n")
F_data <- filter(data, Sesso=="F")
M_data <- filter(data, Sesso=="M")
my.lm_F <- lm(Peso ~ Gestazione + Lunghezza, data = F_data)
my.lm_M <- lm(Peso ~ Gestazione + Lunghezza, data = M_data)
summary(my.lm_F)
summary(my.lm_M)
s3d$plane3d(my.lm_F, lty.box = "solid", col = "pink", lwd = 1.5)
s3d$plane3d(my.lm_M, lty.box = "solid", col = "lightblue", lwd = 1.5)

#Dai due piani di regressione vediamo che, se manteniamo fisso il numero di settimane di gestazione,
#il peso aumenterà di più per un maschio che per una femmina.

#In conclusione, facciamo una previsione sul peso di un neonato. Ad esempio, consideriamo una madre che:
  
#- ha già affrontato 2 gravidanze;
#- partorirà alla 39a settimana.

#Supponiamo inoltre di non avere informazioni sulla lunghezza e sul diametro del cranio. 
#In questo caso, possiamo utilizzare i loro valori medi per fornire una stima di questi parametri. 
#Per essere più precisi, consideriamo i valori medi relativi alle bambine, poiché abbiamo visto prima 
#che la lunghezza e il diametro sono significativamente influenzati dal sesso del neonato. 
#Detto questo, otteniamo che il valore previsto del suo peso sarà:

predict(mod9, 
        newdata = data.frame(Sesso="F",Gestazione=39,N.gravidanze=2,
                             Lunghezza=mean(data$Lunghezza[data$Sesso=="F"]),
                             Cranio=mean(data$Cranio[data$Sesso=="F"])),
        interval = "predict")

#dove `lwr` e `upr` rappresentano i valori estremi del corrispondente intervallo di previsione del 95%. 
#In altre parole, c'è il 95% di probabilità che il peso del neonato sia compreso tra
#$2643,\text{g}$ e $3690,\text{g}$.

