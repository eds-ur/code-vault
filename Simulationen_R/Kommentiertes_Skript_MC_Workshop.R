#################################################
#####                                       #####
#####   Workshop Monte Carlo Simulationen   #####
#####                                       #####
#################################################

### Seed setzen

set.seed(2e3+15)


### Grundlagen

# Sequenzen

s1 <- seq(from = 0, to = 1, by = .1) # Sequenz von 0 bis 1
s2 <- seq(0,1, length = 11) # Gleiche Sequenz auf andere Art

s1 == s2


# Ein paar Beispiele fuer den Rep(licate) Befehl

rep(x = 42,times = 100)
rep(c("a","b"),4)
rep(letters[1:2]) # Default ist eine Wiederhlung

rep(LETTERS[24:25],times = 4, each = 2) # das "each" Argument gibt an, wie oft jedes Element wiederholt wird
sort(rep(LETTERS[24:25],times = 4, each = 2))
sort(rep(s1,10)) #Einfache Art eine Reihe zu sortiert zu vervielfaeltigen


# Zufallsziehung ganzer Zahlen ohne und mit Zuruecklegen

sample(10)
sample(10, replace = T) #Replace = Zuruecklegen


urne <- c(2,3,5,1,9,10,332,2,51)
sample(urne,4)


### Schleifen (und Listen)

sample.loop <- list(Zahl = sample(10), Bedingung = rep(NA,10))
sample.loop[[2]]  # Auf Elemente von Listen zugreifen
sample.loop[[1]][1:3] # Auf Unterelemente von Listen zugreifen


for (i in 1 : 10){ # i nimmt in jedem durchlauf den naechsten Wert an
  if (sample.loop[[1]][i] <= 10 && sample.loop[[1]][i] > 6) sample.loop[[2]][i] <- "larger than six" 
  else if (sample.loop[[1]][i] == 6) sample.loop[[2]][i] <- "six" 
  else sample.loop[[2]][i] <- "smaller than six"
}

frame.loop <- data.frame("Zahl" = sample.loop[[1]], "Wert" = sample.loop[[2]])
frame.loop


frame.loop.ordered <- frame.loop[order(frame.loop$Zahl),]
frame.loop.ordered


frame.loop$Zahl[frame.loop$Wert == "smaller than six"]


# Die Loop-Elemente koennen flexibel gewaehlt werden

sample.loop.2 <- list(Zahl = sample(10), Bedingung = rep(NA,10))

for (i in seq(from = 2, to = 10, by = 2)){ # i nimmt in jedem durchlauf den naechsten Wert an
  if (sample.loop.2[[1]][i] <= 10 && sample.loop.2[[1]][i] > 6) sample.loop.2[[2]][i] <- "larger than six" 
  else if (sample.loop.2[[1]][i] == 6) sample.loop.2[[2]][i] <- "six" 
  else sample.loop.2[[2]][i] <- "smaller than six"
}

frame.loop.2 <- data.frame("Zahl" = sample.loop.2[[1]], "Wert" = sample.loop.2[[2]])
frame.loop.2


frame.loop.ordered.2 <- frame.loop.2[order(frame.loop.2$Zahl),]
frame.loop.ordered.2


### Funktionen

my.function <- function(n = 100, min = 51){ #In der Klammer stehen die Argumente, die die Funktion benoetigt, hinter dem = steht der Default-Wert
  s <- sample(n)
  bigger <- s[s>=min]
  return(bigger) # Return gibt die Ausgabe der Funktion an
}


over.50 <- my.function()
mean(over.50)
length(over.50)/100 # Gibt an, wie viel Prozent der Werte genomme

over.30 <- my.function(200,30)
mean(over.30)
length(over.30)/200

### Ziehen mit Zuruecklegen

s.r <- sample(100,replace = T)
ap <- length(subset(s.r,s.r > 50.5))
ap/100 #Gibt an wie viel Prozent der Verteilung

s.r.frame <- data.frame(Werte = s.r, hit = rep(NA,100))

up <- s.r[s.r > 50.5]
misses <- up[up < ap]
down <- s.r[s.r < 50.5]
misses <- c(misses,down[down > ap])

s.r.frame[s.r > 50.5, 2] <- "hitup"
s.r.frame[s.r < 50.5, 2] <- "hitdown"

for(i in 1: length(misses)){
  s.r.frame[s.r.frame[,1] == misses[i],2] <- "miss"
}  
s.r.frame


plot(s.r.frame[,1] ,ylim=c(0,100))
abline(ap, 0, cex  = 2, col = "red")
abline(50,0, lty = 3, cex = 2, col = "red")
points(seq(100)[s.r.frame[,2] == "hitup"], s.r.frame[s.r.frame[,2] == "hitup",1], col = "green", pch = 19)
points(seq(100)[s.r.frame[,2] == "hitdown"], s.r.frame[s.r.frame[,2] == "hitdown",1], col = "green", pch = 19)
points(seq(100)[s.r.frame[,2] == "miss"], s.r.frame[s.r.frame[,2] == "miss",1], col = "red", pch = 19)


### Monte Carlo

# Verfahren aus der Stochastik. 
# Grosse Zahl gleichartiger Zufallsexperimente wird durchgefuehrt.
# Durch Wahrscheinlichkeitstheoretische Ansaetze wird ein Wert numerisch approximiert.
# Grundlage ist das Starke Gesetz der Grossen Zahlen.


### Simulation der Ziehung aus einer Population

set.seed(3567)

s.r.2 <- sample(200000,replace = F) #Population wird simuliert
mean(s.r.2) #Populationsparameter

s.r.sample.50 <- sample(s.r.2, 50)
mean(s.r.sample.50)

s.r.sample.150 <-sample(s.r.2, 150)
mean(s.r.sample.150)

s.r.sample.500 <- sample(s.r.2, 500)
mean(s.r.sample.500)

s.r.sample.10000 <- sample(s.r.2, 10000)
mean(s.r.sample.10000)


par(mfrow=c(1,3))

plot(s.r.sample.50,
       col = "green4", 
       pch = 19)
abline(mean(s.r.sample.50),0,col = "red", cex = 2)
abline(mean(s.r.2),0,col = "red",lty=3, cex = 2)

plot(s.r.sample.150,
     col = "green3", 
     pch = 19)
abline(mean(s.r.sample.150),0,col = "red", cex = 2)
abline(mean(s.r.2),0,col = "red",lty=3, cex = 2)

plot(s.r.sample.500,
     col = "green", 
     pch = 19)
abline(mean(s.r.sample.500),0,col = "red", cex = 2)
abline(mean(s.r.2),0,col = "red",lty=3, cex = 2)


### Beispiel Pi, also bekanntem Parameter
# 4 ist quadratflaeche, da eine Seite = 2
# Kathetenquadrate ergeben das Hypothenusenquadrat, daher x^2 + y^2
# P(x^2 + y^2 < 1) = Flaeche_Kreis / Flaeche_Quadrat = pi/4
# Also: Flaeche_Kreis / Flaeche_Quadrat * 4 = pi

set.seed(1335)

pi

s1 <- runif(10,-1,1)
t1 <- runif(10,-1,1)
4*length(s1[s1^2 + t1^2 <= 1]) / 10 #Es werden nur Werte ausgewaehlt, deren Quadratsumme maximal 1 ergibt

plot(s1[s1^2 + t1^2 <= 1],t1[s1^2 + t1^2 <= 1],col = "green", pch = 19, xlim = c(-1,1), ylim = c(-1,1))
points(s1[s1^2 + t1^2 > 1],t1[s1^2 + t1^2 > 1],col = "red", pch = 19)


s2 <- runif(100,-1,1)
t2 <- runif(100,-1,1)
4*length(s2[(s2+t2)^2<=1])/100

plot(s2[s2^2 + t2^2<=1],t2[s2^2 + t2^2 <= 1],col = "green", pch = 19, xlim = c(-1,1), ylim = c(-1,1))
points(s2[s2^2 + t2^2 > 1],t2[s2^2 + t2^2 > 1],col = "red", pch = 19)


s3 <- runif(1000,-1,1)
t3 <- runif(1000,-1,1)
4*length(s3[(s3^2+t3^2)<=1])/1000



plot(s3[s3^2+t3^2<=1],t3[s3^2+t3^2<=1],col = "green", pch = 19)
points(s3[s3^2+t3^2>=1],t3[s3^2+t3^2>=1],col = "red", pch = 19)


### Verteilungen

n <- 1000

# Normalverteilung


nv <- rnorm(n = n, mean = 100, sd = 15)
nv

# Numerisch-deskriptive Ueberpruefung

summary(nv) #Gibt deskriptive Zusammenfassung aus
density(nv) #Gibt Wahrscheinlichkeitsdichte aus
ecdf(nv) #Gibt empirische kummulative Dichtefunktion aus

# Graphisch-deskripitive Ueberpruefung

par(mfrow = c(3,1))

hist(nv) #Histogramm
plot(density(nv), col = "darkred", main = "f(X)") #Die Density Funktion ist vor allem graphisch informativ
plot(ecdf(nv), col = "darkorchid4", main = "F(X)") #Die ECDF Funktion ist ebenso vor allem graphisch informativ


# Binomialverteilung

bv <- rbinom(n = n, size = 1, prob = .3) #Size gibt die Anzahl der Ziehungen an, Prob die Wahrscheinlichkeit eines guenstigen Ergebnisses
bv

# Numerisch-deskriptive Ueberpruefung

summary(bv) 
table(bv) #Gibt Haeufigkeiten aus

# Graphisch-deskripitive Ueberpruefung

par(mfrow = c(1,1))
barplot(table(bv), col = c("darkred","darkorchid4"), main = "Results") #Table kann auch in der Plotfunktion verwendet werden


### Verteilungen von Parametern erstellen

### Replicate

r <- 1000 #Anzahl Replicationen Festlegen
par(mfrow = c(1,1))

# Normalverteilung

nv.means <- replicate(r, mean(rnorm(n,100,15)))
plot(density(nv.means))
abline(v = mean(nv.means), col = "darkred", cex = 2)

mean(nv.means)
sd(nv.means)

# Binomialverteilung

bv.means <- replicate(r, mean(rbinom(n,1,.3)))
plot(density(bv.means))
abline(v = mean(bv.means), col = "darkred", cex = 2)

mean(bv.means)
sd(bv.means)


### Funktionen zur Vereinfachung der Schaetzung von Verteilungsparametern

nv.means.fun <- function(reps = 1000, mean = 100, sd = 15){
  nv.ms <- replicate(reps, mean(rnorm(1000,mean,sd)))
  plot(density(nv.ms), xlab = c(paste("MW =", mean(nv.ms)),paste("sd =", sd(nv.ms))))
  abline(v = mean(nv.ms))
  return(nv.ms)
}


nv.means.out <- nv.means.fun(r,100,15)


bv.means.fun <- function(reps = 1000, k = 1, p = .5){
  bv.ms <- replicate(reps, mean(rbinom(1000, k, p)))
  plot(density(bv.ms), xlab = c(paste("MW =", mean(bv.ms)),paste("sd =", sd(bv.ms))))
  abline(v = mean(bv.ms))  
  return(bv.ms)
}


bv.means.out <- bv.means.fun(r,1,.3)


### Bootstrap

reps.bs = 10000


# Normalverteilung Bootsrap

strap.nv <- rep(NA,reps.bs) #Vektor kreieren, in den die Bootstrap-Ergebnisse eingetragen werden

for (i in 1 : reps.bs){
  strap.nv[i] <- mean(sample(nv, length(nv), replace = T)) #Ziehen mit Zuruecklegen als Prinzip des Bootstraps
}

summary(strap.nv)
plot(density(strap.nv))


# Binomialverteilung

strap.bv <- rep(NA,reps.bs)

for (i in 1 : reps.bs){
  strap.bv[i] <- mean(sample(bv, length(bv), replace = T)) #Ziehen mit Zuruecklegen als Prinzip des Bootstraps
}


summary(strap.bv)
plot(density(strap.bv))

### Manipulation Check
# Der Standdardfehler des Parameters sollte fuer alle Simulationen
# gleich sein und dem Standardfehler des wahren Wertes aehneln.

# Normalverteilung

sd(strap.nv)
sd(nv.means.out)
sd(nv)/sqrt(length(nv)) ### SE of M: sd/sqrt(n)
15/sqrt(1000)

# Binomialverteilung

sd(strap.bv)
sd(bv.means.out)
sqrt(mean(bv)*(1-mean(bv))/length(bv)) ### SE of p: sqrt(k*p*(1-p)/n)
sqrt(.3*.7/1000)

### sapply und der zentrale Grenzwertsatz

mean.frame  <- data.frame(Norm = nv.means.out, 
                          Binom = bv.means.out)

mean.frame #Mittelwerte sind in zwei Spalten angeordnet

apply(mean.frame,2,sd) #apply wendet Befehl fuer jede Zeile (1) oder Spalte (2) an
ms <- sapply(mean.frame,sd) #Vereinfachte Form von apply, die auch fuer Listen funktioniert und keine Listen ausgibt (ungleich lapply)


draws.small <- sapply(1:100,function(x) mean(rnorm((x)))) #kreirt 100 Mittelwerte normalverteilter Samples
plot(draws.small) #Je mehr Werte genutzt wurden, desto naeher kommen die Ziehungen an den wahren Wert, also null
plot(density(draws.small))

draws.big <- sapply(501:600,function(x) mean(rnorm((x))))
plot(draws.big) #Die Werte sind alle nahe null und die Unterschiede werden kleiner
plot(density(draws.big))

draws.combined <- c(draws.small, draws.big)
plot(draws.combined) #Plot fuer die kombinierten Werte
plot(density(draws.combined))

draws.large <- sapply(1:600,function(x) mean(rnorm((x))))
plot(draws.large) # Illustration bei Stichproben der Groesse 1 bis 600
plot(density(draws.large))

draws.large.sd <- sapply(2:600,function(x) sd(rnorm((x))))
plot(draws.large.sd) #Auch die Standardabweichung kann approximiert werden
plot(density(draws.large.sd))

draws.large.se <- sapply(2:600,function(x) sd(rnorm((x)))/sqrt(x))
plot(draws.large.se) #Der Standardfehler des Mittelwerts sinkt mit steigender Stichprobengroesse
plot(density(draws.large.se))


### Seed setzen

set.seed(pi)


### Korrelierte Daten simulieren
# Eine Pearson-PM-Korrelation besagt, dass die Werte einer Variablen
# von den Werten einer anderen mit einer Addition, einer
# Multiplikation und einer normalverteilten Abweichung zu erklaeren sind.
# Bei der Simulation korrelierter Daten kann diesem Prinzip nachgegangen
# werden.


x.simple <- rnorm(50,2,1)
y.simple <- x.simple + rnorm(50,3,1)
cor(x.simple,y.simple)
x.simple.2 <- y.simple + rnorm(50,3,1)
cor(x.simple,x.simple.2)


summary(lm(y.simple ~ x.simple + x.simple.2)) #Beide Variablen korrelieren, aber intercept nicht Abweichung standardnormalverteilt


# Daten mit multivariater NV simulieren

require(mvtnorm) #Das Paket "mvtnorm" kann genutzt werden, um multivariat normalverteilte Daten zu simulieren

n.simple <- 50 #Anzahl der Tupel
mu.simple <- c(2,3) #Mittlwerte der beiden Variablen
sigma.simple <- c(1,1) #Varianzen der beiden Variablen
rho <- .7 #Kovarianz der Variablen
covar.simple <- matrix(c(1,rho,rho,1),nrow=2) #Zusammensetzen der Korrelationsmatrix

#Alternativ
covar.simple.a <- matrix(c(rep(rho,4)),nrow=2)
diag(covar.simple.a) <- rep(1,2)
covar.simple

covar.simple #Durch die SD von 1 ist die Kovarianzmatrix gleich der Korrelationsmatrix
X.simple <- rmvnorm(n.simple,mu.simple,covar.simple) #Simulation der Daten
cor(X.simple)
cov(X.simple) - covar.simple #Vergleich der Stichprobenkovarianzmatrix mit der wahren Kovarianzmatrix
cor(X.simple) - covar.simple #Vergleich der Korrelation

### Minimalabweichung der Korrelation garantieren

tol <- .02 #Maximalabweichung voreinstellen
reps.need <- 1

# Die Schaetzung wird so lange wiederholt, bis die Abweichung der simulierten
# Daten die festgelegte Maximalabweichung nicht uebertrifft

repeat {
  X.rep <- rmvnorm(n.simple,mu.simple,covar.simple)
  co <- cor(X.rep) [1,2]
  if (co >= rho - tol && co <= rho + tol)
    break
  else
    reps.need <- reps.need + 1
}

cat(reps.need,
    " Iterationen wurden zur Erzeugung von X gebraucht\n")

cor(X.rep)
cor(X.rep) - covar.simple


### Seed setzen

set.seed(pi*2)


### Lineare Regression mit multivariater NV simulieren

# Daten simulieren

n <- 50
mu <- c(1,3,4)
sigma <- c(1,1,1)
covar <- matrix(c(1,.5,.4,.5,1,.2,.4,.2,1),nrow=3)


X <- rmvnorm(n,mu,covar)
cor(X)

### Daten mit mindestanforderungen an empirischer Korrelation

tolerance <- .0001 #Tolleranz fuer Abweichunssumme der Kovarianzen festlegen

reps.needed <- 1
sum(cor(X) - covar)

# Diesmal muss die Summe der Korrelationen als Abweichungskriterium dienen
# Alternativ koennte auch fuer jede Korrelation eine Bedinung gestellt werden

repeat {
  X <- rmvnorm(n,mu,covar)
  if (sum(cor(X)) >= sum(covar - tolerance) && sum(cor(X)) <= sum(covar + tolerance))
    break
  else
    reps.needed <- reps.needed + 1
}

cat(reps.needed," Iterationen wurden zur Erzeugung von X gebraucht\n")

cor(X) #Korrelation entspricht den Bedinungen

summary(lm(X[,1]~X[,2] + X[,3])) #Modell entspricht den Erwartungen (da die Mittelwerte Unterschiedlich sind, ist auch der Intercept signifikant)


### Komplexeres


#Beispiel aus Fan & Fan (2005): Using SAS for Monte Carlo
#Simulation Research in SEM. STRUCTURAL EQUATION MODELING, 12(2), 299?333


#Die 8 Populationsmatrizen (plus Identitaetsmatrix) werden definiert:

LX <- matrix(c(1.00, 0.50),nrow=2) # Faktorladungs-Lambda-x-Matrix (Ladungsmatrix von *exogenen* latenten Variablen)
LY <- matrix(c(1.00, 0.95, 0.00, 0.00, 0.00, 0.00, 1.00, 0.90),ncol=2) # Faktorladungs-Lambda-y-Matrix (Ladungsmatrix von *endogenen* latenten Variablen)
GA <- matrix(c(-0.60, -0.25),nrow=2) # Gamma-Matrix: Strukturelle Parameter, die latente *endogenen* mit der exogenen Variablen verbindet
PH <- 7.00 # FaktorKovarianzmatrix Phi. Ist in diesem Beispiel ein Skalar (da LX nur eine exogene LV, also nur deren Varianz), sonst eine Matrix
PS <- matrix(c(5.00, 0.00, 0.00, 4.00), nrow=2) # Psi: Fehlerkovarianzen zwischen Residuen latenter Variablen. Keine Fehlerkovarianzen in dem Beispiel
TD <- matrix(c(3.00, 0.00, 0.00, 2.50),nrow=2) # Theta delta: Fehlerkovarianzen zwischen den Residuen der x-Variablen
TE <- matrix(c(4.75, 0.00, 1.60, 0.00, 0.00, 2.50, 0.00, 0.30,
               1.60, 0.00, 4.50, 0.00, 0.00, 0.30, 0.00, 3.00),nrow=4) # Theta sigma: Fehlerkovarianzen zwischen den Residuen der y-Variablen
B <- matrix(c(0.00, 0.60, 0.00, 0.00),nrow=2) # Strukturelle parameter, die die latenten *endogenen* Variblen verbindet. Eta_1 hat hier Regressionsgewicht auf Eta_2
I <- matrix(c(1, 0, 0, 1),nrow=2) # Identitaetsmatrix


# Die vier Quadranten (Kovarianzmatrizen) werden erstellt (Equation 2)
COVY <- LY%*%(solve(I-B))%*%(GA%*%PH%*%(t(GA))+PS)%*%(solve(I-(t(B))))%*%(t(LY))+TE
COVX <- LX%*%PH%*%(t(LX)) + TD
COVYX <- LY%*%(solve(I-B))%*%GA%*%PH%*%(t(LX))
COVXY <- LX%*%PH%*%(t(GA))%*%(solve(I-(t(B))))%*%t(LY)

# Die vier Quadranten werden jetzt zur (Gesamt-) Populationskovarianzmatrix zusammengef?gt
COV <- matrix(c(rbind(COVX,COVYX),rbind(COVXY,COVY)),ncol=6)
COV 

#Nur zum Check: Korrelationsmatrix ausgeben lassen:
R.1  <-  solve(diag(sqrt(diag(COV)), nrow = 6))%*%COV%*%solve(diag(sqrt(diag(COV)), nrow = 6)) #also COV teilen durch Standardabweichungen 
#d.h. prae- und postmultiplizieren mit Inverse einer Diagonalmatrix mit der Wurzel der Hauptdiagonalen von COV:

# Schrittweise
diag.vector <- sqrt(diag(COV)) #Extrahiert Hauptdiagonale von COV als Vektor und zieht Wurzel
S.2 <- diag(diag.vector) #Definiert eine Diagonal-MATRIX mit Hilfe des Vektors diag.vector
S.2
R.2 <- solve(S.2)%*%COV%*%solve(S.2)
R.2

R <- cov2cor(COV)

R == R.1 && R.1 == R.2

# Naechste Schritte: Package "mvtnorm" nutzen, um Daten zu obiger Kovarianzmatrix zu generieren
# und Schleife definieren fuer Anzahl zu replizierender Datenmatrizen.
## HINWEIS: Das Sampeln geht mit MASS-Package ggf. leichter, weil die Anzahl der samples direkt (nicht ueber Schleife) spezifiziert werden kann.
## Siehe mvrnorm-Befehl in MASS-Dokumentation. Allerdings werden dann Individualdaten generiert, die dann ueber lavaan analysiert werden koennen, aber ist nat?rlich
## dann nicht sehr ressourcenschonend

sample.size <- 500
replication <- 5

### Generierung der Daten:


# Mit Schleife
set.seed(6787890)

data.list <- list(NA)

for(i in 1 : replication){
  data.list[[i]]  <- as.data.frame(rmvnorm(n = sample.size, mean = rep(0,nrow(COV)), sigma=COV))
  names(data.list[[i]]) <- paste(c("x1","x2","y1","y2","y3","y4"))
}


lapply(data.list, names)
lapply(data.list, cov)


## Alle Kov.-Matrizen mit lavaan analysieren:

require(lavaan) # Lavaan (LAtent VAriable ANalysis) wird fuer die Schaetzung von Strukturgleichungsmodellen verwendet


### Modell in Lavaan

model.correct <- '
LX =~ 1 * x1 + .5 * x2
LY1 =~ 1 * y1 + .95 * y2
LY2 =~ 1 * y3 + .9 * y4
LX =~ -.6 * LY1 + -.25 * LY2
LY1 ~~ .6 * LY2
y1 ~~ 1.6 * y3
y2 ~~ .3 * y4
LX ~~ 7 * LX
LY1 ~~ 5 * LY1
LY2 ~~ 4 * LY2
x1 ~~ 3 * x1
x2 ~~ 4 * x2
y1 ~~ 4.75 * y1
y2 ~~ 2.5 * y2
y3 ~~ 4.5 * y3
y4 ~~ 3 * y4
'
summary(sem(model.correct, data.list[[2]], orthogonal = T), fit.measures = T) #Kann nicht geschaetzt werden

# Ein frei zu schaetzendes Modell
#Eine Varianz muss zum Konvergieren spezifiziert werden

model.free <- '
LX =~ x1 + x2
LY1 =~ y1 + y2
LY2 =~ y3 + y4
LX =~ LY1 + LY2
LY1 =~ LY2
y1 ~~ y3
y2 ~~ y4
LX ~~ 7 * LX 
'

summary(sem(model.free, data.list[[2]], orthogonal = T), fit.measures = T)


# Ein frei zu schaetzendes Modell
# Kovarianzen Y1-Y3, Y2-Y4 und LY1-LY2 null gesetzt
# Eine Varianz muss zum Konvergieren spezifiziert werden

model.misspecified <- '
LX =~ x1 + x2
LY1 =~ y1 + y2
LY2 =~ y3 + y4
LX =~ LY1 + LY2 
LX ~~ 7 * LX 
'

summary(sem(model.misspecified, data.list[[2]], orthogonal = T), fit.measures = T)


### Eine Verteilung der Parameter und Fit-Indizes kann mit einer Schleifenfunktion geschaetzt werden

estim.models <- list(NA)
global.fit <- list(NA)

for (i in 1:replication) {
  
    estim.models <- sem(model.misspecified,data=data.list[[i]], orthogonal = T) #Der sem() Befehl schaetzt ein SEM mit dem vorgegebenen Modell. Orthogonal = T setzt alle nicht-spezifizierten Kovarianzen im Strukturmodell null
    global.fit[[i]] <- fitMeasures(estim.models, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))  
    
    
}
  
global.fit



### Simuliert wird ueber die Population, die anhand eines Modells erstellt wird.
### Danach wird ein Modell mit Bespielsweise inkorrekt spezifizierten Ladungen anhand
### dieser Population berechnet.


### Die Schaetzung der Parameter als Funktion



model.est <- function(model = NULL, data = NULL, r = length(data)){
  
  fit <- data.frame(chi = rep(NA,r), df = rep(NA,r), pvalue = rep(NA,r), cfi  = rep(NA,r),rmsea = rep(NA,r), srmr = rep(NA,r))
  estim.models <- list(NA)
  
  for (i in 1:r) {
    
    estim.models <- cfa(model, data=data[[i]], orthogonal = T)
    fit[i,1:6] <- fitMeasures(estim.models, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))  
    
  }
  
  return(fit)
  
}

values <- model.est(model = model.misspecified,data = data.list, r = replication)
values

sapply(values,mean)
sapply(values,sd)
