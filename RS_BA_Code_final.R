#Read excel library
library("readxl")

#set working directory 
setwd("~/Desktop/BA BWL")

#lädt die Einkommensdaten
Einkommen_BA_Daten <- read_excel("Einkommen_BA_Daten.xlsx")

#öffnet die Datei in einem Fenster
View(Einkommen_BA_Daten) 

#benennt die Datei in myData um
myData = read_xlsx("Einkommen_BA_Daten.xlsx") 

#zeigt die Häufigkeit der jeweiligen Ausprägungen an
table(myData$GESCHL)
table(myData$ABSCHLUSS)

#Descriptive Statistiken zum Einkommen:
summary(myData$NETTO)
var(myData$NETTO, na.rm = TRUE)
sd(myData$NETTO, na.rm = TRUE)

#Kontingenztabelle
table(myData$GESCHL, myData$ABSCHLUSS)
propTable = prop.table(table(myData$GESCHL, myData$ABSCHLUSS),1)

#Barplot für die Proportionen
myData$GESCHL[myData$GESCHL == "MAENNLICH"] <-"MALE"
myData$GESCHL[myData$GESCHL == "WEIBLICH"] <-"FEMALE"
myData$ABSCHLUSS[myData$ABSCHLUSS == "1"] <- "HEEQ"
myData$ABSCHLUSS[myData$ABSCHLUSS == "0"] <- "No HEEQ"
tableForPlot = table(myData$ABSCHLUSS, myData$GESCHL)
tablePropForPlot <- prop.table(tableForPlot, 2)
barplot(tablePropForPlot [1:2,2:1], 
        legend.text = TRUE, beside = TRUE, 
        main = "Proportions of Men and Women with and without HEEQ", 
        xlab = "Gender", ylab = "Proportions")
#HEEQ und No HEEQ wieder zurück in 1 und 0 codieren
myData$ABSCHLUSS[myData$ABSCHLUSS == "HEEQ"] <- "1"
myData$ABSCHLUSS[myData$ABSCHLUSS == "No HEEQ"] <- "0"


#Ab hier der Code für die frequentistische Inferenz im ersten Beispiel
#Für den Null distribution plot:
propTable = prop.table(table(myData$GESCHL, myData$ABSCHLUSS),1)
diffProp <- propTable["1", "MALE"] - propTable["1", "FEMALE"]
##erstellt ein sample für Abschluss und gibt dies als Value wieder
tempHEEQ <- sample(myData$ABSCHLUSS,         
                   size = nrow(myData),
                   replace = TRUE)
#erstellt Tabelle mit der gasampleten Variable
tempTable <- table(tempHEEQ, myData$GESCHL)
##gibt die relativen Häufigkeiten mit der gesampleten Variable aus
tempRates <- prop.table(tempTable, 2)
#Differenz der beiden Proportionen nach Sampling
tempRates["1", "MALE"] -      
  tempRates["1", "FEMALE"]
#erstellt die null distribution
tableCountsNull <- numeric()

for(i in 1:10000){
  tempHEEQ <- sample(myData$ABSCHLUSS,
                           size = nrow(myData),
                           replace = TRUE)
  tempTable <- table(tempHEEQ, myData$GESCHL)
  tempRates <- prop.table(tempTable, 2)
  tableCountsNull[i] <- tempRates["1", "MALE"] -
    tempRates["1", "FEMALE"]
}
#plottet die null distribution
hist(tableCountsNull,
     main = "Null distribution for difference in proportions",
     xlab = "Difference in proportions")
#fügt Linien hinzu, die die in der Stichprobe beobachtete Differenz markieren
abline(v = c(diffProp,
             -diffProp), col = "blue", lwd = 3, lty = 2)
#gibt den p-Wert wieder
pMoreExtreme <- mean(
  tableCountsNull >=  diffProp |
    tableCountsNull <= -diffProp)


#Sampling distributions der Proportionen:
#speichert die Anzahl der Männer und Frauen mit und ohne HEEQ als Variable
maleResponses = c(rep("1",642),
                  rep("0",384))
femaleResponses = c(rep("1", 306),
                    rep("0",  239))
#speichert die Proportionen der Männer mit HEEQ und Frauen mit HEEQ als Variable
propMale <- mean(maleResponses == "1")
propFemale <- mean(femaleResponses == "1")
#bootstrapping/Erstellung der sampling distribution der männlichen Proportion
MaleProp <- numeric()
for(i in 1:10000){
  tempMaleSample <- sample(maleResponses,
                           size = length(maleResponses),
                           replace =  TRUE) 
  MaleProp[i] <- mean(tempMaleSample == "1")
}
#gibt das Histogramm aus
hist(MaleProp, breaks = 40, freq = FALSE, 
     main = "Sampling Distribution of the Proportion of Men with HEEQ", 
     xlab = "Proportion")
#gibt den Standardfehler aus
sd(MaleProp)
#markiert das KI
abline(v = propMale + c(-2,2)*sd(MaleProp),
       col = "blue", lwd = 3, lty = 2)

#Das gleiche nochmal für den Anteil der Frauen
FemaleProp <- numeric()
for(i in 1:10000){
  tempFemaleSample <- sample(femaleResponses,
                             size = length(femaleResponses),
                             replace =  TRUE) 
  FemaleProp[i] <- mean(tempFemaleSample == "1")
}
#gibt das Histogramm aus
hist(FemaleProp, breaks = 40, freq = FALSE, 
     main = "Sampling Distribution of the Proportion of Women with HEEQ", 
     xlab = "Proportion")
#gibt den Standardfehler aus
sd(FemaleProp)
#markiert das KI
abline(v = propFemale + c(-2,2)*sd(FemaleProp),
       col = "blue", lwd = 3, lty = 2)

#Sampling distribution für die Differenz
diffProp = numeric()
for(i in 1:10000){
  tempMaleSample <- sample(maleResponses, replace = TRUE)
  tempFemaleSample <- sample(femaleResponses, replace = TRUE)
  
  tempPropMale <-   mean(tempMaleSample   == "1")
  tempPropFemale <- mean(tempFemaleSample == "1")
  
  diffProp[i] <- tempPropMale - tempPropFemale
}
#Das Histogramm
hist(diffProp, freq = FALSE, breaks = 40, 
     main = "Sampling Distribution of the Difference in Proportions", 
     xlab = "Difference in Proportions")

#Berechnung des Standardfehlers 
mySE = sqrt(
  ( propMale * (1 - propMale) / length(maleResponses)) +
    ( propFemale * (1 - propFemale) / length(femaleResponses))
)
#Berechnung des Margin of Errors
myME <- qnorm( c(0.025,0.975) ) * mySE
#Einzeichnung des KI
abline(v = (propMale - propFemale) + myME,
       col = "blue", lwd = 3, lty = 2)



#Ab hier der Code für Bayesch'sche Inferenz im ersten Beispiel
#Quelle: https://sites.google.com/site/doingbayesiandataanalysis/software-installation?authuser=0
#Die Skripte sind auf der Webseite in der zip Datei: DBDA2Eprograms.zip

#Lädt die Pakete aus dem Buch von Kruschke
source("DBDA2E-utilities.R")
#Lädt die BernBeta Funktion aus dem Buch von Kruschke
source("BernBeta.R")
#Spezifiziert den Prior und die Daten und gibt sowohl diese als auch den posterior aus
#Der Code für Figur 5 mit uninformiertem beta prior
#Für die Proportion der Männer mit HEEQ:
a =  1 # Convert to beta shape parameter a.
b =  1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
# Specify the data:
N = 1026 # The total number of flips.
z = 642 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0’s and 1’s.
openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" ,
                      showCentTend="Mode" , showHDI=FALSE , showpD=FALSE )
saveGraph(file="PropMenEx1",type="png")
#Für die Proportion der Frauen mit HEEQ:
a =  1 # Convert to beta shape parameter a.
b =  1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
# Specify the data:
N = 545 # The total number of flips.
z = 306 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0’s and 1’s.
openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" ,
                      showCentTend="Mode" , showHDI=FALSE , showpD=FALSE )
saveGraph(file="PropWomenEx1",type="png")

#Als nächstes dasselbe mit stark informiertem Prior (Figur 6)
#Für die Proportion der Männer mit HEEQ:
t = 0.391 # Specify the prior mode.
n = 223800 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
# Specify the data:
N = 1026 # The total number of flips.
z = 642 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0’s and 1’s.
openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" ,
                      showCentTend="Mode" , showHDI=FALSE , showpD=FALSE )
saveGraph(file="PropMenEx2",type="png")
#Für die Proportion der Frauen mit HEEQ:
t = 0.409 # Specify the prior mode.
n = 195140 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
# Specify the data:
N = 545 # The total number of flips.
z = 306 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0’s and 1’s.
openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" ,
                      showCentTend="Mode" , showHDI=FALSE , showpD=FALSE )
saveGraph(file="PropWomenEx2",type="png")

#Als drittes und letztes Beispiel mit moderatem prior (Figur 7)
#Für die Proportion der Männer mit HEEQ:
t = 0.391 # Specify the prior mode.
n = 815 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
# Specify the data:
N = 1026 # The total number of flips.
z = 642 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0’s and 1’s.
openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" ,
                      showCentTend="Mode" , showHDI=FALSE , showpD=FALSE )
saveGraph(file="PropMenEx3",type="png")
#Für die Proportion der Frauen mit HEEQ:
t = 0.409 # Specify the prior mode.
n = 850 # Specify the effective prior sample size.
a = t*(n-2) + 1 # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1 # Convert to beta shape parameter b.
Prior = c(a,b) # Specify Prior as vector with the two shape parameters.
# Specify the data:
N = 545 # The total number of flips.
z = 306 # The number of heads.
Data = c(rep(0,N-z),rep(1,z)) # Convert N and z into vector of 0’s and 1’s.
openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" ,
                      showCentTend="Mode" , showHDI=FALSE , showpD=FALSE )
saveGraph(file="PropWomenEx3",type="png")


#Ab hier der Code für das MCMC Sampling und Figur 9
#Lädt die Funktionen, die dafür nötig sind ein MCMC sampling durchzuführen, die Ergebnisse anzeigen zu lassen und die Ergebnisse zu plotten
source("Jags-Ydich-XnomSsubj-MbernBeta.R")
#Nachfolgend das komplette Skript:
# Jags-Ydich-XnomSsubj-Mbernbeta.R 
# Accompanies the book:
#   Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
#   A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier.
source("DBDA2E-utilities.R")
#===============================================================================

genMCMC = function( data , numSavedSteps=50000 , saveName=NULL ) { 
  require(rjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  # N.B.: This function expects the data to be a data frame, 
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  #Ich habe die Variable Geschlecht erst in numerische Werte überführt
  myData$Gender_num <- as.character(myData$GESCHL) 
  myData$Gender_num[myData$GESCHL=="MAENNLICH"]<-1
  myData$Gender_num[myData$GESCHL=="WEIBLICH"]<-0
  #Dann hier die Daten eingetragen
  y = myData$ABSCHLUSS 
  s = myData$Gender_num 
  #Nachfolgend habe ich nichts mehr anpassen müssen bis auf den Namen unter dem ich das spezifizierte Modell gespeichert habe (Zeile 304)
  # Do some checking that data make sense:
  if ( any( y!=0 & y!=1 ) ) { stop("All y values must be 0 or 1.") }
  Ntotal = length(y)
  Nsubj = length(unique(s))
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    y = y ,
    s = s ,
    Ntotal = Ntotal ,
    Nsubj = NsubjcodaSamples[,"theta"] , main="theta" , xlab=bquote(theta)
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
    for ( i in 1:Ntotal ) {
      y[i] ~ dbern( theta[s[i]] )
    }
    for ( sIdx in 1:Nsubj ) {
      theta[sIdx] ~ dbeta( 1 , 1 ) # N.B.: 2,2 prior; change as appropriate. #Habe für eine bessere Vergleichbarkeit mit dem frequentistischen Ansatz den voreingestellten beta(2,2)prior in einen uniformen beta(1,1)prior geändert
    }
  }
  " # close quote for modelString
  writeLines( modelString , con="PropDiffModel.txt" ) #das Modell unter dem Namen gespeichert
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  # Option 1: Use single initial value for all chains:
  #  thetaInit = rep(0,Nsubj)
  #  for ( sIdx in 1:Nsubj ) { # for each subject
  #    includeRows = ( s == sIdx ) # identify rows of this subject
  #    yThisSubj = y[includeRows]  # extract data of this subject
  #    thetaInit[sIdx] = sum(yThisSubj)/length(yThisSubj) # proportion
  #  }
  #  initsList = list( theta=thetaInit )
  # Option 2: Use function that generates random values near MLE:
  initsList = function() {
    thetaInit = rep(0,Nsubj)
    for ( sIdx in 1:Nsubj ) { # for each subject
      includeRows = ( s == sIdx ) # identify rows of this subject
      yThisSubj = y[includeRows]  # extract data of this subject
      resampledY = sample( yThisSubj , replace=TRUE ) # resample
      thetaInit[sIdx] = sum(resampledY)/length(resampledY) 
    }
    thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
    return( list( theta=thetaInit ) )
  }
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "theta")     # The parameters to be monitored
  adaptSteps = 500             # Number of steps to adapt the samplers
  burnInSteps = 500            # Number of steps to burn-in the chains
  nChains = 4                  # nChains should be 2 or more for diagnostics 
  thinSteps = 1
  numSavedSteps=10000 #habe ich zur Vergleichbarkeit mit dem Bootstrapping auf 10000 gestellt
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( file = "PropDiffModel.txt" , data=dataList , inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function

#===============================================================================

smryMCMC = function(  codaSamples , compVal=0.5 , rope=NULL , 
                      compValDiff=0.0 , ropeDiff=NULL , saveName=NULL ) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  summaryInfo = NULL
  rowIdx = 0
  for ( tIdx in 1:Ntheta ) {
    parName = paste0("theta[",tIdx,"]")
    summaryInfo = rbind( summaryInfo , 
                         summarizePost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  for ( t1Idx in 1:(Ntheta-1) ) {
    for ( t2Idx in (t1Idx+1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      summaryInfo = rbind( summaryInfo , 
                           summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                                          compVal=compValDiff , ROPE=ropeDiff ) )
      rowIdx = rowIdx+1
      rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
    }
  }
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
}

#===============================================================================

plotMCMC = function( codaSamples , data , compVal=0.5 , rope=NULL , 
                     compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame, 
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  y = data$y
  s = as.numeric(data$s) # converts character to consecutive integer levels
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  openGraph(width=2.5*Ntheta,height=2.0*Ntheta)
  par( mfrow=c(Ntheta,Ntheta) )
  for ( t1Idx in 1:(Ntheta) ) {
    for ( t2Idx in (1):Ntheta ) {
      parName1 = paste0("theta[",t1Idx,"]")
      parName2 = paste0("theta[",t2Idx,"]")
      if ( t1Idx > t2Idx) {  
        # plot.new() # empty plot, advance to next
        par( mar=c(3.5,3.5,1,1) , mgp=c(2.0,0.7,0) )
        nToPlot = 700
        ptIdx = round(seq(1,chainLength,length=nToPlot))
        plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , cex.lab=1.75 ,
               xlab=parName2 , ylab=parName1 , col="skyblue" )
      } else if ( t1Idx == t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost( mcmcMat[,parName1] , cex.lab = 1.75 , 
                             compVal=compVal , ROPE=rope , cex.main=1.5 ,
                             xlab=parName1 , main="" )
        includeRows = ( s == t1Idx ) # identify rows of this subject in data
        dataPropor = sum(y[includeRows])/sum(includeRows) 
        points( dataPropor , 0 , pch="+" , col="red" , cex=3 )
      } else if ( t1Idx < t2Idx ) {
        par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) )
        postInfo = plotPost(mcmcMat[,parName1]-mcmcMat[,parName2] , cex.lab = 1.75 , 
                            compVal=compValDiff , ROPE=ropeDiff , cex.main=1.5 ,
                            xlab=paste0(parName1,"-",parName2) , main="" )
        includeRows1 = ( s == t1Idx ) # identify rows of this subject in data
        dataPropor1 = sum(y[includeRows1])/sum(includeRows1) 
        includeRows2 = ( s == t2Idx ) # identify rows of this subject in data
        dataPropor2 = sum(y[includeRows2])/sum(includeRows2) 
        points( dataPropor1-dataPropor2 , 0 , pch="+" , col="red" , cex=3 )
      }
    }
  }
  #-----------------------------------------------------------------------------  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Post",sep=""), type=saveType)
  }
}

#===============================================================================

#Erstellt ein MCMC sample, führt die Iteration dabei 10000 mal durch.
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 )
#Plottet die durch das MCMC sampling generierten Posterior distributions
plotMCMC( mcmcCoda , data=myData , compVal=NULL , compValDiff=0.0, rope = c(-0.05,0.05))#Die Rope habe ich so eingestellt


#Ab hier der Code für den Bayes factor im ersten Beispiel
#Quelle: https://bayesmodels.com/ 
#Die Skripte befinden sich in dem code.zip Ordner, den man dort herunterladen kann
#der komplette Code für den Bayes factor für den Proportionsunterschied:
library(R2jags)

# data:
s1 <- 642
s2 <- 306
n1 <- 1026
n2 <- 545

# Analytical Bayes factor:
log.BF01 <- lchoose(n1,s1) + lchoose(n2,s2) + log(n1+1) + log(n2+1) - lchoose((n1+n2),(s1+s2)) - log(n1+n2+1)
BF01 <- exp(log.BF01)

data  <- list("s1","s2","n1","n2") # to be passed on to JAGS

myinits <- list(
  list(theta1 = runif(1), theta2 = runif(1), theta1prior = runif(1), theta2prior = runif(1)),
  list(theta1 = runif(1), theta2 = runif(1), theta1prior = runif(1), theta2prior = runif(1)),
  list(theta1 = runif(1), theta2 = runif(1), theta1prior = runif(1), theta2prior = runif(1)))

parameters <- c("theta1", "theta2", "delta", "deltaprior")

# The following command calls JAGS with specific options.
# For a detailed description see the R2jags documentation.
samples <- jags(data, inits=myinits, parameters,
                model.file ="Pledgers_1.txt",
                n.chains=3, n.iter=10000, n.burnin=1000, n.thin=1, DIC=T)
# Now the values for delta and deltaprior are in the "samples" object, ready for inspection.

######################################################
# H1: delta is unrestricted
######################################################

# Collect posterior samples across all chains:
delta.posterior  <- samples$BUGSoutput$sims.list$delta      
delta.prior      <- samples$BUGSoutput$sims.list$deltaprior

#============ BFs based on logspline fit ===========================
library(polspline) # this package can be installed from within R
fit.prior     <- logspline(delta.prior, lbound=-1, ubound=1) # note the bounds.
fit.posterior <- logspline(delta.posterior, lbound=-1, ubound=1)

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior     <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior         <- dlogspline(0, fit.prior)     # based on the logspline fit
BF01          <- posterior/prior
# 1/BF01 
BF01          <- posterior # because we know the height of the prior equals 1 at delta = 0 
# 1/BF01

#============ Plot Prior and Posterior ================================
# Two plots, once globally and once zoomed-in

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
#======= Plot Prior and Posterior ======================
Nbreaks <- 20
y <- hist(delta.prior, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=1,
     xlim=c(-1,1), ylim=c(0,25), xlab=" ", ylab="Density", axes=F) 
axis(1, at = c(-1, -0.5, 0, 0.50, 1), lab=c("-1", "-0.5", "0", "0.5", "1"))
axis(2)
mtext(expression(delta), side=1, line = 2.8, cex=2)
par(new=T)
x <- hist(delta.posterior, Nbreaks, plot=F)
plot(c(x$breaks, max(x$breaks)), c(0,x$density,0), type="S", lwd=2, lty=2,
     xlim=c(-1,1), ylim=c(0,25), xlab=" ", ylab="Density", main ="Full Scale", axes=F) 
axis(1, at = c(-1, -0.5, 0, 0.50, 1), lab=c("-1", "-0.5", "0", "0.5", "1"))
axis(2)
#now bring in log spline density estimation:
par(new=T)
# plot the triangular prior:
lines(c(-1,0),c(0,1), lty=1, lwd=1)
lines(c(0,1),c(1,0), lty=1, lwd=1)
par(new=T)
plot(fit.posterior, ylim=c(0,25), xlim=c(-1,1), lty=1, lwd=1, axes=F)

text(-1, 20, labels = "Posterior", cex = 1.5, pos=4)
text(-1, 2, labels = "Prior", cex=1.5, pos=4)

######## Second plot, zoom in:
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
xmin <- -0.05
xmax <- 0.05
ymax <- 5
plot(0,0, ylim=c(0,ymax), xlim=c(xmin,xmax), lwd=2, lty=3, ylab="Density", xlab=" ", main="Zoomed in", axes=F, col="white") 
#white makes this invisible
axis(1, at = c(xmin, 0, xmax), lab=c(paste(xmin), "0", paste(xmax)))
axis(2)
mtext(expression(delta), side=1, line = 2.8, cex=2)
par(new=T)
plot(fit.posterior, ylim=c(0,ymax), xlim=c(xmin,xmax), lty=2, lwd=2, axes=F)
lines(c(-1,0),c(0,1), lty=1, lwd=2)
lines(c(0,1),c(1,0), lty=1, lwd=2)
points(0, 1, pch=19, cex=2)
points(0, dlogspline(0, fit.posterior),pch=19, cex=2)

text(-0.015, 4, labels = "Posterior", cex = 1.5, pos=4)
text(0.01, 1.3, labels = "Prior", cex=1.5, pos=4)



#Ab hier die deskriptiven Plots für das zweite Beispiel
#Erst einmal alle fehlenden Werte raus:
myDataEx2 = na.omit(myData)
#Aufteilung der Einkommensdaten in die Gruppe mit und die Gruppe ohne HEEQ
Data2Groups = split(x = myDataEx2$NETTO, f = myDataEx2$ABSCHLUSS)
#Erstellt für jede Gruppe ein Data frame
IncomeNoHEEQ=data.frame(Data2Groups[["0"]])
IncomeHEEQ=data.frame(Data2Groups[["1"]])
##Histogramme des Einkommens beider Gruppen:
#Gruppe mit HEEQ (Figur 11)
hist(IncomeHEEQ$Data2Groups...2..., 
     breaks = 40, xlab = "Income of Participants with HEEQ", 
     main = "Histrogram of Income with HEEQ" )
#Gruppe ohne HEEQ mit allen Werten (Figur 13)
hist(IncomeNoHEEQ$Data2Groups...1...[GroupIncomeNoHEEQ<60000], 
     breaks = 40, xlab = "Income of Participants without HEEQ", 
     main = "Histrogram of Income without HEEQ" )
#Gruppe ohne HEEQ ohne den extremen Ausreißer (Figur 14)
hist(IncomeNoHEEQ$Data2Groups...1...[GroupIncomeNoHEEQ<60000], 
     breaks = 40, xlab = "Income of Participants without HEEQ", 
     main = "Histrogram of Income without HEEQ 
                      (Outlier with x = 60000 excluded)" )
#Die Boxplots für beide Gruppen (Figur 12 & 15)
boxplot(IncomeHEEQ, xlab = "Box Plot: Income HEEQ")
boxplot(IncomeNoHEEQ, xlab = "Box Plot: Income No HEEQ" )
#Erstellt Vektoren für das Einkommen beider Gruppen_
IncomeNoHEEQ=Data2Groups[["0"]]
IncomeHEEQ=Data2Groups[["1"]]
#Schätzung von Populationsmittelwert, Standardabweichung und Freiheitsgrade mittels t-Verteilung
IncomeHEEQEst <- fitdistr(IncomeHEEQ,"t")
IncomeNoHEEQEst <- fitdistr(IncomeNoHEEQ,"t")

#Ab hier die Sampling distributions für das Einkommen (Figur 16)
#Für das Einkommen derjenigen mit HEEQ:
sample_means = rep(NA, 10000)
for(i in 1:10000){
  sample_meansHEEQ[i] = mean(abs(rt(767, df=3.38)*795.54 + 2030.05))
}
hist(sample_means[sample_means>1800], 
     breaks = 40, xlab = "Sample Means", 
     main = "Sampling Distribution Means:
     Income with HEEQ")
#Für das Einkommen derjenigen ohne HEEQ:
sample_meansNoHEEQ = rep(NA, 10000)
for(i in 1:10000){
  sample_meansNoHEEQ[i] = mean(abs(rt(506, df=2.73)*548.49 + 1569.43))
}
hist(sample_meansNoHEEQ[sample_meansNoHEEQ>1300], 
     breaks = 40, xlab = "Sample Means", 
     main = "Sampling Distribution Means:
     Income without HEEQ")
#Für den Mittelwertsunterschied:

sample_meansDiff = rep(NA, 10000)
for(i in 1:10000){
  sample_meansDiff[i] = mean(abs(rt(767, df=3.38)*795.54 + 2030.05))-mean(abs(rt(506, df=2.73)*548.49 + 1569.43))
}
hist(sample_meansDiff, breaks = 40, xlab = "Sample Means", main = "Sampling Distribution for the Difference of Means")
abline(v = mean(sample_meansDiff) + c(-1.96,1.96)*sd(sample_meansDiff),col = "blue", lwd = 3, lty = 2)
#Ab hier der Code für den t.Test 
#Ziehung von Stichproben aus der t-Verteilung mit den vorher robust geschätzten Parameterwerten
RobustIncomeHEEQ = abs(rt(767, df=3.38)*795.54 + 2030.05)
RobustIncomeNoHEEQ = abs(rt(506, df=2.73)*548.49 + 1569.43)
#Der t-Test:
t.test(RobustIncomeHEEQ, RobustIncomeNoHEEQ)
#Der F-Test:
#Erstellt einen Vektor, der die Werte beider Gruppen beinhaltet
AllGroup = c(RobustIncomeHEEQ, RobustIncomeNoHEEQ)
#Erstellt eine Liste mit der Anzahl aller Daten und ihrer jeweiligen Zuordnung zur Gruppe 1 oder 2
groupList <- as.factor(c(rep(1, length(RobustIncomeHEEQ)), rep(2, length(RobustIncomeNoHEEQ))))
#Der Levene Test:
leveneTest(AllGroup, groupList, center = mean)



#Ab hier die Bayes Analyse für das Einkommen
#Verwendet wurde wieder ein Skript von Kruschke aus der oben genannten Quelle
source("DBDA2E-utilities.R")
#Generierung der MCMC Samples
mcmcCoda= genMCMC( datFrm = myData, yName="myData$NETTO" , 
                   xName = "myData$ABSCHLUSS" ,
                   saveName=NULL , numSavedSteps=50000 , thinSteps=1 ,
                   runjagsMethod=runjagsMethodDefault , 
                   nChains=nChainsDefault )
#Plotten der Ergebnisse
plotMCMC ( mcmcCoda , datFrm = myData , yName="myData$NETTO" , 
           xName="myData$ABSCHLUSS" ,
           RopeMuDiff=c(-0.1,0.1) , RopeSdDiff=c(-0.1,0.1) , RopeEff=c(-0.1,0.1) , #von mir festgelegt
           showCurve=FALSE , pairsPlot=TRUE ,
           saveName= "Posterior Distribution for Income" , saveType="png" )
#Speichert den erstellten Plot mit einer Höhe von 1800ppi, einer Breite von 1800ppi und einer Auflösung von 150ppi 
ppi <- 300
dev.print(device = png, filename = 'PosteriorIncomeDiff.png', width=6*ppi, height=6*ppi, res=150)
dev.off()
#Nachfolgend das komplette Skript: 
#Bis auf die Daten und den Namen für die Speicherung der Modellspezifikationen entspricht alles dem Original
#Jags-Ymet-Xnom2grp-MrobustHet.R
#===============================================================================

genMCMC = function( datFrm, yName="y" , xName="x" ,
                    saveName=NULL , numSavedSteps=50000 , thinSteps=1 ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = myData$NETTO
  x = myData$ABSCHLUSS
  xLevels = myData$ABSCHLUSS
  # Do some checking that data make sense:
  if ( any( x!=1 & x!=2 ) ) { stop("All x values must be 1 or 2.") }
  if ( any( !is.finite(y) ) ) { stop("All y values must be finite.") }
  if ( length(x) != length(y) ) { stop("x and y must be same length.") }
  Ntotal = length(y)
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    y = y ,
    x = x ,
    Ntotal = Ntotal ,
    meanY = mean(y) ,
    sdY = sd(y)
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
    for ( i in 1:Ntotal ) {
      y[i] ~ dt( mu[x[i]] , 1/sigma[x[i]]^2 , nu )
    }
    for ( j in 1:2 ) { # 2 groups
      mu[j] ~ dnorm( meanY , 1/(100*sdY)^2 )
      sigma[j] ~ dunif( sdY/1000 , sdY*1000 )
    }
    nu ~ dexp(1/30.0)
}
  " # close quote for modelString
# Write out modelString to a text file
writeLines( modelString , con="IncomeDiffModel.txt" )
#-----------------------------------------------------------------------------
# INTIALIZE THE CHAINS.
# Initial values of MCMC chains based on data:
mu = c( mean(y[x==1]) , mean(y[x==2]) )
sigma = c( sd(y[x==1]) , sd(y[x==2]) )
# Regarding initial values in next line: (1) sigma will tend to be too big if 
# the data have outliers, and (2) nu starts at 5 as a moderate value. These
# initial values keep the burn-in period moderate.
initsList = list( mu = mu , sigma = sigma , nu = 5 )
#-----------------------------------------------------------------------------
# RUN THE CHAINS
parameters = c( "mu" , "sigma" , "nu" )     # The parameters to be monitored
adaptSteps = 500               # Number of steps to "tune" the samplers
burnInSteps = 1000
runJagsOut <- run.jags( method=runjagsMethod ,
                        model="IncomeDiffModel.txt" , 
                        monitor=parameters , 
                        data=dataList ,  
                        inits=initsList , 
                        n.chains=nChains ,
                        adapt=adaptSteps ,
                        burnin=burnInSteps , 
                        sample=ceiling(numSavedSteps/nChains) ,
                        thin=thinSteps ,
                        summarise=FALSE ,
                        plots=FALSE )
codaSamples = as.mcmc.list( runJagsOut )
# resulting codaSamples object has these indices: 
#   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
if ( !is.null(saveName) ) {
  save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
}
return( codaSamples )
} # end function

#===============================================================================

smryMCMC = function(  codaSamples , RopeMuDiff=NULL , RopeSdDiff=NULL , 
                      RopeEff=NULL , saveName=NULL ) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  summaryInfo = rbind( summaryInfo , 
                       "mu[1]" = summarizePost( mcmcMat[,"mu[1]"] ) )
  summaryInfo = rbind( summaryInfo , 
                       "mu[2]" = summarizePost( mcmcMat[,"mu[2]"] ) )
  summaryInfo = rbind( summaryInfo , 
                       "muDiff" = summarizePost( 
                         mcmcMat[,"mu[2]"] - mcmcMat[,"mu[1]"] , 
                         compVal=0.0 , ROPE=RopeMuDiff ) )
  summaryInfo = rbind( summaryInfo , 
                       "sigma[1]" = summarizePost( mcmcMat[,"sigma[1]"] ) )
  summaryInfo = rbind( summaryInfo , 
                       "sigma[2]" = summarizePost( mcmcMat[,"sigma[2]"] ) )
  summaryInfo = rbind( summaryInfo , 
                       "sigmaDiff" = summarizePost( 
                         mcmcMat[,"sigma[2]"] - mcmcMat[,"sigma[1]"] , 
                         compVal=0.0 , ROPE=RopeSdDiff ) )
  summaryInfo = rbind( summaryInfo , 
                       "nu" = summarizePost( mcmcMat[,"nu"] ) )
  summaryInfo = rbind( summaryInfo , 
                       "log10(nu)" = summarizePost( log10(mcmcMat[,"nu"]) ) )
  summaryInfo = rbind( summaryInfo , 
                       "effSz" = summarizePost( 
                         ( mcmcMat[,"mu[2]"]-mcmcMat[,"mu[1]"] ) 
                         / sqrt((mcmcMat[,"sigma[1]"]^2+mcmcMat[,"sigma[2]"]^2)/2) ,
                         compVal=0.0 , ROPE=RopeEff ) )
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}

#===============================================================================

plotMCMC = function( codaSamples , datFrm , yName="y" , xName="x" ,
                     RopeMuDiff=NULL , RopeSdDiff=NULL , RopeEff=NULL , 
                     showCurve=FALSE , pairsPlot=FALSE ,
                     saveName=NULL , saveType="jpg" ) {
  # RopeMuDiff is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the difference of means.
  # RopeSdDiff is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the difference of standard deviations.
  # RopeEff is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the effect size.
  # showCurve is TRUE or FALSE and indicates whether the posterior should
  #   be displayed as a histogram (by default) or by an approximate curve.
  # pairsPlot is TRUE or FALSE and indicates whether scatterplots of pairs
  #   of parameters should be displayed.
  #-----------------------------------------------------------------------------
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  mu1 = mcmcMat[,"mu[1]"]
  mu2 = mcmcMat[,"mu[2]"]
  sigma1 = mcmcMat[,"sigma[1]"]
  sigma2 = mcmcMat[,"sigma[2]"]
  nu = mcmcMat[,"nu"]
  #-----------------------------------------------------------------------------
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph(width=7,height=7)
    nPtToPlot = 1000
    plotIdx = floor(seq(1,length(mu1),by=length(mu1)/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.25 ) # was cex=cex.cor*r
    }
    pairs( cbind( mu1 , mu2 , sigma1 , sigma2 , log10(nu) )[plotIdx,] ,
           labels=c( expression(mu[1]) , expression(mu[2]) , 
                     expression(sigma[1]) , expression(sigma[2]) , 
                     expression(log10(nu)) ) , 
           lower.panel=panel.cor , col="skyblue" )
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"PostPairs",sep=""), type=saveType)
    }
  }
  #-----------------------------------------------------------------------------
  # Set up window and layout:
  openGraph(width=6.0,height=8.0)
  layout( matrix( c(4,5,7,8,3,1,2,6,9,10) , nrow=5, byrow=FALSE ) )
  par( mar=c(3.5,3.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  # Select thinned steps in chain for plotting of posterior predictive curves:
  nCurvesToPlot = 20
  stepIdxVec = seq( 1 , chainLength , floor(chainLength/nCurvesToPlot) )
  # Compute limits for plots of data with posterior pred. distributions
  y = as.numeric(datFrm[,yName])
  x = as.numeric(as.factor(datFrm[,xName]))
  xLevels = levels(as.factor(datFrm[,xName]))
  y1 = y[x==1]
  y2 = y[x==2]
  xLim = c( min(y)-0.1*(max(y)-min(y)) , max(y)+0.1*(max(y)-min(y)) )
  xBreaks = seq( xLim[1] , xLim[2] , 
                 length=ceiling((xLim[2]-xLim[1])/(mean(c(sd(y1),sd(y2)))/4)) )
  histInfo1 = hist(y1,breaks=xBreaks,plot=FALSE)
  histInfo2 = hist(y2,breaks=xBreaks,plot=FALSE)
  yMax = 1.2 * max( c( histInfo1$density , histInfo2$density ) )
  xVec = seq( xLim[1] , xLim[2] , length=501 )
  #-----------------------------------------------------------------------------
  # Plot data y1 and smattering of posterior predictive curves:
  histInfo = hist( y1 , prob=TRUE , xlim=xLim , ylim=c(0,yMax) , breaks=xBreaks,
                   col="red2" , border="white" , xlab="y" , ylab="" , 
                   yaxt="n" , cex.lab=1.5 , 
                   main=paste("Data for",xLevels[1],"w. Post. Pred.") )
  for ( stepIdx in 1:length(stepIdxVec) ) {
    lines(xVec, dt( (xVec-mu1[stepIdxVec[stepIdx]])/sigma1[stepIdxVec[stepIdx]], 
                    df=nu[stepIdxVec[stepIdx]] )/sigma1[stepIdxVec[stepIdx]] , 
          type="l" , col="skyblue" , lwd=1 )
  }
  text( max(xVec) , yMax , bquote(N[1]==.(length(y1))) , adj=c(1.1,1.1) )
  #-----------------------------------------------------------------------------
  # Plot data y2 and smattering of posterior predictive curves:
  histInfo = hist( y2 , prob=TRUE , xlim=xLim , ylim=c(0,yMax) , breaks=xBreaks,
                   col="red2" , border="white" , xlab="y" , ylab="" , 
                   yaxt="n" , cex.lab=1.5 , 
                   main=paste("Data for",xLevels[2],"w. Post. Pred.") )
  for ( stepIdx in 1:length(stepIdxVec) ) {
    lines(xVec, dt( (xVec-mu2[stepIdxVec[stepIdx]])/sigma2[stepIdxVec[stepIdx]], 
                    df=nu[stepIdxVec[stepIdx]] )/sigma2[stepIdxVec[stepIdx]] , 
          type="l" , col="skyblue" , lwd=1 )
  }
  text( max(xVec) , yMax , bquote(N[2]==.(length(y2))) , adj=c(1.1,1.1) )
  #-----------------------------------------------------------------------------  
  # Plot posterior distribution of parameter nu:
  histInfo = plotPost( log10(nu) , col="skyblue" , # breaks=30 ,
                       showCurve=showCurve ,
                       xlab=bquote("log10("*nu*")") , cex.lab = 1.75 , 
                       cenTend="mode" ,
                       main="Normality" ) #  (<0.7 suggests kurtosis)
  
  #-----------------------------------------------------------------------------
  # Plot posterior distribution of parameters mu1, mu2, and their difference:
  xlim = range( c( mu1 , mu2 ) )
  histInfo = plotPost( mu1 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(mu[1]) , main=paste(xLevels[1],"Mean") , 
                       col="skyblue" )
  histInfo = plotPost( mu2 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(mu[2]) , main=paste(xLevels[2],"Mean") , 
                       col="skyblue" )
  histInfo = plotPost( mu2-mu1 , compVal=0 ,  showCurve=showCurve ,
                       xlab=bquote(mu[2] - mu[1]) , cex.lab = 1.75 , 
                       ROPE=RopeMuDiff,
                       main="Difference of Means" , col="skyblue" )
  #-----------------------------------------------------------------------------
  # Plot posterior distribution of param's sigma1, sigma2, and their difference:
  xlim=range( c( sigma1 , sigma2 ) )
  histInfo = plotPost( sigma1 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(sigma[1]) , 
                       main=paste(xLevels[1],"Scale") , 
                       col="skyblue" , cenTend="mode" )
  histInfo = plotPost( sigma2 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(sigma[2]) , 
                       main=paste(xLevels[2],"Scale") , 
                       col="skyblue" , cenTend="mode" )
  histInfo = plotPost( sigma2 - sigma1 , 
                       compVal=0 ,  showCurve=showCurve ,
                       xlab=bquote(sigma[2] - sigma[1]) , cex.lab = 1.75 , 
                       ROPE=RopeSdDiff ,
                       main="Difference of Scales" , col="skyblue" , 
                       cenTend="mode" )
  #-----------------------------------------------------------------------------
  # Plot effect size. 
  effectSize = ( mu2 - mu1 ) / sqrt( ( sigma1^2 + sigma2^2 ) / 2 )
  histInfo = plotPost( effectSize , compVal=0 ,  ROPE=RopeEff ,
                       showCurve=showCurve ,
                       xlab=bquote( (mu[2]-mu[1])
                                    /sqrt((sigma[1]^2 +sigma[2]^2 )/2 ) ),
                       cenTend="mode" , cex.lab=1.0 , main="Effect Size" ,
                       col="skyblue" )
  #-----------------------------------------------------------------------------
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Post",sep=""), type=saveType)
  }
}



#Ab hier der Code für den Bayes factor im zweiten Beispiel, wieder der Webseite von Lee & Wagenmakers entnommen
#Es mussten nur die Daten eingetragen werden
library(R2jags)

x <- RobustIncomeHEEQ
y <- RobustIncomeNoHEEQ

n1 <- length(x)
n2 <- length(y)

# Rescale
y <- y - mean(x)
y <- y/sd(x)
x <- (x-mean(x))/sd(x); 

data <- list("x", "y", "n1", "n2") # to be passed on to JAGS

myinits <- list(
  list(delta = rnorm(1,0,3), mu = rnorm(1,0,1), sigmatmp = runif(1,0,5)),
  list(delta = rnorm(1,0,3), mu = rnorm(1,0,1), sigmatmp = runif(1,0,5)),
  list(delta = rnorm(1,0,3), mu = rnorm(1,0,1), sigmatmp = runif(1,0,5)))

# Parameters to be monitored
parameters <- c("delta")

# The following command calls JAGS with specific options.
# For a detailed description see the R2jags documentation.
samples <- jags(data, inits=myinits, parameters,
                model.file ="TwoSample.txt",
                n.chains=3, n.iter=10000, n.burnin=5000, n.thin=1, DIC=T)
# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

plot(samples)

# Collect posterior samples across all chains:
delta.posterior <- samples$BUGSoutput$sims.list$delta  

#============ BFs based on logspline fit ===========================
library(polspline) # this package can be installed from within R
fit.posterior <- logspline(delta.posterior)

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior     <- dcauchy(1)                   # height of order--restricted prior at delta = 0
BF01      <- posterior/prior
BF01

#============ Plot Prior and Posterior  ===========================
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
xlow  <- -3
xhigh <- 3
yhigh <- 2
Nbreaks <- 80
y       <- hist(delta.posterior, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=2,
     xlim=c(xlow,xhigh), ylim=c(0,yhigh), xlab=" ", ylab="Density", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0", "1", "2", "3", "4"))
axis(2)
mtext(expression(delta), side=1, line = 2.8, cex=2)
#now bring in log spline density estimation:
par(new=T)
plot(fit.posterior, ylim=c(0,yhigh), xlim=c(xlow,xhigh), lty=1, lwd=1, axes=F)
points(0, dlogspline(0, fit.posterior),pch=19, cex=2)
# plot the prior:
par(new=T)
plot ( function( x ) dcauchy( x, 0, 1 ), xlow, xhigh, ylim=c(0,yhigh), xlim=c(xlow,xhigh), lwd=2, lty=1, ylab=" ", xlab = " ", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0", "1", "2", "3", "4"))
axis(2)
points(0, dcauchy(0), pch=19, cex=2)



