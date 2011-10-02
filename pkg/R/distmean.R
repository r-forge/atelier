#-----------------------------------------------------------------------------------------
#                           Distribution d'une moyenne d'échantillon
#-----------------------------------------------------------------------------------------

.ws3 = proto(

  create = function(.,h,...) {

    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Distribution\nd\'une moyenne" %in% names(.ws$nb)) return()
    
   .$distribution = gdroplist(names(.$availDists),horizontal=FALSE,handler=.$onChangeDist)
   .$paramLabel1 = glabel("Borne gauche")
   .$paramLabel2 = glabel("Borne droite")
   .$sampleSize = gradio(c(10,20,50,100),handler=.$updatePlot)
   .$nvar = gradio(c(5,10,50,500),handler=.$updatePlot)
   .$displayNorm = gcheckbox("la loi normale (moyennes)",handler=.$updatePlot)
   .$popDistr = gcheckbox("la distribution d\'origine (scores)",handler=.$updatePlot)
   .$param1 = gedit("0",width=5,coerce.with=as.numeric)
   .$param2 = gedit("1",width=5,coerce.with=as.numeric)
   .$xbar = glabel("")
   .$var = glabel("")
   .$s = glabel("")

    # Construction de l'interface
    add(.ws$nb,group <- ggroup(horizontal=FALSE),label="Distribution\nd\'une moyenne")

    tmp = gframe("Distribution", container = group)
    distribGroup = glayout(container=tmp)
    distribGroup[1,1,anchor=c(-1,0)]=glabel("Loi")
    distribGroup[1,2]=.$distribution
    distribGroup[2,1,anchor=c(-1,0)]=.$paramLabel1
    distribGroup[2,2]=.$param1
    distribGroup[3,1,anchor=c(-1,0)]=.$paramLabel2
    distribGroup[3,2]=.$param2
    visible(distribGroup)=TRUE

    sizeGroup = ggroup(cont = group,expand=TRUE)
    tmp = gframe("Echantillons", container = sizeGroup,expand=TRUE)
    add(tmp, .$nvar)
    tmp = gframe("Obs. par éch.", container = sizeGroup,expand=TRUE)
    add(tmp, .$sampleSize)

    # Options d'affichage
    tmp = gframe("Afficher", container = group, horizontal=FALSE)
    add(tmp,.$displayNorm)
    add(tmp,.$popDistr)

    # Statistiques descriptives
    tmp = gframe("Statistiques descriptives", container = group, horizontal=FALSE)
    resGroup = glayout(container=tmp)
    resGroup[2,2,anchor=c(-1,0)]=glabel("Moyenne")
    resGroup[2,3] = .$xbar
    resGroup[3,2,anchor=c(-1,0)]=glabel("Variance")
    resGroup[3,3] = .$var
    resGroup[4,2,anchor=c(-1,0)]=glabel("Ecart-type")
    resGroup[4,3] = .$s

    addSpring(group)

    buttonGroup=ggroup(container=group)
    addSpring(buttonGroup)
    gbutton("  Echantillonner  ",container=buttonGroup, handler=.$updatePlot)
  
  },
  
  updatePlot = function(.,h,...) {
  
    # Vérification des paramètres
    if(any(is.na(c(svalue(.$param1),svalue(.$param2))))) {
      gmessage("Spécifiez des valeurs de paramètres.")
      return()
    }
    
    distrib = svalue(.$distribution)
    nobs = as.numeric(svalue(.$sampleSize))
    nsamp = as.numeric(svalue(.$nvar))
    plotNorm = svalue(.$displayNorm)
    plotDens = svalue(.$popDistr)
    param1 = svalue(.$param1)
    param2 = svalue(.$param2)
    
    # Moyenne et écart-type de la distribution des moyennes d'échantillon
    if(distrib=="Uniforme") { 
      y = colMeans(matrix(runif(nobs*nsamp,param1,param2),nobs,nsamp))
      xlim = c(min(y),max(y))
      if(plotDens) xlim = c(param1,param2)
      true.mean = (param1+param2)/2
      true.sd = (param2-param1)/sqrt(12*nobs)
    }	
    else if(distrib=="Binomiale") {  
      y = colMeans(matrix(rbinom(nobs*nsamp,param1,param2),nobs,nsamp))
      xlim = c(min(y),max(y))
      if(plotDens) xlim = c(0,param1)
      true.mean = param1 * param2
      true.sd = sqrt(param1*param2*(1-param2)/nobs)
    }
    else {
      y = colMeans(matrix(rnorm(nobs*nsamp,param1,param2),nobs,nsamp))
      xlim = c(min(y),max(y))
      if(plotDens) xlim = c(param1-4.5*param2,param1+4.5*param2)
      true.mean = param1
      true.sd = param2/sqrt(nobs)
    }

    # Affichage des stats descriptives
    svalue(.$xbar) = paste(round(mean(y),3))
    svalue(.$var) = paste(round(var(y),3))
    svalue(.$s) = paste(round(sd(y),3))

    # Affichage graphique  
    xlab = "Valeurs de moyennes d\'échantillons"
    title = "Distribution des moyennes"
    ylab = "Densités"
    
    if(distrib=="Uniforme") {
      hist(y,freq=FALSE,main=title,xlab=xlab,ylab=ylab,xlim=xlim)
      if(plotDens) abline(h=(1/(param2-param1)),col="red")
    }
    
    else if(distrib=="Normale") {
      hist(y,freq=FALSE,main=title,xlab=xlab,ylab=ylab,xlim=xlim)
      if(plotDens) curve(dnorm(x,param1,param2),from=xlim[1],to=xlim[2],add=TRUE,lwd=2,col=2)
    }
    
    # Binomiale
    else if(distrib=="Binomiale") {
      res = plot((nobs*table(y))/nsamp,main=title,xlab=xlab,ylab=ylab,xlim=xlim)
      if(plotDens) lines(0.01+(0:param1),dbinom(0:param1,param1,param2),col="red",type="h")
    }
    
    if(plotNorm) curve(dnorm(x,true.mean,true.sd),from=min(y),to=max(y),add=TRUE,lwd=2,col="blue")
  

  },

  onChangeDist = function(.,h,...) {
  
    newDist = svalue(.$distribution)
    
    # Warning: a droplist may be temporarily set to NULL when changed
    if(is.null(newDist)) return()
    
    svalue(.$paramLabel1) = .$paramNames[[newDist]][1]
    svalue(.$paramLabel2) = .$paramNames[[newDist]][2]
    
  },
  
  #---------------------------------------------------------------------------------------
  #  SLOT                   INITIAL VALUE             CONTENT
  #---------------------------------------------------------------------------------------
  availDists   = c(Uniforme = "unif", 
                   Binomiale = "binom",
                   Normale = "norm"),
  distribution = NULL,
  paramNames   = list(Uniforme=c("Borne gauche","Borne droite"),Binomiale=c("Taille","Probabilité"),Normale=c("Moyenne","Ecart-type")),
  paramLabel1  = NULL,
  paramLabel2  = NULL,
  sampleSize   = NULL,
  nvar         = NULL,
  displayNorm  = NULL,
  popDistr     = NULL,
  param1       = NULL,
  param2       = NULL,
  xbar         = NULL,
  var          = NULL,
  s            = NULL
)


