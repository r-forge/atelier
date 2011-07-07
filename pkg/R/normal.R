#-----------------------------------------------------------------------------------------
#                           Construction de la loi normale
#-----------------------------------------------------------------------------------------
.ws1 = proto(

  create = function(.,h,...) {
  
    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Construction de\nla loi normale" %in% names(.ws$nb)) return()
    
   .$distribution = gdroplist(names(.$availDists),horizontal=FALSE,handler=.$updateParamNames)
   .$sampleSize = gradio(c(500, 1000, 5000, 50000),handler=.$updatePlot,coerce.with=as.numeric)
   .$nvar = gradio(c(1, 2, 10, 50),handler=.$updatePlot,coerce.with=as.numeric)
   .$displayWhat = gradio(c("Effectifs","Fréquences","Densités"),handler=.$updatePlot)
   .$displayFunc = gcheckbox("théorique",handler=.$updatePlot)
   .$displayNorm = gcheckbox("normale",handler=.$updatePlot)
   .$param1 = gedit("0",width=5,coerce.with=as.numeric)
   .$paramLabel1 = glabel("Borne gauche")
   .$param2 = gedit("1",width=5,coerce.with=as.numeric)
   .$paramLabel2 = glabel("Borne droite")
   .$cutpoints = gedit("",handler=.$updatePlot)

    add(.ws$nb, group <- ggroup(horizontal=FALSE),label="Construction de\nla loi normale")
    tmp = gframe("Distribution", container = group)
    distribGroup = glayout(container=tmp)
    distribGroup[2,2,anchor=c(-1,0)]=glabel("Loi")
    distribGroup[2,3]=.$distribution
    distribGroup[3,2,anchor=c(-1,0)]=.$paramLabel1
    distribGroup[3,3]=.$param1
    distribGroup[4,2,anchor=c(-1,0)]=.$paramLabel2
    distribGroup[4,3]=.$param2
    visible( distribGroup)=TRUE

    sizeGroup = ggroup(cont = group,expand=TRUE)
    tmp = gframe("Nombre d\'obs.", container =  sizeGroup,expand=TRUE)
    add(tmp, .$sampleSize)
    tmp = gframe("Variables", container =  sizeGroup,expand=TRUE)
    add(tmp, .$nvar)

   histGroup = ggroup(cont = group,expand=TRUE)
    tmp = gframe("Afficher", container = histGroup,expand=TRUE)
    add(tmp,.$displayWhat)
    tmp = gframe("Loi", container = histGroup, horizontal=FALSE,expand=TRUE)
    add(tmp,.$displayFunc)
    add(tmp,.$displayNorm)

    tmp = gframe("Coupures", container = group)
    add(tmp,.$cutpoints,expand=TRUE)

    addSpring(group)

    buttonGroup=ggroup(container=group)
    addSpring(buttonGroup)
    gbutton("  Afficher  ",container=buttonGroup, handler=.$updatePlot)

  },
  
  updatePlot = function(.,h,...) {

    # Vérification des paramètres
    if(any(is.na(c(svalue(.$param1),svalue(.$param2))))) {
      gmessage("Spécifiez des valeurs de paramètres.")
      return()
    }
    
    rfunc = paste("r",.$availDists[svalue(.$distribution)],sep="")
    dfunc = paste("d",.$availDists[svalue(.$distribution)],sep="")
    
    # Génération des données
    nvar = as.numeric(svalue(.$nvar))
    nobs = as.numeric(svalue(.$sampleSize))
    
    y = do.call(rfunc, list(nobs*nvar,svalue(.$param1),svalue(.$param2)))
    x = rowSums(matrix(y,nobs,nvar))
    
    # Définition des coupures
    if(nchar(svalue(.$cutpoints))) { 
      breaks = unlist(strsplit(svalue(.$cutpoints)," "))
      breaks = breaks[breaks!=""]
      if(!length(breaks)) breaks="sturges"
      else breaks = as.numeric(breaks)
    } 
    else { breaks = "sturges" }

    # Affichage de l'histogramme empirique (distributions continues)
    if(svalue(.$distribution)!="Binomiale") { 
      hh = hist(x,breaks=breaks,plot=FALSE)
      if(svalue(.$displayWhat)=="Fréquences") {
        hh$counts = hh$counts / nobs
      }
      xlab = ifelse(nvar==1,"Valeurs de la variable","Valeurs de la variable somme")
      plot(hh,freq=svalue(.$displayWhat)!="Densités",main = paste("Distribution",svalue(.$distribution)),xlab=xlab,ylab=svalue(.$displayWhat))

      # Affichage de la loi théorique
      if(svalue(.$displayFunc) && (nvar==1)) {
        z = seq(min(x),max(x),len=100)
        lines(z,do.call(dfunc, list(z,svalue(.$param1),svalue(.$param2))),lwd=2,col="red")
      }
    }
    
    # Loi binomiale
    else {
      # Affichage de l'histogramme empirique
      nn = table(x)
      if(svalue(.$displayWhat) %in% c("Fréquences","Densités")) {
        nn = nn / nobs
      }
      xlab = ifelse(svalue(nvar)==1,"Valeurs de la variable","Valeurs de la variable somme")
      res = plot(nn,main = paste("Distribution",svalue(.$distribution)),xlab=xlab,ylab=svalue(.$displayWhat))
	
	  # Affichage des probabilités théoriques
      if(svalue(.$displayFunc) && (nvar==1)) {
        z = min(x):max(x)
	    lines(z+.1,dbinom(z,svalue(.$param1),svalue(.$param2)),col="red",lwd=2,type="h")
      }
	
    }

    if(svalue(.$displayNorm)) {
      z = seq(min(x),max(x),len=100)
      lines(z,dnorm(z,mean(x),sd(x)),lwd=2,col="blue")
    }

  },
  
  updateParamNames = function(.,h,...) {
  
    newDist = svalue(.$distribution)
    
    # Warning: a droplist may be temporarily set to NULL when changed
    if(is.null(newDist)) return()
    
    svalue(.$paramLabel1) = .$paramNames[[newDist]][1]
    svalue(.$paramLabel2) = .$paramNames[[newDist]][2]
    
  },
  #---------------------------------------------------------------------------------------
  #  SLOT                   INITIAL VALUE                                    CONTENT
  #---------------------------------------------------------------------------------------
  availDists     = c(Uniforme = "unif", Binomiale = "binom", Normale = "norm", Gamma = "gamma"),
  paramNames     = list(Uniforme=c("Borne gauche","Borne droite"),Binomiale=c("Taille","Probabilité"),Normale=c("Moyenne","Ecart-type"),Gamma=c("Forme","Echelle")),
  distribution   = NULL,
  sampleSize     = NULL,
  nvar           = NULL,
  displayWhat    = NULL,
  displayFunc    = NULL,
  displayNorm    = NULL,
  param1         = NULL,
  paramLabel1    = NULL,
  param2         = NULL,
  paramLabel2    = NULL,
  cutpoints      = NULL
)


