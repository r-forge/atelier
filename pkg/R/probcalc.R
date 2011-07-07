#-----------------------------------------------------------------------------------------
#                           Calculateur de probabilités
#-----------------------------------------------------------------------------------------
.ws4 = proto(

  create = function(.,h,...) {

    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Calculateur\nde probabilités" %in% names(.ws$nb)) return()
    
   .$distribution = gdroplist(names(.$availDists),horizontal=FALSE,handler=.$initOptions)
   .$calcWhat = gradio(c("Quantile    =>  probabilité","Probabilité =>  quantile"),handler=.$updatePlot)
   .$side = gradio(c("A gauche","A droite"),handler=.$updatePlot)
   .$param1 = gedit("0",width=15)
   .$param2 = gedit("1",width=15)
   .$paramLabel1 = glabel("Moyenne")
   .$paramLabel2 = glabel("Ecart-type")
   .$value  = gedit(width=15,handler=.$updatePlot)
   .$resultx = glabel("")
   .$resultp = glabel("")

    # Construction de l'interface
    add(.ws$nb, group <- ggroup(horizontal=FALSE),label="Calculateur\nde probabilités")

    tmp = gframe("Distribution d\'origine",container=group)
    distribGroup = glayout(container=tmp)
    distribGroup[2,2,anchor=c(-1,0)]=glabel("Loi")
    distribGroup[2,3]=.$distribution
    distribGroup[3,2,anchor=c(-1,0)]=.$paramLabel1
    distribGroup[3,3]=.$param1
    distribGroup[4,2,anchor=c(-1,0)]=.$paramLabel2
    distribGroup[4,3]=.$param2
    visible(distribGroup)=TRUE

    tmp = gframe("Type de calcul",container=group)
    add(tmp,.$calcWhat)

    tmp = gframe("Cumul",container=group)
    add(tmp,.$side)

    tmp = gframe("Valeur ou expression à calculer",container=group)
    add(tmp,.$value,expand=TRUE)

    tmp = gframe("Résultat",container=group)
    resultGroup = glayout(container=tmp)
    resultGroup[2,2] = " x ="
    resultGroup[2,3,expand=TRUE,anchor=c(-1,0)] = .$resultx
    resultGroup[3,2] = " p ="
    resultGroup[3,3,expand=TRUE,anchor=c(-1,0)] = .$resultp

    addSpring(group)

    # buttons
    buttonGroup = ggroup(container=group)
    addSpring(buttonGroup)
    gbutton("  Afficher  ",container=buttonGroup, handler=.$updatePlot)
  
  },
  
  updatePlot = function(.,h,...) {

    param1 = eval(parse(text=svalue(.$param1)))
    param2 = eval(parse(text=svalue(.$param2)))
    
    if(is.null(param1)) {
      gmessage("Spécifiez des valeurs de paramètres.")
      return()
    }
    
    distrib = svalue(.$distribution)
    is1P = distrib %in% c("Student","Chi-2","Poisson")

    if(!is1P && is.null(param2)) {
        gmessage("Spécifiez des valeurs de paramètres.")
        return()
    }
      
    value = eval(parse(text=svalue(.$value)))
    
    if(is.null(value)) {
      gmessage("Spécifiez une valeur.")
      return()
    }

    if( (distrib %in% c("Binomiale","Poisson","Chi-2","Fisher","Gamma","Beta"))  && (value < 0))  {
      gmessage("Cette distribution n\'est pas définie pour les valeurs négatives.")
      return()  
    }
    
    isDiscrete = distrib %in% c("Binomiale","Poisson")
    is01 = function(x) (x>=0)&&(x<=1)
    isInteger = function(x) abs(x)==round(x)
    probf = .$availDists
    
    p = svalue(.$calcWhat,index=T) == 2 # "Probabilité =>  quantile"
    right = svalue(.$side)=="A droite"

    if(p && !is01(value)) {
      gmessage("Une probabilité est comprise entre 0 et 1.")
      return()
    }
    
    dfunction = eval(parse(text=paste("d",probf[distrib],sep="")))
    pfunction = eval(parse(text=paste("p",probf[distrib],sep="")))
    qfunction = eval(parse(text=paste("q",probf[distrib],sep="")))
    rfunction = eval(parse(text=paste("r",probf[distrib],sep="")))
    
    # Chosen distribution has two parameters
    if(!is1P) {
    
      # Check parameter values
      if(distrib=="Binomiale") {
        stopifnot(isInteger(param1) && is01(param2)) }
      if(distrib=="Fisher") {
        stopifnot(isInteger(param1) && isInteger(param2)) }
      
      # prob. to quantile
      if(p) {
      
        prob = value
        
        # Continuous distribution
        if(!isDiscrete) {
          if(right) value = qfunction(1-prob,param1,param2)
          else      value = qfunction(prob,param1,param2)
        }
        
        # Discrete distribution
        else {
          if(right) value = qfunction(1-prob,param1,param2)
          else      value = qfunction(prob,param1,param2)
        }
      }
      
      # Quantile to prob.
      else {
      
        # Continuous distribution
        if(!isDiscrete) {
          if(right) prob = 1-pfunction(value,param1,param2)
          else      prob = pfunction(value,param1,param2)
        }
        
        # Discrete distribution
        else {
          if(right)  prob = 1-pfunction(value-1,param1,param2)
          else       prob = pfunction(value,param1,param2)
        }
      }
      
      dens = dfunction(value,param1,param2)
    }
    
    # One-parameter distributions
    else {
    
      svalue(.$param2)=""
      
      # Check parameter values
      if(distrib=="Student") {
        stopifnot(isInteger(param1)) }
      if(distrib=="Chi-2") {
        stopifnot(isInteger(param1)) }
      
      # Prob. to quantile
      if(p) {
      
        prob = value
        
        # Continuous distribution
        if(!isDiscrete) {
          if(right) value = qfunction(1-prob,param1)
          else      value = qfunction(prob,param1)
        }
        
        # Discrete distribution
        else {
          if(right) value = qfunction(1-prob,param1)
          else      value = qfunction(prob,param1)
        }
      }
      
      # Quantile to prob.
      else {
      
        # Continuous distribution
        if(!isDiscrete) {
          if(right) prob = 1-pfunction(value,param1)
          else      prob = pfunction(value,param1)
        }
        
        # Discrete distribution
        else {
          if(right) prob = 1-pfunction(value-1,param1) 
          else      prob = pfunction(value,param1)
        }
      }
      
      dens = dfunction(value,param1)
    }
    
    # Result
    svalue(.$resultx) = paste(value)
    svalue(.$resultp) = paste(prob)
    
    # Affichage
    xlab="X"
    title = paste("Distribution :",distrib)
    ylab = expression(f(X==x))
    from = 0
    
    # Two-parameter distribution
    if(!is1P) {
    
      # Continuous distribution
      if(!isDiscrete) { 
        from = ifelse(distrib=="Normale",param1-4*param2,0)
        from = min(from,value)
        to = ifelse(distrib=="Normale",param1+4*param2,max(rfunction(1000,param1,param2)))
        to = max(to,value)
        curve(dfunction(x,param1,param2),n=1000,from=from,to=to,lwd=2,main=title,xlab=xlab,ylab=ylab)
        
        if(!right) {
          z = c(seq(from,value,len=1000),value,from)
          dz = c(dfunction(z[1:1000],param1,param2),0,0)
          polygon(z,dz,density=-1,col="red",lwd=2)
        }
        else {
          z = c(seq(value,to,len=1000),to,value)
          dz = c(dfunction(z[1:1000],param1,param2),0,0)
          polygon(z,dz,density=-1,col="red",lwd=2)
        }
      }
      
      # Discrete distribution
      else {
        from = 0
        to = ifelse(distrib=="Binomiale",param1,max(rfunction(1000,param1,param2)))
        z = 0:to
        plot(z,dfunction(z,param1,param2),type="h",lwd=2,main=title,xlab=xlab,ylab=ylab)
        
        if(!right) {
          for(i in 0:(value-1)) { lines(rbind(c(i,0),c(i,dfunction(i,param1,param2))),lwd=2,col="red") }
          lines(rbind(c(value,0),c(value,prob-pfunction(value-1,param1,param2))),lwd=2,col="red")
        }
        else {
          for(i in param1:(value+1)) {
          lines(rbind(c(i,0),c(i,dfunction(i,param1,param2))),lwd=2,col="red") }
          lines(rbind(c(value,0),c(value,prob-1+pfunction(value,param1,param2))),lwd=2,col="red")
        }
      }
    } 
    
    # One parameter distributions
    else {
    
      # Continuous distribution
      if(!isDiscrete) {
      
        from = ifelse(distrib=="Student",min(rfunction(1000,param1)),0)
        from = min(from,value)
        to = max(rfunction(1000,param1))
        to = max(to,value)
        curve(dfunction(x,param1),n=1000,from=from,to=to,lwd=2,main=title,xlab=xlab,ylab=ylab)
        
        if(!right) {
          z = c(seq(from,value,len=1000),value,from)
          dz = c(dfunction(z[1:1000],param1),0,0)
          polygon(z,dz,density=-1,col="red",lwd=2)
        }
        else {
          z = c(seq(value,to,len=1000),to,value)
          dz = c(dfunction(z[1:1000],param1),0,0)
          polygon(z,dz,density=-1,col="red",lwd=2)
        }
      }
      
      # Discrete distribution
      else {
        from = 0
        to = max(rfunction(1000,param1))
        z = 0:to
        plot(z,dfunction(z,param1),type="h",lwd=2,main=title,xlab=xlab,ylab=ylab)
        
        if(!right) {
          for(i in 0:(value-1)) {
            lines(rbind(c(i,0),c(i,dfunction(i,param1))),lwd=2,col="red")
          }
          lines(rbind(c(value,0),c(value,prob-pfunction(value-1,param1))),lwd=2,col="red")
        }
        
        else {
          for(i in to:(value+1)) {
            lines(rbind(c(i,0),c(i,dfunction(i,param1))),lwd=2,col="red")
          }
          lines(rbind(c(value,0),c(value,prob-1+pfunction(value,param1))),lwd=2,col="red")
        }
      }
    }

  },
  initOptions = function(.,h,...) {

    distrib = svalue(.$distribution)
    is1P = distrib %in% c("Student","Chi-2","Poisson")
    
    # Warning: a droplist may be temporarily set to NULL when changed
    if(is.null(distrib)) return()
    
    svalue(.$paramLabel1) = .$paramNames[[distrib]][1]
    svalue(.$paramLabel2) = .$paramNames[[distrib]][2]
    
    if(is1P) {
      svalue(.$param2) = ""
      enabled(.$param2) = FALSE
    }
    else {
      enabled(.$param2) = TRUE
    }
    
    svalue(.$resultx) = ""
    svalue(.$resultp) = ""    
  },

  #---------------------------------------------------------------------------------------
  #  SLOT                   INITIAL VALUE                          CONTENT
  #---------------------------------------------------------------------------------------
  availDists   = c(Normale="norm",Student="t",
                  "Chi-2"="chisq",Fisher="f",
                   Binomiale="binom",Poisson="pois",
                   Gamma="gamma",Beta="beta"),       #     Distributions disponibles
  paramNames   = list(Uniforme=c("Borne gauche","Borne droite"),
                      Binomiale=c("Taille","Probabilité"),
                      Normale=c("Moyenne","Ecart-type"),
                      Gamma=c("Forme","Echelle"),
                      Beta=c("alpha","beta"),
                      Poisson=c("Moyenne","-"),
                      Student=c("DDL","-"),
                      "Chi-2"=c("DDL","-")),         #    Noms des paramètres de lois
  distribution = NULL,                               #    Distribution choisie
  calcWhat     = NULL,                               #    Type de calcul (de quantile à prob. ou l'inverse)
  side         = NULL,                               #    Cumul à droite ou à gauche
  param1       = NULL,                               #    Valeur du paramètre 1 de la loi 
  param2       = NULL,                               #    Valeur du paramètre 2 de la loi (s'il y en a)   
  paramLabel1  = NULL,                               #    Nom du paramètre 1 de la loi
  paramLabel2  = NULL,                               #    Nom du paramètre 2 de la loi (s'il y en a)   
  value        = NULL,                               #    Valeur numérique de départ (quantile ou probabilité)
  resultx      = NULL,                               #    Résultat quantile
  resultp      = NULL                                #    Résultat probabilité
)


