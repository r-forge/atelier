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
   .$param1 = gedit("0",width=15)
   .$param2 = gedit("1",width=15)
   .$param3 = gedit("",width=15)
    enabled(.$param3) = FALSE
   .$paramLabel1 = glabel("Moyenne")
   .$paramLabel2 = glabel("Ecart-type")
   .$paramLabel3 = glabel("")
   
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
    distribGroup[5,2,anchor=c(-1,0)]=.$paramLabel3
    distribGroup[5,3]=.$param3
    visible(distribGroup)=TRUE

   .$calcWhat = gradio(c("Quantile    =>  probabilité","Probabilité =>  quantile"),handler=.$updatePlot)
    tmp = gframe("Type de calcul",container=group)
    add(tmp,.$calcWhat)

   .$side = gradio(c("A gauche","A droite"),handler=.$updatePlot)
    tmp = gframe("Cumul",container=group)
    add(tmp,.$side)

   .$value  = gedit(width=15,handler=.$updatePlot)
   .$resultx = glabel("")
   .$resultp = glabel("")

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
    param3 = eval(parse(text=svalue(.$param3)))
    
    if(is.null(param1)) {
      gmessage("Spécifiez des valeurs de paramètres.")
      return()
    }
    
    distrib = svalue(.$distribution)
    is1P = distrib %in% c("Student","Chi-2","Poisson")
    is2P = distrib %in% c("Normale","Chi-2 inverse","Fisher","Binomiale","Gamma","Gamma inverse","Beta")
    is3P = distrib %in% c("Student non standard","Student non centrale","Chi-2 non centrale","Fisher non centrale","Lambda prime")
    
    if(is2P && is.null(param2)) {
        gmessage("Il manque des valeurs de paramètres.")
        return()
    }
      
    if(is3P && is.null(param3)) {
        gmessage("Il manque des valeurs de paramètres.")
        return()
    }
      
    value = eval(parse(text=svalue(.$value)))
    
    if(is.null(value)) {
      gmessage("Spécifiez une valeur.")
      return()
    }

    if( (distrib %in% c("Binomiale","Poisson","Chi-2","Chi-2 inverse","Fisher","Gamma","Gamma inverse","Beta"))  && (value < 0))  {
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
    
    # Some extra distributions for Bayesian stats
    
    # Non-central Chi-square
    dncchisq = function(x,nu,ncp) dchisq(x,nu,ncp)
    qncchisq = function(p,nu,ncp) qchisq(p,nu,ncp)
    pncchisq = function(q,nu,ncp) pchisq(q,nu,ncp)
    rncchisq = function(n,nu,ncp) rchisq(n,nu,ncp)

    # Non-central F
    dncf = function(x,nu1,nu2,ncp) df(x,nu1,nu2,ncp)
    qncf = function(p,nu1,nu2,ncp) qf(p,nu1,nu2,ncp)
    pncf = function(q,nu1,nu2,ncp) pf(q,nu1,nu2,ncp)
    rncf = function(n,nu1,nu2,ncp) rf(n,nu1,nu2,ncp)
    
    # Inverse Gamma
    dinvgamma = function(z,a,b) exp(a*log(b) - lgamma(a) -(a+1)*log(z) -(b/z))
    qinvgamma = function(p,a,b) ifelse(((1 - p) <= .Machine$double.eps),Inf,1/qgamma(1-p,a,b))
    pinvgamma = function(z,a,b) 1-pgamma(1/z,a,b)
    rinvgamma = function(n,a,b) 1/rgamma(n=n,shape=a,rate=b)
    
    # Scaled Inversed Chi-Square
    dscaledInvChi2 = function(z,nu,s2) dinvgamma(z,nu/2,nu*s2/2)
    pscaledInvChi2 = function(p,nu,s2) pinvgamma(p,nu/2,nu*s2/2)
    qscaledInvChi2 = function(z,nu,s2) qinvgamma(z,nu/2,nu*s2/2)
    rscaledInvChi2 = function(n,nu,s2) rinvgamma(n,nu/2,nu*s2/2)
    
    # Non standard (3P) Student
    dnst = function(x,nu,pos,scale) dt((x-pos)/scale,nu)/scale
    qnst = function(p,nu,pos,scale) qt(p,nu)*scale + pos
    pnst = function(q,nu,pos,scale) pt((q-pos)/scale,nu)
    rnst = function(n,nu,pos,scale) rt(n,nu)*scale + pos
    
    # Non central scaled Student
    dnct = function(x,nu,ncp,scale) dt(x/scale,nu,ncp/scale)/scale
    qnct = function(p,nu,ncp,scale) qt(p,nu,ncp/scale)*scale
    pnct = function(q,nu,ncp,scale) pt(q/scale,nu,ncp/scale)
    rnct = function(n,nu,ncp,scale) rt(n,nu,ncp/scale)*scale
    
    # Lambda-prime distribution (Lecoutre, 1999)
    rlambdaprime = function(n,nu,ncp,scale) rnorm(n,0,scale) + ncp*sqrt(rchisq(n,nu)/nu)
    plambdaprime = function(x,nu,ncp,scale) 1-pt(ncp/scale,nu,x/scale)
    qlambdaprime = function(p,nu,ncp,scale) {

      t = ncp/scale
      k = exp( ((log(2)-log(nu))/2) + lgamma((nu+1)/2) - lgamma(nu/2) )
      M = k*t
      V = 1 + t**2 - M**2

      N = 100
      from = M - 4.5*sqrt(V)
      to   = M + 4.5*sqrt(V)

      for(iter in 1:3) {
      
        range = seq(from,to,len=N)
        index = which.min(abs(p - plambdaprime(range,nu,ncp,scale)))
        from = range[index-1]
        to = range[index+1]
      }
      
      range[index]
    }
    dlambdaprime = function(x,nu,ncp,scale) {
    
      # Numerical derivatives
      dx = .0001
      dF1 = plambdaprime(x-dx/2,nu,ncp,scale)
      dF2 = plambdaprime(x+dx/2,nu,ncp,scale)
      
      (dF2-dF1)/dx
    }

    dfunction = eval(parse(text=paste("d",probf[distrib],sep="")))
    pfunction = eval(parse(text=paste("p",probf[distrib],sep="")))
    qfunction = eval(parse(text=paste("q",probf[distrib],sep="")))
    rfunction = eval(parse(text=paste("r",probf[distrib],sep="")))
    
    # Chosen distribution has two parameters
    if(is2P) {
    
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
    else if(is1P) {
    
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
    
    # Chosen distribution has 3 parameters
    else if(is3P) {
          
      # prob. to quantile
      if(p) {
      
        prob = value
        
        # Continuous distribution
        if(!isDiscrete) {
          if(right) value = qfunction(1-prob,param1,param2,param3)
          else      value = qfunction(prob,param1,param2,param3)
        }
        
        # Discrete distribution
        else {
          if(right) value = qfunction(1-prob,param1,param2,param3)
          else      value = qfunction(prob,param1,param2,param3)
        }
      }
      
      # Quantile to prob.
      else {
      
        # Continuous distribution
        if(!isDiscrete) {
          if(right) prob = 1-pfunction(value,param1,param2,param3)
          else      prob = pfunction(value,param1,param2,param3)
        }
        
        # Discrete distribution
        else {
          if(right)  prob = 1-pfunction(value-1,param1,param2,param3)
          else       prob = pfunction(value,param1,param2,param3)
        }
      }
      
      dens = dfunction(value,param1,param2,param3)
    }
    
    # Result
    svalue(.$resultx) = paste(value)
    svalue(.$resultp) = paste(prob)
    
    # Construction du graphique
    xlab="X"
    title = paste("Distribution :",distrib)
    ylab = expression(f(X==x))
    from = 0
    
    # Two-parameter distribution
    if(is2P) {
    
      # Continuous distribution
      if(!isDiscrete) { 
        from = ifelse(distrib=="Normale",param1-4*param2,0)
        from = min(from,value,na.rm=TRUE)
        to = ifelse(distrib=="Normale",param1+4*param2,max(rfunction(1000,param1,param2),na.rm=TRUE))
        to = max(to,value,na.rm=TRUE)
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
    else if(is1P) {
    
      # Continuous distribution
      if(!isDiscrete) {
      
        from = ifelse(distrib=="Student",min(rfunction(1000,param1)),0)
        from = min(from,value,na.rm=TRUE)
        to = max(rfunction(1000,param1),na.rm=TRUE)
        to = max(to,value,na.rm=TRUE)
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
        to = max(rfunction(1000,param1),na.rm=TRUE)
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
    
    # Three-parameter distribution
    else if(is3P) {
    
      # Continuous distribution
      if(!isDiscrete) { 
        sample = rfunction(1000,param1,param2,param3)
        from = min(sample,value,na.rm=TRUE)
        to = max(sample,value,na.rm=TRUE)
        curve(dfunction(x,param1,param2,param3),n=1000,from=from,to=to,lwd=2,main=title,xlab=xlab,ylab=ylab)
        if(!right) {
          z = c(seq(from,value,len=1000),value,from)
          dz = c(dfunction(z[1:1000],param1,param2,param3),0,0)
          polygon(z,dz,density=-1,col="red",lwd=2)
        }
        else {
          z = c(seq(value,to,len=1000),to,value)
          dz = c(dfunction(z[1:1000],param1,param2,param3),0,0)
          polygon(z,dz,density=-1,col="red",lwd=2)
        }
      }
      
      # Discrete distribution
      else {
        from = 0
        to = max(rfunction(1000,param1,param2,param3),value,na.rm=TRUE)
        z = 0:to
        plot(z,dfunction(z,param1,param2,param3),type="h",lwd=2,main=title,xlab=xlab,ylab=ylab)
        
        if(!right) {
          for(i in 0:(value-1)) { lines(rbind(c(i,0),c(i,dfunction(i,param1,param2,param3))),lwd=2,col="red") }
          lines(rbind(c(value,0),c(value,prob-pfunction(value-1,param1,param2,param3))),lwd=2,col="red")
        }
        else {
          for(i in param1:(value+1)) {
          lines(rbind(c(i,0),c(i,dfunction(i,param1,param2,param3))),lwd=2,col="red") }
          lines(rbind(c(value,0),c(value,prob-1+pfunction(value,param1,param2,param3))),lwd=2,col="red")
        }
      }
    } 

  },
  initOptions = function(.,h,...) {

    distrib = svalue(.$distribution)
    param2 = eval(parse(text=svalue(.$param2)))
    
    is1P = distrib %in% c("Student","Chi-2","Poisson")
    is2P = distrib %in% c("Normale","Uniforme","Chi-2 inverse","Chi-2 non centrale","Fisher","Binomiale","Gamma","Gamma inverse","Beta")
    is3P = distrib %in% c("Student non standard","Lambda prime","Student non centrale","Fisher non centrale")
    
    # Warning: a droplist may be temporarily set to NULL in gWidgets when changed
    if(is.null(distrib)) return()
    
    svalue(.$paramLabel1) = .$paramNames[[distrib]][1]
    svalue(.$paramLabel2) = .$paramNames[[distrib]][2]
    svalue(.$paramLabel3) = .$paramNames[[distrib]][3]
    
    if(is1P) {
      svalue(.$param2) = ""
      enabled(.$param2) = FALSE
      svalue(.$param3) = ""
      enabled(.$param3) = FALSE
    }

    if(is2P) {
      svalue(.$param2) = ""
      enabled(.$param2) = TRUE
      svalue(.$param3) = ""
      enabled(.$param3) = FALSE
    }

    if(is3P) {
      svalue(.$param2) = ""
      enabled(.$param2) = TRUE
      svalue(.$param3) = ""
      enabled(.$param3) = TRUE
    }   
    
    svalue(.$resultx) = ""
    svalue(.$resultp) = ""    
  },

  #---------------------------------------------------------------------------------------
  #  SLOT                   INITIAL VALUE                          CONTENT
  #---------------------------------------------------------------------------------------
  availDists   = c(Normale="norm",Uniforme="unif",Binomiale="binom",Poisson="pois",                                  #     Distributions disponibles
                   Student="t","Student non standard"="nst","Student non centrale"="nct",
                  "Chi-2"="chisq","Chi-2 inverse"="scaledInvChi2","Chi-2 non centrale"="ncchisq",
                   Fisher="f","Fisher non centrale"="ncf",
                   Gamma="gamma","Gamma inverse"="invgamma",
                   Beta="beta","Lambda prime"="lambdaprime"),       
  paramNames   = list(Uniforme              = c("Borne gauche","Borne droite  ","       "),          #    Noms des paramètres de lois
                      Binomiale             = c("Effectif    ","Probabilité   ","       "),
                      Normale               = c("Moyenne     ","Ecart-type    ","       "),
                      Gamma                 = c("Forme       ","Echelle       ","       "),
                     "Gamma inverse"        = c("Forme       ","Echelle       ","       "),
                     "Chi-2"                = c("ddl         ","              ","       "),
                     "Chi-2 inverse"        = c("ddl         ","Echelle       ","       "),
                     "Chi-2 non centrale"   = c("ddl         ","Non centralité","       "),
                      Beta                  = c("alpha       ","beta          ","       "),
                      Poisson               = c("Moyenne     ","              ","       "),
                      Student               = c("ddl         ","              ","       "),
                     "Student non standard" = c("ddl         ","Centre        ","Echelle"),
                     "Student non centrale" = c("ddl         ","Non-centralité","Echelle"),
                     "Fisher"               = c("ddl1        ","ddl2          ","       "),
                     "Fisher non centrale"  = c("ddl1        ","ddl2          ","Non centralité"),
                     "Lambda prime"         = c("ddl         ","Non-centralité","Echelle")),
  distribution = NULL,                                 #    Distribution choisie
  calcWhat     = NULL,                                 #    Type de calcul (de quantile à prob. ou l'inverse)
  side         = NULL,                                 #    Cumul à droite ou à gauche
  param1       = NULL,                                 #    Valeur du paramètre 1 de la loi 
  param2       = NULL,                                 #    Valeur du paramètre 2 de la loi (s'il y en a)   
  param3       = NULL,                                 #    Valeur du paramètre 3 de la loi (s'il y en a)   
  paramLabel1  = NULL,                                 #    Nom du paramètre 1 de la loi
  paramLabel2  = NULL,                                 #    Nom du paramètre 2 de la loi (s'il y en a)
  paramLabel3  = NULL,                                 #    Nom du paramètre 3 de la loi (s'il y en a)
  value        = NULL,                                 #    Valeur numérique de départ (quantile ou probabilité)
  resultx      = NULL,                                 #    Résultat quantile
  resultp      = NULL                                  #    Résultat probabilité
)


