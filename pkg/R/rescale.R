#-----------------------------------------------------------------------------------------
#                           Changement d'origine et d'échelle
#-----------------------------------------------------------------------------------------
.ws2 = proto(

  create = function(.,h,...) {
   
    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Changement\nde variable" %in% names(.ws$nb)) return()
    
   .$nobs = gradio(c(50, 500, 50000),handler=.$updatePlot,coerce.with=as.numeric)
   .$param1 = gedit("100",width=5,coerce.with=as.numeric)
   .$param2 = gedit("15",width=5,coerce.with=as.numeric)
   .$add = gedit("0",width=5)
   .$mult = gedit("1",width=5)
   .$standard = gcheckbox("Standardiser l\'échantillon",handler=.$updatePlot)
   .$xbar = glabel("")
   .$s = glabel("")

    # Construction de l'interface
    add(.ws$nb,group <- ggroup(horizontal=FALSE),label="Changement\nde variable")

    # Distribution
    tmp = gframe("Paramètres de population", container = group)
    distribGroup = glayout(container= tmp)
    distribGroup[2,2:3]=glabel("Loi normale")
    distribGroup[3,2,anchor=c(-1,0)]=glabel("Moyenne")
    distribGroup[3,3]=.$param1
    distribGroup[4,2,anchor=c(-1,0)]=glabel("Ecart-type")
    distribGroup[4,3]=.$param2
    visible(distribGroup)=TRUE

    # Effectifs          
    tmp = gframe("Nombre d\'observations", container = group)
    add( tmp,.$nobs)

    # Transformation
    tmp = gframe("Transformation", container = group, horizontal=FALSE)
    transfoGroup = glayout(container= tmp)
    transfoGroup[2,2:3]=glabel(" X' = aX + b ")
    transfoGroup[3,2,anchor=c(-1,0)]=glabel(" a = ")
    transfoGroup[3,3,expand=TRUE]=.$mult
    transfoGroup[4,2,anchor=c(-1,0)]=glabel(" b = ")
    transfoGroup[4,3,expand=TRUE]=.$add
    visible(transfoGroup)=TRUE
    add( tmp,.$standard)

    # Statistiques descriptives
    tmp = gframe("Statistiques descriptives", container = group, horizontal=FALSE)
    resultGroup = glayout(container= tmp)
    resultGroup[2,2] = glabel("Moyenne :")
    resultGroup[2,3] = .$xbar
    resultGroup[3,2] = glabel("Ecart-type :")
    resultGroup[3,3] = .$s

    addSpring(group)

    # Boutons de commande
    buttonGroup=ggroup(container=group)
    addSpring(buttonGroup)
    gbutton(" Echantillonner ",container=buttonGroup, handler=.$updatePlot)

  },
  
  updatePlot = function(.,h,...) {
  
    # Vérification des paramètres
    if(any(is.na(c(svalue(.$param1),svalue(.$param2))))) {
      gmessage("Spécifiez des valeurs de paramètres.")
      return()
    }

    if(is.na(svalue(.$add)))  svalue(.$add)  = 0
    if(is.na(svalue(.$mult))) svalue(.$mult) = 1

    nobs = svalue(.$nobs)
    param1 = svalue(.$param1)
    param2 = svalue(.$param2)
    add = eval(parse(text=svalue(.$add)))
    mult = eval(parse(text=svalue(.$mult)))

    # Génération des données
    y = rnorm(nobs,param1,param2)
    m1 = mean(y)
    sd1 = sd(y)
    
    # Transformation des données
    if(svalue(.$standard)) y = as.numeric(scale(y))
    y = y * mult + add
    m2 = mean(y)
    sd2 = sd(y)
    
    # Affichage des stats descriptives
    svalue(.$xbar)=paste(round(m2,3))
    svalue(.$s)=paste(round(sd2,3))

    # Paramètres transformés de population
    new.mu = param1*mult+add
    new.sigma = param2*abs(mult)
    
    # Représentation graphique
    hist(y,xlab="Variable",ylab="Densité",main="Distribution normale",freq=FALSE)
    if(svalue(.$standard)) curve(dnorm(x,0,1),from=-4,to=4,add=TRUE,lwd=2,col="blue")
    else curve(dnorm(x,param1*mult+add,param2*abs(mult)),from=min(y),to=max(y),add=TRUE,lwd=2,col="blue")
    
    # Afficher moyenne vraie transformée en rouge
    abline(v=new.mu,lwd=2,col="red")
    
    # Afficher écart-type vrai transformé en vert
    lines(rbind(c(new.mu,dnorm(new.mu+new.sigma,new.mu,new.sigma)),c(new.mu+new.sigma,dnorm(new.mu+new.sigma,new.mu,new.sigma))),col="green",lwd=2)

  },
  
  #---------------------------------------------------------------------------------------
  #  SLOT         INITIAL VALUE                  CONTENT
  #---------------------------------------------------------------------------------------
  nobs            = NULL,                    #
  param1          = NULL,                    #
  param2          = NULL,                    #
  add             = NULL,                    #
  mult            = NULL,                    #
  standard        = NULL,                    #
  xbar            = NULL,                    #
  s               = NULL                     #
)


