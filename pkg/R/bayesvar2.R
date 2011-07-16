#-----------------------------------------------------------------------------------------
#            Inférence bayésienne sur une moyenne (loi a priori informative)
#-----------------------------------------------------------------------------------------
.ws11 = proto(

  create = function(.,h,...) {
  
    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Inférence bayésienne\nsur une variance" %in% names(.ws$nb)) return()
    
   .$priorparam1 = gedit("",width=15,handler=.$updatePlot)
   .$priorparam2 = gedit("",width=15,handler=.$updatePlot)
   .$priorn      = gedit("2",width=15,coerce.with=as.numeric,handler=.$updatePlot)

    add(.ws$nb,group <- ggroup(horizontal=FALSE),label="Inférence bayésienne\nsur une variance")
    tmp = gframe("Loi normale a priori", horizontal=FALSE,container = group,expand=TRUE)
    priorGroup = glayout(container=tmp)
    priorGroup[2,2,anchor=c(-1,0)]=glabel("Effectif")
    priorGroup[2,3]=.$priorn
    priorGroup[3,2,anchor=c(-1,0)]=glabel("Moyenne")
    priorGroup[3,3]=.$priorparam1
    priorGroup[4,2,anchor=c(-1,0)]=glabel("Ecart-type")
    priorGroup[4,3]=.$priorparam2
    visible(priorGroup)=TRUE

   .$xbar = gedit("",width=15,handler=.$updatePlot)
   .$n = gedit("",width=15,coerce.with=as.numeric,handler=.$updatePlot)
   .$s = gedit("",width=15,handler=.$updatePlot)

    tmp = gframe("Données observées", container = group,expand=TRUE)
    dataGroup = glayout(container=tmp)
    dataGroup[2,2,anchor=c(-1,0)]=glabel("Effectif")
    dataGroup[2,3]=.$n
    dataGroup[3,2,anchor=c(-1,0)]=glabel("Moyenne")
    dataGroup[3,3]=.$xbar
    dataGroup[4,2,anchor=c(-1,0)]=glabel("Ecart-type")
    dataGroup[4,3]=.$s
    visible(dataGroup)=TRUE

   .$level = gedit("0.95",width=5,coerce.with=as.numeric,handler=.$updatePlot)
   .$postmean = glabel("")
   .$postsd = glabel("")
   .$interval = glabel("")

    tmp = gframe("Statistiques a posteriori", container = group,expand=TRUE)
    statsGroup = glayout(container=tmp)
    statsGroup[2,2,anchor=c(-1,0)]=glabel("Niveau")
    statsGroup[2,3]=.$level
    statsGroup[3,2]=glabel("Crédibilité")
    statsGroup[3,3:4]=.$interval
    statsGroup[4,2]=glabel("Moyenne")
    statsGroup[4,3]=.$postmean
    statsGroup[5,2]=glabel("Ecart-type")
    statsGroup[5,3]=.$postsd
    visible(statsGroup)=TRUE

   .$value = gedit("",width=10,handler=.$updatePlot)
   .$priorprob = gedit("0.5",width=5,handler=.$updatePlot)
   .$bf = glabel("")
   .$postprob = glabel("")
   .$op = gdroplist(c("H : σ² <","H : σ² =","H : σ² >"),handler=.$updatePlot)

    tmp = gframe("Test d\'hypothèse", container = group,expand=TRUE)
    testGroup = glayout(container=tmp)
    testGroup[2,2]=.$op
    testGroup[2,3]=.$value
    testGroup[3,2,anchor=c(-1,0)]=glabel("Pr(H) a priori")
    testGroup[3,3]=.$priorprob
    testGroup[4,2]=glabel("Facteur de Bayes")
    testGroup[4,3]=.$bf
    testGroup[5,2]=glabel("Pr(H|D)")
    testGroup[5,3]=.$postprob
    visible(testGroup)=TRUE

    addSpring(group)

    buttonGroup=ggroup(container=group)
    addSpring(buttonGroup)
    gbutton("  Afficher   ",container=buttonGroup, handler=.$updatePlot)

  },
  
  updatePlot = function(.,h,...) {
    
    # Vérification des paramètres
    if(any(is.na(c(svalue(.$priorn),svalue(.$priorparam1),svalue(.$priorparam2))))) {
      gmessage("Spécifiez des valeurs de paramètres pour la loi a priori.")
      return()
    }
    
    # Vérification des paramètres
    if(any(is.na(c(svalue(.$xbar),svalue(.$n))))) {
      gmessage("Indiquez des statistiques observées.")
      return()
    }

    # Get input info
    xbar = eval(parse(text=svalue(.$xbar)))
    n = svalue(.$n)
    s = eval(parse(text=svalue(.$s)))
    conf = svalue(.$level)
    prior.mean = eval(parse(text=svalue(.$priorparam1)))
    prior.sd = eval(parse(text=svalue(.$priorparam2)))
    prior.var = prior.sd^2
    prior.precision = 1/prior.var
    prior.n = svalue(.$priorn)
    m0 = eval(parse(text=svalue(.$value)))
    op = svalue(.$op)
    prior.prob = eval(parse(text=svalue(.$priorprob)))
        
    # Vérification des paramètres
    if(is.na(svalue(.$s))) {
      gmessage("Entrez une valeur d'écart-type estimé.")
      return()
    }
    post.n = prior.n + n
    nu0 = prior.n - 1
    
    if(nu0<1) {
      gmessage("Le paramètre d'effectif a priori est trop faible (0 ddl).")
      return()
    }
    
    # dinvgamma = function(z,a,b) (b**a)/gamma(a) * z**(-a-1) * exp(-b/z)
    dinvgamma = function(z,a,b) exp(a*log(b) - lgamma(a) -(a+1)*log(z) -(b/z))
    qinvgamma = function(p,a,b) ifelse(((1 - p) <= .Machine$double.eps),Inf,1/qgamma(1-p,a,b))
    pinvgamma = function(z,a,b) 1-pgamma(1/z,a,b)
    rinvgamma = function(n,a,b) 1/rgamma(n=n,shape=a,rate=b)
    
    dscaledInvChi2 = function(z,nu,s2) dinvgamma(z,nu/2,nu*s2/2)
    pscaledInvChi2 = function(q,nu,s2) pinvgamma(q,nu/2,nu*s2/2)
    qscaledInvChi2 = function(p,nu,s2) qinvgamma(p,nu/2,nu*s2/2)
    rscaledInvChi2 = function(n,nu,s2) rinvgamma(n,nu/2,nu*s2/2)
    
    nu.n = nu0 + n
    s2.n = (nu0*prior.var + (n-1)*s**2 + (prior.n*n)*((xbar-prior.mean)**2)/post.n)/nu.n
    left.lim  = 0
    right.lim = qscaledInvChi2(.999,nu.n,s2.n)
    x = seq(left.lim,right.lim,len=500)
    post = dscaledInvChi2(x,nu.n,s2.n)
    q1 = qscaledInvChi2((1-conf)/2,nu.n,s2.n)
    q2 = qscaledInvChi2((1+conf)/2,nu.n,s2.n)
    prior = dscaledInvChi2(x,nu0,prior.var)
    lh = (n-1)*dchisq((n-1)*(s**2)/x,n-1)/x   

    svalue(.$bf) = ""
    svalue(.$postprob) = ""
      
    # Bayesian test (if some normative value has been provided)
    if(!is.null(m0) && !is.null(prior.prob)) {
      
      # Default two-sided test (Savage-Dickey ratio)
      BF = dscaledInvChi2(m0,nu0,prior.var)/dscaledInvChi2(m0,nu.n,s2.n)
      post.prob = prior.prob * BF/(prior.prob * BF + 1 - prior.prob)
      
      if(op != "H : σ² =") {
        priorH = pscaledInvChi2(m0,nu0,prior.var)
        priorA = 1 - priorH
        prior.odds = priorH/priorA
        postH = pscaledInvChi2(m0,nu.n,s2.n)
        postA = 1 - postH
        post.odds = postH/postA
        BF = post.odds/prior.odds
        post.prob = postH
        if(op=="H : σ² >") { BF = 1/BF ; post.prob = postA }
      }        

      svalue(.$bf) = round(BF,4)
      svalue(.$postprob) = round(post.prob,4)
    }

    # m = max(c(prior, lh, post),na.rm=TRUE)
    m = max(c(prior, post),na.rm=TRUE)

    svalue(.$interval) = paste("[",round(q1,4),";",round(q2,4),"]")
    post.mean = "Non définie"
    if(nu.n > 2) post.mean = round((nu.n/(nu.n-2)) * s2.n,4)
    svalue(.$postmean) = post.mean
    post.sd = "Non défini"
    if(nu.n > 4) post.sd = round(sqrt(((s2.n**2)*2*nu.n**2)/((nu.n-4)*(nu.n-2)**2)),4)
    svalue(.$postsd) = post.sd

    plot(x, post, type = "l", xlab="Variance",ylab = "Densité", lty = 1, lwd = 2, main = "Probabilité a posteriori",ylim = c(0,m))
    z = seq(q1,q2,len=500)

    # One-sided regions
    if(!is.null(m0) && (op != "H : mu =")) {  
      if(op == "H : σ² >") z = seq(m0,max(x),len=500)
      else                 z = seq(min(x),m0,len=500)        
    }

    dz = dscaledInvChi2(z,nu.n,s2.n)
    polygon(c(z,max(z),min(z)),c(dz,0,0),density=-1,col="lightgrey",lwd=2)

    lines(x, lh, lty = 2, lwd = 2, col = "red")
    lines(x, prior, lty = 3, lwd = 2, col = "darkgreen")
    if(is.null(m0)) text(post.mean,max(post)/3,cex=1.3,paste(round(conf*100),"%",sep=""))
    legend("topright", c("Loi a priori", "Vraisemblance", "Loi a Posteriori"), cex=.8,lty = c(3,2,1), lwd = rep(2,3), col = c("darkgreen", "red", "black"),inset=0.01,bg="white")

  },
  
  #---------------------------------------------------------------------------------------
  #  SLOT            INITIAL VALUE                           CONTENT
  #---------------------------------------------------------------------------------------
  priorparam1     =  NULL,                     #
  priorparam2     =  NULL,                     #
  priorn          =  NULL,                     #
  xbar            =  NULL,                     #
  n               =  NULL,                     #
  s               =  NULL,                     #
  level           =  NULL,                     #
  postmean        =  NULL,                     #
  postsd          =  NULL,                     #
  interval        =  NULL,                     #
  value           =  NULL,                     #
  priorprob       =  NULL,                     #
  bf              =  NULL,                     #
  postprob        =  NULL,                     #
  op              =  NULL                      #
)


