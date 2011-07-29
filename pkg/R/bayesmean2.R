#-----------------------------------------------------------------------------------------
#            Inférence bayésienne sur une moyenne (loi a priori informative)
#-----------------------------------------------------------------------------------------
.ws8 = proto(

  create = function(.,h,...) {
  
    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Inférence bayésienne\nsur une moyenne" %in% names(.ws$nb)) return()
    
   .$priorparam1 = gedit("",width=15,handler=.$updatePlot)
   .$priorparam2 = gedit("",width=15,handler=.$updatePlot)
   .$priorn      = gedit("2",width=15,coerce.with=as.numeric,handler=.$updatePlot)

    add(.ws$nb,group <- ggroup(horizontal=FALSE),label="Inférence bayésienne\nsur une moyenne")
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
    enabled(.$s) = FALSE
   .$sdFixed = gradio(c("Connu","Estimé"),horizontal=TRUE,handler=.$onCheck)

    tmp = gframe("Données observées", container = group,expand=TRUE)
    dataGroup = glayout(container=tmp)
    dataGroup[2,2,anchor=c(-1,0)]=glabel("Effectif")
    dataGroup[2,3]=.$n
    dataGroup[3,2,anchor=c(-1,0)]=glabel("Moyenne")
    dataGroup[3,3]=.$xbar
    dataGroup[4,2,anchor=c(-1,0)]=glabel("Ecart-type")
    dataGroup[4,3]=.$s
    dataGroup[5,3]=.$sdFixed
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
   .$op = gdroplist(c("H : µ <","H : µ =","H : µ >"),handler=.$updatePlot)

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
  
  onCheck = function(.,h,...) {
    
    if(svalue(.$sdFixed)=="Connu") {
      svalue(.$s) = ""
      enabled(.$s) = FALSE
    }
    else {
      enabled(.$s) = TRUE
    }
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
        
    # Case 1: Variance known
    if(svalue(.$sdFixed)=="Connu") {
      
      post.var = prior.var / (n+prior.n)
      post.sd = sqrt(post.var)
      post.mean = (xbar * n + prior.mean*prior.n)/(n+prior.n)
      x = seq(post.mean-3*post.sd,post.mean+3*post.sd,len=500)
      prior = dnorm(x,prior.mean,prior.sd/sqrt(prior.n))
    
      post = dnorm(x,post.mean,post.sd)
      q1 = qnorm((1-conf)/2,post.mean,post.sd)
      q2 = qnorm((1+conf)/2, post.mean, post.sd)   
      lh = dnorm(xbar,x,prior.sd/sqrt(n))

      svalue(.$bf) = ""
      svalue(.$postprob) = ""
      
      # Bayesian test (if some normative value has been provided)
      if(!is.null(m0) && !is.null(prior.prob)) {
      
        # Default two-sided test
        BF = dnorm(m0,post.mean,post.sd)/dnorm(m0,prior.mean,prior.sd/sqrt(prior.n))
        post.prob = prior.prob * BF/(prior.prob * BF + 1 - prior.prob)
        
        # One-sided test
        if(op != "H : µ =") {
          priorH = pnorm(m0, prior.mean, prior.sd/sqrt(prior.n))
          priorA = 1 - priorH
          prior.odds = priorH/priorA
          postH = pnorm(m0, post.mean, post.sd)
          postA = 1 - postH
          post.odds = postH/postA
          BF = post.odds/prior.odds
          post.prob = postH
          if(op=="H : µ >") { BF = 1/BF     ; post.prob = postA }
        }
        svalue(.$bf) = round(BF,4)
        svalue(.$postprob) = round(post.prob,4)
      }
    }
    
    # Case 2: Variance unknown
    else {
    
      # Vérification des paramètres
      if(is.null(s)) {
        gmessage("Entrez une valeur d'écart-type estimé.")
        return()
      }
      post.n = prior.n + n
      nu0 = prior.n - 1

      if(nu0<1) {
        gmessage("Le paramètre d'effectif a priori est trop faible (0 ddl).")
        return()
      }
    
      nu.n = nu0 + n
      post.mean = (n*xbar + prior.n*prior.mean)/post.n
      scale = sqrt((nu0*prior.var + (n-1)*s**2 + (prior.n*n)*((xbar-prior.mean)**2)/post.n)/nu.n)
      post.sd = (scale/sqrt(post.n)) * sqrt((post.n-1)/(post.n-3))
      
      left.lim  = qt((1-.995)/2,nu.n)*(scale/sqrt(post.n)) + post.mean
      right.lim = qt((1+.995)/2,nu.n)*(scale/sqrt(post.n)) + post.mean      
      x = seq(left.lim,right.lim,len=500)
      post = dt(sqrt(post.n)*(x-post.mean)/scale,nu.n)/(scale/sqrt(post.n))
      q1 = qt((1-conf)/2,nu.n)*(scale/sqrt(post.n)) + post.mean
      q2 = qt((1+conf)/2,nu.n)*(scale/sqrt(post.n)) + post.mean
      prior = dt(sqrt(prior.n)*(x-prior.mean)/prior.sd,nu0)/(prior.sd/sqrt(prior.n))
      lh = dt(sqrt(n)*(xbar-x)/s,n-1)/(scale/sqrt(post.n))

      svalue(.$bf) = ""
      svalue(.$postprob) = ""
      
      # Bayesian test (if some normative value has been provided)
      if(!is.null(m0) && !is.null(prior.prob)) {
      
        # Default two-sided test
        BF = (dt(sqrt(prior.n)*(m0-prior.mean)/prior.sd,nu0)/(prior.sd/sqrt(prior.n)))/(dt(sqrt(post.n)*(m0-post.mean)/scale,nu.n)/(scale/sqrt(post.n)))
        post.prob = prior.prob * BF/(prior.prob * BF + 1 - prior.prob)
        
        if(op != "H : µ =") {
          priorH = pt(sqrt(prior.n)*(m0-prior.mean)/prior.sd,nu0)
          priorA = 1 - priorH
          prior.odds = priorH/priorA
          postH = pt(sqrt(post.n)*(m0-post.mean)/scale,nu.n)
          postA = 1 - postH
          post.odds = postH/postA
          BF = post.odds/prior.odds
          post.prob = postH
          if(op=="H : µ >") { BF = 1/BF     ; post.prob = postA }
        }        

      svalue(.$bf) = round(BF,4)
      svalue(.$postprob) = round(post.prob,4)
      }
    }

    m = max(c(prior, lh, post))
      
    svalue(.$interval) = paste("[",round(q1,4),";",round(q2,4),"]")
    svalue(.$postmean) = round(post.mean,4)
    svalue(.$postsd)   = round(post.sd,4)

    plot(x, post, type = "l", xlab="Moyenne",ylab = "Densité", lty = 1, lwd = 2, main = "Probabilité a posteriori",ylim = c(0,m))
    z = seq(q1,q2,len=500)

    # One-sided regions
    if(!is.null(m0) && (op != "H : µ =")) {  
      if(op == "H : µ >") z = seq(m0,max(x),len=500)
      else                z = seq(min(x),m0,len=500)        
    }
    if(svalue(.$sdFixed)=="Connu") dz = dnorm(z,post.mean,post.sd)
    else                           dz = dt(sqrt(post.n)*(z-post.mean)/scale,nu.n)/(scale/sqrt(post.n))
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
  sdFixed         =  NULL,                     #
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


