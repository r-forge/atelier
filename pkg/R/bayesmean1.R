#-----------------------------------------------------------------------------------------
#            Inférence bayésienne sur une moyenne (loi a priori non-informative)
#-----------------------------------------------------------------------------------------
.ws7 = proto(

  create = function(.,h,...) {
  
    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Inférence bayésienne\nsur une moyenne\n(non informatif)" %in% names(.ws$nb)) return()
    
   .$priorparam2 = gedit("",width=15,handler=.$updatePlot)
    enabled(.$priorparam2) = FALSE
   .$priorn      = gedit("1",width=15,coerce.with=as.numeric)
   .$sdFixed = gcheckbox("Valeur connue :",handler=.$onCheck)
   .$priorlabel = glabel("Loi uniforme (en log)")

    add(.ws$nb,group <- ggroup(horizontal=FALSE),label="Inférence bayésienne\nsur une moyenne\n(non informatif)")
    tmp = gframe("Loi a priori", horizontal=FALSE,container = group,expand=TRUE)
    priorGroup = glayout(container=tmp)
    priorGroup[2,2,anchor=c(-1,0)] = glabel("Moyenne")
    priorGroup[2,3] = "Loi uniforme"
    priorGroup[3,2,anchor=c(-1,0)]=glabel("Ecart-type")
    priorGroup[3,3] = .$priorlabel
    priorGroup[4,3]=.$sdFixed
    priorGroup[5,3]=.$priorparam2
    visible(priorGroup)=TRUE

   .$xbar = gedit("",width=15,handler=.$updatePlot)
   .$n = gedit("",width=15,coerce.with=as.numeric,handler=.$updatePlot)
   .$s = gedit("",width=15)

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
   .$postprob = glabel("")
   .$op = gdroplist(c("H : mu <","H : mu >"),handler=.$updatePlot)

    tmp = gframe("Test d\'hypothèse", container = group,expand=TRUE)
    testGroup = glayout(container=tmp)
    testGroup[2,2]=.$op
    testGroup[2,3]=.$value
    testGroup[3,2]=glabel("Pr(H|D)")
    testGroup[3,3]=.$postprob
    visible(testGroup)=TRUE

    addSpring(group)

    buttonGroup=ggroup(container=group)
    addSpring(buttonGroup)
    gbutton("  Afficher  ",container=buttonGroup, handler=.$updatePlot)

  },
  
  onCheck = function(.,h,...) {
    
    if(svalue(.$sdFixed)) {
      enabled(.$priorparam2) = TRUE
      enabled(.$s) = FALSE
      svalue(.$priorlabel) = "Fixé"
    }
    else {
      svalue(.$priorparam2) = ""
      enabled(.$priorparam2) = FALSE
      enabled(.$s) = TRUE
      svalue(.$priorlabel) = "Loi uniforme (en log)"
    }
  },
  
  updatePlot = function(.,h,...) {
    
    # Vérification des paramètres
    if(is.na(svalue(.$n))) {
      gmessage("Entrez l'effectif observé.")
      return()
    }

    if(is.na(svalue(.$xbar))) {
      gmessage("Entrez la moyenne observée.")
      return()
    }

    if(!(svalue(.$sdFixed)) && is.na(svalue(.$s))) {
      gmessage("Entrez la variance observée.")
      return()
    }

    # Get input info
    xbar = eval(parse(text=svalue(.$xbar)))
    n = svalue(.$n)
    s = eval(parse(text=svalue(.$s)))
    conf = svalue(.$level)
    prior.sd = eval(parse(text=svalue(.$priorparam2)))
    m0 = eval(parse(text=svalue(.$value)))
    op = svalue(.$op)
        
    # Case 1: Variance known
    if(svalue(.$sdFixed)) {
      
      post.mean = xbar
      post.sd = prior.sd/sqrt(n)
      x = seq(post.mean-3*post.sd,post.mean+3*post.sd,len=500)
      prior = rep(1/prior.sd,length(x))
    
      post = dnorm(x,post.mean,post.sd)
      q1 = qnorm((1-conf)/2,post.mean,post.sd)
      q2 = qnorm((1+conf)/2, post.mean, post.sd)   

      svalue(.$postprob) = ""
      
      # Bayesian test (if some normative value has been provided)
      if(!is.null(m0)) {
      
        # One-sided test
        postH = pnorm(m0, post.mean, post.sd)
        postA = 1 - postH
        post.prob = ifelse(op == "H : mu <",postH,postA)
        svalue(.$postprob) = round(post.prob,4)
      }
    }

    # Case 2: Variance unknown
    else {
    
      post.mean = xbar
      scale = s/sqrt(n)
      post.sd = scale * sqrt((n-1)/(n-3))
      x = seq(post.mean-3*post.sd,post.mean+3*post.sd,len=500)
      prior = rep(1/s,length(x))
    
      post = dt((x-post.mean)/scale,n-1)/scale
      q1 = qt((1-conf)/2,n-1)*scale + post.mean
      q2 = qt((1+conf)/2,n-1)*scale + post.mean

      svalue(.$postprob) = ""
      
      # Bayesian test (if some normative value has been provided)
      if(!is.null(m0)) {
      
        # One-sided test
        if(op == "H : mu <") postH = pt((m0-post.mean)/scale, n-1)
        else                 postH = 1 - pt((m0-post.mean)/scale, n-1)

        svalue(.$postprob) = round(postH,4)
      }
    }

    m = max(c(prior, post))
      
    svalue(.$interval) = paste("[",round(q1,4),";",round(q2,4),"]")
    svalue(.$postmean) = round(post.mean,4)
    svalue(.$postsd)   = round(post.sd,4)
    
    # Plot posterior density
    plot(x, post, type = "l", xlab="Moyenne",ylab = "Densité", lty = 1, lwd = 2, main = "Probabilité a posteriori",ylim = c(0,m))
    z = seq(q1,q2,len=500)

    # One-sided regions
    if(!is.null(m0)) {  
      if(op == "H : mu >") z = seq(m0,max(x),len=500)
      else                 z = seq(min(x),m0,len=500)        
    }
    
    if(svalue(.$sdFixed)) dz = dnorm(z,post.mean,post.sd)
    else                  dz = dt((z-post.mean)/scale,n-1)/scale
    polygon(c(z,max(z),min(z)),c(dz,0,0),density=-1,col="lightgrey",lwd=2)

    lines(x, prior, lty = 3, lwd = 2, col = "darkgreen")
    if(is.null(m0)) text(post.mean,max(post)/3,cex=1.3,paste(round(conf*100),"%",sep=""))
    legend("topright", c("Loi a priori","Loi a Posteriori"), cex=.8,lty = c(3,2,1), lwd = rep(2,3), col = c("darkgreen","black"),inset=0.01,bg="white")
  },
  
  #---------------------------------------------------------------------------------------
  #  SLOT            INITIAL VALUE                           CONTENT
  #---------------------------------------------------------------------------------------
  priorparam1     =  NULL,                     #
  priorparam2     =  NULL,                     #
  priorlabel      =  NULL,                     #
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


