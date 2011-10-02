#-----------------------------------------------------------------------------------------
#                  Inférence bayésienne sur plusieurs proportions
#-----------------------------------------------------------------------------------------

.ws10 = proto(

  create = function(.,h,...) {

    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Inférence bayésienne\nsur table de contingence" %in% names(.ws$nb)) return()
    add(.ws$nb,group <- ggroup(horizontal=FALSE),label="Inférence bayésienne\nsur table de contingence")
    
    add(group,tmp <- gframe("Données observées"),expand=TRUE)
    add(tmp, .$counts <- gtext(""),expand=TRUE,font.attr=c(family="monospace"))

   .$alpha = gedit("1",width=3,coerce.with=as.numeric,handler=.$compute)
   .$priorprob = gedit("",width=5,handler=.$compute)
   .$model = gedit("",width=15,handler=.$compute)
   .$bf = glabel("")
   .$possible = glabel("")
   .$postprob = glabel("")
   .$testAll = gcheckbox("Tout tester",checked=FALSE,handler=.$onTestAll)
   .$bestOne = glabel("")

    add(group,tmp <- gframe("Validation de modèle"))
    testGroup = glayout(container=tmp)
    testGroup[2,2,anchor=c(-1,0)] = glabel("Param. a priori")
    testGroup[2,3] = .$alpha
    testGroup[3,2,anchor=c(-1,0)] = glabel("Modèles possibles")
    testGroup[3,3] = .$possible
    testGroup[4,2,anchor=c(-1,0)] = glabel("Modèle cible")
    testGroup[4,3] = .$model
    testGroup[5,2,anchor=c(-1,0)] = glabel("Pr(M) a priori")
    testGroup[5,3] =.$priorprob
    testGroup[6,3] =.$testAll
    testGroup[7,2] = glabel("Meilleur modèle")
    testGroup[7,3] =.$bestOne
    testGroup[8,2] = glabel("Facteur de Bayes")
    testGroup[8,3] =.$bf
    testGroup[9,2] = glabel("Pr(M|D)")
    testGroup[9,3] =.$postprob
    testGroup[10,1] = ""
    visible(testGroup)=TRUE

    add(group, tmp <- gframe("Estimations a posteriori"),expand=TRUE)
    add(tmp, .$estimNb <- gnotebook(), expand=TRUE)
    add(.$estimNb, tmp <- ggroup(),label="Modèle",expand=TRUE)
    add(tmp,.$postestim <- gtext("",font.attr=c(family="monospace")),expand=TRUE)
    add(.$estimNb, tmp <- ggroup(),label="Moyennes",expand=TRUE)
    add(tmp,.$avestim <- gtext("",font.attr=c(family="monospace")),expand=TRUE)
    svalue(.$estimNb,index=TRUE) = 1

    buttonGroup=ggroup(container=group)
    addSpring(buttonGroup)
    gbutton("  Calculer  ",container=buttonGroup, handler=.$compute)
  
    # A associer en dernier pour ne pas déclencher pendant la construction
    addHandlerChanged(.$estimNb,.$updatePlot)
    addHandlerChanged(.$testAll,.$compute)

  },
  compute = function(.,h,...) {

    # Vérification des paramètres
    if(is.na(svalue(.$alpha))) {
      gmessage("Spécifiez le paramètre pour la loi Dirichlet symétrique a priori.")
      return()
    }
    a = svalue(.$alpha)
    
    # Vérification des données
   .$getData()
    if(is.null(.$n)) {
      gmessage("Indiquez des valeurs observées.")
      return()
    }
    
    if(any(.$n<0)) {
      gmessage("Erreur : toutes les lignes n'ont pas le même nombre de valeurs.")
      return()
    }

    I = nrow(.$n)
    if(I<2) {
      gmessage("Spécifiez au moins deux lignes de comptages.")
      return()
    }

    # Vérification du modèle
    if(!nchar(svalue(.$model))) {
      svalue(.$model) = paste(1:I,collapse=" ")
      # Return or the analysis is performed twice!
      return()
    }
    
    m = .$getModel()
    if( (length(m) != I) && !svalue(.$testAll)) {
      gmessage("Nombre de groupes incorrect dans la définition du modèle.")
      return()
    }
    
    # Nombre de modèles possibles
    if(I==2) models = cbind(c(1,1),c(1,2))
    else     models = setparts(I)
    nmodels = ncol(models)
    svalue(.$possible) = paste(nmodels)
    
    # Vérification de la proba a priori du modèle
    priorprob = eval(parse(text=svalue(.$priorprob)))
    if(is.null(priorprob)) {
      svalue(.$priorprob) = paste("1/",nmodels,sep="")
      # Return or the analysis is performed twice!
      return()
    }

   .ws$setStatus("Analyse en cours...")
    svalue(.$bestOne) = ""

    # Reference models (null and saturated)
   .$p0 = .$testModel(rep("1",I),a)$estimates
   .$ps = .$testModel(1:I,a)$estimates

    # Bayesian test
    if(svalue(.$testAll)) {
    
      # All possible models
     .$avestimates = matrix(0,nrow(.$n),ncol(.$n))
      group.names = paste(1:I)
      sum.bf = 0
      best.bf = -1
      best.model = ""
      
      for(i in 1:nmodels) {
        svalue(.$possible) = paste(i,"/",nmodels)
        Sys.sleep(.001)
        test = .$testModel(models[,i],a)
        sum.bf = sum.bf + test$bf
       .$avestimates = .$avestimates + test$bf*test$estimates
        if(test$bf> best.bf) {
          best.bf = test$bf
          model.name = tapply(group.names,models[,i],paste,collapse=",")
          model.name = paste(sapply(model.name,function(x) paste("(",x,")",sep="")),collapse=",")
          best.model = model.name
          best.model.index = i
         .$p1 = test$estimates
        }
      }
     .$avestimates = .$avestimates / sum.bf
      
      # Best model
      svalue(.$bf) = round(best.bf,4)
      svalue(.$bestOne) = best.model
      post.prob = priorprob * best.bf/(priorprob * best.bf + 1 - priorprob)
      svalue(.$postprob) = round(post.prob,4)
     .$groups = models[,best.model.index]
      
      # Estimates
      svalue(.$postestim) = ""
      insert(.$postestim,capture.output(round(.$p1,3)),font.attr=c(family="mono"))
      svalue(.$avestim) = ""
      insert(.$avestim,capture.output(round(.$avestimates,3)),font.attr=c(family="mono"))

    }
    else {

      # Convert model symbols in integers if letters were supplied
      if(all(m %in% letters)) m = match(tolower(m),letters)
    
      test = .$testModel(m,a)
      post.prob = priorprob * test$bf/(priorprob * test$bf + 1 - priorprob)
     .$groups = as.numeric(factor(m))
      
      svalue(.$bf) = paste(round(test$bf,4))
      svalue(.$postprob) = paste(round(post.prob,4))

      # A posteriori model averaged estimations
     .$p1 = test$estimates
     .$avestimates = post.prob*.$p1 + (1-post.prob)*.$p0
      svalue(.$postestim) = ""
      insert(.$postestim,capture.output(round(.$p1,3)))
      svalue(.$avestim) = ""
      insert(.$avestim,capture.output(round(.$avestimates,3)))
    }
    
   .$updatePlot(h,...)
   .ws$setStatus("Prêt.")
   
  },
  
  updatePlot = function(.,h,...) {
  
    # Graphique de base : modèle homogène
    C = ncol(.$n)
    responses = paste(1:C)
    matplot(t(.$p0),type="l",col="grey",lwd=2,xlab="Réponses",ylab="Probabilités estimées",ylim=c(0,1),main="",xaxt="n")
    axis(1,1:C,responses)

    # Estimation modèle
    estimType = if(is.null(h$pageno)) svalue(.$estimNb) else h$pageno
    if(estimType == 1) {
      matlines(t(.$p1),col="red",lwd=2,lty=.$groups)
      legend("topleft",lty=1,lwd=2,col=c("red","gray"),legend=c(ifelse(svalue(.$testAll),"Meilleur","Cible"),"Homogène"),inset=.01)
    }
    else {
      matlines(t(.$avestimates),lwd=2,col="blue",lty=.$groups)
      legend("topleft",lty=1,lwd=2,col=c("blue","gray"),legend=c("Moyenne","Homogène"),inset=.01)
    }
    
    # Saturé
    matpoints(t(.$ps),type="p",pch=21,bg="white",col=1,cex=1.5)
    matpoints(t(.$ps),col=1,cex=.5)
  },
  
  getData = function(.,h,...) {
  
    data = svalue(.$counts)
    for(ch in c(",",";","\t")) data = gsub(ch," ",data)
    
    # Strip double spaces
    while(length(grep("  ",data))) data = gsub("  "," ",data)
    
    # Read lines
    rows = unlist(strsplit(data,split="\n"))
    rows = rows[rows != ""]
    
    if(!length(rows)) return(-1)
    
    rows = sapply(rows,strsplit,split=" ")
    l = sapply(rows,length)

    if(var(l)) return(-1)

   .$n = do.call("rbind",lapply(rows,as.numeric))
    row.names(.$n) = paste("l",1:nrow(.$n),sep="")
    
  },
  getModel = function(.) {
  
    m = unlist(strsplit(svalue(.$model)," "))
    m[m != ""]
  },

  testModel = function(.,mod,a) {
  
    stopifnot(is.matrix(n))
    
    C  = ncol(n)
    Kj = colSums(n)
    n2 = rowsum(n,mod)
    A  = matrix(a,nrow(n),ncol(n))
    A2 = matrix(a,nrow(n2),ncol(n2))
    
    lBeta = function(z)     rowSums(lgamma(z)) - lgamma(rowSums(z))
    lPsi  = function(nn,aa) lBeta(nn+aa) - lBeta(aa)
    
    lnum = sum(lPsi(n2,A2))
    lden = lPsi(matrix(Kj,ncol=C),matrix(a,1,C))

    bf = exp(lnum - lden)
    estimates = n2[mod,] + A
    estimates = estimates/rowSums(estimates)
    row.names(estimates) = paste(1:nrow(estimates)," ")

    list(estimates=estimates,bf=bf)
  },

  onTestAll = function(.,h,...) {
  
    if(svalue(.$testAll)) enabled(.$model) = FALSE
    else                  enabled(.$model) = TRUE    
  },

  #---------------------------------------------------------------------------------------
  #  SLOT                   INITIAL VALUE                                    CONTENT
  #---------------------------------------------------------------------------------------
  alpha        =  NULL,                #
  counts       =  NULL,                #
  n            =  NULL,                #
  priorprob    =  NULL,                #
  model        =  NULL,                #
  m            =  NULL,                #
  possible     =  NULL,                #
  testAll      =  NULL,                #
  bestOne      =  NULL,                #
  bf           =  NULL,                #
  postprob     =  NULL,                #
  estimNb      =  NULL,                #
  postestim    =  NULL,                #
  avestim      =  NULL,                #
  p0           =  NULL,                #
  p1           =  NULL,                #
  ps           =  NULL,                #
  avestimates  =  NULL                 #
)


