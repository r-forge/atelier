#-----------------------------------------------------------------------------------------
#                  Inférence bayésienne sur plusieurs moyennes
#-----------------------------------------------------------------------------------------
library(partitions)

.ws13 = proto(

  create = function(.,h,...) {

    # Don't create if no main interface exists
    if(inherits(try(is.environment(.ws)),"try-error")) return()
    
    # Don't create if already opened
    if("Inférence bayésienne\nsur plusieurs moyennes" %in% names(.ws$nb)) return()
    
    add(.ws$nb,group <- ggroup(horizontal=FALSE),label="Inférence bayésienne\nsur plusieurs moyennes")

   .$means = gedit("10  4 29 24 11",width=25,handler=.$updatePlot)
   .$sds   = gedit("5.126960 3.162278 6.164414 6.369571 6.718843",width=25,handler=.$updatePlot)
   .$Ntot  = gedit("8 8 8 8 8",width=25,handler=.$updatePlot)

    add(group,tmp <- gframe("Données observées"))
    dataGroup = glayout(container=tmp)
    dataGroup[2,2,anchor=c(-1,0)] = glabel("Moyennes")
    dataGroup[2,3] = .$means
    dataGroup[3,2,anchor=c(-1,0)] = glabel("Ecarts-type")
    dataGroup[3,3] = .$sds
    dataGroup[4,2,anchor=c(-1,0)] = glabel("Effectifs")
    dataGroup[4,3] = .$Ntot
    dataGroup[5,1]= ""
    visible(dataGroup) = TRUE

   .$priorprob = gedit("",width=5,handler=.$updatePlot)
   .$model = gedit("",width=15,handler=.$updatePlot)
   .$bf = glabel("")
   .$possible = glabel("")
   .$postprob = glabel("")
   .$testAll = gcheckbox("Tout tester",checked=FALSE,handler=.$onTestAll)
   .$bestOne = glabel("")
   .$doBMA = gcheckbox("Moyenner",handler=.$updatePlot)

    add(group,tmp <- gframe("Test d\'hypothèse"))
    testGroup = glayout(container=tmp)
    testGroup[2,2,anchor=c(-1,0)] = glabel("Modèles possibles")
    testGroup[2,3] = .$possible
    testGroup[3,2,anchor=c(-1,0)] = glabel("Modèle cible")
    testGroup[3,3] = .$model
    testGroup[4,2,anchor=c(-1,0)] = glabel("Pr(M) a priori")
    testGroup[4,3] =.$priorprob
    testGroup[5,2] =.$testAll
    testGroup[5,3] =.$doBMA
    testGroup[6,2] = glabel("Meilleur modèle")
    testGroup[6,3] =.$bestOne
    testGroup[7,2] = glabel("BIC")
    testGroup[7,3] =.$bf
    testGroup[8,2] = glabel("Pr(M|D)")
    testGroup[8,3] =.$postprob
    testGroup[9,1] = ""
    visible(testGroup)=TRUE

    add(group, tmp <- gframe("Estimations a posteriori des modèles"),expand=TRUE)
    add(tmp,.$postestim <- gtable(cbind(Groupe=rep("",10),Moyennes=rep("",10),Inf95=rep("",10),Sup95=rep("",10))),expand=TRUE)

    buttonGroup = ggroup(container=group)
    addSpring(buttonGroup)
    gbutton("  Calculer  ",container=buttonGroup, handler=.$updatePlot)
  
  },
  onTestAll = function(.,h,...) {
  
    if(svalue(.$testAll)) { enabled(.$model) = FALSE ; enabled(.$priorprob) = FALSE }
    else                  { enabled(.$model) = TRUE  ; enabled(.$priorprob) = TRUE ; enabled(.$doBMA) = FALSE }
    svalue(.$model) = ""
    
  },
  updatePlot = function(.,h,...) {

    if(svalue(.$doBMA) && !svalue(.$testAll)) {
      svalue(.$testAll) = TRUE
      # Return or the analysis is performed twice!
      return()
    }
    
    # Vérification des données
    if(any(is.na(c(svalue(.$means),svalue(.$sds))))) {
      gmessage("Indiquez des valeurs observées.")
      return()
    }
    
    mn = unlist(strsplit(svalue(.$means)," "))
    mn = as.numeric(mn[mn != ""])
    sd = unlist(strsplit(svalue(.$sds)," "))
    sd = as.numeric(sd[sd != ""])
    N = unlist(strsplit(svalue(.$Ntot)," "))
    N = as.numeric(N[N != ""])

    # Check data input
    if( (length(mn)<2) || (length(sd)<2) || (length(N)<2) || (var(c(length(mn),length(sd),length(N)))!=0) ) {
      gmessage("Spécifiez au moins deux groupes.\nVérifiez qu'il y a autant de valeurs dans les 3 champs")
      return()
    }
    
    # Number of groups
    K = length(mn)

    # Default model is the saturated one
    if(!nchar(svalue(.$model))) {
      svalue(.$model) = paste(1:length(mn),collapse=" ")
      # Return or the analysis is performed twice!
      return()
    }

    # Check model definition
    m = .$getModel()
    if(length(m) != K) {
      gmessage("Nombre de groupes incorrect dans la définition du modèle.")
      return()
    }
    
   .ws$setStatus("Analyse en cours...")
    svalue(.$bestOne) = ""

    # Generate all possible model definitions
    if(K==2) models = cbind(c(1,1),c(1,2))
    else     models = setparts(K)
    nmodels = ncol(models)
    svalue(.$possible) = paste(nmodels)

    # Get model prior prob
    priorprob = eval(parse(text=svalue(.$priorprob)))
    if(is.null(priorprob)) {
      svalue(.$priorprob) = paste("1/",nmodels,sep="")
      # Return or the analysis is performed twice!
      return()
    }

    m0   = .$testModel(mn,sd,N,rep(1,K))
    msat = .$testModel(mn,sd,N,1:K)

    if(svalue(.$testAll)) {
    
      # Compute all possible models
      results = matrix(0,0,3*K)
      modProbs = vector()
      for(i in 1:ncol(models)) {
        test = .$testModel(mn,sd,N,models[,i])
        results = rbind(results,c(test$means,test$IC.inf,test$IC.sup))
        modProbs = c(modProbs,test$prob)
        rownames(results)[i] = test$name
      }
      colnames(results) = c(paste("m",1:K,sep=""),paste("Inf",1:K,sep=""),paste("Sup",1:K,sep=""))

      # Compute approximate Bayes factors
      bfs = modProbs/m0$prob
      
      # Compute model posterior probs
      modProbs = modProbs/sum(modProbs)
      
      # Bayesian Model Averaging
      if(svalue(.$doBMA)) {
      
        # A posteriori model averaged estimations (Neath & Cavanaugh, 2006)
        post.estim = colSums(results*modProbs)
        target = list(name="Averaged",means=post.estim[1:K],IC.inf=post.estim[(K+1):(2*K)],IC.sup=post.estim[(2*K+1):(3*K)])
        svalue(.$bestOne) = "(Moyenné)"
        svalue(.$bf) = ""
        svalue(.$postprob) = ""
      }
      
      # Best model selection
      else {

        best.one = which.max(modProbs)
        target = .$testModel(mn,sd,N,models[,best.one])
        svalue(.$bestOne) = target$name
        svalue(.$bf) = round(target$bic,4)
        svalue(.$postprob) = round(modProbs[best.one],4)
      }

     .$postestim[,] = matrix("",10,4)
     .$postestim[1:K,1] = paste(1:K)
     .$postestim[1:K,2] = round(target$means,4)
     .$postestim[1:K,3] = round(target$IC.inf,4)
     .$postestim[1:K,4] = round(target$IC.sup,4)
    }
    
    # Theoretical model
    else {
    
      target = .$testModel(mn,sd,N,m)

      # Compute posterior prob
      target.bf = target$prob/m0$prob
      post.prob = priorprob * target.bf/(priorprob * target.bf + 1 - priorprob)
      
      svalue(.$bf) = paste(round(target$bic,4))
      svalue(.$postprob) = paste(round(post.prob,4))

      # A posteriori model averaged estimations (Stein, 1955)
      post.estim = post.prob*target$means + (1-post.prob)*m0$means

     .$postestim[,] = matrix("",10,4)
     .$postestim[1:K,1] = paste(1:K)
     .$postestim[1:K,2] = round(target$means,4)
     .$postestim[1:K,3] = round(target$IC.inf,4)
     .$postestim[1:K,4] = round(target$IC.sup,4)
    }
    
    # Plot
    ymin = min(msat$means) - 3*msat$error/sqrt(min(N))
    ymax = max(msat$means) + 3*msat$error/sqrt(min(N))
    plot(1:K,msat$means,xlab="Groupes",ylab="Moyennes estimées",main="",type="n",xaxt="n",ylim=c(ymin,ymax))
    axis(1,1:K,paste(1:K))
    abline(h=m0$means[1],col="lightgrey",lty=2,lwd=2)
    
    points(1:K,msat$means,cex=1.5)
    for(k in 1:K) lines(rbind(c(k,target$IC.inf[k]),c(k,target$IC.sup[k])),col="red")
    points(1:K,target$means,pch=19,col="red")
    legend("topleft",pch=c(1,19),pt.cex=1.2,col=c("black","red"),legend=c("Données","Modèle"),inset=.01)
    
   .ws$setStatus("Prêt.")
  },
  getModel = function(.) {
  
    m = unlist(strsplit(svalue(.$model)," "))
    m[m != ""]
  },
  testModel = function(.,m0,s0,N0,mod,conf=.95) {
  
    # Dimension
    N = sum(N0)
    K = length(unique(mod))

    # Sums
    sumx = N0*m0
    sumx2 = (N0-1)*s0**2
    
    # Constrained means
    Nt  = tapply(N0,mod,sum)
    mt  = tapply(sumx,mod,sum)/Nt

    # new sum of squares wrt the new means mt
    sumx2 = sumx2 + N0*(m0-mt[mod])**2
    s2t = sum(sumx2)/N
    corr.s2t = (N/(N-K)) * s2t
    
    # Credibility interval
    IC.inf = mt + qt((1-conf)/2,N-K)*sqrt(corr.s2t/Nt)
    IC.sup = mt + qt((1+conf)/2,N-K)*sqrt(corr.s2t/Nt)
    
    # BIC
    t = length(mt) + 1
    bic = N + N*log(2*pi*s2t) + t*log(N)
    prob = exp(-.5 * bic)
    
    # Model name
    group.names=paste(1:length(m0))
    model.name = tapply(group.names,mod,paste,collapse=",")
    model.name = paste(sapply(model.name,function(x) paste("(",x,")",sep="")),collapse=",")
    
    list(model=mod,name=model.name,means=mt[mod],error=sqrt(corr.s2t),bic=bic,prob=prob,IC.inf=IC.inf[mod],IC.sup=IC.sup[mod])
  },

  #---------------------------------------------------------------------------------------
  #  SLOT                   INITIAL VALUE                                    CONTENT
  #---------------------------------------------------------------------------------------
  means        =  NULL,                #
  sds          =  NULL,                #
  Ntot         =  NULL,                #
  priorprob    =  NULL,                #
  model        =  NULL,                #
  possible     =  NULL,                #
  testAll      =  NULL,                #
  bestOne      =  NULL,                #
  bf           =  NULL,                #
  postprob     =  NULL,                #
  postestim    =  NULL                 #
)


