#-----------------------------------------------------------------------------------------
#
#               Yvonnick Noel, U. of Brittany, Rennes, France, 2007-2011
#                        Statistical workshops for teaching
#
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#                           Construction of main GUI
#-----------------------------------------------------------------------------------------

.ws = proto(

   create = function(.) {
   
     # Interface
    .$window = gwindow(.$translate("AtelieR: Statistical workshops in R"),visible=FALSE)
    .$bigGroup = ggroup(cont = window)
    .$statusBar = gstatusbar(.$translate("Ready."),cont=.$window)
     add(.$bigGroup,.$nb <- gnotebook(tab.pos=2,closebuttons=TRUE),expand=TRUE)
     add(.$bigGroup,ggraphics())

     # File menu
     aClose     = gaction(label=.$translate("Quit"),icon="quit",handler=function(h,...) dispose(.$window))
     aNormal    = gaction(label=.$translate("Construction of the gaussian distribution"),handler=.ws1$create)
     aScale     = gaction(label=.$translate("Change of origin and scale"),               handler=.ws2$create)
     aMean      = gaction(label=.$translate("Distribution of a sample mean"),            handler=.ws3$create)
     aVar       = gaction(label=.$translate("Distribution of a sample variance"),        handler=.ws5$create)
     aCalc      = gaction(label=.$translate("Probability calculator"),                   handler=.ws4$create)
     aProp      = gaction(label=.$translate("Bayesian inference on a proportion"),       handler=.ws6$create)
     aKprop     = gaction(label=.$translate("Bayesian inference on several proportions"),handler=.ws9$create)
     aTable     = gaction(label=.$translate("Bayesian inference on a contingency table"),handler=.ws10$create)
     aBayesvar  = gaction(label=.$translate("Bayesian inference on a variance"),         handler=.ws11$create)
     aBayesmean = gaction(label=.$translate("Bayesian inference on a mean"),             handler=.ws8$create)
     aKmeans    = gaction(label=.$translate("Bayesian inference on several means"),      handler=.ws13$create)
     
     tmp = list(Session = list(Quit=aClose),
                Modules = list(Understand = list(normal=aNormal,scale=aScale,mean=aMean,var=aVar),
                               Compute    = list(calc=aCalc,prop=aProp,kprop=aKprop,tab=aTable,bvar=aBayesvar,bmean=aBayesmean,km=aKmeans)))
     names(tmp$Session) = .$translate(names(tmp$Session))
     names(tmp$Modules) = .$translate(names(tmp$Modules))
     names(tmp) = .$translate(names(tmp))
    .$menu = gmenu(tmp,cont=.$window)
   
   },
   
   show = function(.) {
     svalue(.$nb) = 1
     visible(.$window) = TRUE
   },
   
   setStatus = function(.,text) {
     svalue(.$statusBar)
     svalue(.$statusBar) = text
   },

   ### Gettext utility for translating messages
   translate = function(.,...) {
     gettext(..., domain="R-AtelieR")
   },

  #---------------------------------------------------------------------------------------
  #  SLOT       INITIAL VALUE                CONTENT
  #---------------------------------------------------------------------------------------
  window      = NULL,                   # Main window
  bigGroup    = NULL,                   # Main group
  nb          = NULL,                   # Main notebook
  menu        = NULL,                   # Menu
  statusBar   = NULL                    # Status bar
 )

AtelieR = function() {

  .ws$create()
  .ws$show()

}
