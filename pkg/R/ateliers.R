#-----------------------------------------------------------------------------------------
#
#                     Yvonnick Noël, U. de Rennes 2, 2007-2011
#                  Ateliers pédagogiques de simulation statistique
#
#-----------------------------------------------------------------------------------------

library(proto)
options(guiToolkit="RGtk2")
library(gWidgets)

#-----------------------------------------------------------------------------------------
#                           Construction de l'interface générale
#-----------------------------------------------------------------------------------------

.ws = proto(

   create = function(.) {
   
     # Interface
    .$window = gwindow("AtelieR : Ateliers de statistiques sous R",visible=FALSE)
    .$bigGroup = ggroup(cont = window)
    .$statusBar = gstatusbar("Prêt.",cont=.$window)
     add(.$bigGroup,.$nb <- gnotebook(tab.pos=2,closebuttons=TRUE),expand=TRUE)
     add(.$bigGroup,ggraphics())

     # Menus
     tmp = list()

     # Menu fichier
     tmp$Session$Quitter$handler = function(h,...) dispose(window)
     tmp$Session$Quitter$icon = "quit"

     # Menu ateliers
     tmp$Ateliers$Comprendre$"Construction de la loi normale"$handler              = .ws1$create
     tmp$Ateliers$Comprendre$"Changement d\'origine et d\'échelle"$handler         = .ws2$create
     tmp$Ateliers$Comprendre$"Distribution d'une moyenne d\'échantillon"$handler   = .ws3$create
     tmp$Ateliers$Calculer$"Calculateur de probabilités"$handler                   = .ws4$create
     tmp$Ateliers$Comprendre$"Distribution d\'une variance d\'échantillon"$handler = .ws5$create
     tmp$Ateliers$Calculer$"Inférence bayésienne sur une proportion"$handler       = .ws6$create
     tmp$Ateliers$Calculer$"Inférence bayésienne sur plusieurs proportions"$handler = .ws9$create
     tmp$Ateliers$Calculer$"Inférence bayésienne sur table de contingence"$handler = .ws10$create
     tmp$Ateliers$Calculer$"Inférence bayésienne sur une moyenne (a priori non informative)"$handler = .ws7$create
     tmp$Ateliers$Calculer$"Inférence bayésienne sur une moyenne (a priori informative)"$handler = .ws8$create
    .$menu = gmenu(tmp,cont=window)

   },
   
   show = function(.) {
     svalue(.$nb) = 1
     visible(.$window) = TRUE
   },
   
   setStatus = function(.,text) {
     svalue(.$statusBar)
     svalue(.$statusBar) = text
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
