.onAttach = function(...) {

  options(guiToolkit="RGtk2")
  
  # Create gtkrc file and select Windows theme if necessary
  # (Default Gnome theme is ugly under Win)
  if(.Platform$OS.type == "windows") {
    rc.path = paste(Sys.getenv("R_HOME"),"library/RGtk2/gtk/i386/etc/gtk-2.0",sep="/")
    rc.file = paste(rc.path,"gtkrc",sep="/")
    if(file.exists(rc.path) && !file.exists(rc.file)) writeLines("gtk-theme-name=\"MS-Windows\"",rc.file)
  }

  AtelieR()
}

