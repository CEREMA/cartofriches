load_data <- function() {
  
  ## LECTURE DE LA DATA ############################################################
  
  # > FRICHES ----
  f.xy <<- readRDS("data/friches/f.xy.rds")
  f.tup <<- readRDS("data/friches/f.tup.rds")
  f.xy$layerId      <- paste0("industrielle_xy_", f.xy$site_id)
  f.tup$layerId     <- paste0("industrielle_tup_", f.tup$site_id)
  
  # > STATS ---
  # Affichage des stats régionales
  # sous la forme de cercles 
  # au lancement de l'application
  regs.pts  <<- readRDS("data/stats/regs.pts.rds")
  deps.pts  <<- readRDS("data/stats/deps.pts.rds")
  regs.pts$layerId  <- glue("stat_reg_{regs.pts$code}")
  deps.pts$layerId  <- glue("stat_dep_{deps.pts$code}")
  
  
  # > LISTE ----
  # On construit une liste avec les différents éléments
  Data <<- list(points = f.xy,
               polygons = f.tup,
               stats = list(regs = regs.pts,
                            deps = deps.pts))
  
  # > CONTOURS ----
  deps  <<- readRDS("data/contours/deps.rds")
  regs  <<- readRDS("data/contours/regs.rds")
  comms <<- readRDS("data/contours/comms.rds")
  
  # EMPRISES ####
  emprises <<- readRDS("data/contours/emprises.rds")
}
