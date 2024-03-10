transform_utf8 <- function(v) {
  v <- gsub("Ã©", "é", v)
  v <- gsub("Ã¨", "è", v)
  v <- gsub("Ã´", "ô", v)
  v <- gsub("Ã¢", "â", v)
  v <- gsub("ÃŽ", "Î", v)
  v <- gsub("Ã\\s", "à", v)
  v
}

export_data <- function(only_checked = TRUE, 
                        deps = NA, 
                        outputDir = outputDir) {
  
  # Création du dossier
  dir.create(outputDir, recursive = T)
  
  # Filtre par département
  deps <- NA
  
  if(!is.na(deps)) {
    w <- which(substr(f.xy$idcom, 1, 2) %in% deps)
    friches_points <- f.xy %>% slice(w)  
  } else {
    friches_points <- f.xy
  }
  
  # Filtre qualifiées
  if(ONLY_CHECKED) {
    friches_points <- friches_points %>% filter(checked)
    table(substr(friches_points$idcom, 1, 2))
  }
  
  # OFF
  # Traitement des champs de type Liste
  # w <- which(sapply(names(friches_points), function(x) class(friches_points[[x]])) == "list") # donne zones_implantation2
  # friches_points$zones_implantation2 <- sapply(friches_points$zones_implantation2, function(x) paste(unlist(x), collapse=", "))
  # f.tup$zones_implantation2 <- sapply(f.tup$zones_implantation2, function(x) paste(unlist(x), collapse=", "))
  # /OFF
  
  # Surfaces TUP
  sites_numeros <- friches_points %>% pull(site_numero)
  friches_surfaces <- f.tup %>% filter(site_numero %in% sites_numeros)
  
  # Contrôle
  message("Nombre de points égal à nombre de surfaces ? ",nrow(friches_points) == nrow(friches_surfaces))
  
  # Export
  message("Export des friches points...")
  st_write(friches_surfaces, file.path(outputDir, "friches-surfaces.gpkg"), delete_layer = TRUE, delete_dsn = TRUE)
  message("Export des friches surfaces")
  st_write(friches_points, file.path(outputDir, "friches-points.gpkg"), delete_layer = TRUE, delete_dsn = TRUE)
  
  # Copie de la documentation
  message("Copie de la documentation...")
  file.copy("../cartofriches.wiki/Dictionnaire-de-variables.md", outputDir, recursive = TRUE)
  
  return(list(friches_points, friches_surfaces))
}


find_colonnes_with_http <- function(f) {
  colonnes <- c()
  for(i in 1:length(names(f))) {
    colonne <- names(f)[i]
    v <- f[[colonne]]
    if(any(grepl("http", v))) colonnes <- c(colonnes, names(f)[i])
  }
  colonnes
}

get_comptage_na <- function(f) {
  
  mycolonnes <- names(f)
  out <- vector(mode="list")
  for (colonne in mycolonnes) {
    v <- f[[colonne]]
    df <- table(is.na(v)) %>% as.matrix %>% t %>%  data.frame
    if(!'TRUE.' %in% names(df)) df[['TRUE.']] <- 0
    if(!'FALSE.' %in% names(df)) df[['FALSE.']] <- 0
    df$colonne <- colonne
    df <- df %>% select(colonne, 1, 2)
    out[[colonne]] <- df
  }
  res <- do.call(rbind, out) %>% arrange(colonne)
  
  res
  
}

get_stats <- function(f.pols, f.xy){
  
  # f.pols <- get_stats(regs2, f.xy)
  # f.pols %>% head(1)
  
  get_n <- function(targets, sources) {
    i <- targets %>% st_intersects(sources)
    n <- sapply(i, function(x) length(x))
    return(n)
  }
  
  f.pols$n_friches_avec_projet <- f.pols %>% 
    get_n(f.xy %>% filter(site_statut == "friche avec projet"))
  
  f.pols$n_friches_sans_projet <- f.pols %>% 
    get_n(f.xy %>% filter(site_statut == "friche sans projet"))
  
  f.pols$n_friches_potentielles <- f.pols %>% 
    get_n(f.xy %>% filter(site_statut == "friche potentielle"))
  
  f.pols$n_friches_reconverties <- f.pols %>% 
    get_n(f.xy %>% filter(site_statut == "friche reconvertie"))
  
  return(f.pols)
}

process_milieu_implantation <- function(x, sep=",") {
  x %>% as.character %>% {ifelse((is.na(.) | . == ""), "", strsplit(., sep))} %>% lapply(tolower)
}