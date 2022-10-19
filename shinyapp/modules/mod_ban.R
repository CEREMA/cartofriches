my_modal <- function(labels, session) {
  ns <- session$ns
  
  ui_precision <- NULL
  if(any(grepl("\\*", names(labels)))) {
    ui_precision <- div(glue("* dans un rayon de {round(SEARCH_DISTANCE/1000)} km"),
    style="float: right;
           margin-top: 5px;
           margin-bottom: 10px;
           font-size: 0.8em;")
  }
  
  modalDialog(title = "Sélectionner une adresse", 
              div(
                awesomeRadio(
                  inputId = ns("adresses"),
                  label = NULL, 
                  choices = labels,
                  selected = NULL,
                  status = "primary",
                  width = "100%"
                ),
                ui_precision
                , style="
              padding-left:20px;
              padding-left: 20px;
              padding-top: 20px;
              padding-bottom: 10px;"), 
              footer = actionButton(ns("btn_gototheadresse"), 
                                    tagList(icon("paper-plane"), 
                                            "Aller vers cette adresse"),
                                    class = "goto"), 
              easyClose = TRUE)
}

modui_ban <- function(id, width) {
  ns <- NS(id)
  searchInput(
    inputId = ns("search"),
    label = NULL,
    placeholder = Txt_search,
    btnSearch = icon("search"),
    btnReset = icon("remove"),
    width = width
  )
}

mod_ban <- function(id, session_parent){
  
  moduleServer(id, function(input, output, session, tab = "Rechercher une friche") {
    
    # r_coords ----
    r_coords <- reactive({
      message(">> r_coords")
      
      # Choix d'adresses
      value <- input$adresses
      message('>> Valeur ', value)

      # Si pas encore d'adresse ou pas encore de bouton
      if(is.null(value) & is.null(input$btn_gototheadresse)) {
        return()
      } else {
        # Si le bouton n'a pas encore été activé
        if(input$btn_gototheadresse == 0) {
          return()
        } else if(grepl("ban", value)) {
          # localisation à l'adresse
          type <- "adresse"
          message(">> adresse de type BAN")
          res <- interpret_label(value)
        } else if (grepl("comm", value)) {
          # localisation à l'INSEE
          type <- "commune"
          message(">> adresse de type INSEE")
          res <- interpret_label(value)
        } else if (grepl("refCad", value)) {
          # localisation à la référence cadastrale
          message(">> adresse de type Référence cadastrale")
          type <- "refCad"
          res <- interpret_label(value)
        } else if (grepl("dep|reg", value)) {
          # localisation au département ou à la région
          type <- "depReg"
          message(">> adresse de type DEP ou REG")
          res <- interpret_label(value)
        }
      }
      return(res)
    })
    
    # input$search ----
    observeEvent(input$search, {
      req(input$search)
      
      adresse <- input$search
      message(">> input$search ", adresse)

      # Test Regex
      if(is_insee(adresse)) {
        message(">> Commune")
        res <- get_labels_commune(adresse)
        if(is.null(res)) {
          warning_dialog(texte = glue("Code INSEE {adresse} inconnu"))
          return()
        }
      } else if(is_parcelle(adresse)) {
        message(">> Référence cadastrale")
        res <- get_labels_refCad(adresse)
        if(is.null(res)) {
          warning_dialog(texte = glue("Référence cadastrale {adresse}"))
          return()
        }
      } else {
        res <- geocode(query = adresse)

        if(nrow(res) == 0) {
          warning_dialog(texte = glue("Impossible de trouver {adresse}"))
          return()
        }

        # Libellés des rues et communes
        message(">> Recherche BAN")
        labels <- get_labels_ban(res)

        # Départements ou régions ? (on complète si jamais)
        message(">> Recherche DEP REG de ", adresse)
        res_depreg <- get_labels_depReg(adresse)

        # on ajoute le département ou la région à la liste des candidats
        res        <- c(labels, res_depreg)
        names(res) <- c(names(labels), names(res_depreg))
      }

      # Boîte modale
      showModal(my_modal(res, session))
    })
    
    # input$btn_gototheadresse (va sur l'adresse sélectionnée) ----
    observeEvent(input$btn_gototheadresse, {
      message(">> input$btn_gototheadresse")
      updateNavbarPage(session_parent, "app_navbar", selected = tab)
      removeModal()
      updateSearchInput(session, "search", "NULL", "", placeholder = Txt_search)
    })
    
   return(r_coords) 
  })
}