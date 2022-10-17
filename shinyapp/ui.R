mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

ui <- 
  
  # NAVBARPAGE ####
  navbarPage(
    id="app_navbar",
    windowTitle = "Cartofriches",
    collapsible = TRUE,
    responsive = TRUE,
    title = tagList(
      
      # WAITER ----
      # Utilisation de waiter pour l'écran d'attente
      # nécessaire pour activer waiter. waiter_hide() utilisé dans server.R
      use_waiter(), 
      waiterPreloader(html = tagList(
        includeHTML(path = "www/spinner/spinner.html")
      ), color = transparent(1)),
      
      # SHINY DISCONNECT ----
      disconnectMessage(
        text = "Votre session a pris fin",
        refresh = "Cliquez ici pour vous reconnecter",
        background = "#EF7757",
        colour = "#FFFFFF",
        refreshColour = "#337AB7",
        overlayColour = "#EF7757",
        overlayOpacity = 1,
        width = "full",
        top = "center",
        size = 24,
        css = "padding: 15px !important; box-shadow: none !important;"
      ),
      # actionButton('disconnect', 'Disconnect the app'),
      
      # LOGO, .... ----
      get_logo(appli = "Cartofriches"),
      get_css(),
      tags$head(
        # tags$title("Rapport de mobilité du Cerema"),
        get_favicon(),
        get_matomo() # // Matomo
      )
    ),
    position="static-top",
    
    # HOME ####
    tabPanel(icon("home", lib = "glyphicon"),
             # icon = icon("home", lib = "glyphicon"),
             
             # DETECTION MOBILE ----
             mobileDetect('isMobile'),
             
              
             # BANDEAU DU HAUT 1 ----
             uiOutput("ui_bandeau"),
             
             # PAVES ----
             uiOutput("ui_pave"),
             
             # BOUTONS ----
             fluidRow(
               column(10,
                      offset=1,
                      fluidRow(
                        get_bandeau2(titre = "Valoriser vos friches", 
                                     sous_titre = "et rejoignez la communauté")
                 ),
                 tags$br(),
                 tags$br(),
                 
                fluidRow(
                  column(4, div(get_bouton("button1", "Publier une friche", url = 'https://artificialisation.biodiversitetousvivants.fr/cartofriches/contribuer'), style="text-align:center;margin-top:5px;")),
                  column(4, div(get_bouton("button2", "Voir les contributeurs", url = "https://artificialisation.biodiversitetousvivants.fr/cartofriches/observatoires-locaux"), style="text-align:center;margin-top:5px;")),
                  column(4, div(get_bouton("button3", "Découvrir le Cerema", url = "https://www.cerema.fr/fr"), style="text-align:center;margin-top:5px;"))
                 )
             ), 
             style="background-color:#EF7757;
                    padding-top:70px;
                    padding-bottom:70px;"),
  ),
  
  # ONGLET RECHERCHE UNE FRICHE ----
  tabPanel(
    "Rechercher une friche",
    icon = icon("search"),
    fluidRow(
      column(4,
             div(
             tabsetPanel(id="tabs",
                        type = "tabs",
                         tabPanel(tagList(icon("map-marker", lib="glyphicon"),"Adresse"), 
                                  div( 
                                    div(
                                      div(
                                        modui_ban("search2", width = "100%")
                                      ),
                                      tags$hr(style="border-top:1px solid #ffae99;"),
                                      selectInput("slc_secteurs", 
                                                  label = NULL, 
                                                  choices = Secteurs, 
                                                  selected = NULL, 
                                                  width = "80%", 
                                                  selectize = FALSE)),
                                    class = "search"),                                  
                                  ),
                         tabPanel(tagList(icon("binoculars"), "Observatoires"), 
                                  div( 
                                    selectInput("slc_observatoires",
                                                NULL,
                                                choices = Observatoires, 
                                                width = "80%",
                                                selectize = FALSE), class = "search")                                  
                                  )
             )),
             
             fluidRow(
               column(12,
                      # LEGENDE ----
                      div(uiOutput("ui_legende"),
                          uiOutput("ui_filtres"),
                          uiOutput("ui_txt_zoom"),
                          class="information"),
                      
                      )),
             
             
             
             class="carte"),
      
      # Carte ----
      column(8, leafletOutput("mymap"))
    )
  ),

  # ONGLET PUBLIER UNE FRICHE ----
  tabPanel(
    "Publier une friche",
    icon = icon("plus-circle"),
    tags$br(),
    div(
      div(tags$span("Publier une friche sur Démarches simplifiées")),
      div(tags$a(tags$img(src="logos/demarches-simplifiees.png"), 
           href="https://www.demarches-simplifiees.fr/commencer/cartofriches-declarer-un-site",
           target="_blank")),
      style="
          color:#144391;
          margin-top: 5px;
          padding-top: 10px;
          text-align: center;
          padding-bottom: 10px;
          border-radius: 5px;
          background-color: whitesmoke;
          font-size: 1.2em;
          width:400px;
          margin:auto;
            ")

  ),
  
  # ONGLET PUBLIER UNE FRICHE ----
  tabPanel(
    "Données",
    icon = icon("database"),
    tags$br(),
    div(
      div(tags$span("Télécharger les données sur data.gouv.fr")),
      div(tags$a(tags$img(src="logo-datagouv.png", width = "200px", style="margin-top:10px;"), 
                 href="https://www.data.gouv.fr/fr/datasets/sites-references-dans-cartofriches/",
                 target="_blank")),
      style="
          color:#144391;
          margin-top: 5px;
          padding-top: 10px;
          text-align: center;
          padding-bottom: 10px;
          border-radius: 5px;
          background-color: whitesmoke;
          font-size: 1.2em;
          width:400px;
          margin:auto;
            ")
    
  ),
    
    navbarMenu("À propos",
        
    # A PROPOS / Cartofriches ----       
    tabPanel("Cartofriches",
             fluidRow(column(8, offset=2, uiOutput("ui_apropos_cartofriches")))),
    
    # Un clic amène vers la page observatoires du portail artificialisation.biodiversitetousvivants
    tabPanel(tags$a(href = "https://artificialisation.biodiversitetousvivants.fr/cartofriches/observatoires-locaux",
    "Observatoires locaux", 
    target="_blank")),
    
    # Un clic amène vers la page données du portail artificialisation.biodiversitetousvivants
    tabPanel(tags$a(href = "https://artificialisation.biodiversitetousvivants.fr/cartofriches/donnees-utilisees",
    "Données utilisées",
    target = "_blank")),
    
    tabPanel("Mentions légales",
             fluidRow(column(8, offset=2, includeMarkdown("www/textes/mentions_legales.md"))))
    ),
    
    
    # FOOTER ---------------------------
    tags$hr(),
    footer = get_ui_footer()
  )