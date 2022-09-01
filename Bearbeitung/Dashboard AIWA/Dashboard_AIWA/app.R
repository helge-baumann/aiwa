
library(shiny)

source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(
    tags$style(
      "body{
            font-family: Arial;
            background: #f5f5f5;
            color:red;
    height: auto;
    min-width:596px;
    max-width: 596px;
    margin: none;
            }
      p { color:blue;}
            .shiny-input-container {
   color:black;
            }"
    )
  ),



  # Application title

  # Show a plot of the generated distribution
  mainPanel(
    selectInput(
      "auswahl",
      HTML("<div style='color:red;'> Bitte wählen Sie Ihr Thema:</div>"),
      choices = unname(unlist(map(Data, "title"))),
      selected = unname(unlist(map(Data, "title")))[1]
    ),

    #  HTML("<hr style='width:596px; border-top: 1px solid black;'>"),

    uiOutput("secondSelection"),

    HTML("<hr style='width:596px; border-top: 1px solid black;'>"),

    radioButtons("farben", HTML("<div style='color:red;'> Bitte wählen Sie eine Farbskala:</div>"),
      choices = c("lila", "pink", "grau", "tuerkis", "blau"),
      selected = "lila", inline = T
    ),

    HTML("<hr style='width:596px; border-top: 1px solid black;'>"),

    tabsetPanel(
      type = "tabs",
      tabPanel("Karte", leafletOutput("interactiveMap", width = 596, height = 596 * 1.25)),
      tabPanel("Tabelle", dataTableOutput("table", width = "596px"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Daten_auswahl <- reactive({
    #Data[[names(which(map(Data, "title") == input$auswahl))]]
 # })

  output$secondSelection <- renderUI({
    radioButtons("auswahl2",
      HTML("<div style='color:red;'> Welche Kategorie möchten Sie darstellen?:</div>"),
      choices = names(Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts),
      selected = names(Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts)[1],
      inline = T
    )
  })


  shape <- reactive({
    load_shapefile(path = NULL, level = Data[[names(which(map(Data, "title") == input$auswahl))]]$Ebene, leaflet = T)
  })

  geo <- reactive({

    # Farbenpalette: Woran geknüpft?


    geodata <- merge(shape(), Data[[names(which(map(Data, "title") == input$auswahl))]]$dat, by = Data[[names(which(map(Data, "title") == input$auswahl))]]$Key)
    geodata$gruppe <- cut(geodata[[input$auswahl2]],
      breaks = Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]],
      labels =
        c(
          paste0("unter ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][2]),
          paste0(
            "von ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][2],
            " bis ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][3]
          ),
          paste0(
            "von ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][3],
            " bis ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][4]
          ),
          paste0(
            "von ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][4],
            " bis ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][5]
          ),
          paste0("über ", Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts[[input$auswahl2]][5])
        )
    )



    # Korrekturfaktor
    geodata$pc1 <- NULL
    geodata$pc2 <- NULL

    for (i in 1:length(geodata@polygons)) {
      pc <- geodata@polygons[[i]]@Polygons[[1]]@coords
      pc <- centroid(pc)
      geodata$pc1[i] <- 9.5 - pc[1]
      geodata$pc2[i] <- 51.5 - pc[2]
    }

    colfunc <- colorFactor(farben(input$farben), domain = geodata$gruppe, ordered = T)
    geodata$color <- colfunc(geodata$gruppe)


    return(geodata)
  })

  colors <- reactive({
    colfunc2 <- colorFactor(farben(input$farben), domain = geo()$gruppe, ordered = T)
    colfunc2(levels(geo()$gruppe))
  })

  lvl <- reactive({
    levels(geo()$gruppe)
  })


  output$interactiveMap <- renderLeaflet({

    # Ländergrenzen (für später, um sie einzuzeichnen)
    if (Data[[names(which(map(Data, "title") == input$auswahl))]]$Ebene %in% c("KRS", "AMR")) {
      geo_land <- load_shapefile(path = NULL, level = "LAN", leaflet = T)
    }

    # Karte zeichnen-------

    # Karte


    fig <- geo() %>%
      leaflet_basis() %>%
      leaflet_text(Data[[names(which(map(Data, "title") == input$auswahl))]]$title, size = 20) %>%
      leaflet_text(
        lat = 55.25,
        Data[[names(which(map(Data, "title") == input$auswahl))]]$subtitle, size = 15
      ) %>%
      leaflet_legend(colors = colors(), lvl = lvl()) %>%
      leaflet_logo() %>%
      leaflet_quelle(Text = paste0("Quelle: <br> ", Data[[names(which(map(Data, "title") == input$auswahl))]]$quelle)) %>%
      leaflet_anmerkung(Text = "Mehr Informationen erhalten Sie per Mausklick auf die Länder!") %>%
      leaflet_polygons(
        geo = geo(),
        bg = colors(),
        variablen = names(Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts),
        headers = names(Data[[names(which(map(Data, "title") == input$auswahl))]]$cuts),
        k = 0, suffix = "Euro" # Nachkommastellen
      )

    # Ländergrenzen (nur bei Kreisen, Bezirken, Arbeitsmarktregionen sinnvoll)
    if (Data[[names(which(map(Data, "title") == input$auswahl))]]$Ebene %in% c("KRS", "AMR")) {
      fig <- fig %>%
        addPolygons(
          data = geo_land,
          color = "white", weight = 2, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fill = F
        )
    }

    # Leaflet-Stil anwenden und in HTML überführen----
    fig$dependencies <- list(
      htmlDependency(
        name = "leaflet_hbs",
        version = "1.0.0"
        # if local file use file instead of href below
        #  with an absolute path
        , src = c(file = normalizePath(paste0(getwd(), "/CSS/Leaflet-Stil"))),
        stylesheet = "rstudio_leaflet_aiwa.css"
      )
    )

    fig
  })

  output$table <- renderDT({
    Data[[names(which(map(Data, "title") == input$auswahl))]]$dat %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      as_tibble() %>%
      datatable(
        width = 596,
        extensions = "Buttons",
        options = list(
          auto_width = F,
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print"),
          lengthMenu = list(
            c(10, 25, 50, -1),
            c(10, 25, 50, "All")
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Arial'});",
            "}"
          ),
          headerCallback = JS(headerCallback)
        ),
        rownames = F,
        filter = "top"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
