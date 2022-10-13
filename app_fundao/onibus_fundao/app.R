


library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(dipsaus)

# Define UI for application that draws a histogram
ui <- fluidPage(

  shinyjs::useShinyjs(), # autorefresh

  # Application title
  titlePanel("Cadê o meu ônibus?"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(

    sidebarPanel(
      p("Encontre os ônibus que vão para o Fundão em tempo real."),

      selectInput("linha",
                  "Linha de ônibus",
                  c('321', '323', '325', '327', '329', '485', '486', '616',
                    '634', '635', '663', '696', '910A', '913', '914', '915',
                    '936', '945')),

      selectInput("sentido",
                  "Indo ou voltando do Fundão",
                  c('I', 'V'))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      p(),
      geoloc::onload_geoloc(),
      leafletOutput("mymap")
      )
    ),
  hr(),
  print(paste0("Desenvolvido por ")),
  tags$a(href = "https://igorlaltuf.github.io/", "Igor Laltuf")
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

    shinyjs::runjs(
      "function reload_page() {
      window.location.reload();
      setTimeout(reload_page, 60000);
    }
    setTimeout(reload_page, 60000);
    ")

    query_gtfs <- function(linha, ida_volta) {

    # retornar shape da linha
    shape_fundao <- st_read('shape_linhas_fundao/shape_linhas_fundao.shp') %>%
      dplyr::filter(grepl(linha, shape_id)) %>%
      dplyr::filter(grepl(input$sentido, shape_id))

    return(shape_fundao)
  }

  # acessar dados de GPS da API da prefeitura ----------------------------------

  query_sppo <- function(linha) {

    shape_fundao <- query_gtfs(linha)

    link <- paste0('https://jeap.rio.rj.gov.br/dadosAbertosAPI/v2/transporte/veiculos/onibus2/', linha)

    x <- fromJSON(txt = link) %>%
          as_tibble(x, validate = F) %>%
          mutate(latitude = as.numeric(latitude),
                 longitude = as.numeric(longitude))

    # Usar o EPSG 5641 para projecao em metros

    st_transform(shape_fundao, st_crs(5641))
    shape_buffer <- st_buffer(shape_fundao, 50) # distância em metros por causa da projeção que usei

    # plot(shape_buffer$geometry)
    pontos <- st_as_sf(x, coords = c('longitude','latitude'), crs = 5641)
    st_transform(pontos, st_crs(5641))
    # plot(pontos$geometry)
    st_crs(shape_buffer) <- 5641
    st_transform(shape_buffer, st_crs(5641))
    pontos <- st_intersection(pontos, shape_buffer)
    # plot(pontos$geometry)
    pontos <- left_join(pontos, x)

    return(pontos)

  }

  # fazer mapa com base nisso
  output$mymap <- renderLeaflet({

    shape_fundao <- query_gtfs(linha = input$linha, ida_volta = input$sentido)

    pontos <- query_sppo(linha = input$linha)

    req(input$geoloc_lon)
    req(input$geoloc_lat)

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = sf::st_zm(shape_fundao)) %>%
      setView(as.numeric(input$geoloc_lon), as.numeric(input$geoloc_lat), zoom = 17) %>%
      addCircleMarkers(
        data = pontos,
        color = 'red',
        fillOpacity = 0.5,
        radius = 3,
        lng = ~longitude,
        lat = ~latitude
      )
  })

}

shinyApp(ui = ui, server = server)
