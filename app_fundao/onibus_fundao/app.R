#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(dipsaus)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Ônibus para o Fundão"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(

    sidebarPanel(

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
      actionButtonStyled('recalc', 'Atualiza aí mermão', type = 'success'),
      p(),
      leafletOutput("mymap")
      )
    ),
  hr(),
  print("Desenvolvido por Igor Laltuf")

  )


# Define server logic required to draw a histogram
server <- function(input, output) {



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


  observeEvent(input$recalc, ignoreInit = FALSE, ignoreNULL = FALSE, {

    # pontos <- query_sppo(linha = input$linha)
    #
    # output$mymap <- renderLeaflet({
    #
    # leaflet() %>%
    #   addProviderTiles(providers$CartoDB.Positron) %>%
    #   addPolylines(data = sf::st_zm(shape_fundao)) %>%
    #   addCircleMarkers(
    #     data = pontos,
    #     color = 'red',
    #     fillOpacity = 0.5,
    #     radius = 3,
    #     lng = ~longitude,
    #     lat = ~latitude
    #   )
    #
    # })

  })



  # fazer mapa com base nisso
  output$mymap <- renderLeaflet({

    shape_fundao <- query_gtfs(linha = input$linha, ida_volta = input$sentido)

    pontos <- query_sppo(linha = input$linha)

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = sf::st_zm(shape_fundao)) %>%
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

# colocar o botao do buy me a coffee no shiny
# incluir desenvolvido por igor laltuf
