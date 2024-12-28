library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)
library(shinythemes)
library(jsonlite)
options(jsonlite.auto_unbox = TRUE)

# Читаем данные о ДТП
dtp_data <- st_read("./data/dtp_khakassia.gpkg")
dtp_data <- dtp_data[!st_is_empty(dtp_data), ]
dtp_data <- dtp_data %>% mutate(month = month(datetime, label = TRUE))

# Читаем данные о границах районов Хакасии
khakassia_districts <- st_read("./data/khakassia_muni_border.gpkg")
khakassia_districts <- st_transform(khakassia_districts, crs = 4326)

region_mapping <- c(
  "Таштапский район" = "Таштыпский муниципальный район",
  "Бейский район" = "Бейский муниципальный район",
  "Усть-Абаканский район" = "Усть-Абаканский муниципальный район",
  "Ширинский район" = "Ширинский муниципальный район",
  "Орджоникидзевский район" = "Орджоникидзевский муниципальный район",
  "Боградский район" = "Боградский муниципальный район",
  "Аскизский район" = "Аскизский муниципальный район",
  "Алтайский район" = "Алтайский муниципальный район",
  "Абаза" = "город Абаза",
  "Саяногорск" = "город Саяногорск",
  "Абакан" = "город Абакан",
  "Черногорск" = "город Черногорск",
  "Сорск" = "город Сорск"
)

dtp_data <- dtp_data %>%
  mutate(region = recode(region, !!!region_mapping))

# Текст для информационного окна
text_about <- HTML(
  "<p><strong>Анализ данных о ДТП в Республике Хакасия</strong></p>
   <ul>
     <li>Наибольшее число ДТП наблюдается в летние месяцы, особенно в июне и августе, что может быть связано с увеличением трафика в период отпусков.</li>
     <li>Чаще всего ДТП происходят в светлое время суток, что обусловлено более высокой активностью транспортных средств.</li>
     <li>Основные категории происшествий включают столкновения и съезды с дороги, которые составляют большую часть зарегистрированных инцидентов.</li>
     <li>Тяжесть происшествий выше в темное время суток и при неблагоприятных погодных условиях, таких как дождь и снег.</li>
     <li>Наезд на пешеходов и столкновения с другими транспортными средствами чаще приводят к тяжелым последствиям.</li>
   </ul>
   <p><strong>Источник данных:</strong> https://dtp-stat.ru/opendata.<br>
      <strong>Автор приложения:</strong> [Наталья Сосова].</p>"
)


# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Анализ ДТП в Республике Хакасия"),
  sidebarLayout(
    sidebarPanel(
      h4("Фильтры анализа"),
      selectInput("district", "Район:",
                  unique(dtp_data$region)),
      dateRangeInput("dates", "Период времени:",
                     start = min(dtp_data$datetime, na.rm = TRUE),
                     end = max(dtp_data$datetime, na.rm = TRUE)),
      checkboxGroupInput("severity", "Последствия для здоровья:",
                         choices = c("Легкий", "Тяжелый", "С погибшими"),
                         selected = c("Легкий", "Тяжелый", "С погибшими")),
      checkboxGroupInput(
        "light_conditions", 
        "Время суток:", 
        choices = unique(dtp_data$light), 
        selected = unique(dtp_data$light)
      ),
      selectInput("weather", "Погода:",
                  unique(dtp_data$weather)),
      actionButton("showInfo", "О проекте"),
      hr(),
      h6("Выберите фильтры и изучите данные о ДТП.")
    ),
    mainPanel(
      h4(textOutput("summary")),
      tabsetPanel(
        tabPanel("Карта", leafletOutput("map", height = "600px")),
        tabPanel("Статистика", plotlyOutput("plot1", height = "400px")),
        tabPanel("Таблица", DTOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  
  dtp = reactive(dtp_data %>% 
                   filter(region==input$district, 
                          severity %in% input$severity,
                          light %in% input$light_conditions,
                          weather %in% input$weather))
  district = reactive(khakassia_districts %>% filter(name==input$district))
  bounds = reactive(district() %>% st_bbox() %>% as.character())
  
  output$map <- leaflet::renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data=dtp_data, clusterOptions = markerClusterOptions(), group="ДТП кластеры")%>%
      addCircles(data=dtp(), group="ДТП  точки", radius=0.5, color="purple") %>%
      addPolygons(data=district(), fillOpacity = 0, weight=1, group="Районы")  %>%
      hideGroup("ДТП кластеры") %>%
      addLayersControl(
        overlayGroups = c("ДТП  точки", "Районы"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      fitBounds(bounds()[1],bounds()[2],bounds()[3],bounds()[4])
  })
  
  output$summary <- renderText({
    req(dtp())
    paste(
      "Всего ДТП:", nrow(dtp()),
      "\nСреднее число участников:", round(mean(dtp()$participants_count, na.rm = TRUE), 2),
      "\nСреднее число пострадавших:", round(mean(dtp()$injured_count, na.rm = TRUE), 2)
    )
  })
  
  output$plot1 = plotly::renderPlotly({
    ggplot(dtp(), aes(x=month, fill=category))+
      geom_bar()
  })
  
  output$table <- DT::renderDT({
    datatable(
      dtp() %>%
        as.data.frame() %>%
        select(category, address, light, weather, datetime, severity, participants_count),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    )
  })
  
  
  observeEvent(input$showInfo, {
    showModal(modalDialog(
      text_about,
      title = "О проекте"
    ))
  })
  
}

# Запуск приложения 
shinyApp(ui = ui, server = server)










