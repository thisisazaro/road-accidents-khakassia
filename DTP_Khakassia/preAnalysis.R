library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(lubridate)
library(plotly)


# Worlflow создания карты и графика на основе данных ДТП
## данные о дтп загружены с сайта https://dtp-stat.ru/opendata
## данные о границах районов Хакасии khakassia_muni_border.gpkg предварительно обработаны в QGIS

# Экспортируем данные
dtp_data = st_read("./data/dtp_khakassia.gpkg")
dtp_data = dtp_data[!st_is_empty(dtp_data),]
dtp_data = dtp_data %>% mutate(month = month(datetime, label = TRUE))

khakassia_districts = st_read("./data/khakassia_muni_border.gpkg")

# Фильтруем данные о ДТП по району
dtp_data_center <- dtp_data %>% filter(region == "Усть-Абаканский район")

# Фильтруем границы района
districts_center <- khakassia_districts %>% filter(name == "Усть-Абаканский муниципальный район")

# Преобразуем систему координат границ района
districts_center <- st_transform(districts_center, crs = 4326)

# Преобразуем данные о ДТП в систему координат WGS 84
dtp_data_center <- st_transform(dtp_data_center, crs = 4326)
dtp_data_center
# Определяем экстент (границы района)
bounds <- st_bbox(districts_center) %>% as.character()

# Создаём карту
leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = districts_center,
    fillOpacity = 0.3,
    color = "blue",
    weight = 2,
    label = ~paste("Район:", name),
    group = "Район"
  ) %>%
  addCircleMarkers(
    data = dtp_data_center,
    radius = 5,
    color = "red",
    label = ~paste("Категория:", category, "<br>Дата:", datetime),
    group = "ДТП"
  ) %>%
  fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
  addLayersControl(
    overlayGroups = c("Район", "ДТП"),
    options = layersControlOptions(collapsed = FALSE)
  )

# карта интенсивности ДТП в Хакасии 
leaflet() %>%
  addTiles() %>%
  addHeatmap(
    data = dtp_data_center,
    lng = ~st_coordinates(geom)[,1],
    lat = ~st_coordinates(geom)[,2],
    intensity = ~participants_count,
    blur = 20,
    max = 0.05,
    radius = 15
  )


#Количество участников
sum(dtp_data_center$participants_count)

#Количество погибших
sum(dtp_data_center$dead_count)

#Количество поcстрадавших
sum(dtp_data_center$injured_co)


#2.Графики с анализом ДТП в Республике Хакасия

ggplot(dtp_data_center, aes(x=month, fill=category))+
  geom_bar()

# Тенденции ДТП по месяцам
dtp_data_center %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Тенденции ДТП по месяцам", x = "Месяц", y = "Количество ДТП")

# Количество пострадавших по категориям
dtp_data_center %>%
  group_by(category) %>%
  summarise(total_injured = sum(injured_count)) %>%
  ggplot(aes(x = reorder(category, -total_injured), y = total_injured, fill = category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Количество пострадавших по категориям", x = "Категория", y = "Количество пострадавших")

# Тяжесть ДТП в зависимости от освещения
ggplot(dtp_data_center, aes(x = light, fill = severity)) +
  geom_bar() +
  labs(title = "Тяжесть ДТП в зависимости от освещения", x = "Освещение", y = "Количество")

# Распределение ДТП по времени суток
dtp_data_center %>%
  mutate(hour = hour(datetime)) %>%
  ggplot(aes(x = hour, fill = category)) +
  geom_histogram(binwidth = 1, position = "stack") +
  labs(title = "Распределение ДТП по времени суток", x = "Часы", y = "Количество")


# Выводы по анализу ДТП в Республике Хакасия
# Наибольшее число ДТП наблюдается в летние месяцы, особенно в июне и августе, что может быть связано с увеличением трафика в период отпусков. 
# Чаще всего ДТП происходят в светлое время суток, что обусловлено более высокой активностью транспортных средств. 
# Основные категории происшествий включают столкновения и съезды с дороги, которые составляют большую часть зарегистрированных инцидентов.
# Тяжесть происшествий выше в темное время суток и при неблагоприятных погодных условиях, таких как дождь и снег.
# Наезд на пешеходов и столкновения с другими транспортными средствами чаще приводят к тяжелым последствиям.
