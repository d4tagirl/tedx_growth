library(rtweet)
library(dplyr)
library(lubridate)
library(ggmap)
library(tidyr)
library(purrr)
library(ggthemes)
library(maps)
library(plotly)
library(gganimate)
library(tibble)

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #
#  Extracción y preparación de datos: Twitter y Google Maps #
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #

# Descargo integrantes de la lista tedx/tedx
# local_tedx <- lists_members(slug = "tedx", owner_user = "tedx", n = 5000)

# Guardo así no tengo que ir contra la API cada vez
# saveRDS(local_tedx, "local_tedx")

# La levanto del archivo 
local_tedx <- readRDS("local_tedx") %>% 
  filter(location != "") %>%
  mutate(age_days = difftime(as.Date('2011-04-01'), created_at, unit = 'days')) %>%
  select(screen_name, location, created_at, followers = followers_count, age_days) %>%
  mutate(longlat = purrr::map(.$location, geocode)) %>% 
  unnest() 

# por el funcionamiento de la API de Google maps hay que hacer esto varias veces hasta que 
# geocodifica todo

local_tedx_withlon <- local_tedx %>% 
  filter(!is.na(lon))

temp <- local_tedx %>% 
  select(-lon, -lat) %>% 
  anti_join(local_tedx_withlon %>% select(-lon, -lat)) %>% 
  mutate(longlat = purrr::map(.$location, geocode)) %>% 
  unnest() %>% 
  filter(!is.na(lon))

local_tedx_withlon <- local_tedx_withlon %>% 
  bind_rows(temp) %>% 
  distinct()

# Guardo df final
# saveRDS(local_tedx_withlon, "local_tedx_withlon")

# Con este data frame es con el que trabajo
tedx <- readRDS("local_tedx_withlon")

# ~ ~ ~ ~ #
#  Mapas! #
# ~ ~ ~ ~ #

# mapa base del mundo
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

# static map 
map <- world +
  geom_point(aes(x = lon, y = lat,
                 size = followers),    # add the size aes for later gganimate
             data = tedx, 
             colour = '#e62b1e', alpha = .5) +
  scale_size_continuous(range = c(1, 9), 
                        breaks = c(250, 1000, 10000, 25000, 450000),
                        labels = c("0.25k", "1k", "10k", "25k", "450k")) +
  labs(size = 'Followers') +
  theme(legend.text=element_text(size=10),
        legend.title=element_text(size=15),
        legend.position = c(0.05, 0.15),
        plot.title=element_text(size=14, hjust = 0.05, vjust=-0.12)) 

# mapa animado!

# punto inicial con mapa vacío para comienzo del gif
ghost_point <- tedx %>%
  add_row(
    created_at = as.Date('2008-03-12'),
    followers = 0,
    lon = 0,
    lat = 0,
    .before = 1) %>%
  slice(1) %>% 
  mutate(date = format(created_at, format = '%Y-%m-%d'),
         est_followers = 0)

# creo secuencia de fechas con los primeros de cada mes
dates <- as_tibble(seq(floor_date(as.Date(min(tedx$created_at)), 
                                  unit = "month"),
                       as.Date('2011-04-01'),
                       by = 'days')) %>%
  filter(day(value) %in% c(1))

# creo un data frame con cada punto para cada cuenta de Twitter
tedx_frames <- tedx %>%
  select(screen_name) %>%
  expand(screen_name, date = dates$value) %>%
  right_join(tedx, by = 'screen_name') %>%
  filter(date > created_at) %>%
  mutate(date = format(date, format = '%Y-%m-%d'),
         age_total = as.numeric(age_days, units = 'days'),
         age_at_date = as.numeric(difftime(date, created_at, units = 'days'),
                                  units = 'days'),
         est_followers = ((followers - 1) / age_total) * age_at_date) 

#   Animación sólo de Bs As:
#   %>%
#   filter(screen_name=="tedxbuenosaires" )
#
# ggplot(tedx_frames, aes(x=as.Date(date), y=est_followers, group = 1)) +
#   geom_line(color = "turquoise", size=3) + 
#   theme_classic() +
#   # ggtitle("Crecimiento de seguidores estimado") +
#   xlab("") +
#   ylab("") +
#   theme(axis.text.x = element_text(size=16, angle=60, hjust=1.2),
#         axis.text.y = element_text(size=14),
#         axis.title = element_text(size=16),
#         plot.title=element_text(size=20),
#         panel.grid.major = element_line( color="grey90" )) +
#   scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y")

# tedx_less_frames <- tedx_frames %>%
#   filter((day(date) == 1 & month(date) %% 2 == 0))

# Animación final!
map_frames <- world +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = zoo::as.yearmon(date)),
             # data = tedx_less_frames, colour = '#e62b1e', alpha = .5) +
             data = tedx_frames, colour = '#e62b1e', alpha = .5) +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = ghost_point, alpha = 0)  +
  theme(legend.text=element_text(size=16),
        legend.title=element_text(size=20),
        legend.position = c(0.05, 0.15),
        plot.title=element_text(size=20, hjust = 0.05, vjust=-0.12)) +
  scale_size_continuous(range = c(1, 9), breaks = c(250, 1000, 10000, 100000, 450000),
                        labels = c("0.25k", "1k", "10k", "100k", "450k")) +
  labs(size = 'Seguidores') 

animation::ani.options(ani.width = 1125, ani.height = 675)
gganimate::gganimate(map_frames, interval = .25, "tedx_growth.gif")
