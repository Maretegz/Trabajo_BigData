library(tidyverse)
library(klippy)  
library(knitr)
library(tibble)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggrepel)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(pjpv2020.01)
library(plotly)
library(gganimate)
library (gifski)
library(gt)
library(kableExtra)
library(DT)
library(leaflet) 
library(leafem) 
library(DT)
library(tmap)
library(leaflet)
library(vembedr)
library(hrbrthemes)
library(readxl)
mpd2020 <- read_excel("mpd2020.xlsx", sheet = "Full data")
mpd2020_1 <- mpd2020 %>% 
  filter(countrycode == "ZAF")
mpd2020_1 %>%
  ggplot( aes(x=year, y=gdppc)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=1) +
  theme_ipsum() +
  ggtitle("PIB PER CÁPITA SUDÁFRICA") +
  scale_x_continuous("Año", limits = c(1700, 2020)) + 
  scale_y_continuous("PIB per cáptita", labels = scales::dollar)

g1 <-   ggplot(mpd2020_1, aes(x=year, y=gdppc)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=1) +
  theme_ipsum() +
  ggtitle("PIB PER CÁPITA SUDÁFRICA") +
  scale_x_continuous("Año", limits = c(1700, 2020)) + 
  scale_y_continuous("PIB per cáptita", labels = scales::dollar) +
  labs(caption = "Elaboración propia con datos del Madison Project Database")
g1
g1 + scale_y_log10("Logaritmo PPC", NULL)
library(tmap)
data(World) 
world<- World %>%
  rename(countrycode = iso_a3) %>%
  select(countrycode, continent)
df1 <- inner_join(mpd2020, world) %>%
  filter(continent == "Africa")
df2 <- df1 %>% filter(year == 1995)
mapa <- ggplot() +
  geom_sf(data = df2, aes(geometry = geometry, fill = gdppc)) +
  theme_void()  +
  theme(plot.subtitle = element_text(family = "serif", face = "italic"),
        plot.title = element_text(family = "serif", face =  "italic")) +
  labs(title = "PIB per cápita en África", fill = "En dólares")  +
  scale_fill_continuous(low="#fffcbf",high="#3e9851") + 
  labs(title = "PIB per cápita en África año 1995",
       caption = "Fuente: Elaboración propia con datos del Madison Project Database")
mapa 


df4<- df1 %>% filter(year >= 1950)

df5 <- df4 %>% mutate(paises = country)  
df6 <- df5 %>%
  filter(country == 'South Africa')
g7 <- ggplot(df6, aes(x=year, y=gdppc)) +
  geom_line(data=df5 %>% dplyr::select(-country), aes(group=paises), color="grey", size=0.5, alpha=1) +
  geom_line( aes(color=country), color="#69b3a2", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "left",
    plot.title = element_text(size=14),
    panel.grid = element_blank()) +
  labs(title = "PIB per cápita países africanos", caption = "Elaboración propia con datos del Madison Project Database") + 
  ylab(NULL) + xlab(NULL) +
  scale_y_continuous(breaks = seq(0, 25000, 5000), limits = c(0, 25000))
ggplotly(g7) 
g7
df9 <- mpd2020 %>% filter(country %in% c("Spain", "Brazil", "India", "China", "South Africa")) %>% filter(year %in% c("1700", "1850", "1900")) %>% drop_na(gdppc)
g9 <- ggplot(df9, aes(country, gdppc)) + 
  geom_bar(stat="identity", fill="MediumSlateBlue") + 
  theme_classic() + 
  geom_text(aes(y = gdppc, label = gdppc), 
            position = position_dodge(width = 0.9), size=3,
            vjust=-0.1, hjust=0.5 ,col="black", fontface = "bold") +
  labs(title = "Comparación con los BRICS + España",
       subtitle = "(PIB per cápita)",
       x = "Países",
       y = "PIB per cápita", 
       caption = "Elaboración propia con datos del Madison Project Database") + 
  theme(plot.title = element_text(size = 20, face = "bold", 
                                  hjust = 0.5), 
        plot.subtitle = element_text(size = 20, hjust = 0.5)) +
  facet_wrap(vars(year), nrow = 1) +
  scale_x_discrete(labels=c("Brasil","China","India",
                            "Sudáfrica", "España")) +
  scale_y_continuous(limits = c(0, 3000)) + theme(axis.text.x = element_text(angle=90))
g9

df10 <- mpd2020 %>% filter(country %in% c("Spain", "Brazil", "India", "China", "Russian Federation", "South Africa")) %>% filter(year %in% c("1950", "1995", "2018")) %>% drop_na(gdppc)
g8 <- ggplot(df10, aes(country, gdppc)) + 
  geom_bar(stat="identity", fill="MediumSlateBlue") + 
  theme_classic() + 
  geom_text(aes(y = gdppc, label = gdppc), 
            position = position_dodge(width = 0.9), size=3,
            vjust=-0.1, hjust=0.5 ,col="black", fontface = "bold") +
  labs(title = "Comparación con los BRICS + España",
       subtitle = "(PIB per cápita)",
       x = "Países",
       y = "PIB per cápita", 
       caption = "Elaboración propia con datos del Madison Project Database") + 
  theme(plot.title = element_text(size = 20, face = "bold", 
                                  hjust = 0.5), 
        plot.subtitle = element_text(size = 20, hjust = 0.5)) +
  facet_wrap(vars(year), nrow = 1) +
  scale_x_discrete(labels=c("Brasil","China","India",
                            "Rusia", "Sudáfrica", "España")) +
  scale_y_continuous(limits = c(0, 32000)) + theme(axis.text.x = element_text(angle=90))
g8
library(hrbrthemes)
library(viridis)
library(forcats)
banco_mundial <- read_excel("banco_mundial.xlsx")
banco_mundial[, c(5:65)] <- sapply(banco_mundial[, c(5:65)], as.numeric)
banco_mundial <-banco_mundial%>%
  pivot_longer(cols = c(5:65), values_to = "series", names_to = "year" )

str(banco_mundial)
banco_mundial_1 <- banco_mundial %>% select(year, series, 'Series Name', 'Country Name', `Country Code`) %>%
  pivot_wider(names_from = 'Series Name', values_from = series, values_fn = sum)
esp <- banco_mundial_1 %>% select(year, 'Country Name', 'Esperanza de vida al nacer, total (años)')
esp <- janitor::clean_names(esp)
esp_1 <- esp %>% filter(country_name == "Sudáfrica")

esp_2 <- esp %>% mutate(paises = country_name) %>% filter(country_name %in% c("Brasil", "Federación de Rusia", "India", "China", "España")) 
g20 <- ggplot(esp_1, aes(x=year, y=esperanza_de_vida_al_nacer_total_anos )) +
  geom_line(data=esp_2 %>% dplyr::select(-country_name), aes(group=paises), color="grey", size=0.5, alpha=1) +
  geom_line( aes(group=country_name), color="#3e9851", size=1.2 ) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "left",
    plot.title = element_text(size=14),
    panel.grid = element_blank()) +
  labs(title = "Esperanza de vida en el mundo", caption = "Elaboración propia con datos del Banco Mundial") + 
  ylab(NULL) + xlab("1960 - 2020") +
  scale_y_continuous(breaks = seq(20, 100, 10)) + scale_x_discrete( breaks = c(1960, 1980, 1990, 2000, 2010, 2020))+ 
  theme(axis.text.x = element_text(angle=90)) +
  annotate(geom = "text", x= "2018[YR2018]", y = 66,label = "Sudáfrica",size = 3,  color = "black") + annotate(geom = "text", x= "2018[YR2018]", y = 71,label = "India",size = 3,  color = "black") + annotate(geom = "text", x= "2018[YR2018]", y = 85,label = "España",size = 3,  color = "black") + annotate(geom = "text", x= "2018[YR2018]", y = 79,label = "China",size = 3,  color = "black") + annotate(geom = "text", x= "2018[YR2018]", y = 76,label = "Brasil",size = 3,  color = "black") + annotate(geom = "text", x= "2018[YR2018]", y = 74,label = "Rusia",size = 3,  color = "black")
ggplotly(g20)
g20
esp <- esp %>% mutate(paises = country_name)
g21 <- ggplot(esp_1, aes(x=year, y=esperanza_de_vida_al_nacer_total_anos )) +
  geom_line(data=esp %>% dplyr::select(-country_name), aes(group=paises), color="grey", size=0.5, alpha=1) +
  geom_line( aes(group=country_name), color="#3e9851", size=1.2 ) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "left",
    plot.title = element_text(size=14),
    panel.grid = element_blank()) +
  labs(title = "Esperanza de vida en el mundo", caption = "Elaboración propia con datos del Banco Mundial") + 
  ylab("Años esperados") + xlab("1960,2020") +
  scale_y_continuous(breaks = seq(20, 100, 20)) + scale_x_discrete( breaks = c(1960, 2020))+ 
  theme(axis.text.x = element_text(angle=90)) 
ggplotly(g21)

df_sector <- banco_mundial_1 %>% select(year, `Country Name`, `Agricultura, valor agregado (% del PIB)`, `Industria, valor agregado (% del PIB)`) 
df_sector <- janitor::clean_names(df_sector)
df_sector <- df_sector %>% filter(country_name == 'Sudáfrica') %>% 
  mutate(servicios = 100 - (agricultura_valor_agregado_percent_del_pib + industria_valor_agregado_percent_del_pib))
df_sector1 <- df_sector %>%
  filter(year == "2013 [YR2013]") %>% select(-c(year, country_name))
library(lessR)
pie(s1960, labels = paste0(s1960, "%"))
s1960 <- c(1.92, 24.55, 73.52)
legend("topleft", legend = c("Agricultura", "Industria", "Servicios"),
       fill =  c("white", "lightblue", "mistyrose"))
title("Peso de los sectores en el VAB en 2013")

df_comercio <- banco_mundial_1 %>% select(year, `Country Name`, `Exportaciones de bienes y servicios (% del PIB)`, `Importaciones de bienes y servicios (% del PIB)`) %>% 
  filter(`Country Name` == 'Sudáfrica')
df_comercio <- df_comercio %>% mutate(tasa_apertura = `Exportaciones de bienes y servicios (% del PIB)`+ `Importaciones de bienes y servicios (% del PIB)`) %>%
  mutate(tasa_cobertura = `Exportaciones de bienes y servicios (% del PIB)`/`Importaciones de bienes y servicios (% del PIB)`)
date <- c(1960:2020)
df_comercio<-cbind(df_comercio, date)


df_comercio <- df_comercio%>% select(-year)

comercio <-   ggplot(df_comercio, aes(x=date, y=tasa_apertura)) +
  geom_line(data = df_comercio, aes(date, tasa_apertura), color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=1) +
  theme_ipsum() +
  labs(title = "Tasa de apertura", caption = "Elaboración propia con datos del Banco Mundial")  + ylab(NULL) + xlab("Año") +
  scale_y_continuous(breaks = seq(20, 100, 10), limits = c(10, 100)) + 
  scale_x_continuous(breaks = seq(1960,2020, 20))
comercio 
df_comercio2 <- banco_mundial_1 %>% select(year, `Country Name`, `Exportaciones de bienes y servicios (% del PIB)`, `Importaciones de bienes y servicios (% del PIB)`)          
df_comercio2 <- janitor::clean_names(df_comercio2)
df_comercio2<-cbind(df_comercio2, date)
df_comercio2 <- df_comercio2%>% select(-year)
df_comercio3 <- df_comercio2 %>%
  filter(country_name %in% c("Angola", "Sudáfrica", "Burundi", "Cabo Verde", "República Centroafricana", "República Democrática del Congo", "República del Congo", "Côte d'Ivoire", "Eritrea", "Etiopía", "Guinea", "Kenya", "Lesotho", "Liberia", "Madagascar", "Mauritania", "Mozambique", "Sierra Leona", "Somalia", "Sudán", "Botswana", "Tanzania", "Uganda", "Zimbabwe"))
df_comercio3 <- df_comercio3 %>% mutate(tasa_apertura = exportaciones_de_bienes_y_servicios_percent_del_pib + importaciones_de_bienes_y_servicios_percent_del_pib) %>%
  mutate(tasa_cobertura = (exportaciones_de_bienes_y_servicios_percent_del_pib/importaciones_de_bienes_y_servicios_percent_del_pib)*100) %>%
  group_by(date) %>% drop_na(tasa_apertura) %>% 
  mutate(media = mean(tasa_apertura))
df_comercio4 <- df_comercio3 %>% filter(country_name == 'Sudáfrica')
comercio2 <-   ggplot(df_comercio4, aes(x=date, y=tasa_apertura)) +
  geom_line(data = df_comercio4, aes(date, tasa_apertura), color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=1) +
  theme_ipsum() +
  labs(title = "Tasa de apertura (en %)", caption = "Elaboración propia con datos del Banco Mundial")  + ylab(NULL) + xlab("Año") +
  scale_y_continuous(breaks = seq(20, 100, 10), limits = c(20, 90)) + 
  scale_x_continuous(breaks = seq(1960,2020, 20))
comercio2 + geom_line(data = df_comercio3, aes(date, media), color="grey") +
  geom_point(data = df_comercio3, aes(date, media),  shape=21, color="green", fill="#69b3a2", size=1) +  annotate(geom = "text", x= 2020, y = 48,label = "Sudáfrica",size = 3,  color = "darkgrey") + annotate(geom = "text", x= 2016, y = 78,label = "África subsahariana",size = 3,  color = "green")
df_comercio3 <- df_comercio3 %>% select(-media) %>%
  mutate(media = mean(tasa_cobertura))
df_comercio4 <- df_comercio3 %>% filter(country_name == 'Sudáfrica')
comercio3 <-   ggplot(df_comercio4, aes(x=date, y=tasa_cobertura)) +
  geom_line(data = df_comercio4, aes(date, tasa_cobertura), color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=1) +
  theme_ipsum() +
  labs(title = "Tasa de cobertura (en %)", caption = "Elaboración propia con datos del Banco Mundial")  + ylab(NULL) + xlab("Año") +
  scale_y_continuous(breaks = seq(50, 150, 10), limits = c(50, 150)) + 
  scale_x_continuous(breaks = seq(1960,2020, 20))
comercio3 + geom_line(data = df_comercio3, aes(date, media), color="grey") +
  geom_point(data = df_comercio3, aes(date, media),  shape=21, color="green", fill="#69b3a2", size=1) +  annotate(geom = "text", x= 2020, y = 124,label = "Sudáfrica",size = 3,  color = "darkgrey") + annotate(geom = "text", x= 2016, y = 80,label = "África subsahariana",size = 3,  color = "green")
world <- World %>% rename(country_code = iso_a3)
banco_mundial_1<- banco_mundial_1  %>% rename(country_code = `Country Code`)
df_pobreza <-banco_mundial_1 %>% select(country_code, `Country Name`, year, `Tasa de incidencia de la pobreza, sobre la base de $1,90 por día (2011 PPA) (% de la población)`)
world <- world %>% select(country_code)
df_pobreza2 <- inner_join(world, df_pobreza)
df_pobreza2<-cbind(df_pobreza2, date)
df_pobreza2 <- df_pobreza2 %>% filter(date <= 1995) %>% drop_na(Tasa.de.incidencia.de.la.pobreza..sobre.la.base.de..1.90.por.día..2011.PPA.....de.la.población.)
df_pobreza3 <- df_pobreza2 %>% group_by(Country.Name) %>% summarise(media = mean(Tasa.de.incidencia.de.la.pobreza..sobre.la.base.de..1.90.por.día..2011.PPA.....de.la.población.))


mapa2 <- ggplot() +
  geom_sf(data = df_pobreza3, aes(geometry = geometry, fill = media)) +
  theme_void()  +
  theme(plot.subtitle = element_text(family = "serif", face = "italic"),
        plot.title = element_text(family = "serif", face =  "italic")) +
  labs(title = "Tasa de incidencia de la pobreza sobre la base de 1.90$ por día (2011 PPA)", 
       subtitle = "Media entre 1960 - 1995 " , 
       caption = "Fuente: Elaboración propia con datos del Madison Project Database",
       fill = "% de población")  +
  scale_fill_continuous(low="#fffcbf",high="#3e9851") + 
  theme(plot.title = element_text(hjust = 20), plot.subtitle = element_text(hjust = 0.3), 
        plot.caption = element_text(hjust = 1.5))

mapa2 
df_pobreza2 <- df_pobreza2 %>% filter(date >= 1995) %>% drop_na(Tasa.de.incidencia.de.la.pobreza..sobre.la.base.de..1.90.por.día..2011.PPA.....de.la.población.)
df_pobreza3 <- df_pobreza2 %>% group_by(Country.Name) %>% summarise(media = mean(Tasa.de.incidencia.de.la.pobreza..sobre.la.base.de..1.90.por.día..2011.PPA.....de.la.población.))
mapa3 <- ggplot() +
  geom_sf(data = df_pobreza3, aes(geometry = geometry, fill = media)) +
  theme_void()  +
  theme(plot.subtitle = element_text(family = "serif", face = "italic"),
        plot.title = element_text(family = "serif", face =  "italic")) +
  labs(title = "Tasa de incidencia de la pobreza sobre la base de 1.90$ por día (2011 PPA)", 
       subtitle = "Media entre 1995 - 2020 " , 
       caption = "Fuente: Elaboración propia con datos del Madison Project Database",
       fill = "% de población")  +
  scale_fill_continuous(low="#fffcbf",high="#3e9851") + 
  theme(plot.title = element_text(hjust = 20), plot.subtitle = element_text(hjust = 0.3), 
        plot.caption = element_text(hjust = 1.5))

mapa3 

ied <- banco_mundial_1 %>% select(year, 'Country Name', `Inversión extranjera directa, entrada neta de capital (balanza de pagos, US$ a precios actuales)`)
ied <- janitor::clean_names(ied)
ied <- ied %>% filter(country_name == "Sudáfrica")
date <- c(1960:2020)
ied<-cbind(ied, date)

ied <- ied %>% rename(IED = inversion_extranjera_directa_entrada_neta_de_capital_balanza_de_pagos_us_a_precios_actuales)
ied <- ied%>% select(-year)
ied <- ied %>% mutate(IED = IED/1000000)
ied <- ied%>% drop_na()
gied <-   ggplot(ied, aes(x=date, y=IED)) +
  geom_line(data = ied, aes(date, IED), color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=1) +
  theme_ipsum() +
  labs(title = "Inversión extranjera directa, entrada neta de capital ", subtitle = "Balanza de pagos, millones de US$ a precios actuales", caption = "Elaboración propia con datos del Banco Mundial")  + ylab(NULL) + xlab("Año") +
  scale_y_continuous(breaks = seq(-3000, 12000, 2000), limits = c(-3000, 12000)) + 
  scale_x_continuous(breaks = seq(1970,2020, 10))
gied
