# CONSTRUIR MAPA INTERATIVO
# Código criado por Jefferson Rodrigues em marco/2019

# carregar os pacotes:
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(rgdal)
library(sp)
library(htmlwidgets)


# Selecionar arquivo na janela pop-up
#arquivo_carregado <- file.choose(new = TRUE) #'geocodificado.csv' na pasta de trabalho

base <- read_delim('geocodificado.csv',
#base <- read_delim(arquivo_carregado,
                   #col_types = cols(lon = col_number(), 
                    #                lat = col_number()),
                   ";", escape_double = FALSE)
base <- base %>% 
        filter(!str_detect(NO_FANTASIA, 'MOTOE')) #*REMOVE A ENTRADA OUTLIER QUE APARECEU EM PORTUGAL



#carregar .CSV preparada com os dados agregados por DA
dados_da <- read_delim("rd_pcapta_pop_mortinf.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
View(dados_da)                     

#carregar shape files
dir('SHAPE')
distr_adm <- readOGR(dsn = 'SHAPE', 
                     layer = 'SIRGAS_SHP_distrito_polygon'
                     )

#carregar as coordenadas geograficas corretas (SIRGAS 2000)
proj4string(distr_adm) <- CRS("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#transformar as coordenadas para o leaflet (WGS84)
distr_adm_longlat <- spTransform(distr_adm, CRS("+proj=longlat +datum=WGS84 +no_defs"))


#juntando o shape com os DA (distr_adm_longlat) com o .CSV (dados_da) carregados
distr_adm_longlat <- merge(x = distr_adm_longlat, y= dados_da)

#ver a lista de base cartografica para ser escolhida
names(providers) 

#montar a base cartografica do mapa
mapa <- base %>%  
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 

# adicionar as base alternativas do mapa (color/pb)
        addTiles(group = "Colorido") %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite", 
                         group = "Preto e Branco")


#Grupos por Tipo de Unidade
unique(base$TP_UNIDADE)

tipos_unidade <- c("CONSULTORIO ISOLADO",
                   "CLINICA/CENTRO DE ESPECIALIDADE",
                   "SERVICO DE ATENCAO DOMICILIAR ISOLADO(HOME CARE)",
                   "HOSPITAL GERAL",
                   "POLICLINICA",
                   "HOSPITAL ESPECIALIZADO",
                   "COOPERATIVA OU EMPRESA DE CESSAO DE TRABALHADORES NA SAUDE",
                   "UNIDADE MISTA",
                   "HOSPITAL/DIA - ISOLADO",
                   "PRONTO ATENDIMENTO",
                   "PRONTO SOCORRO GERAL",
                   "PRONTO SOCORRO ESPECIALIZADO"
)

#escolher as paletas de cores
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=FALSE)

paleta_cores <- colorFactor(palette = brewer.pal(12,"Paired"), levels = tipos_unidade)
pal_mortinf <- colorNumeric(palette = "Reds",
                       domain = distr_adm_longlat@data$`Taxa de Mortalidade Infantil-2016`)

pal_renda <- colorNumeric(palette = "Greens",
                            domain = distr_adm_longlat@data$`Renda per Capita - Censo Demografico (Em reais correntes)`)

pal_pop <- colorNumeric(palette = "Blues",
                            domain = distr_adm_longlat@data$`Populacao 2019`)


#Agrupar as categorias da coluna TP_UNIDADE
cons_isol <- base %>%
        filter(TP_UNIDADE == "CONSULTORIO ISOLADO")

especialidade <- base %>%
        filter(TP_UNIDADE == "CLINICA/CENTRO DE ESPECIALIDADE")

home_care <- base %>%
        filter(TP_UNIDADE == "SERVICO DE ATENCAO DOMICILIAR ISOLADO(HOME CARE)")

hosp_geral <- base %>%
        filter(TP_UNIDADE ==  "HOSPITAL GERAL")

policlinica <- base %>%
        filter(TP_UNIDADE ==  "POLICLINICA")

hosp_espec <- base %>%
        filter(TP_UNIDADE ==  "HOSPITAL ESPECIALIZADO")

cooperativa <- base %>%
        filter(TP_UNIDADE =="COOPERATIVA OU EMPRESA DE CESSAO DE TRABALHADORES NA SAUDE")

unidd_mista <- base %>%
        filter(TP_UNIDADE == "UNIDADE MISTA")

hosp_dia <- base %>%
        filter(TP_UNIDADE == "HOSPITAL/DIA - ISOLADO")

pa <- base %>%
        filter(TP_UNIDADE == "PRONTO ATENDIMENTO")

ps <- base %>%
        filter(TP_UNIDADE == "PRONTO SOCORRO GERAL")

ps_esp <- base %>%
        filter(TP_UNIDADE =="PRONTO SOCORRO ESPECIALIZADO") 
                       
publicar_mapa <- 
        mapa %>%
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = especialidade,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "CONSULTORIO ISOLADO") %>% 
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = cons_isol,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "CLINICA/CENTRO DE ESPECIALIDADE") %>% 
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = home_care,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "SERVICO DE ATENCAO DOMICILIAR ISOLADO(HOME CARE)") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = hosp_geral,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "HOSPITAL GERAL") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = policlinica,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "POLICLINICA") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = hosp_espec,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "HOSPITAL ESPECIALIZADO") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = cooperativa,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "COOPERATIVA OU EMPRESA DE CESSAO DE TRABALHADORES NA SAUDE") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = unidd_mista,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "UNIDADE MISTA") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = hosp_dia,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "HOSPITAL/DIA - ISOLADO") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = pa,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "PRONTO ATENDIMENTO") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = ps,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "PRONTO SOCORRO GERAL") %>%
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         data = ps_esp,
                         color = 'black', 
                         fillColor = ~paleta_cores(TP_UNIDADE),
                         fillOpacity = 1,
                         stroke = TRUE,
                         radius = 3,
                         label = ~NO_FANTASIA,
                         group = "PRONTO SOCORRO ESPECIALIZADO") %>% 
        
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         radius = 1, 
                         clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE),
                         group = "UNIDADES DE SA?DE - CONCENTRA??O") %>%
        
        addHeatmap(lng = ~lon,
                   lat = ~lat,
                   blur = 20, 
                   max = 0.05, 
                   radius = 15,
                   group = "UNIDADES DE SA?DE - HEATMAP")                         %>%
        
        addMapPane(name = "DA", zIndex = 300) %>%
        addMapPane(name = "polygons", zIndex = 400) %>%
        
        addPolygons(data = distr_adm_longlat,
                    weight = 1,
                    color = 'black',
                    fillColor = 'orange',
                    fillOpacity = 0.1,
                    options = leafletOptions(pane = "DA"),
                    label =  ~ds_nome) %>%
        
        addPolygons(data = distr_adm_longlat,
                    weight = 1,
                    color = 'black',
                    fillColor = 'orange',
                    options = leafletOptions(pane = "polygons"),
                    highlight = highlightOptions(weight = 3,
                                                 color = 'red',
                                                 fillOpacity = 0.5,
                                                 bringToFront = TRUE),
                    label =  ~ds_nome,
                    group = 'DISTRITO ADMINISTRATIVO') %>%
        
        addPolygons(data = distr_adm_longlat,
                    weight = 1,
                    fillOpacity = 0.5,
                    color = 'black',
                    fillColor = ~pal_mortinf(`Taxa de Mortalidade Infantil-2016`),
                    label = ~paste0("Tx Mortalidade Infantil:", (`Taxa de Mortalidade Infantil-2016`)),
                    options = leafletOptions(pane = "polygons"),
                    highlight = highlightOptions(weight = 3,
                                                 color = "yellow",
                                                 bringToFront = TRUE),
                    group = 'MORTALIDADE INFANTIL - 2016(CEInfo)') %>% 
        
        addPolygons(data = distr_adm_longlat,
                    weight = 1,
                    fillOpacity = 0.5,
                    color = 'black',
                    fillColor = ~pal_pop(`Populacao 2019`),
                    label = ~paste0("Popula??o em 2019:", (`Populacao 2019`)),
                    options = leafletOptions(pane = "polygons"),
                    highlight = highlightOptions(weight = 3,
                                                 color = "red",
                                                 bringToFront = TRUE),
                    group = 'ESTIMATIVA POPULA??O - 2019(FUND. SEADE)') %>% 
        
        addPolygons(data = distr_adm_longlat,
                    weight = 1,
                    fillOpacity = 0.5,
                    color = 'black',
                    fillColor =  ~pal_renda(`Renda per Capita - Censo Demografico (Em reais correntes)`),
                    label = ~paste0("Renda per capta:", (`Renda per Capita - Censo Demografico (Em reais correntes)`)),
                    options = leafletOptions(pane = "polygons"),
                    highlight = highlightOptions(weight = 3,
                                                 color = "red",
                                                 bringToFront = TRUE),
                    group = 'RENDA PER CAPTA - 2010(FUND. SEADE)') %>% 
        
        
        
        # Adicionar controle de camadas por grupo de base e pontos 
        addLayersControl(baseGroups = c("Colorido",
                                        "Preto e Branco"),
                         overlayGroups = c("UNIDADES DE SAUDE - CONCENTRACAO",
                                           "UNIDADES DE SAUDE - HEATMAP",
                                           "DISTRITO ADMINISTRATIVO",
                                           "RENDA PER CAPTA - 2010(FUND. SEADE)",
                                           "ESTIMATIVA POPULACAO - 2019(FUND. SEADE)",
                                           "MORTALIDADE INFANTIL - 2016(CEInfo)",
                                           "CLINICA/CENTRO DE ESPECIALIDADE",
                                           "CONSULTORIO ISOLADO",
                                           "COOPERATIVA OU EMPRESA DE CESSAO DE TRABALHADORES NA SAUDE",
                                           "HOSPITAL ESPECIALIZADO",
                                           "HOSPITAL GERAL",
                                           "HOSPITAL/DIA - ISOLADO",
                                           "POLICLINICA",
                                           "PRONTO ATENDIMENTO",
                                           "PRONTO SOCORRO GERAL",
                                           "PRONTO SOCORRO ESPECIALIZADO",
                                           "SERVICO DE ATENCAO DOMICILIAR ISOLADO(HOME CARE)",
                                           "UNIDADE MISTA")) 
publicar_mapa

saveWidget(publicar_mapa, file="2019_01_CNES_saude_supl-jeff.html", selfcontained = FALSE)
