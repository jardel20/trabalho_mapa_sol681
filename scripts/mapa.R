# Carregar pacotes
pacman::p_load(sf,
  dplyr,
  geobr,
  here,
  terra,
  dataCleanML,
  update = F
)

# Mostra a área que será usada como limite ou contorno
contorno <- sf::read_sf(here("dados", "limite", "limite.shp"))
plot(contorno)

# corrigir campo FID ------------------------------------------------------
corrigir_fid <- function(arquivo_entrada, arquivo_saida) {
  # Ler a camada do arquivo
  camada <- st_read(arquivo_entrada)

  # Remover campo chamado FID, se existir
  if ("FID" %in% colnames(camada)) {
    camada <- select(camada, -FID)
  }

  # Criar campo FID numérico sequencial (inteiro)
  camada$FID <- seq_len(nrow(camada))

  # Salvar a camada corrigida, substituindo se existir
  st_write(camada, arquivo_saida, delete_layer = TRUE)
}

# Exemplo de uso:
corrigir_fid("dados/limite/limite.shp", "dados/limite/corrigido.shp")

# A função cria_projeto não está nem funcionando
cria_projeto(
  nome = "triangulo",
  epsg = 31983, # não usar esse EPSG, escolher uma projeção métrica
  # trabalhar em separado essas áreas separados por fuso
  limite = here::here("dados", "limite", "corrigido.shp"),
  resolucao = 30
)

projeto <- readRDS(file = "triangulo/triangulo.RDS")

shp <- terra::vect(projeto$limite)
plot(shp)


# Rasterizando camadas vetoriais ------------------------------------------
# As variáveis de interesse estão armazenadas no formato vetorial;
# para utilizá-las em MDS, devemos convertê-las para raster

fn <- here::here("dados", "solos", "solo_corrigido.shp")
solos <- sf::st_read(fn)
str(solos)

vetor_path <- here::here(
  "dados",
  "solos",
  "solo_corrigido.shp"
)

info <- readRDS(here::here(
  "triangulo",
  "triangulo.RDS"
))


# limpar geometrias inválidas ---------------------------------------------

# Ler o arquivo espacial (shapefile, geodatabase etc)
camada <- st_read(vetor_path)

# Corrigir geometrias inválidas
camada_corrigida <- st_make_valid(camada)

# Alternativamente, outra técnica comum
# camada_corrigida <- st_buffer(vetor_path, 0)

# Salvar camada corrigida
st_write(
  obj = camada_corrigida,
  dsn = here("dados", "solos", "solo_corrigido.shp"),
  delete_layer = TRUE
)

solo_corrigido_path <- here("dados", "solos", "solo_corrigido.shp")

fn <- "dados/solos/solo_corrigido.shp"
solos <- sf::st_read(fn)
str(solos)

solos$Legenda <- iconv(solos$Legenda, from = "UTF-8", to = "ASCII//TRANSLIT") |>
  stringr::str_replace_all(pattern = " ", replacement = "_") |>
  stringr::str_replace_all(pattern = "-", replacement = "_")

st_write(
  obj = solos,
  dsn = here("dados", "solos", "solo_corrigido_2.shp"),
  delete_layer = TRUE
)

solo_corrigido2_path <- "dados/solos/solo_corrigido_2.shp"

prep <- prepara_vetor_projeto(
  vetor_path = solo_corrigido2_path,
  campo = "Legenda", # precisa ser o nome do campo entre aspas
  tipo = "categorico",
  info = info
)

tabela_area_por_classe(vetor_path = prep$fnvetor, campo_id = "campo_raster")

grupos <- list(
  nodata = c("Agua", "Afloramento_rochoso", "Area_Urbanizada")
)

nome_saida <- here::here(info$vetores, "solos_reclas.gpkg")

# agrupar_classes_shapefile(vetor_path = ,campo_id = ,grupos = ,nome_saida = )
vetor_agrupado <- agrupar_classes_shapefile(
  vetor_path = prep$fnvetor,
  campo_id   = "campo_raster",
  grupos     = grupos,
  nome_saida = nome_saida
)

fn <- nome_saida
tabela_area_por_classe(vetor_path = fn, campo_id = "classe_agrupada")

rr <- rasteriza_vetor_projeto(
  vetor_path = fn,
  tipo = "categorico",
  info = info,
  nome_base = prep$nome_base,
  legenda = prep$legenda,
  pasta = "covariaveis"
)

r <- rast(rr)
terra::plot(r,
  main = "Mapa de Classes de Solo",
  plg = list(cex = 0.9)
)

rb <- dataCleanML::raster_categorico_para_stack_binario(
  raster_path = rr,
  pasta_saida = here::here(info$covariaveis),
  nome_saida = "solos_bin.tif"
)

rrb <- rast(rb)
terra::plot(rrb,
  plg = list(cex = 0.9)
)
