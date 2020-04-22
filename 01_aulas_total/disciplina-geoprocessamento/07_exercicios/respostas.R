# packages
library(sf)
library(fasterize)
library(raster)
library(tmap)
library(tidyverse)

# 37
uso <- sf::read_sf("/home/mude/data/github/disciplina-geoprocessamento/03_dados/01_vetor/PE_2611606_USO_topologia/PE_2611606_USO_topologia.shp")
uso

tm_shape(uso) +
  tm_polygons()

# 38
uso_flo <- uso %>% 
  dplyr::filter(CLASSE_USO == "formação florestal")
uso_flo

tm_shape(uso_flo) +
  tm_polygons()

sf::write_sf(uso_flo, 
             "/home/mude/data/github/disciplina-geoprocessamento/03_dados/01_vetor/PE_2611606_USO_topologia/PE_2611606_USO_topologia_floresta.shp")
# 39-40
uso_flo_unique <- uso_flo %>% 
  sf::st_cast("POLYGON")
uso_flo_unique

sf::write_sf(uso_flo_unique,
             "/home/mude/data/github/disciplina-geoprocessamento/03_dados/01_vetor/PE_2611606_USO_topologia/PE_2611606_USO_topologia_floresta_unico.shp")

# 41
uso_flo_unique_id <- uso_flo_unique %>% 
  dplyr::mutate(id = uso_flo_unique %>% nrow %>% seq)
uso_flo_unique_id

# 42
uso_flo_unique_id <- uso_flo_unique_id %>% 
  dplyr::select(id)
uso_flo_unique_id

# 43 
uso_flo_unique_id_area <- uso_flo_unique_id %>% 
  dplyr::mutate(a_frag_ha = sf::st_area(uso_flo_unique_id)/10000)
uso_flo_unique_id_area

# 44 
uso_flo_unique_id_core <- uso_flo_unique_id %>% 
  sf::st_buffer(-60) %>% 
  dplyr::mutate(a_core_ha = sf::st_area(.)/10000)
uso_flo_unique_id_core

tm_shape(uso_flo_unique_id_core) +
  tm_polygons()

sf::write_sf(uso_flo_unique_id_core, 
             "/home/mude/data/github/disciplina-geoprocessamento/03_dados/01_vetor/PE_2611606_USO_topologia/PE_2611606_USO_topologia_floresta_unico_core60m.shp")

# 45
uso_flo_unique_id_edge <- uso_flo_unique_id %>% 
  sf::st_difference(sf::st_combine(uso_flo_unique_id_core)) %>% 
  dplyr::mutate(a_edge_ha = sf::st_area(.)/10000)
uso_flo_unique_id_edge

tm_shape(uso_flo_unique_id_edge) +
  tm_polygons()

sf::write_sf(uso_flo_unique_id_edge, 
             "/home/mude/data/github/disciplina-geoprocessamento/03_dados/01_vetor/PE_2611606_USO_topologia/PE_2611606_USO_topologia_floresta_unico_edge60m.shp")

# 46
dem <- raster::raster("/home/mude/data/github/disciplina-geoprocessamento/03_dados/02_raster/dem_mosaico_recife_e_o_mundo2.tif")
dem
plot(dem)

uso_flo_unique_id_area_raster <- fasterize::fasterize(sf = uso_flo_unique_id_area,
                                                      raster = dem,
                                                      field = "a_frag_ha")
plot(uso_flo_unique_id_area_raster)

uso_flo_unique_id_core_raster <- fasterize::fasterize(sf = uso_flo_unique_id_core %>%
                                                        dplyr::filter(!st_is_empty(.)) %>% 
                                                        sf::st_cast("MULTIPOLYGON"),
                                                      raster = dem,
                                                      field = "a_core_ha")
uso_flo_unique_id_core_raster
plot(uso_flo_unique_id_core_raster)

uso_flo_unique_id_edge_raster <- fasterize::fasterize(sf = uso_flo_unique_id_edge %>%
                                                        dplyr::filter(!st_is_empty(.)) %>% 
                                                        sf::st_cast("MULTIPOLYGON"),
                                                      raster = dem,
                                                      field = "a_edge_ha")
uso_flo_unique_id_edge_raster
plot(uso_flo_unique_id_edge_raster)

# export
raster::writeRaster(x = uso_flo_unique_id_area_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

raster::writeRaster(x = uso_flo_unique_id_core_raster_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_core60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

raster::writeRaster(x = uso_flo_unique_id_edge_raster, 
                    filename = "PE_2611606_USO_topologia_floresta_unico_edge60m",
                    format = "GTiff",
                    options= "COMPRESS=DEFLATE")

# 47
# import
st <- raster::stack(dem, uso_flo_unique_id_area_raster, uso_flo_unique_id_core_raster, uso_flo_unique_id_edge_raster)
st

names(st) <- c("dem", "flo", "core", "edge")
plot(st)

# values
st_va <- st %>% 
  raster::values() %>% 
  tibble::as_tibble()
st_va

# floresta
ggplot(data = st_va) +
  aes(x = dem, y = log10(flo + 1)) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de floresta (log10)") +
  theme_bw()

# core
ggplot(data = st_va) +
  aes(x = dem, y = log10(core + 1)) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de floresta core (log10)") +
  theme_bw()

# edge
ggplot(data = st_va) +
  aes(x = dem, y = log10(edge + 1)) +
  geom_point() +
  labs(x = "Elevação (m)", y = "Área de borda de floresta (log10)") +
  theme_bw()


# end ---------------------------------------------------------------------
