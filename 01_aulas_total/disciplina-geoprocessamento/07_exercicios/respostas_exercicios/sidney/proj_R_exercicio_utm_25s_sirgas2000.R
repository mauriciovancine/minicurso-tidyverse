setwd("E:\\Documentos\\Mestrado\\MestDisci\\Geoprocessamento\\disciplina-geoprocessamento\\03_dados\\01_vetor\\Recifeprocessamento")
dir()
library(sf)
library(st)
library(raster)
library(fasterize)
# Exercício 47.1 ------------------------------------------------------------

rec_uso <- sf::st_read("PE_2611606_USO_topologia.shp")
plot(rec_uso[1])

# Exercício 47.2------------------------------------------------------------
library(rgdal)
library(dplyr)
floresta <- rec_uso[4,]
setwd("E:\\Documentos\\Mestrado\\MestDisci\\Geoprocessamento\\disciplina-geoprocessamento\\07_exercicios\\Exercício_R")
sf::st_write(floresta, "PE_2611606_USO_topologia_floresta.shp")
# Exercício 47.3 ------------------------------------------------------------

flor <- sf::st_cast(floresta, "POLYGON")
flor_tab <- sf::st_drop_geometry(flor)
sf::st_write(flor, "PE_2611606_USO_topologia_floresta_unico.shp")
# Exercício 47.4 ------------------------------------------------------------
nrow(flor)
x <- seq(nrow(flor))
flor$id <- x

# Exercício 47.5 ------------------------------------------------------------
flor_id <- flor[,"id"]
plot(flor_id)
# Exercício 47.6 ------------------------------------------------------------
flor_id$a_frag_ha <- sf::st_area(flor_id)/10000

# Exercício 47.7 ------------------------------------------------------------
flor_buff <- flor_id %>% sf::st_buffer(-60)
sf::st_write(flor_buff, "PE_2611606_USO_topologia_floresta_unico_core60m.shp")
flor_buff <- flor_buff[,"id"] 
plot(flor_buff)
flor_buffah$a_core_ha <- sf::st_area(flor_buffah)/10000

# Exercício 47.8 ------------------------------------------------------------

flor_borda <- sf::st_difference(flor_id, sf::st_combine(flor_buff))
flor_borda <- flor_borda[,"id"]
plot(flor_borda)
flor_borda$a_ed60m_ha <- sf::st_area(flor_borda)/10000
sf::st_write(flor_borda, "PE_2611606_USO_topologia_floresta_unico_edge60m.shp")

# Exercício 47.9 ------------------------------------------------------------
setwd("E:\\Documentos\\Mestrado\\MestDisci\\Geoprocessamento\\disciplina-geoprocessamento\\03_dados\\02_raster\\juntar")
bio <- raster::raster("dem_mosaico_recife.tif")
ras_florid <- fasterize(flor_id, bio)
raster::plot(bio, legend = F)
par(new = T)
raster::plot(ras_florid, col = "red", legend = F)

setwd("E:\\Documentos\\Mestrado\\MestDisci\\Geoprocessamento\\disciplina-geoprocessamento\\07_exercicios\\Exercício_R")
raster::writeRaster(ras_florid, "PE_2611606_USO_topologia_floresta_unico.tif")

flor_buff <- sf::st_cast(flor_buffah, "POLYGON")
ras_flobuff <- fasterize(flor_buff, bio)
fasterize::plot(ras_flobuff)
raster::plot(bio, legend = F)
par(new = T)
raster::plot(ras_flobuff, col = "red", legend = F)
raster::writeRaster(ras_flobuff, "PE_2611606_USO_topologia_floresta_unico_core60m.tif")

flor_borda <- sf::st_cast(flor_borda, "POLYGON")
ras_flobord <- fasterize(flor_borda, bio)
raster::plot(bio, legend = F)
par(new = T)
raster::plot(ras_flobord, col = "red", legend = F)
raster::writeRaster(ras_flobuff, "PE_2611606_USO_topologia_floresta_unico_edge60m.tif")


# Exercício 48 ------------------------------------------------------------
fs <- list.files(path="E:\\Documentos\\Mestrado\\MestDisci\\Geoprocessamento\\disciplina-geoprocessamento\\07_exercicios\\Exercício_R", pattern = "tif$", full.names = TRUE)
s <- raster::stack(fs)
