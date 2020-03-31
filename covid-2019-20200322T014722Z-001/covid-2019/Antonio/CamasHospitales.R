setwd("~/Desktop/covid-2019-20200322T014722Z-001/covid-2019/Antonio/")
dir()
localidades <- read.csv('hospital_localida_urbana_clues.csv',
                        stringsAsFactors = FALSE)
sapply(localidades, class)
catalogo <- read.csv('CatalogoLocalidadEntidadINEGI.csv',
                     stringsAsFactors = FALSE)
sapply(catalogo, class)
data <- merge(localidades, catalogo, all.x = FALSE,
              by.x = 'localidad_urbana_cvegeo',
              by.y = 'Mapa')
names(data)
data.sub <- data[, c('localidad_urbana_cvegeo', "total_camas",
                     "Nom_Ent" , "Nom_Mun", "Nom_Loc" )]
write.csv(data.sub, 'Camas_localidad.csv', row.names = FALSE)
rm(list=ls())
################# municipios
dir()
municipios <- read.csv('Camas_munipios.csv',
                       stringsAsFactors = FALSE)
sapply(municipios, class)
nchar(municipios$municipio_cvegeo)
longitudes <- sapply(municipios$municipio_cvegeo, function(x) nchar(as.character(x)))
municipios$clave <- as.character(municipios$municipio_cvegeo)
for (i in 1:dim(municipios)[1])
{
    if( longitudes[i] == 4)
    {
        municipios$clave[i] <-  paste0('0',
                                       as.character(municipios$municipio_cvegeo[i]))

    }

}
######################################
catalogo <- read.csv('CatalogoLocalidadEntidadINEGI.csv',
                     stringsAsFactors = FALSE)
sapply(catalogo, class)
catalogo$Cve_Ent <- as.character(catalogo$Cve_Ent)
index <- which( sapply(catalogo$Cve_Ent, nchar) == 1 )
catalogo$cve_ent <- catalogo$Cve_Ent
catalogo$cve_ent[index] <- paste0( '0',catalogo$Cve_Ent[index] )

catalogo$Cve_Mun <- as.character(catalogo$Cve_Mun)
index <- which( sapply(catalogo$Cve_Mun, nchar) == 1 )
catalogo$cve_mun <- catalogo$Cve_Mun
catalogo$cve_mun[index] <- paste0( '00',catalogo$Cve_Mun[index] )
index <- which( sapply(catalogo$Cve_Mun, nchar) == 2 )
catalogo$cve_mun[index] <- paste0( '0',catalogo$Cve_Mun[index] )

# clave
catalogo$Mapa <- NULL
catalogo$Mapa <- paste0( catalogo$cve_ent, catalogo$cve_mun)
names(catalogo)
catalogo <- catalogo[, c("Nom_Ent", "Nom_Mun",  "Mapa"   )]
catalogo <- unique(catalogo)

data <- merge(municipios, catalogo, all.x = FALSE,
              by.x = 'clave',
              by.y = 'Mapa')
names(data)
data$municipio_cvegeo <- NULL
data$nombre <- NULL
names(data) <- c('municipio_cvegeo',  "total_camas",
                 "Nom_Ent",     "Nom_Mun" )
write.csv(data, 'Camas_munipios_count.csv', row.names = FALSE)
