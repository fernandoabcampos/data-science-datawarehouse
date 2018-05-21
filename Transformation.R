library(XML)
library(WriteXLS)

#table.aux2 = readHTMLTable(paste0("PRA2-Data/aec-214_2013.html"), which = 1, skip.rows = 1)
#table.aux2 <- table.aux2[,!apply(table.aux2, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))]
#colnames(table.aux2) <- labels1
#table.aux2 <- table.aux2[complete.cases(table.aux2), ]
#indices <- suppressWarnings(data.frame(ind = which(table.aux2 == ":", arr.ind = TRUE))) # He detectado mirando los ficheros que habian datos incompletos
#if (nrow(indices) > 0) {
#  table.aux2 <- table.aux2[-unique(indices$ind.row), ]
#}

#table.aux2 <- convert_to_numeric(table.aux2, numerics1)
#return(table)

convert_to_numeric <- function(table, numerics) {
  table[numerics] <- lapply(table[numerics], function(x) as.numeric(as.character(as.numeric(gsub(",", ".", gsub("\\.", "", x))))))
  return(table)
}

extract_table_from_html <- function(file.name, labels, N, R, eliminate_empty_column, numerics) {
  table = readHTMLTable(paste0("PRA2-Data/", file.name), which = N, skip.rows = R)
  if(eliminate_empty_column) {
    table <- table[,!apply(table, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))]
  }
  colnames(table) <- labels
  table <- table[complete.cases(table), ]
  
  
  indices <- suppressWarnings(data.frame(ind = which(table == ":", arr.ind = TRUE))) # He detectado mirando los ficheros que habian datos incompletos
  if (nrow(indices) > 0) {
    table <- table[-unique(indices$ind.row), ]
  }
  
  table <- convert_to_numeric(table, numerics)
  return(table)
}

generate_xls <- function(source, labels.table1, lables.table2, numerics.table1, numerics.table2, skip.table1, skip.table2) {
  
  eliminate_empty <- TRUE
  table1 <- extract_table_from_html(source, labels.table1, 1, skip.table1, eliminate_empty, numerics.table1)
  
  if (!is.na(lables.table2)) {
    table2 <- extract_table_from_html(source, lables.table2, 2, skip.table2, eliminate_empty, numerics.table2) 
    x <- list(sheet_a = table1, sheet_b = table2)
  } else {
    x <- list(sheet_a = table1)
  }
  WriteXLS(x, ExcelFileName = paste0(source, "_tabla.xls"))
}

########## ------------- Ficheros aec-214 -------------------------------------------------------------------------------------------------------------
labels_aec_214_1 <- c("Comarca", "Estaciones", "Altitud", "Media anual", "Media de máximas", "Media de mínimas","Máxima absoluta", "Mínima absoluta")
labels_aec_214_2 <- c("Comarca", "Estaciones", "Altitud", "Precipitación anual", "Humedad relativa", "Velocidad media", "Dirección dominante")
numerics_aec_214_1 <- labels_aec_214_1[3:8]
numerics_aec_214_2 <- labels_aec_214_2[3:6]

generate_xls("aec-214_2009.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2)
generate_xls("aec-214_2010.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2)
generate_xls("aec-214_2011.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2)
generate_xls("aec-214_2012.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2)
generate_xls("aec-214_2013.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2)



########## ------------- Ficheros aec-925 -------------------------------------------------------------------------------------------------------------

labels_aec_925_1 <- c("Municipio", "Comarca", "Código", "Altitud (m)", "Superficie (km2)", "Población")
numerics_aec_925_1 <- labels_aec_925_1[4:6]

generate_xls("aec-925_2009.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA)
generate_xls("aec-925_2010.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA)
generate_xls("aec-925_2011.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA)
generate_xls("aec-925_2012.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA)
generate_xls("aec-925_2013.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA)
generate_xls("aec-925_2014.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA)


