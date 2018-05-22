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
  table[numerics] <- lapply(table[numerics], function(x) suppressWarnings(as.numeric(as.character(as.numeric(gsub(",", ".", gsub("\\.", "", x)))))))
  return(table)
}

extract_table_from_html <- function(file.name, labels, N, R, eliminate_empty_column, numerics) {
  table = readHTMLTable(paste0("PRA2-Data/", file.name), which = N, skip.rows = R)
  
  if(eliminate_empty_column && !('Provincia' %in% labels)) {
    table <- table[,!apply(table, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))]
  }
  
  table$V2 <- gsub(" \\(2\\)", "", table$V2) # misma estación, replacing (2)
  colnames(table) <- labels
  
  if ('Provincia' %in% labels) {
    table$Codigo <- as.character(table$Codigo)
    provincia <- c('VI', 'AB', 'A', 'AL', 'AV', 'BA', 'IB', 'B', 'BU', 'CC', 'CA', 'CS', 'CR', 'CO', 'C', 'CU', 'GI', 'GR', 'GU', 'SS', 'H', 'HU', 'J', 'LE', 'L', 'LO', 'LU', 'M', 'MA', 'MU', 'NA', 'OU', 'O', 'P', 'GC', 'PO', 'SA', 'TF', 'S', 'SG', 'SE', 'SO', 'T', 'TE', 'TO', 'V', 'VA', 'BI', 'ZA', 'Z', 'CE', 'ML')
    table$Provincia <- provincia[substr(table$Codigo, 1, 2)]
  }
  
  table <- convert_to_numeric(table, numerics)
  return(table)
}

eliminate_colon <- function(table) {
  table <- table[complete.cases(table), ]
  indices <- suppressWarnings(data.frame(ind = which(table == ":", arr.ind = TRUE))) # He detectado mirando los ficheros que habian datos incompletos
  if (nrow(indices) > 0) {
    table <- table[-unique(indices$ind.row), ]
  }
  return(table)
}

generate_xls <- function(source, labels.table1, labels.table2, numerics.table1, numerics.table2, skip.table1, skip.table2, merge.by) {
  
  eliminate_empty <- TRUE
  table1 <- extract_table_from_html(source, labels.table1, 1, skip.table1, eliminate_empty, numerics.table1)
  
  if (any(!is.na(labels.table2))) {
    table2 <- extract_table_from_html(source, labels.table2, 2, skip.table2, eliminate_empty, numerics.table2) 
    
    table_final <- merge(table1, table2, by = intersect(names(table1), names(table2)),
          by.x = merge.by, by.y = merge.by, all = TRUE)
    
    table_final <- eliminate_colon(table_final)
    x <- list(sheet_a = table_final)
  } else {
    table1 <- eliminate_colon(table1)
    x <- list(sheet_a = table1)
  }
  
  WriteXLS(x, ExcelFileName = paste0(source, "_tabla.xls"))
}

########## ------------- Ficheros aec-214 -------------------------------------------------------------------------------------------------------------
labels_aec_214_1 <- c("Comarca", "Estaciones", "Altitud (m)", "Media anual", "Media de máximas", "Media de mínimas","Máxima absoluta", "Mínima absoluta")
labels_aec_214_2 <- c("Comarca", "Estaciones", "Altitud (m)", "Precipitación anual", "Humedad relativa", "Velocidad media", "Dirección dominante")
numerics_aec_214_1 <- labels_aec_214_1[3:8]
numerics_aec_214_2 <- labels_aec_214_2[3:6]
merge_aec_214 <- labels_aec_214_1[1:3]

generate_xls("aec-214_2009.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2, merge_aec_214)
generate_xls("aec-214_2010.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2, merge_aec_214)
generate_xls("aec-214_2011.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2, merge_aec_214)
generate_xls("aec-214_2012.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2, merge_aec_214)
generate_xls("aec-214_2013.html", labels_aec_214_1, labels_aec_214_2, numerics_aec_214_1, numerics_aec_214_2, 1, 1:2, merge_aec_214)



########## ------------- Ficheros aec-217 -------------------------------------------------------------------------------------------------------------
labels_aec_217_1 <- c("Comarca", "Estaciones", "Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio")
labels_aec_217_2 <- c("Comarca", "Estaciones", "Julio", "Agosto", "Septiembre", "Octubre","Noviembre", "Diciembre", "Total")
numerics_aec_217_1 <- labels_aec_217_1[3:8]
numerics_aec_217_2 <- labels_aec_217_2[3:9]
merge_aec_217 <- labels_aec_217_1[1:2]

generate_xls("aec-217_2009.html", labels_aec_217_1, labels_aec_217_2, numerics_aec_217_1, numerics_aec_217_2, 1, 1, merge_aec_217)
generate_xls("aec-217_2010.html", labels_aec_217_1, labels_aec_217_2, numerics_aec_217_1, numerics_aec_217_2, 1, 1, merge_aec_217)
generate_xls("aec-217_2011.html", labels_aec_217_1, labels_aec_217_2, numerics_aec_217_1, numerics_aec_217_2, 1, 1, merge_aec_217)
generate_xls("aec-217_2012.html", labels_aec_217_1, labels_aec_217_2, numerics_aec_217_1, numerics_aec_217_2, 1, 1, merge_aec_217)
generate_xls("aec-217_2013.html", labels_aec_217_1, labels_aec_217_2, numerics_aec_217_1, numerics_aec_217_2, 1, 1, merge_aec_217)

########## ------------- Ficheros aec-925 -------------------------------------------------------------------------------------------------------------

labels_aec_925_1 <- c("Municipio", "Comarca", "Codigo", "Altitud (m)", "Superficie (km2)", "Población", "Provincia")
numerics_aec_925_1 <- labels_aec_925_1[5:7]

generate_xls("aec-925_2009.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA, NA)
generate_xls("aec-925_2010.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA, NA)
generate_xls("aec-925_2011.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA, NA)
generate_xls("aec-925_2012.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA, NA)
generate_xls("aec-925_2013.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA, NA)
generate_xls("aec-925_2014.html", labels_aec_925_1, NA, numerics_aec_925_1, NA, 1, NA, NA)

## ---- Structure to prove transformations - ---------------------------------------------------------------------------------

table.aux2 = readHTMLTable(paste0("PRA2-Data/aec-925_2009.html"), which = 1, skip.rows = 1)
str(table.aux2)


