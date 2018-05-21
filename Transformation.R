library(XML)
library(WriteXLS)

table.aux = readHTMLTable(paste0("PRA2-Data/aec-214_2012.html"), which = 1, skip.rows = 1)
table.aux2 = readHTMLTable(paste0("PRA2-Data/aec-214_2012.html"), which = 2, skip.rows = 1:2)


convert_to_numeric <- function(table, numerics) {
  table[numerics] <- lapply(table[numerics], function(x) as.numeric(as.character(as.numeric(gsub(",", ".", gsub("\\.", "", x))))))
  return(table)
}
extract_table_from_html <- function(file.name, labels, N, R, eliminate_empty_column, numerics) {
  table = readHTMLTable(paste0("PRA2-Data/", file.name), which = N, skip.rows = R)
  head(table)
  colnames(table)
  if(eliminate_empty_column) {
    table <- table[,!apply(table, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))]
  }
  colnames(table)
  colnames(table) <- labels
  table <- convert_to_numeric(table, numerics)
  return(table)
}

file.name <- "aec-214_2012.html"
eliminate_empty <- TRUE

labels <- c("Comarca", "Estaciones", "Altitud", "Media anual", "Media de máximas", "Media de mínimas","Máxima absoluta", "Mínima absoluta")
numerics <- labels[3:8]
table1 <- extract_table_from_html(file.name, labels, 1, 1, eliminate_empty, numerics)

labels <- c("Comarca", "Estaciones", "Altitud", "Precipitación anual", "Humedad relativa", "Velocidad media", "Dirección dominante")
numerics <- labels[3:6]
table2 <- extract_table_from_html(file.name, labels, 2, 1:2, eliminate_empty, numerics) 

head(table1)
str(table1)
head(table2)
str(table2)

x <- list(sheet_a = table1, sheet_b = table2)
WriteXLS(x, ExcelFileName = paste0(file.name, "_tabla_", 1, ".xls"))
