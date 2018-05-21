library(XML)

extract_table_from_html <- function(file.name, labels, N, R) {
  table <- readHTMLTable(paste("PRA2-Data/", file.name), which = N, skip.rows = R)
  head(table)
  colnames(table)
  table <- table[ , -which(names(table) %in% c("V9"))]
  colnames(table)
  colnames(table) <- labels
  table
  return(table)
}

file.name <- "aec-214_2012.html"
labels <- c("Comarca", "Estaciones", "Altitud", "Media anual", "Media de máximas", "Media de mínimas",
            "Máxima absoluta", "Mínima absoluta")

table1 <- extract_table_from_html(file.name, labels, 1, 1) #leyendo 1ª tabla del html y saltando 1 linea
head(table1)

