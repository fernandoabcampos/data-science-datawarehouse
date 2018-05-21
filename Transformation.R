library(XML)
labels <- c("Comarca", "Estaciones", "Altitud", "Media anual", "Media de máximas", "Media de mínimas",
            "Máxima absoluta", "Mínima absoluta")
url <- "file:///Users/fbarbeiro/Desktop/Master%20-%20Data%20Science/UOC/Datawarehouse/PRACs/20180522-PRAC2/PRA2-Datos/aec-214_2012.html"

table = readHTMLTable(url, which = 1, skip.rows = 1) # leyendo la primera tabla
head(table)
str(table)
colnames(table)

#Identificada columna sin datos V9, borramos
table <- table[ , -which(names(table) %in% c("V9"))]
colnames(table)
colnames(table) <- labels
table
