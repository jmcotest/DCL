rm(list=ls(all=TRUE))
library(writexl)
#Generando estructuras de carpetas
# Eliminar la carpeta salidas y todo su contenido
if (dir.exists(file.path(".", "salidas"))) {
  unlink(file.path(".", "salidas"), recursive = TRUE)
}

# Crear nuevamente las carpetas
for (carpeta in c("graficos", "excel", "pdf", "html")) {
  dir.create(
    file.path(".", "salidas", carpeta),
    recursive = TRUE,
    showWarnings = FALSE
  )
}
dir.create(file.path(".","script"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","rmd"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","qmd"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","BD"),showWarnings = FALSE, recursive=TRUE)

#Datos 
datos <- data.frame(
  cuadrado = factor(rep(c("1", "1", "1", "2", "2", "2"), times = 3)),
  novilla = factor(rep(1:6, times = 3)),
  semana = factor(rep(1:3, each = 6)),
  tratamiento = factor(c(
    "1", "2", "3", "1", "2", "3",
    "2", "3", "1", "3", "1", "2",
    "3", "1", "2", "2", "3", "1"
  ), labels = c("Forraje_Alfalfa", "Maiz_Ensilado", "Pastillas_Pasto_Azul")),
  consumo = c(
    2.7, 2.6, 1.9, 3.3, 2.3, 0.1,
    2.1, 0.2, 2.3, 1.7, 2.8, 1.8,
    1.9, 2.1, 2.4, 2.1, 1.7, 2.7
  )
)

datosc1<-datos[datos$cuadrado=="1",]


