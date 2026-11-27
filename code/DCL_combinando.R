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

#####################################
# 1. ANÁLISIS GRÁFICO DE LOS DATOS  #
#####################################
source(file.path(".","code","funciones.R"))

#Estadistica descriptiva
descriptiva<-aggregate(consumo~tratamiento,data=datos,resumen)
# Operaciona previas a la gráfica descriptiva
descriptiva<-data.frame(tratamiento=descriptiva$tratamiento,descriptiva$consumo)

# Uso de la funcion graficos
graficos_personalizado(
  data_g = descriptiva,formula_g=tratamiento~Media+Mediana+Inferior+Superior,
  datos=datos,formula_m=consumo~tratamiento,
  ordenar="Media", typeplot="lineas-violines", prefixjpg="inicial_combinado",
  xlab="Raciones de alimentación", 
  ylab="Consumo de materia seca (kg/día)")
  #Para el de lineas
  #Fig 1. Gráfico descriptivo para el contenido de nitróneno en 6 tratamientos de cepas de Rhizoboum.
  #.      La barras indican la media del tratamiento la cual también está resaltada en negro. La Mediana
  #       se resalta en color naranja. Las observaciones originales están en color azul y las barras 
  #.      de error indican los límites del intervalo de confianza de 95%.

#####################################
# 2. MODELACIÓN ESTADÍSTICA         #
#####################################
modelo<-formula(consumo~tratamiento+semana+novilla)

library(MCMCglmm)

# m0<-MCMCglmm(consumo~tratamiento, random=~cuadrado+novilla:cuadrado+semana+semana:tratamiento, 
#     data = datos, nitt=1e4L,verbose=FALSE)

m0<-MCMCglmm(consumo~tratamiento, random=~cuadrado+novilla:cuadrado+semana:cuadrado, 
    data = datos, nitt=1e6L,verbose=FALSE)

k <- nlevels(datos$tratamiento)

prior1 <- list(
  R = list(
    V = diag(k),
    nu = k + 2
  ),
  G = list(
    G1 = list(
      V = 1, nu = 1,
      alpha.mu = 0, alpha.V = 1000
    ),
    G2 = list(
      V = 1, nu = 1,
      alpha.mu = 0, alpha.V = 1000
    ),
    G3 = list(
      V = 1, nu = 1,
      alpha.mu = 0, alpha.V = 1000
    )
  )
)

m1<-MCMCglmm(consumo~tratamiento, random=~cuadrado+novilla:cuadrado+semana:cuadrado, 
    rcov=~idh(tratamiento):units, prior=prior1,
    data = datos, nitt=1e6L,verbose=FALSE)


m0$DIC
m1$DIC
m0$DIC-m1$DIC

#' 
#' Interpretación:
#' 
#' - Menor DIC: mejor ajuste considerando complejidad.
#' - $\Delta$DIC < 2: evidencia débil a favor de m1.
#' - $\Delta$DIC entre 3 y 7: evidencia moderada a favor de m1.
#' - $\Delta$DIC > 10: evidencia fuerte a favor de m1.

m<-m1

## Cálculo de los predichos obtenidos Bayes - Bayesian Linear Prediction
# Predicciones de todas las iteraciones MCMC
tmp<- m[["X"]] %*% t(m[["Sol"]])
grupo <- datos[["tratamiento"]]
indices <- split(seq_len(nrow(tmp)), grupo)

MCMC_trt <- lapply(indices, function(i) colMeans(tmp[i, , drop = FALSE]))

#Resumen de distribucion *a posteriori*
resumenMCMC<-lapply(MCMC_trt,function(x) c(mean(x),HPDinterval(as.mcmc(x),prob=0.90)))
resumenMCMC<-do.call(rbind,resumenMCMC)
colnames(resumenMCMC)<-c("Est Bayes", "lower.HPD", "upper.HPD")

# Analisis de la distribución *a posteriori* para cada tratamiento: 
# Probabilidad de ser el mejor y el peor tratamiento
# Esto es mejor que las pruebas de comparación multiples en cuanto a una toma 
# de decisiones más clara.
# El autor quien realizó la prouesta es "Julian Besag, David Higdon, 
# Bayesian Analysis of Agricultural Field Experiments, 
# Journal of the Royal Statistical Society Series B: Statistical Methodology, 
# Volume 61, Issue 4, November 1999, Pages 691–746, 
# https://doi.org/10.1111/1467-9868.00201"

MatrixMCMC<-as.matrix(do.call(cbind,MCMC_trt))
rank_MatrixMCMC<-apply(MatrixMCMC,1L,rank)
pbest<-rowSums(rank_MatrixMCMC==nlevels(grupo))/ncol(rank_MatrixMCMC)
pworst<-rowSums(rank_MatrixMCMC==1)/ncol(rank_MatrixMCMC)

pbest_c<-format(round(pbest,4L), nsmall=4L) 
pworst_c<-format(round(pworst,4L), nsmall=4L)

#Nuevo probabilidad de superar un valor de referencia
p_ref<-sapply(MCMC_trt,function(x) sum(x > 2L) / length(x))
p_ref_c<-sapply(p_ref,function(x) format(round(x,4L), nsmall=4L))

#Nuevo probabilidad de superar el testigo Compuesto
#p_check<-sapply(MCMC_trt,function(x) sum(x > MCMC_trt[["Compuesto"]]) / length(x))
#p_check_c<-sapply(p_check,function(x) format(round(x,4L), nsmall=4L))
#p_check_c["A"]<-""
#Juntando complementos
complementos<-data.frame(tratamiento=rownames(resumenMCMC), resumenMCMC, pbest=pbest_c, pworst=pworst_c, pref=p_ref_c)

#####################################
# 3. COMPLEMENTAR Y AJUSTAR GRÁFICO #
#####################################
analisis_post_modelo <- merge(descriptiva,complementos, by.x = "tratamiento",
 by.y = "tratamiento", all.x = TRUE)

analisis_post_modelo[["E Bayes"]] <- analisis_post_modelo[["Est.Bayes"]] 

graficos_personalizado(
  data_g = analisis_post_modelo,formula_g=tratamiento~`E Bayes`+Mediana+lower.HPD+upper.HPD,
  datos=datos,formula_m=consumo~tratamiento,
  typeplot="lineas-violines",
  ordenar="E Bayes",
  prefixjpg="moderno_heterogeneo_combinado", 
  xlab="Fungicidas para semillas", 
  ylab="Plántulas emergidas por 25 semillas",
  add = function(g) {
    with(g, {
      text(x=bp, y=lower.HPD, labels=pworst, col = "#000000", cex=0.6,offset=0.2,pos=1L)
      text(x=bp, y=upper.HPD, labels=pbest, col = "#000000", cex=0.6,offset=0.2,pos=3L)
      #abline(h=15,lty=2,lwd=2,col = "#8B0000")
      #text(x=bp, y=15, labels=pref, col = "#8B0000", cex=0.6,adj=0,offset=0.2,pos=3)
      #text(x=bp, y=`E Bayes`, labels=pcheck, col = "#E65100", cex=0.6,adj=0,offset=0.4,pos=2)
    })
  }
)

# 
# Gráfica resumen más completo basado en el resumen de la distribución *a posteriori*
# Fig n. Número de plantas emergidas en diferentes tratamiento de control de Diplodia spp._
#        Las barras de error indican los límites del intervalo de mas alta densidad a posterior de 90%. En la
#        parte superior de límite se indica la probabilidad de que ese tratamiento sea el mejor mientras en el
#        límite inferior se indica la probabilidad de ser el peor. En rojo se indica la probabilidad de que ese
#        tratamiento supere emergencia de 15 plantas. Al lado izquiero de la media se indica la probabilidad de
#        superar el testigo A. La línea punteada indica el valor de referencia de 15 plantas.

###########################################################
# 4. Guardando resultados en archivos de salida           #
###########################################################
write_xlsx(list(modelo=analisis_post_modelo), 
path = file.path(".","salidas","excel","modelo_moderno.xlsx"))

saveRDS(list(modelo=m, modelo=analisis_post_modelo), 
        file = file.path(".","salidas","modelo_moderno.rds"),compress = "xz")
