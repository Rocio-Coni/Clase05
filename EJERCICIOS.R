#EJERCICIO: PARTE1-1j
m <- (0.3*0.15)/(0.3*0.15+0.2*0.8+0.5*0.12)
n <- ((5^6)/factorial(6))*exp(-5)

choose(20,7)*(0.4^7)*(0.6^13)
#EJERCICIO: PARTE1-2
sum(1:1000)
sum(2^(0:10))

#EJERCICIO: PARTE1-3
data <- load("datos.RData")
length(grupo)
which(grupo == "A")

#EJERCICIO: PARTE1-4
nota
sum(nota)
mean(nota)
which(nota >= 7.0)
nota[order(nota, decreasing = T)]
sort(nota, decreasing = T)
which(nota == max(nota))

#EJERCICIO: PARTE1-5
sum(nota[1:10])
length(grupo[grupo=="C"])
length(nota[nota>=5])
length(nota[grupo == "B" & nota >= 5])
aprob_C <- (length(nota[grupo == "C" & nota >= 5])/length(nota[grupo == "C"]))*100
maxgr <- grupo[nota == max(nota)]
mingr <- grupo[nota == min(nota)]
## Nota media de alumnos de los grupos ‘A’ y ‘B’, 
##reunidos, teniendo en cuenta sólo a los que han aprobado

not_a_aprob <- nota[grupo == "A" & nota >= 5]
not_b_aprob <- nota[grupo == "B" & nota >= 5]
mean(c(nota[grupo == "A" & nota >= 5],nota[grupo == "B" & nota >= 5]))
sum_not_ab <- suma_a_b_aprob <- sum(not_a_aprob, not_b_aprob)
cant_a_aprob <- length(grupo[grupo == "A" & nota >= 5])
cant_b_aprob <- length(grupo[grupo == "B" & nota >= 5])
sum_cant_ab <- sum(cant_a_aprob, cant_b_aprob)
ma_a_b_aprob <- sum_not_ab/sum_cant_ab

#EJERCICIO: PARTE1-6
quantile(nota, probs = 0.66)
quantile(nota[grupo=="C"], probs = 0.66)

#EJERCICIO: PARTE1-7
a1 <- length(nota[nota <= 4.9])
b1 <- length(nota)
porcent_men <- a1/b1*100
((length(nota[nota <= 4.9]))/(length(nota)))*100
a2 <- length(nota[nota >= 4.9])
porcent_may <- a2/b1*100
((length(nota[nota >= 4.9]))/(length(nota)))*100
#EJERCICIO: PARTE1-8
par(mfrow = c(2,3))
boxplot(nota[grupo == "A"], main = "Grupo")
boxplot(nota[grupo == "B"])
boxplot(nota[grupo == "C"])
boxplot(nota[grupo == "D"])
boxplot(nota[grupo == "E"])
dev.off()

#EJERCICIO: PARTE1-9
conc
max(conc)
#¿En cuántos de los muestreos se ha superado la concentración de 40.0 ppm?
length(conc[conc > 40])
mean(conc)

#¿Cuáles fueron las 10 mediciones más bajas del día?
mediciones_bajas <- sort(conc)
mediciones_bajas[1:10]
sort(conc)[1:10]
#Si la primera medida fue a las 00:00. 
#¿A qué hora del día se alcanzó la concentración máxima?
which(conc == max(conc))
date1 <- seq(
  as.POSIXct("2021-01-01 00:00"),
  length.out = length(conc),
  by = "5 min"
)
length(conc)
df03 <- data.frame(conc, date1)
df03[conc == max(conc),]
max(conc)

#EJERCICIO: PARTE2
x1 <- 1:10
y1 <- x1^2
matrix1 <- matrix(c(1:10,(1:10)^2), nrow = 10)
plot(matrix(c(1:10,(1:10)^2), nrow = 10))
plot(matrix(c(1:10,(1:10)^2), nrow = 10), xlab = "X", ylab = "Y",
     main = "Ejercicio 1 - Parte 2")
A1 <- matrix(c(1,2,3,2,4,6,3,6,9,4,8,12), nrow = 4, byrow = T)
I1 <- diag(x = 1,nrow = 3)

#Crea una función que cree una matriz nula ingresando las dimensiones
matrix_null <- function(val1,val2){
  resultado <- matrix(0, nrow = val1, ncol = val2)
  return(resultado)
}
matrix_null(4,5)

B1 <- diag(x = c(0,2,3,4),nrow = 4)
B1%*%A1
t(A1)

A1+B1
sum(A1,B1)
B2 <- 3*B1

A3 <- rbind(c(3, -1, 1), 
           c(9, -2, 1), 
           c(3, 1, -2))
B3 <- c(-1, -9, -9)
solve(A3, B3)

?eigen()

B4 <- matrix(c(1:10, seq(2, 20, by = 2), seq(3, 30, by = 3), seq(4, 40, by = 4), 
               seq(5, 50, by = 5)), nrow = 10)
A4 <- matrix(c(rep(c(0,1),7), rep(c(0,0,1),2),1,0,1,1,0), nrow = 5, byrow = T)
(B4)%*%(A4)-(A4)%*%t(B4)

x2 <- matrix(c(rep(1,5), 1, -1, 0:2), nrow = 5, byrow = F)
y2 <- matrix(c(0,0,1,1,3), nrow = 5, byrow = F)

p = matrix(c(1,2,3,-2,4,-2,1,0,1), nrow = 3, byrow = T)

p1 <- p%*%p
p2 <- p1%*%p
p3 <- p2%*%p
p4 <- p3%*%p
p5 <- p4%*%p

#install.packages("expm")
#library(expm)
p%^%6

f2 <- function(x,k){
  r <- diag(dim(x)[2])
  for(i in 1:k){
    r <- r%*%x
  }
  r
}
f2(p,6)

matrix2 <- solve(t(x2)%*%x2)%*%t(x2)%*%y2

data("co2")
means = aggregate(co2, FUN=mean)
year = as.vector(time(means))
co2 = as.vector(means)

#Calcular un vector de diferencias de CO2 entre años consecutivos
df_co2 <- data.frame(year = year, co2 = co2) %>%
  mutate(co2_2 = lag(co2), dif_co2 = co2 - co2_2)
vect_diferencia <- df_co2$dif_co2[2:39]
vect_diferencia
#Crear un plot con lineas y puntos mostrando las diferencias consecutivas de  
#CO2 en función del tiempo (1960, 1961, etc…), en negrita

plot(x = df_co2$year[2:39], y = vect_diferencia, xlab = "A?os", ylab = "Variaci?n de CO2", 
     main = "Diferencia de CO2 & tiempo", type = "o", pch = 16, xlim = c(1960,2020), 
     ylim = c(0,3))
points(2020, 2.64, pch = 4, col = "red")

plot(x = df_co2$year[2:39], y = vect_diferencia, xlab = "A?o", ylab = "CO2", 
     main = "Diferencia de CO2 & tiempo", type = "o", pch = 16)

# Lee el archivo rainfall.csv como un data.frame
df_rain <- as_tibble(read_csv("ProgramacionR-master/data/rainfall.csv"))
# Calcula e imprime un vector con los nombres de las estaciones donde al menos 
#uno de los meses tiene una precipitaci?n superior a 180mm.
names_est <- dplyr::select(df_rain, sep:apr, name) %>% 
  dplyr::filter(sep > 180 | oct >180)

###################
data2 <- as_tibble(read.csv("ProgramacionR-master/data/raingaugeDataset.csv"))
names(data2)
data3 <- as_tibble(read.csv("ProgramacionR-master/data/listRaingauge.csv"))
names(data3)

est <- dplyr::select(data3, NOM_EST, CODIGO) %>% 
  dplyr::filter(NOM_EST == "MORROPON")
#EJERCICIO 1
ejerc1 <- data2 %>% dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  rename(pp = qc00000235) %>% 
  dplyr::select(date, pp) %>%
  group_by(date) %>% 
  mutate(missVal = sum(is.na(pp)))  
ejerc1
#EJERCICIO 2
ejerc2 <- data2 %>% dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  rename(pp = qc00000235) %>%
  dplyr::select(date, pp) %>%
  group_by(date = str_sub(date, 1, 7)) %>% 
  mutate(missVal = sum(is.na(pp)) * 100 / n()) %>%   
  summarize(
    pp = sum(pp , na.rm = T),
    missVal = unique(missVal)
  ) %>% 
  mutate(
    pp = ifelse(missVal >= 10 , NA, pp),
    date = as.Date(sprintf("%1$s-01", date)), #Para agregar el 01 a la fecha
    month= str_sub(date, 6, 7)
  )
#EJERCICIO 3
sum(is.na(ejerc2$pp))
max(ejerc2$pp, na.rm = T)
min(ejerc2$pp, na.rm = T)
#EJERCICIO 4

ejerc4 <- ejerc2 %>%
  group_by(pp)
  mutate(climatologia = bucle)
           
bucle <- for (pp1 in ejerc2$pp){
  if (pp1 %in% 0:250) {
    print("desertico")
  } else if (pp1 %in% 251:500) {
    print("arido")
  } else if (pp1 %in% 501: 1030.7) {
    print("moderadamente lluvioso")
  } else {
    print("NA")
  }
}
length(bucle)
#EJERCICIO 5
class(bucle)
view(ejerc2)

boxplot(ejerc2$pp)
?boxplot
