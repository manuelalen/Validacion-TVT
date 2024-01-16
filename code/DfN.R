# Definir la función f(N)
f <- function(N) {
  # Define la función que representa la desviación para N <= N_c
  # Puedes ajustar esta función según tus necesidades
  return(N^2)
}

# Definir la función g(N, h)
g <- function(N, h) {
  # Define la función que representa la transición suave en N_c
  # Puedes ajustar esta función según tus necesidades
  return(h * exp(-N))
}

# Definir la función D(N)
D <- function(N) {
  # Definir el punto crítico donde la desviación se estabiliza
  N_c <- 5
  
  # Calcular la desviación utilizando la función f(N)
  deviation <- f(N)
  
  # Calcular la contribución suave directamente hasta el punto crítico
  if (N <= N_c) {
    h <- 1e-10  # Pequeño incremento positivo
    deviation <- deviation + g(N, h)
  } else {
    # Si N es mayor que N_c, la desviación se mantiene constante
    deviation <- f(N_c)
  }
  
  return(deviation)
}

# Visualizar la función D(N) con ggplot2
library(ggplot2)

# Crear un conjunto de datos para la gráfica
data <- data.frame(N = seq(0, 10, by = 0.1),
                   D = sapply(seq(0, 10, by = 0.1), D))

# Crear la gráfica
ggplot(data, aes(x = N, y = D)) +
  geom_line() +
  labs(title = "Función D(N)",
       x = "N",
       y = "D")+
  theme(panel.grid = element_blank(),  # Eliminar el fondo cuadriculado
        panel.border = element_blank(),  # Eliminar los bordes del panel
        axis.line = element_line(color = "black", size = 0.5),  # Resaltar los ejes
        axis.text = element_text(size = 10),  # Ajustar el tamaño del texto del eje
        axis.title = element_text(size = 12))  # Ajustar el tamaño del título del eje
