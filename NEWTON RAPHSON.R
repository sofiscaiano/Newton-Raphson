Newton.Raphson <- function (x0,Tol,N){ #x0: Aproximacion inicial / Tol: Tolerancia / N= Cantidad de iteraciones
  Funcion <- function(x){
    f <- log(x-1) + cos(x-1)
    return (f)  #Llamo la funcion a analizar
  }
  
  Derivada <- function(x){
    g <- (x-1)^(-1) - sin(x-1)
    return (g)  #Llamo a su derivada
  }
  N <- N
  i <- 1 #Paso 1
  while (i <= N){  #Paso 2
    x1 <- x0 - Funcion(x0)/Derivada(x0) #Paso 3
    if (abs(1.397748476 - x1) < Tol) { #Paso 4
      return (x1)
    }
    i <- i+1 #Paso 5
    x0 <- x1 #Paso 6
  }
  return(paste('El metodo fracaso después de ',N,' iteraciones')) #Paso 7
}
Newton.Raphson(1.5,0.0001,10) #Ejecuto la funcion programada

Raiz.NR <- Newton.Raphson(1.5,0.0001,5) #Guardo el resultado en el enviroment.

par(mfrow=c(1,2)) 

plot(Raiz.NR,xlab="x",ylab="y") #Grafico de la raiz

Funcion <- function(x){
  f <- log(x-1) + cos(x-1)
  return (f)  #Llamo la funcion a analizar
}

curve(expr=Funcion,from=1.1,to=1.4,n=1000,add=FALSE) #Grafico de la Funcion

paste(Raiz.NR)



