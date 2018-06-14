# #m?todo polar de transforma??o de vari?veis aleat?rias com distribui??o
# #uniforme em variaveis com distribui??o Gaussiana ou Normal


ipolar <- function (centro, dp) {

u <- 0
v <- 0
s <- 0
 
if (centro !=0 | dp != 1) #verifica se se alterou o centro e a variancia da distribui??o a gerar
{
variavel = TRUE
}

while (s >= 1 || s == 0){
#gera coordenadas u e v entre -1 e 1
	u = runif (1,0,1) * 2 - 1  
	v = runif (1,0,1) * 2 - 1   
	s = u * u + v * v 
}
    if (variavel) # se a media e variancia ? diferente de 0 e 1 respectivamente
	{
	valor_y = centro + dp * v * sqrt ( - 2.0 * log ( s ) / s )
	valor_x = centro + dp * u * sqrt ( - 2.0 * log ( s ) / s )
	ponto = cbind(valor_x,valor_y)
	return (ponto)
	}
	else{ # m?dia 0, variancia 1
	valor_y = v * sqrt ( - 2 * log ( s ) / s )
	valor_x = u * sqrt ( - 2 * log ( s ) / s )
	ponto = cbind(valor_x,valor_y)
	return (ponto)
	}
}

main <- function(nreps, centro, dp) #nreps- n?mero de repetic?es, centro da distribui??o e dp - desvio padrao da distribui??o a gerar
{
estimativas <- NULL
estimativas_x <- NULL
estimativas_y <- NULL

	for (i in 1:nreps)
	{
	aux <- ipolar(centro,dp)
	estimativas_x[i] <- aux[1]
    estimativas_y[i] <- aux[2]
	}
	#browser()
estimativas <- cbind(estimativas_x, estimativas_y)
}

centro <- 5 #centro da distribui??o ou m?dia 
dp <- 2 #variancia 4 
simvalues <- main(10000, centro, dp)

hist(simvalues[,1], xlim=c(centro-(dp*2)^2,centro+(dp*2)^2), col="brown",main="Transforma??o Polar",xlab="Ambas as Distribui??es Representadas c/intercep??o", ylab="Frequ?ncias das Estimativas")
hist(simvalues[,2], add=T, col=rgb(0, 1, 0, 0.5)) # adiciona histogramas das coordenadas x e y dos pontos gerados e transformados
print(summary(simvalues[,1]))
print(summary(simvalues[,2]))



