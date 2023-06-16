## Primeiro importar as bibliotecas necessárias. 
library(ggplot2)
library(caret)
library(MASS)

X1 <- c(2.0,4.5,5.5,6.6,7.9,5.0)
X2 <- c(2.1,4.2,5.3,6.4,7.5,5.1)
X3 <- c(2.0,4.5,5.5,6.6,7.9,5.0)

## As variáveis independentes (X1, X2, X3, etc.) precisam ser preparadas.
dataset <- data.frame(X1, X2, X3)

## Criando os conjuntos de treinamento (modelo) e de teste.
train_set <- dataset[1:round(0.75*nrow(dataset)),]
test_set <- dataset[(round(0.75*nrow(dataset)) + 1):nrow(dataset),]

## Criando o modelo de regressão linear.
modelo <- lm(X2~., data=train_set)
modelo

## Previsões utilizando o modelo de regressão linear.
predicoes <- predict(modelo, newdata=test_set)
predicoes

## Avaliando o modelo como um todo.
sumario <- summary(modelo)
sumario

## Plotagem dos resultados obtidos. 
ggplot(test_set, aes(x=X1, y=X2)) + 
  geom_point() +
  geom_line(aes(y=predicoes), size=1.5, color="red")
