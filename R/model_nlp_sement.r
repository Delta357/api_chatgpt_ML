library(tm)
library(SnowballC)
library(caret)
library(e1071)

# Criar a base de dados
frases <- c("Eu amo esse filme, é incrível!",
            "O serviço nesse restaurante é péssimo.",
            "O tempo está lindo hoje.",
            "Não gostei dessa nova música.",
            "Essa série é muito envolvente.",
            "A comida aqui é deliciosa.",
            "Que jogo emocionante, valeu a pena assistir!",
            "Estou muito triste com essa notícia.",
            "As férias foram maravilhosas!",
            "Não recomendo esse produto, não funcionou para mim.")

rotulos <- c("positivo",
             "negativo",
             "positivo",
             "negativo",
             "positivo",
             "positivo",
             "positivo",
             "negativo",
             "positivo",
             "negativo")

dados <- data.frame(frases, rotulos)
head(dados)

# Pré-processamento dos dados
corpus <- Corpus(VectorSource(dados$frases))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
corpus <- tm_map(corpus, stripWhitespace)

# Transformação dos dados
matriz_termos <- DocumentTermMatrix(corpus)

# Divisão dos dados em treinamento e teste
set.seed(123)
divisao <- createDataPartition(dados$rotulos, p = 0.8, list = FALSE)
dados_treino <- dados[divisao, ]
dados_teste <- dados[-divisao, ]

# Atualizar níveis das variáveis categóricas
dados_teste$rotulos <- factor(dados_teste$rotulos, levels = levels(dados_treino$rotulos))

# Treinamento do modelo
modelo <- naiveBayes(rotulos ~ ., data = dados_treino)

# Avaliação do modelo
previsoes <- predict(modelo, newdata = dados_teste, type = "class")
matriz_confusao <- table(previsoes, dados_teste$rotulos)
acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)

print(matriz_confusao)
print(paste("Acurácia:", acuracia))
