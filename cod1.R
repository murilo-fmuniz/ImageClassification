# Instalar e carregar os pacotes necessários
install.packages('OpenImageR')
install.packages('randomForest')
install.packages('ggplot2')
library(OpenImageR)
library(randomForest)
library(ggplot2)
library(reshape2)

# Função para zerar um canal específico de uma imagem
zerar_canal <- function(img, canal) {
  img[, , canal] <- 0
  return(img)
}

# Função para ler e preprocessar imagens
ler_imagem <- function(caminho) {
  img <- readImage(caminho)
  # Se precisar zerar os canais, descomente as linhas abaixo
  # img <- zerar_canal(img, 3) # zerar canal Blue
  # img <- zerar_canal(img, 2) # zerar canal Green
  # img <- zerar_canal(img, 1) # zerar canal Red
  return(img)
}

# Ler as imagens
anos <- c(2005, 2007, 2008, 2010, 2012, 2013, 2014, 2017, 2018, 2019, 2020, 2021, 2022, 2024)
imagens <- lapply(paste0('entornos utfpr ', anos, '.jpg'), ler_imagem)

# Ler as amostras rotuladas
floresta <- readImage('Florestas.jpg')
terreno <- readImage('Terrenos.jpg')
residencias <- readImage('Residências.jpg')

# Preprocessar amostras rotuladas
mfloresta <- cbind(c(floresta[,,1]), c(floresta[,,2]), c(floresta[,,3]))
mterreno <- cbind(c(terreno[,,1]), c(terreno[,,2]), c(terreno[,,3]))
mresidencias <- cbind(c(residencias[,,1]), c(residencias[,,2]), c(residencias[,,3]))

# Amostrar dados para balancear o conjunto de treinamento
set.seed(123)
mfloresta <- mfloresta[sample(1:nrow(mfloresta), 10000),]
mterreno <- mterreno[sample(1:nrow(mterreno), 10000),]
mresidencias <- mresidencias[sample(1:nrow(mresidencias), 10000),]

# Criar o conjunto de dados de treinamento
dados <- rbind(cbind(mfloresta, 0), cbind(mterreno, 1), cbind(mresidencias, 2))
colnames(dados) <- c("R", "G", "B", "Y")

# Treinar o modelo
modelo <- randomForest(as.factor(Y) ~ R + G + B, data = dados)

# Função para prever categorias em uma imagem
prever_imagem <- function(img, modelo) {
  mtudo <- cbind(c(img[,,1]), c(img[,,2]), c(img[,,3]))
  colnames(mtudo) <- c('R', 'G', 'B')
  pred <- predict(modelo, newdata = mtudo)
  pred <- as.numeric(pred) - 1
  return(matrix(pred, ncol = dim(img)[2]))
}

# Prever categorias em todas as imagens e contar os pixels em cada categoria
resultados <- lapply(imagens, prever_imagem, modelo = modelo)
contagens <- t(sapply(resultados, function(img) table(factor(img, levels = 0:2))))

# Adicionar anos aos resultados
df_resultados <- data.frame(Ano = anos, Floresta = contagens[, 1], Terreno = contagens[, 2], Residencias = contagens[, 3])

# Plotar os resultados
df_resultados_long <- melt(df_resultados, id.vars = 'Ano', variable.name = 'Categoria', value.name = 'Pixels')
ggplot(df_resultados_long, aes(x = Ano, y = Pixels, color = Categoria, group = Categoria)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Mudança nas Categorias de Terreno ao Longo dos Anos", x = "Ano", y = "Número de Pixels")

