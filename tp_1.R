install.packages('data.table')
install.packages("stringr")
install.packages("gridExtra")

library("stringr")
library("data.table")
library(ggplot2)
library("dplyr")
library(gridExtra)

file_route <- 'C:Users/elosasso/OneDrive - Universidad Torcuato Di Tella/Mineria de datos/Fundamentos_DataMining'

load_csv_data <- function(csv_file, sample_ratio = 1, drop_cols = NULL,
                          sel_cols = NULL) {
  # Esta funcion carga un unico csv con los parametros que le indique
  
  dt <- fread(csv_file, header = TRUE, sep = ",", stringsAsFactors = TRUE,
              na.strings = "", drop = drop_cols, select = sel_cols,
              showProgress = TRUE)
  return(dt)
}



load_train_data <- function(data_dir, train_file="/train_", sample_ratio=1,
                            drop_cols=NULL, sel_cols=NULL) {
  # Esta funcion se encarga de concatenar todos los dataframes juntos
  
  
  
  train_days <- seq(1, 5, by=1)
  
  dfs <- list()
  
  for (i in train_days){
  
    dfs[[i]] <- load_csv_data(csv_file = paste(data_dir,train_file,as.character(i),".csv", sep = ''), 
                              sample_ratio = sample_ratio, drop_cols = drop_cols, sel_cols = sel_cols)
  }
  
  # Uno todos los dataframes en uno solo
  
  df <- (rbindlist(dfs, fill=TRUE))
  
  # Reordeno las columnas alfabeticamente
  
  setcolorder(df, sort(colnames(df)))
  
  # Creo la columna Label con las condiciones de churn explicadas
  
  df[, Label := as.numeric(Label_max_played_dsi == 3)] 
  
  if (sample_ratio < 1) {
    
    sample_size <- as.integer(sample_ratio * nrow(df))
    
    df <- df[sample(.N, sample_size)]
  }
  
  rm(dfs)
  
  
  return(df)
}



csv_dir <- "C:/Users/elosasso/OneDrive - Universidad Torcuato Di Tella/Mineria de datos/datasets"

train <- load_train_data(csv_dir, sample_ratio = 0.1)

#########################################################################################################

# Análisis exploratorio

#########################################################################################################

# Dimension del dataframe

dim(train)
colnames(train)

str(train)

#########################################################################################################
# Miro los valores nulos 

columnas_nas <- list()

for (i in colnames(train)){
  nans <- nrow(filter(select(train,i), !complete.cases(select(train,i))))
  
  if (nans != 0){
    
    columnas_nas[[i]] <- nans/nrow(train)
  }
  
}

as.data.frame(columnas_nas)

# Se ve que en algunos features hay muchos nulos:
# En la columna age y en la columna site, el porcentaje de nulos es muy alto, hay que analizar posteriormente
# si deben tomarse para el modelo, o si fue un error particular de la muestra (tal vez tomar un porcentaje mayor)

#########################################################################################################

ggplot(train) + geom_bar(mapping = aes(x = TutorialStart)) + facet_wrap(~TutorialFinish) + xlab('Empezo el tutorial')

ggplot(train) + geom_bar(mapping = aes(x = platform))


# Armo un top de los paises con mas usuarios

sort(table(train$country), decreasing = TRUE)[c(1:10)]

# Hay que tener cuidado porque en algunos paises no hay registros, con lo cual, puede terminar influyendo al modelo


tabla <-prop.table(table(train$country,churn = train$Label),1)[,2]

pdf("imagen01.pdf")

barplot(sort(tabla, decreasing = FALSE), horiz = TRUE, ylab = 'Pais', xlab = 'Ratio de churn'
        , main = 'Proporción de churn por pais')
dev.off() 

train$churn <- train$Label == 1

ggplot(train) + geom_bar(mapping = aes(x = platform, fill = churn), position = 'fill') +
  labs(title = 'Proporcion de churn por plataforma', x = 'Plataforma', y = '%')

# Quiero ver si la cantidad de veces que gano o perdió, en función de las partidas iniciadas pueden estar relacionados con el churn
# Creo las variables sumadas de todas las veces que:
# Perdio una batalla

train$losebattle <- train$LoseBattle_sum_dsi0 + train$LoseBattle_sum_dsi1 + train$LoseBattle_sum_dsi2 + train$LoseBattle_sum_dsi3

# Gano una batalla

train$winbattle <- train$WinBattle_sum_dsi0 + train$WinBattle_sum_dsi1 + train$WinBattle_sum_dsi2 + train$WinBattle_sum_dsi3

# Empezó una batalla

train$startbattle <- train$StartBattle_sum_dsi0 + train$StartBattle_sum_dsi1 + train$StartBattle_sum_dsi2 + train$StartBattle_sum_dsi3

# Creo las proporciones

train$lose_vs_start_battle <- train$losebattle/train$startbattle
train$lose_vs_win_battle <- train$losebattle/train$winbattle

# Grafico

ggplot(train) + geom_histogram(aes(x = lose_vs_start_battle, alpha = 0.3, fill = churn))
ggplot(train) + geom_histogram(aes(x = lose_vs_win_battle, alpha = 0.3, fill = churn))
# Cuantas columnas hay que terminan en 'sum_dsi'

columnas_sum_names <- colnames(train)[c(str_detect(colnames(train), 'sum_dsi'))]
columnas_sum <- length(colnames(train)[c(str_detect(colnames(train), 'sum_dsi'))])


plot1 <- ggplot(train) + geom_boxplot(aes(BuyCard_sum_dsi0),outlier.colour = 'red', outlier.shape = 1, outlier.alpha = 0.3) 
plot2 <- ggplot(train) + geom_boxplot(aes(BuyCard_sum_dsi1),outlier.colour = 'red', outlier.shape = 1, outlier.alpha = 0.3)
plot3 <- ggplot(train) + geom_boxplot(aes(BuyCard_sum_dsi2),outlier.colour = 'red', outlier.shape = 1, outlier.alpha = 0.3)
plot4 <- ggplot(train) + geom_boxplot(aes(BuyCard_sum_dsi3),outlier.colour = 'red', outlier.shape = 1, outlier.alpha = 0.3)
  
final_plot <- grid.arrange(plot1,plot2,plot3,plot4, nrow = 2, ncol = 2, top = 'BuyCard')

ggsave("test.pdf", plot = final_plot, width = 20, height = 20, units = "cm")


colnames(train, colnames(train)[c(str_detect(colnames(train), 'sum_dsi'))][c(1:4)][1])

summary(select(train, columnas_sum_names))



