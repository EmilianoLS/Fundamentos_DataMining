install.packages('data.table')

library("data.table")
library(ggplot2)
library("dplyr")


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


grepl('^sum_dsi:',colnames(train))







