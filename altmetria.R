# Adaptado de: https://github.com/ropensci/rAltmetric

library(rAltmetric)
library(tidyverse)

# .txt file
ids <- split(dois, seq(nrow(dois)))   # Converter linhas para lista 

# Coletar os dados mesmo que haja erros na execução 
# Neste caso, irá retornar NULL caso a API não encontre um DOI para o artigo
safe_altmetrics <- purrr::safely(altmetrics, otherwise = NULL)
alm <- function(x)  safe_altmetrics(doi = x)
# Chamar API

requests <- map(ids, alm) 
# Mapear os resultados para uma lista,
# Remover os valores nulos (NULL),
# Então, executar o altmeric_data nos dados restantes

results <- requests %>%  
  map("result") %>% 
  compact(.) %>% 
  modify_depth(1, altmetric_data)

data <- bind_rows(results) %>% select(doi, contains(c("title","cited","readers","score")))

# Substituir NA por  0
data[is.na(data)] <- 0

# Exportar o resultado para Excel
library("writexl")
write_xlsx(data,"alt.xlsx")