# Selecionando o diretório de trabalho
setwd("C:/")
getwd()

# Pacotes necessários para a análise
library(dplyr)
library(arules)
library(arulesViz)
library(readxl)

# Carregando os dados
base_conluio <- read_excel('nome_do_arquivo.xlsx')
View(base_conluio)

# Manipulação do dataset - criando uma nova coluna "ID_LICITACAO" para criar um ID para cada licitação
base_conluio$ID_LICITACAO <- paste(base_conluio$ENT_CODIGO, base_conluio$MODALIDADE, base_conluio$PLIC_NUMERO, sep = "_")
View(base_conluio)
nrow(base_conluio) # qntd de registros na base

# Removendo eventuais valores NA do campo de CNPJ
base_conluio2 <- base_conluio %>% 
  subset(!is.na(base_conluio$CPF_CNPJ_PARTICIPANTE)) 
View(base_conluio2)
nrow(base_conluio2) # qntd de registros na nova base, após exclusão dos NAs

# Informações sobre o data set de trabalho
nrow(base_conluio2) # quantidade de registros na base
length(unique(base_conluio2$ID_LICITACAO)) # quantidade de licitações na base
length(unique(base_conluio2$CPF_CNPJ_PARTICIPANTE)) #quantidadde de licitantes na base

# Criando as regras de associação - Para gerar as regras de associações, precisamos dos fornecedores de cada licitação
# O comando abaixo irá dividir os registros do data set de acordo com a licitação a que se refere.
# Para isso, usaremos a função "split". No caso, iremos dividir as licitações com os respectivos registros de CNPJs.
licit_lista <- split(base_conluio2$CPF_CNPJ_PARTICIPANTE, f=factor(base_conluio2$ID_LICITACAO))

# O comando abaixo irá remover os licitantes que aparecem em duplicidade em cada licitação.
# A função "lapply" permite que apliquemos uma função, no caso a função "unique" em cada elemento da minha lista 
# A função "unique" extrai os componentes únicos do objeto.
# Tem-se então uma lista na qual cada componente (licitação) é composta por um vetor contendo os CNPJs
licit_lista <- lapply(licit_lista, unique)

# Para trabalhar com o pacote "arules" é preciso converter a lista (licit_lista) num objeto da classe "transactions"
transacoes_cnpj <- as(licit_lista, "transactions")

# O comando abaixo irá aplicar a função "apriori" (algortimo de regra de associção do pacote "arules").
regras <- apriori(transacoes_cnpj, parameter = list(support = 0.001, confidence = 0.7))
regras2 <- subset(regras, lift > 1.0)
regras2 <- sort(regras2, by = "confidence")
summary(regras2)
inspect(regras2)

# Para inspecionar as regras redundantes
is.redundant(regras2)

# Removendo as regras duplicadas
red <- generatingItemsets(regras2)
red1 <- which(duplicated(red))

# Gravando o objeto Regras2 sem as regras redundantes
regras2 <- regras2[-red1]
summary(regras2)
inspect(regras2)

# Graficos das regras de associacao geradas:
plot(regras2, method = 'graph', engine = 'htmlwidget')
plot(regras2, method = 'scatter', engine = 'htmlwidget')
plot(regras2, 'graph', control=list(cex=0.6))
plot(regras2, control=list(cex=1.2))

# Salvar resultado em csv
write(regras2, file = 'endereço do diretório de trabalho.csv', sep = ';', col.names = NA)