library(sqldf)
library(data.table)
library(dplyr)
library(tidyr)
library(tree)
library(rpart)

# http://inep.gov.br/web/guest/microdados

setwd("/home/cloves/Documentos/microdados_censo_superior_2016/")

# para verificar no terminal 
# quantidade de linhas no dataset
# wc -l < DM_ALUNO.CSV

# colunas que iremos usar
col = c(
  'CO_ALUNO',
  'DS_TURNO_ALUNO',
  'NO_OCDE_AREA_GERAL',
  'DS_GRAU_ACADEMICO',
  'DS_MODALIDADE_ENSINO',
  'DS_CATEGORIA_ADMINISTRATIVA',
  'DS_COR_RACA_ALUNO', 
  'DS_SEXO_ALUNO', 
  'CO_NACIONALIDADE_ALUNO', 
  'DS_ALUNO_SITUACAO',
  'IN_ING_VESTIBULAR', 
  'IN_ING_ENEM',
  'IN_ING_AVALIACAO_SERIADA', 
  'IN_ING_SELECAO_SIMPLIFICADA', 
  'IN_ING_SELECAO_VAGA_REMANESC',
  'IN_ING_SELECAO_VAGA_PROG_ESPEC', 
  'IN_ING_TRANSF_EXOFFICIO', 
  'IN_ING_DECISAO_JUDICIAL',
  'IN_ING_CONVENIO_PECG', 
  'IN_RESERVA_VAGAS', 
  'IN_APOIO_SOCIAL',
  'IN_ATIVIDADE_EXTRACURRICULAR',
  'CO_TIPO_ESCOLA_ENS_MEDIO',
  'ANO_INGRESSO'
)



# tabela para manipulação 
dt = fread("DADOS/DM_ALUNO.CSV",select=col, encoding = 'Latin-1')

# será que há observações repetidas?
sum(duplicated(dt))

# removendo repetidos
dt = dt[!duplicated(dt), ]

# numero de anos no curso
dt = data.frame(dt)
dt['qtd_anos'] = max(dt$ANO_INGRESSO) - dt$ANO_INGRESSO
hist(dt$qtd_anos)

# após tirar observações repetidas
# remover a coluna CO_ALUNO
dt = dt[ , -1]
dt = dt[, -23]

# usar apenas os brasileiros
dt = dt[dt['CO_NACIONALIDADE_ALUNO'] == 1,]
# remover a coluna de nacionalidade
dt = dt[, -8]
names(dt)
# área do curso
area_curso = unique(dt$NO_OCDE_AREA_GERAL)

dt = dt[!dt$NO_OCDE_AREA_GERAL=="", ]

# verificando se há valores null 
#apply(dt, 2, function(x) any(is.na(x)))

# removendo os na
#na_rows = as.data.frame(which(is.na(dt), arr.ind=TRUE))
#df = df[!na_row$row]



renomear_var = function(df){
n = nrow(df)
coluna = df$NO_OCDE_AREA_GERAL
for(i in 1:n){
  if(coluna[i] == "Ciências sociais, negócios e direito" |
     coluna[i] == "Humanidades e artes" ){
    coluna[i] = "humanas"
  }
 else{if(coluna[i] == "Engenharia, produção e construção" |
        coluna[i] == "Ciências, matemática e computação"){
    coluna[i] = "exatas"
  }
  else{if( coluna[i] == "Agricultura e veterinária"){
    coluna[i] = "agro_veter"
  }
  else{if(coluna[i] == "Serviços"){
    coluna[i] = "servico"
  }
  else{if(coluna[i] == "Saúde e bem estar social"){
    coluna[i] = "saude"
  }
  else{
    coluna[i] = "educacao"
  }}}}}}

return(coluna)
  
}


# renomeando coluna área geral
dt$NO_OCDE_AREA_GERAL = renomear_var(dt)

# renomenado modalidade ensino
dt$DS_MODALIDADE_ENSINO = ifelse(dt$DS_MODALIDADE_ENSINO == "Curso a distância",
                                 'distancia', 'presencial')

# renomeando categoria adim
unique(dt$DS_CATEGORIA_ADMINISTRATIVA)

coluna = dt$DS_CATEGORIA_ADMINISTRATIVA
for(i in 1:length(coluna)){
  if(coluna[i] == "Pública Federal" |
     coluna[i] == "Pública Estadual" |
     coluna[i] == "Pública Municipal"){
    coluna[i] = "publica"
  }
  else{if(coluna[i] == "Privada com fins lucrativos" |
          coluna[i] == "Privada sem fins lucrativos"){
    coluna[i] = 'privada'
  }
    else{coluna[i] = 'especial'}}
}
dt$DS_CATEGORIA_ADMINISTRATIVA = coluna

# renomenado turno
unique(dt$DS_TURNO_ALUNO)
coluna = dt$DS_TURNO_ALUNO
for(i in 1:length(coluna)){
  if(coluna[i] == "Não aplicável"){
    coluna[i] = 'nao_aplicavel'
  }
}
dt$DS_TURNO_ALUNO = coluna
# renomeando raça
unique(df$DS_COR_RACA_ALUNO)
coluna = dt$DS_COR_RACA_ALUNO
for(i in 1:length(coluna)){
  if(coluna[i] == "Aluno não quis declarar cor/raça"){
    coluna[i] = 'nao_quis_decl'
  }
  else{if(coluna[i] == "Não dispõe da informação")
    {coluna[i] = 'sem_inf'}}
}

dt$DS_COR_RACA_ALUNO = coluna
plot(df$DS_COR_RACA_ALUNO)
                
# uma amostra da tabela dt
# para visualização 
df = dt[sample(nrow(dt), 100), ]


# transformando variáveis em categóricas

cols = names(dt)
cols

# transformando todas as variáveis em categorias
dt[cols] <- lapply(dt[cols], factor)
# verificando as categorias
sapply(dt,class)

dt$qtd_anos = as.integer(dt$qtd_anos)
# treinando modelo
dt$
#arvore = tree(DS_ALUNO_SITUACAO ~., dt)
arvore = rpart(DS_ALUNO_SITUACAO ~., data = dt)

summary(arvore)
arvore$y

plot(arvore)
text(arvore, pretty = 0)



# salvar objeto em um arquivo
saveRDS(dt, file = "DADOS/dados_processados.rds")
saveRDS(arvore, file = "modelo_arvore.rds")
# fazer o load do objeto
dt = readRDS(file = "DADOS/dados_processados.rds")
arvore = readRDS(file =  "modelo_arvore.rds")

plot(dt$DS_ALUNO_SITUACAO)
unique(dt$DS_ALUNO_SITUACAO)
