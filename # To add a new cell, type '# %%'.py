# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %% [markdown]
# # **Challenge Project 0 1 - Boston Houses**
# 
# *18 de março, 2020*
# %% [markdown]
# ## **1. Descrição geral do problema**
# %% [markdown]
# ---
# 
# ![Boston Houses](https://i.ya-webdesign.com/images/houses-for-sale-in-png-1.png)
# 
# Este conjunto de dados contém informações coletadas pelo Serviço de Censo dos EUA sobre moradias na área de  Massachusetts Mass. Foi obtido no arquivo [StatLib](http://lib.stat.cmu.edu/datasets/boston) e tem sido amplamente utilizado em todo o país na literatura para comparar algoritmos. O [conjunto de dados](http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html) é pequeno, com apenas 506 registros.
# 
# **Objetivo:** prever os preços de casas em Boston a partir das informações fornecidas pelo conjunto de dados. Iremos avaliar os modelos preditivos a serem desenvolvidos a partir da métrica *R-squared* (**R2**).
# 
# ---
# %% [markdown]
# ## **2. Carregando dados**
# %% [markdown]
# ### **2.1 Importando bibliotecas necessárias**
# 
# %% [markdown]
# Vamos começar nosso projeto importanto todas as bilbiotecas necessárias para a realização das fases iniciais de exploração e transformação dos dados (*Data Munging*).

# %%
# Definindo a ocultação de warnings.

options(warn = -1)

# Caso não possua uma das bibliotecas importadas abaixo, a instale com um dos comandos a seguir:

install.packages(c(
    'MASS',
    'ggplot2',
    'ggthemes',
    'GGally',
    'corrplot',
    'dplyr',
    'caret',
    'e1071'
))


# %%
# Importando bibliotecas.

library(MASS)
library(ggplot2)
library(ggthemes)
library(GGally)
library(corrplot)
library(dplyr)
library(caret)
library(e1071)

# %% [markdown]
# ### **2.2 Carregando dados**
# %% [markdown]
# Nosso dataset é carregado junto com a biblioteca **MASS** e é dela que faremos a importação dos dados.

# %%
# Importando os dados do dataset Boston incluso na biblioteca MASS.

data <- Boston

# Visualizando as primeiras linhas do dataset.

head(data)

# %% [markdown]
# ## **3. Análise exploratória dos dados**
# %% [markdown]
# Nesta etapa vamos buscar entender a disposição e as características dos dados dentro do dataset além de extrair insigths que possam auxiliar no processo de criação do modelo preditivo.
# 
# Segundo a [documentação](http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html) referente ao projeto, cada linha dos dados contém um registro, com as seguintes variáveis:
# 
# | Variável         | Descrição                                                                                    |
# |:-----------------|:---------------------------------------------------------------------------------------------|
# | **crim**         | é a taxa de criminalidade per capita por cidade;                                             |
# | **zn**           | é a proporção de terrenos residenciais divididos por lotes acima de 25.000 pés quadrados;    |
# | **indus**        | é a proporção de acres não comerciais por cidade;                                            |
# | **chas**         | indica se a casa limita ou não o Rio Charles (1 se o trecho limita o rio; 0 caso contrário); |
# | **nox**          | é a concentração de óxidos nítricos (partes por 10 milhões);                                 |
# | **rm**           | é o número médio de quartos por habitação;                                                   |
# | **age**          | é a proporção de unidades ocupadas pelos proprietários construídas antes de 1940;            |
# | **dis**          | é a distância ponderada de cinco centros de emprego em Boston;                               |
# | **rad**          | é o índice de acessibilidade às rodovias radiais;                                            |
# | **tax**          | é a taxa de imposto sobre a propriedade de valor total por US 10.000;                        |
# | **ptratio**      | é a proporção aluno-professor por cidade;                                                    |
# | **black**        | é definida por 1000 (Bk - 0,63) ^ 2 onde Bk é a proporção de negros por cidade;              |
# | **lstat**        | é a menor porcentagem do status da população e;                                              |
# | **medv (Target)**| é o valor médio das casas ocupadas pelos proprietários em US 1.000.                          |
# 
# 
# %% [markdown]
# ### **3.1 Visão geral dos dados**

# %%
# Verificando os tipos das colunas carregadas do dataset.

glimpse(data)

# %% [markdown]
# Verificamos a existência de 14 variáveis numéricas e 506 observações dentro do dataset.

# %%
# Verificando a existência de valores NA no dataset.

print(sapply(data, function(v) {
    anyNA(v)
}))

# %% [markdown]
# Não foi detectado nenhum valor *NA* dentro do conjunto de dados.

# %%
# Verificando o número de valores únicos presentes em cada uma das variáveis especificadas.

print(sapply(data, function(v) {
    length(unique(v))
}))

# %% [markdown]
# Note que as variáveis **chas** e **rad** são as que apresentam as menores quantidades de valores únicos. Isso nos indica que ambas podem estar representando categorias de classes em cada variável.
# 
# Podemos confirmar esta teoria para a variável **chas** a partir da documentação do dataset que a classifica como uma variável categórica. E caso seja necessário, também podemos tratar a variável **rad** da mesma forma.
# 
# Agora vamos calcular algumas estatísticas para cada uma das variáveis quantitativas, isto é, iremos desconsiderar a variável qualitativa **chas** nas etapas a seguir.

# %%
# Definindo as variáveis quantitativas dentro do dataset.

numVars <- colnames(data) != 'chas'

# Verificando estatísticas das variáveis do dataset que representam variáveis quantitativas.

stats <- do.call(cbind, lapply(data[, numVars], summary))

# Determinando o valor do desvio-padrão de cada variável.

dataSD <- sapply(data[, numVars], sd)

# Inserindo os desvios-padrão no dataset.

stats <- as.data.frame(rbind(stats, sd = dataSD))

# Determinando o valor do coeficiente de variação de cada variável.

dataCV <- sapply(data[, numVars], function(c){
    sd(c) / mean(c) * 100
})

# Inserindo os coeficiente de variação no dataset.

stats <- as.data.frame(rbind(stats, CV = dataCV))

# Alterando nome das linhas.

rownames(stats) <- c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', 'Sd', 'CV')

# Exibindo o dataset.

stats

# %% [markdown]
# As variáveis que apresentam média, moda e mediana iguais são classificadas como tendo uma distribuição normal. No dataset que estamos manipulando não há nenhuma variável com está característica, mas podemos verificar se existe alguma que possua uma distribuição aproximadamente normal.

# %%
# Calculando a diferença entre o valor médio e mediano de cada variável.

d <- sapply(stats[c('Median', 'Mean'), ], diff)

# Ordenando em ordem decrescente a diferença entre o valor médio e mediano de cada variável.

print(d[order(d, decreasing = T)])

# %% [markdown]
# Podemos observar que as variáveis **rm** e **nox** são as que apresentam as menores diferenças absolutas entre seus valores médio e mediano no dataset, ou seja, são as que mais se aproximam de uma distribuição normal.
# 
# O coeficiente de variação das variáveis será a próxima estatística que vamos analisar.

# %%
# Transpondo o dataset com as estatísticas das variáveis.

statsT <- as.data.frame(t(stats))

# Ordenando em ordem crescente as linhas do dataframe a partir dos valores da variável CV.

statsT[order(statsT$CV), ]

# %% [markdown]
# As variáveis **rm** e **ptratio** são as que apresentam os menores coeficientes de variação, ou seja, seus conjuntos de dados são os mais homogêneos. Isto nos indica que: 
# 
# * **rm**: O número de quartos varia muito pouco e;
# * **ptratio**: a proporção aluno-professor por cidade varia muito pouco.
# 
# As variáveis  **zn** e **crim** são as que apresentam os maiores coeficientes de variação, ou seja, seus conjuntos de dados são os mais heterogêneos. Isto nos indica que:
# 
# * **zn**: a proporção de terrenos residenciais divididos por lotes acima de 25.000 pés quadrados varia muito e;
# * **crim**: a taxa de criminalidade per capita por cidade varia muito.
# %% [markdown]
# ### **3.2 Analisando a correlação entre as variáveis**
# %% [markdown]
# Nesta etapa desejamos verificar como as variáveis se correlacionam, ou seja, como uma variável ajuda a prever o valor de outra variável no dataset.

# %%
# Criando um pair plot para visualizar as correlações entre as variáveis númericas. 

ggpairs(data[, numVars]) + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1))

# %% [markdown]
# Algo interessante de se notar neste gráfico é a distribuição da variável **indus** que aparenta possuir 2 modas. Isso pode estar indicando a presença de dois subconjuntos de dados dentro da variável.
# 
# Bom, vamos investigar mais detalhadamente a força e a direção dessas correlações.

# %%
# Verificando a correlação entre as variáveis do dataset.

corrplot(cor(data[, numVars]),
         method      = 'color', 
         addCoef.col = 'white', 
         type        = 'upper', 
         tl.col      = 'black',
         diag        = F
)

# %% [markdown]
# Vemos que a variável alvo **medv** possui uma correlação moderada ou forte com todas as demais variáveis preditoras exceto com **chas**.
# 
# A variável **chas** apresenta um correlação fraca com todas as demais variáveis.
# 
# Existem correlações muito fortes entre as variáveis preditoras com as quais deveremos ter atenção para evitar problemas de *overfitting*.
# %% [markdown]
# ### **3.3 Explorando a distribuição de cada variável individualmente**
# %% [markdown]
# #### **3.3.1 Criando funções auxiliares**

# %%
# Definindo uma função pra criar gráficos de densidade.

densityPlot <- function(col, data) {

  ggplot() +
    geom_density(aes(data[, col]), fill = "#C3D7A4") +
    labs(title = paste('Density plot for variable:', col)) +
    xlab(col) +
    theme_bw() 
}


# %%
# Definindo uma função pra criar gráficos de boxplot.

boxPlot <- function(col, data) {

    ggplot() +
        geom_boxplot(aes(y = data[, col]), fill = "#B9314F") +
        labs(title = paste('Box plot for variable:', col)) +
        xlab(col) +
        theme_bw() + 
        theme(axis.text.x = element_blank()) +
        ylab('')
}


# %%
# Definindo uma função pra criar gráficos de barras.

barPlot <- function(col, data) {

    ggplot() +
        geom_bar(aes(x = data[, col]), fill = "#FF934F") +
        labs(title = paste('Bar plot for variable:', col)) +
        xlab(col) +
        theme_bw()
}

# %% [markdown]
# #### **3.3.2 Variável crim**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'crim'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que a taxa de criminalidade per capita por cidade está mais densamente concentrada no valor 0.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que há registros de casas com uma taxa de criminalidade per capita por cidade alta e discrepante dentro do conjunto de dados.
# %% [markdown]
# #### **3.3.3 Variável zn**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'zn'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que a proporção de terrenos residenciais está mais densamente concentrada no valor 0.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que há registros de casas com proporção de terrenos residenciais alta e discrepante dentro do conjunto de dados.
# %% [markdown]
# #### **3.3.4 Variável indus**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'indus'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que a proporção de acres não comerciais por cidade possui duas modas. Talvez, uma possível explicação para isso possa ser que o dataset possui conjunto de dados de regiões urbanas e rurais.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados de proporções de acres não comerciais por cidade apresenta uma assimetria à direita.
# %% [markdown]
# #### **3.3.5 Variável chas**
# %% [markdown]
# A variável chas é categórica, por isso vamos convertê-la para o tipo factor.

# %%
# Definindo o nome da variável a ser analisada.

col <- 'chas'

# Criando uma cópia do conjunto de dados.

dt <- data

# Convertendo variável para o tipo factor.

dt$chas <- factor(dt$chas, labels = c('No', 'Yes'))

# Criando um gráfico de densidade para a variável especificada.

barPlot(col, data = dt)

# %% [markdown]
# Há um desbalançeamento entre as classes deste conjunto de dados. Vamos mensurar numericamente esta desproporção.

# %%
# Determinando a proporção de registros para cada classe.

prop.table(table(chas = dt$chas))

# Eliminando cópia do dataset.

rm(dt)

# %% [markdown]
# Concluímos que aproximadamente 93% dos registros indica casas que não limitam o rio Charles enquanto apenas 7% o limita.
# %% [markdown]
# #### **3.3.6 Variável nox**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'nox'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra a densidade da concentração de óxidos nítricos nos registros do conjunto de dados. Parece haver uma assimetria à direita. 

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot parece indicar que os dados entre o primeiro e o terceiro quartil estão igualmente distribuidos, mas a calda superior do boxplot é muito maior que a inferior.
# 
# Para este caso, vamos calcular o coeficiente de **Assimetria** e **Curtose**.

# %%
# Determinando o coeficiente de assimetria para o conjunto de dados nox.

sk <- skewness(data$nox)

# Imprimindo o valor do índice.

paste('SK:', sk)

# %% [markdown]
# Para interpretar o resultado obtido, vamos olhar a tabela a seguir:
# 
# | Índice de Assimetria | Descrição                                                                                                                        |
# |:---------------------|:---------------------------------------------------------------------------------------------------------------------------------|
# | **SK ≈ 0**           | Os dados são simétricos. Tanto a cauda do lado direito quanto a do lado esquerdo da função densidade de probabilidade são iguais;|
# | **SK < 0**           | A assimetria é negativa. A cauda do lado esquerdo da função densidade de probabilidade é maior que a do lado direito e;          |
# | **SK > 0**           | A assimetria é positiva. A cauda do lado direito da função densidade de probabilidade é maior que a do lado esquerdo.            |
# 
# Como o índice de assimetria da variável **nox** foi maior que 0, podemos afirmar que este conjunto de dados possui uma assimetria positiva.

# %%
# Determinando o coeficiente de curtose para o conjunto de dados nox.

ck <- kurtosis(data$nox)

# Imprimindo o valor do índice.

paste('CK:', ck)

# %% [markdown]
# O coeficiente de *curtose* é uma medida que caracteriza o **achatamento da curva** da função de distribuição.
# 
# Para interpretar o resultado obtido, vamos olhar a tabela a seguir:
# 
# | Índice de Curtose | Descrição                                                                                                      |
# |:------------------|:---------------------------------------------------------------------------------------------------------------|
# | **CK ≈ 0**        | A distribuição é normal e é chamada de Curtose *Mesocúrtica*;                                                  |
# | **CK < 0**        | A Cauda é mais leve que a normal. Para um coeficiente de Curtose negativo, tem-se uma Curtose *Platicúrtica* e;|
# | **CK > 0**        | A Cauda é mais pesada que a normal. Para um coeficiente de Curtose positivo, tem-se uma Curtose *Leptocúrtica*.|
# 
# Como o índice de Curtose da variável **nox** é aproximadamente igual a 0, podemos afirmar que este conjunto de dados possui uma distribuição normal sendo classificada com curtose *mesocúrtica*.
# 
# **Atenção:** Há diferentes fórmulas para calcular estes coeficientes. Mas, para este estudo utilizamos as funções fornecidas pela biblioteca **e1071** com suas respectivas configurações padrão. Em caso de dúvida, consulte a [documentação](https://cran.r-project.org/web/packages/e1071/e1071.pdf).
# %% [markdown]
# #### **3.3.7 Variável rm**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'rm'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que o número médio de quartos por habitação aparenta possuir uma distribuição normal.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que há outliers na parte superior e inferior dos dados, mas a simetria parece se manter no gráfico.
# 
# Vamos confirmar essa teoria utilizando os coeficientes de **Assimetria** e **Curtose** mais uma vez.

# %%
# Determinando o coeficiente de assimetria para o conjunto de dados rm.

sk <- skewness(data$rm)

# Imprimindo o valor do índice.

paste('SK:', sk)

# %% [markdown]
# O conjunto de dados da variável **rm** apresenta uma assimetria positiva, ou seja, a cauda do lado direito é maior do que a do lado esquerdo.

# %%
# Determinando o coeficiente de curtose para o conjunto de dados rm.

ck <- kurtosis(data$rm)

# Imprimindo o valor do índice.

paste('CK:', ck)

# %% [markdown]
# Como o coeficiente de **Curtose** para o conjunto de dados da variável **rm** é maior do que 0, podemos afirmar que este conjunto de dados possui uma calda mais pesada do que a normal.
# %% [markdown]
# #### **3.3.8 Variável age**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'age'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que a proporção de unidades ocupadas pelos proprietários construídas antes de 1940 tem a maior densidade entre 85 e 95 anos.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados da variável **age** possui uma assimetria à esquerda.
# %% [markdown]
# #### **3.3.9 Variável dis**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'dis'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que a distância ponderada de cinco centros de emprego em Boston tem a maior densidade em torno do valor 2.5.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados da variável **dis** possui uma assimetria à direita.
# %% [markdown]
# #### **3.3.10 Variável rad**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'rad'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que o índice de acessibilidade às rodovias radiais possui dois valores aproximadamente mais frequentes, 5 e 24.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados da variável **rad** possui uma assimetria à direita.
# %% [markdown]
# #### **3.3.11 Variável tax**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'tax'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que  a taxa de imposto sobre a propriedade possui aproximadamente dois valores mais frequentes, 300 e 660.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados da variável **tax** possui uma assimetria à direita.
# %% [markdown]
# #### **3.3.12 Variável ptratio**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'ptratio'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que a proporção aluno-professor por cidade tem maior densidade no valor 20.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados da variável **ptratio** possui uma assimetria à esquerda.
# %% [markdown]
# #### **3.3.13 Variável black**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'black'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que  a proporção de negros apresenta a maior densidade em torno do valor 400.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados da variável **black** possui uma assimetria à esquerda com muitos outliers inferiores.
# %% [markdown]
# #### **3.3.14 Variável lstat**

# %%
# Definindo o nome da variável a ser analisada.

col <- 'lstat'

# Criando um gráfico de densidade para a variável especificada.

densityPlot(col, data = data)

# %% [markdown]
# O gráfico nos demonstra que a menor porcentagem do status da população tem maior densidade em torno do valor 10.

# %%
# Criando um boxplot para a variável especificada.

boxPlot(col, data = data)

# %% [markdown]
# O boxplot nos indica que o conjunto de dados da variável **lstat** possui uma assimetria à direita com muitos outliers superiores.
# %% [markdown]
# ### **3.4 Teste de hipóteses entre as variáveis medv e chas**
# %% [markdown]
# Por curiosidade, vamos análisar se existe uma diferença estatisticamente significativa entre os preços de casas que limitam ou não o rio Charles. Para isto, vamos criar um gráfico para visualizar a distribuição dos preços das casas em função das classes da variável **chas**.

# %%
# Análisando a distribuição dos preços das casas a partir da variável chas.

ggplot() +
  geom_boxplot(aes(x = factor(data[, 'chas'], labels = c('No', 'Yes')), y = data[, 'medv'] ), fill = "#C96480") +
  labs(title = 'Relationship between medv and chas') +
  xlab('chas') +
  ylab('medv') +
  theme_bw() 

# %% [markdown]
# Interessante, parece que as casas que não limitam o rio Charles apresentam preços medianos menores do que as casas que o fazem.
# 
# Mas, vamos aplicar um **teste T** para verificar se essa interpretação está efetivamente correta.

# %%
# Criando um teste T para verificar se a diferença das médias dos preços das casas (medv) agrupados por chas
# apresentam uma diferença na média estatisticamente significativa.

# Agrupando grupos dos valores das casas segundo a variável chas.

chasY <- data[data$chas == 1, 'medv'] 

chasN <- data[data$chas == 0, 'medv'] 

# %% [markdown]
# Definiremos nossas hipóteses da seguinte maneira:
# %% [markdown]
# | Teste de hipótese                                                                           |
# |:--------------------------------------------------------------------------------------------|
# |**H0:** As médias dos preços das casas que limitam ou não o rio Charles River são iguais.    |
# |**Ha:** As médias dos preços das casas que limitam ou não o rio Charles River são diferentes.|
# 

# %%
# Aplicando o teste T.

t.test(chasY, chasN)

# %% [markdown]
# Há evidências suficientes, com um nível de significância de 5%, para rejeitar a hipótese nula de que as médias dos preços das casas que limitam ou não o rio Charles River são iguais.
# 
# Concluímos que *há uma diferença estatisticamente* significativa entre os preços das casas que limitam e as que não limitam o rio charles. 
# %% [markdown]
# ## **4. Feature Engineering**
# 
# %% [markdown]
# Nesta etapa vamos escalar os valores das variáveis do dataset entre 0 e 1.

# %%
# Definindo método de pré-processamento.

params <- preProcess(data, method = 'range')

# Transformando os dados.

data <- predict(params, data)

# Visualizando as primeiras linhas do dataset.

head(data)

# %% [markdown]
# ## **5. Modelagem Preditiva**
# %% [markdown]
# ### **5.1 Importando bibliotecas necessárias**

# %%
# Caso não possua uma das bibliotecas importadas abaixo, a instale com um dos comandos a seguir:

install.packages(c(
    'e1071',
    'neuralnet',
    'xgboost',
    'randomForest',
    'pROC',
    'C50',
    'fastAdaboost'
))


# %%
# Importando bibliotecas.

library(e1071)
library(neuralnet)
library(xgboost)
library(randomForest)
library(pROC)
library(C50)
library(fastAdaboost)

# %% [markdown]
# ### **5.2 Criando dados de treino e de teste**
# %% [markdown]
# Para iniciar a modelagem preditiva, iremos criar os dados de treino e de teste.

# %%
# Definindo um seed.

set.seed(100)

# Criando as partições com dados de treino e de teste.

inTrain <- createDataPartition(data$medv, p = .80, list = F)

# Segmentando dados de treino e de teste por partição.

train <- data[inTrain, ]
test  <- data[-inTrain, ]

# %% [markdown]
# ### **5.3 Avaliando a importância das variáveis com o algoritmo Random Forest**

# %%
# Criando o dataframe para salvar os resultados dos modelos.

featuresRF <- data.frame()

# Definindo o número de nós e árvores a serem combinados para a criação de diferentes modelos.

nTrees <- 1:100
nNodes <- 1:100

# Define o número total de modelos a serem criados.

total <- length(nTrees) * length(nNodes)

# Define uma varíavel auxiliar para permitir o acompanhamento do progresso na avaliação dos modelos criados.

count <- 0

for(t in nTrees) {
    
  for(n in nNodes) {
    
    # Define um seed para permitir que os mesmos resultados dos experimentos sejam reproduzíveis.
    
    set.seed(100)
    
    # Cria o modelo Random Forest a ser avaliado.

    model <- randomForest(medv ~ .,
                          data       = train, 
                          ntree      = t, 
                          nodesize   = n, 
                          importance = T)

    # Realizando as predições com o modelo criado.
    
    pred <- predict(model, train[!colnames(train) %in% 'medv'])

    # Armazena os parâmetros utilizados para criação do modelo e o valor da métrica R2 obtida no dataframe.

    featuresRF <- rbind(featuresRF, data.frame(
        nodes    = n, 
        nTree    = t, 
        r2       = R2(pred, train$medv)
    ))
    
    # Incrementa o número de modelos avaliados.

    count <- count + 1

    # Imprime a porcentagem de progresso do treinamento e o melhor score R2 já alcançado.

    print(paste(100 * count / total, '%, best R2: ', max(featuresRF$r2)))
  }
}

# %% [markdown]
# Salvaremos o dataframe gerado em um arquivo CSV e imprimiremos o registro do modelo que apresentou o maior valor para a métrica R2.

# %%
# Salvando dataframe em um arquivo .csv.

write.csv(featuresRF, 'featuresRF.csv')

# Imprimindo registro do modelo que alcançou a maior acurácia.

bestRF <- featuresRF[featuresRF$r2 == max(featuresRF$r2),]

bestRF 

# %% [markdown]
# Recriaremos este modelo e imprimiremos suas estatísticas.

# %%
# Criando modelo.

set.seed(100)

model <- randomForest(medv ~ .,
                      data       = train, 
                      ntree      = bestRF[, 'nTree'], 
                      nodesize   = bestRF[, 'nodes'], 
                      importance = T)
                      
# Imprimindo o modelo.

model

# %% [markdown]
# Agora podemos plotar o modelo em um gráfico e verificar o nível de importância das variáveis do dataset para prever a variável alvo.

# %%
# Plotando gráfico para visualizar o nível de importância de cada variável no processo de predição da variável alvo.

v <- as.data.frame(varImpPlot(model))

# %% [markdown]
# Também podemos mensurar numericamente o quanto cada variável auxilia no processo de predição da variável alvo.

# %%
# Captura em ordem decrescente de nível de importância o nome das variáveis.

names <- rownames(v[order(v[, '%IncMSE'], decreasing = T),])

# Imprime o resultado.

v[order(v[, '%IncMSE'], decreasing = T),]

# %% [markdown]
# ### **5.4 Feature Selection**
# %% [markdown]
# Nesta etapa, vamos selecionar as melhores variáveis para a criação dos modelos preditivos.
# 
# Segundo o modelo Random Forest criado anteriormente, as variáveis **zn**, **rad** e **chas** foram as que apresentaram o menor índice de importância tanto para a métrica ***%IncMSE*** quanto para ***IncNodePurity*** como ilustrado no gráfico. 
# 
# **zn** e **rad** possuem forte correlações com outras variáveis preditoras o que poderá levar a problemas de *overfitting*.
# 
# Já **chas** possuí uma correlação muito fraca com todas as variáveis do dataset.
# 
# Por estes motivos, iremos desconsiderar estas 3 variáveis preditoras no processo de criação de modelos preditivos.
# 
# 

# %%
# Definindo fórmula a ser utilizada pelos modelos preditivos.

f <- medv ~ . -zn -rad - chas


# %%
# Definindo um vetor lógico indicando quais colunas devem ser ignoradas pelos modelos preditivos.

ignoreCols <- colnames(train) %in% c('zn', 'rad', 'chas', 'medv')

# %% [markdown]
# ### **5.5 Criando modelos**
# %% [markdown]
# Nesta fase iremos criar modelos baseados em diferentes algoritmos e selecionaremos aquele que obter o maior score para a métrica **R2**. 
# %% [markdown]
# #### **5.5.1 Modelo Random Forest**

# %%
# Definindo um Seed.

set.seed(100)

# Criando modelo Random Forest com as melhores configurações de estrutura encontradas na avaliação de importância 
# das variáveis.

model_rf <- randomForest(f,
                      data       = train, 
                      ntree      = bestRF[, 'nTree'], 
                      nodesize   = bestRF[, 'nodes'])
                      
# Imprimindo o modelo.

model_rf


# %%
# Realizando previsões com o modelo baseado no algoritmo Random Forest.

pred <- predict(model_rf, test[!colnames(test) %in% 'medv'])


# %%
# Calculando a métrica R2 gerada ao se utilizar o modelo.

r2_rf <- R2(pred, test$medv)

# Imprimindo o valor da métrica R2.

r2_rf

# %% [markdown]
# #### **5.5.2 Modelo Xgboost**

# %%
# Criando modelo XGboost.

  model_xgboost <- xgboost(
      data      = as.matrix(train[, ignoreCols]), # Define as variáveis preditoras.
      label     = as.matrix(train$medv), # Define a variável target.
      max.depth = 50,                    # Defie o tamanho máximo da árvore.
      eta       = 1,                     # Define a taxa de aprendizado do modelo.
      nthread   = 4,                     # Define o número de threads que devem ser usadas. 
                                         # Quanto maior for esse número, mais rápido será o treinamento.
      nrounds   = 15,                    # Define o número de iterações.
      verbose   = T,                     # Exibe a queda da taxa de erro durante o treinamento.
      eval_metric = 'rmse'               # Define a métrica de avaliação com a qual o modelo deve ser avaliado.
)


# %%
# Realizando as previsões como o modelo baseado no algoritmo XGboost.

pred <- predict(model_xgboost, as.matrix(test[, ignoreCols]))


# %%
# Calculando a métrica R2 gerada ao se utilizar o modelo.

r2_xgboost <- R2(pred, test$medv)

# Imprimindo o valor da métrica R2.

r2_xgboost

# %% [markdown]
# #### **5.5.3 Modelo neuralnet**

# %%
# Criando diversos modelos de rede neural com diferentes arquiteturas e avaliando seu nível de precisão para a 
# avaliação dos dados de teste.

# Criando um data.frame para armazenar os parâmetros utilizados em cada modelo criado.

results <- data.frame()

# Definindo o número de neurônios que devem ser utilizados em cada arquitetura de rede neural a ser criada.

neurons    <- 2:100

# Definindo diferentes taxas de aprendizagem para serem aplicadas a cada arquitetura de rede neural.

thresholds <- c(0.008, 0.009, 0.01)

# Define o número total de modelos a ser criado.

total <- length(neurons) * length(thresholds)

# Define um variável para contabilizar o número de modelos já criado.

count <- 0

# Definindo um loop para percorrer o número de neurônios que cada modelo criado deve possuir.

for(n in neurons) {
  
  # Definindo um loop para aplicar diferentes taxas de aprendizagem a cada arquitetura de rede neural criada.

  for(r in thresholds) {
    
    # Define um seed para permitir que os resultados gerados possam ser reproduzíveis.

    set.seed(100)
    
    # Cria o modelo baseado no algoritmo neuralnet.

    nn <- neuralnet(f, data = train, hidden = c(n), linear.output = T, threshold = r)
    
    # Realizando as previsões para os dados de teste.

    pre <- predict(nn, test)

    # Calculando o valor da métrica R2.

    r2 <- R2(pre, test$medv)
    
    # Armazenando os resultados no dataset.

    results <- rbind(results, data.frame(n = n, threshold = r, r2 = r2))
    
    # Incrementando o número de modelos criados.

    count <- count + 1

    # Imprimindo uma mensagem com o progresso dos treinamento dos modelos.

    print(paste(100 * count / total, '%, best R2: ', max(results$r2)))
  }
}

# %% [markdown]
# Salvaremos o dataframe gerado em um arquivo CSV e imprimiremos o registro do modelo que apresentou o maior valor para a métrica R2.

# %%
# Salvando dataframe em um arquivo .csv.

write.csv(featuresRF, 'featuresNN.csv')

# Imprimindo registro do modelo que alcançou o maior score para a métrica R2.

bestNN <- results[results$r2 == max(results$r2),]

bestNN

# %% [markdown]
# #### **5.5.4 Selecionando o melhor modelo criado**
# %% [markdown]
# Determinaremos qual modelo teve o melhor desempenho para o conjunto de dados de teste a partir do score da métrica **R2** (*R-squared*) gerados por cada um.

# %%
# Criando um vetor com os scores para a métrica R2 obtidos por cada modelo.

r2Models <- c(
    randomForest = r2_rf,
    xgboost      = r2_xgboost,
    neuralnet    = bestNN$r2
)

# Determinando o modelo com o maior score R2.

head(sort(r2Models, decreasing = T), 1)

# %% [markdown]
# Finalizamos esta análise concluindo que o algoritmo **XGboost** foi aquele que obteve a melhor performance para a métrica **R2** em relação aos demais algoritmos implementados com um score de:
# 
# * **XGboost:** 0.999308948695104
# %% [markdown]
# ## **Entre em contato comigo!**
# %% [markdown]
# Caso tenha alguma dúvida, sugestão ou apenas queira trocar uma ideia sobre este projeto, não hesite em entrar em contato comigo!
# %% [markdown]
# <table align='center'>
#     <tr style=" background-color: rgba(0,0,0,0);">
#         <td>
#             <img src="https://image.flaticon.com/icons/svg/732/732026.svg" alt="Email" height="42" width="42">
#         </td>
#         <td>
#             <p>
#                 <a href="mailto:clauder.noronha@gmail.com">E-mail</href>
#             </p>
#         </td>
#     <td>
#             <img src="https://image.flaticon.com/icons/svg/254/254394.svg" alt="Linkedin" height="42" width="42">
#     </td>
#     <td>
#         <p>
#             <a href="https://www.linkedin.com/in/clauder-noronha-912b6655/">Linkedin</href>
#         </p>
#     </td>
#     <td>
#         <img src="https://image.flaticon.com/icons/svg/2111/2111432.svg" alt="Github" height="42" width="42">
#     </td>
#     <td>
#         <p>
#             <a href="https://github.com/CLAUDERNORONHA">Github</href>
#         </p>
#     </td>
#     <td>
#         <img src="https://image.flaticon.com/icons/svg/1667/1667233.svg" alt="Site" height="42" width="42">
#     </td>
#     <td>
#         <p>
#             <a href="https://claudernoronha.github.io/cn/">Site</href>
#         </p>
#     </td>
#   </tr>
# </table>
# 
