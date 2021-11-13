# Pokemon com estatisticas
# Este conjunto de dados inclui 721 Pokémon,
# incluindo seu número, nome, primeiro e segundo tipo e estatísticas básicas:
# HP, Ataque, Defesa, Ataque Especial, Defesa Especial e Velocidade.
# Tem sido de grande utilidade no ensino de estatística para crianças.
# Com certos tipos, você também pode dar uma introdução geek ao aprendizado de máquina.

# O dataset não possui uma variavel alvo bem definida para ser prevista,
# entao, foi decidido vamos tentar prever os pontos de defesa
# conforme os seus pontos de ataque, resultando em uma REGRESSÃO LINEAR SIMPLES

# The data as described by Myles O'Neill is:

# #: ID for each pokemon

# Name: Name of each pokemon

# Type 1: Each pokemon has a type, this determines weakness/resistance to attacks

# Type 2: Some pokemon are dual type and have 2

# Total: sum of all stats that come after this, a general guide to how strong a pokemon is

# HP: hit points, or health, defines how much damage a pokemon can withstand before fainting

# Attack: the base modifier for normal attacks (eg. Scratch, Punch)

# Defense: the base damage resistance against normal attacks

# SP Atk: special attack, the base modifier for special attacks (e.g. fire blast, bubble beam)

# SP Def: the base damage resistance against special attacks

# Speed: determines which pokemon attacks first each round

# Por que esse dataset?
# Simples, porque eu sou apaixonado pela franquia pokemon,
# joguei alguns jogos e assisti muito as temporadas classicas quando passava na redeTV,
# achei esse dataset por acaso entao vi uma otima oportunidade para analisa-lo

setwd("~/PROJETOS/CIENTISTA_DE_DADOS/MATERIA_CIENCIA_DE_DADOS/pokemon")


# Importando pacotes
library(ggplot2)
library(caret)
library(dplyr)
library(cowplot)

# Leitura de dados
df <- read.csv('Pokemon.csv')

str(df)
View(df)
df

df$X. <- NULL
View(df)


# Descritivo e exploração de dados com groupbys


# Análise Exploratória e Figuras que mostrem insights sobre os dados com GGPLOT2

tipo1 <- ggplot(df,aes(y = Type.1, fill = Type.1)) +
  geom_bar()

tipo2 <- ggplot(df,aes(y = Type.2, fill = Type.2)) +
  geom_bar()

plot_grid(tipo1, tipo2)

#


# Os tipos de pokemons mais registrados nesse dataset são do tipo agua e tipo normal,
# e a sua minoria é do tipo voador e fada

# A imensa maioria dos pokemons nao tem um segundo tipo,
# mas uma boa quando tem um tipo segundario é do tipo voador ou psicico


#

boxAtt <- ggplot(df, aes(x = Type.1, y = Attack, fill = Type.1)) +
  geom_boxplot()

boxDef <- ggplot(df, aes(x = Type.1, y = Defense, fill = Type.1)) +
  geom_boxplot()

plot_grid(boxAtt, boxDef)

#


# Os graficos nos mostram que em relação ao ataque, temos 12 valores outliers,
# E na defesa temos um total de 11


#

generation <- ggplot(df, aes(x = Generation, fill = Generation)) +
  geom_bar()

legendary <- ggplot(df,aes(x = Legendary, fill = Legendary)) +
  geom_bar()

plot_grid(generation, legendary)

#


# As gerações 1, 3, 5 são as que mais contem pokemons nesse conjunto de dados
# E para a surpresa de 0 pessoas, a minoria dos pokemons sao lendarios


#

Att <- ggplot(df, aes(x = Type.1, y = Attack, fill = Type.1)) +
  stat_summary(fun = "mean", geom = "bar")

Def <- ggplot(df, aes(x = Type.1, y = Defense, fill = Type.1)) +
  stat_summary(fun = "mean", geom = "bar")


plot_grid(Att, Def)

#


# Em relação ao ataque, foi observados que pokemons do tipo DRAGÃO tem a maior média em relação ao seu atributo ofensivo
# e do outro lado, as FADAS em média sao as mais fracas ofensivamente

# Em relação a defesa, os pokemons to tipo AÇO lideram essa estatistica com uma certa folga,
# em seguida aparecem os pokemons do tipo ROCHA/PEDRA e os pokemons do tipo NORMAL são os mais fracos


#

SPAtt <- ggplot(df, aes(x = Type.1, y = Sp..Atk, fill = Type.1)) +
  stat_summary(fun = "mean", geom = "bar")

SPDef <- ggplot(df, aes(x = Type.1, y = Sp..Def, fill = Type.1)) +
  stat_summary(fun = "mean", geom = "bar")


plot_grid(SPAtt, SPDef)

#


# Os tipos voador, dragão e psiquico sao os que possuem maior média de pontos de ataques especiais

# Em relaçao a defesa especial,
# os dragões estao nas cabeças novamente, mas nao muito distante estao os psiquicos e fadas


#


HP <- ggplot(df, aes(x = Type.1, y = HP, fill = Type.1)) +
  stat_summary(fun = "mean", geom = "bar")

Speed <- ggplot(df, aes(x = Type.1, y = Speed, fill = Type.1)) +
  stat_summary(fun = "mean", geom = "bar")


plot_grid(SPAtt, SPDef)

#

# E pra nao perder o costume,
# o tipo dragao novamente esta na liderança de alguma estatistica
#e dessa vez é nos pontos de vida e sao seguidos pelos tipos normais

# Em relaçao a velocidade,
# o tipo voador é quem lidera de forma disparada essa estatistica


#

AttLeng <- ggplot(df, aes(x = Legendary, y = Attack, )) +
  stat_summary(fun = "mean", geom = "bar")

DefLeng <- ggplot(df, aes(x = Legendary, y = Defense, )) +
  stat_summary(fun = "mean", geom = "bar")


SPAttLeng <- ggplot(df, aes(x = Legendary, y = Sp..Atk, )) +
  stat_summary(fun = "mean", geom = "bar")


SPDefLeng <- ggplot(df, aes(x = Legendary, y = Sp..Def, )) +
  stat_summary(fun = "mean", geom = "bar")


HPLeng <- ggplot(df, aes(x = Legendary, y = HP,)) +
  stat_summary(fun = "mean", geom = "bar")


SpeedLeg <- ggplot(df, aes(x = Legendary, y = Speed,)) +
  stat_summary(fun = "mean", geom = "bar")

plot_grid(AttLeng,DefLeng, SPAttLeng,SPDefLeng, HPLeng, SpeedLeg )

#


# E sempre nenhum tipo de surpresa, os pokemons lendarios mostram porque sao os mais raros

# Voce deve ta se perguntando:
# "Pablo, porque voce nao fez a mesma coisa quando o eixo X era o Tipo 1 dos pokemons?",
# e eu respondo, eu nao fiz tal ato pois em cada grafico tinha uma informação diferente a ser comentada,
# entao para cada dupla de graficos, foi extraido uma informaçao diferente.
# Agora em relaçao ao lendarios, quando fizemos o "groupby",
# por ter poucos numeros,
# deu pra ver claramente que eles liderariam todas as estatisticas


#

plot1 <- ggplot(df, aes(x = Generation, y = Attack, )) +
  stat_summary(fun = "mean", geom = "bar")

plot2 <- ggplot(df, aes(x = Generation, y = Defense, )) +
  stat_summary(fun = "mean", geom = "bar")


plot3 <- ggplot(df, aes(x = Generation, y = Sp..Atk, )) +
  stat_summary(fun = "mean", geom = "bar")


plot4 <- ggplot(df, aes(x = Generation, y = Sp..Def, )) +
  stat_summary(fun = "mean", geom = "bar")


plot5 <- ggplot(df, aes(x = Generation, y = HP,)) +
  stat_summary(fun = "mean", geom = "bar")


plot6 <- ggplot(df, aes(x = Generation, y = Speed,)) +
  stat_summary(fun = "mean", geom = "bar")

plot_grid(plot1, plot2, plot3, plot4, plot5, plot6)


#


# Ha um bom equilibrio em todas as estatisticas entre as geraçoes,
# mas a geração 4 se mostra mais forte,
# pois lidera a média de HP, ataque, defesa, ataque e defesa especiais,
# so perdendo na velocidade


#

Sub2_Scatter_Plots <-  function(x, y, z, df){
  plt1 <- ggplot(df, aes(x = x, y = z,)) + geom_point()
  plt2 <- ggplot(df, aes(x = y, y = z,)) + geom_point()

  plot_grid(plt1, plt2)
}


Sub2_Scatter_Plots(df$Attack, df$Defense, df$HP, df)





















# Quebrar os dados em conjunto de treino e conjunto de teste



# Treinar um estimador do caret de aprendizagem supervisionada (regressor ou classificador) no conjunto de treino




# Verificar o desempenho do estimador do caret no conjunto de teste

