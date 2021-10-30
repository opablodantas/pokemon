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

#Type 1: Each pokemon has a type, this determines weakness/resistance to attacks

#Type 2: Some pokemon are dual type and have 2

#Total: sum of all stats that come after this, a general guide to how strong a pokemon is

#HP: hit points, or health, defines how much damage a pokemon can withstand before fainting

#Attack: the base modifier for normal attacks (eg. Scratch, Punch)

#Defense: the base damage resistance against normal attacks

#SP Atk: special attack, the base modifier for special attacks (e.g. fire blast, bubble beam)

#SP Def: the base damage resistance against special attacks

#Speed: determines which pokemon attacks first each round

# Por que esse dataset?
# Simples, porque eu sou apaixonado pela franquia pokemon,
# joguei alguns jogos e assisti muito as temporadas classicas quando passava na redeTV,
# achei esse dataset por acaso entao vi uma otima oportunidade para analisa-lo


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
tipo1 <- ggplot(df,aes(x = Type.1, fill = Type.1)) +
  geom_bar()

tipo2 <- ggplot(df,aes(x = Type.2, fill = Type.2)) +
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
#ggplot(df) +
 # geom_boxplot(mapping = aes(x = Type.1, y = Attack))


# Quebrar os dados em conjunto de treino e conjunto de teste



# Treinar um estimador do caret de aprendizagem supervisionada (regressor ou classificador) no conjunto de treino




# Verificar o desempenho do estimador do caret no conjunto de teste

