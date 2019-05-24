
<!-- README.md foi gerado por README.Rmd -->

# Análise Descritiva

Pacote de R para análise descritiva.

## Instalação

``` r
install.packages("devtools")
devtools::install_github("elemosjr/analisedescritiva")
```

## Exemplos

``` r
library(analisedescritiva)

conjunta_bar_cd(iris)
```

![](man/figures/README-example-1.png)<!-- -->

# To Do

  - Fork xda ou criar funções parecidas.
  - Adicionar outros tipos de função de grafico (univariada e bivariada
    com outras geometrias)
  - Função que identifica variaveis de tempo e/ou series temporais.
  - Criar função de série temporais.
  - Adicionar função que cria grid de fact\_wraps a partir de uma dada
    variavel
  - Adicionar sumarios de dados categoricos e numericos.
  - Adicionar opções de fatores e de NA’s.
  - Adicionar opção de temas em funções de plot.
  - Quando criada função de sumario criar opção de exportação para
    latex…
  - Estudar forma de fazer com que plot\_all classifique automaticamente
    qual plot fazer para var continua
