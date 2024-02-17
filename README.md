# MODELO DE PREVISÃO APLICADO NO CONSUMO DE SERVIÇOS DE NUVEM

Uma empresa oferece serviços de Cloud, tanto de infraestrutura quanto plataforma de serviço, IaaS e PaaS. 

Por meio de compra de créditos os clientes tem acesso a esses serviços.

Nesse estudo temos a análise do comportamento do consumo dos créditos em nuvem adquiridos pelos clientes ao longo do tempo.  

A previsão de uma série temporal é uma abordagem estatística ou de machine learning que usa os dados históricos para fazer previsões sobre determinados pontos de tempos futuros. 

## Ferramentas
A manipulação, análise e modelo de previsão foram desenvolvidos na **linguagem R** de programação estatística utilizando o **R Studio.** 
O script do modelo está disponível em "Script Temporal Trabalho.R"

## Base de Dados
O conjunto de dados utilizados no modelo está disponível no arquivo "Consumo.xlsx". 

## Resultados
Apesar do histórico em análise seja pequeno (02 anos), o melhor modelo obtido foi o de **Tendência Quadrática** obtendo a melhor performance na avaliação através dos MAPEs resultantes. 
Relatório completo do estudo está disponível em "Modelo de Previsão no Consumo de Serviços de Nuvem.pdf"
