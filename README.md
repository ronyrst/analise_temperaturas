# Análise de temperaturas máximas e mínimas
aplicado à cidade de São Paulo no período entre 1961 e 2015.

O código aqui presente fez parte do projeto de Cultura e Extensão realizado na Universidade de São Paulo junto à professora Rita Ynoue (do Instituto de Astronomia, Geofísica e Ciências Atmosféricas - IAG USP) entre os anos de 2016 e 2017.

O objetivo do projeto era descobrir se os dias frios em Sâo Paulo estavam diminuindo em quantidade.
Isto foi feito utilizando a metodologia criada por Estévez et al. (2012), baseada nas duas *Tabelas* abaixo:

<img src="Imagens/1.tabelaprojeto.jpg" height=260>

As tabelas de entrada podem ser de dois tipos.

1. Estrutura do observatório do **IAG** (com uma tabela para as temperaturas máximas e outra para as mínimas):

<table>
  <thead>
    <th>DATA</th>
    <th>max(tmax)</th>
  </thead>
  <tbody>
    <tr>
      <td>1961-01-01 14:00:00</td>
      <td>26.4</td>
    </tr>
  </tbody>
</table>


<table>
  <thead>
    <th>DATA</th>
    <th>min(tmin)</th>
  </thead>
  <tbody>
    <tr>
      <td>1961-01-01 14:00:00</td>
      <td>19</td>
    </tr>
  </tbody>
</table>


2. Estrutura do observatório de **Mirante de Santana** (os valores de *Precipitação* e *Hora* não são utilizados):

| Estacao | Data       | Hora | Precipitacao | TempMaxima | TempMinima |
|---------|------------|------|--------------|------------|------------|
| 83781   | 01/01/1961 | 0000 |              | 24.9       |            |
| 83781   | 01/01/1961 | 1200 | 0            |            | 18.5       |
| 83781   | 02/01/1961 | 0000 |              | 22.7       |            |


Com elas, é possível fazer a análise das temperaturas através da criação de matrizes. Como no exemplo abaixo, referente à saída do programa para os dados do observatório do IAG entre 1961 e 2015 (em formato *.csv*):

<img src="Imagens/5.exemplo.jpg">

O código permite fazer diversas alterações nas matrizes de saída: separação anual, a cada 5 anos, 10 anos e 30. E também a possibilidade de adicionar as quatro estações (verão, outono, inverno, primavera) para cada período de tempo.

Neste caso específico, apresento os valores para os verões entre 1961 e 1965 e para os verões entre 2011 e 2015 (o arquivo recebeu um breve tratamento). É perceptível o aumento de dias quentes entre os dois períodos):

<img src="Imagens/6.tabela5.jpg">

Também é possível apresentar as matrizes de forma gráfica. O *Gráfico 2* apresenta outro observatório de São Paulo.

<img src="Imagens/8.grafico1.jpg">
<img src="Imagens/9.grafico2.jpg">


<h3>Sobre o código.</h3>

É exigido que o arquivo *dias.txt* esteja na pasta que o código rodará.

O arquivo do código possui:
<ol>
  <li> Uma breve explicação de cada função presente no código. </li>
  <li> As funções propriamente ditas, separadas em 3 partes </li>
  <li> Um breve tutorial da funcionalidade do código todo, separado em 4 passos. </li>
</ol>


<h3>Referência:</h3>

Estévez, David Martín, Luis Bartolomé Lecha Estela, Jorge Olcina Cantos, Pablo Fernández de Arróyabe (2012). CLASIFICACIÓN COMPLEJA Y OBJETIVA DE LOS ESTADOS DEL TIEMPO DIARIOS SEGÚN LA ESTRUCTURA DEL RÉGIMEN TÉRMICO DEL AIRE Y OTROS INDICADORES BIOMETEOROLÓGICOS. Conference: VIII Congreso de la Asociación Española de Climatología, At Salamanca, España.
