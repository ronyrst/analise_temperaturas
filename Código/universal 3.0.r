#### FUNCIONALIDADES (a partir da linha 33):
# -'save_data' salva matriz, data.frame ou lista em um arquivo .csv.
# -'ctrab_na' para uso interno dos outros programas.
# -'ctrab_cons' para uso interno dos outros programas.
# -'confere_na' mostra quantos NAs existem em um data.frame já na estrutura de convert_universal, os salvando em um arquivo .csv.
# -'confere_cons' mostra quantas inconsistências há em um data.frame já na estrutura de convert_universal, os salvando em um arquivo .csv.
# -'plot_temps' gera gráfico das temperaturas máximas e mínimas.
# -'graficaliza' gera gráfico(s) de barras das matrizes já calculadas.
# -'checa_data' verifica se a data usada é do tipo YYYY-MM-DD ou DD/MM/YYYY.
# -'def_dia' recebe um string de data e o transforma em uma lista que separa dia, mês e ano em valores numéricos.
# -'compara_data' faz comparação lógica entre duas datas.
# -'separa_estacao' coloca a estação em cada uma das datas de um data.frame.

#### CONVERTER DADOS (a partir da linha 1013):
# -'convert_universal' função que converte as estruturas do IAG e de Mirante para a estrutura aceita pelas funções aqui descritas.

#### MATRIZES (a partir da linha 1171):
# -'define_minima' para uso interno de separa_temp.
# -'define_maxima' para uso interno de separa_temp.
# -'separa_temp' calcula a matriz de máximas e mínimas de um data.frame na estrutura de convert_universal.
# -'quinquenal' calcula matrizes de 5 em 5 anos, ou sazonalmente em cada período de 5 anos, com o data.frame na estrutura de convert_universal.
# -'sazonal' calcula a matriz para cada uma das quatro estações de um data.frame na estrutura de convert_universal.
# -'decenio' calcula a matriz de 10 em 10 anos, ou sazonalmente em cada período de 10 anos, com o data.frame na estrutura de convert_universal.
# -'trinta' calcula a matriz em períodos de 30 anos, ou sazonalmente dentro de cada período de 30 anos, com o data.frame na estrutura de convert_universal.
# -'anual'calcula a matriz ano a ano, ou sazonalmente em cada ano, com o data.frame já na estrutura de convert_universal.

#### TUTORIAL (a partir da linha 1865):
# apresenta uma forma rápida de uso dos programas abaixo.


########################################################

# F U N C I O N A L I D A D E S

########################################################
save_data <- function(dado, titulo = ""){
  #####
  # Entra com uma matriz, data.frame ou lista (com matriz e/ou data.frame), e com um título (do tipo string, opcional), e salva em arquivo no computador.
  # 
  #####
  
  if(is.matrix(dado)){ # se o dado é uma matriz, salva aqui.
    op <- paste(titulo, " matriz.csv", sep = "")
    write.csv2(dado, file = op)
    
  } else if(is.data.frame(dado)){ # se o dado é um data.frame, salva aqui.
    op <- paste(titulo, " data_frame.csv", sep = "")
    write.csv2(dado, file = op, quote = F)
    
  } else if(is.list(dado)){ # se é lista, salva aqui.
    guard <- names(dado)
    
    for(i in 1:length(dado)){
      op <- paste(i, ". ", titulo, " ", guard[i], ".csv", sep = "") # código que define o título do arquivo. Fica, p/ ex: "1.  Verao.csv".
      
      if(is.matrix(dado[[i]])){
        write.csv2(dado[[i]], file = op)
      } else if(is.data.frame(dado[[i]])){
        write.csv2(dado[[i]], file = op, quote = F)
      } else {
        print(paste("A entrada de número", i, "não é nem uma matriz nem um data.frame. Portanto, não foi salva.", sep = " "))
      }
    }
  } else {
    print("O dado entrado não é uma matriz, data.frame ou lista.")
    return(NULL)
  }
}

########################################################
ctrab_na <- function(dado, salva = F, altera = F){
  #####
  # Entra com um data.frame já na estrutura de convert_universal, e checa se há entradas NA nas colunas de máximas e de mínimas.
  #  Ele necessariamente retorna o mesmo data.frame, mas com ou sem alterações.
  #  Se o programa não encontrou NAs no data.frame, ele retorna o mesmo data.frame de entrada.
  #  Se o programa encontrou NAs e 'altera' = FALSE, o programa também retorna o mesmo data.frame de entrada.
  #
  # 'salva': booleano. Define se as linhas onde foram encontrados NAs são salvas ou não em um arquivo .csv à parte.
  #  Se TRUE: as linhas onde foram encontrados NAs são colocadas em um data.frame à parte e salvas.
  #  Se FALSE: nada é feito.
  #
  # 'altera': booleano. Define se as linhas que possuem NA são ou não retiradas do data.frame que retorna da função.
  #  Se TRUE: as linhas são retiradas do data.frame 'dado', e retornadas.
  #  Se FALSE: as linhas permanecem no data.frame 'dado'. Nada é feito.
  #####
  
  contador <- 0 # guarda quantos NAs o data.frame possui.
  opera <- c() # guarda as posições dos NAs encontrados no data.frame.
  for( i in 1:nrow(dado) ){
    if(is.na(dado$tmin[i]) | is.na(dado$tmax[i])){ # se ou a coluna de mínimas ou a coluna de máximas tiver NA, entra aqui.
      contador <- contador + 1
      opera <- c(opera, i)
    }
  }
  
  if(contador == 0){ # se contador for igual a zero, o for acima não encontrou NAs, então nada é feito, e se retorna o data.frame original.
    return(dado)
  }
  
  if(salva == T){ # se salva é definido por TRUE, o programa faz o caminho de salvar as linhas com NA presente.
    
    perdidos <- data.frame() # isso é feito por esse data.frame.
    for( i in opera ){
      perdidos <- rbind(perdidos, dado[i,]) # rbind coloca a linha de dado dentro de perdidos.
    }
    save_data(perdidos, "NAs encontrados") # o data.frame finalizado é então salvo usando a função save_data.
  }
  
  if(altera == T){ # se a opção altera for TRUE, é retirado do data.frame original as linhas em que há NAs presentes.
    dado <- dado[-opera,]
  }
  return(dado)
}

########################################################
ctrab_cons <- function(dado, salva = F, altera = F){
  #####
  # Entra com um data.frame já na estrutura de convert_universal, e checa se há consistência nos dados: se as mínimas são maiores ou iguais às máximas.
  #  Ele necessariamente retorna o mesmo data.frame, mas com ou sem alterações.
  #  Se o programa não encontrou inconsistências no data.frame, ele retorna o mesmo data.frame de entrada.
  #  Se o programa encontrou inconsistências e 'altera' = FALSE, o programa também retorna o mesmo data.frame de entrada.
  #  O programa não faz nada ao encontrar temperaturas NA.
  #
  # 'salva': booleano. Define se as linhas onde foram encontradas inconsistências são salvas ou não em um arquivo .csv à parte.
  #  Se TRUE: as linhas onde foram encontradas inconsistências são colocadas em um data.frame à parte e salvas.
  #  Se FALSE: nada é feito.
  #
  # 'altera': booleano. Define se as linhas que possuem inconsistências são ou não retiradas do data.frame que retorna da função.
  #  Se TRUE: as linhas são retiradas do data.frame 'dado', e retornadas.
  #  Se FALSE: as linhas permanecem no data.frame 'dado'. Nada é feito.
  #####
  
  contador <- 0 # guarda quantas linhas inconsistentes o data.frame possui.
  opera <- c() # guarda as posições das linhas inconsistentes encontradas no data.frame.
  
  for( i in 1:nrow(dado) ){
    if( is.na(dado$tmin[i]) | is.na(dado$tmax[i]) ){
      next
      
    } else if( dado$tmin[i] >= dado$tmax[i] ){ # se o valor do dia da mínima for maior que o da máxima, entra aqui.
      contador <- contador + 1
      opera <- c(opera, i)
    }
  }
  
  if(contador == 0){ # se contador for igual a zero, o for acima não encontrou inconsistências, então nada é feito, e se retorna o data.frame original.
    return(dado)
  }
  
  if(salva == T){ # se salva é definido por TRUE, o programa faz o caminho de salvar as linhas com inconsistências.
    
    perdidos <- data.frame() # perdidos é um data.frame criado, onde são salvas as linhas de inconsistências.
    for( i in opera ){
      perdidos <- rbind(perdidos, dado[i,]) # rbind coloca a linha de dado dentro de perdidos.
    }
    save_data(perdidos, "Dados inconsistentes") # o data.frame finalizado é então salvo usando a função save_data.
  }
  
  if(altera == T){ # se a opção altera for TRUE, são retiradas do data.frame original as linhas inconsistentes.
    dado <- dado[-opera,]
  }
  return(dado)
}

########################################################
confere_na <- function(dado){
  #####
  # Entra com um data.frame já na estrutura de convert_universal, escreve um arquivo .csv, caso foram achados NAs no data.frame, ou printa
  #  uma mensagem, caso nenhuma linha com NAs foi encontrada.
  #  O programa não retorna nada.
  #####
  conf <- ctrab_na(dado, salva = T, altera = T) # faz uso da função 'ctrab_na' - função essa, que se usada em contextos errados, pode fazer outros
                                                #  programas retornarem erros.
                                                # para que isso não ocorra, é usada essa função 'confere_na', que garante que o usuário a use 
                                                #  indiscriminadamente, sem maiores perigos à integridade dos outros programas.
  
  if(nrow(dado) == nrow(conf)){
    print("O data.frame entrado não possui linhas com NA.") # sai o print se não forem encontrados NAs no data.frame.
  }
  rm(conf)
}

########################################################
confere_cons <- function(dado){
  #####
  # Entra com um data.frame já na estrutura de convert_universal, escreve um arquivo .csv, caso sejam encontradas inconsistências no data.frame,
  #  ou printa uma mensagem, caso nenhuma linha com inconsistências foi encontrada.
  #  O programa não retorna nada.
  #####
  conf <- ctrab_cons(dado, salva = T, altera = T) # faz uso da função 'ctrab_cons' - função essa, que se usada em contextos errados, pode fazer outros
                                                  #  programas retornarem erros.
                                                  # para que isso não ocorra, é usada essa função 'confere_cons', que garante que o usuário a use 
                                                  #  indiscriminadamente, sem maiores perigos à integridade dos outros programas.
  
  if(nrow(dado) == nrow(conf)){
    print("O data.frame entrado não possui inconsistências.") # sai o print se não foram encontradas inconsistências no data.frame.
  }
  rm(conf)
}

########################################################
plot_temps <- function(dados, titulo = " "){
  #####
  # Entra com um data.frame de temperaturas máximas e mínimas, e plota um gráfico das temperaturas.
  #
  # 'titulo': string com texto que descreve o gráfico, a entrar como subtítulo no plot. 
  #####
  
  copia <- ctrab_na(dados, altera = T)
  i <- nrow(dados) - nrow(copia)
  
  mini <- min(copia[[2]]) # encontra a temperatura máxima e a mínima observadas.
  maxi <- max(copia[[3]]) # espera-se que a menor mínima esteja no data.frame de mínimas, e a maior máxima no de máximas.
                          # levando em conta que o data.frame 'copia' tem a estrutura 'Data' 'tmin' 'tmax'. copia[[2]] é o 'tmin', copia[[3]] é o 'tmax'.
  
  if(i == 0){
    
    plot(copia[[3]], col = "red", type = "l", ylim = c(mini, maxi), xlab = "Dias do período", ylab = "Temperatura", sub = titulo)
    points(copia[[2]], col = "blue", type = "l")
  } else{
    
    newtitulo <- paste(titulo, " (o data.frame original tinha ", i, " linhas com valores NA, que foram omitidos)", sep = "")
    plot(copia[[3]], col = "red", type = "l", ylim = c(mini, maxi), xlab = "Dias do período", ylab = "Temperatura", sub = newtitulo)
    points(copia[[2]], col = "blue", type = "l")
  }
}

########################################################
graficaliza <- function(dado, sazonal = F){
  #####
  # Entra com matriz ou lista de matrizes, plota gráfico(s) segundo o modelo de Estévez, indicando a quantidade de dias em cada classificação de temperatura
  #  do período presente na matriz. A classificação dos dias se dá por: "Muito frio", "frio", "fresco", "ameno", "quente", "muito quente" e "sufocante".
  #
  # 'sazonal': se TRUE, plota gráficos separando as estações, contando que a lista de matrizes já possua as estações separadas. Se a lista original não possuir
  #  as estações já separadas, o resultado não será correto (e possivelmente, nenhum erro será mostrado). Caso entre TRUE, mas 'dado' for apenas uma matriz, 
  #  nada é feito, e só um gráfico é gerado.
  #   Se FALSE (default), só plota um gráfico.
  #####
  
  ###
  if( is.matrix(dado) ){ # se o dado entrado for só uma matriz.
    m <- matrix(rep(0, len = 14), nrow = 7, dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"))))
    # gera uma matriz de zeros, com 7 linhas, e nesse caso, 2 colunas.
    
    muitofrio <- as.numeric(dado[6,1]) + sum(dado[7,1:2]) # nesse conjunto de linhas, os valores de cada classificação são somados, dos presentes na matriz.
    frio <- sum(dado[4:5,1]) + sum(dado[5:6,2]) + sum(dado[6:7,3]) # essa estrutura se repete no código mais vezes.
    fresco <- sum(dado[3:4,2]) + sum(dado[4:5,3]) + sum(dado[5:6,4])
    ameno <- sum(dado[2:3,3]) + sum(dado[3:4,4]) + sum(dado[4:5,5])
    quente <- sum(dado[1:2,4]) + sum(dado[2:3,5]) + sum(dado[3:4,6]) + as.numeric(dado[4,7])
    muitoquente <- as.numeric(dado[1,5]) + sum(dado[1:2,6]) + sum(dado[2:3,7])
    sufocante <- as.numeric(dado[1,7])
    
    m[,1] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante) # a matriz 'm' recebe os valores de cada classificação.
    
    barplot(m, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias", # a matriz é plotada.
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"))
    
  ###
  } else if( is.list(dado) & sazonal == F ){ # caso de que os dados entrados estão em uma lista, e sazonal é FALSE (só um gráfico é plotado).
    
    ops <- ceiling(length(dado)/0.7) - length(dado) # ceiling arredonda um possível valor quebrado para o menor inteiro maior que o número.
                                                    # ou seja: 7.2 > 8,
                                                    #          7.8 > 8.
                                                    # length(dado)/0.7 é feito para se gerar, no total, uma matriz com 30% de colunas de zeros, o suficiente
                                                    # para que a legenda não tampe valor algum.
    contornos <- c(as.vector(names(dado)), rep(" ", ops)) # contornos é feito para criar "títulos" vazios para os 30% de colunas de zeros.
    
    m <- matrix(rep(0, len = (7*ceiling(length(dado)/0.7))), nrow = 7, # matriz é gerada.
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), contornos)))
    coluna <- 1
    
    for( i in dado ){ # itera em cada matriz presente em 'dado'.
      muitofrio <- as.numeric(i[6,1]) + sum(i[7,1:2])
      frio <- sum(i[4:5,1]) + sum(i[5:6,2]) + sum(i[6:7,3])
      fresco <- sum(i[3:4,2]) + sum(i[4:5,3]) + sum(i[5:6,4])
      ameno <- sum(i[2:3,3]) + sum(i[3:4,4]) + sum(i[4:5,5])
      quente <- sum(i[1:2,4]) + sum(i[2:3,5]) + sum(i[3:4,6]) + as.numeric(i[4,7])
      muitoquente <- as.numeric(i[1,5]) + sum(i[1:2,6]) + sum(i[2:3,7])
      sufocante <- as.numeric(i[1,7])
      
      m[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante)
      coluna <- coluna + 1
    }
    
    barplot(m, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , # plota a matriz criada.
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"))
    
  ###
  } else if( is.list(dado) & sazonal == T ){ # caso em que os dados entrados estão em uma lista, e a sazonal é TRUE.
    
    o <- 1
    cont_ver <- c() # cria as "labels" para cada barra, no gráfico final. As "labels" são separadas por estação.
    cont_out <- c()
    cont_inv <- c()
    cont_pri <- c()
    for( i in as.vector(names(dado)) ){ # itera nos nomes das matrizes, presentes na lista.
      if( o == 1){
        o <- 2
        cont_ver <- c(cont_ver, i)
      } else if( o == 2 ){
        o <- 3
        cont_out <- c(cont_out, i)
      } else if( o == 3 ){
        o <- 4
        cont_inv <- c(cont_inv, i)
      } else if( o == 4 ){
        o <- 1
        cont_pri <- c(cont_pri, i)
      }
    }
    
    total <- length(dado)/4 # o "tamanho" de cada estação, do total das presentes na lista.
    if( total != round(total) ){ # se total for um valor quebrado, implica que não foi entrada uma lista com estações separadas.
                                 # é o único erro que pode ser acusado, caso se entre com dados incorretamente.
                                 # convém notar que: se os dados não forem de estações separadas, mas forem divisíveis por 4, nada ocorre aqui.
      print("Os dados entrados não condizem com o esperado.")
      return(NULL)
    }
    ops <- ceiling(total/0.7) - total # ceiling( total/0.7 ) possui a mesma argumentação de acima. Com os 30% de colunas vazias.
    
    cont_ver <- c(cont_ver, rep(" ", ops)) # adiciona as "labels" vazias, para os 30% de barras vazias geradas.
    cont_out <- c(cont_out, rep(" ", ops))
    cont_inv <- c(cont_inv, rep(" ", ops))
    cont_pri <- c(cont_pri, rep(" ", ops))
    
    m_verao <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, # 4 matrizes são geradas, uma para cada estação.
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_ver)))
    m_outono <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, 
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_out)))
    m_inverno <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, 
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_inv)))
    m_primavera <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, 
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_pri)))
    
    qual <- 1 # 'qual' indica qual estação é a atual.
    coluna <- 1 # 'coluna' itera na... coluna das matrizes. É a coluna onde os dados devem ser entrados.
    for( i in dado ){ # itera nas matrizes presentes na lista 'dado'.
      muitofrio <- as.numeric(i[6,1]) + sum(i[7,1:2])
      frio <- sum(i[4:5,1]) + sum(i[5:6,2]) + sum(i[6:7,3])
      fresco <- sum(i[3:4,2]) + sum(i[4:5,3]) + sum(i[5:6,4])
      ameno <- sum(i[2:3,3]) + sum(i[3:4,4]) + sum(i[4:5,5])
      quente <- sum(i[1:2,4]) + sum(i[2:3,5]) + sum(i[3:4,6]) + as.numeric(i[4,7])
      muitoquente <- as.numeric(i[1,5]) + sum(i[1:2,6]) + sum(i[2:3,7])
      sufocante <- as.numeric(i[1,7])
      
      if( qual == 1){ # 'qual' = 1: verão.
        qual <- 2
        m_verao[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante) # os valores são adicionados na coluna da matriz de verão.
        
      } else if( qual == 2 ){ # 'qual' = 2: outono.
        qual <- 3
        m_outono[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante)
        
      } else if( qual == 3 ){ # 'qual' = 3: inverno.
        qual <- 4
        m_inverno[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante)
        
      } else if( qual == 4 ){ # 'qual' = 4: primavera.
        qual <- 1
        m_primavera[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante)
        coluna <- coluna + 1 # coluna só itera quando chega-se na estação de primavera, fazendo com que verão receba valores na próxima coluna, and so on.
      }
    }
    
    barplot(m_verao, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , # os gráficos são plotados.
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Verão")
    barplot(m_outono, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , 
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Outono")
    barplot(m_inverno, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , 
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Inverno")
    barplot(m_primavera, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , 
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Primavera")
    
  ###
  } else { # se o entrado não foi nem matriz nem uma lista: retorna erro.
    print("O dado entrado não condiz com o esperado.")
    return(NULL)
  }
}

########################################################
checa_data <- function(string){
  #####
  # Entra com string, sai com booleano, indicando o tipo de data que se está lidando.
  #
  # Se TRUE: a data é do tipo YYYY-MM-DD.
  # Se FALSE: a data é do tipo DD/MM/YYYY.
  #####
  
  x <- as.character(string)
  a <- strsplit(x, " ")[[1]][1] # a recebe o string "data", separado da "hora".
  
  azero <- strsplit(a, "") # mais um split de string, agora quebrando caractere a caractere os dados.
  
  for(i in 1:length(azero[[1]])){
    if(azero[[1]][i] == "-"){ # se encontrar um "-", é do tipo YYYY-MM-DD.
      return(T)
    } else if(azero[[1]][i] == "/"){ # se encontrar um "/", é do tipo DD/MM/YYYY.
      return(F)
    }
  }
}

########################################################
def_dia <- function(data){
  #####
  # Recebe um string de data, retorna uma lista com o dia, o mês, e o ano separados.
  # 
  #####
  
  nova <- strsplit(data, "")
  sabe <- checa_data(data) # 'sabe' define se o tipo de dado tratado é YYYY-MM-DD ou DD/MM/YYYY.
  
  resp <- vector("list", 3) # resp é a lista a ser retornada, com os valores de dia, mês e ano.
  names(resp) <- c("dia", "mês", "ano")
  op <- c()
  
  if(sabe == T){ # YYYY-MM-DD
    count <- 3 # define a posição que os valores entrarão na lista 'resp'.
    for(i in 1:length(nova[[1]])){
      
      if(nova[[1]][i] == "-"){ # serve para pegar os dois primeiros valores numéricos do string. No caso, YYYY e MM.
        opa <- ""
        for(i in 1:length(op)){ # junta os strings separados em um somente.
          opa <- paste(opa, op[i], sep = "")
        }
        
        resp[count] <- as.numeric(opa)
        count <- count - 1
        op <- c()
      } else {
        op <- c(op,nova[[1]][i])
      }
    }
    opa <- ""
    for(i in 1:length(op)){ # pega o último valor numérico do string, no caso, DD.
      opa <- paste(opa, op[i], sep = "")
    }
    
    resp[count] <- as.numeric(opa)
    return(resp)
  
    
  } else if(sabe == F){ # DD/MM/YYYY
    count <- 1 # define a posição que os valores entrarão na lista 'resp'.
    for(i in 1:length(nova[[1]])){
      
      if(nova[[1]][i] == "/"){ # ao encontrar "/", junta todos os strings em um valor numérico. Pega DD e MM.
        opa <- ""
        for(i in 1:length(op)){
          opa <- paste(opa, op[i], sep = "")
        }
        
        resp[count] <- as.numeric(opa)
        count <- count + 1
        op <- c()
      } else{
        op <- c(op,nova[[1]][i])
      }
    }
    
    opa <- ""
    for(i in 1:length(op)){ # pega YYYY.
      opa <- paste(opa, op[i], sep = "")
    }
    
    resp[count] <- as.numeric(opa)
    return(resp)
  }
}

########################################################
compara_data <- function(data1, data2, string){
  #####
  # Recebe duas datas, e as compara. Retorna booleano, de acordo com a veracidade da comparação realizada.
  #  As datas podem estar ou como string, ou como lista, conforme 'def_dia'.
  # 
  # 'string': pode receber as entradas "=", "!=", ">", "<", ">=", "<=".
  #
  # A função compara a primeira data entrada com a segunda. Na forma: data1 *string_de_comparação* data2.
  #  Se a comparação for verdadeira, retorna True, caso contrário, retorna False.
  #####
  
  if(is.character(data1)){ # checa se data1 é string, o converte a lista.
    data1 <- def_dia(data1)
  }
  
  if(is.character(data2)){ # checa se data2 é string, o converte a lista.
    data2 <- def_dia(data2)
  }
  
  if((!is.list(data1)) & (!is.list(data2))){ # se ambas as datas não são listas: o programa encontrou um erro e termina, retornando NULL.
    print("Os valores entrados em compara_data não puderam ser comparados.")
    return(NULL)
  } else {
    nula <- NULL # nula checa se os dias, meses e anos são iguais.
    if( (data1[[1]] == data2[[1]]) & (data1[[2]] == data2[[2]]) & (data1[[3]] == data2[[3]]) ){
      nula <- T
    } else {
      nula <- F
    }
    
    if(nula == T){ # nula lida com situações que falsificam ou não a comparação, dado seu resultado.
      if(string == "=" | string == ">=" | string == "<="){
        return(T)
      } else if(string == "!=" | string == ">" | string == "<"){
        return(F)
      }
    } else if(nula == F){
      if(string == "="){
        return(F)
      } else if(string == "!="){
        return(T)
      }
    }

    if(string == ">" | string == ">="){ # situações que sobram, das anteriores, de ser maior ou maior-igual ( ">" e ">=" ).
      if(data1[[3]] > data2[[3]]){ # compara se um ano é maior que o outro.
        return(T)
      } else if(data1[[3]] < data2[[3]]){
        return(F)
      } else if(data1[[2]] > data2[[2]]){ # compara se um mês é maior que o outro.
        return(T)
      } else if(data1[[2]] < data2[[2]]){
        return(F)
      } else{ # caso ambos os casos falhem, compara os dias.
        if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 1)){ # checa de Janeiro a Dezembro, levando em conta bissextos, para ver se um dia é maior que outro.
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Janeiro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 2)){ # roda a comparação de Fevereiro, onde checa os bissextos.
          bis <- NULL
          ao <- 1960
          
          for(i in 1:20){ # checa se o ano comparado é bissexto.
            ao <- ao + 4
            if(data1[[3]] == ao){
              bis <- T
              break
            } else if(data1[[3]] < ao){
              bis <- F
              break
            } else if(ao == 2016){
              break
            }
          }
          if(is.null(bis)){
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          
          if(bis == F){
            if( ((data1[[1]] >= 1) & (data1[[1]] <= 28)) & ((data2[[1]] >= 1) & (data2[[1]] <= 28)) ){
              if(data1[[1]] > data2[[1]]){
                return(T)
              } else{
                return(F)
              }
            } else{
              print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                          " possuem algum erro.", sep = ""))
              return(NULL)
            }
            
          } else if(bis == T){
            if( ((data1[[1]] >= 1) & (data1[[1]] <= 29)) & ((data2[[1]] >= 1) & (data2[[1]] <= 29)) ){
              if(data1[[1]] > data2[[1]]){
                return(T)
              } else{
                return(F)
              }
            } else{
              print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                          " possuem algum erro.", sep = ""))
              return(NULL)
            }
          }
          ## Fevereiro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 3)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Março
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 4)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Abril
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 5)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Maio
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 6)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Junho
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 7)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Julho
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 8)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Agosto
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 9)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Setembro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 10)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Outubro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 11)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Novembro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 12)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] > data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Dezembro
        } 
      }
    }
    
    if(string == "<" | string == "<="){ # realiza as mesmas operações de acima, mas para menor e menor-igual ( "<" e "<=" ).
      if(data1[[3]] < data2[[3]]){
        return(T)
      } else if(data1[[3]] > data2[[3]]){
        return(F)
      } else if(data1[[2]] < data2[[2]]){
        return(T)
      } else if(data1[[2]] > data2[[2]]){
        return(F)
      } else{
        if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 1)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Janeiro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 2)){
          bis <- NULL
          ao <- 1960
          
          for(i in 1:20){
            ao <- ao + 4
            if(data1[[3]] == ao){
              bis <- T
              break
            } else if(data1[[3]] < ao){
              bis <- F
              break
            } else if(ao == 2016){
              break
            }
          }
          if(is.null(bis)){
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          
          if(bis == F){
            if( ((data1[[1]] >= 1) & (data1[[1]] <= 28)) & ((data2[[1]] >= 1) & (data2[[1]] <= 28)) ){
              if(data1[[1]] < data2[[1]]){
                return(T)
              } else{
                return(F)
              }
            } else{
              print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                          " possuem algum erro.", sep = ""))
              return(NULL)
            }
            
          } else if(bis == T){
            if( ((data1[[1]] >= 1) & (data1[[1]] <= 29)) & ((data2[[1]] >= 1) & (data2[[1]] <= 29)) ){
              if(data1[[1]] < data2[[1]]){
                return(T)
              } else{
                return(F)
              }
            } else{
              print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                          " possuem algum erro.", sep = ""))
              return(NULL)
            }
          }
          ## Fevereiro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 3)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Março
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 4)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Abril
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 5)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Maio
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 6)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Junho
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 7)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Julho
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 8)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Agosto
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 9)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Setembro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 10)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Outubro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 11)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 30)) & ((data2[[1]] >= 1) & (data2[[1]] <= 30)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Novembro
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 12)){
          if( ((data1[[1]] >= 1) & (data1[[1]] <= 31)) & ((data2[[1]] >= 1) & (data2[[1]] <= 31)) ){
            if(data1[[1]] < data2[[1]]){
              return(T)
            } else{
              return(F)
            }
          } else{
            print(paste("As datas ", data1[[1]], "/", data1[[2]], "/", data1[[3]], " e ", data2[[1]], "/", data2[[2]], "/", data2[[3]], 
                        " possuem algum erro.", sep = ""))
            return(NULL)
          }
          ## Dezembro
        } else {
          print(paste("Os meses ", data1[[2]], "/", data1[[3]], " e ", data2[[2]], "/", data2[[3]], " possuem algum erro.", sep = ""))
          return(NULL)
        }
      }
    }
  }
}

########################################################
separa_estacao <- function(dado){
  #####
  # Entra com data.frame de temperaturas máximas e mínimas, e retorna o data.frame original com uma coluna a mais,
  #  indicando a estação do ano que a data pertence.
  #
  # O código funciona apenas com o modelo 'dia' 'tmin' 'tmax', adicionando logo após 'tmax' a coluna 'season'.
  #
  # datas das estações do ano coletadas de:
  # https://www.timeanddate.com/calendar/seasons.html?year=1950&n=233
  #####
  
  seasons <- c("1961-03-20", "1961-06-21", "1961-09-23", "1961-12-21", "1962-03-20", "1962-06-21", "1962-09-23", "1962-12-22",
               "1963-03-21", "1963-06-22", "1963-09-23", "1963-12-22", "1964-03-20", "1964-06-21", "1964-09-22", "1964-12-21",
               "1965-03-20", "1965-06-21", "1965-09-23", "1965-12-21", "1966-03-20", "1966-06-21", "1966-09-23", "1966-12-22",
               "1967-03-21", "1967-06-21", "1967-09-23", "1967-12-22", "1968-03-20", "1968-06-21", "1968-09-22", "1968-12-21",
               "1969-03-20", "1969-06-21", "1969-09-23", "1969-12-21", "1970-03-20", "1970-06-21", "1970-09-23", "1970-12-22",
               "1971-03-21", "1971-06-21", "1971-09-23", "1971-12-22", "1972-03-20", "1972-06-21", "1972-09-22", "1972-12-21",
               "1973-03-20", "1973-06-21", "1973-09-23", "1973-12-21", "1974-03-20", "1974-06-21", "1974-09-23", "1974-12-22",
               "1975-03-21", "1975-06-21", "1975-09-23", "1975-12-22", "1976-03-20", "1976-06-21", "1976-09-22", "1976-12-21",
               "1977-03-20", "1977-06-21", "1977-09-23", "1977-12-21", "1978-03-20", "1978-06-21", "1978-09-23", "1978-12-22",
               "1979-03-21", "1979-06-21", "1979-09-23", "1979-12-22", "1980-03-20", "1980-06-21", "1980-09-22", "1980-12-21",
               "1981-03-20", "1981-06-21", "1981-09-23", "1981-12-21", "1982-03-20", "1982-06-21", "1982-09-23", "1982-12-22",
               "1983-03-21", "1983-06-21", "1983-09-23", "1983-12-22", "1984-03-20", "1984-06-21", "1984-09-22", "1984-12-21",
               "1985-03-20", "1985-06-21", "1985-09-22", "1985-12-21", "1986-03-20", "1986-06-21", "1986-09-23", "1986-12-22",
               "1987-03-21", "1987-06-21", "1987-09-23", "1987-12-22", "1988-03-20", "1988-06-21", "1988-09-22", "1988-12-21",
               "1989-03-20", "1989-06-21", "1989-09-22", "1989-12-21", "1990-03-20", "1990-06-21", "1990-09-23", "1990-12-22",
               "1991-03-21", "1991-06-21", "1991-09-23", "1991-12-22", "1992-03-20", "1992-06-21", "1992-09-22", "1992-12-21",
               "1993-03-20", "1993-06-21", "1993-09-22", "1993-12-21", "1994-03-20", "1994-06-21", "1994-09-23", "1994-12-22",
               "1995-03-20", "1995-06-21", "1995-09-23", "1995-12-22", "1996-03-20", "1996-06-20", "1996-09-22", "1996-12-21",
               "1997-03-20", "1997-06-21", "1997-09-22", "1997-12-21", "1998-03-20", "1998-06-21", "1998-09-23", "1998-12-21",
               "1999-03-20", "1999-06-21", "1999-09-23", "1999-12-22", "2000-03-20", "2000-06-20", "2000-09-22", "2000-12-21",
               "2001-03-20", "2001-06-21", "2001-09-22", "2001-12-21", "2002-03-20", "2002-06-21", "2002-09-23", "2002-12-21",
               "2003-03-20", "2003-06-21", "2003-09-23", "2003-12-22", "2004-03-20", "2004-06-20", "2004-09-22", "2004-12-21",
               "2005-03-20", "2005-06-21", "2005-09-22", "2005-12-21", "2006-03-20", "2006-06-21", "2006-09-23", "2006-12-21",
               "2007-03-20", "2007-06-21", "2007-09-23", "2007-12-22", "2008-03-20", "2008-06-20", "2008-09-22", "2008-12-21",
               "2009-03-20", "2009-06-21", "2009-09-22", "2009-12-21", "2010-03-20", "2010-06-21", "2010-09-23", "2010-12-21",
               "2011-03-20", "2011-06-21", "2011-09-23", "2011-12-22", "2012-03-20", "2012-06-20", "2012-09-22", "2012-12-21",
               "2013-03-20", "2013-06-21", "2013-09-22", "2013-12-21", "2014-03-20", "2014-06-21", "2014-09-22", "2014-12-21",
               "2015-03-20", "2015-06-21", "2015-09-23", "2015-12-22", "final") # datas dos equinócios e solstícios, para mudança de estação.
                                                                                #  O string "final" é para que o código não dê erro ao terminar
                                                                                #   a última mudança de estação na checagem de verdadeiro/falso
                                                                                #    do if abaixo. Fora disso, é inútil.
  if(ncol(dado) != 3){
    print("O número de colunas não é o esperado no data.frame de entrada.")
    print("O código não foi rodado.")
    return(NULL)
  }
  
  estacao <- c("verao", "outono", "inverno", "primavera")
  dado$season <- c(1:nrow(dado)) # é adicionado ao data.frame 'dado' a coluna 'season', que é preenchida de 1 ao número de linhas total do data.frame.
  
  conta <- 1 # itera no vetor seasons.
  guarda_est <- 1 # indica a estação do vetor estacao.
  
  for(i in 1:(nrow(dado))){
    anovo <- dado[i, 1] # anovo recebe a data da linha i do data.frame 'dado'.
    
    if(anovo == seasons[conta]){ # caso chegou em uma data de mudança de estação, o código faz a estação mudar.
      conta <- conta + 1
      if(guarda_est == 1){
        guarda_est <- 2
      } else if(guarda_est == 2){
        guarda_est <- 3
      } else if(guarda_est == 3){
        guarda_est <- 4
      } else if(guarda_est == 4){
        guarda_est <- 1
      }
    }
    
    dado[i,]$season <- estacao[guarda_est] # guarda em dado a estação que a data pertence.
  }
  
  return(dado)
}

########################################################

# C O N V E R T E R   D A D O S 

########################################################
convert_universal <- function(dado1, dado2 = NA, Mirante = T){
  #####
  # Entra com um ou dois data.frames: tabelas de dados de temperaturas máximas e mínimas (dois data.frames: um de máximas, outro de mínimas, padrão IAG;
  #  um data.frame: com máximas e mínimas no mesmo arquivo, padrão Mirante) e retorna um único data.frame com temperaturas máximas e mínimas dos dias
  #  01/01/1961 a 31/12/2015.
  #  O programa adiciona NA a dados faltantes.
  #   A estrutura esperada do padrão IAG: [Data, tmax], [Data, tmin].
  #   A estrutura esperada do padrão Mirante: [__, Data, Hora, __, TempMaxima, TempMinima]
  # 
  # 'Mirante': se a estrutura a ser usada nos dados seguindo padrão Mirante usa o dia posterior ou o mesmo dia, para a coleta de temperaturas.
  #  O padrão é dia posterior, com 'Mirante' = T. Toma-se que a hora é em padrão UTC, o que implica que a coleta do dia posterior, às 00:00 horas UTC
  #  na verdade condiz com o dia anterior, às 21 horas. Por isso esse método se vale de pegar o valor coletado às 12:00 horas UTC do mesmo dia, como
  #  temperatura mínima, e o valor do dia posterior, coletado às 00:00 horas UTC, como o valor da temperatura máxima daquele dia.
  #   Usando 'Mirante' = F, os dados são arranjados de forma a, no dia i, a temperatura mínima desse dia é a coletada às 12:00 horas UTC, e a temperatura
  #   máxima é a coletada às 00:00 horas UTC do mesmo dia i, no data.frame original.
  #####
  
  op <- ncol(dado1) # op checa se o data.frame dado1 é um padrão IAG ou Mirante.
  
  if( (op == 2) & (is.data.frame(dado2)) ){ # se há um data.frame em dado2, e o tamanho de op é 2, então é padrão IAG.
    trabalho <- T
  } else if( (op > 2) & (is.na(dado2)) ){ # se não há um data.frame em dado2, e op é maior que 2, é padrão Mirante.
    trabalho <- F
  }
  
  if(trabalho == T){ # aqui, há uma segunda rechecagem, para saber se o data.frame IAG tem as datas formatadas em padrão IAG.
    ea <- checa_data(dado1[1,1])
    eb <- checa_data(dado2[1,1])
    
    if( (ea == eb) & (ea == trabalho) ){
      ea <- "a"
    } else {
      print("Há um erro na entrada dos data.frames. Certifique-se de que ambos são dados do IAG (temperaturas máximas e mínimas).")
      return(NULL) # retorna NULL se algo errado, em qualquer das duas rechecagens.
    }
    
    rm(ea, eb)
    
  } else if(trabalho == F){ # outra segunda rechecagem, mas para o padrão Mirante, para ver a formatação das datas, também.
    
    ea <- checa_data(dado1[1,2])
    
    if(ea == trabalho){
      ea <- "b"
    } else {
      print("Há um erro na entrada do data.frame. Certifique-se de é o tipo de data.frame de Mirante.")
      return(NULL)
    }
    
    rm(ea)
  }
  
  dia <- read.table("dias.txt") # a forma que cheguei para obter o data.frame de datas foi deixar um arquivo em separado com os valores de cada data.
                                # esse data.frame é acessado cada vez que o programa roda, portanto.
  dia$tmax <- c(1:nrow(dia)) # o data.frame criado recebe uma coluna de tmax e outra de tmin.
  dia$tmin <- c(1:nrow(dia))
  names(dia) <- c("Data", "tmin", "tmax") # a ordem é Data, tmin, e tmax.
  
  ######
  if(trabalho == T){ # caso de trabalharmos com estrutura IAG.
    
    opera_dado1 <- 1 # opera_dado1 e opera_dado2 são duas iterações que são realizadas além das feitas pelo for.
    opera_dado2 <- 1
    
    for(i in 1:nrow(dia)){
      
      maxa <- as.vector(dado1[opera_dado1,1])
      mina <- as.vector(dado2[opera_dado2,1])
      maxa_n <- strsplit(maxa, " ")[[1]][1] # pedaço de código usado para extrair apenas os valores de data da estrutura IAG.
      mina_n <- strsplit(mina, " ")[[1]][1] # uma vez que as datas aqui são da forma "data hora". A "hora" é retirada, para que o programa avance.
      
      if(compara_data(as.character(dia[i,1]), maxa_n, "=")){ # usa-se compara_data para... comparar as datas de dia e do data.frame de máximas, e colocar 
                                                             # os valores.
        dia[i,3] <- dado1[opera_dado1, 2]
        opera_dado1 <- opera_dado1 + 1 # opera_dado1 e opera_dado2 itera aqui, para saber a posição que o data.frame entrado está.
                                       # isso porque podem haver saltos nos dados coletados, e isso não seria captado se todos fossem iterados pelo for.
      } else { # se a comparação deu resultado negativo, então esse dia não possui o valor de temperatura coletado, e recebe NA.
        dia[i,3] <- NA
      }
      
      if(compara_data(as.character(dia[i,1]), mina_n, "=")){ # mesmo processo descrito acima, mas lidando com a temperatua mínima, do outro data.frame.
        dia[i,2] <- dado2[opera_dado2, 2]
        opera_dado2 <- opera_dado2 + 1
      } else {
        dia[i,2] <- NA
      }
    }
    
  ######
  } else if(trabalho == F){ # caso de trabalharmos com estrutura Mirante.
    ## ----
    if(Mirante == T){ # se Mirante = T, usaremos o dia posterior para pegar os dados.
      
      opera_dado1 <- 2
      
      for(i in 1:nrow(dia)){
        if(as.character(dado1$Data[opera_dado1]) != "01/01/1961"){ # situação em que o primeiro valor do data.frame padrão Mirante é descartado, pois condiz com
                                                                   # a data de 31/12/1960.
          if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 0000) ){ # se é 0000, é máxima, 
                                                                                                                                      # e vai pra data anterior
                                                                                                                                      # à atual.
            dia[i-1,3] <- dado1$TempMaxima[opera_dado1]
            opera_dado1 <- opera_dado1 + 1
          
          } else { # caso contrário, o data.frame padrão Mirante não tem o dado, e entra NA.
            dia[i-1,3] <- NA
          }
        }
        
        
        if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 1200) ){ # se é 1200, é mínima, e
                                                                                                                                    # vai para a data atual.
          dia[i, 2] <- dado1$TempMinima[opera_dado1]
          opera_dado1 <- opera_dado1 + 1
        
        } else { # caso não haja no data.frame padrão Mirante, entra NA.
          dia[i, 2] <- NA
        }
      }
      
      dia[i,3] <- dado1$TempMaxima[opera_dado1] # Coloca a máxima do dia 31/12/2015.
      
    ## ----
    } else if(Mirante == F){ # se Mirante = F, usaremos o mesmo dia para pegar os dados.
      
      opera_dado1 <- 1
      
      for(i in 1:nrow(dia)){
        
        if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 0000) ){ # se é 0000, entra como 
                                                                                                                                    # temperatura máxima do 
                                                                                                                                    # dia atual.
          dia[i,3] <- dado1$TempMaxima[opera_dado1]
          opera_dado1 <- opera_dado1 + 1
        } else { # se não tem a máxima, entra como NA.
          dia[i,3] <- NA
        }
        
        if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 1200) ){ # se é 1200, entra como 
                                                                                                                                    # temperatura mínima do 
                                                                                                                                    # dia atual.
          dia[i, 2] <- dado1$TempMinima[opera_dado1]
          opera_dado1 <- opera_dado1 + 1
        } else { # caso contrário, entra NA.
          dia[i, 2] <- NA
        }
      }
    }
  }
  
  return(dia)
}

########################################################

# M A T R I Z E S

########################################################
define_minima <- function(mima){ # encontra a posição na matriz da temperatura mínima obtida.
  #####
  # Entra com um valor de temperatura mínima, define em que linha da matriz ela entrará.
  #
  #####
  
  if(mima <= -5){
    return("G")
  } else if(mima <= 0){
    return("F")
  } else if(mima <= 5){
    return("E")
  } else if(mima <= 10){
    return("D")
  } else if(mima <= 15){
    return("C")
  } else if(mima <= 20){
    return("B")
  } else {
    return("A")
  }
}

########################################################
define_maxima <- function(maxa){ # encontra a posição na matriz da temperatura máxima obtida.
  #####
  # Entra com um valor de temperatura máxima, define em que coluna da matriz ela entrará.
  #
  #####
  
  if(maxa <= 10){
    return(1)
  } else if(maxa <= 15){
    return(2)
  } else if(maxa <= 20){
    return(3)
  } else if(maxa <= 25){
    return(4)
  } else if(maxa <= 30){
    return(5)
  } else if(maxa <= 35){
    return(6)
  } else {
    return(7)
  }
}

########################################################
separa_temp <- function(dado, save = F){
  #####
  # Entra com data.frame na estrutura de convert_universal, sai com matriz de temperaturas calculada.
  #
  #####
  
  resultado <- matrix(rep(0L, len=49), nrow=7, dimnames=list(c("A", "B", "C", "D", "E", "F", "G"))) # gera a matriz da resposta.
  
  if( is.null(dado) | (nrow(dado) == 0) ){ # estrutura que impede de ctrab_na e ctrab_cons retornem erro ao encontrarem a 
                                           #totalidade de linhas com NAs ou inconsistentes.
    return(resultado)
  }
  dado_na <- ctrab_na(dado, salva = F, altera = T)
  
  if( is.null(dado_na) | (nrow(dado_na) == 0) ){
    return(resultado)
  }
  dado_fim <- ctrab_cons(dado_na, salva = F, altera = T)
  
  if( is.null(dado_fim) | (nrow(dado_fim) == 0) ){
    return(resultado)
  }
  
  for( i in 1:nrow(dado_fim) ){
    mbm <- define_minima( dado_fim$tmin[i] )
    mam <- define_maxima( dado_fim$tmax[i] )
    
    resultado[mbm,mam] <- resultado[mbm,mam] + 1 # e mbm e mam são encaixadas como coordenadas da matriz resposta.
  }
  
  if( save == T ){
    save_data(resultado)
  }
  
  return(resultado)
}

########################################################
quinquenal <- function(dado, sazonal = F, save = F, plota = F){
  #####
  # Entra com data.frame de temperaturas máximas e mínimas, retorna as matrizes calculadas quinquênio a quinquênio (5 em 5 anos).
  #
  # 'sazonal': se TRUE, calcula as matrizes para cada estação de cada quinquênio. Sendo, então, 4 matrizes para cada quinquênio.
  #   Se FALSE (default), cada quinquênio só terá calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada quinquênio (ou cada estação de cada quinquênio, a depender de 'sazonal') terá um gráfico plotado de seus valores.
  #####
  
  ####
  if(sazonal == T){ # essa estrutura se repete no resto do código. Se a opção sazonal é escolhida, o código declara variáveis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a estação de cada data da tabela.
  }
  
  ####
  if(sazonal == T){ # pedaço de código onde se declara a lista que retornará os resultados.
    resultado <- vector("list", 44)
    prog <- c("Verao 1961-1965", "Outono 1961-1965", "Inverno 1961-1965", "Primavera 1961-1965",
              "Verao 1966-1970", "Outono 1966-1970", "Inverno 1966-1970", "Primavera 1966-1970",
              "Verao 1971-1975", "Outono 1971-1975", "Inverno 1971-1975", "Primavera 1971-1975",
              "Verao 1976-1980", "Outono 1976-1980", "Inverno 1976-1980", "Primavera 1976-1980",
              "Verao 1981-1985", "Outono 1981-1985", "Inverno 1981-1985", "Primavera 1981-1985",
              "Verao 1986-1990", "Outono 1986-1990", "Inverno 1986-1990", "Primavera 1986-1990",
              "Verao 1991-1995", "Outono 1991-1995", "Inverno 1991-1995", "Primavera 1991-1995",
              "Verao 1996-2000", "Outono 1996-2000", "Inverno 1996-2000", "Primavera 1996-2000",
              "Verao 2001-2005", "Outono 2001-2005", "Inverno 2001-2005", "Primavera 2001-2005",
              "Verao 2006-2010", "Outono 2006-2010", "Inverno 2006-2010", "Primavera 2006-2010",
              "Verao 2011-2015", "Outono 2011-2015", "Inverno 2011-2015", "Primavera 2011-2015")
    names(resultado) <- prog
  } else {
    resultado <- vector("list", 11)
    prog  <- c("1961-1965", "1966-1970", "1971-1975", "1976-1980", "1981-1985", "1986-1990", "1991-1995", 
               "1996-2000", "2001-2005", "2006-2010", "2011-2015")
    names(resultado) <- prog
  }
  
  ####
  datas <- c("1961-01-01", "1966-01-01", "1971-01-01", "1976-01-01", "1981-01-01", "1986-01-01", "1991-01-01", 
             "1996-01-01", "2001-01-01", "2006-01-01", "2011-01-01") # datas para quebrar nos quinquênios.
  
  data_count <- 2 # itera no vetor datas.
  wee <- 1 # itera na posição a salvar da lista resultado.
  o <- 0 # faz as operações de cortar o data.frame.
  
  abs <- nrow(dado)
  dado_atual <- dado
  
  ####
  for(i in 1:abs){
    o <- o + 1
    const <- as.character(dado_atual$Data[i])
    
    ###
    if( const == datas[data_count] ){
      dado_novo <- dado[1:(o-1),] # quebra dado no quinquênio "encontrado".
      dado <- dado[-c(1:(o-1)),]
      
      if(sazonal == T){ # calcula para cada estação do quinquênio obtido.
        
        ##
        ver_op <- subset(dado_novo, season == "verao")
        if(plota == T){
          plot_temps(ver_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(ver_op)
        wee <- wee + 1
        
        ##
        out_op <- subset(dado_novo, season == "outono")
        if(plota == T){
          plot_temps(out_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(out_op)
        wee <- wee + 1
        
        ##
        inv_op <- subset(dado_novo, season == "inverno")
        if(plota == T){
          plot_temps(inv_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(inv_op)
        wee <- wee + 1
        
        ##
        pri_op <- subset(dado_novo, season == "primavera")
        if(plota == T){
          plot_temps(pri_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(pri_op)
        wee <- wee + 1

      } else { # situação onde se calcula apenas o quinquênio, sem considerar as estações.
        if(plota == T){
          plot_temps(dado_novo, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(dado_novo)
        wee <- wee + 1
      }
      
      data_count <- data_count + 1
      o <- 1
    }
    
    ###
    if( (const == "2011-01-01") & (sazonal == F) ){ # última parte do último quinquênio, é salva aqui.
      if(plota == T){
        plot_temps(dado, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(dado)
      break
      
    } else if( (const == "2011-01-01") & (sazonal == T)){ # última parte do último quinquênio, calculando as estações,
      
      ##
      ver_op <- subset(dado, season == "verao")
      if(plota == T){
        plot_temps(ver_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(ver_op)
      wee <- wee + 1
      
      ##
      out_op <- subset(dado, season == "outono")
      if(plota == T){
        plot_temps(out_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(out_op)
      wee <- wee + 1
      
      ##
      inv_op <- subset(dado, season == "inverno")
      if(plota == T){
        plot_temps(inv_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(inv_op)
      wee <- wee + 1
      
      ##
      pri_op <- subset(dado, season == "primavera")
      if(plota == T){
        plot_temps(pri_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(pri_op)
      
      break
    }
  }
  
  ####
  if(save == T){
    save_data(resultado)
  }
  
  return(resultado)
}

########################################################
sazonal <- function(dado, save = F, plota = F){
  #####
  # Entra com data.frame de temperaturas, retorna 4 matrizes calculadas: uma para cada estação presente no data.frame inteiro.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada estação terá um gráfico plotado de seus valores.
  #####
  
  dado <- separa_estacao(dado) # função que coloca, para cada dia do data.frame original, a estação do ano à qual aquele dia pertence.
  
  resultado <- vector("list", 4)
  names(resultado) <- c("Verão", "Outono", "Inverno", "Primavera") # identifica cada matriz de resposta com o título da estação.
  
  ver_op <- subset(dado, season == "verao") # com os valores das estações nos data.frames, é só fazer subset dos dados,
  if(plota == T){                           # e com o subset realizar a entrada na função separa_temp.
    plot_temps(ver_op, titulo = "Verão")
  }
  resultado[[1]] <- separa_temp(ver_op)
  
  out_op <- subset(dado, season == "outono")
  if(plota == T){
    plot_temps(out_op, titulo = "Outono")
  }
  resultado[[2]] <- separa_temp(out_op)
  
  inv_op <- subset(dado, season == "inverno")
  if(plota == T){
    plot_temps(inv_op, titulo = "Inverno")
  }
  resultado[[3]] <- separa_temp(inv_op)
  
  pri_op <- subset(dado, season == "primavera")
  if(plota == T){
    plot_temps(pri_op, titulo = "Primavera")
  }
  resultado[[4]] <- separa_temp(pri_op)
  
  if(save == T){
    save_data(resultado)
  }
  return(resultado)
}

########################################################
decenio <- function(dado, sazonal = F, save = F, plota = F){
  #####
  # Entra com data.frame de temperaturas, retorna as matrizes calculadas decênio a decênio (10 em 10 anos).
  #
  # 'sazonal': se TRUE, calcula as matrizes para cada estação de cada decênio. Sendo, então, 4 matrizes para cada decênio.
  #   Se FALSE (default), cada decênio só terá calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada decênio (ou cada estação de cada decênio, a depender de 'sazonal') terá um gráfico plotado de seus valores.
  #####
  
  if(sazonal == T){ # essa estrutura se repete no resto do código. Se a opção sazonal é escolhida, o código declara variáveis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a estação de cada data das tabelas.
  }
  
  if(sazonal == T){ # pedaço de código onde se declara a lista que retornará os resultados.
    resultado <- vector("list", 20)
    prog <- c("Verao 1961-1970", "Outono 1961-1970", "Inverno 1961-1970", "Primavera 1961-1970",
              "Verao 1971-1980", "Outono 1971-1980", "Inverno 1971-1980", "Primavera 1971-1980",
              "Verao 1981-1990", "Outono 1981-1990", "Inverno 1981-1990", "Primavera 1981-1990",
              "Verao 1991-2000", "Outono 1991-2000", "Inverno 1991-2000", "Primavera 1991-2000",
              "Verao 2001-2010", "Outono 2001-2010", "Inverno 2001-2010", "Primavera 2001-2010")
    names(resultado) <- prog
  } else {
    resultado <- vector("list", 5)
    prog <- c("1961-1970", "1971-1980", "1981-1990", "1991-2000", "2001-2010")
    names(resultado) <- prog
  }
  
  datas <- c("1961-01-01", "1971-01-01", "1981-01-01", "1991-01-01", "2001-01-01", "2011-01-01") # datas para quebrar nos decênios.
  
  data_count <- 2 # itera no vetor datas.
  wee <- 1 # itera na posição a salvar da lista resultado.
  o <- 0 # faz as operações de cortar o data.frame.
  
  abs <- nrow(dado)
  dado_atual <- dado
  
  for(i in 1:abs){
    o <- o + 1
    const <- as.character(dado_atual$Data[i])
    
    if( const == datas[data_count] ){
      dado_novo <- dado[1:(o-1),] # quebra dado no decênio "encontrado".
      dado <- dado[-c(1:(o-1)),]
      
      if(sazonal == T){ # calcula para cada estação do decênio obtido.
        ver_op <- subset(dado_novo, season == "verao")
        resultado[[wee]] <- separa_temp(ver_op)
        wee <- wee + 1
        
        out_op <- subset(dado_novo, season == "outono")
        resultado[[wee]] <- separa_temp(out_op)
        wee <- wee + 1
        
        inv_op <- subset(dado_novo, season == "inverno")
        resultado[[wee]] <- separa_temp(inv_op)
        wee <- wee + 1
        
        pri_op <- subset(dado_novo, season == "primavera")
        resultado[[wee]] <- separa_temp(pri_op)
        wee <- wee + 1
        
        if(plota == T){
          plot_temps(ver_op, titulo = prog[wee-4])
          plot_temps(out_op, titulo = prog[wee-3])
          plot_temps(inv_op, titulo = prog[wee-2])
          plot_temps(pri_op, titulo = prog[wee-1])
        }
        
      } else { # situação onde se calcula apenas o decênio, sem considerar as estações.
        if(plota == T){
          plot_temps(dado_novo, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(dado_novo)
        wee <- wee + 1
      }
      
      data_count <- data_count + 1
      o <- 1
    }
    
    if( const == "2011-01-01" ){ # nos 5 anos finais, o código para, uma vez que ele não completa um decênio.
      break
    }
  }
  
  if(save == T){
    save_data(resultado)
  }
  return(resultado)
}

########################################################
trinta <- function(dado, sazonal = F, save = F, plota = F){
  #####
  # Entra com data.frame de temperaturas, retorna as matrizes calculadas de trinta em trinta anos.
  #
  # 'sazonal': se TRUE, calcula as matrizes para cada estação de cada período de trinta anos. Sendo, então, 4 matrizes para cada período.
  #   Se FALSE (default), cada período só terá calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada período de trinta anos (ou cada estação de cada período, a depender de 'sazonal') terá um gráfico plotado de seus valores.
  #####
  
  if(sazonal == T){ # essa estrutura se repete no resto do código. Se a opção sazonal é escolhida, o código declara variáveis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a estação de cada data.
  }
  
  if(sazonal == T){ # pedaço de código onde se declara a lista que retornará os resultados.
    resultado <- vector("list", 12)
    prog <- c("Verao 1961-1990", "Outono 1961-1990", "Inverno 1961-1990", "Primavera 1961-1990",
              "Verao 1971-2000", "Outono 1971-2000", "Inverno 1971-2000", "Primavera 1971-2000",
              "Verao 1981-2010", "Outono 1981-2010", "Inverno 1981-2010", "Primavera 1981-2010")
    names(resultado) <- prog
    
  } else {
    resultado <- vector("list", 3)
    prog <- c("1961-1990", "1971-2000", "1981-2010")
    names(resultado) <- prog
  }
  
  dataA <- c("1961-01-01", "1971-01-01", "1981-01-01", "fim") # datas que são importantes para que o código rode. "fim", aqui, serve
                                                              # para que o programa não retorne um erro, ao terminar de iterar as datas desse vetor.
  dataZ <- c("1991-01-01", "2001-01-01", "2011-01-01")
  
  a1 <- 0 # marcam as datas de início de cada período de 30 anos.
  a2 <- 0
  a3 <- 0
  a <- 1
  operaA <- 1 # itera em dataA.
  
  z1 <- 0 # marcam as datas de fim de cada período de 30 anos.
  z2 <- 0
  z3 <- 0
  z <- 1
  operaZ <- 1 # itera em dataZ.
  
  wee <- 1 # itera na posição a salvar da lista resultado.
  abs <- nrow(dado) # número de linhas do vetor original.
  dado_atual <- dado # cópia simples do vetor original.
  
  for(i in 1:abs){
    
    const <- as.character(dado_atual$Data[i])
    
    if( const == dataA[operaA] ){ # a função de ambos os ifs é de guardar as posições que demarcam um período de 30 anos nos dados.
      if(a == 1){ # indica os inícios dos períodos.
        a1 <- i
      } else if (a == 2){
        a2 <- i
      } else if (a == 3){
        a3 <- i
      }
      a <- a + 1
      operaA <- operaA + 1
    }
    
    if( const == dataZ[operaZ] ){ # indica os fins dos períodos.
      if(z == 1){
        z1 <- i - 1
      } else if (z == 2){
        z2 <- i - 1
      } else if (z == 3){
        z3 <- i - 1
        break # ao chegar na última data, não é necessário andar pelo resto dos dados, por isso o break.
      }
      z <- z + 1
      operaZ <- operaZ + 1
    }
  }
  
  for(i in 1:3){ # aqui, os dados são tratados em matrizes.
    if(i == 1){
      a <- a1
      z <- z1
    } else if(i == 2){
      a <- a2
      z <- z2
    } else if(i == 3){
      a <- a3
      z <- z3
    }
    
    dado_novo <- dado[a:z,]
    
    
    if(sazonal == T){ # caso de cada estação, nos 30 anos.
      ver_op <- subset(dado_novo, season == "verao")
      resultado[[wee]] <- separa_temp(ver_op)
      wee <- wee + 1
      
      out_op <- subset(dado_novo, season == "outono")
      resultado[[wee]] <- separa_temp(out_op)
      wee <- wee + 1
      
      inv_op <- subset(dado_novo, season == "inverno")
      resultado[[wee]] <- separa_temp(inv_op)
      wee <- wee + 1
      
      pri_op <- subset(dado_novo, season == "primavera")
      resultado[[wee]] <- separa_temp(pri_op)
      wee <- wee + 1
      
      if(plota == T){ # se plota for marcado como TRUE, ele faz o plot dos dados para cada estação.
        plot_temps(ver_op, titulo = prog[wee-4])
        plot_temps(out_op, titulo = prog[wee-3])
        plot_temps(inv_op, titulo = prog[wee-2])
        plot_temps(pri_op, titulo = prog[wee-1])
      }
      
    } else { # situação sem considerar as estações.
      if(plota == T){
        plot_temps(dado_novo, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(dado_novo)
      wee <- wee + 1
    }
  }
  
  if(save == T){
    save_data(resultado) # se nada errado for identificado, os dados são salvos, se a opção foi dada na chamada da função.
  }
  
  return(resultado) # e então, vem o return dos dados já analisados, em matrizes.
}

########################################################
anual <- function(dado, sazonal = F, save = F, plota = F){
  #####
  # Entra com data.frame de temperaturas, retorna as matrizes calculadas ano a ano.
  #
  # 'sazonal': se TRUE, calcula as matrizes para cada estação de cada ano. Sendo, então, 4 matrizes para cada ano.
  #   Se FALSE (default), cada ano "só" terá calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada ano (ou cada estação de cada ano, a depender de 'sazonal') terá um gráfico plotado de seus valores.
  #####
  
  if(sazonal == T){ # essa estrutura se repete no resto do código. Se a opção sazonal é escolhida, o código declara variáveis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a estação de cada data das tabelas.
  }
  
  if(sazonal == T){ # pedaço de código onde se declara a lista que retornará os resultados.
    resultado <- vector("list", 220)
    prog <- c("Verao 1961", "Outono 1961", "Inverno 1961", "Primavera 1961", "Verao 1962", "Outono 1962", "Inverno 1962", "Primavera 1962",
              "Verao 1963", "Outono 1963", "Inverno 1963", "Primavera 1963", "Verao 1964", "Outono 1964", "Inverno 1964", "Primavera 1964",
              "Verao 1965", "Outono 1965", "Inverno 1965", "Primavera 1965", "Verao 1966", "Outono 1966", "Inverno 1966", "Primavera 1966",
              "Verao 1967", "Outono 1967", "Inverno 1967", "Primavera 1967", "Verao 1968", "Outono 1968", "Inverno 1968", "Primavera 1968",
              "Verao 1969", "Outono 1969", "Inverno 1969", "Primavera 1969", "Verao 1970", "Outono 1970", "Inverno 1970", "Primavera 1970",
              "Verao 1971", "Outono 1971", "Inverno 1971", "Primavera 1971", "Verao 1972", "Outono 1972", "Inverno 1972", "Primavera 1972",
              "Verao 1973", "Outono 1973", "Inverno 1973", "Primavera 1973", "Verao 1974", "Outono 1974", "Inverno 1974", "Primavera 1974",
              "Verao 1975", "Outono 1975", "Inverno 1975", "Primavera 1975", "Verao 1976", "Outono 1976", "Inverno 1976", "Primavera 1976",
              "Verao 1977", "Outono 1977", "Inverno 1977", "Primavera 1977", "Verao 1978", "Outono 1978", "Inverno 1978", "Primavera 1978",
              "Verao 1979", "Outono 1979", "Inverno 1979", "Primavera 1979", "Verao 1980", "Outono 1980", "Inverno 1980", "Primavera 1980",
              "Verao 1981", "Outono 1981", "Inverno 1981", "Primavera 1981", "Verao 1982", "Outono 1982", "Inverno 1982", "Primavera 1982",
              "Verao 1983", "Outono 1983", "Inverno 1983", "Primavera 1983", "Verao 1984", "Outono 1984", "Inverno 1984", "Primavera 1984",
              "Verao 1985", "Outono 1985", "Inverno 1985", "Primavera 1985", "Verao 1986", "Outono 1986", "Inverno 1986", "Primavera 1986",
              "Verao 1987", "Outono 1987", "Inverno 1987", "Primavera 1987", "Verao 1988", "Outono 1988", "Inverno 1988", "Primavera 1988",
              "Verao 1989", "Outono 1989", "Inverno 1989", "Primavera 1989", "Verao 1990", "Outono 1990", "Inverno 1990", "Primavera 1990",
              "Verao 1991", "Outono 1991", "Inverno 1991", "Primavera 1991", "Verao 1992", "Outono 1992", "Inverno 1992", "Primavera 1992",
              "Verao 1993", "Outono 1993", "Inverno 1993", "Primavera 1993", "Verao 1994", "Outono 1994", "Inverno 1994", "Primavera 1994",
              "Verao 1995", "Outono 1995", "Inverno 1995", "Primavera 1995", "Verao 1996", "Outono 1996", "Inverno 1996", "Primavera 1996",
              "Verao 1997", "Outono 1997", "Inverno 1997", "Primavera 1997", "Verao 1998", "Outono 1998", "Inverno 1998", "Primavera 1998",
              "Verao 1999", "Outono 1999", "Inverno 1999", "Primavera 1999", "Verao 2000", "Outono 2000", "Inverno 2000", "Primavera 2000",
              "Verao 2001", "Outono 2001", "Inverno 2001", "Primavera 2001", "Verao 2002", "Outono 2002", "Inverno 2002", "Primavera 2002",
              "Verao 2003", "Outono 2003", "Inverno 2003", "Primavera 2003", "Verao 2004", "Outono 2004", "Inverno 2004", "Primavera 2004",
              "Verao 2005", "Outono 2005", "Inverno 2005", "Primavera 2005", "Verao 2006", "Outono 2006", "Inverno 2006", "Primavera 2006",
              "Verao 2007", "Outono 2007", "Inverno 2007", "Primavera 2007", "Verao 2008", "Outono 2008", "Inverno 2008", "Primavera 2008",
              "Verao 2009", "Outono 2009", "Inverno 2009", "Primavera 2009", "Verao 2010", "Outono 2010", "Inverno 2010", "Primavera 2010",
              "Verao 2011", "Outono 2011", "Inverno 2011", "Primavera 2011", "Verao 2012", "Outono 2012", "Inverno 2012", "Primavera 2012",
              "Verao 2013", "Outono 2013", "Inverno 2013", "Primavera 2013", "Verao 2014", "Outono 2014", "Inverno 2014", "Primavera 2014",
              "Verao 2015", "Outono 2015", "Inverno 2015", "Primavera 2015")
    names(resultado) <- prog
  } else {
    resultado <- vector("list", 55)
    prog  <- c("1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976",
               "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992",
               "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
               "2009", "2010", "2011", "2012", "2013", "2014", "2015")
    names(resultado) <- prog
  }
  
  datas <- c("1961-01-01", "1962-01-01", "1963-01-01", "1964-01-01", "1965-01-01", "1966-01-01", "1967-01-01", "1968-01-01", "1969-01-01", "1970-01-01",
             "1971-01-01", "1972-01-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01", "1977-01-01", "1978-01-01", "1979-01-01", "1980-01-01", 
             "1981-01-01", "1982-01-01", "1983-01-01", "1984-01-01", "1985-01-01", "1986-01-01", "1987-01-01", "1988-01-01", "1989-01-01", "1990-01-01",
             "1991-01-01", "1992-01-01", "1993-01-01", "1994-01-01", "1995-01-01", "1996-01-01", "1997-01-01", "1998-01-01", "1999-01-01", "2000-01-01",
             "2001-01-01", "2002-01-01", "2003-01-01", "2004-01-01", "2005-01-01", "2006-01-01", "2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01",
             "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01") # datas definidas que o código corta os dados, para cada ano.
  
  data_count <- 2 # itera no vetor datas.
  wee <- 1 # itera na posição a salvar da lista resultado.
  o <- 0 # faz as operações de cortar o data.frame.
  
  abs <- nrow(dado)
  dado_atual <- dado
  
  for(i in 1:abs){
    
    const <- as.character(dado_atual$Data[i])
    o <- o + 1
    
    if( const == datas[data_count] ){
      dado_novo <- dado[1:(o-1),] # quebra a cópia de max e min em cada ano.
      dado <- dado[-c(1:(o-1)),]
      
      if(sazonal == T){ # calcula para cada estação do ano
        ver_op <- subset(dado_novo, season == "verao")
        if(plota == T){
          plot_temps(ver_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(ver_op)
        wee <- wee + 1
        
        out_op <- subset(dado_novo, season == "outono")
        if(plota == T){
          plot_temps(out_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(out_op)
        wee <- wee + 1
        
        inv_op <- subset(dado_novo, season == "inverno")
        if(plota == T){
          plot_temps(inv_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(inv_op)
        wee <- wee + 1
        
        pri_op <- subset(dado_novo, season == "primavera")
        if(plota == T){
          plot_temps(pri_op, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(pri_op)
        wee <- wee + 1
        
        
      } else { # situação onde se calcula apenas o ano, sem considerar as estações.
        if(plota == T){
          plot_temps(dado_novo, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(dado_novo)
        wee <- wee + 1
      }
      
      data_count <- data_count + 1
      o <- 1
    }
    
    if( (const == "2015-01-01") & (sazonal == F) ){ # última parte do último ano é salva aqui.
      if(plota == T){
        plot_temps(dado, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(dado)
      break
      
    } else if( (const == "2015-01-01") & (sazonal == T)){ # última parte do último ano, calculando as estações, é salva aqui.
      
      ver_op <- subset(dado, season == "verao")
      if(plota == T){
        plot_temps(ver_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(ver_op)
      wee <- wee + 1
      
      out_op <- subset(dado, season == "outono")
      if(plota == T){
        plot_temps(out_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(out_op)
      wee <- wee + 1
      
      inv_op <- subset(dado, season == "inverno")
      if(plota == T){
        plot_temps(inv_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(inv_op)
      wee <- wee + 1
      
      pri_op <- subset(dado, season == "primavera")
      if(plota == T){
        plot_temps(pri_op, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(pri_op)
      
      break
    }
  }
  
  if(save == T){
    save_data(resultado)
  }
  
  return(resultado)
}

########################################################

# T U T O R I A L

########################################################

# CONFIRME O QUE DESEJA FAZER, E PARA TAL, APAGUE O " # " À FRENTE DO CÓDIGO.
# APÓS ISSO, RODE NORMALMENTE.

########################################################



#  1.
#  caso da estrutura do IAG.
#  com duas tabelas, uma de máximas, uma de mínimas.
#max <- read.table("tmax_dia.txt", header = T, sep = ",")
#min <- read.table("tmin_dia.txt", header = T, sep = ",")
#op <- convert_universal(max, min)

#  caso da estrutura de Mirante.
#  com uma única tabela, onde há informações de hora UTC, precipitação, temperatura máxima e mínima.
#data <- read.table("Estação - 83781_Mirante_1961_2016.txt", header = T, sep = ";")
#  caso trabalhe com o dia posterior na obtenção da tabela de dados, usa-se Mirante = T. Caso contrário, usando o mesmo dia, usa-se Mirante = F.
#op <- convert_universal(data, Mirante = T)



#  2.
#  agora, para conferir a quantidade de entradas NA no data.frame gerado (as entradas com NA serão salvas em um arquivo .csv), usa-se:
#confere_na(op)
#  para conferir inconsistência dos dados de entrada (será salvo em um arquivo .csv), usa-se:
#confere_cons(op)



#  3.
#  recomenda-se retirar as matrizes já geradas e colocá-las em pasta à parte, descritas corretamente, para que não hajam confusões.
#  alguns programas retornam muitas matrizes, o que pode tornar as coisas confusas, sem o devido cuidado.
#  para calcular a matriz dos dados todos (save = T salva em arquivo .csv a matriz gerada).
#resultado1 <- separa_temp(op, save = T)

#  para calcular as matrizes em quinquênios (5 em 5 anos).
#  aqui em diante: sazonal > se calcula a matriz em estações dentro do quinquênio (ou outros períodos, como 10 anos, anual, 30 anos).
#                  save > se salva em arquivo .csv as matrizes geradas.
#                  plota > se faz um gráfico dos dados separados.
#resultado2 <- quinquenal(op, sazonal = T, save = F, plota = F)

#  para calcular as matrizes em estações (4 estações para o data.frame inteiro.
#resultado3 <- sazonal(op, save = F, plota = F)

#  para calcular as matrizes em decênios (10 em 10 anos).
#resultado4 <- decenio(op, sazonal = T, save = F, plota = F)

#  para calcular as matrizes de trinta em trinta anos.
#resultado5 <- trinta(op, sazonal = T, save = F, plota = F)

#  para calcular as matrizes ano a ano.
#resultado6 <- anual(op, sazonal = T, save = T, plota = F)



#  4.
#  por fim, é possível pôr em gráfico(s) o que foi gerado em resultado1, resultado2, ... , resultado6. Isso é feito pela função graficaliza.
#  basta entrar com o resultadoi (i = 1, ... ,6), que foi calculado acima.
#  usando sazonal = T, o programa separa as estações, contando que o dado de entrada já possua as estações diferenciadas.
#  em outras palavras, usa-se sazonal = T aqui, quando para calcular o resultadoi (i = 1, ... , 6), a opção sazonal = T estava definida nele.
#graficaliza(resultado1, sazonal = F)
#graficaliza(resultado6, sazonal = T)