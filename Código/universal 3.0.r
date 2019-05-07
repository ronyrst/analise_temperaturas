#### FUNCIONALIDADES (a partir da linha 33):
# -'save_data' salva matriz, data.frame ou lista em um arquivo .csv.
# -'ctrab_na' para uso interno dos outros programas.
# -'ctrab_cons' para uso interno dos outros programas.
# -'confere_na' mostra quantos NAs existem em um data.frame j� na estrutura de convert_universal, os salvando em um arquivo .csv.
# -'confere_cons' mostra quantas inconsist�ncias h� em um data.frame j� na estrutura de convert_universal, os salvando em um arquivo .csv.
# -'plot_temps' gera gr�fico das temperaturas m�ximas e m�nimas.
# -'graficaliza' gera gr�fico(s) de barras das matrizes j� calculadas.
# -'checa_data' verifica se a data usada � do tipo YYYY-MM-DD ou DD/MM/YYYY.
# -'def_dia' recebe um string de data e o transforma em uma lista que separa dia, m�s e ano em valores num�ricos.
# -'compara_data' faz compara��o l�gica entre duas datas.
# -'separa_estacao' coloca a esta��o em cada uma das datas de um data.frame.

#### CONVERTER DADOS (a partir da linha 1013):
# -'convert_universal' fun��o que converte as estruturas do IAG e de Mirante para a estrutura aceita pelas fun��es aqui descritas.

#### MATRIZES (a partir da linha 1171):
# -'define_minima' para uso interno de separa_temp.
# -'define_maxima' para uso interno de separa_temp.
# -'separa_temp' calcula a matriz de m�ximas e m�nimas de um data.frame na estrutura de convert_universal.
# -'quinquenal' calcula matrizes de 5 em 5 anos, ou sazonalmente em cada per�odo de 5 anos, com o data.frame na estrutura de convert_universal.
# -'sazonal' calcula a matriz para cada uma das quatro esta��es de um data.frame na estrutura de convert_universal.
# -'decenio' calcula a matriz de 10 em 10 anos, ou sazonalmente em cada per�odo de 10 anos, com o data.frame na estrutura de convert_universal.
# -'trinta' calcula a matriz em per�odos de 30 anos, ou sazonalmente dentro de cada per�odo de 30 anos, com o data.frame na estrutura de convert_universal.
# -'anual'calcula a matriz ano a ano, ou sazonalmente em cada ano, com o data.frame j� na estrutura de convert_universal.

#### TUTORIAL (a partir da linha 1865):
# apresenta uma forma r�pida de uso dos programas abaixo.


########################################################

# F U N C I O N A L I D A D E S

########################################################
save_data <- function(dado, titulo = ""){
  #####
  # Entra com uma matriz, data.frame ou lista (com matriz e/ou data.frame), e com um t�tulo (do tipo string, opcional), e salva em arquivo no computador.
  # 
  #####
  
  if(is.matrix(dado)){ # se o dado � uma matriz, salva aqui.
    op <- paste(titulo, " matriz.csv", sep = "")
    write.csv2(dado, file = op)
    
  } else if(is.data.frame(dado)){ # se o dado � um data.frame, salva aqui.
    op <- paste(titulo, " data_frame.csv", sep = "")
    write.csv2(dado, file = op, quote = F)
    
  } else if(is.list(dado)){ # se � lista, salva aqui.
    guard <- names(dado)
    
    for(i in 1:length(dado)){
      op <- paste(i, ". ", titulo, " ", guard[i], ".csv", sep = "") # c�digo que define o t�tulo do arquivo. Fica, p/ ex: "1.  Verao.csv".
      
      if(is.matrix(dado[[i]])){
        write.csv2(dado[[i]], file = op)
      } else if(is.data.frame(dado[[i]])){
        write.csv2(dado[[i]], file = op, quote = F)
      } else {
        print(paste("A entrada de n�mero", i, "n�o � nem uma matriz nem um data.frame. Portanto, n�o foi salva.", sep = " "))
      }
    }
  } else {
    print("O dado entrado n�o � uma matriz, data.frame ou lista.")
    return(NULL)
  }
}

########################################################
ctrab_na <- function(dado, salva = F, altera = F){
  #####
  # Entra com um data.frame j� na estrutura de convert_universal, e checa se h� entradas NA nas colunas de m�ximas e de m�nimas.
  #  Ele necessariamente retorna o mesmo data.frame, mas com ou sem altera��es.
  #  Se o programa n�o encontrou NAs no data.frame, ele retorna o mesmo data.frame de entrada.
  #  Se o programa encontrou NAs e 'altera' = FALSE, o programa tamb�m retorna o mesmo data.frame de entrada.
  #
  # 'salva': booleano. Define se as linhas onde foram encontrados NAs s�o salvas ou n�o em um arquivo .csv � parte.
  #  Se TRUE: as linhas onde foram encontrados NAs s�o colocadas em um data.frame � parte e salvas.
  #  Se FALSE: nada � feito.
  #
  # 'altera': booleano. Define se as linhas que possuem NA s�o ou n�o retiradas do data.frame que retorna da fun��o.
  #  Se TRUE: as linhas s�o retiradas do data.frame 'dado', e retornadas.
  #  Se FALSE: as linhas permanecem no data.frame 'dado'. Nada � feito.
  #####
  
  contador <- 0 # guarda quantos NAs o data.frame possui.
  opera <- c() # guarda as posi��es dos NAs encontrados no data.frame.
  for( i in 1:nrow(dado) ){
    if(is.na(dado$tmin[i]) | is.na(dado$tmax[i])){ # se ou a coluna de m�nimas ou a coluna de m�ximas tiver NA, entra aqui.
      contador <- contador + 1
      opera <- c(opera, i)
    }
  }
  
  if(contador == 0){ # se contador for igual a zero, o for acima n�o encontrou NAs, ent�o nada � feito, e se retorna o data.frame original.
    return(dado)
  }
  
  if(salva == T){ # se salva � definido por TRUE, o programa faz o caminho de salvar as linhas com NA presente.
    
    perdidos <- data.frame() # isso � feito por esse data.frame.
    for( i in opera ){
      perdidos <- rbind(perdidos, dado[i,]) # rbind coloca a linha de dado dentro de perdidos.
    }
    save_data(perdidos, "NAs encontrados") # o data.frame finalizado � ent�o salvo usando a fun��o save_data.
  }
  
  if(altera == T){ # se a op��o altera for TRUE, � retirado do data.frame original as linhas em que h� NAs presentes.
    dado <- dado[-opera,]
  }
  return(dado)
}

########################################################
ctrab_cons <- function(dado, salva = F, altera = F){
  #####
  # Entra com um data.frame j� na estrutura de convert_universal, e checa se h� consist�ncia nos dados: se as m�nimas s�o maiores ou iguais �s m�ximas.
  #  Ele necessariamente retorna o mesmo data.frame, mas com ou sem altera��es.
  #  Se o programa n�o encontrou inconsist�ncias no data.frame, ele retorna o mesmo data.frame de entrada.
  #  Se o programa encontrou inconsist�ncias e 'altera' = FALSE, o programa tamb�m retorna o mesmo data.frame de entrada.
  #  O programa n�o faz nada ao encontrar temperaturas NA.
  #
  # 'salva': booleano. Define se as linhas onde foram encontradas inconsist�ncias s�o salvas ou n�o em um arquivo .csv � parte.
  #  Se TRUE: as linhas onde foram encontradas inconsist�ncias s�o colocadas em um data.frame � parte e salvas.
  #  Se FALSE: nada � feito.
  #
  # 'altera': booleano. Define se as linhas que possuem inconsist�ncias s�o ou n�o retiradas do data.frame que retorna da fun��o.
  #  Se TRUE: as linhas s�o retiradas do data.frame 'dado', e retornadas.
  #  Se FALSE: as linhas permanecem no data.frame 'dado'. Nada � feito.
  #####
  
  contador <- 0 # guarda quantas linhas inconsistentes o data.frame possui.
  opera <- c() # guarda as posi��es das linhas inconsistentes encontradas no data.frame.
  
  for( i in 1:nrow(dado) ){
    if( is.na(dado$tmin[i]) | is.na(dado$tmax[i]) ){
      next
      
    } else if( dado$tmin[i] >= dado$tmax[i] ){ # se o valor do dia da m�nima for maior que o da m�xima, entra aqui.
      contador <- contador + 1
      opera <- c(opera, i)
    }
  }
  
  if(contador == 0){ # se contador for igual a zero, o for acima n�o encontrou inconsist�ncias, ent�o nada � feito, e se retorna o data.frame original.
    return(dado)
  }
  
  if(salva == T){ # se salva � definido por TRUE, o programa faz o caminho de salvar as linhas com inconsist�ncias.
    
    perdidos <- data.frame() # perdidos � um data.frame criado, onde s�o salvas as linhas de inconsist�ncias.
    for( i in opera ){
      perdidos <- rbind(perdidos, dado[i,]) # rbind coloca a linha de dado dentro de perdidos.
    }
    save_data(perdidos, "Dados inconsistentes") # o data.frame finalizado � ent�o salvo usando a fun��o save_data.
  }
  
  if(altera == T){ # se a op��o altera for TRUE, s�o retiradas do data.frame original as linhas inconsistentes.
    dado <- dado[-opera,]
  }
  return(dado)
}

########################################################
confere_na <- function(dado){
  #####
  # Entra com um data.frame j� na estrutura de convert_universal, escreve um arquivo .csv, caso foram achados NAs no data.frame, ou printa
  #  uma mensagem, caso nenhuma linha com NAs foi encontrada.
  #  O programa n�o retorna nada.
  #####
  conf <- ctrab_na(dado, salva = T, altera = T) # faz uso da fun��o 'ctrab_na' - fun��o essa, que se usada em contextos errados, pode fazer outros
                                                #  programas retornarem erros.
                                                # para que isso n�o ocorra, � usada essa fun��o 'confere_na', que garante que o usu�rio a use 
                                                #  indiscriminadamente, sem maiores perigos � integridade dos outros programas.
  
  if(nrow(dado) == nrow(conf)){
    print("O data.frame entrado n�o possui linhas com NA.") # sai o print se n�o forem encontrados NAs no data.frame.
  }
  rm(conf)
}

########################################################
confere_cons <- function(dado){
  #####
  # Entra com um data.frame j� na estrutura de convert_universal, escreve um arquivo .csv, caso sejam encontradas inconsist�ncias no data.frame,
  #  ou printa uma mensagem, caso nenhuma linha com inconsist�ncias foi encontrada.
  #  O programa n�o retorna nada.
  #####
  conf <- ctrab_cons(dado, salva = T, altera = T) # faz uso da fun��o 'ctrab_cons' - fun��o essa, que se usada em contextos errados, pode fazer outros
                                                  #  programas retornarem erros.
                                                  # para que isso n�o ocorra, � usada essa fun��o 'confere_cons', que garante que o usu�rio a use 
                                                  #  indiscriminadamente, sem maiores perigos � integridade dos outros programas.
  
  if(nrow(dado) == nrow(conf)){
    print("O data.frame entrado n�o possui inconsist�ncias.") # sai o print se n�o foram encontradas inconsist�ncias no data.frame.
  }
  rm(conf)
}

########################################################
plot_temps <- function(dados, titulo = " "){
  #####
  # Entra com um data.frame de temperaturas m�ximas e m�nimas, e plota um gr�fico das temperaturas.
  #
  # 'titulo': string com texto que descreve o gr�fico, a entrar como subt�tulo no plot. 
  #####
  
  copia <- ctrab_na(dados, altera = T)
  i <- nrow(dados) - nrow(copia)
  
  mini <- min(copia[[2]]) # encontra a temperatura m�xima e a m�nima observadas.
  maxi <- max(copia[[3]]) # espera-se que a menor m�nima esteja no data.frame de m�nimas, e a maior m�xima no de m�ximas.
                          # levando em conta que o data.frame 'copia' tem a estrutura 'Data' 'tmin' 'tmax'. copia[[2]] � o 'tmin', copia[[3]] � o 'tmax'.
  
  if(i == 0){
    
    plot(copia[[3]], col = "red", type = "l", ylim = c(mini, maxi), xlab = "Dias do per�odo", ylab = "Temperatura", sub = titulo)
    points(copia[[2]], col = "blue", type = "l")
  } else{
    
    newtitulo <- paste(titulo, " (o data.frame original tinha ", i, " linhas com valores NA, que foram omitidos)", sep = "")
    plot(copia[[3]], col = "red", type = "l", ylim = c(mini, maxi), xlab = "Dias do per�odo", ylab = "Temperatura", sub = newtitulo)
    points(copia[[2]], col = "blue", type = "l")
  }
}

########################################################
graficaliza <- function(dado, sazonal = F){
  #####
  # Entra com matriz ou lista de matrizes, plota gr�fico(s) segundo o modelo de Est�vez, indicando a quantidade de dias em cada classifica��o de temperatura
  #  do per�odo presente na matriz. A classifica��o dos dias se d� por: "Muito frio", "frio", "fresco", "ameno", "quente", "muito quente" e "sufocante".
  #
  # 'sazonal': se TRUE, plota gr�ficos separando as esta��es, contando que a lista de matrizes j� possua as esta��es separadas. Se a lista original n�o possuir
  #  as esta��es j� separadas, o resultado n�o ser� correto (e possivelmente, nenhum erro ser� mostrado). Caso entre TRUE, mas 'dado' for apenas uma matriz, 
  #  nada � feito, e s� um gr�fico � gerado.
  #   Se FALSE (default), s� plota um gr�fico.
  #####
  
  ###
  if( is.matrix(dado) ){ # se o dado entrado for s� uma matriz.
    m <- matrix(rep(0, len = 14), nrow = 7, dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"))))
    # gera uma matriz de zeros, com 7 linhas, e nesse caso, 2 colunas.
    
    muitofrio <- as.numeric(dado[6,1]) + sum(dado[7,1:2]) # nesse conjunto de linhas, os valores de cada classifica��o s�o somados, dos presentes na matriz.
    frio <- sum(dado[4:5,1]) + sum(dado[5:6,2]) + sum(dado[6:7,3]) # essa estrutura se repete no c�digo mais vezes.
    fresco <- sum(dado[3:4,2]) + sum(dado[4:5,3]) + sum(dado[5:6,4])
    ameno <- sum(dado[2:3,3]) + sum(dado[3:4,4]) + sum(dado[4:5,5])
    quente <- sum(dado[1:2,4]) + sum(dado[2:3,5]) + sum(dado[3:4,6]) + as.numeric(dado[4,7])
    muitoquente <- as.numeric(dado[1,5]) + sum(dado[1:2,6]) + sum(dado[2:3,7])
    sufocante <- as.numeric(dado[1,7])
    
    m[,1] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante) # a matriz 'm' recebe os valores de cada classifica��o.
    
    barplot(m, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias", # a matriz � plotada.
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"))
    
  ###
  } else if( is.list(dado) & sazonal == F ){ # caso de que os dados entrados est�o em uma lista, e sazonal � FALSE (s� um gr�fico � plotado).
    
    ops <- ceiling(length(dado)/0.7) - length(dado) # ceiling arredonda um poss�vel valor quebrado para o menor inteiro maior que o n�mero.
                                                    # ou seja: 7.2 > 8,
                                                    #          7.8 > 8.
                                                    # length(dado)/0.7 � feito para se gerar, no total, uma matriz com 30% de colunas de zeros, o suficiente
                                                    # para que a legenda n�o tampe valor algum.
    contornos <- c(as.vector(names(dado)), rep(" ", ops)) # contornos � feito para criar "t�tulos" vazios para os 30% de colunas de zeros.
    
    m <- matrix(rep(0, len = (7*ceiling(length(dado)/0.7))), nrow = 7, # matriz � gerada.
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
  } else if( is.list(dado) & sazonal == T ){ # caso em que os dados entrados est�o em uma lista, e a sazonal � TRUE.
    
    o <- 1
    cont_ver <- c() # cria as "labels" para cada barra, no gr�fico final. As "labels" s�o separadas por esta��o.
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
    
    total <- length(dado)/4 # o "tamanho" de cada esta��o, do total das presentes na lista.
    if( total != round(total) ){ # se total for um valor quebrado, implica que n�o foi entrada uma lista com esta��es separadas.
                                 # � o �nico erro que pode ser acusado, caso se entre com dados incorretamente.
                                 # conv�m notar que: se os dados n�o forem de esta��es separadas, mas forem divis�veis por 4, nada ocorre aqui.
      print("Os dados entrados n�o condizem com o esperado.")
      return(NULL)
    }
    ops <- ceiling(total/0.7) - total # ceiling( total/0.7 ) possui a mesma argumenta��o de acima. Com os 30% de colunas vazias.
    
    cont_ver <- c(cont_ver, rep(" ", ops)) # adiciona as "labels" vazias, para os 30% de barras vazias geradas.
    cont_out <- c(cont_out, rep(" ", ops))
    cont_inv <- c(cont_inv, rep(" ", ops))
    cont_pri <- c(cont_pri, rep(" ", ops))
    
    m_verao <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, # 4 matrizes s�o geradas, uma para cada esta��o.
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_ver)))
    m_outono <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, 
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_out)))
    m_inverno <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, 
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_inv)))
    m_primavera <- matrix(rep(0, len = (7*ceiling(total/0.7))), nrow = 7, 
                dimnames = (list(c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), cont_pri)))
    
    qual <- 1 # 'qual' indica qual esta��o � a atual.
    coluna <- 1 # 'coluna' itera na... coluna das matrizes. � a coluna onde os dados devem ser entrados.
    for( i in dado ){ # itera nas matrizes presentes na lista 'dado'.
      muitofrio <- as.numeric(i[6,1]) + sum(i[7,1:2])
      frio <- sum(i[4:5,1]) + sum(i[5:6,2]) + sum(i[6:7,3])
      fresco <- sum(i[3:4,2]) + sum(i[4:5,3]) + sum(i[5:6,4])
      ameno <- sum(i[2:3,3]) + sum(i[3:4,4]) + sum(i[4:5,5])
      quente <- sum(i[1:2,4]) + sum(i[2:3,5]) + sum(i[3:4,6]) + as.numeric(i[4,7])
      muitoquente <- as.numeric(i[1,5]) + sum(i[1:2,6]) + sum(i[2:3,7])
      sufocante <- as.numeric(i[1,7])
      
      if( qual == 1){ # 'qual' = 1: ver�o.
        qual <- 2
        m_verao[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante) # os valores s�o adicionados na coluna da matriz de ver�o.
        
      } else if( qual == 2 ){ # 'qual' = 2: outono.
        qual <- 3
        m_outono[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante)
        
      } else if( qual == 3 ){ # 'qual' = 3: inverno.
        qual <- 4
        m_inverno[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante)
        
      } else if( qual == 4 ){ # 'qual' = 4: primavera.
        qual <- 1
        m_primavera[,coluna] <- c(muitofrio, frio, fresco, ameno, quente, muitoquente, sufocante)
        coluna <- coluna + 1 # coluna s� itera quando chega-se na esta��o de primavera, fazendo com que ver�o receba valores na pr�xima coluna, and so on.
      }
    }
    
    barplot(m_verao, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , # os gr�ficos s�o plotados.
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Ver�o")
    barplot(m_outono, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , 
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Outono")
    barplot(m_inverno, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , 
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Inverno")
    barplot(m_primavera, col = c("darkblue", "blue", "cadetblue1", "white", "yellow", "red", "darkred"), ylab = "Quantidade de dias" , 
            legend = c("muito frio", "frio", "fresco", "ameno", "quente", "muito quente", "sufocante"), sub = "Primavera")
    
  ###
  } else { # se o entrado n�o foi nem matriz nem uma lista: retorna erro.
    print("O dado entrado n�o condiz com o esperado.")
    return(NULL)
  }
}

########################################################
checa_data <- function(string){
  #####
  # Entra com string, sai com booleano, indicando o tipo de data que se est� lidando.
  #
  # Se TRUE: a data � do tipo YYYY-MM-DD.
  # Se FALSE: a data � do tipo DD/MM/YYYY.
  #####
  
  x <- as.character(string)
  a <- strsplit(x, " ")[[1]][1] # a recebe o string "data", separado da "hora".
  
  azero <- strsplit(a, "") # mais um split de string, agora quebrando caractere a caractere os dados.
  
  for(i in 1:length(azero[[1]])){
    if(azero[[1]][i] == "-"){ # se encontrar um "-", � do tipo YYYY-MM-DD.
      return(T)
    } else if(azero[[1]][i] == "/"){ # se encontrar um "/", � do tipo DD/MM/YYYY.
      return(F)
    }
  }
}

########################################################
def_dia <- function(data){
  #####
  # Recebe um string de data, retorna uma lista com o dia, o m�s, e o ano separados.
  # 
  #####
  
  nova <- strsplit(data, "")
  sabe <- checa_data(data) # 'sabe' define se o tipo de dado tratado � YYYY-MM-DD ou DD/MM/YYYY.
  
  resp <- vector("list", 3) # resp � a lista a ser retornada, com os valores de dia, m�s e ano.
  names(resp) <- c("dia", "m�s", "ano")
  op <- c()
  
  if(sabe == T){ # YYYY-MM-DD
    count <- 3 # define a posi��o que os valores entrar�o na lista 'resp'.
    for(i in 1:length(nova[[1]])){
      
      if(nova[[1]][i] == "-"){ # serve para pegar os dois primeiros valores num�ricos do string. No caso, YYYY e MM.
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
    for(i in 1:length(op)){ # pega o �ltimo valor num�rico do string, no caso, DD.
      opa <- paste(opa, op[i], sep = "")
    }
    
    resp[count] <- as.numeric(opa)
    return(resp)
  
    
  } else if(sabe == F){ # DD/MM/YYYY
    count <- 1 # define a posi��o que os valores entrar�o na lista 'resp'.
    for(i in 1:length(nova[[1]])){
      
      if(nova[[1]][i] == "/"){ # ao encontrar "/", junta todos os strings em um valor num�rico. Pega DD e MM.
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
  # Recebe duas datas, e as compara. Retorna booleano, de acordo com a veracidade da compara��o realizada.
  #  As datas podem estar ou como string, ou como lista, conforme 'def_dia'.
  # 
  # 'string': pode receber as entradas "=", "!=", ">", "<", ">=", "<=".
  #
  # A fun��o compara a primeira data entrada com a segunda. Na forma: data1 *string_de_compara��o* data2.
  #  Se a compara��o for verdadeira, retorna True, caso contr�rio, retorna False.
  #####
  
  if(is.character(data1)){ # checa se data1 � string, o converte a lista.
    data1 <- def_dia(data1)
  }
  
  if(is.character(data2)){ # checa se data2 � string, o converte a lista.
    data2 <- def_dia(data2)
  }
  
  if((!is.list(data1)) & (!is.list(data2))){ # se ambas as datas n�o s�o listas: o programa encontrou um erro e termina, retornando NULL.
    print("Os valores entrados em compara_data n�o puderam ser comparados.")
    return(NULL)
  } else {
    nula <- NULL # nula checa se os dias, meses e anos s�o iguais.
    if( (data1[[1]] == data2[[1]]) & (data1[[2]] == data2[[2]]) & (data1[[3]] == data2[[3]]) ){
      nula <- T
    } else {
      nula <- F
    }
    
    if(nula == T){ # nula lida com situa��es que falsificam ou n�o a compara��o, dado seu resultado.
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

    if(string == ">" | string == ">="){ # situa��es que sobram, das anteriores, de ser maior ou maior-igual ( ">" e ">=" ).
      if(data1[[3]] > data2[[3]]){ # compara se um ano � maior que o outro.
        return(T)
      } else if(data1[[3]] < data2[[3]]){
        return(F)
      } else if(data1[[2]] > data2[[2]]){ # compara se um m�s � maior que o outro.
        return(T)
      } else if(data1[[2]] < data2[[2]]){
        return(F)
      } else{ # caso ambos os casos falhem, compara os dias.
        if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 1)){ # checa de Janeiro a Dezembro, levando em conta bissextos, para ver se um dia � maior que outro.
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
        } else if( (data1[[2]] == data2[[2]]) & (data1[[2]] == 2)){ # roda a compara��o de Fevereiro, onde checa os bissextos.
          bis <- NULL
          ao <- 1960
          
          for(i in 1:20){ # checa se o ano comparado � bissexto.
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
          ## Mar�o
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
    
    if(string == "<" | string == "<="){ # realiza as mesmas opera��es de acima, mas para menor e menor-igual ( "<" e "<=" ).
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
          ## Mar�o
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
  # Entra com data.frame de temperaturas m�ximas e m�nimas, e retorna o data.frame original com uma coluna a mais,
  #  indicando a esta��o do ano que a data pertence.
  #
  # O c�digo funciona apenas com o modelo 'dia' 'tmin' 'tmax', adicionando logo ap�s 'tmax' a coluna 'season'.
  #
  # datas das esta��es do ano coletadas de:
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
               "2015-03-20", "2015-06-21", "2015-09-23", "2015-12-22", "final") # datas dos equin�cios e solst�cios, para mudan�a de esta��o.
                                                                                #  O string "final" � para que o c�digo n�o d� erro ao terminar
                                                                                #   a �ltima mudan�a de esta��o na checagem de verdadeiro/falso
                                                                                #    do if abaixo. Fora disso, � in�til.
  if(ncol(dado) != 3){
    print("O n�mero de colunas n�o � o esperado no data.frame de entrada.")
    print("O c�digo n�o foi rodado.")
    return(NULL)
  }
  
  estacao <- c("verao", "outono", "inverno", "primavera")
  dado$season <- c(1:nrow(dado)) # � adicionado ao data.frame 'dado' a coluna 'season', que � preenchida de 1 ao n�mero de linhas total do data.frame.
  
  conta <- 1 # itera no vetor seasons.
  guarda_est <- 1 # indica a esta��o do vetor estacao.
  
  for(i in 1:(nrow(dado))){
    anovo <- dado[i, 1] # anovo recebe a data da linha i do data.frame 'dado'.
    
    if(anovo == seasons[conta]){ # caso chegou em uma data de mudan�a de esta��o, o c�digo faz a esta��o mudar.
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
    
    dado[i,]$season <- estacao[guarda_est] # guarda em dado a esta��o que a data pertence.
  }
  
  return(dado)
}

########################################################

# C O N V E R T E R   D A D O S 

########################################################
convert_universal <- function(dado1, dado2 = NA, Mirante = T){
  #####
  # Entra com um ou dois data.frames: tabelas de dados de temperaturas m�ximas e m�nimas (dois data.frames: um de m�ximas, outro de m�nimas, padr�o IAG;
  #  um data.frame: com m�ximas e m�nimas no mesmo arquivo, padr�o Mirante) e retorna um �nico data.frame com temperaturas m�ximas e m�nimas dos dias
  #  01/01/1961 a 31/12/2015.
  #  O programa adiciona NA a dados faltantes.
  #   A estrutura esperada do padr�o IAG: [Data, tmax], [Data, tmin].
  #   A estrutura esperada do padr�o Mirante: [__, Data, Hora, __, TempMaxima, TempMinima]
  # 
  # 'Mirante': se a estrutura a ser usada nos dados seguindo padr�o Mirante usa o dia posterior ou o mesmo dia, para a coleta de temperaturas.
  #  O padr�o � dia posterior, com 'Mirante' = T. Toma-se que a hora � em padr�o UTC, o que implica que a coleta do dia posterior, �s 00:00 horas UTC
  #  na verdade condiz com o dia anterior, �s 21 horas. Por isso esse m�todo se vale de pegar o valor coletado �s 12:00 horas UTC do mesmo dia, como
  #  temperatura m�nima, e o valor do dia posterior, coletado �s 00:00 horas UTC, como o valor da temperatura m�xima daquele dia.
  #   Usando 'Mirante' = F, os dados s�o arranjados de forma a, no dia i, a temperatura m�nima desse dia � a coletada �s 12:00 horas UTC, e a temperatura
  #   m�xima � a coletada �s 00:00 horas UTC do mesmo dia i, no data.frame original.
  #####
  
  op <- ncol(dado1) # op checa se o data.frame dado1 � um padr�o IAG ou Mirante.
  
  if( (op == 2) & (is.data.frame(dado2)) ){ # se h� um data.frame em dado2, e o tamanho de op � 2, ent�o � padr�o IAG.
    trabalho <- T
  } else if( (op > 2) & (is.na(dado2)) ){ # se n�o h� um data.frame em dado2, e op � maior que 2, � padr�o Mirante.
    trabalho <- F
  }
  
  if(trabalho == T){ # aqui, h� uma segunda rechecagem, para saber se o data.frame IAG tem as datas formatadas em padr�o IAG.
    ea <- checa_data(dado1[1,1])
    eb <- checa_data(dado2[1,1])
    
    if( (ea == eb) & (ea == trabalho) ){
      ea <- "a"
    } else {
      print("H� um erro na entrada dos data.frames. Certifique-se de que ambos s�o dados do IAG (temperaturas m�ximas e m�nimas).")
      return(NULL) # retorna NULL se algo errado, em qualquer das duas rechecagens.
    }
    
    rm(ea, eb)
    
  } else if(trabalho == F){ # outra segunda rechecagem, mas para o padr�o Mirante, para ver a formata��o das datas, tamb�m.
    
    ea <- checa_data(dado1[1,2])
    
    if(ea == trabalho){
      ea <- "b"
    } else {
      print("H� um erro na entrada do data.frame. Certifique-se de � o tipo de data.frame de Mirante.")
      return(NULL)
    }
    
    rm(ea)
  }
  
  dia <- read.table("dias.txt") # a forma que cheguei para obter o data.frame de datas foi deixar um arquivo em separado com os valores de cada data.
                                # esse data.frame � acessado cada vez que o programa roda, portanto.
  dia$tmax <- c(1:nrow(dia)) # o data.frame criado recebe uma coluna de tmax e outra de tmin.
  dia$tmin <- c(1:nrow(dia))
  names(dia) <- c("Data", "tmin", "tmax") # a ordem � Data, tmin, e tmax.
  
  ######
  if(trabalho == T){ # caso de trabalharmos com estrutura IAG.
    
    opera_dado1 <- 1 # opera_dado1 e opera_dado2 s�o duas itera��es que s�o realizadas al�m das feitas pelo for.
    opera_dado2 <- 1
    
    for(i in 1:nrow(dia)){
      
      maxa <- as.vector(dado1[opera_dado1,1])
      mina <- as.vector(dado2[opera_dado2,1])
      maxa_n <- strsplit(maxa, " ")[[1]][1] # peda�o de c�digo usado para extrair apenas os valores de data da estrutura IAG.
      mina_n <- strsplit(mina, " ")[[1]][1] # uma vez que as datas aqui s�o da forma "data hora". A "hora" � retirada, para que o programa avance.
      
      if(compara_data(as.character(dia[i,1]), maxa_n, "=")){ # usa-se compara_data para... comparar as datas de dia e do data.frame de m�ximas, e colocar 
                                                             # os valores.
        dia[i,3] <- dado1[opera_dado1, 2]
        opera_dado1 <- opera_dado1 + 1 # opera_dado1 e opera_dado2 itera aqui, para saber a posi��o que o data.frame entrado est�.
                                       # isso porque podem haver saltos nos dados coletados, e isso n�o seria captado se todos fossem iterados pelo for.
      } else { # se a compara��o deu resultado negativo, ent�o esse dia n�o possui o valor de temperatura coletado, e recebe NA.
        dia[i,3] <- NA
      }
      
      if(compara_data(as.character(dia[i,1]), mina_n, "=")){ # mesmo processo descrito acima, mas lidando com a temperatua m�nima, do outro data.frame.
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
        if(as.character(dado1$Data[opera_dado1]) != "01/01/1961"){ # situa��o em que o primeiro valor do data.frame padr�o Mirante � descartado, pois condiz com
                                                                   # a data de 31/12/1960.
          if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 0000) ){ # se � 0000, � m�xima, 
                                                                                                                                      # e vai pra data anterior
                                                                                                                                      # � atual.
            dia[i-1,3] <- dado1$TempMaxima[opera_dado1]
            opera_dado1 <- opera_dado1 + 1
          
          } else { # caso contr�rio, o data.frame padr�o Mirante n�o tem o dado, e entra NA.
            dia[i-1,3] <- NA
          }
        }
        
        
        if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 1200) ){ # se � 1200, � m�nima, e
                                                                                                                                    # vai para a data atual.
          dia[i, 2] <- dado1$TempMinima[opera_dado1]
          opera_dado1 <- opera_dado1 + 1
        
        } else { # caso n�o haja no data.frame padr�o Mirante, entra NA.
          dia[i, 2] <- NA
        }
      }
      
      dia[i,3] <- dado1$TempMaxima[opera_dado1] # Coloca a m�xima do dia 31/12/2015.
      
    ## ----
    } else if(Mirante == F){ # se Mirante = F, usaremos o mesmo dia para pegar os dados.
      
      opera_dado1 <- 1
      
      for(i in 1:nrow(dia)){
        
        if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 0000) ){ # se � 0000, entra como 
                                                                                                                                    # temperatura m�xima do 
                                                                                                                                    # dia atual.
          dia[i,3] <- dado1$TempMaxima[opera_dado1]
          opera_dado1 <- opera_dado1 + 1
        } else { # se n�o tem a m�xima, entra como NA.
          dia[i,3] <- NA
        }
        
        if( compara_data(as.character(dia[i,1]), as.character(dado1$Data[opera_dado1]), "=") & (dado1$Hora[opera_dado1] == 1200) ){ # se � 1200, entra como 
                                                                                                                                    # temperatura m�nima do 
                                                                                                                                    # dia atual.
          dia[i, 2] <- dado1$TempMinima[opera_dado1]
          opera_dado1 <- opera_dado1 + 1
        } else { # caso contr�rio, entra NA.
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
define_minima <- function(mima){ # encontra a posi��o na matriz da temperatura m�nima obtida.
  #####
  # Entra com um valor de temperatura m�nima, define em que linha da matriz ela entrar�.
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
define_maxima <- function(maxa){ # encontra a posi��o na matriz da temperatura m�xima obtida.
  #####
  # Entra com um valor de temperatura m�xima, define em que coluna da matriz ela entrar�.
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
    
    resultado[mbm,mam] <- resultado[mbm,mam] + 1 # e mbm e mam s�o encaixadas como coordenadas da matriz resposta.
  }
  
  if( save == T ){
    save_data(resultado)
  }
  
  return(resultado)
}

########################################################
quinquenal <- function(dado, sazonal = F, save = F, plota = F){
  #####
  # Entra com data.frame de temperaturas m�ximas e m�nimas, retorna as matrizes calculadas quinqu�nio a quinqu�nio (5 em 5 anos).
  #
  # 'sazonal': se TRUE, calcula as matrizes para cada esta��o de cada quinqu�nio. Sendo, ent�o, 4 matrizes para cada quinqu�nio.
  #   Se FALSE (default), cada quinqu�nio s� ter� calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada quinqu�nio (ou cada esta��o de cada quinqu�nio, a depender de 'sazonal') ter� um gr�fico plotado de seus valores.
  #####
  
  ####
  if(sazonal == T){ # essa estrutura se repete no resto do c�digo. Se a op��o sazonal � escolhida, o c�digo declara vari�veis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a esta��o de cada data da tabela.
  }
  
  ####
  if(sazonal == T){ # peda�o de c�digo onde se declara a lista que retornar� os resultados.
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
             "1996-01-01", "2001-01-01", "2006-01-01", "2011-01-01") # datas para quebrar nos quinqu�nios.
  
  data_count <- 2 # itera no vetor datas.
  wee <- 1 # itera na posi��o a salvar da lista resultado.
  o <- 0 # faz as opera��es de cortar o data.frame.
  
  abs <- nrow(dado)
  dado_atual <- dado
  
  ####
  for(i in 1:abs){
    o <- o + 1
    const <- as.character(dado_atual$Data[i])
    
    ###
    if( const == datas[data_count] ){
      dado_novo <- dado[1:(o-1),] # quebra dado no quinqu�nio "encontrado".
      dado <- dado[-c(1:(o-1)),]
      
      if(sazonal == T){ # calcula para cada esta��o do quinqu�nio obtido.
        
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

      } else { # situa��o onde se calcula apenas o quinqu�nio, sem considerar as esta��es.
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
    if( (const == "2011-01-01") & (sazonal == F) ){ # �ltima parte do �ltimo quinqu�nio, � salva aqui.
      if(plota == T){
        plot_temps(dado, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(dado)
      break
      
    } else if( (const == "2011-01-01") & (sazonal == T)){ # �ltima parte do �ltimo quinqu�nio, calculando as esta��es,
      
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
  # Entra com data.frame de temperaturas, retorna 4 matrizes calculadas: uma para cada esta��o presente no data.frame inteiro.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada esta��o ter� um gr�fico plotado de seus valores.
  #####
  
  dado <- separa_estacao(dado) # fun��o que coloca, para cada dia do data.frame original, a esta��o do ano � qual aquele dia pertence.
  
  resultado <- vector("list", 4)
  names(resultado) <- c("Ver�o", "Outono", "Inverno", "Primavera") # identifica cada matriz de resposta com o t�tulo da esta��o.
  
  ver_op <- subset(dado, season == "verao") # com os valores das esta��es nos data.frames, � s� fazer subset dos dados,
  if(plota == T){                           # e com o subset realizar a entrada na fun��o separa_temp.
    plot_temps(ver_op, titulo = "Ver�o")
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
  # Entra com data.frame de temperaturas, retorna as matrizes calculadas dec�nio a dec�nio (10 em 10 anos).
  #
  # 'sazonal': se TRUE, calcula as matrizes para cada esta��o de cada dec�nio. Sendo, ent�o, 4 matrizes para cada dec�nio.
  #   Se FALSE (default), cada dec�nio s� ter� calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada dec�nio (ou cada esta��o de cada dec�nio, a depender de 'sazonal') ter� um gr�fico plotado de seus valores.
  #####
  
  if(sazonal == T){ # essa estrutura se repete no resto do c�digo. Se a op��o sazonal � escolhida, o c�digo declara vari�veis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a esta��o de cada data das tabelas.
  }
  
  if(sazonal == T){ # peda�o de c�digo onde se declara a lista que retornar� os resultados.
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
  
  datas <- c("1961-01-01", "1971-01-01", "1981-01-01", "1991-01-01", "2001-01-01", "2011-01-01") # datas para quebrar nos dec�nios.
  
  data_count <- 2 # itera no vetor datas.
  wee <- 1 # itera na posi��o a salvar da lista resultado.
  o <- 0 # faz as opera��es de cortar o data.frame.
  
  abs <- nrow(dado)
  dado_atual <- dado
  
  for(i in 1:abs){
    o <- o + 1
    const <- as.character(dado_atual$Data[i])
    
    if( const == datas[data_count] ){
      dado_novo <- dado[1:(o-1),] # quebra dado no dec�nio "encontrado".
      dado <- dado[-c(1:(o-1)),]
      
      if(sazonal == T){ # calcula para cada esta��o do dec�nio obtido.
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
        
      } else { # situa��o onde se calcula apenas o dec�nio, sem considerar as esta��es.
        if(plota == T){
          plot_temps(dado_novo, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(dado_novo)
        wee <- wee + 1
      }
      
      data_count <- data_count + 1
      o <- 1
    }
    
    if( const == "2011-01-01" ){ # nos 5 anos finais, o c�digo para, uma vez que ele n�o completa um dec�nio.
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
  # 'sazonal': se TRUE, calcula as matrizes para cada esta��o de cada per�odo de trinta anos. Sendo, ent�o, 4 matrizes para cada per�odo.
  #   Se FALSE (default), cada per�odo s� ter� calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada per�odo de trinta anos (ou cada esta��o de cada per�odo, a depender de 'sazonal') ter� um gr�fico plotado de seus valores.
  #####
  
  if(sazonal == T){ # essa estrutura se repete no resto do c�digo. Se a op��o sazonal � escolhida, o c�digo declara vari�veis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a esta��o de cada data.
  }
  
  if(sazonal == T){ # peda�o de c�digo onde se declara a lista que retornar� os resultados.
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
  
  dataA <- c("1961-01-01", "1971-01-01", "1981-01-01", "fim") # datas que s�o importantes para que o c�digo rode. "fim", aqui, serve
                                                              # para que o programa n�o retorne um erro, ao terminar de iterar as datas desse vetor.
  dataZ <- c("1991-01-01", "2001-01-01", "2011-01-01")
  
  a1 <- 0 # marcam as datas de in�cio de cada per�odo de 30 anos.
  a2 <- 0
  a3 <- 0
  a <- 1
  operaA <- 1 # itera em dataA.
  
  z1 <- 0 # marcam as datas de fim de cada per�odo de 30 anos.
  z2 <- 0
  z3 <- 0
  z <- 1
  operaZ <- 1 # itera em dataZ.
  
  wee <- 1 # itera na posi��o a salvar da lista resultado.
  abs <- nrow(dado) # n�mero de linhas do vetor original.
  dado_atual <- dado # c�pia simples do vetor original.
  
  for(i in 1:abs){
    
    const <- as.character(dado_atual$Data[i])
    
    if( const == dataA[operaA] ){ # a fun��o de ambos os ifs � de guardar as posi��es que demarcam um per�odo de 30 anos nos dados.
      if(a == 1){ # indica os in�cios dos per�odos.
        a1 <- i
      } else if (a == 2){
        a2 <- i
      } else if (a == 3){
        a3 <- i
      }
      a <- a + 1
      operaA <- operaA + 1
    }
    
    if( const == dataZ[operaZ] ){ # indica os fins dos per�odos.
      if(z == 1){
        z1 <- i - 1
      } else if (z == 2){
        z2 <- i - 1
      } else if (z == 3){
        z3 <- i - 1
        break # ao chegar na �ltima data, n�o � necess�rio andar pelo resto dos dados, por isso o break.
      }
      z <- z + 1
      operaZ <- operaZ + 1
    }
  }
  
  for(i in 1:3){ # aqui, os dados s�o tratados em matrizes.
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
    
    
    if(sazonal == T){ # caso de cada esta��o, nos 30 anos.
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
      
      if(plota == T){ # se plota for marcado como TRUE, ele faz o plot dos dados para cada esta��o.
        plot_temps(ver_op, titulo = prog[wee-4])
        plot_temps(out_op, titulo = prog[wee-3])
        plot_temps(inv_op, titulo = prog[wee-2])
        plot_temps(pri_op, titulo = prog[wee-1])
      }
      
    } else { # situa��o sem considerar as esta��es.
      if(plota == T){
        plot_temps(dado_novo, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(dado_novo)
      wee <- wee + 1
    }
  }
  
  if(save == T){
    save_data(resultado) # se nada errado for identificado, os dados s�o salvos, se a op��o foi dada na chamada da fun��o.
  }
  
  return(resultado) # e ent�o, vem o return dos dados j� analisados, em matrizes.
}

########################################################
anual <- function(dado, sazonal = F, save = F, plota = F){
  #####
  # Entra com data.frame de temperaturas, retorna as matrizes calculadas ano a ano.
  #
  # 'sazonal': se TRUE, calcula as matrizes para cada esta��o de cada ano. Sendo, ent�o, 4 matrizes para cada ano.
  #   Se FALSE (default), cada ano "s�" ter� calculada uma matriz.
  # 
  # 'save': se TRUE, salva todas as matrizes calculadas em arquivos separados, devidamente nomeados.
  #
  # 'plota': se TRUE, cada ano (ou cada esta��o de cada ano, a depender de 'sazonal') ter� um gr�fico plotado de seus valores.
  #####
  
  if(sazonal == T){ # essa estrutura se repete no resto do c�digo. Se a op��o sazonal � escolhida, o c�digo declara vari�veis diferentes.
    dado <- separa_estacao(dado) # adiciona a coluna com a esta��o de cada data das tabelas.
  }
  
  if(sazonal == T){ # peda�o de c�digo onde se declara a lista que retornar� os resultados.
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
             "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01") # datas definidas que o c�digo corta os dados, para cada ano.
  
  data_count <- 2 # itera no vetor datas.
  wee <- 1 # itera na posi��o a salvar da lista resultado.
  o <- 0 # faz as opera��es de cortar o data.frame.
  
  abs <- nrow(dado)
  dado_atual <- dado
  
  for(i in 1:abs){
    
    const <- as.character(dado_atual$Data[i])
    o <- o + 1
    
    if( const == datas[data_count] ){
      dado_novo <- dado[1:(o-1),] # quebra a c�pia de max e min em cada ano.
      dado <- dado[-c(1:(o-1)),]
      
      if(sazonal == T){ # calcula para cada esta��o do ano
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
        
        
      } else { # situa��o onde se calcula apenas o ano, sem considerar as esta��es.
        if(plota == T){
          plot_temps(dado_novo, titulo = prog[wee])
        }
        resultado[[wee]] <- separa_temp(dado_novo)
        wee <- wee + 1
      }
      
      data_count <- data_count + 1
      o <- 1
    }
    
    if( (const == "2015-01-01") & (sazonal == F) ){ # �ltima parte do �ltimo ano � salva aqui.
      if(plota == T){
        plot_temps(dado, titulo = prog[wee])
      }
      resultado[[wee]] <- separa_temp(dado)
      break
      
    } else if( (const == "2015-01-01") & (sazonal == T)){ # �ltima parte do �ltimo ano, calculando as esta��es, � salva aqui.
      
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

# CONFIRME O QUE DESEJA FAZER, E PARA TAL, APAGUE O " # " � FRENTE DO C�DIGO.
# AP�S ISSO, RODE NORMALMENTE.

########################################################



#  1.
#  caso da estrutura do IAG.
#  com duas tabelas, uma de m�ximas, uma de m�nimas.
#max <- read.table("tmax_dia.txt", header = T, sep = ",")
#min <- read.table("tmin_dia.txt", header = T, sep = ",")
#op <- convert_universal(max, min)

#  caso da estrutura de Mirante.
#  com uma �nica tabela, onde h� informa��es de hora UTC, precipita��o, temperatura m�xima e m�nima.
#data <- read.table("Esta��o - 83781_Mirante_1961_2016.txt", header = T, sep = ";")
#  caso trabalhe com o dia posterior na obten��o da tabela de dados, usa-se Mirante = T. Caso contr�rio, usando o mesmo dia, usa-se Mirante = F.
#op <- convert_universal(data, Mirante = T)



#  2.
#  agora, para conferir a quantidade de entradas NA no data.frame gerado (as entradas com NA ser�o salvas em um arquivo .csv), usa-se:
#confere_na(op)
#  para conferir inconsist�ncia dos dados de entrada (ser� salvo em um arquivo .csv), usa-se:
#confere_cons(op)



#  3.
#  recomenda-se retirar as matrizes j� geradas e coloc�-las em pasta � parte, descritas corretamente, para que n�o hajam confus�es.
#  alguns programas retornam muitas matrizes, o que pode tornar as coisas confusas, sem o devido cuidado.
#  para calcular a matriz dos dados todos (save = T salva em arquivo .csv a matriz gerada).
#resultado1 <- separa_temp(op, save = T)

#  para calcular as matrizes em quinqu�nios (5 em 5 anos).
#  aqui em diante: sazonal > se calcula a matriz em esta��es dentro do quinqu�nio (ou outros per�odos, como 10 anos, anual, 30 anos).
#                  save > se salva em arquivo .csv as matrizes geradas.
#                  plota > se faz um gr�fico dos dados separados.
#resultado2 <- quinquenal(op, sazonal = T, save = F, plota = F)

#  para calcular as matrizes em esta��es (4 esta��es para o data.frame inteiro.
#resultado3 <- sazonal(op, save = F, plota = F)

#  para calcular as matrizes em dec�nios (10 em 10 anos).
#resultado4 <- decenio(op, sazonal = T, save = F, plota = F)

#  para calcular as matrizes de trinta em trinta anos.
#resultado5 <- trinta(op, sazonal = T, save = F, plota = F)

#  para calcular as matrizes ano a ano.
#resultado6 <- anual(op, sazonal = T, save = T, plota = F)



#  4.
#  por fim, � poss�vel p�r em gr�fico(s) o que foi gerado em resultado1, resultado2, ... , resultado6. Isso � feito pela fun��o graficaliza.
#  basta entrar com o resultadoi (i = 1, ... ,6), que foi calculado acima.
#  usando sazonal = T, o programa separa as esta��es, contando que o dado de entrada j� possua as esta��es diferenciadas.
#  em outras palavras, usa-se sazonal = T aqui, quando para calcular o resultadoi (i = 1, ... , 6), a op��o sazonal = T estava definida nele.
#graficaliza(resultado1, sazonal = F)
#graficaliza(resultado6, sazonal = T)