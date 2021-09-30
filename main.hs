import Main (Item(Jogador))
-- Presentes ou bomba
data Objeto = Patins | Arremesso | Bomba deriving (Show, Eq)

-- Auxiliares para o jogador
type Capacidades = ((Objeto, Int), (Objeto, Int), (Objeto, Int))
type Posicao = (Int, Int)
type ID = Int

-- Jogador
type Jogador = (ID, Posicao, Char, Capacidades) 

-- Itens
data Item = Grama | Objeto Objeto | Parede | Pedra | Jogador Int deriving (Show, Eq)

-- Tabuleiro 
type Celula = [Item]
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)

conf :: Celula -> Bool
conf [Grama] = True
conf [Grama, Jogador _] = True
conf [Grama, Objeto _] = True
conf [Grama, Objeto Arremesso, Parede] = True
conf [Grama, Objeto Patins, Parede] = True
conf [Grama, Parede] = True
conf [Pedra] = True
conf [Parede] = True
conf c
  | null c = True
  |otherwise = False

criaTabuleiro :: Tabuleiro -> Tabuleiro
criaTabuleiro t@(
  (c1, c2, c3, c4, c5, c6, c7, c8),
  (c9, c10, c11, c12, c13, c14, c15, c16),
  (c17, c18, c19, c20, c21, c22, c23, c24),
  (c25, c26, c27, c28, c29, c30, c31, c32),
  (c33, c34, c35, c36, c37, c38, c39, c40),
  (c41, c42, c43, c44, c45, c46, c47, c48),
  (c49, c50, c51, c52, c53, c54, c55, c56),
  (c57, c58, c59, c60, c61, c62, c63, c64)
  ) = if l1 && l2 && l3 && l4 && l5 && l6 && l7 && l8 then t
                                                      else error "Tabuleiro inválido"
  where l1 = conf c1 && conf c2 && conf c3 && conf c4 && conf c5 && conf c6 && conf c7 && conf c8
        l2 = conf c9 && conf c10 && conf c11 && conf c12 && conf c13 && conf c14 && conf c15 && conf c16
        l3 = conf c17 && conf c18 && conf c19 && conf c20 && conf c21 && conf c22 && conf c23 && conf c24
        l4 = conf c25 && conf c26 && conf c27 && conf c28 && conf c29 && conf c30 && conf c31 && conf c32
        l5 = conf c33 && conf c34 && conf c35 && conf c36 && conf c37 && conf c38 && conf c39 && conf c40
        l6 = conf c41 && conf c42 && conf c43 && conf c44 && conf c45 && conf c46 && conf c47 && conf c48
        l7 = conf c49 && conf c50 && conf c51 && conf c52 && conf c53 && conf c54 && conf c55 && conf c56
        l8 = conf c57 && conf c58 && conf c59 && conf c60 && conf c61 && conf c62 && conf c63 && conf c64

linha1 :: Linha
linha1 = ([Grama, Jogador 1], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha2 :: Linha
linha2 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha3 :: Linha
linha3 = ([Grama], [Grama], [Grama], [Grama], [Grama, Objeto Patins], [Grama], [Grama], [Grama])

linha4 :: Linha
linha4 = ([Grama], [Grama], [Grama], [Grama, Parede], [Grama, Objeto Bomba], [Grama, Jogador 2], [Grama], [Grama])

linha5 :: Linha
linha5 = ([Grama], [Grama], [Grama], [Grama], [Grama, Parede], [Grama], [Grama], [Grama])

linha6 :: Linha
linha6 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha7 :: Linha
linha7 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha8 :: Linha
linha8 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

tab :: Tabuleiro
tab = (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8)

jogadores :: [Jogador]
jogadores = [(1, (0,0), 'N', ((Patins, 0),(Arremesso, 3),(Bomba, 1))), (2, (7,7), 'S', ((Patins, 0),(Arremesso, 0),(Bomba, 1)))]

-- Recebe a linha antes de ser modificada, o indice da celula que vai mudar, e a celula em si
-- Retornando uma nova linha com a celula nova
novaLinha :: Linha -> Int -> Celula -> Linha
novaLinha (c0, c1, c2, c3, c4, c5, c6, c7) c cel
  | c == 0 = (cel, c1, c2, c3, c4, c5, c6, c7)
  | c == 1 = (c0, cel, c2, c3, c4, c5, c6, c7)
  | c == 2 = (c0, c1, cel, c3, c4, c5, c6, c7)
  | c == 3 = (c0, c1, c2, cel, c4, c5, c6, c7)
  | c == 4 = (c0, c1, c2, c3, cel, c5, c6, c7)
  | c == 5 = (c0, c1, c2, c3, c4, cel, c6, c7)
  | c == 6 = (c0, c1, c2, c3, c4, c5, cel, c7)
  | c == 7 = (c0, c1, c2, c3, c4, c5, c6, cel)
  | otherwise = error "Coluna inválida"

-- Recebe um tabuleiro antes de ser modificado, o indice (linha, coluna) da celula que vai mudar, e a celula em si
-- Retornando um novo tabuleiro com a celula nova
novoTab :: Tabuleiro -> Posicao -> Celula -> Tabuleiro
novoTab (linha0, linha1, linha2, linha3, linha4, linha5, linha6, linha7) (l, c) cel
  | l == 0 = (novaLinha linha0 c cel, linha1, linha2, linha3, linha4, linha5, linha6, linha7)
  | l == 1 = (linha0, novaLinha linha1 c cel, linha2, linha3, linha4, linha5, linha6, linha7)
  | l == 2 = (linha0, linha1, novaLinha linha2 c cel, linha3, linha4, linha5, linha6, linha7)
  | l == 3 = (linha0, linha1, linha2, novaLinha linha3 c cel, linha4, linha5, linha6, linha7)
  | l == 4 = (linha0, linha1, linha2, linha3, novaLinha linha4 c cel, linha5, linha6, linha7)
  | l == 5 = (linha0, linha1, linha2, linha3, linha4, novaLinha linha5 c cel, linha6, linha7)
  | l == 6 = (linha0, linha1, linha2, linha3, linha4, linha5, novaLinha linha6 c cel, linha7)
  | l == 7 = (linha0, linha1, linha2, linha3, linha4, linha5, linha6, novaLinha linha7 c cel)
  | otherwise = error "Linha inválida"

-- Verifica se a direção d é válida, ou seja é N ou S ou L ou O
direcaoValida :: Char -> Bool
direcaoValida d
  | d == 'N' = True
  | d == 'S' = True
  | d == 'L' = True
  | d == 'O' = True
  | otherwise = False

-- Recebe um tabuleiro e o indice da linha que vai ser devolvida
pegaLinha :: Tabuleiro -> Int -> Linha
pegaLinha (linha0, linha1, linha2, linha3, linha4, linha5, linha6, linha7) l
  | l == 0 = linha0
  | l == 1 = linha1
  | l == 2 = linha2
  | l == 3 = linha3
  | l == 4 = linha4
  | l == 5 = linha5
  | l == 6 = linha6
  | l == 7 = linha7
  | otherwise = error "Linha inválida"

-- Recebe uma linha e o indice da celula que vai ser devolvida
pegaCelula :: Linha -> Int -> Celula
pegaCelula (c0, c1, c2, c3, c4, c5, c6, c7) c
  | c == 0 = c0
  | c == 1 = c1
  | c == 2 = c2
  | c == 3 = c3
  | c == 4 = c4
  | c == 5 = c5
  | c == 6 = c6
  | c == 7 = c7
  | otherwise = error "Coluna inválida"

-- Recebe um tabuleiro e o indice da linha e da celula que vai ser devolvida
pegaIndice :: Tabuleiro -> (Int, Int) -> Celula
pegaIndice t (l, c) = pegaCelula (pegaLinha t l) c

-- Recebe o id de um jogador e uma lista de jogadores, para retornar o jogador com aquele id
pegaJogador :: Int -> [Jogador] -> Jogador
pegaJogador _ [] = error "Id inválido"
pegaJogador id (x@(i,_,_,_):xs)
  | i == id = x
  | otherwise = pegaJogador id xs

-- Verifica se existe jogador na célula
existeJogador :: Celula -> Int -> Bool
existeJogador [] _ = False
existeJogador cel@(x:xs) id
  | x == Jogador id = True
  | otherwise = existeJogador xs id

-- retorna a nova posição do jogador dependendo da direção
novaPosicao :: Posicao -> Char -> Posicao
novaPosicao pos@(linhaAtual,colunaAtual) d
  | d == 'N' && proxL < 7 = (linhaAtual+1, colunaAtual)
  | d == 'S' && prevL >= 0 = (linhaAtual-1, colunaAtual)
  | d == 'L' && proxC < 7 = (linhaAtual, colunaAtual+1)
  | d == 'O' && prevC >= 0 = (linhaAtual, colunaAtual-1)
  | otherwise = pos -- posição não muda
  where proxL = linhaAtual+1
        prevL = linhaAtual-1
        proxC = colunaAtual+1
        prevC = colunaAtual-1

attPosicaoEDirecao :: [Jogador] -> Int -> Posicao -> Char -> [Jogador]
attPosicaoEDirecao [] _ _ _ = []
attPosicaoEDirecao ((i,p,d,c):xs) id novaPos novaDir
  | i == id = (i,novaPos,novaDir,c):xs
  |otherwise = (i,novaPos,d,c):attPosicaoEDirecao xs id novaPos novaDir

attDirecao :: [Jogador] -> Int -> Char -> [Jogador]
attDirecao [] _ _ = []
attDirecao ((i,p,d,c):xs) id novaDir
  | i == id = (i,p,novaDir,c):xs
  |otherwise = (i,p,d,c):attDirecao xs id novaDir

attCapacidades :: [Jogador] -> Int -> Capacidades -> [Jogador]
attCapacidades [] _ _ = []
attCapacidades ((i,p,d,c):xs) id novasCapacidades
  | i == id = (i,p,d,novasCapacidades):xs
  |otherwise = (i,p,d,c):attCapacidades xs id novasCapacidades

removeJogador :: Jogador -> [Jogador] -> [Jogador]
removeJogador _ [] = []
removeJogador x (y:ys) | x == y = removeJogador x ys
                       | otherwise = y : removeJogador x ys

pegaObj :: [Jogador] -> Int -> Objeto -> Capacidades
pegaObj listaJ id obj
  | obj == Patins = ((Patins, p+1),(Arremesso, a),b)
  | obj == Arremesso = ((Patins, p),(Arremesso, a+1),b)
  | otherwise = error "Objeto inválido"
  where j@(_, _, _, ((Patins, p),(Arremesso, a),b)) = pegaJogador id jogadores


-- Recebe um tabuleiro, a posicao da bomba, a dir que ela ira, e o valor do arremesso -> Retornando um novo Tabuleiro
-- A função movimenta a bomba uma célula de cada vez, diminuindo o valor do arremesso, pois caso chegue a 0 ela para
-- E a cada chamada, a função verifica se é possível ir para a próx célula, caso não, a bomba para
arremesso :: Tabuleiro -> Posicao -> Char -> Int -> Tabuleiro
arremesso t p dir arr
  | arr == 0 = t
  | ult == Parede || ult == Pedra || null celulaProx = t
  | ult == Objeto Patins || ult == Objeto Arremesso = t
  | otherwise = arremesso finalt novaPos dir (arr-1)
  where novaPos = novaPosicao p dir -- devolve a nova posicao (X, Y) do jogador dependendo da direcao 
        celulaAtual = pegaIndice t p -- devolve a celula atual
        celulaProx = pegaIndice t novaPos -- devolve a proxima celula
        ult = last celulaProx -- ultimo elemento da proxima celula
        celulaAtualSemBomba = drop 1 (reverse celulaAtual) -- faz uma nova celula removendo a bomba arremessada
        celulaProxComBomba = celulaProx ++ [last celulaAtual] -- faz uma nova celula adicionando a bomba arremessada
        novot = novoTab t p celulaAtualSemBomba -- tabuleiro atualizado com a celulaAtual modificada
        finalt = novoTab novot novaPos celulaProxComBomba -- tabuleiro atualizado com as duas celulas modificadas (atual e prox)


-- função de movimento ainda em desenvolvimento 
-- Recebe um tabuleiro, uma lista de jogadores, id do jogador que vai se mover, e a diração do movimento
-- Retorna uma tupla com um novo tabuleiro, e uma nova lista de jogadores
movimenta :: Tabuleiro -> [Jogador] -> Int -> Char -> (Tabuleiro, [Jogador])
movimenta t listaJ id dir
  | not(direcaoValida dir) = error "Direção Inválida"
  | not(existeJogador celulaAtual id) = error "Jogador não existe"
  | ult == Pedra || ult == Parede = (t, listaJComNovaDirecao)
  | ult == Grama = (tabAposMovimento, listaJAposMovimento)
  | ult == Objeto Patins = (tabAposPegoItem, listaJAposItemColetado Patins)
  | ult == Objeto Arremesso = (tabAposPegoItem, listaJAposItemColetado Arremesso)
  | ult == Objeto Bomba && arr >= 0 = (arremesso t novaPos dir arr, listaJComNovaDirecao)
  | null celulaProx = (novot, listaJSemJogadorId)
  |otherwise = (t, listaJ)
  where j@(_, pos@(linhaAtual,colunaAtual), _, (_,(_,arr),_)) = pegaJogador id jogadores
        novaPos = novaPosicao pos dir -- devolve a nova posicao (X, Y) do jogador dependendo da direcao

        celulaAtual = pegaIndice t pos -- devolve a celula atual
        celulaProx = pegaIndice t novaPos -- devolve a proxima celula
        ult = last celulaProx -- ultimo elemento da proxima celula
        celulaAtualSemJogador = drop 1 (reverse celulaAtual) -- faz uma nova celula removendo o jogador que movimentou
        celulaProxComJogador = celulaProx ++ [last celulaAtual] -- faz uma nova celula adicionando o jogador que movimentou
        celulaProxPegoUmItem = drop 1 (reverse celulaProx) ++ [last celulaAtual] -- faz uma nova celula removendo o item pego pelo jogador movimentou

        novot = novoTab t pos celulaAtualSemJogador -- tabuleiro atualizado com a celulaAtual modificada
        tabAposMovimento = novoTab novot novaPos celulaProxComJogador -- tabuleiro atualizado com as duas celulas modificadas (atual e prox)
        tabAposPegoItem = novoTab novot novaPos celulaProxPegoUmItem -- tabuleiro atualizado com as duas celulas modificadas (atual e prox) e pego um item pelo jogador

        listaJAposMovimento = attPosicaoEDirecao listaJ id novaPos dir -- nova lista de jogadores apos o movimento
        listaJComNovaDirecao = attDirecao listaJ id dir -- nova lista de jogadores atualizando apenas a direção
        listaJSemJogadorId = removeJogador j jogadores -- jogador que caiu no buraco é removido
        listaJAposItemColetado obj = attCapacidades jogadores id (pegaObj jogadores id obj)

-- Recebe um tabuleiro, uma lista de jogadores, id do jogador que vai soltar a bomba
-- Retorna uma tupla com um novo tabuleiro, e uma nova lista de jogadores
soltaBomba :: Tabuleiro -> [Jogador] -> Int -> (Tabuleiro, [Jogador])
soltaBomba t listaJ id
  | not(existeJogador celulaAtual id) = error "Jogador não existe"
  | otherwise = (novot, listaJ)
  where j@(_, pos@(linhaAtual,colunaAtual), _, _) = pegaJogador id jogadores
        --Celulas
        celulaAtual = pegaIndice t pos -- devolve a celula atual
        celulaAtualAposBomba = celulaAtual ++ [Objeto Bomba]-- faz uma nova celula adicionando uma bomba nela
        -- Tabuleiros
        novot = novoTab t pos celulaAtualAposBomba -- tabuleiro atualizado com a celulaAtual modificada

obterTab:: (Tabuleiro, [Jogador]) -> Tabuleiro
obterTab (tab, l) = tab

obterJog:: (Tabuleiro, [Jogador]) -> [Jogador]
obterJog (tab, l) = l

explodeBomba :: Tabuleiro -> [Jogador] -> Posicao -> (Tabuleiro, [Jogador])
explodeBomba t listaJ posicaoBomba = explodeBomba' t listaJ posicaoBomba 4

-- Recebe um tabuleiro, uma lista de jogadores, a posição da bomba, e a diração da explosão
-- Retorna uma tupla com um novo tabuleiro, e uma nova lista de jogadores
explodeBomba' :: Tabuleiro -> [Jogador] -> Posicao -> Int -> (Tabuleiro, [Jogador])
explodeBomba' t listaJ posicaoBomba num
  | num > 1 = (obterTab (explodeBomba' t listaJ posicaoBomba (num - 1)), obterJog (explodeBomba' t listaJ posicaoBomba (num - 1)))
  | ult == Pedra || ult == Objeto Patins || ult == Objeto Arremesso = (tabAposDestruicao, listaJ)
 -- | ult == Parede || ult == Grama = (t, listaJ)
  | otherwise = (t, listaJ)
  where dir
           | num == 4 = 'N'
           | num == 3 = 'S'
           | num == 2 = 'L'
           | otherwise = 'O'
        novaPos = novaPosicao posicaoBomba dir -- devolve a nova posicao (X, Y) do jogador dependendo da direcao

        celulaAtual = pegaIndice t posicaoBomba -- devolve a celula atual
        celulaProx = pegaIndice t novaPos -- devolve a proxima celula
        ult = last celulaProx -- ultimo elemento da proxima celula
        
        celulaProxDestruida = drop 1 (reverse celulaProx) -- faz uma nova celula removendo o item atingido pela bomba

        novot = novoTab t posicaoBomba celulaProxDestruida -- tabuleiro atualizado com a celulaAtual modificada
        tabAposDestruicao = novoTab novot novaPos celulaProxDestruida -- tabuleiro atualizado com as duas celulas modificadas (atual e prox)

{-
>>>pegaLinha tab 0
>>>pegaLinha tab 1
>>>pegaLinha tab 2
>>>pegaLinha tab 3
>>>pegaLinha tab 4
>>>pegaLinha tab 5
>>>pegaLinha tab 6
>>>pegaLinha tab 7
([Grama,Jogador 1],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama,Objeto Patins],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama,Parede],[Grama,Objeto Bomba],[Grama,Jogador 2],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama,Parede],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])

>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 0
>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 1
>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 2
>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 3
>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 4
>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 5
>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 6
>>>pegaLinha (obterTab (explodeBomba tab jogadores (3,4))) 7
([Grama,Jogador 1],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama,Objeto Patins],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama,Parede],[Grama,Objeto Bomba],[Grama,Jogador 2],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama,Parede],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])
([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])

>>>soltaBomba tab jogadores 1
((([Grama,Jogador 1,Objeto Bomba],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama]),([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama]),([Grama],[Grama],[Grama],[Grama],[Grama,Objeto Patins],[Grama],[Grama],[Grama]),([Grama],[Grama],[Grama],[Grama,Parede],[Grama,Objeto Bomba],[Grama,Jogador 2],[Grama],[Grama]),([Grama],[Grama],[Grama],[Grama],[Grama,Parede],[Grama],[Grama],[Grama]),([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama]),([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama]),([Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama])),[(1,(0,0),'N',((Patins,0),(Arremesso,3),(Bomba,1))),(2,(7,7),'S',((Patins,0),(Arremesso,0),(Bomba,1)))])

-}

fimDeJogo :: [Jogador]-> Bool
fimDeJogo listaJ = length listaJ == 1
