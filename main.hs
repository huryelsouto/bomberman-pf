-- Presentes ou bomba
data Objeto = Patins | Arremesso | Bomba deriving (Eq)

-- Auxiliares para o jogador
type Capacidades = ((Objeto, Int), (Objeto, Int), (Objeto, Int))
type Posicao = (Int, Int)
type ID = Int

-- Jogador
type Jogador = (ID, Posicao, Char, Capacidades) 

-- Itens
data Item = Grama | Objeto Objeto | Parede | Pedra | Jogador Int deriving (Eq)

-- Tabuleiro 
type Celula = [Item]
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)

linha1 :: Linha
linha1 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha2 :: Linha
linha2 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha3 :: Linha
linha3 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha4 :: Linha
linha4 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha5 :: Linha
linha5 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha6 :: Linha
linha6 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha7 :: Linha
linha7 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

linha8 :: Linha
linha8 = ([Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama])

tab :: Tabuleiro
tab = (linha1, linha2, linha3, linha4, linha5, linha6, linha7, linha8)

jogadores :: [Jogador]
jogadores = [(1, (0,0), 'N', ((Patins, 0),(Arremesso, 0),(Bomba, 1))), (2, (7,7), 'S', ((Patins, 0),(Arremesso, 0),(Bomba, 1)))]

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
  | d == 'N' = (linhaAtual, colunaAtual+1)
  | d == 'S' = (linhaAtual, colunaAtual-1)
  | d == 'L' = (linhaAtual+1, colunaAtual)
  | d == 'O' = (linhaAtual-1, colunaAtual)
  | otherwise = error "Direção inválida"

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


-- Recebe um tabuleiro, uma lista de jogadores, id do jogador que vai se mover, e a diração do movimento
-- Retorna uma tupla com um novo tabuleiro, e uma nova lista de jogadores
movimenta :: Tabuleiro -> [Jogador] -> Int -> Char -> (Tabuleiro, [Jogador])
movimenta t listaJ id dir
  | not(direcaoValida dir) = error "Direção Inválida"
  | not(existeJogador celulaAtual id) = error "Jogador não existe"
  | ult == Pedra || ult == Parede || ult == Objeto Bomba = (t, listaJComNovaDirecao)
  | ult == Grama = (tabAposMovimento, listaJAposMovimento)
  | ult == Objeto Patins = (tabAposPegoItem, listaJAposItemColetado Patins)
  | ult == Objeto Arremesso = (tabAposPegoItem, listaJAposItemColetado Arremesso)
  | null celulaProx = (novot, listaJSemJogadorId)
  |otherwise = (t, listaJ)
  where j@(_, pos@(linhaAtual,colunaAtual), _, _) = pegaJogador id jogadores
        novaPos = novaPosicao pos dir -- devolve a nova posicao (X, Y) do jogador dependendo da direcao
        --Celulas
        celulaAtual = pegaIndice t pos -- devolve a celula atual
        celulaProx = pegaIndice t novaPos -- devolve a proxima celula
        ult = last celulaProx -- ultimo elemento da proxima celula
        celulaAtualSemJogador = drop 1 (reverse celulaAtual) -- faz uma nova celula removendo o jogador que movimentou
        celulaProxComJogador = celulaProx ++ [last celulaAtual] -- faz uma nova celula adicionando o jogador que movimentou
        celulaProxPegoUmItem = drop 1 (reverse celulaProx) ++ [last celulaAtual] -- faz uma nova celula removendo o item pego pelo jogador movimentou
        -- Tabuleiros
        novot = novoTab t pos celulaAtualSemJogador -- tabuleiro atualizado com a celulaAtual modificada
        tabAposMovimento = novoTab novot novaPos celulaProxComJogador -- tabuleiro atualizado com as duas celulas modificadas (atual e prox)
        tabAposPegoItem = novoTab novot novaPos celulaProxPegoUmItem -- tabuleiro atualizado com as duas celulas modificadas (atual e prox) e pego um item pelo jogador
        -- Listas de Jogadores
        listaJAposMovimento = attPosicaoEDirecao listaJ id novaPos dir -- nova lista de jogadores apos o movimento
        listaJComNovaDirecao = attDirecao listaJ id dir -- nova lista de jogadores atualizando apenas a direção
        listaJSemJogadorId = removeJogador j jogadores -- jogador que caiu no buraco é removido
        listaJAposItemColetado obj = attCapacidades jogadores id (pegaObj jogadores id obj)


fimDeJogo :: [Jogador]-> Bool
fimDeJogo listaJ = length listaJ == 1