-- Presentes ou bomba
data Objeto = Patins | Arremesso | Bomba deriving (Eq)

-- Auxiliares para o jogador
type Capacidades = ((Objeto, Int), (Objeto, Int), (Objeto, Int))
type Posicao = (Int, Int)

-- Itens
data Item = Grama | Objeto | Parede | Pedra | Jogador (Int, Posicao, Char, Capacidades) deriving (Eq)

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



movimenta :: Tabuleiro -> Posicao -> Char -> Bool
movimenta tabuleiro pos@(linhaAtual, colunaAtual) direcao
  | not(direcaoValida direcao) = error "Direção Inválida"
  | otherwise = False