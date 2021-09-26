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