#-----------------------------------------------------------------------
# Carrega os pacotes.

library(XML)
ls("package:XML")

library(RCurl)
ls("package:RCurl")

#-----------------------------------------------------------------------
# O básico de XML.

# String contendo um documento XML.
frag <- '<?xml version="1.0" encoding="UTF-8"?>
<biblioteca>
<livro>
<autor>José de Alencar</autor>
<titulo>Iracema</titulo>
<ano>1865</ano>
<genero>Romance</genero>
</livro>
<livro>
<autor>Machado de Assis</autor><titulo>Dom Casmurro</titulo>
<ano>1899</ano><genero>Romance</genero>
</livro>
<livro>
<titulo>Viagem</titulo>
<autor>Cecília Meireles</autor>
<ano>1939</ano>
<genero>Poesia</genero>
</livro>
</biblioteca>
'

# Argumentos da função xmlParse().
args(xmlParse)

# Faz o parse da string.
doc <- xmlParse(frag, asText = TRUE)
str(doc)

# Classe e métodos.
class(doc)
methods(class = class(doc)[1])

# Exibe o documento. CUIDADO com arquivos longos!
# print(doc)
# show(doc)
doc

# Tabela de frequecia dos elementos ou tags.
summary(doc)

# Converte para data.frame.
xmlToDataFrame(doc)

# Converte para lista.
xmlToList(doc)

# Do nó raíz até o <autor>.
doc["/biblioteca/livro/autor"]
doc["//livro/autor"]
doc["//autor"]

# Extração serial do conteúdo.
sapply(doc["//autor"], FUN = xmlValue)

#-----------------------------------------------------------------------
# Lendo dados de uma API.

# Faz busca por filmes com o termo batman.
url <- "http://www.omdbapi.com/?apikey=eb0c0475&s=terminator&r=xml"
# browseURL(url)

# Parse do documento.
doc <- xmlParse(url)

# Resumo (tabela de frequência dos elementos).
summary(doc)

# Função que vai operar em cada nó da lista, i.e., cada <result>, para
# extrair dados que estão nos atributos.
f <- function(node) {
  c(title = xmlGetAttr(node, name = "title"),
    year = xmlGetAttr(node, name = "year"))
}

# Aplica uma função em cada elemento da lista (no caso <result>).
xpathSApply(doc, path = "//result", fun = f)

#-----------------------------------------------------------------------
# Expressões Xpath, caminhos e predicados.

s <-
  '<div>
<ul type="bolo" class="ingred">
<li>3 ovos</li>
<li>150 ml de leite</li>
<li>3 colheres de manteiga</li>
<li>Massa preparada de bolo</li>
</ul>
<ul class="preparo">
<li>Deixe o forno preaquecer por 20 min à 180 graus</li>
<li>Bata os ingredientes até uniformizar</li>
<li>Unte a forma</li>
<li>Despeje na forma e leve ao fogo por aproximadamente 1 hora</li>
</ul>
</div>'

h <- xmlParse(s)

xpathSApply(h, path = "//ul[2]")
xpathSApply(h, path = "//ul/li[2]")
xpathSApply(h, path = "//ul/li[position() = 2]")
xpathSApply(h, path = "//ul/li[last()]")
xpathSApply(h, path = "//ul/li[position() = last()]")
xpathSApply(h, path = "//ul/li[position() != last()]")
xpathSApply(h, path = "//ul/li[position() < last()]")
xpathSApply(h, path = "//ul[count(li) >= 3]")

xpathSApply(h, path = "//ul[@type]")
xpathSApply(h, path = "//ul[not(@type)]")
xpathSApply(h, path = "//ul[@class = 'preparo']")
xpathSApply(h, path = "//ul[@class != 'preparo']")
xpathSApply(h, path = "//ul[@type and @class]")
xpathSApply(h, path = "//ul[@class = 'ingred' or @class = 'preparo']")

#-----------------------------------------------------------------------

url <- "https://www.cifraclub.com.br/lulu-santos/assim-caminha-humanidade/simplificada.html"
# browseURL(url)

# Importa o fonte da página.
txt <- getURL(url)

# Faz o parse e gera do DOM.
doc <- htmlParse(txt, asText = TRUE, encoding = "utf-8")
summary(doc)

#--------------------------------------------
# Extração do título.

# Xpath query para capturar o título da canção.
xp <- "//h1"         # Não é precisa.
xp <- "//div/div/h1" # Seleciona corretamente.
xpathSApply(doc, path = xp)
xpathSApply(doc, path = xp, fun 
            6= xmlValue)

# Delimitando melhor a captura usando predicados.
xp <- "//h1[@class = 't1']"
xpathSApply(doc, path = xp, fun = xmlValue)

#--------------------------------------------
# Extraindo os acordes.

xp <- "//pre/b"
unique(xpathSApply(doc, path = xp, fun = xmlValue))

#--------------------------------------------
# Extraindo a letra com a cifra.

xp <- "//pre"
lyrics <- xpathSApply(doc,
                      path = xp,
                      fun = xmlValue)
cat(lyrics, "\n")

#--------------------------------------------
# Extraindo apenas a letra sem os acordes.

xp <- "//pre"
lyrics <- xpathSApply(doc,
                      path = xp,
                      fun = xmlValue,
                      recursive = FALSE, # <- não entra nos <b>.
                      trim = TRUE)

# Removendo excesso de quebras de linha.
cat(gsub("\n *\n+", "\n", lyrics), sep = "\n")

#-----------------------------------------------------------------------
# Informações sobre veículos à venda.

url <- "https://www.webmotors.com.br/carros/estoque/volkswagen/crossfox?tipoveiculo=carros&marca1=volkswagen&modelo1=crossfox&estadocidade=estoque"
# browseURL(url)

# Mostra quais são os parâmetros fornecidos na query string da URL.
getFormParams(url)

# Faz o parse da página.
doc <- htmlParse(getURL(url))
summary(doc)

# Tirar a URL de cada veículo anunciado.
xp <- "//div[contains(@id, 'boxResultado')]/a"
href <- xpathSApply(doc,
                    path = xp,
                    fun = xmlGetAttr,
                    name = "href")
head(href)

# Extraindo as informações anunciadas e gerar uma tabela.
xp <- "//div[@class = 'advert']"
x <- xpathSApply(doc,
                 path = xp,
                 fun = xmlValue,
                 trim = TRUE)

# Para criar um CSV.
x <- gsub("  +", ";", x)

# Escreve em disco e em seguida importa (poderia usar textConnection()).
cat(x, file = "carros.txt", sep = "\n")
carros <- read.csv2("carros.txt",
                    header = FALSE,
                    stringsAsFactors = FALSE)
names(carros) <- c("fts", "model", "espec", "preco", "ano", "km",
                   "cambio", "negoc", "ender", "local")
str(carros)

# Requer pós processamento de strings para tratar (limpeza,
# pradronização e conversão) cada campo da tabela.

carros$preco <- as.integer(gsub("\\D", "", carros$preco))/1000
carros$km <- as.integer(gsub("\\D", "", carros$km))/1000

table(carros$espec)

plot(preco ~ km,
     data = carros,
     xlab = "Distância percorrida (km/1000)",
     ylab = "Preço (R$/1000)")

#-----------------------------------------------------------------------
# RSelenium.

library(RSelenium)

# help(rsDriver, h = "html")
args(rsDriver)

# Essa função instala o Selenium quando chamada pela primeira
# vez. Diretórios serão criados na home do usuário para instalação de
# web drives do navegador.
rD <- rsDriver(browser = "firefox")
dir("~/.local/share/")

# Abrir uma instância nova.
remDr <- rD[["client"]]
remDr$open()

class(remDr)
str(remDr)

# Abrir uma página.
remDr$navigate("http://www.google.com/")

# Reporta o status da conexão?
remDr$getStatus()

# Abre outra página.
remDr$navigate("http://www.ufpr.br")

# Mostra endereço atual da página.
remDr$getCurrentUrl()

# Volta a página anterior.
remDr$goBack()

# Retorna para a página em que estava.
remDr$goForward()

# Atualiza a página (F5).
remDr$refresh()

# Fecha a página.
remDr$close()

#-----------------------------------------------------------------------
# Campeonato inglês temporada 2016/2017.

url <- paste0("https://www.whoscored.com/Regions/252/",
              "Tournaments/2/Seasons/6335/England-Premier-League")
# browseURL(url)

# remDr <- rD[["client"]]
remDr$open()
remDr$navigate(url)

# Dá um refesh para voltar para a data mais recente.
remDr$refresh()

# Clica para ir para datas para trás.
webElem <- remDr$findElement(using = "xpath",
                             value = "//div[@id='date-controller']/a[1]")
webElem$highlightElement()

# ATTENTION.
# Pega o texto da página (estado inicial).
pg0 <- remDr$getPageSource()[[1]]

# Expressões Xpath para a data e a tabela com placares.
path_dt <- '//a[@id="date-config-toggle-button"]/span[1]'
path_placar <- "//table[@id='tournament-fixture']"

# Lista vazia para armazenar as tabelas extraídas com os placares das
# rodadas.
L <- list()
i <- 1
repeat {
  # Para sair logo do loop.
  if (i > 3) break
  # Faz o parse.
  doc <- htmlParse(pg0)
  # Extrai o range de data dos jogos (semana).
  dt <- xpathSApply(doc, path = path_dt, fun = xmlValue)
  cat(i, ": ", dt, "\n", sep = "")
  # Extrai a tabela com o placar.
  u <- getNodeSet(doc, path = path_placar)
  # Passa tabela para a lista.
  L[[i]] <- u[[1]]
  names(L)[i] <- dt
  # Clica no botão.
  webElem$clickElement()
  # Aguarda.
  Sys.sleep(5)
  # Lê o fonte.
  pg1 <- remDr$getPageSource()[[1]]
  # Testa se o conteúdo e segue.
  if (pg0 == pg1) {
    break
  } else {
    pg0 <- pg1
    i <- i + 1
  }
}

# Fecha a página.
remDr$close()

#--------------------------------------------
# Salva resultados em disco.

length(L)
save(L, file = "premier-league.RData")
load("premier-league.RData")

#--------------------------------------------
# Prototipa o tratamento da tabela.

da <- readHTMLTable(L[[1]], stringsAsFactors = FALSE)
str(da)

# Para escrever a data onde está vazio usando a última data.
cumstring <- function(x, y) {
  if (y == "" & x != "") {
    y <- x
  }
  return(y)
}

da$V1 <- Reduce(f = cumstring, x = da$V1, accumulate = TRUE)
subset(da, !is.na(V5), select = c(1:2, 4:6))

#--------------------------------------------
# Tratando todas as tabelas.

tidytable <- function(obj) {
  da <- readHTMLTable(obj, stringsAsFactors = FALSE)
  da$V1 <- Reduce(f = cumstring, x = da$V1, accumulate = TRUE)
  subset(da, !is.na(V5), select = c(1:2, 4:6))
}

# Extrai e organiza as tabelas.
tbs <- lapply(L, FUN = tidytable)
names(tbs) <- NULL

# Empilha e dá nomes para as colunas.
tb <- do.call(rbind, tbs)
names(tb) <- c("date", "time", "home", "result", "away")

# Remove números nas extremidades.
regex <- "^\\d?([^0-9]+)\\d?$"
tb$home <- gsub(regex, "\\1", tb$home)
tb$away <- gsub(regex, "\\1", tb$away)

# Conta quantas vezes cada time apareceu.
table(c(tb$home, tb$away))

#-----------------------------------------------------------------------
# Imóveis.

library(jsonlite)

chd <- getCurlHandle(cookie = "")

# Página inicial com anúncios.
url <- paste0("http://www.imovelweb.com.br/",
              "apartamentos-venda-centro-curitiba.html")
# browseURL(url)

doc <- htmlParse(getURL(url, curl = chd), asText = TRUE)

tot <- xpathSApply(doc,
                   path = "//h1[@class = 'resultado-title']/strong",
                   fun = xmlValue)
tot <- as.integer(sub("\\D", "", tot))
tot

# Total de páginas.
tot %/% 20 + 1

# URL no botão de "próxima página".
next_url <- xpathSApply(doc,
                        path = "//li[@class = 'pagination-action-next ']/a",
                        fun = xmlGetAttr,
                        name = "href")
next_url

# Para remover caracteres de espaço em excesso com REGEX.
xmlValueClean <- function(node, ...) {
  val <- xmlValue(node, ...)
  gsub("[[:space:]][[:space:]]+", " ", val)
}

# Apartamentos a venda em Curitiba no Imoveis Web.
# Função que faz a extração dentro de cada imóvel anunciado.
getAnuncInfo <- function(node) {
  # Cria um documento gerando a raíz a partir do nó.
  node_doc <- xmlDoc(node)
  # Extrai o ID.
  id <- xmlGetAttr(xmlRoot(node_doc), name = "data-aviso")
  # Extrai o preço.
  preco <- node_doc[["//span[@class = 'aviso-data-price-value']"]]
  preco <- gsub("\\D", "", xmlValueClean(preco, trim = TRUE))
  # Extrai o endereço.
  ender <- node_doc[["//span[@class = 'aviso-data-location dl-aviso-link']"]]
  ender <- xmlValueClean(ender, trim = TRUE)
  # Extrai as características métricas.
  carac <-
    xpathSApply(
      node_doc,
      path = "//ul[@class = 'aviso-data-features dl-aviso-link']/li",
      fun = xmlValueClean,
      trim = TRUE)
  # Tratamento com REGEX.
  n <- sub(".*\\d (.+)$", "\\1", carac)
  x <- as.list(as.numeric(sub("^(.*\\d).*$", "\\1", carac)))
  names(x) <- sub("s$", "", n)
  # Cria lista e transforma em tabela.
  res <- append(x,
                values = list(id = id,
                              preco = preco,
                              ender = ender))
  # Escreve em disco.
  cat(toJSON(res), sep = "", ",\n",
      file = "anunc-cwb.json",
      append = TRUE)
  invisible()
}

unlink("anunc-cwb.json")
cat("[", file = "anunc-cwb.json")
imov <- xpathApply(doc,
                   path = "//li[@data-tipoaviso='Clasificados  superdestacado']",
                   fun = getAnuncInfo)
cat("null ]", file = "anunc-cwb.json", append = TRUE)

imov <- fromJSON("anunc-cwb.json", simplifyDataFrame = FALSE)
length(imov)
str(imov)

# Retina o NULL no final que foi colocado para evitar problema com a
# última vírgula antes de fechar o array.
imov[length(imov)] <- NULL
length(imov)

imov <- lapply(imov,
               FUN = as.data.frame,
               stringsAsFactors = FALSE,
               check.names = FALSE)
imov <- do.call(what = plyr::rbind.fill, args = imov)
str(imov)

imov[, 1:3]

#-----------------------------------------------------------------------