from PIL import Image, ImageFilter                                                                  # Carrega método Image da bibilioteca PIL
import numpy as np                                                                                  # Carrega método Image da bibilioteca numpy
import matplotlib.pyplot as plt
import os

#############################################################################################################################################################################
## TRABALHANDO COM UM ARQUIVO DE IMAGEM:
#############################################################################################################################################################################
try:
    original = Image.open('cdf0000_14_4_3.tif')                                                     # carrega uma imagem
except:
    print("Não é possível carregar a imagem")                                                       # imprime mensagem de erro, caso não seja possível abrir ou encontrar a imagem

path = os.getcwd()                                                                                  # retorna o caminho do diretório atual
print('O diretório corrente é: %s' % path)

#original.save('teste.png')                                                                         # conversão: salva imagem em outro formato (no caso, png)
#original2 = original.filter(ImageFilter.BLUR)                                                      # aplicação de filtro na imagem
#original2.save('original2.tif')                                                                    # salva imagem com filtro em outro arquivo (obs.: posso adicionar o path tbm)

print("As características da imagem são:")
print('\n Formato:', original.format,'\n', 'Tamanho:', original.size, '\n', 'Modo:', original.mode,'\n') # size = uma tupla com largura e altura (em pixels). Perceber que é uma imagem de modo “1” (preto e branco)

#largura, altura = original.size                                                                    # ideia = criando duas variáveis e adicionando cada valor da tupla 'original.size'

#original.show()                                                                                    # Exibe a imagem

#arrayOrig = np.array(original)                                                                     # transforma em um array com valores dos pixels (no entanto, o retorno é boleano). Não desejo isso! A solução é informar dtype = "int"
#print(arrayOrig)

nparray = np.array(original,dtype="int")                                                            # um array para retornar os valores dos pixel (que representam a imagem). Agora sim! retorno de inteiro 1 (branco) e 0 (preto)
print(nparray)                                                                                      # imprime o array com os valores dos pixels

plt.imshow(nparray)                                                                                 # um gráfico da imagem (gerado a partir dos pixels contidos no array)
plt.show()                                                                                          # Exibe o gráfico da imagem                                                                         

nparray2 = np.array(original,dtype="int").reshape((1,original.size[0]*original.size[1]))
print(nparray2)

#plt.imshow(nparray2)                                                                                # um gráfico da imagem (gerado a partir dos pixels contidos no reshape do array)
#plt.show()                                                                                          # Exibe o gráfico da imagem

#############################################################################################################################################################################
## TRABALHANDO COM UMA LISTA DE IMAGEM:
#############################################################################################################################################################################
# 1) Leia todas as imagens em “data” (DICA: módulos OS e PIL)

caminho = os.path.join('/home/deivison/DSBD/Linguagem Programação/Python/Imagens/data/')               # define o caminho para o diretório 'data' (onde estão as imagens)

for f in os.listdir(caminho):                                                                         # busca no diretório 'data'
    if f.endswith('.tif'):                                                                         # todos os arquivos com extensão '.tif'
        print(f)                                                                                   # imprime a lista de arquivos do diretório 'data'
        try:                                                                                         
           imgs = Image.open(f)                                                                    # carrega todas as imagens com a extensão específicada
        except OSError:
           print('Não foi possível carregar as imagens.') 
        else:
           print('As imagens foram carregadas com sucesso.')

        #fn, fext = os.path.splitext(f)                                                            # Aqui faz-se um split de 'f' (nome dos arquivo) e atribui a duas variáveis (fn e fext). fn = nome do arquivo e fext = extensão do arquivo
        #print('o nome do arquivo é:', fn)                                                         # imprime o nome do arquivo
        #print('a extensão do arquivo é:',fext)                                                    # imprime a extensão do arquivo
        #diretorio = os.makedirs('IMG/Sub-IMG')
        #img.save('pngs/{}.png'.format(fn))                                                        # Aqui salva os arquivos no diretório 'pngs' usando o nome obtido em 'fn' (que será passado nos {})






# define um nome para um novo diretório:
path = os.path.join(os.getcwd(), 'data_new')
print(path)

try:  
    os.mkdir(path)
except OSError:  
    print ("A criação do diretório falhou: %s" % path)
else:  
    print ("Diretório criado com sucesso em: %s " % path)




#############################################################################################################################################################################

# um loop for para procurar dentro do diretório corrente todos os arquivos terminados com '.tif' e, em seguida, imprimi-los em tela (aqui usa-se a biblioteca 'os')
#for f in os.listdir('.'):
    #if f.endswith('.tif'):
        #print(f)

# um for para mudar extensão de vários arquivos: 
# Aqui procura-se um arquivo com formato específico (no caso '.tif').Depois salva com outro formato (no caso '.png') em um diretório específico (que já deve estar criado!). No caso o nome dodiretório foi 'pngs'.
#for f in os.listdir('.'):                                                                         # busca no diretório corrente
    #if f.endswith('.tif'):                                                                        # os arquivos com extensão '.tif'
        #print(f)
        #i = Image.open(f)                                                                         # carrega todas as imagens com a extensão específicada
        #fn, fext = os.path.splitext(f)                                                            # Aqui faz-se um split de 'f' (nome dos arquivo) e atribui a duas variáveis (fn e fext). fn = nome do arquivo e fext = extensão do arquivo
        #print('o nome do arquivo é:', fn)                                                         # imprime o nome do arquivo
        #print('a extensão do arquivo é:',fext)                                                    # imprime a extensão do arquivo
        #i.save('pngs/{}.png'.format(fn))                                                          # Aqui salva os arquivos no diretório 'pngs' usando o nome obtido em 'fn' (que será passado nos {})


# um loop for para mudar o size (tamanho) de vários arquivos: 
# Aqui procura-se um arquivo com formato específico (no caso '.tif'). Depois salva com outro formato (no caso '.png') em um diretório específico (que já deve estar criado!). No caso o nome dodiretório foi 'pngs'.

#size_2025 = (20,25)                                                                               # define uma tupla com largura e altura (em pixels). (obs.: no teste que fiz não funcionou perfeitamente! os tamanhos variaram!)
#size_1520 = (15,20)                                                                               # define outra tupla com largura e altura (em pixels).                            

#for f in os.listdir('.'):                                                                         # busca no diretório corrente
    #if f.endswith('.tif'):                                                                        # os arquivos com extensão '.tif'
        #print(f)
        #i = Image.open(f)                                                                         # carrega todas as imagens com a extensão específicada
        #fn, fext = os.path.splitext(f)                                                            # Aqui faz-se um split de 'f' (nome dos arquivo) e atribui a duas variáveis (fn e fext). fn = nome do arquivo e fext = extensão do arquivo
        #print('o nome do arquivo é:', fn)                                                         # imprime o nome do arquivo
        #print('a extensão do arquivo é:',fext)                                                    # imprime a extensão do arquivo
        #i.thumbnail(size_2025)                                                                    # define o novo size (tamanho) para as imagens
        #i.save('newsize_2025/{}_2025{}'.format(fn,fext))                                          # Aqui salva os arquivos no diretório 'newsize_2025' usando o nome obtido em 'fn' e a extensão obtida em fext (que serão passados nos {})
        
        #i.thumbnail(size_1520)                                                                    # define o novo size (tamanho) para as imagens
        #i.save('newsize_1520/{}_1520{}'.format(fn,fext))                                          # Aqui salva os arquivos no diretório 'newsize_1520' usando o nome obtido em 'fn' e a extensão obtida em fext (que serão passados nos {})

#############################################################################################################################################################################
# Módulo 'os': alguns métodos
#path = os.getcwd()
#print('O caminho corrente para o diretório atual é %s' % path)                                      # identifica o caminho para o diretório corrente
#print(os.listdir(os.getcwd()))
#print(os.listdir())                                                                                 # lista os arquivos existentes no diretório corrente
#os.makedirs('Dir/Sub-Dir')                                                                          # cria um novo diretório no diretório corrente
#os.rmdir('Dir/Sub-Dir')                                                                             # remove diretório do final da árvore
#os.removedirs('Dir/Sub-Dir')                                                                        # remove toda árvore de diretórios
#os.rename('files.txt', 'arq.txt')                                                                   # renomeia um arquivo especificado no diretorio corrente
#print(os.environ.get('HOME'))                                                                       # retorna o usuário HOME
#print(os.path.split('Dir/Sub-Dir'))                                                                 # um split do caminho. Gera uma tupla com duas informaçoes
#print(os.path.splitext('data/cdf0011_13_13_4.tif'))                                                 # um split da extensão de um arquivo
#print(dir(os.path))                                                                                 # métodos disponíveis para diretório?


#x = os.path.join('/home/deivison/DSBD/Linguagem Programação/Python', 'Imagens')                     # listar arquivos dentro de um diretório específico
#print(os.listdir(x))                                                                                # imprime a lista dos arquivos, diretorios, etc...

#file_path = os.path.join(os.environ.get('HOME'), 'Caminho.txt')                    # usa o método 'os.path.join' para juntar o caminho do usuario 'HOME' e o arquivo 'Caminho.txt' (que deve ser criado no HOME). Caminho.txt não foi criado?
#print(file_path)

#for dirpath, dirname, filenames in os.walk('/home/deivison/DSBD/Linguagem Programação/Python/Imagens'): # um loop for: para retornar todos os diretórios, caminho atual e nome dos arquivos dentro do diretorio imagem....
    #print('Current Path:', dirpath)
    #print('Diretórios:', dirname)
    #print('Files:', filenames)
#############################################################################################################################################################################

#Redimensionando uma Imagem: Image.resize (size) - O tamanho aqui é fornecido como uma largura e altura de duas tuplas.
#largura, altura = original.size
#orig_resize = original.resize((int(largura/2), int(altura/2)))
#orig_resize.save('orig_resize.tif') 


#print(original.histogram())


