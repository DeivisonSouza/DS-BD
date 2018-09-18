from PIL import Image, ImageFilter

try:
    original = Image.open("Lenna.png")
except:
    print("Não é possível carregar a imagem")


print("As características da imagem são:")
print('\n Formato:', original.format,'\n', 'Tamanho:', original.size, '\n', 'Modo:', original.mode) # size = largura e altura (em pixels)

original.show()         # Exibindo a imagem

