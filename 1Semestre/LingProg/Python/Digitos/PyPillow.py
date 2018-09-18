from PIL import image

img = Image.open("moon.pgm")
print(img)

print(img.format)
print(img.size)
print(img.show())
#img.save("blah.png")
#img2 = Image



