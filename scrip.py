import os
from PIL import Image

def convertir_png_a_bmp(carpeta):
    # Recorre todas las carpetas y subcarpetas
    for ruta_directorio, subdirectorios, archivos in os.walk(carpeta):
        for archivo in archivos:
            # Si el archivo es un .png
            if archivo.lower().endswith('.png'):
                ruta_png = os.path.join(ruta_directorio, archivo)
                ruta_bmp = os.path.splitext(ruta_png)[0] + '.bmp'

                # Abre el archivo .png y lo guarda como .bmp
                try:
                    with Image.open(ruta_png) as img:
                        img.save(ruta_bmp, 'BMP')
                        print(f"Convertido: {ruta_png} a {ruta_bmp}")
                except Exception as e:
                    print(f"Error al convertir {ruta_png}: {e}")
                    
# Llamar a la función
convertir_png_a_bmp("./assets")
