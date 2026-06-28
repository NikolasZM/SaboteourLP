import os
from PIL import Image

# ── CONFIGURACIÓN (Cambia estos números a tu gusto) ──────────────────
ANCHO = 254  # Tamaño base en píxeles
ALTO = 169   # Tamaño base en píxeles
# ─────────────────────────────────────────────────────────────────────

# Carpetas de origen (donde están tus fotos) y destino
carpeta_origen = "./assets" 
carpeta_destino = "./imagenes_redimensionadas"

# Crear la carpeta de destino si no existe
os.makedirs(carpeta_destino, exist_ok=True)

# Formatos de imagen que va a buscar para procesar
formatos_validos = (".png", ".jpg", ".jpeg", ".webp", ".bmp")

print(f"🚀 Procesando imágenes a {ANCHO}x{ALTO} px, rotándolas 90° y convirtiendo a PNG...\n")

contador = 0

# Recorrer todos los archivos de la carpeta de origen
for archivo in os.listdir(carpeta_origen):
    # Validar si el archivo es una imagen soportada
    if archivo.lower().endswith(formatos_validos):
        ruta_imagen = os.path.join(carpeta_origen, archivo)
        
        try:
            with Image.open(ruta_imagen) as img:
                # 1. Redimensionar usando un filtro de alta calidad (LANCZOS)
                img_redimensionada = img.resize((ANCHO, ALTO), Image.Resampling.LANCZOS)
                
                # 2. Rotar 90 grados en sentido horario (expand=True invierte a 169x254)
                img_nueva = img_redimensionada.rotate(-90, expand=True)
                
                # 3. Forzar la extensión a .png
                nombre_sin_ext, _ = os.path.splitext(archivo)
                nombre_png = nombre_sin_ext + ".png"
                
                # Guardar en la nueva carpeta con el nuevo nombre PNG
                ruta_salida = os.path.join(carpeta_destino, nombre_png)
                img_nueva.save(ruta_salida, format="PNG")
                
                print(f"✓ Convertida a PNG y rotada: {nombre_png}")
                contador += 1
        except Exception as e:
            print(f"❌ No se pudo procesar {archivo}. Error: {e}")

print(f"\n🎉 ¡Proceso terminado! Se exportaron {contador} imágenes en formato PNG.")
print(f"📁 Revisa la carpeta: '{carpeta_destino}'")