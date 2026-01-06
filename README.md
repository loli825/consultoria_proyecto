# consultoria_proyecto
scraping en Python (.py) y analisis en R (.Rmd)

# GRUÍA DE TRABAJO

Tareas ordenadas cronologicamente:

- (MANUELA) scrapear listado de obras y obtener URLs ('scraper_listado.py' --> 'prado_listado.csv')

- (MANUELA) scrapear cada obra y obtener fichas técnicas ('scraper_ficha_tecnica.py' --> 'prado_ficha_técnica.jsonl')

- (MANUELA) revisar y arreglar fichas técnicas, construir dataset base para trabajar ('gestion.py' --> 'prado.jsonl' --> 'json_a_csv.py' --> 'prado.csv')

- (MANUELA) gestion de datos y construcción de variables ('gestion_datos.Rmd' --> 'prado_gestionado.csv', 'prado_variables.csv')

# ARCHIVOS

## scraper_listado.py

CONTENIDO: 

- Código Python para obtener URLs (entre otras cosas) de todas las obras 

- Guarda resultados en un archivo tipo .csv

- Archivo salida: 'prado_listado.csv'

- Archivos extra:

  - 'prado_checpoint.json' (para reanudar sin perder progresos en caso de error)

  - 'debug_respuestas' (carpeta para guardar respuestas problematicas, no ha sido útil)

USO: 

- No es necesario ejecutar, ya existe el archivo salida 'prado_listado.csv' en el repositorio

- Si se quiere ejecutar, debe proporcionarse una cookie personal en el objeto "COOKIE_HEADER", igualmente no estoy segura de si funcionará con solo ese cambio ya que se hacen peticiones escritas a partir de mi cURL 

- Es un código muy lento, tarda unos 30 mins (ya que se deben hacer múltiples peticiones a la página para carga todas las obras)

## scraper_ficha_tecnica.py

CONTENIDO: 

- Código para obtener las fichas técnicas de cada obra

- Guarda resultados BRUTOS en un archivo tipo .jsonl con una entrada json por fila (contiene también información de errores y dimensiones mal estructuradas por eso "BRUTOS")

- Archivo entrada: 'prado_listado.csv'

- Archivo salida: 'prado_ficha_tecnica.jsonl'

USO: 

- No es necesario ejecutar, ya existe el archivo salida 'prado_ficha_tecnica.jsonl' en el repositorio

- Si se quiere ejecutar, no cambiar a "HEADLESS = True" porque salta bloqueo anti-bot, es más lento pero es la única manera (que he encontrado) de no ser bloqueados. Se abre el navegador pero no se cargan los recursos pesados

- Es un código muy lento, probablemente entre 1 y 3 horas de ejecución. Se realizan 6 peticiones por ver sin pausas entre ellas pero se demora unos segundos en cargar la página y encontrar el selector de ficha técnica 

## prado_listado.csv

CONTENIDO: 

- Listado de todas las obras que son "Tipo de obra: Pinturas" 

- con la información que aparece en la página del listado de la web

ESTRUCTURA: 7.117 filas × 6 columnas (una fila = una obra)

COLUMNAS:

- obra_id: identificador único de la obra. Coincide exactamente con el que aparece al final de url.

- url: enlace a la página de la obra en la web del Prado. (columna de interés)

- titulo: título de la obra 

- autor: autor “principal” o etiqueta de autor 

- soporte: en realidad es un texto combinado del tipo “Técnica sobre soporte. Fecha” (p. ej. “Óleo sobre lienzo. 1907”).

- img_url: URL directa a la imagen 

## prado_ficha_tecnica.jsonl

CONTENIDO: listado de las obras, principalmente con su ficha técnica

ESTRUCTURA: 7.117 registros (mismo número de obras que en prado_listado.csv, todas obtenidas), 1 JSON por línea y obra

ESTRUCTURA POR LINEA:

- url: el mismo enlace de la obra.

- ok: booleano de éxito (todos True).

- ficha_tecnica: diccionario con campos de ficha técnica (elemento de interés).

- error: detalle de error (todos null).

CAMPOS DE FICHA TÉCNICA: no todas las obras tienen los mismos campos

- Siempre (en todas las obras):

  - Número de catálogo (único por obra)

  - Título

  - Fecha

- Casi siempre:

  - Técnica (7.114/7.117)

  - Dimensión (7.107/7.117)

  - Procedencia (7.098/7.117)

  - Soporte (7.096/7.117)

- A veces:

  - Serie (1.415/7.117)

- Muy raros:

  - Materia, Lugar de producción, Edición / Estado

## gestion_json.py

CONTENIDO: código para la gestión de la información obtenida del scrapping, unificación de esta

MODIFICACIONES APLICADAS: indicadas en los comendarios del archivo

- Archivo entrada: 'prado_ficha_tecnica.jsonl'

- Archivo salida: 'prado.jsonl'

## json_a_csv.py

CONTENIDO: código para transformar el archivo jsonl a un csv y poder gestionarlo en RStudio

- Archivo entrada: 'prado.jsonl'

- Archivo salida: 'prado.csv'

## padro.jsonl, padro.csv

CONTENIDO: 

- Archivos finales después del scrapping. 

- No estan preparados para el análisi todavía.

- Datos faltantes indicados con "0"

VARIABLES: alto, ancho, autor, autora, autores, estado, fecha, lugar_produccion, materia, numero_catalogo, procedencia, serie, soporte, tecnica, titulo, url

## gestion_datos.Rmd

CONTENIDO: codigo y redacción de la sección de gestión de datos

- Archivo entrada: 'prado.csv'

- Archivos salida; 'prado_gestionado.csv', 'prado_variables.csv'

## prado_gestionado.csv

CONTENIDO: base de datos preparada para el analisis

- Contiene variables no analizables pero con información de posible interés futuro: numero_catalogo, titulo, url, nombre_autor, procedencia

## prado_variables.csv

CONTENIDO: base de datos preparada para el analisis

- Contiene únicamente las variables analizables
