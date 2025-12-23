# consultoria_proyecto
scraping en Python (.py) y analisis en R (.Rmd)

# REGLAS

No trabajar directamente en main porque se pisan cambios, aparecen conflictos y no sabemso que version es la buena.

En main solo entra el código estable y final.

No editar partes de los demas (podemos trabajar sobre le mismo documento .Rmd a la vez si solo editamos nuestra parte porque github hace merge correcto y se unen las modificacioens, pero si editamos la misma parte a la vez se genera conflicto de versiones)

## Pasos

- Cada persona crea su branch

- Trabaja tranquila

- Hace commits

- Pide “ok” al equipo

- Merge a main

- Borra la branch

EXTRA: ir anotando los cambios/procesos que llevan a cabo en la siguiente sección 'GRUÍA DE TRABAJO' para llevar un control (posiblemente solo sea necesario para la fase de scraping)

# GRUÍA DE TRABAJO

- scrapear listado de obras y obtener URLs ('scraper_listado.py' --> 'prado_listado.csv')

- scrapear cada obra y obtener fichas técnicas ('scraper_ficha_tecnica.py' --> 'prado_ficha_técnica.jsonl')

- revisar y arreglar fichas técnicas, construir dataset base para trabajar (...en proceso...)

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

CONTENIDO: listado de todas las obras que son "Tipo de obra: Pinturas" con la información que aparece en la página del listado de la web

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

ESTRUCTURA: 7.117 registros (mismo número de obras que en el CSV), 1 JSON por línea y obra

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


