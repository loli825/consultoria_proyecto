import json

inp = "prado_ficha_tecnica.jsonl"
out = "prado.jsonl"

with open(inp, "r", encoding="utf-8") as f_in, open(out, "w", encoding="utf-8") as f_out:
    for line in f_in:
        d = json.loads(line)

        # (1) conservar solo 'url' y 'ficha_tecnica'
        nuevo = {
            "url": d["url"],
            "ficha_tecnica": d["ficha_tecnica"],
        }

        # (2) aplanar diccionario 'ficha_tecnica'
        for k, v in nuevo["ficha_tecnica"].items():
            if k in nuevo:
                nuevo["ficha_tecnica__" + k] = v
            else:
                nuevo[k] = v
        del nuevo["ficha_tecnica"]

        # (3) eliminar registros que no tengan la clave 'Dimensión'
        if "Dimensión" not in nuevo:
            continue

        # (4) Conservar solo claves de dimension 'Alto' y 'Ancho'
        dim = nuevo["Dimensión"]
        partes = [p.strip() for p in dim.split(";") if p.strip()]

        alto_part = None
        ancho_part = None
        for p in partes:
            if p.startswith("Alto:"):
                alto_part = p
            elif p.startswith("Ancho:"):
                ancho_part = p

        if alto_part is None or ancho_part is None:
            continue

        partes = [alto_part, ancho_part]

        # (5) dividir 'Dimensión' en dos claves: 'Alto' y 'Ancho' (y eliminar 'Dimensión')
        nuevo["Alto"] = partes[0].split(":", 1)[1].strip()
        nuevo["Ancho"] = partes[1].split(":", 1)[1].strip()
        del nuevo["Dimensión"]

        # (6) quitar unidades en 'Alto' y 'Ancho'
        nuevo["Alto"] = nuevo["Alto"].split()[0]
        nuevo["Ancho"] = nuevo["Ancho"].split()[0]

        # (7) Renombrar claves
        RENOMBRAR = {
            "Alto": "alto",
            "Ancho": "ancho",
            "Autor": "autor",
            "Autora": "autora",
            "Autores": "autores",
            "Edición / Estado": "estado",
            "Fecha": "fecha",
            "Lugar de producción": "lugar_produccion",
            "Materia": "materia",
            "Número de catálogo": "numero_catalogo",
            "Procedencia": "procedencia",
            "Serie": "serie",
            "Soporte": "soporte",
            "Técnica": "tecnica",
            "Título": "titulo",
            "url": "url",
        }

        renombrado = {}
        for k, v in nuevo.items():
            renombrado[RENOMBRAR.get(k, k)] = v
        nuevo = renombrado

        f_out.write(json.dumps(nuevo, ensure_ascii=False) + "\n")

