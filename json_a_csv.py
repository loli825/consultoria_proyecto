import json
import csv

INPUT = "prado.jsonl"
OUTPUT = "prado.csv"

COLS = [
    "alto", "ancho", "autor", "autora", "autores", "estado", "fecha",
    "lugar_produccion", "materia", "numero_catalogo", "procedencia",
    "serie", "soporte", "tecnica", "titulo", "url"
]

with open(INPUT, "r", encoding="utf-8") as fin, open(OUTPUT, "w", newline="", encoding="utf-8") as fout:
    w = csv.DictWriter(fout, fieldnames=COLS)
    w.writeheader()

    for line in fin:
        if not line.strip():
            continue
        obj = json.loads(line)
        w.writerow({k: obj.get(k, 0) for k in COLS})

