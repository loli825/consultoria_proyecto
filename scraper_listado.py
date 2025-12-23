### Código para obtener el listado de obras y su URL

# =========================
# LIBRERÍAS
# =========================
import csv
import json
import math
import random
import time
from pathlib import Path
from typing import Dict, List, Optional, Tuple  

from bs4 import BeautifulSoup
from curl_cffi import Session


# =========================
# CONFIGURACIÓN (a partir de la cURL)
# =========================
URL = "https://servicios.museodelprado.es/resultados/CargadorResultados/CargarResultados"

HEADERS = {
    "accept": "application/json, text/javascript, */*; q=0.01",
    "accept-language": "es,es-ES;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
    "content-type": "application/x-www-form-urlencoded; charset=UTF-8",
    "origin": "https://www.museodelprado.es",
    "referer": "https://www.museodelprado.es/coleccion/obras-de-arte?cidoc:p2_has_type@@@pm:objectTypeNode=http://museodelprado.es/items/objecttype_20",
    "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36 Edg/143.0.0.0",
    "x-requested-with": "XMLHttpRequest",
}

# Cookie personal
COOKIE_HEADER = "PONER COOKIE PERSONAL AQUÍ"

P_PARAMETROS_BASE = "cidoc:p2_has_type@@@pm:objectTypeNode=http://museodelprado.es/items/objecttype_20"

FORM_BASE = {
    "pUsarMasterParaLectura": "false",
    "pProyectoID": "7317a29a-d846-4c54-9034-6a114c3658fe",
    "pEsUsuarioInvitado": "true",
    "pIdentidadID": "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF",
    "pLanguageCode": "es",
    "pPrimeraCarga": "false",  
    "pAdministradorVeTodasPersonas": "false",
    "pTipoBusqueda": "0",
    "pNumeroParteResultados": "1",
    "pGrafo": "7317a29a-d846-4c54-9034-6a114c3658fe",
    "pFiltroContexto": "",
    "pParametros_adiccionales": (
        "PestanyaActualID=c89fbb0c-a52c-4700-9220-79f4964d3949|"
        "rdf:type=pmartwork|"
        "orden=asc|"
        "ordenarPor=pm:relevance,ecidoc:p62_E52_p79_has_time-span_beginning,"
        "ecidoc:p62_E52_p80_has_time-span_end,gnoss:hasfechapublicacion"
    ),
    "cont": "0",
}

# ARCHIVOS DE SALIDA
OUTPUT_CSV = Path("prado_listado.csv") # listado final
CHECKPOINT = Path("prado_checkpoint.json") # checpoint para reanudar
DEBUG_DIR = Path("debug_respuestas") # respuestas problemáticas
DEBUG_DIR.mkdir(exist_ok=True)

# Paudad para parecer "humano"
BASE_DELAY = 1.8
JITTER = 1.2
PAUSE_EVERY = 25
PAUSE_SECONDS = 15


# =========================
# FUNCIONES
# =========================
def atomic_write_json(path: Path, data: dict) -> None:
    # Escribe un JSON de forma segura (archivo temporal + rename)
    tmp = path.with_suffix(".tmp")
    tmp.write_text(json.dumps(data, ensure_ascii=False, indent=2), encoding="utf-8")
    tmp.replace(path)

def load_checkpoint() -> dict:
    # Carga el checkpoint (para reanudar la ejecución)
    if CHECKPOINT.exists():
        try:
            return json.loads(CHECKPOINT.read_text(encoding="utf-8"))
        except Exception:
            return {}
    return {}

def load_seen_ids(csv_path: Path) -> set:
    # Lee el CSV existente y devuelve los obra_id ya guardados (evita duplicados)
    seen = set()
    if not csv_path.exists():
        return seen
    with csv_path.open("r", encoding="utf-8", newline="") as f:
        r = csv.DictReader(f)
        for row in r:
            oid = row.get("obra_id")
            if oid:
                seen.add(oid)
    return seen

def infer_next_page_from_csv(seen_count: int, page_size_guess: int = 18) -> int:
    # Estima la página por la que continuar según cuántas obras hay ya en el CSV
    return max(1, (seen_count // page_size_guess) + 1)

def looks_like_cloudflare_or_block(text: str) -> bool:
    # Detecta si la respuesta parece un bloqueo/verificación (Cloudflare, etc.)
    t = (text or "").lower()
    signals = [
        "checking your browser",
        "attention required",
        "cloudflare",
        "cf-chl",
        "challenge",
        "verify you are human",
        "access denied",
    ]
    return any(s in t for s in signals)


# =========================
# PARSEO
# =========================
def parse_items_from_value(value_html: str) -> List[Dict[str, str]]:
    # Parsea el HTML del campo "Value" y extrae una lista de obras (id, url, título, autor, soporte, imagen)
    soup = BeautifulSoup(value_html, "lxml")
    out: List[Dict[str, str]] = []

    for fig in soup.select("figure"):
        a_title = fig.select_one("p.titulo a")
        a_img = fig.select_one(".imgwrap a[href*='/coleccion/obra-de-arte/']")

        url = (a_img.get("href") if a_img else None) or (a_title.get("href") if a_title else None)
        if not url:
            continue

        titulo = a_title.get_text(" ", strip=True) if a_title else ""

        a_autor = fig.select_one("p.autor a")
        autor = a_autor.get_text(" ", strip=True) if a_autor else ""

        p_soporte = fig.select_one("p.soporte")
        soporte = p_soporte.get_text(" ", strip=True) if p_soporte else ""

        img = fig.select_one("img")
        img_url = img.get("src", "") if img else ""

        obra_id = url.rstrip("/").split("/")[-1]

        out.append({
            "obra_id": obra_id,
            "url": url,
            "titulo": titulo,
            "autor": autor,
            "soporte": soporte,
            "img_url": img_url,
        })

    return out


# =========================
# DESCARGA 
# =========================
def fetch_page(session: Session, page: int, retries: int = 10) -> Tuple[List[Dict[str, str]], Optional[int]]:
    # Descarga una página de resultados (JSON), reintenta si falla, y devuelve (items, total_results)
    form = dict(FORM_BASE)
    form["pParametros"] = f"{P_PARAMETROS_BASE}|pagina={page}"
    form["pPrimeraCarga"] = "true" if page == 1 else "false"

    last_text = ""
    last_status = None

    for attempt in range(1, retries + 1):
        r = session.post(
            URL,
            data=form,
            impersonate="chrome",
            timeout=60,
        )

        last_status = r.status_code
        content = (r.content or b"").strip()
        ctype = (r.headers.get("content-type") or "").lower()
        last_text = (r.text or "")[:1200]

        if r.status_code in (403, 429):
            wait = min(120, 6 * attempt + random.random() * 3)
            print(f"[WARN] {r.status_code} en pág {page}. Reintento {attempt}/{retries} en {wait:.1f}s...")
            time.sleep(wait)
            continue

        if not content:
            wait = min(120, 6 * attempt + random.random() * 3)
            print(f"[WARN] Respuesta vacía en pág {page}. Reintento {attempt}/{retries} en {wait:.1f}s...")
            time.sleep(wait)
            continue

        if "application/json" not in ctype and not content.startswith(b"{"):
            dbg = DEBUG_DIR / f"nojson_pagina_{page}.html"
            dbg.write_text(r.text or "", encoding="utf-8", errors="ignore")

            if looks_like_cloudflare_or_block(r.text or ""):
                raise RuntimeError(
                    f"Bloqueo/verificación detectada en página {page}. "
                    f"Se guardó la respuesta en {dbg}."
                )

            wait = min(120, 8 * attempt + random.random() * 4)
            print(f"[WARN] No-JSON en pág {page} (guardado {dbg.name}). Reintento {attempt}/{retries} en {wait:.1f}s...")
            time.sleep(wait)
            continue

        try:
            data = r.json()
        except Exception:
            dbg = DEBUG_DIR / f"jsonerror_pagina_{page}.txt"
            dbg.write_text(r.text or "", encoding="utf-8", errors="ignore")

            if looks_like_cloudflare_or_block(r.text or ""):
                raise RuntimeError(
                    f"JSON inválido por verificación/bloqueo en página {page}. "
                    f"Se guardó la respuesta en {dbg}."
                )

            wait = min(120, 8 * attempt + random.random() * 4)
            print(f"[WARN] JSON inválido en pág {page} (guardado {dbg.name}). Reintento {attempt}/{retries} en {wait:.1f}s...")
            time.sleep(wait)
            continue

        total_results = data.get("Key") if isinstance(data, dict) else None
        value_html = (data.get("Value", "") if isinstance(data, dict) else "") or ""
        items = parse_items_from_value(value_html)
        return items, total_results

    dbg = DEBUG_DIR / f"agotados_intentos_pagina_{page}.txt"
    dbg.write_text(f"status={last_status}\n\n{last_text}", encoding="utf-8", errors="ignore")
    raise RuntimeError(
        f"No se pudo obtener JSON válido en página {page} tras {retries} intentos. Guardé info en {dbg}."
    )


# =========================
# MAIN CON CHECKPOINT
# =========================
def main():
    # Junta todo: reanuda desde checkpoint/CSV, recorre páginas, escribe CSV y guarda checkpoint
    cp = load_checkpoint()
    seen = load_seen_ids(OUTPUT_CSV)

    if cp.get("next_page"):
        start_page = int(cp["next_page"])
        print(f"📌 Reanudando desde checkpoint: página {start_page}")
    else:
        if seen:
            start_page = infer_next_page_from_csv(len(seen))
            print(f"📌 No hay checkpoint. CSV tiene {len(seen)} obras -> empiezo por página {start_page}")
        else:
            start_page = 1
            print("📌 Empiezo desde página 1")

    expected_pages = cp.get("expected_pages")
    total_results = cp.get("total_results")

    file_exists = OUTPUT_CSV.exists()

    with Session(headers=HEADERS) as session:
        session.headers["cookie"] = COOKIE_HEADER

        with OUTPUT_CSV.open("a", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=["obra_id", "url", "titulo", "autor", "soporte", "img_url"])
            if not file_exists:
                w.writeheader()

            page = start_page
            added_this_run = 0
            consecutive_empty = 0

            while True:
                try:
                    items, total_from_json = fetch_page(session, page)
                except Exception as e:
                    cp_update = {
                        "next_page": page,
                        "last_ok_page": cp.get("last_ok_page"),
                        "total_ids_in_csv": len(seen),
                        "added_this_run": added_this_run,
                        "total_results": total_results,
                        "expected_pages": expected_pages,
                        "last_error": str(e),
                    }
                    atomic_write_json(CHECKPOINT, cp_update)

                    print("\n⛔ Se ha detenido por error/bloqueo.")
                    print(f"   Guardado checkpoint: {CHECKPOINT} (reanuda desde página {page})")
                    return

                if page == 1 and total_from_json is not None:
                    total_results = int(total_from_json)
                    expected_pages = math.ceil(total_results / 18)
                    print(f"ℹ️ Total resultados (Key): {total_results} -> páginas estimadas: {expected_pages}")

                if not items:
                    consecutive_empty += 1
                    print(f"[FIN?] Página {page} sin items (vacía) [{consecutive_empty}/2].")
                    if consecutive_empty >= 2:
                        break
                else:
                    consecutive_empty = 0

                new_count = 0
                for it in items:
                    if it["obra_id"] in seen:
                        continue
                    seen.add(it["obra_id"])
                    w.writerow(it)
                    new_count += 1

                f.flush()

                added_this_run += new_count
                print(f"[OK] Página {page}: {new_count} nuevas | total_csv={len(seen)}")

                cp_update = {
                    "next_page": page + 1,
                    "last_ok_page": page,
                    "total_ids_in_csv": len(seen),
                    "added_this_run": added_this_run,
                    "total_results": total_results,
                    "expected_pages": expected_pages,
                    "last_error": None,
                }
                atomic_write_json(CHECKPOINT, cp_update)

                if expected_pages is not None and page >= expected_pages:
                    print("✅ Llegamos a la última página estimada. Fin.")
                    break

                page += 1

                if PAUSE_EVERY and (page % PAUSE_EVERY == 0):
                    print(f"[PAUSA] {PAUSE_SECONDS}s para reducir riesgo de bloqueo...")
                    time.sleep(PAUSE_SECONDS)

                time.sleep(BASE_DELAY + random.random() * JITTER)

    print(f"\n✅ Terminado. CSV: {OUTPUT_CSV} | checkpoint: {CHECKPOINT}")

# =========================
# EJECUCIÓN
# =========================
if __name__ == "__main__":
    main()
