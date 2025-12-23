### Codigo para obtener la información/ficha técnica de cada obra

# =====================
# LIBRERÍAS
# =====================
from __future__ import annotations

import asyncio
import csv
import json
import os
import re
from typing import Dict, Any, List, Set, Optional, Tuple

from playwright.async_api import async_playwright, TimeoutError as PWTimeoutError  # type: ignore


# =====================
# CONFIGURACIÓN
# =====================
INPUT_CSV = "prado_listado.csv" # archivo CSV con las URLs a procesar
OUTPUT_JSONL = "prado_ficha_tecnica.jsonl"   # archivo final JSONL con las fichas técnicas
HEADLESS = False # NO usar headless para evitar detecciones anti-bot

CONCURRENCY = 6          # número de páginas que se procesan a la vez
TIMEOUT_MS = 30000       # timeout de carga por URL
RETRY_PER_URL = 2        # reintentos por URL

WAIT_SELECTOR_MS = 8000 # tiempo de carga máximo para selectores ficha técnica
SLEEP_BETWEEN_TASKS_SEC = 0.0 # tiempo entre peticiones (para parecer más "humano")


# =====================
# CHECPOINTS / CARGA DE URLS
# =====================
def load_urls_from_csv(path: str) -> List[str]:
    # Lee el CSV y devuelve una lista con todas las URLs (columna "url")
    
    if not os.path.exists(path):
        raise FileNotFoundError(f"No existe el archivo: {path}")

    with open(path, "r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        if not reader.fieldnames:
            raise ValueError("El CSV no tiene cabeceras.")

        field_map = {name.strip().lower(): name for name in reader.fieldnames}
        if "url" not in field_map:
            raise ValueError(f"No encuentro la columna 'url'. Columnas: {reader.fieldnames}")

        url_field = field_map["url"]
        urls: List[str] = []
        for row in reader:
            u = (row.get(url_field) or "").strip()
            if u:
                urls.append(u)
        return urls


def load_processed_ok_urls(jsonl_path: str) -> Set[str]:
    # Lee el JSONL y devuelve el set de URLs que ya tienen ok=true (para reanudar sin repetir).
    
    done: Set[str] = set()
    if not os.path.exists(jsonl_path):
        return done

    with open(jsonl_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
                if obj.get("ok") is True and isinstance(obj.get("url"), str):
                    done.add(obj["url"])
            except Exception:
                continue
    return done


# =====================
# SCRAPE HELPERS
# =====================
def _normalize_space(s: str) -> str:
    # Normaliza espacios: convierte múltiples espacios/saltos en uno solo
    return re.sub(r"\s+", " ", s).strip()


async def try_accept_cookies(page) -> None:
    # Intenta clicar un botón de cookies (si aparece)
    cookie_regex = re.compile(r"aceptar|acepto|accept|agree|consent", re.I)
    try:
        await page.get_by_role("button", name=cookie_regex).first.click(timeout=1500)
    except Exception:
        pass


async def extract_dt_dd(page) -> Dict[str, Any]:
    # Extrae la ficha técnica leyendo el <dl> (pares dt->dd) dentro de section#ficha.

    try:
        await page.wait_for_selector("section#ficha div.ficha-tecnica dl dt", timeout=WAIT_SELECTOR_MS)
    except Exception:
        return {}

    pairs: List[List[str]] = await page.eval_on_selector_all(
        "section#ficha div.ficha-tecnica dl dt",
        """(dts) => dts.map(dt => {
            const dd = dt.nextElementSibling;
            const k = (dt.innerText || "").trim();
            const v = dd ? (dd.innerText || "").trim() : "";
            return [k, v];
        })"""
    )

    ficha: Dict[str, Any] = {}
    for k, v in pairs:
        key = _normalize_space((k or "")).replace(":", "")
        val = _normalize_space((v or ""))
        if not key or not val:
            continue
        if key in ficha:
            if isinstance(ficha[key], list):
                ficha[key].append(val)
            else:
                ficha[key] = [ficha[key], val]
        else:
            ficha[key] = val

    return ficha


async def ensure_ficha_visible(page) -> None:
    # Intenta abrir la pestaña "FICHA TÉCNICA" y hacer scroll hasta la sección de ficha.

    try:
        await page.get_by_role("link", name=re.compile(r"FICHA TÉCNICA", re.I)).first.click(timeout=2500)
    except Exception:
        try:
            await page.locator("text=FICHA TÉCNICA").first.click(timeout=2500)
        except Exception:
            pass

    try:
        await page.locator("section#ficha").scroll_into_view_if_needed(timeout=2500)
    except Exception:
        pass


async def scrape_one(page, url: str) -> Tuple[bool, Dict[str, Any], Optional[str]]:
    # Abre una URL y devuelve (ok, ficha_tecnica, error)
    try:
        await page.goto(url, wait_until="domcontentloaded", timeout=TIMEOUT_MS)
        await try_accept_cookies(page)

        ficha = await extract_dt_dd(page)
        if ficha:
            return True, ficha, None

        await ensure_ficha_visible(page)
        ficha = await extract_dt_dd(page)
        if ficha:
            return True, ficha, None

        return False, {}, "No se encontró dl/dt/dd en la ficha técnica"

    except PWTimeoutError as e:
        return False, {}, f"Timeout: {e}"
    except Exception as e:
        return False, {}, f"{type(e).__name__}: {e}"


# =====================
# MAIN (ASYNC + CONCURRENCY)
# =====================
async def main() -> None:
    # Junta todo: lee CSV, filtra pendientes, ejecuta workers en paralelo y guarda JSONL
    urls = load_urls_from_csv(INPUT_CSV)
    done_ok = load_processed_ok_urls(OUTPUT_JSONL)

    pending = [u for u in urls if u not in done_ok]

    print(f"Total URLs CSV: {len(urls)}")
    print(f"OK ya guardadas en {OUTPUT_JSONL}: {len(done_ok)}")
    print(f"Pendientes: {len(pending)}")

    if not pending:
        print("Nada que hacer.")
        return

    write_lock = asyncio.Lock()              # evita escrituras simultáneas al JSONL
    sem = asyncio.Semaphore(CONCURRENCY)     # limita cuántos workers corren a la vez

    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=HEADLESS,
            args=["--disable-blink-features=AutomationControlled"],
        )

        context = await browser.new_context(
            locale="es-ES",
            viewport={"width": 1400, "height": 900},
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/120.0.0.0 Safari/537.36"
            ),
        )

        await context.add_init_script(
            "Object.defineProperty(navigator, 'webdriver', {get: () => undefined});"
        )

        async def route_handler(route, request):
            # Bloquea recursos pesados (imágenes, fuentes, etc.) y trackers para ir más rápido
            rtype = request.resource_type
            if rtype in ("image", "media", "font"):
                await route.abort()
                return
            url = request.url.lower()
            if "google-analytics" in url or "doubleclick" in url or "facebook" in url:
                await route.abort()
                return
            await route.continue_()

        await context.route("**/*", route_handler)

        async def worker(url: str) -> None:
            # Procesa una URL: scrapea ficha técnica y escribe una línea en el JSONL si ok
            async with sem:
                page = await context.new_page()
                ok = False
                ficha: Dict[str, Any] = {}
                err: Optional[str] = None

                for attempt in range(1, RETRY_PER_URL + 1):
                    ok, ficha, err = await scrape_one(page, url)
                    if ok:
                        break

                await page.close()

                # Si falla: parar ejecución 
                if not ok:
                    msg = f"FAIL -> {url} | error: {err}"
                    print(msg)
                    raise RuntimeError(msg)

                # Si ok: escribir el resultado en JSONL
                record = {
                    "url": url,
                    "ok": True,
                    "ficha_tecnica": ficha,
                    "error": None,
                }

                async with write_lock:
                    with open(OUTPUT_JSONL, "a", encoding="utf-8") as f:
                        f.write(json.dumps(record, ensure_ascii=False) + "\n")

                print(f"OK -> {url}")

                if SLEEP_BETWEEN_TASKS_SEC:
                    await asyncio.sleep(SLEEP_BETWEEN_TASKS_SEC)

        tasks = [asyncio.create_task(worker(u)) for u in pending]

        # Ejecuta tareas y se detiene en el primer error
        done, pending_tasks = await asyncio.wait(tasks, return_when=asyncio.FIRST_EXCEPTION)

        # Recolecta la primera excepción (si hubo)
        first_exc: Optional[BaseException] = None
        for t in done:
            exc = t.exception()
            if exc is not None:
                first_exc = exc
                break

        # Si hubo error: cancela el resto y relanza la excepción
        if first_exc is not None:
            for t in pending_tasks:
                t.cancel()
            await asyncio.gather(*pending_tasks, return_exceptions=True)
            await browser.close()
            raise first_exc

        await browser.close()

    print(f"✅ Listo. Resultados en: {OUTPUT_JSONL}")

# =====================
# EJECUCIÓN
# =====================
if __name__ == "__main__":
    asyncio.run(main())
