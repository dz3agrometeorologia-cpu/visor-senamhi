# =============================================================================
# VISOR DE MONITOREO AGROMETEOROLÓGICO · SENAMHI · DIRECCIÓN ZONAL 3
# =============================================================================

library(shiny)
library(leaflet)
library(reticulate)

# En tu PC: usar el entorno de Python "rgee". En la nube no se toca.
if (!nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
  try(reticulate::use_condaenv("rgee", required = TRUE), silent = TRUE)
}

PROYECTO_GCP <- "capable-boulder-511318-c7"
RUTA_ASSETS  <- paste0("projects/", PROYECTO_GCP, "/assets/")
RUTA_AOI     <- paste0(RUTA_ASSETS, "dz3_Senamhi")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

hoy_pe <- function() as.Date(format(Sys.time(), tz = "America/Lima"))

# Mensajes con hora: aparecen en los logs de Posit y muestran en qué paso se demora
t_log <- function(x) message("[VISOR ", format(Sys.time(), "%H:%M:%S"), "] ", x)

# -----------------------------------------------------------------------------
# Visualización: mismos rangos, paletas y leyendas del script de Earth Engine
# -----------------------------------------------------------------------------
VIS <- list(
  NDVI = list(
    titulo  = "🌿 NDVI - Vigor vegetativo",
    min = 0, max = 0.8,
    palette = c("#8B0000", "#CD5C5C", "#F0E68C", "#9ACD32", "#228B22", "#006400"),
    marcas  = c("0", "0.4", "0.8"),
    clases  = list(
      list("#8B0000", "Suelo desnudo/estrés severo"),
      list("#F0E68C", "Vegetación escasa"),
      list("#228B22", "Vegetación saludable"),
      list("#006400", "Vegetación vigorosa"))),
  NDMI = list(
    titulo  = "💧 NDMI - Humedad del suelo",
    min = -0.4, max = 0.4,
    palette = c("#8B4513", "#D2691E", "#F4A460", "#87CEEB", "#4682B4", "#000080"),
    marcas  = c("-0.4", "0", "0.4"),
    clases  = list(
      list("#8B4513", "Muy seco (estrés hídrico)"),
      list("#F4A460", "Seco (baja humedad)"),
      list("#87CEEB", "Húmedo (óptimo)"),
      list("#000080", "Saturado (exceso agua)")))
)

# Clases para el valor de un punto (cortes aprobados)
clase_ndvi <- function(x) {
  if (x < 0.16)      list(color = "#8B0000", etiqueta = "Suelo desnudo/estrés severo")
  else if (x < 0.48) list(color = "#F0E68C", etiqueta = "Vegetación escasa")
  else if (x < 0.72) list(color = "#228B22", etiqueta = "Vegetación saludable")
  else               list(color = "#006400", etiqueta = "Vegetación vigorosa")
}
clase_ndmi <- function(x) {
  if (x < -0.24)    list(color = "#8B4513", etiqueta = "Muy seco (estrés hídrico)")
  else if (x < 0)   list(color = "#F4A460", etiqueta = "Seco (baja humedad)")
  else if (x < 0.24) list(color = "#87CEEB", etiqueta = "Húmedo (óptimo)")
  else              list(color = "#000080", etiqueta = "Saturado (exceso agua)")
}

# Iconos (SVG) de las tarjetas de valores
svg_hoja <- function(px, vena) {
  paste0('<svg viewBox="0 0 24 24" width="', px, '" height="', px, '">',
         '<path d="M20 4C11 4 4.5 9.5 4.5 16.5c0 1.2.2 2.3.6 3.3 1 .4 2.1.6 3.3.6C15.5 20.4 20 13 20 4z" fill="currentColor"/>',
         '<path d="M5 20c2-5 5.5-9 11-12" fill="none" stroke-width="1.6" stroke-linecap="round" style="stroke:var(', vena, ')"/></svg>')
}
svg_gota <- function(px, vena) {
  paste0('<svg viewBox="0 0 24 24" width="', px, '" height="', px, '">',
         '<path d="M12 2.5C12 2.5 5.5 10 5.5 14.5a6.5 6.5 0 0 0 13 0C18.5 10 12 2.5 12 2.5z" fill="currentColor"/>',
         '<path d="M9 15a3 3 0 0 0 3 3" fill="none" stroke-width="1.6" stroke-linecap="round" style="stroke:var(', vena, ')"/></svg>')
}
ICONO_PIN <- '<svg viewBox="0 0 24 24" width="16" height="16"><path fill="currentColor" d="M12 2a7 7 0 0 0-7 7c0 5 7 13 7 13s7-8 7-13a7 7 0 0 0-7-7zm0 9.5A2.5 2.5 0 1 1 12 6.5a2.5 2.5 0 0 1 0 5z"/></svg>'
ICONO_OBJETIVO <- '<svg viewBox="0 0 24 24" width="16" height="16"><circle cx="12" cy="12" r="3" fill="currentColor"/><circle cx="12" cy="12" r="7" stroke="currentColor" stroke-width="2" fill="none"/><path d="M12 1.5v4M12 18.5v4M1.5 12h4M18.5 12h4" stroke="currentColor" stroke-width="2" fill="none"/></svg>'
ICONO_FLECHA <- '<svg viewBox="0 0 24 24" width="14" height="14"><path d="M6 9l6 6 6-6" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/></svg>'

# Formulario desplegable "Ir a coordenadas" (va dentro del cuadro de valores)
FORM_COORD <- '<div id="pc-form" class="pc-form"><div class="pc-tabs"><button type="button" id="pc-tab-geo" class="activo" data-modo="geo" onclick="modoCoord(this.dataset.modo)">Geográficas</button><button type="button" id="pc-tab-utm" data-modo="utm" onclick="modoCoord(this.dataset.modo)">UTM</button></div><div id="pc-geo"><div class="pc-fila"><div class="pc-campo"><label for="pc-lat">Latitud</label><input id="pc-lat" type="text" placeholder="-7.377491" autocomplete="off"></div><div class="pc-campo"><label for="pc-lon">Longitud</label><input id="pc-lon" type="text" placeholder="-78.964927" autocomplete="off"></div></div><div class="pc-ayuda">Decimales (-7.377491) o 7°22′39″ S · WGS84</div></div><div id="pc-utm" style="display:none"><div class="pc-fila"><div class="pc-campo"><label for="pc-zona">Zona</label><select id="pc-zona"><option value="17" selected>17</option><option value="18">18</option><option value="19">19</option></select></div><div class="pc-campo"><label for="pc-hem">Hemisferio</label><select id="pc-hem"><option value="S" selected>Sur</option><option value="N">Norte</option></select></div></div><div class="pc-fila"><div class="pc-campo"><label for="pc-este">Este (m)</label><input id="pc-este" type="text" placeholder="724636" autocomplete="off"></div><div class="pc-campo"><label for="pc-norte">Norte (m)</label><input id="pc-norte" type="text" placeholder="9184008" autocomplete="off"></div></div><div class="pc-ayuda">Metros, sin separador de miles · WGS84 · aquí: 17 Sur (oeste) o 18 Sur (este)</div></div><button type="button" class="pc-ir" onclick="irACoordenadas()">Localizar</button><div id="pc-msg" class="pc-msg"></div></div>'

# Color de la tarjeta según la clase del valor
tema_de <- function(etq) {
  if (grepl("^Suelo desnudo", etq)) "rojo"
  else if (grepl("^Vegetaci", etq)) "verde"
  else if (grepl("^(Muy )?[Ss]eco", etq)) "naranja"
  else "azul"
}

# Cabecera (Lat | Lon + botones) y formulario de coordenadas
bloque_cabecera <- function(lat, lng) {
  la <- formatC(lat, format = "f", digits = 6)
  lo <- formatC(lng, format = "f", digits = 6)
  paste0('<div class="vc-caja"><div class="vc-head">', ICONO_PIN,
         '<span><b>Lat:</b>', la, '</span><i class="vc-sep"></i>',
         '<span><b>Lon:</b>', lo, '</span>',
         '<div class="vc-btns">',
         '<button type="button" title="Acercar a este punto" onclick="centrarPunto(',
         la, ',', lo, ')">', ICONO_OBJETIVO, '</button></div></div></div>')
}

armar_aviso <- function(texto, lat, lng) {
  paste0('<div class="vc">', bloque_cabecera(lat, lng),
         '<div class="vc-aviso">', texto, '</div></div>')
}

armar_popup <- function(v, lat, lng) {
  tarjeta <- function(nombre, subtitulo, valor, clasif, icono) {
    if (is.null(valor) || length(valor) == 0 || is.na(valor)) {
      tema <- "gris"; txt <- "—"; etq <- "Sin dato en este punto"
    } else {
      cl   <- clasif(valor)
      tema <- tema_de(cl$etiqueta)
      txt  <- formatC(valor, format = "f", digits = 3)
      etq  <- cl$etiqueta
    }
    paste0('<div class="vc-card t-', tema, '"><div class="vc-top">',
           '<div class="vc-ico">', icono(20, "--vc"), '</div>',
           '<div class="vc-tit"><b>', nombre, '</b><span>', subtitulo, '</span></div>',
           '<div class="vc-val">', txt, '</div></div>',
           '<div class="vc-pill">', icono(16, "--vc-pill"), '<span>', etq, '</span></div></div>')
  }
  paste0('<div class="vc">', bloque_cabecera(lat, lng),
         tarjeta("NDVI", "Vegetación", v$NDVI, clase_ndvi, svg_hoja),
         tarjeta("NDMI", "Humedad del suelo", v$NDMI, clase_ndmi, svg_gota),
         '</div>')
}

# -----------------------------------------------------------------------------
# Código Python (API de Earth Engine): réplica del script del Code Editor
# -----------------------------------------------------------------------------
PY_CODIGO <- r"---(
import ee
import json
import math

RUTA_AOI = '__RUTA_AOI__'


def _mask_s2(image):
    scl = image.select('SCL')
    buenos = scl.eq(4).Or(scl.eq(5)).Or(scl.eq(6))
    qa = image.select('QA60')
    nubes = qa.bitwiseAnd(1 << 10).eq(0).And(qa.bitwiseAnd(1 << 11).eq(0))
    return (image.updateMask(buenos.And(nubes))
            .divide(10000)
            .copyProperties(image, ['system:time_start']))


def _mask_l9(image):
    qa = image.select('QA_PIXEL')
    claro = (qa.bitwiseAnd(1 << 3).eq(0)
             .And(qa.bitwiseAnd(1 << 4).eq(0))
             .And(qa.bitwiseAnd(1 << 5).eq(0)))
    return (image.updateMask(claro)
            .multiply(0.0000275).add(-0.2)
            .copyProperties(image, ['system:time_start']))


def _agregar_indices(image):
    ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI')
    ndmi = image.normalizedDifference(['B8', 'B11']).rename('NDMI')
    return image.addBands([ndvi, ndmi])


def compuesto(inicio, fin):
    aoi = ee.FeatureCollection(RUTA_AOI)
    s2 = (ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
          .filterBounds(aoi)
          .filterDate(inicio, fin)
          .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 90))
          .select(['B4', 'B8', 'B11', 'SCL', 'QA60'])   # solo las bandas que se usan
          .map(_mask_s2))
    l9 = (ee.ImageCollection('LANDSAT/LC09/C02/T1_L2')
          .filterBounds(aoi)
          .filterDate(inicio, fin)
          .select(['SR_B4', 'SR_B5', 'SR_B6', 'QA_PIXEL'])   # solo las bandas que se usan
          .map(_mask_l9)
          .select(['SR_B5', 'SR_B6', 'SR_B4'],
                  ['B8', 'B11', 'B4']))
    combinada = s2.merge(l9)
    n = combinada.size().getInfo()
    con_indices = combinada.map(_agregar_indices)
    ndvi = con_indices.select('NDVI').median().clip(aoi)
    ndmi = con_indices.select('NDMI').median().clip(aoi)
    img = ee.Image.cat([ndvi.rename('NDVI'), ndmi.rename('NDMI')])
    return {'img': img, 'n': n}


MAX_PARCELAS = 200
_A = 6378137.0
_F = 1 / 298.257223563
_E2 = _F * (2 - _F)
_E = math.sqrt(_E2)


def _q(phi):
    s = math.sin(phi)
    return (1 - _E2) * (s / (1 - _E2 * s * s)
                        - (1 / (2 * _E)) * math.log((1 - _E * s) / (1 + _E * s)))


def _area_anillo(anillo):
    # Área (m²) de un anillo [lon, lat] sobre el elipsoide WGS84, con la proyección
    # cilíndrica equivalente de Lambert (conserva el área): mismo criterio que el
    # área elipsoidal de QGIS. Earth Engine usa una esfera y da ~0.65 % más en Perú.
    if len(anillo) < 3:
        return 0.0
    pts = [(_A * math.radians(p[0]), _A * _q(math.radians(p[1])) / 2.0) for p in anillo]
    x0, y0 = pts[0]
    n = len(pts)
    s = 0.0
    for i in range(n):
        x1, y1 = pts[i][0] - x0, pts[i][1] - y0
        x2, y2 = pts[(i + 1) % n][0] - x0, pts[(i + 1) % n][1] - y0
        s += x1 * y2 - x2 * y1
    return abs(s) / 2.0


def _area_polis(polis):
    total = 0.0
    for poli in polis:
        if not poli:
            continue
        total += _area_anillo(poli[0]) - sum(_area_anillo(h) for h in poli[1:])
    return max(total, 0.0)


def _parcelas_desde(datos):
    if isinstance(datos, dict):
        if datos.get('tipo') == 'parcelas':
            return [{'nombre': str(p.get('nombre') or ''), 'polis': p['coordinates']}
                    for p in datos['parcelas']]
        return [{'nombre': '', 'polis': datos['coordinates']}]
    return [{'nombre': '', 'polis': [[datos]]}]


def _areas_por_parcela(clase, fc, n):
    # clase: 0 = sin dato; 1..4 = rangos. Devuelve, por parcela, 5 áreas (m²)
    img = ee.Image.pixelArea().addBands(clase.toInt().rename('c'))
    info = img.reduceRegions(
        collection=fc,
        reducer=ee.Reducer.sum().group(groupField=1, groupName='c'),
        scale=10, tileScale=4).getInfo()
    out = [[0.0] * 5 for _ in range(n)]
    for f in info['features']:
        props = f.get('properties') or {}
        i = int(props['i'])
        for g in props.get('groups', []):
            out[i][int(round(g['c']))] += float(g['sum'])
    return out


def _ajustar_al_total(areas, total):
    suma = sum(areas)
    if suma <= 0:
        return [total, 0.0, 0.0, 0.0, 0.0]
    return [a / suma * total for a in areas]


def analizar_poligono(img, coords_json):
    datos = json.loads(coords_json)
    parcelas = [p for p in _parcelas_desde(datos) if p['polis']]
    if not parcelas:
        return json.dumps({'error': 'vacio'})
    if len(parcelas) > MAX_PARCELAS:
        return json.dumps({'error': 'demasiadas', 'n': len(parcelas)})
    totales = [_area_polis(p['polis']) for p in parcelas]
    parcelas = [p for p, t in zip(parcelas, totales) if t > 0]
    totales = [t for t in totales if t > 0]
    if not parcelas:
        return json.dumps({'error': 'vacio'})
    total = sum(totales)
    if total > 5e8:
        return json.dumps({'error': 'grande', 'total': total})
    feats = [ee.Feature(ee.Geometry.MultiPolygon(p['polis'], None, False), {'i': i})
             for i, p in enumerate(parcelas)]
    fc = ee.FeatureCollection(feats)
    ndvi = img.select('NDVI').unmask(-99, False)
    ndmi = img.select('NDMI').unmask(-99, False)
    # 0 = sin dato; 1..4 = rangos aprobados (mismos cortes del popup y la leyenda)
    c_ndvi = (ndvi.gte(-1).And(ndvi.lt(0.16)).multiply(1)
              .add(ndvi.gte(0.16).And(ndvi.lt(0.48)).multiply(2))
              .add(ndvi.gte(0.48).And(ndvi.lt(0.72)).multiply(3))
              .add(ndvi.gte(0.72).multiply(4)))
    c_ndmi = (ndmi.gte(-1).And(ndmi.lt(-0.24)).multiply(1)
              .add(ndmi.gte(-0.24).And(ndmi.lt(0)).multiply(2))
              .add(ndmi.gte(0).And(ndmi.lt(0.24)).multiply(3))
              .add(ndmi.gte(0.24).multiply(4)))
    a_ndvi = _areas_por_parcela(c_ndvi, fc, len(parcelas))
    a_ndmi = _areas_por_parcela(c_ndmi, fc, len(parcelas))
    salida = []
    suma_ndvi = [0.0] * 5
    suma_ndmi = [0.0] * 5
    for i, p in enumerate(parcelas):
        n1 = _ajustar_al_total(a_ndvi[i], totales[i])
        n2 = _ajustar_al_total(a_ndmi[i], totales[i])
        suma_ndvi = [a + b for a, b in zip(suma_ndvi, n1)]
        suma_ndmi = [a + b for a, b in zip(suma_ndmi, n2)]
        salida.append({'nombre': p['nombre'], 'total': totales[i], 'ndvi': n1, 'ndmi': n2})
    return json.dumps({'error': None, 'total': total, 'ndvi': suma_ndvi,
                       'ndmi': suma_ndmi, 'parcelas': salida})


def valor_punto(img, lon, lat):
    pt = ee.Geometry.Point([float(lon), float(lat)])
    return img.reduceRegion(reducer=ee.Reducer.first(),
                            geometry=pt, scale=10).getInfo()


def _redondear(c, n=4):
    if isinstance(c, (list, tuple)):
        return [_redondear(x, n) for x in c]
    return round(c, n)


def contorno_pais(ruta, error_m):
    # Contorno exterior del país: une todos los departamentos en un solo polígono
    fc = ee.FeatureCollection(ruta)
    g = fc.geometry(500).dissolve(500)
    try:
        # Lago Titicaca: el polígono de departamentos deja fuera la parte del lago que
        # corresponde al Perú y el contorno se desvía por la orilla. Se completa esa parte
        # con el límite internacional oficial (LSIB) solo dentro de la zona del lago
        # (todas las operaciones pesadas se hacen únicamente en esa caja pequeña).
        lsib = (ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017')
                .filter(ee.Filter.eq('country_na', 'Peru')).geometry(200))
        caja = ee.Geometry.Rectangle([-70.6, -17.3, -68.5, -14.5])
        g_local = g.intersection(caja, 200)
        lago = (lsib.intersection(caja, 200).difference(g_local, 200)
                .buffer(-2500, 200).buffer(2800, 200))
        g2 = g.union(lago, 200).simplify(error_m)
        info = g2.getInfo()
    except Exception:
        info = g.simplify(error_m).getInfo()
    if 'coordinates' in info:
        info['coordinates'] = _redondear(info['coordinates'])
    feat = {'type': 'Feature', 'properties': {}, 'geometry': info}
    return json.dumps({'type': 'FeatureCollection', 'features': [feat]},
                      separators=(',', ':'))


def limites_geojson(ruta, campos, error_m):
    campos = list(campos)
    fc = ee.FeatureCollection(ruta)

    def preparar(f):
        g = f.geometry().simplify(error_m)
        return ee.Feature(g, {c: f.get(c) for c in campos})

    info = fc.map(preparar).getInfo()
    feats = []
    tabla = []
    for ft in info['features']:
        geom = ft.get('geometry') or {}
        if 'coordinates' in geom:
            geom['coordinates'] = _redondear(geom['coordinates'])
        props = ft.get('properties') or {}
        feats.append({'type': 'Feature', 'properties': props, 'geometry': geom})
        tabla.append(props)
    return {
        'geojson': json.dumps({'type': 'FeatureCollection', 'features': feats},
                              separators=(',', ':')),
        'tabla': json.dumps(tabla, separators=(',', ':')),
    }
)---"

# -----------------------------------------------------------------------------
# Conexión a Earth Engine (diferida: se hace al abrir el visor, no al arrancar)
# -----------------------------------------------------------------------------
gee_listo <- FALSE
CACHE <- new.env()
CACHE_COMP <- new.env()   # composiciones y URLs ya calculadas (compartido entre sesiones)

inicializar_gee <- function() {
  message("Earth Engine: importando librería")
  ee_py  <- reticulate::import("ee")
  correo <- Sys.getenv("GEE_SA_EMAIL")
  json   <- Sys.getenv("GEE_KEY_JSON")
  clave  <- Sys.getenv("GEE_KEY_FILE")
  
  cred <- NULL
  if (nzchar(correo) && nzchar(json)) {
    cred <- ee_py$ServiceAccountCredentials(correo, key_data = json)
  } else if (nzchar(correo) && nzchar(clave)) {
    cred <- ee_py$ServiceAccountCredentials(correo, clave)
  }
  
  if (!is.null(cred)) {
    message("Earth Engine: usando cuenta de servicio")
    ee_py$Initialize(credentials = cred, project = PROYECTO_GCP)
  } else {
    message("Earth Engine: usando sesión personal")
    ee_py$Initialize(project = PROYECTO_GCP)
  }
  message("Earth Engine: conexión lista")
  invisible(ee_py)
}

asegurar_gee <- function() {
  if (!gee_listo) {
    inicializar_gee()
    reticulate::py_run_string(sub("__RUTA_AOI__", RUTA_AOI, PY_CODIGO, fixed = TRUE))
    gee_listo <<- TRUE
  }
  invisible(TRUE)
}

# Límites administrativos: se leen una sola vez desde Earth Engine (simplificados)
# Caché en disco: la primera vez se calculan los límites en Earth Engine y se guardan;
# las siguientes veces (y otras sesiones) se leen del archivo y cargan casi al instante.
# Si cambias los assets de límites, borra la carpeta ".cache_visor".
# La carpeta "cache_visor" (SIN punto) viaja con el despliegue: se genera una vez en tu PC
# y se sube junto con la app, así el primer visitante NO espera a Earth Engine.
# Si cambias los assets de límites, borra la carpeta "cache_visor" y vuelve a generarla.
DIR_CACHE <- file.path(getwd(), "cache_visor")

leer_cache <- function(nombre) {
  for (d in c(DIR_CACHE, file.path(getwd(), ".cache_visor"), tempdir())) {
    f <- file.path(d, nombre)
    if (file.exists(f)) {
      r <- tryCatch(readRDS(f), error = function(e) NULL)
      if (!is.null(r)) { t_log(paste("Caché leída:", f)); return(r) }
    }
  }
  t_log(paste("SIN caché de", nombre, "(se calculará en Earth Engine)"))
  NULL
}
guardar_cache <- function(obj, nombre) {
  for (d in c(DIR_CACHE, tempdir())) {
    ok <- tryCatch({
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
      saveRDS(obj, file.path(d, nombre))
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) return(invisible(TRUE))
  }
  invisible(FALSE)
}

# Contorno nacional: se calcula aparte (después de mostrar los límites) y se guarda en caché
cargar_pais <- function() {
  if (!is.null(CACHE$pais)) return(CACHE$pais)
  p <- leer_cache("pais_v2.rds")
  if (is.null(p)) {
    message("Calculando contorno del Perú")
    p <- tryCatch(
      as.character(reticulate::py$contorno_pais(paste0(RUTA_ASSETS, "DEP_PERU"), 1000)),
      error = function(e) {
        message("No se pudo armar el contorno del Perú: ", conditionMessage(e))
        NULL
      })
    if (!is.null(p)) guardar_cache(p, "pais_v2.rds")
  }
  CACHE$pais <- p
  p
}

cargar_limites <- function() {
  if (!is.null(CACHE$lim)) return(CACHE$lim)
  en_disco <- leer_cache("limites_v2.rds")
  if (!is.null(en_disco)) {
    message("Límites administrativos: leídos de la caché")
    CACHE$lim <- en_disco
    return(en_disco)
  }
  leer <- function(nombre, campos, error_m) {
    r <- reticulate::py$limites_geojson(paste0(RUTA_ASSETS, nombre),
                                        as.list(campos), error_m)
    tabla <- NULL
    if (length(campos) > 0) {
      tabla <- jsonlite::fromJSON(r$tabla)
      tabla[] <- lapply(tabla, as.character)
    }
    list(geojson = r$geojson, tabla = tabla)
  }
  message("Cargando límites administrativos")
  lim <- list(
    dep  = leer("DEP_DZ3",  c("IDDPTO", "DEPARTAMEN"), 300),
    prov = leer("PROV_DZ3", c("IDPROV", "PROVINCIA", "IDDPTO", "DEPARTAMEN"), 100),
    dist = leer("DIST_DZ3", c("IDDIST", "DISTRITO", "IDPROV", "PROVINCIA",
                              "IDDPTO", "DEPARTAMEN"), 50),
    per  = leer("DEP_PERU", character(0), 1500)
  )
  guardar_cache(lim, "limites_v2.rds")
  CACHE$lim <- lim
  lim
}

# -----------------------------------------------------------------------------
# Estilos y JavaScript del mapa
# -----------------------------------------------------------------------------
CSS_CODIGO <- r"---(
/* ---- Tamaño según el monitor (única regla que cambia con la pantalla) ----
   Todos los tamaños de este CSS están en rem (1rem = tamaño de letra de la raíz).
   Hasta 1600 px de ancho la raíz mide 16 px (todo igual que antes); más allá crece con el ancho
   de la ventana (1vw), con un tope de 25.6 px (x1.6). Para agrandar más: baja el 16px o sube el 1vw. */
:root {font-size: clamp(16px, 1vw, 25.6px);}
body {font-size: .875rem;}   /* 14 px a escala normal (el texto heredado también escala) */

html, body {margin:0; padding:0; height:100%; overflow:hidden;
  font-family:"Segoe UI", system-ui, -apple-system, Roboto, Arial, sans-serif;}
.container-fluid {padding:0;}
.app-raiz {display:flex; flex-direction:column; height:100vh;}
.cabecera {height:4rem; flex:0 0 auto; display:flex; align-items:center; gap:1.125rem;
  padding:0 1.25rem; background:#fff; border-bottom:1px solid #ddd;}
.cabecera h3 {margin:0; color:#0b5394; font-size:1.25rem; font-weight:500;}

/* ---- Ventana de filtros: píldora blanca flotante, SIEMPRE en una sola línea ---- */
.filtros {position:absolute; top:0.625rem; left:0.625rem; z-index:1250;
  width:max-content; max-width:calc(100% - 10.9375rem); box-sizing:border-box;
  display:flex; flex-wrap:nowrap; align-items:flex-end; gap:0.5rem;
  padding:0.5625rem 1.625rem 0.6875rem 0.875rem; background:#fff; border-radius:1.625rem;
  box-shadow:0 2px 0.625rem rgba(0,0,0,.28);
  overflow:hidden;}
.f-grupo {display:flex; flex-direction:column; flex:0 0 auto;}
.f-lab {font-size:0.625rem; letter-spacing:.6px; color:#777; margin-bottom:0.1875rem;
  text-transform:uppercase; white-space:nowrap;}
.filtros .form-group {margin:0;}
/* quita el ancho fijo de 300 px que Shiny pone a los campos */
.filtros .shiny-input-container {width:auto !important; min-width:0; margin:0;}
/* letra de las listas: 0.625rem (legible para los agricultores, pero compacta) */
.filtros select {height:2rem; width:8.125rem; padding:0.25rem 1.5rem 0.25rem 0.5rem;
  border:1px solid #c9d4df; border-radius:0.5rem; font-size:0.625rem; font-weight:500; color:#222;
  -webkit-appearance:none; -moz-appearance:none; appearance:none;
  background-color:#fff; background-repeat:no-repeat;
  background-position:right 0.5rem center; background-size:0.75rem 0.75rem;
  background-image:url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'><path d='M6 9l6 6 6-6' fill='none' stroke='%23222222' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round'/></svg>");}
.filtros select:focus {outline:none; border-color:#0b5394;}
.filtros select:disabled {background-color:#f1f1f1; color:#aaa; border-color:#ddd;}
/* anchos fijos desde el inicio: un poco más anchos (+10 px) para que quepan los nombres largos */
.filtros #sel_dep {width:10.125rem;}
.filtros #sel_prov {width:8.5rem;}
.filtros #sel_dist {width:11.625rem;}
.btn-reset {height:2rem; padding:0 0.6875rem; border:1px solid #c9d4df; border-radius:0.5rem;
  background:#fff; font-size:0.75rem; font-weight:700; color:#222; white-space:nowrap; flex:0 0 auto;}
.btn-reset:hover {background:#eef4fb; border-color:#0b5394; color:#0b5394;}
.f-sep {width:1px; align-self:stretch; background:#dde3ea; margin:0 0.25rem; flex:0 0 auto;}
/* periodo: las dos fechas juntas, con el guion en medio */
.filtros .input-daterange {display:flex !important; align-items:center; width:auto !important;
  flex-wrap:nowrap; margin:0;}
.filtros .input-daterange .form-control {float:none; flex:0 0 5.625rem; width:5.625rem !important;
  height:2rem; font-size:0.75rem; text-align:center; color:#555; padding:0.25rem 0.375rem;
  border:1px solid #c9d4df; border-radius:0.5rem !important; box-shadow:none; background:#fff;}
.filtros .input-daterange .form-control:focus {border-color:#0b5394; box-shadow:none;}
.filtros .input-daterange .input-group-addon,
.filtros .input-daterange .input-group-text {display:flex; align-items:center; width:auto;
  height:2rem; border:0; background:transparent; padding:0 0.25rem; color:#555; font-weight:700;}
/* el calendario del periodo debe verse por encima del mapa */
.datepicker, .datepicker-dropdown {z-index:2000 !important;}

.contenedor {flex:1 1 auto; display:flex; min-height:0;}
.panel-lateral {width:17.5rem; min-width:17.5rem; background:#1f2a35; color:#cfd8e0;
  overflow-y:auto;}
.panel-lateral .etiqueta-temas {padding:1rem 1.125rem; font-size:0.75rem; letter-spacing:1px;
  color:#8fa3b5;}
.panel-lateral details {border-top:1px solid #2c3a47;}
.panel-lateral summary {padding:0.875rem 1.125rem; cursor:pointer; font-weight:600;
  font-size:0.8125rem; letter-spacing:.5px; list-style:none;}
.panel-lateral summary::-webkit-details-marker {display:none;}
.panel-lateral summary:hover {background:#2c3a47;}
.panel-lateral details[open] > summary {background:#2a7fd4; color:#fff;}
.panel-lateral .contenido {padding:0.75rem 1.125rem 1.125rem 1.125rem;}
.panel-lateral label {color:#cfd8e0; font-weight:normal;}
.panel-lateral .irs--shiny .irs-bar {background:#2a7fd4; border-color:#2a7fd4;}
.ley-tit {font-weight:700; font-size:0.75rem; margin:2px 0 0.5rem 0; color:#e8eef3;}
.ley-barra {height:0.75rem; border-radius:2px; border:1px solid #556;}
.ley-marcas {display:flex; justify-content:space-between; font-size:0.625rem;
  color:#9fb1c1; margin:2px 0 0.625rem 0;}
.ley-fila {display:flex; align-items:center; gap:0.5rem; font-size:0.75rem; margin:0.25rem 0;}
.ley-sw {width:1.875rem; height:0.875rem; border:1px solid #333; flex:0 0 auto;}
.nota-info {font-size:0.75rem; color:#9fb1c1; margin-top:0.375rem; line-height:1.5;}
.texto-info {font-size:0.75rem; color:#9fb1c1; line-height:1.5;}

.zona-mapa {flex:1; display:flex; flex-direction:column; min-width:0;}
.ruta {flex:0 0 auto; padding:0.4375rem 1rem; background:#fff; border-bottom:1px solid #ddd;
  font-size:0.75rem; letter-spacing:.4px; text-transform:uppercase; color:#555;}
.ruta-link {color:#555; text-decoration:none; border-bottom:1px solid #bbb;}
.ruta-link:hover {color:#0b5394; border-color:#0b5394;}
.ruta-sep {margin:0 0.5rem; color:#aaa;}
.ruta-actual {font-weight:700; color:#111;}
.ruta-raiz {text-transform:none;}
.mapa-wrap {flex:1 1 auto; position:relative; min-height:0; background:#cfe3ee;}
#mapa {position:absolute !important; top:0; left:0; right:0; bottom:0;
  width:100% !important; height:100% !important;}
.ctrl-derecha {position:absolute; top:3.875rem; right:0.625rem; z-index:1000; display:flex;
  flex-direction:column; align-items:flex-end; gap:0.5rem;}
.btn-consulta {display:flex; align-items:center; gap:0.375rem; height:2.125rem; padding:0 0.875rem;
  border:0; border-radius:1.125rem; background:#fff; color:#222; font-size:0.8125rem;
  font-weight:600; box-shadow:0 1px 0.3125rem rgba(0,0,0,.35); cursor:pointer;}
.btn-consulta.activo {background:#d32f2f; color:#fff;}
.btn-icono {width:2.125rem; height:2.125rem; border:0; border-radius:50%; background:#fff;
  color:#222; box-shadow:0 1px 0.3125rem rgba(0,0,0,.35); display:flex;
  align-items:center; justify-content:center; cursor:pointer;}
.btn-icono:hover {background:#eef4fb;}
.spinner-mapa {display:none; position:absolute; top:0.75rem; left:50%;
  transform:translateX(-50%); z-index:1000; background:#fff; padding:0.375rem 0.875rem;
  border-radius:1rem; font-size:0.75rem; box-shadow:0 1px 0.3125rem rgba(0,0,0,.35);}
.shiny-busy .spinner-mapa, .mapa-wrap.cargando .spinner-mapa {display:block;}
.mapa-wrap.modo-consulta .leaflet-container,
.mapa-wrap.modo-consulta .leaflet-interactive {cursor:crosshair !important;}
.leaflet-tooltip.etq-unidad {background:transparent; border:0; box-shadow:none;
  padding:0; font-size:0.6875rem; font-weight:700; color:#1b1b1b; text-transform:uppercase;
  text-shadow:0 0 0.1875rem #fff, 0 0 0.1875rem #fff, 0 0 0.25rem #fff; white-space:nowrap;}
.leaflet-tooltip.etq-unidad:before {display:none;}
.v-pop {min-width:14.375rem; font-size:0.75rem;}
.v-fila {padding:0.3125rem 0; border-bottom:1px solid #eee;}
.v-cab {display:flex; justify-content:space-between; font-size:0.8125rem;}
.v-cls {margin-top:0.1875rem; color:#444;}
.sw {display:inline-block; width:0.875rem; height:0.875rem; border:1px solid #333;
  margin-right:0.375rem; vertical-align:middle;}
.v-coord {margin-top:0.375rem; color:#777; font-size:0.6875rem;}
.diag {position:absolute; left:0.625rem; bottom:1.5rem; z-index:1000; background:rgba(255,255,255,.88);
  font-size:0.6875rem; padding:0.1875rem 0.5rem; border-radius:0.25rem; color:#333; pointer-events:none;}

/* ---- Ajustes de diseño: colores SENAMHI ---- */
:root {--az:#0b5394; --az-osc:#083b6b; --am:#fdc50f; --cel:#3b9be6;}

.marca {padding:1.25rem 1.125rem 1rem; text-align:center; border-bottom:1px solid #2c3a47;}
.marca-tit {color:var(--cel); font-size:1rem; font-weight:700; letter-spacing:1.2px;}
.marca-sub {color:#fff; font-size:0.8125rem; font-weight:600; margin-top:0.375rem;}
.marca-det {color:#9fb1c1; font-size:0.6875rem; margin-top:0.1875rem; line-height:1.4;}

.ruta {min-height:1.125rem; color:var(--az);}
.ruta-link {color:var(--az); border-bottom:1px solid var(--az);}
.ruta-link:hover {color:var(--az-osc); border-color:var(--az-osc);}
.ruta-sep {color:var(--az);}
.ruta-actual {color:var(--az); font-weight:700;}

.ctrl-derecha {top:3.5rem;}
.btn-consulta {transition:background .15s, color .15s;}
.btn-consulta:hover {background:var(--az); color:#fff;}
.btn-consulta.activo, .btn-consulta.activo:hover {background:var(--am); color:var(--az-osc);}
.btn-icono {color:var(--az);}
.btn-icono:hover {background:var(--az); color:#fff;}

.leaflet-bar {border:0 !important; border-radius:0.5rem !important;
  box-shadow:0 1px 0.3125rem rgba(0,0,0,.35) !important; overflow:hidden;}
.leaflet-bar a, .leaflet-touch .leaflet-bar a {width:1.875rem !important; height:1.875rem !important;
  line-height:1.875rem !important; font-size:1rem !important; text-indent:0 !important;
  color:var(--az) !important; background-color:#fff !important;
  font-weight:700; border-bottom-color:#e3e8ee !important;}
.leaflet-bar a:hover, .leaflet-bar a:focus {background-color:var(--az) !important;
  color:#fff !important;}
.leaflet-bar a.leaflet-disabled {color:#9db3c9 !important; background-color:#f4f6f8 !important;}
.leaflet-control-layers {border:0 !important; border-radius:0.5rem !important;
  box-shadow:0 1px 0.3125rem rgba(0,0,0,.35) !important;}
.leaflet-control-layers-toggle, .leaflet-touch .leaflet-control-layers-toggle {
  width:2.125rem !important; height:2.125rem !important; background-size:1.25rem 1.25rem !important;
  background-position:center !important; border-radius:0.5rem;
  background-image:url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%230b5394' stroke-width='2' stroke-linejoin='round' stroke-linecap='round'><path d='M12 2 2 7l10 5 10-5-10-5z'/><path d='m2 12 10 5 10-5'/><path d='m2 17 10 5 10-5'/></svg>") !important;}
.leaflet-control-layers-expanded {padding:0.5rem 0.75rem; color:#222;}
.escala-ctrl {width:max-content; box-sizing:border-box; background:rgba(255,255,255,.72);
  -webkit-backdrop-filter:blur(0.1875rem); backdrop-filter:blur(0.1875rem); border-radius:0.5rem;
  padding:0.375rem 0.75rem 0.4375rem; box-shadow:0 1px 0.25rem rgba(0,0,0,.25); font-size:0.65625rem;
  font-weight:600; color:var(--az-osc); margin:0 0 0.625rem 0.625rem; line-height:1;}
.escala-lab {position:relative; height:0.875rem;}
.escala-lab span {position:absolute; top:0; white-space:nowrap; line-height:0.875rem;}
.escala-barra {display:flex; height:0.3125rem; border:1px solid var(--az-osc); box-sizing:content-box;}
.escala-barra i {display:block; height:100%; background:var(--az-osc);}
.escala-barra i:nth-child(1) {flex:25;}
.escala-barra i:nth-child(2) {flex:25; background:#fff;}
.escala-barra i:nth-child(3) {flex:50;}

.minimapa-wrap {position:absolute; right:0.625rem; bottom:1.875rem; z-index:1000; width:8.25rem;
  height:10.5rem; background:#fff; border:2px solid #3d3530; border-radius:0.625rem;
  box-shadow:0 1px 0.3125rem rgba(0,0,0,.35); overflow:hidden;}
#minimapa {position:absolute; top:0; left:0; right:0; bottom:0; background:#eaf2f8;}
#minimapa.leaflet-container {cursor:default !important;}
.norte {position:absolute; top:0.25rem; right:0.3125rem; z-index:1001; display:flex;
  flex-direction:column; align-items:center; color:var(--az); font-size:0.625rem;
  font-weight:700; line-height:1; pointer-events:none;}
/* ---- Selector de mapa base (Mapa | Satélite | Relieve) ---- */
.selector-base {display:flex; background:#fff; border-radius:0.5rem; overflow:hidden;
  box-shadow:0 1px 0.3125rem rgba(0,0,0,.35);}
.selector-base button {border:0; background:#fff; color:var(--az); font-weight:700;
  font-size:0.75rem; height:2.125rem; padding:0 0.8125rem; cursor:pointer;}
.selector-base button + button {border-left:1px solid #e3e8ee;}
.selector-base button:hover {background:#eef4fb;}
.selector-base button.activo, .selector-base button.activo:hover {background:var(--az); color:#fff;}
/* ---- Controles del mapa siempre por encima (la lista de capas queda sobre los botones) ---- */
.leaflet-top, .leaflet-bottom {z-index:1200 !important;}

/* ---- Cuadro de valores (tarjetas, fondo celeste) ---- */
.popup-valor .leaflet-popup-content-wrapper {background:var(--cel); border-radius:1rem;
  padding:0.75rem; box-shadow:0 0.25rem 0.875rem rgba(0,0,0,.3);}
.popup-valor .leaflet-popup-content {margin:0;}
.popup-valor .leaflet-popup-tip {background:var(--cel);}
.vc {color:#1f2a35; display:flex; flex-direction:column; gap:0.5rem;}
.vc-caja {background:#fff; border-radius:0.75rem; overflow:hidden;}
.vc-head {display:flex; align-items:center; gap:0.375rem; padding:0.5rem 0.625rem; font-size:0.6875rem;
  color:#4a5d70;}
.vc-toggle {display:flex; align-items:center; justify-content:space-between; width:100%;
  border:0; border-top:1px solid #e3ebf3; background:#f6f9fc; color:var(--az);
  font-size:0.71875rem; font-weight:700; padding:0.4375rem 0.625rem; cursor:pointer;}
.vc-toggle:hover {background:#eaf2fa;}
.vc-toggle svg {transition:transform .15s;}
.vc-coord.abierto svg {transform:rotate(180deg);}
.vc-head svg {color:var(--az); display:block;}
.vc-head b {color:#1f2a35; margin-right:0.1875rem;}
.vc-head > span {white-space:nowrap;}
.vc-sep {width:1px; height:1rem; background:#c9d4df; display:inline-block;}
.vc-btns {margin-left:auto; display:flex; gap:2px;}
.vc-head button {border:0; background:none; color:var(--az); padding:0.1875rem; cursor:pointer;
  display:flex; border-radius:0.375rem;}
.vc-head button:hover, .vc-head button.abierto {background:var(--az); color:#fff;}
.vc-head button:hover svg, .vc-head button.abierto svg {color:#fff;}
.vc-aviso {background:#fff; border-radius:0.75rem; padding:0.625rem 0.75rem; font-size:0.75rem;
  color:#4a5d70;}
.vc-card {border-radius:0.75rem; padding:0.5rem; background:var(--vc-bg);
  border:1px solid var(--vc-borde);}
.vc-top {display:flex; align-items:center; gap:0.5rem;}
.vc-ico {width:2.125rem; height:2.125rem; border-radius:50%; background:var(--vc); color:#fff;
  display:flex; align-items:center; justify-content:center; flex:0 0 auto;}
.vc-tit {display:flex; flex-direction:column; flex:1; line-height:1.15; min-width:0;}
.vc-tit b {font-size:1rem; font-weight:800; color:var(--vc-titulo);}
.vc-tit span {font-size:0.6875rem; color:#4a6b8a;}
.vc-val {background:var(--vc-pill); color:var(--vc-texto); font-weight:700; font-size:0.75rem;
  padding:2px 0.625rem; border-radius:0.625rem; align-self:flex-start;}
.vc-pill {display:flex; align-items:center; gap:0.4375rem; margin-top:0.4375rem; background:var(--vc-pill);
  color:var(--vc-texto); border-radius:0.625rem; padding:0.375rem 0.625rem; font-size:0.78125rem;
  font-weight:800;}
.vc-pill svg {flex:0 0 auto;}
.t-verde {--vc:#3a9d4b; --vc-bg:#eaf6ec; --vc-borde:#cfe8d3; --vc-pill:#d6edda;
  --vc-texto:#1d5a2b; --vc-titulo:#1d5a2b;}
.t-naranja {--vc:#e8832a; --vc-bg:#fdefe3; --vc-borde:#f6d8c0; --vc-pill:#fadcc7;
  --vc-texto:#6b2f08; --vc-titulo:#c2610f;}
.t-azul {--vc:#2f80c8; --vc-bg:#e6f1fb; --vc-borde:#c6dcf2; --vc-pill:#d0e4f7;
  --vc-texto:#143f6b; --vc-titulo:#174f86;}
.t-rojo {--vc:#c0392b; --vc-bg:#fbe9e7; --vc-borde:#f2cbc6; --vc-pill:#f6d3cf;
  --vc-texto:#6e1a12; --vc-titulo:#a32a1e;}
.t-gris {--vc:#8795a3; --vc-bg:#f1f4f7; --vc-borde:#dbe2e8; --vc-pill:#e3e9ee;
  --vc-texto:#44525f; --vc-titulo:#44525f;}

/* ---- "Ir a coordenadas" (desplegable dentro del cuadro de valores) ---- */
.pc-form {border-top:1px solid #e3ebf3; padding:0.625rem;}
.pc-tabs {display:flex; background:#e8f1fa; border-radius:0.5rem; padding:2px; margin-bottom:0.5rem;}
.pc-tabs button {flex:1; border:0; background:transparent; border-radius:0.375rem; padding:0.3125rem 0;
  font-size:0.6875rem; font-weight:600; color:var(--az); cursor:pointer;}
.pc-tabs button.activo {background:var(--az); color:#fff;}
.pc-fila {display:flex; gap:0.5rem; margin-bottom:0.375rem;}
.pc-campo {display:flex; flex-direction:column; flex:1; min-width:0;}
.pc-campo label {font-size:0.5625rem; letter-spacing:.5px; text-transform:uppercase; color:#778;
  margin:0 0 2px 0; font-weight:600;}
.pc-campo input, .pc-campo select {height:1.75rem; border:1px solid #bbb; border-radius:0.375rem;
  padding:0 0.375rem; font-size:0.75rem; width:100%; box-sizing:border-box; background:#fff; color:#222;}
.pc-ayuda {font-size:0.625rem; color:#667; margin:0 0 0.5rem; line-height:1.35;}
.pc-ir {width:100%; height:1.875rem; border:0; border-radius:0.5rem; background:var(--az); color:#fff;
  font-weight:700; font-size:0.75rem; cursor:pointer; transition:background .15s, color .15s;}
.pc-ir:hover {background:var(--am); color:var(--az-osc);}
.pc-msg {margin-top:0.375rem; font-size:0.65625rem; line-height:1.35; color:#b3261e;}
.pc-msg:empty {display:none;}
.pc-msg.ok {color:#1d5a2b;}

/* ---- Dibujar terreno y resultados ---- */
.mapa-wrap.modo-dibujo .leaflet-container,
.mapa-wrap.modo-dibujo .leaflet-interactive {cursor:crosshair !important;}
.dibujo-ayuda {display:none; position:absolute; left:50%; bottom:1.125rem; transform:translateX(-50%);
  z-index:1100; flex-direction:column; align-items:center; gap:0.375rem;
  background:rgba(255,255,255,.95); border:2px solid var(--az); border-radius:0.75rem;
  padding:0.375rem 0.5rem; box-shadow:0 2px 0.5rem rgba(0,0,0,.3);}
.dib-btns {display:flex; gap:0.375rem;}
.dib-b {height:1.75rem; padding:0 0.75rem; border:1px solid var(--az); border-radius:0.5rem;
  background:#fff; color:var(--az); font-weight:700; font-size:0.75rem; cursor:pointer;}
.dib-b:hover {background:var(--az); color:#fff;}
.dib-b:focus {outline:none;}
.dib-ok {background:var(--az); color:#fff;}
.dib-ok:hover {background:var(--am); border-color:var(--am); color:var(--az-osc);}
.dib-msg {width:100%; text-align:center; color:#b3261e; font-size:0.6875rem;}
.dib-msg:empty {display:none;}
.panel-result {display:none; flex-direction:column; position:absolute; left:0.625rem; top:5.25rem;
  z-index:1100; width:20rem; box-sizing:border-box; max-height:calc(100% - 6.25rem);
  background:var(--cel); border-radius:1rem; padding:0.75rem; box-shadow:0 0.25rem 0.875rem rgba(0,0,0,.3);
  cursor:grab; user-select:none; -webkit-user-select:none;}
.panel-result .pr-cab {flex:0 0 auto;}
.panel-result #pr-cuerpo {flex:1 1 auto; min-height:0; overflow-y:auto;}
.pr-cab {color:#fff; font-weight:800; font-size:0.875rem; margin-bottom:0.5rem;}
.pr-borrar {width:100%; margin-top:0.5rem; height:1.875rem; border:0; border-radius:0.5rem; background:#fff;
  color:var(--az); font-weight:700; font-size:0.75rem; cursor:pointer;}
.pr-borrar:hover {background:var(--az); color:#fff;}
.rs-total {background:#fff; border-radius:0.75rem; padding:0.5rem 0.75rem;}
.rs-tl {font-size:0.8125rem; font-weight:600; color:#4a5d70;}
.rs-n {font-size:1.375rem; font-weight:800; color:var(--az); line-height:1.15;}
.rs-s {font-size:0.65625rem; color:#667; line-height:1.2;}
.rs-tit {display:flex; flex-direction:column; font-weight:800; font-size:0.9375rem;
  color:var(--vc-titulo); margin-bottom:0.5rem; line-height:1.15;}
.rs-tit span {font-size:0.6875rem; font-weight:400; color:#4a6b8a;}
.rs-fila {display:grid; grid-template-columns:0.6875rem 1fr 4.5rem 2.5rem; column-gap:0.375rem;
  align-items:center; font-size:0.71875rem; color:var(--vc-texto); margin-bottom:0.4375rem;}
.rs-fila:last-child {margin-bottom:0;}
.rs-cab {font-size:0.75rem; font-weight:800; text-transform:uppercase; letter-spacing:.4px;
  color:var(--vc-titulo); padding-bottom:0.3125rem; margin-bottom:0.375rem;
  border-bottom:1px solid rgba(0,0,0,.12);}
.rs-h1 {grid-column:1 / 3;}
.rs-h2, .rs-h3 {text-align:center;}
.rs-u {text-transform:none;}
.rs-sw {width:0.6875rem; height:0.6875rem; border:1px solid #333; border-radius:2px;}
.rs-et {min-width:0; line-height:1.2;}
.rs-ar {font-weight:700; text-align:center; white-space:nowrap; line-height:1.2;}
.rs-m2 {display:block; font-size:0.59375rem; font-weight:400; color:#667; line-height:1.15;}
.rs-pc {font-weight:800; text-align:center; white-space:nowrap;}
.rs-barra {grid-column:1 / -1; height:0.25rem; background:rgba(0,0,0,.08); border-radius:0.1875rem;
  margin-top:-1px; overflow:hidden;}
.rs-barra i {display:block; height:100%;}
/* los valores del cuadro de resultados se pueden seleccionar y copiar */
.panel-result .vc-card, .panel-result .rs-total, .panel-result .rs-parcela {user-select:text; -webkit-user-select:text; cursor:text;}
/* resultados por parcela */
.rs-sec-tit {color:#fff; font-weight:800; font-size:0.8125rem; margin:0.25rem 2px 0;}
.rs-parcela {background:#fff; border-radius:0.75rem; overflow:hidden;}
.rs-parcela > summary {display:flex; align-items:center; justify-content:space-between; gap:0.5rem;
  padding:0.5rem 0.75rem; cursor:pointer; list-style:none; font-size:0.78125rem; font-weight:700; color:var(--az);}
.rs-parcela > summary::-webkit-details-marker {display:none;}
.rs-parcela > summary::after {content:''; width:0.4375rem; height:0.4375rem; border-right:2px solid var(--az);
  border-bottom:2px solid var(--az); transform:rotate(45deg); margin:-0.1875rem 2px 0 0.375rem; flex:0 0 auto;
  transition:transform .15s;}
.rs-parcela[open] > summary::after {transform:rotate(-135deg); margin-top:0.1875rem;}
.rs-parcela > summary:hover {background:#f2f7fc;}
.rs-pn {flex:1; min-width:0; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;}
.rs-pa {white-space:nowrap; color:#1f2a35; text-align:right; line-height:1.2;}
.rs-pa small {display:block; font-size:0.59375rem; font-weight:400; color:#667;}
.rs-pcuerpo {display:flex; flex-direction:column; gap:0.5rem; padding:0 0.5rem 0.5rem; background:#fff;}
.leaflet-tooltip.etq-parcela {background:rgba(255,255,255,.88); border:1px solid #b58900;
  color:var(--az-osc); font-weight:700; font-size:0.625rem; padding:1px 0.3125rem; border-radius:0.375rem;
  box-shadow:none;}
.leaflet-tooltip.etq-parcela:before {display:none;}

/* ---- Ventanas que se pueden mover ---- */
.ventana-valores {display:none; position:absolute; left:0; top:0; z-index:1150; width:17.8125rem;
  box-sizing:border-box; border-radius:1rem; padding:0.75rem;
  background:radial-gradient(120% 90% at 15% 0%, rgba(255,255,255,.22) 0%, rgba(255,255,255,0) 55%),
             linear-gradient(155deg, #4ca43a 0%, #36a04e 40%, #229a86 70%, #168fb2 100%);
  box-shadow:0 0.25rem 0.875rem rgba(0,0,0,.3); cursor:grab; user-select:none;
  -webkit-user-select:none;}
.ventana-valores input, .ventana-coord input {user-select:text; -webkit-user-select:text; cursor:text;}
.ventana-coord {display:none; position:absolute; left:0; top:0; z-index:1150; width:16.875rem;
  box-sizing:border-box; background:var(--cel); border-radius:1rem; padding:0.75rem;
  box-shadow:0 0.25rem 0.875rem rgba(0,0,0,.3); cursor:grab; user-select:none;
  -webkit-user-select:none;}
.ventana-coord.moviendo {cursor:grabbing;}
.ventana-coord .pc-form {background:#fff; border-radius:0.75rem; padding:0.625rem; border-top:0;}
/* menú de "Consultar valor" */
.consulta-wrap {position:relative; display:flex; justify-content:flex-end;}
.menu-consulta {display:none; position:absolute; top:calc(100% + 0.375rem); right:0; min-width:12.8125rem;
  background:#fff; border-radius:0.75rem; padding:0.375rem; box-shadow:0 2px 0.625rem rgba(0,0,0,.35);}
.menu-consulta .mi {display:flex; align-items:center; gap:0.5rem; width:100%; border:0;
  background:transparent; border-radius:0.5rem; padding:0.5rem 0.625rem; font-size:0.8125rem; font-weight:600;
  color:var(--az); cursor:pointer; text-align:left;}
.menu-consulta .mi svg {flex:0 0 auto;}
.menu-consulta .mi:hover {background:var(--az); color:#fff;}
.menu-consulta .mi.activo, .menu-consulta .mi.activo:hover {background:var(--am); color:var(--az-osc);}
.ventana-valores.moviendo, .panel-result.moviendo {cursor:grabbing;}
.asa {position:absolute; top:0.1875rem; left:50%; transform:translateX(-50%); width:2.75rem; height:0.3125rem;
  border-radius:0.1875rem; background:rgba(255,255,255,.85); touch-action:none; cursor:grab;}
.vv-x {position:absolute; top:-0.5625rem; right:-0.5625rem; width:1.5rem; height:1.5rem; border:0;
  border-radius:50%; background:#fff; color:var(--az); font-size:1.125rem; line-height:1.375rem;
  padding:0; cursor:pointer; box-shadow:0 1px 0.25rem rgba(0,0,0,.35);}
.vv-x:hover {background:var(--az); color:#fff;}
.vv-cuerpo {overflow-y:auto; cursor:default;}
.vv-cuerpo .vc-card, .vv-cuerpo .vc-head {cursor:grab;}
@media (max-width: 820px) {
  .panel-result {width:calc(100% - 1.25rem); max-height:45%;}
  .dibujo-ayuda {max-width:calc(100% - 1.25rem);}
}

/* ---- Fondo tipo tarjeta del clima en todas las ventanas de "Consultar valor" ---- */
:root {--fondo-clima: radial-gradient(120% 90% at 15% 0%, rgba(255,255,255,.22) 0%, rgba(255,255,255,0) 55%),
  linear-gradient(160deg, #4ca43a 0%, #39a14c 32%, #24a08a 64%, #0d74b0 100%);}
.ventana-valores, .ventana-coord, .panel-result, .dibujo-ayuda {background:var(--fondo-clima);}
.dibujo-ayuda {border-color:rgba(255,255,255,.6);}
/* las subventanas se ven un poco transparentes, con su color original */
.vc-caja, .vc-aviso, .rs-total, .rs-parcela, .ventana-coord .pc-form {
  background:rgba(255,255,255,.88); -webkit-backdrop-filter:blur(0.25rem); backdrop-filter:blur(0.25rem);}
.vc-card {background:color-mix(in srgb, var(--vc-bg) 88%, transparent);
  -webkit-backdrop-filter:blur(0.25rem); backdrop-filter:blur(0.25rem);}
.rs-pcuerpo {background:transparent;}

/* ---- Importar área de interés ---- */
.zona-soltar {display:none; position:absolute; top:0; left:0; right:0; bottom:0; z-index:1300;
  pointer-events:none; align-items:center; justify-content:center;
  background:rgba(11,83,148,.18); border:0.1875rem dashed var(--az);}
.zona-soltar div {background:#fff; color:var(--az); font-weight:700; font-size:0.9375rem;
  padding:0.875rem 1.375rem; border-radius:0.75rem; box-shadow:0 2px 0.625rem rgba(0,0,0,.3);}
.aviso-mapa {display:none; position:absolute; top:5.75rem; left:50%; transform:translateX(-50%);
  z-index:1250; max-width:min(35rem, calc(100% - 2.5rem)); background:#fff;
  border:2px solid var(--az); border-radius:0.625rem; padding:0.5rem 0.875rem; font-size:0.78125rem;
  color:var(--az-osc); box-shadow:0 2px 0.5rem rgba(0,0,0,.3); text-align:center;}
.aviso-mapa.error {border-color:#b3261e; color:#b3261e;}
/* evita las líneas finas entre mosaicos al acercar con zoom fraccionado */
.leaflet-tile-pane .leaflet-tile {width:16.0625rem !important; height:16.0625rem !important;}
@media (max-width: 820px) {.minimapa-wrap {display:none;}}

/* ---- Ajustes por la ventana de filtros flotante: el zoom y el spinner bajan un poco ---- */
.leaflet-top.leaflet-left {margin-top:7.375rem;}
/* pantallas medianas: filtros un poco más compactos para que nunca se salgan */
@media (max-width: 1150px) {
  .filtros {gap:0.375rem; padding:0.5rem 1.375rem 0.625rem 0.6875rem;}
  .filtros select {font-size:0.5625rem; padding:0.25rem 1.25rem 0.25rem 0.375rem;}
  .filtros #sel_dep {width:8.5rem;}
  .filtros #sel_prov {width:7.25rem;}
  .filtros #sel_dist {width:9.5rem;}
  .btn-reset {font-size:0.6875rem; padding:0 0.5rem;}
  .filtros .input-daterange .form-control {flex:0 0 4.875rem; width:4.875rem !important; font-size:0.6875rem;}
  .f-lab {font-size:0.5625rem;}
}
.spinner-mapa {top:5.5rem;}

@media (max-width: 820px) {
  .contenedor {flex-direction:column;}
  .panel-lateral {width:100%; min-width:0; max-height:34vh;}
  .filtros {max-width:calc(100% - 1.25rem); padding:0.5rem 0.875rem 0.625rem; border-radius:1.25rem;}
  .leaflet-top.leaflet-right {margin-top:4.75rem;}
  .ctrl-derecha {top:8.25rem;}
  .cabecera h3 {font-size:0.9375rem;}
}

/* ================= MÓVIL (celular) ================= */
.app-raiz {height:100dvh;}
.btn-menu, .ley-movil, .velo {display:none;}
@media (max-width: 820px) {
  .cabecera {height:3.5rem; padding:0 0.75rem; justify-content:space-between; gap:0.625rem; position:relative; z-index:2100;}
  .cabecera img {height:2.25rem !important; max-width:calc(100% - 3.75rem); object-fit:contain;}
  .btn-menu {display:flex; flex-direction:column; justify-content:center; gap:0.3125rem; width:2.625rem; height:2.625rem;
    padding:0 0.5625rem; border:0; border-radius:0.625rem; background:#0b5394; flex:0 0 auto;}
  .btn-menu span {display:block; height:0.1875rem; border-radius:2px; background:#fff;}

  /* panel lateral = menú que se desliza sobre el mapa */
  .panel-lateral {position:fixed; top:3.5rem; left:0; bottom:0; width:84%; max-width:20.625rem; max-height:none;
    z-index:2050; transform:translateX(-102%); transition:transform .22s ease; box-shadow:0.25rem 0 1rem rgba(0,0,0,.35);}
  body.menu-abierto .panel-lateral {transform:none;}
  body.menu-abierto .velo {display:block; position:fixed; top:3.5rem; left:0; right:0; bottom:0;
    z-index:2040; background:rgba(0,0,0,.45);}
  .panel-lateral summary {padding:1rem 1.125rem; font-size:0.875rem;}

  /* filtros en dos filas: Dep | Prov | Dist  y  Restablecer | Periodo */
  .filtros {display:grid; grid-template-columns:1fr 1fr 1fr; gap:0.375rem; left:0.5rem; right:0.5rem; top:0.5rem;
    width:auto; max-width:none; padding:0.5rem 0.625rem 0.625rem; border-radius:1rem; overflow:visible;}
  .filtros .f-grupo {min-width:0;}
  .filtros .shiny-input-container {width:100% !important;}
  .filtros select, .filtros #sel_dep, .filtros #sel_prov, .filtros #sel_dist {
    width:100%; height:2.25rem; font-size:0.75rem; padding:0.25rem 1.375rem 0.25rem 0.5rem; text-overflow:ellipsis;}
  .f-lab {font-size:0.5625rem;}
  .f-sep {display:none;}
  .btn-reset {grid-column:1; align-self:end; height:2.25rem; font-size:0.6875rem; padding:0 0.25rem;
    white-space:normal; line-height:1.1;}
  .filtros .f-grupo:last-child {grid-column:2 / 4;}
  .filtros .input-daterange {width:100% !important;}
  .filtros .input-daterange .form-control {flex:1 1 0; width:auto !important; min-width:0;
    height:2.25rem; font-size:0.75rem; padding:0.25rem 2px;}

  /* controles del mapa: se acomodan bajo los filtros (altura medida por JS) */
  .leaflet-top.leaflet-left {margin-top:0;}
  .leaflet-control-zoom {display:none;}
  .leaflet-top.leaflet-right {margin-top:calc(var(--fh,7.375rem) + 0.625rem);}
  .ctrl-derecha {top:calc(var(--fh,7.375rem) + 3.875rem); right:0.625rem;}
  .selector-base button {height:2.25rem; font-size:0.8125rem;}
  .btn-consulta {height:2.5rem; font-size:0.875rem;}
  .btn-icono {width:2.5rem; height:2.5rem;}
  .spinner-mapa {top:calc(var(--fh,7.375rem) + 1.125rem);}
  .aviso-mapa {top:calc(var(--fh,7.375rem) + 1.25rem);}

  /* ventanas: resultados como hoja inferior */
  .panel-result {left:0.5rem; right:0.5rem; top:auto; bottom:0.5rem; width:auto; max-height:52%;}
  .ventana-valores {width:min(17.8125rem, calc(100% - 1rem));}
  .ventana-coord {width:min(16.875rem, calc(100% - 1rem));}
  .dibujo-ayuda {bottom:3.25rem;}

  /* leyenda plegable sobre el mapa */
  .ley-movil {display:block; position:absolute; left:0.5rem; bottom:2.875rem; z-index:1100; max-width:12.5rem;
    background:rgba(255,255,255,.93); border-radius:0.75rem; box-shadow:0 1px 0.375rem rgba(0,0,0,.35);
    font-size:0.6875rem; color:#1f2a35;}
  .ley-movil summary {padding:0.4375rem 0.75rem; font-weight:700; color:#0b5394; cursor:pointer;
    list-style:none; font-size:0.75rem;}
  .ley-movil summary::-webkit-details-marker {display:none;}
  .ley-movil .contenido-ley {padding:0 0.75rem 0.625rem;}
  .ley-movil .ley-tit {color:#083b6b; font-size:0.6875rem;}
  .ley-movil .ley-marcas {color:#556;}
  .ley-movil .ley-fila {font-size:0.6875rem; margin:0.1875rem 0;}
}

/* =====================================================================
   AJUSTES V2 (celular y calendario). Van al final para tener prioridad.
   ===================================================================== */

/* ---- Sin texto de créditos abajo (Leaflet · Google · Sentinel · Landsat) ---- */
.leaflet-control-attribution {display:none !important;}

/* ---- Etiquetas del mapa en azul oscuro ---- */
.leaflet-tooltip.etq-unidad {color:#083b6b;}

/* ---- Calendario del periodo: encabezado normal (sin sombreado de color) ---- */
.datepicker.datepicker-dropdown {padding:0 !important; border:0 !important; border-radius:1.125rem !important;
  overflow:hidden; background:#fff; box-shadow:0 0.625rem 1.75rem rgba(8,59,107,.35) !important;
  font-family:inherit; margin-top:0.375rem;}
.datepicker-dropdown:before, .datepicker-dropdown:after {display:none !important;}
.datepicker > div {background:#fff;}
.datepicker table {width:100%; margin:0; border-collapse:separate; border-spacing:0; padding:0 0.5rem 0.5rem;
  box-sizing:border-box;}
.datepicker table thead tr:first-child th {height:2.75rem; background:transparent !important; color:var(--az-osc);
  font-size:0.8125rem; font-weight:700; border-radius:0 !important; text-shadow:none; cursor:pointer;}
.datepicker table thead tr:first-child th:hover {background:#e3f0fb !important;}
.datepicker table thead tr:first-child th.datepicker-switch {letter-spacing:.3px;}
.datepicker table thead tr:last-child th.dow, .datepicker th.dow {color:var(--az); font-size:0.625rem;
  font-weight:800; text-transform:uppercase; padding:0.5rem 0 0.25rem; letter-spacing:.4px;}
.datepicker table tr td {text-align:center; font-size:0.78125rem; color:#1f2a35;}
.datepicker table tr td.day {width:14.28%; height:2rem; line-height:2rem; padding:0; border-radius:50% !important;
  transition:background .12s, color .12s;}
.datepicker table tr td.day:hover, .datepicker table tr td.focused {background:#e3f0fb !important;
  color:var(--az-osc);}
.datepicker table tr td.old, .datepicker table tr td.new {color:#b3c0cd;}
.datepicker table tr td.today, .datepicker table tr td.today:hover {background:transparent !important;
  box-shadow:inset 0 0 0 2px var(--cel); color:var(--az) !important; font-weight:700;}
.datepicker table tr td.range, .datepicker table tr td.range:hover {background:#e6f1fb !important;
  color:var(--az-osc); border-radius:0 !important;}
.datepicker table tr td.active, .datepicker table tr td.active:hover,
.datepicker table tr td.active.day, .datepicker table tr td.selected,
.datepicker table tr td span.active, .datepicker table tr td span.active:hover {
  background:var(--am) !important; background-image:none !important; color:var(--az-osc) !important;
  font-weight:800; text-shadow:none; box-shadow:0 2px 0.375rem rgba(253,197,15,.55) !important;
  border-radius:50% !important;}
.datepicker table tr td.disabled, .datepicker table tr td.disabled:hover {background:transparent !important;
  color:#cdd6df !important; box-shadow:none !important;}
.datepicker table tr td span.month, .datepicker table tr td span.year {height:2.25rem; line-height:2.25rem;
  margin:0.1875rem 0; border-radius:0.75rem !important; font-size:0.78125rem;}
.datepicker table tr td span.month:hover, .datepicker table tr td span.year:hover {background:#e3f0fb !important;}
.datepicker table tr td span.focused {background:#e3f0fb !important;}

/* ---- Contorno de Perú (PC y celular): sombra sutil solo hacia afuera (canvas) ---- */
.sombra-pais-canvas {position:absolute; left:0; top:0; z-index:401; pointer-events:none;}

/* ---- Menú de listas (en lugar del selector nativo) ---- */
.menu-sel {position:fixed; z-index:3000; display:none; background:rgba(44,52,60,.68);
  -webkit-backdrop-filter:blur(0.875rem); backdrop-filter:blur(0.875rem); border-radius:0.875rem; padding:0.25rem 0;
  box-shadow:0 0.5rem 1.5rem rgba(0,0,0,.4); overflow-y:auto; -webkit-overflow-scrolling:touch;
  border:1px solid rgba(255,255,255,.12);}
.menu-sel div {position:relative; padding:0.5rem 0.875rem 0.5rem 1.875rem; font-size:0.75rem; color:#fff;
  white-space:nowrap; cursor:pointer;}
.menu-sel div.sel:before {content:'\2713'; position:absolute; left:0.6875rem; top:0.5rem; font-size:0.75rem;}
.menu-sel div:active {background:rgba(255,255,255,.16);}
.menu-sel div {break-inside:avoid;}

/* ---- CELULAR: iconos y letras más pequeños, ventanas del tamaño de la leyenda ---- */
@media (max-width: 820px) {
  .cabecera {height:3rem; padding:0 0.625rem;}
  .cabecera img {height:1.875rem !important; max-width:calc(100% - 3.25rem);}
  /* botón del menú: tres líneas finas, sin fondo azul */
  .btn-menu {background:transparent !important; width:2.375rem; height:2.375rem; padding:0 0.3125rem; gap:0.375rem;
    border-radius:0.5rem;}
  .btn-menu span {height:2px; border-radius:2px; background:#2b2f33;}

  /* filtros */
  .filtros {top:0.375rem; left:0.375rem; right:0.375rem; gap:0.3125rem; padding:0.375rem 0.5rem 0.5rem; border-radius:0.875rem;}
  .filtros select, .filtros #sel_dep, .filtros #sel_prov, .filtros #sel_dist {
    height:1.875rem; font-size:0.6875rem; padding:0.1875rem 1.25rem 0.1875rem 0.4375rem; border-radius:0.4375rem; background-size:0.625rem 0.625rem;}
  .btn-reset {height:1.875rem; font-size:0.625rem; border-radius:0.4375rem;}
  .filtros .input-daterange .form-control {height:1.875rem; font-size:0.6875rem; border-radius:0.4375rem !important;}
  .f-lab {font-size:0.5rem; margin-bottom:2px;}

  /* controles del mapa */
  .selector-base button {height:1.875rem; font-size:0.6875rem; padding:0 0.6875rem;}
  .btn-consulta {height:2rem; font-size:0.75rem; padding:0 0.75rem; gap:0.3125rem;}
  .btn-consulta svg {width:0.875rem; height:0.875rem;}
  .btn-icono {width:2rem; height:2rem;}
  .btn-icono svg {width:1rem; height:1rem;}
  .ctrl-derecha {top:calc(var(--fh,6.25rem) + 3.25rem); gap:0.375rem;}
  .menu-consulta {min-width:11.25rem;}
  .menu-consulta .mi {font-size:0.71875rem; padding:0.4375rem 0.5625rem;}
  .aviso-mapa {font-size:0.6875rem; padding:0.375rem 0.625rem;}
  .spinner-mapa {font-size:0.6875rem; padding:0.25rem 0.625rem;}
  .leaflet-tooltip.etq-unidad {font-size:0.5625rem;}
  /* escala: el recuadro se ajusta exactamente a la barra (nada sale del fondo, sin espacios vacíos) */
  .escala-ctrl {width:max-content; max-width:none; font-size:0.59375rem; padding:0.25rem 0.5625rem 0.3125rem; margin:0 0 0.5rem 0.5rem;}

  /* leyenda */
  .ley-movil {max-width:10.625rem; bottom:2.75rem; border-radius:0.625rem;}
  .ley-movil summary {padding:0.3125rem 0.625rem; font-size:0.65625rem;}
  .ley-movil .contenido-ley {padding:0 0.625rem 0.5rem;}
  .ley-movil .ley-tit {font-size:0.625rem; margin-bottom:0.3125rem;}
  .ley-movil .ley-barra {height:0.5625rem;}
  .ley-movil .ley-marcas {font-size:0.5625rem; margin-bottom:0.375rem;}
  .ley-movil .ley-fila {font-size:0.59375rem; margin:2px 0; gap:0.375rem;}
  .ley-movil .ley-sw {width:1.375rem; height:0.625rem;}

  /* menú lateral: más fino, un poco separado del borde y translúcido */
  .panel-lateral {top:3.375rem; left:0.5rem; bottom:0.5rem; width:66%; max-width:14.75rem; border-radius:1rem;
    background:rgba(24,34,45,.78); -webkit-backdrop-filter:blur(0.625rem); backdrop-filter:blur(0.625rem);
    border:1px solid rgba(255,255,255,.14); transform:translateX(-120%);
    box-shadow:0 0.5rem 1.5rem rgba(0,0,0,.35);}
  body.menu-abierto .panel-lateral {transform:none;}
  body.menu-abierto .velo {top:3rem; background:rgba(0,0,0,.28);}
  .panel-lateral .marca {padding:0.75rem 0.75rem 0.625rem;}
  .marca-tit {font-size:0.78125rem; letter-spacing:1px;}
  .marca-sub {font-size:0.65625rem; margin-top:0.25rem;}
  .marca-det {font-size:0.5625rem;}
  .panel-lateral .etiqueta-temas {padding:0.5rem 0.875rem; font-size:0.5625rem;}
  .panel-lateral details {border-top:1px solid rgba(255,255,255,.1);}
  .panel-lateral summary {padding:0.5rem 0.875rem; font-size:0.65625rem; font-weight:600; letter-spacing:.6px;}
  .panel-lateral details[open] > summary {background:rgba(42,127,212,.7);}
  .panel-lateral .contenido {padding:0.5rem 0.875rem 0.75rem;}
  .panel-lateral label, .panel-lateral .control-label {font-size:0.65625rem;}
  .panel-lateral .radio label {font-size:0.65625rem;}
  .panel-lateral .nota-info {font-size:0.59375rem;}
  .panel-lateral .ley-tit {font-size:0.625rem;}
  .panel-lateral .ley-fila {font-size:0.625rem; margin:0.1875rem 0;}
  .panel-lateral .ley-sw {width:1.5rem; height:0.6875rem;}
  .panel-lateral .ley-barra {height:0.5625rem;}
  .panel-lateral .ley-marcas {font-size:0.5625rem;}

  /* ventanas de "Consultar valor": del tamaño de la leyenda */
  .ventana-valores {width:min(11.25rem, calc(100% - 1rem)); padding:0.5rem; border-radius:0.75rem;}
  .vv-x {width:1.25rem; height:1.25rem; font-size:0.9375rem; line-height:1.125rem; top:-0.4375rem; right:-0.4375rem;}
  .asa {width:2.125rem; height:0.25rem;}
  .vc {gap:0.3125rem;}
  .vc-caja {border-radius:0.5625rem;}
  .vc-head {padding:0.3125rem 0.4375rem; font-size:0.5625rem; gap:0.25rem;}
  .vc-head svg {width:0.75rem; height:0.75rem;}
  .vc-head b {margin-right:2px;}
  .vc-sep {height:0.75rem;}
  .vc-card {padding:0.375rem; border-radius:0.5625rem;}
  .vc-top {gap:0.375rem;}
  .vc-ico {width:1.5rem; height:1.5rem;}
  .vc-ico svg {width:0.875rem; height:0.875rem;}
  .vc-tit b {font-size:0.75rem;}
  .vc-tit span {font-size:0.53125rem;}
  .vc-val {font-size:0.625rem; padding:1px 0.375rem; border-radius:0.5rem;}
  .vc-pill {font-size:0.59375rem; padding:0.25rem 0.4375rem; margin-top:0.3125rem; gap:0.3125rem; border-radius:0.5rem;}
  .vc-pill svg {width:0.6875rem; height:0.6875rem;}
  .vc-aviso {font-size:0.625rem; padding:0.4375rem 0.5625rem; border-radius:0.5625rem;}

  .ventana-coord {width:min(12.1875rem, calc(100% - 1rem)); padding:0.5rem; border-radius:0.75rem;}
  .ventana-coord .pc-form {padding:0.4375rem; border-radius:0.5625rem;}
  .pr-cab {font-size:0.71875rem; margin-bottom:0.375rem;}
  .pc-tabs button {font-size:0.625rem; padding:0.25rem 0;}
  .pc-campo label {font-size:0.5rem;}
  .pc-campo input, .pc-campo select {height:1.625rem; font-size:0.6875rem;}
  .pc-ayuda {font-size:0.53125rem; margin-bottom:0.375rem;}
  .pc-ir {height:1.6875rem; font-size:0.6875rem; border-radius:0.4375rem;}
  .pc-msg {font-size:0.59375rem;}

  .panel-result {left:0.5rem; right:auto; top:auto; bottom:0.5rem; width:min(14.6875rem, calc(100% - 1rem));
    max-height:50%; padding:0.5rem; border-radius:0.75rem;}
  .rs-total {padding:0.375rem 0.5625rem; border-radius:0.5625rem;}
  .rs-tl {font-size:0.65625rem;}
  .rs-n {font-size:1.0625rem;}
  .rs-s {font-size:0.5625rem;}
  .rs-tit {font-size:0.75rem; margin-bottom:0.3125rem;}
  .rs-tit span {font-size:0.5625rem;}
  .rs-fila {grid-template-columns:0.5625rem 1fr 3.375rem 1.875rem; column-gap:0.25rem; font-size:0.625rem; margin-bottom:0.3125rem;}
  .rs-cab {font-size:0.59375rem; padding-bottom:0.25rem; margin-bottom:0.25rem;}
  .rs-m2 {font-size:0.53125rem;}
  .rs-sw {width:0.5625rem; height:0.5625rem;}
  .rs-sec-tit {font-size:0.6875rem;}
  .rs-parcela {border-radius:0.5625rem;}
  .rs-parcela > summary {padding:0.375rem 0.5625rem; font-size:0.65625rem;}
  .rs-pa small {font-size:0.53125rem;}
  .dibujo-ayuda {bottom:2.875rem; padding:0.3125rem 0.375rem; border-radius:0.625rem;}
  .dib-b {height:1.625rem; padding:0 0.5625rem; font-size:0.6875rem; border-radius:0.4375rem;}
  .dib-msg {font-size:0.625rem;}

  /* calendario más compacto */
  .datepicker table tr td {font-size:0.71875rem;}
  .datepicker table tr td.day {height:1.875rem; line-height:1.875rem;}
  .datepicker table thead tr:first-child th {height:2.5rem; font-size:0.75rem;}
}

/* =====================================================================
   AJUSTES V3: pastilla y leyenda translúcidas, menú azul fino,
   menú lateral flotante también en PC
   ===================================================================== */
.filtros {background:linear-gradient(135deg, rgba(255,255,255,.80), rgba(255,255,255,.55));
  -webkit-backdrop-filter:blur(0.875rem) saturate(1.3); backdrop-filter:blur(0.875rem) saturate(1.3);
  border:1px solid rgba(255,255,255,.7); box-shadow:0 0.375rem 1.375rem rgba(8,59,107,.25);}
.filtros select {background-color:rgba(255,255,255,.72); border-color:rgba(11,83,148,.22);}
.filtros select:disabled {background-color:rgba(236,240,244,.55); color:#9aa7b4;}
.btn-reset {background:rgba(255,255,255,.72); border-color:rgba(11,83,148,.22);}
.filtros .input-daterange .form-control {background:rgba(255,255,255,.72); border-color:rgba(11,83,148,.22);}
.f-lab {color:#4f6378;}

/* PC: el menú lateral flota sobre el mapa, translúcido y redondeado */
@media (min-width: 821px) {
  .contenedor {position:relative;}
  .panel-lateral {position:absolute; top:0.625rem; left:0.625rem; bottom:0.625rem; width:15.625rem; min-width:0;
    z-index:1300; border-radius:1.125rem; background:rgba(24,34,45,.78);
    -webkit-backdrop-filter:blur(0.625rem); backdrop-filter:blur(0.625rem);
    border:1px solid rgba(255,255,255,.14); box-shadow:0 0.5rem 1.625rem rgba(0,0,0,.35);}
  .panel-lateral .marca {padding:1rem 0.875rem 0.75rem;}
  .marca-tit {font-size:0.875rem;}
  .marca-sub {font-size:0.75rem; margin-top:0.3125rem;}
  .marca-det {font-size:0.625rem;}
  .panel-lateral .etiqueta-temas {padding:0.625rem 1rem; font-size:0.625rem;}
  .panel-lateral details {border-top:1px solid rgba(255,255,255,.1);}
  .panel-lateral summary {padding:0.625rem 1rem; font-size:0.71875rem; letter-spacing:.7px;}
  .panel-lateral summary:hover {background:rgba(255,255,255,.08);}
  .panel-lateral details[open] > summary {background:rgba(42,127,212,.72);}
  .panel-lateral .contenido {padding:0.625rem 1rem 0.875rem;}
  .panel-lateral label, .panel-lateral .radio label {font-size:0.75rem;}
  .panel-lateral .nota-info {font-size:0.6875rem;}
  .panel-lateral .ley-tit {font-size:0.71875rem;}
  .panel-lateral .ley-fila {font-size:0.6875rem;}
  .panel-lateral .ley-sw {width:1.625rem; height:0.75rem;}
  .filtros {left:16.875rem; max-width:calc(100% - 16.875rem - 10.9375rem);}
  .leaflet-top.leaflet-left, .leaflet-bottom.leaflet-left {margin-left:16.375rem;}
  .panel-result {left:16.875rem;}
}
@media (min-width: 821px) and (max-width: 1100px) {
  .panel-lateral {width:13.125rem;}
  .filtros {left:14.375rem; max-width:calc(100% - 14.375rem - 10rem);}
  .leaflet-top.leaflet-left, .leaflet-bottom.leaflet-left {margin-left:13.875rem;}
  .panel-result {left:14.375rem;}
}

@media (max-width: 820px) {
  /* menú ☰: más pequeño y azul */
  .btn-menu {width:1.875rem; height:1.875rem; padding:0 0.1875rem; gap:0.3125rem; margin-right:0.25rem;}
  .btn-menu span {height:2px; background:#0b5394;}
  /* "Consultar valor" separado de Mapa | Satélite */
  .ctrl-derecha {top:calc(var(--fh,6.25rem) + 4.125rem);}
  /* leyenda translúcida (sigue blanca) */
  .ley-movil {background:rgba(255,255,255,.6); -webkit-backdrop-filter:blur(0.75rem);
    backdrop-filter:blur(0.75rem); border:1px solid rgba(255,255,255,.7);}
}

/* =====================================================================
   AJUSTES V4: menú lateral compacto y más oscuro (PC), pastilla azul oscura (PC),
   menú "Consultar valor" y ventanas de parámetros translúcidos
   ===================================================================== */
@media (min-width: 821px) {
  .panel-lateral {background:rgba(12,20,29,.84);}
  .panel-lateral .marca {padding:0.625rem 0.75rem 0.5rem;}
  .marca-tit {font-size:0.8125rem;}
  .marca-sub {font-size:0.6875rem; margin-top:0.1875rem;}
  .marca-det {font-size:0.59375rem; margin-top:2px;}
  .panel-lateral .etiqueta-temas {padding:0.3125rem 0.875rem; font-size:0.5625rem;}
  .panel-lateral summary {padding:0.4375rem 0.875rem; font-size:0.6875rem;}
  .panel-lateral .contenido {padding:0.375rem 0.875rem 0.5625rem;}
  .panel-lateral .form-group, .panel-lateral .shiny-input-container {margin-bottom:0.1875rem;}
  .panel-lateral .control-label {margin-bottom:2px; font-size:0.6875rem;}
  .panel-lateral .radio {margin:2px 0;}
  .panel-lateral .radio label {font-size:0.71875rem;}
  .panel-lateral .nota-info {margin-top:0.1875rem; font-size:0.65625rem;}
  .panel-lateral .ley-tit {margin:0 0 0.3125rem; font-size:0.6875rem;}
  .panel-lateral .ley-barra {height:0.5625rem;}
  .panel-lateral .ley-marcas {margin:1px 0 0.375rem; font-size:0.5625rem;}
  .panel-lateral .ley-fila {margin:2px 0; font-size:0.65625rem;}
  .panel-lateral .ley-sw {height:0.625rem;}
  .panel-lateral .irs-grid {display:none;}
  .panel-lateral .irs {height:2.375rem;}

  /* pastilla de filtros: azul oscuro sutil y translúcido */
  .filtros {background:linear-gradient(135deg, rgba(9,38,72,.66), rgba(18,68,118,.50));
    border:1px solid rgba(255,255,255,.22); box-shadow:0 0.375rem 1.375rem rgba(4,20,40,.38);}
  .filtros .f-lab {color:#d6e6f5;}
  .filtros select {background-color:rgba(255,255,255,.9); border-color:rgba(255,255,255,.35);}
  .filtros select:disabled {background-color:rgba(255,255,255,.45); color:#6b7a88;}
  .btn-reset {background:rgba(255,255,255,.9); border-color:rgba(255,255,255,.35);}
  .filtros .input-daterange .form-control {background:rgba(255,255,255,.9); border-color:rgba(255,255,255,.35);}
  .filtros .input-daterange .input-group-addon,
  .filtros .input-daterange .input-group-text {color:#fff;}
  .f-sep {background:rgba(255,255,255,.28);}
}

/* menú de Consultar valor: igual a la leyenda del celular (blanco translúcido) */
.menu-consulta {background:rgba(255,255,255,.6); -webkit-backdrop-filter:blur(0.75rem);
  backdrop-filter:blur(0.75rem); border:1px solid rgba(255,255,255,.7);}

/* ventanas de parámetros: mismo color, ahora translúcidas */
:root {--fondo-clima: radial-gradient(120% 90% at 15% 0%, rgba(255,255,255,.18) 0%, rgba(255,255,255,0) 55%),
  linear-gradient(160deg, rgba(76,164,58,.74) 0%, rgba(57,161,76,.72) 32%, rgba(36,160,138,.72) 64%, rgba(13,116,176,.74) 100%);}
.ventana-valores, .ventana-coord, .panel-result, .dibujo-ayuda {
  -webkit-backdrop-filter:blur(0.625rem); backdrop-filter:blur(0.625rem);
  border:1px solid rgba(255,255,255,.35);}

/* =====================================================================
   AJUSTES V5 (solo PC): pastilla blanca menos transparente; menú lateral pegado
   a los bordes (sin esquinas redondeadas), letras de nuevo más grandes y repartidas
   ===================================================================== */
@media (min-width: 821px) {
  /* pastilla de filtros: vuelve al blanco, un poco menos transparente */
  .filtros {background:linear-gradient(135deg, rgba(255,255,255,.93), rgba(255,255,255,.78));
    border:1px solid rgba(255,255,255,.85); box-shadow:0 0.375rem 1.375rem rgba(8,59,107,.25);}
  .filtros .f-lab {color:#4f6378;}
  .filtros select {background-color:rgba(255,255,255,.85); border-color:rgba(11,83,148,.22);}
  .filtros select:disabled {background-color:rgba(236,240,244,.6); color:#9aa7b4;}
  .btn-reset {background:rgba(255,255,255,.85); border-color:rgba(11,83,148,.22);}
  .filtros .input-daterange .form-control {background:rgba(255,255,255,.85); border-color:rgba(11,83,148,.22);}
  .filtros .input-daterange .input-group-addon,
  .filtros .input-daterange .input-group-text {color:#555;}
  .f-sep {background:#dde3ea;}

  /* menú lateral: pegado arriba, abajo e izquierda, sin bordes redondeados */
  .panel-lateral {top:0; left:0; bottom:0; width:15.625rem; border-radius:0; border:0;
    border-right:1px solid rgba(255,255,255,.12); box-shadow:0.25rem 0 1rem rgba(0,0,0,.3);
    background:rgba(12,20,29,.86);}
  .panel-lateral .marca {padding:1rem 0.875rem 0.75rem;}
  .marca-tit {font-size:0.875rem;}
  .marca-sub {font-size:0.75rem; margin-top:0.3125rem;}
  .marca-det {font-size:0.625rem; margin-top:0.1875rem;}
  .panel-lateral .etiqueta-temas {padding:0.625rem 1rem; font-size:0.625rem;}
  .panel-lateral summary {padding:clamp(0.5625rem, 1.5vh, 1rem) 1rem; font-size:0.71875rem;}
  .panel-lateral .contenido {padding:clamp(0.5rem, 1.6vh, 1.125rem) 1rem clamp(0.625rem, 2vh, 1.375rem);}
  .panel-lateral .form-group, .panel-lateral .shiny-input-container {margin-bottom:0.375rem;}
  .panel-lateral .control-label {margin-bottom:0.3125rem; font-size:0.75rem;}
  .panel-lateral .radio {margin:clamp(0.3125rem, 1vh, 0.625rem) 0;}
  .panel-lateral .radio label {font-size:0.75rem;}
  .panel-lateral .nota-info {margin-top:0.375rem; font-size:0.6875rem;}
  .panel-lateral .ley-tit {margin:0 0 0.5rem; font-size:0.71875rem;}
  .panel-lateral .ley-barra {height:0.6875rem;}
  .panel-lateral .ley-marcas {margin:2px 0 clamp(0.375rem, 1.2vh, 0.75rem); font-size:0.59375rem;}
  .panel-lateral .ley-fila {margin:clamp(0.1875rem, .6vh, 0.375rem) 0; font-size:0.6875rem;}
  .panel-lateral .ley-sw {width:1.625rem; height:0.75rem;}
  .panel-lateral .irs-grid {display:block;}
  .panel-lateral .irs {height:3.125rem;}

  .filtros {left:16.25rem; max-width:calc(100% - 16.25rem - 10.9375rem);}
  .leaflet-top.leaflet-left, .leaflet-bottom.leaflet-left {margin-left:15.75rem;}
  .panel-result {left:16.25rem;}

  /* listas desplegables de Departamento / Provincia / Distrito (solo PC):
     menú oscuro con check, con la misma transparencia que la pastilla de filtros */
  .menu-sel {overflow:visible; background:rgba(44,52,60,.85); -webkit-backdrop-filter:blur(0.875rem) saturate(1.3);
    backdrop-filter:blur(0.875rem) saturate(1.3); border:1px solid rgba(255,255,255,.18);
    box-shadow:0 0.5rem 1.5rem rgba(0,0,0,.35);}
  .menu-sel div:hover {background:rgba(255,255,255,.16);}
}
@media (min-width: 821px) and (max-width: 1100px) {
  .panel-lateral {width:13.4375rem;}
  .filtros {left:14.0625rem; max-width:calc(100% - 14.0625rem - 10rem);}
  .leaflet-top.leaflet-left, .leaflet-bottom.leaflet-left {margin-left:13.5625rem;}
  .panel-result {left:14.0625rem;}
}
.ventana-valores, .ventana-coord, .panel-result, .asa,
.ventana-valores .vc-card, .ventana-valores .vc-caja, .ventana-valores .vc-head,
.ventana-valores .vv-cuerpo {
  touch-action: none;
}
.ventana-valores .vc-card, .ventana-valores .vc-head {
  user-select: text; -webkit-user-select: text; cursor: text;
}
/* ---- Todas las ventanas: asa visible y cursor de redimensionar en el borde superior ---- */
.ventana-valores .asa, .ventana-coord .asa, .panel-result .asa {
  box-sizing: content-box; width: 2.75rem; height: 0.3125rem; padding: 0.5rem 0.875rem;
  background-clip: content-box; top: -2px; cursor: ns-resize; z-index: 5;
  transition: background-color .15s;
}
.ventana-valores:hover .asa, .ventana-coord:hover .asa, .panel-result:hover .asa {
  background-color: #fff;
}
.ventana-valores .asa:hover, .ventana-coord .asa:hover, .panel-result .asa:hover {
  background-color: #fdc50f;
}

/* Minimapa más grande en pantalla completa (PC) */
@media (min-width: 821px) {
  #mapa-wrap:fullscreen .minimapa-wrap {width:11rem; height:14rem;}
  #mapa-wrap:-webkit-full-screen .minimapa-wrap {width:11rem; height:14rem;}
}
/* Celular: "pantalla completa" propia (sin el aviso del navegador): el mapa cubre toda la ventana */
body.pseudo-completa #mapa-wrap {position:fixed !important; top:0; left:0; right:0; bottom:0;
  width:100% !important; height:100vh !important; height:100dvh !important; z-index:99999;}
/* ---- Pantalla completa en PC: filtros, zoom (+/-) y escala van a la izquierda (solo PC; el celular no cambia) ---- */
@media (min-width: 821px) {
  #mapa-wrap:fullscreen .leaflet-top.leaflet-left,
  #mapa-wrap:fullscreen .leaflet-bottom.leaflet-left {margin-left:0 !important;}
  #mapa-wrap:-webkit-full-screen .leaflet-top.leaflet-left,
  #mapa-wrap:-webkit-full-screen .leaflet-bottom.leaflet-left {margin-left:0 !important;}
  #mapa-wrap:fullscreen .filtros {left:0.625rem !important; max-width:calc(100% - 0.625rem - 10.9375rem) !important;}
  #mapa-wrap:-webkit-full-screen .filtros {left:0.625rem !important; max-width:calc(100% - 0.625rem - 10.9375rem) !important;}
  #mapa-wrap:fullscreen .panel-result {left:0.625rem !important;}
  #mapa-wrap:-webkit-full-screen .panel-result {left:0.625rem !important;}
}
/* ---- Escala en PC: recuadro pequeño y barra más larga (el celular conserva su tamaño) ---- */
@media (min-width: 821px) {
  .escala-ctrl {font-size:0.6875rem; padding:0.3125rem 0.75rem 0.375rem;}
  .escala-lab {height:0.875rem;}
  .escala-lab span {line-height:0.875rem;}
  .escala-barra {height:0.3125rem;}
}
)---"

JS_CODIGO <- r"---(
(function () {
  function arrancar() {
  var mapa = null, DATA = null, grupo = null, capaGee = null, marcador = null, limitesAOI = null;
  var ubic = null, PERU = null;
  var estado = { dep: '', prov: '', dist: '' };
  var modoConsulta = false, idClic = 0, dibujando = false;
  var iniciado = false, cola = [];
  var diagEstado = { mapa: 'no', limites: 'no', capa: 'no' };

  function diag() {
    var el = document.getElementById('diag');
    if (!el) { return; }
    el.textContent = 'mapa: ' + diagEstado.mapa + ' | límites: ' + diagEstado.limites +
      ' | capa: ' + diagEstado.capa;
  }

  // Captura el mapa en cuanto Leaflet lo crea (no depende de htmlwidgets)
  function engancharLeaflet(n) {
    n = n || 0;
    if (window.L && L.Map && L.Map.addInitHook) {
      L.Map.addInitHook(function () { window.mapaLeaflet = this; });
      return;
    }
    if (n < 100) { setTimeout(function () { engancharLeaflet(n + 1); }, 100); }
  }
  var ATRIB = '';

      // Líneas en azul (antes negras)
  var CELESTE = '#2a6fd6';   // líneas internas (azul medio)
  var CONTORNO = '#0a2f6b';  // contorno de la selección (azul marino)

  var ESTILO = {
    base:  { color: '#165a9f', weight: 1.3, opacity: 1, fillColor: '#ffffff', fillOpacity: 0.03 },
    hover: { color: '#0b5394', weight: 3, opacity: 1, fillColor: '#0b5394', fillOpacity: 0.22 },
    sel:   { color: '#0b5394', weight: 3.5, opacity: 1, fillColor: '#0b5394', fillOpacity: 0.12 },
    ctx:   { color: '#165a9f', weight: 1.2, opacity: 0.95, fill: false },
    // Líneas internas finas (azul medio) y contorno de la selección más grueso (azul marino)
    gBase:  { color: CELESTE, weight: 1.4, opacity: 1, fillColor: '#ffffff', fillOpacity: 0.03 },
    gHover: { color: CELESTE, weight: 1.8, opacity: 1, fillColor: '#0b5394', fillOpacity: 0.22 },
    gSel:   { color: CONTORNO, weight: 3, opacity: 1, fillColor: '#0b5394', fillOpacity: 0.12 },
    gCtx:   { color: CELESTE, weight: 1.4, opacity: 1, fill: false },
    gBorde: { color: CONTORNO, weight: 3, opacity: 1, fill: false },
    // límites departamentales de todo el Perú: delgados, azul algo más oscuro que el original, sin sombra
    peru:  { color: '#000000', weight: 0.4, opacity: 0.75, fill: false },
    // contorno del Perú: negro; la sombra se dibuja solo hacia afuera
    pais:  { color: '#000000', weight: 1, opacity: 1, fill: false },
    paisSombra: { stroke: false, fill: true, fillColor: '#000000', fillOpacity: 1,
                  className: 'lim-pais-sombra' },
    casing: { color: '#ffffff', weight: 7, opacity: 0.9, fill: false },
    borde: { color: '#0b5394', weight: 3.5, opacity: 1, fill: false }
  };

  // Ya no se usa degradado: se deja la función vacía para no tocar el resto del código
  function asegurarGradiente() { }











  // Sombra sutil solo por fuera del contorno del Perú. Se dibuja en un canvas
  // (funciona igual en PC, Android y iPhone/Safari; los filtros SVG no funcionan en Safari).
  var sombraCanvas = null, sombraRAF = null, pellizcando = false;

  function sombraDibujar() {
    if (!sombraCanvas || !mapa || !DATA || !DATA.pais) { return; }
    var tam = mapa.getSize(), dpr = window.devicePixelRatio || 1;
    var w = Math.round(tam.x * dpr), h = Math.round(tam.y * dpr);
    if (!w || !h) { return; }
    if (sombraCanvas.width !== w) { sombraCanvas.width = w; }
    if (sombraCanvas.height !== h) { sombraCanvas.height = h; }
    sombraCanvas.style.width = tam.x + 'px';
    sombraCanvas.style.height = tam.y + 'px';
    var ctx = sombraCanvas.getContext('2d');
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, w, h);
    ctx.scale(dpr, dpr);
    function trazarPais() {
      DATA.pais.features.forEach(function (f) {
        var g = f.geometry;
        if (!g) { return; }
        var polis = g.type === 'Polygon' ? [g.coordinates] : g.coordinates;
        polis.forEach(function (p) {
          p.forEach(function (anillo) {
            anillo.forEach(function (c, i) {
              var pt = mapa.latLngToContainerPoint([c[1], c[0]]);
              if (i === 0) { ctx.moveTo(pt.x, pt.y); } else { ctx.lineTo(pt.x, pt.y); }
            });
            ctx.closePath();
          });
        });
      });
    }
    // Sombra solo por FUERA del contorno: se recorta todo lo que queda fuera del país
    // y se dibuja el contorno varias veces, cada vez más ancho y muy transparente
    // (degradado negro que se desvanece hacia afuera).
    ctx.save();
    ctx.beginPath();
    ctx.rect(-10, -10, tam.x + 20, tam.y + 20);
    trazarPais();
    ctx.clip('evenodd');
    ctx.beginPath();
    trazarPais();
    ctx.lineJoin = 'round';
    ctx.lineCap = 'round';
    ctx.strokeStyle = 'rgba(0,0,0,0.055)';
    var pasos = 16, paso = 1.8, i2;
    for (i2 = 1; i2 <= pasos; i2++) {
      ctx.lineWidth = 2 * paso * i2;
      ctx.stroke();
    }
    ctx.restore();
  }

  function sombraProgramar() {
    if (sombraRAF || pellizcando) { return; }
    sombraRAF = requestAnimationFrame(function () {
      sombraRAF = null;
      sombraDibujar();
    });
  }

  function sombraCrear() {
    if (sombraCanvas || !mapa) { return; }
    sombraCanvas = document.createElement('canvas');
    sombraCanvas.className = 'sombra-pais-canvas';
    mapa.getContainer().appendChild(sombraCanvas);
    mapa.on('move zoom moveend zoomend viewreset resize', sombraProgramar);
  }

  // Deja solo el contorno exterior del Perú continental: sin huecos, islas ni
  // formas sueltas (por ejemplo las piezas del lago Titicaca)
  function soloContornoPais(gj) {
    var mejor = null, mejorArea = -1;
    (gj.features || []).forEach(function (f) {
      var g = f.geometry;
      if (!g) { return; }
      var polis = g.type === 'Polygon' ? [g.coordinates] : (g.coordinates || []);
      polis.forEach(function (p) {
        if (!p || !p[0]) { return; }
        var a = Math.abs(areaFirmada(p[0]));
        if (a > mejorArea) { mejorArea = a; mejor = p[0]; }
      });
    });
    if (!mejor) { return gj; }
    return { type: 'FeatureCollection', features: [
      { type: 'Feature', properties: {}, geometry: { type: 'Polygon', coordinates: [mejor] } } ] };
  }

  function obtenerMapa() {
    if (window.mapaLeaflet) { return window.mapaLeaflet; }
    var m = $('#mapa').data('leaflet-map');
    if (m) { return m; }
    try {
      var w = HTMLWidgets.find('#mapa');
      if (w && w.getMap) { return w.getMap(); }
    } catch (e) {}
    return null;
  }

  function conMapa(cb, n) {
    n = n || 0;
    var m = obtenerMapa();
    if (m) { cb(m); return; }
    if (n === 130) { diagEstado.mapa = 'NO SE ENCONTRÓ'; diag(); }
    if (n < 300) { setTimeout(function () { conMapa(cb, n + 1); }, 150); }
  }

  function cuandoListo(cb) {
    if (iniciado) { cb(); } else { cola.push(cb); }
  }

  function mostrarCarga() { $('#mapa-wrap').addClass('cargando'); }
  function ocultarCarga() { $('#mapa-wrap').removeClass('cargando'); if (window.splashFin) { window.splashFin(); } }

  function ajustarMinZoom() {
    if (!mapa || !PERU) { return; }
    var tam = mapa.getSize();
    if (!tam.x || !tam.y) { return; }
    mapa.setMinZoom(mapa.getBoundsZoom(PERU, false));
  }

  function ajustarVistaInicial(animar) {
    if (!mapa || !PERU) { return; }
    mapa.invalidateSize();
    var tam = mapa.getSize();
    if (!tam.x || !tam.y) { return; }
    ajustarMinZoom();
    mapa.fitBounds(PERU, { animate: !!animar });
  }

  function limitesDe(geojson, filtro) {
    return L.geoJSON(geojson, { filter: filtro }).getBounds();
  }

  function igual(campo, valor) {
    return function (f) { return String(f.properties[campo]) === String(valor); };
  }
  function distinto(campo, valor) {
    return function (f) { return String(f.properties[campo]) !== String(valor); };
  }

  function agregar(capa) { grupo.addLayer(capa); }

  function contorno(geojson, filtro, estilo) {
    return L.geoJSON(geojson, {
      filter: filtro,
      style: function () { return estilo; },
      interactive: false
    });
  }

  function contornoSel(geojson, filtro) {
    var g = L.layerGroup();
    g.addLayer(contorno(geojson, filtro, ESTILO.gBorde));
    return g;
  }

  // Sin resalte azul de la unidad elegida mientras se consulta, se dibuja,
  // se calcula o hay un terreno delimitado/importado con su ventana de resultados
  function sinResalte() { return modoConsulta || dibujando || !!polyFinal; }

  function selecc(geojson, filtro) {
    return sinResalte() ? contorno(geojson, filtro, ESTILO.gCtx)
                        : contornoSel(geojson, filtro);
  }

  // Control del relleno al pasar el mouse / girar la rueda
  var hoverActual = null, hoverCapa = null, suprimida = null;

  var tRueda = 0, rellenoOculto = false, capaInter = null;

  // Quita el relleno azul de todas las áreas (al girar la rueda o hacer zoom)
  function quitarRelleno() {
    rellenoOculto = true;
    tRueda = Date.now();
    if (hoverActual) { suprimida = hoverActual; }
    hoverActual = null;
    if (capaInter) {
      capaInter.eachLayer(function (l) { capaInter.resetStyle(l); });
    }
  }




// (MAYÚSCULAS, sin tildes).
  var POS_ETQ = [
    { clave: 'SUR DE CAJAMARCA', pos: [-7.073047, -78.641578] },
    { clave: 'LIBERTAD',         pos: [-8.044397, -78.288823] }
  ];

  function normalizarNombre(s) {
    return String(s || '').toUpperCase().trim()
      .normalize('NFD').replace(/[\u0300-\u036f]/g, '');
  }

  function posicionFija(nombre) {
    var n = normalizarNombre(nombre), i;
    for (i = 0; i < POS_ETQ.length; i++) {
      if (n.indexOf(POS_ETQ[i].clave) !== -1) {
        return L.latLng(POS_ETQ[i].pos[0], POS_ETQ[i].pos[1]);
      }
    }
    return null;
  }

  // Punto de la etiqueta: centro de la pieza más grande de cada unidad.
  // Si ese centro cae fuera del polígono, se usa el centro de su tramo horizontal más ancho.
  function puntoEtiqueta(f) {
    var g = f.geometry;
    if (!g) { return null; }
    var polis = g.type === 'Polygon' ? [g.coordinates] : (g.coordinates || []);
    var mejor = null, mejorA = -1;
    polis.forEach(function (p) {
      if (!p || !p[0] || p[0].length < 3) { return; }
      var a = Math.abs(areaFirmada(p[0]));
      if (a > mejorA) { mejorA = a; mejor = p[0]; }
    });
    if (!mejor) { return null; }
    var n = mejor.length, A = 0, cx = 0, cy = 0, i, j, w;
    for (i = 0, j = n - 1; i < n; j = i++) {
      w = mejor[j][0] * mejor[i][1] - mejor[i][0] * mejor[j][1];
      A += w;
      cx += (mejor[j][0] + mejor[i][0]) * w;
      cy += (mejor[j][1] + mejor[i][1]) * w;
    }
    if (A !== 0) {
      cx = cx / (3 * A);
      cy = cy / (3 * A);
      if (puntoEnAnillo([cx, cy], mejor)) { return L.latLng(cy, cx); }
    }
    var minY = Infinity, maxY = -Infinity;
    mejor.forEach(function (c) {
      if (c[1] < minY) { minY = c[1]; }
      if (c[1] > maxY) { maxY = c[1]; }
    });
    var y = (minY + maxY) / 2, xs = [];
    for (i = 0, j = n - 1; i < n; j = i++) {
      var a = mejor[j], b = mejor[i];
      if ((a[1] > y) !== (b[1] > y)) {
        xs.push(a[0] + (y - a[1]) * (b[0] - a[0]) / (b[1] - a[1]));
      }
    }
    xs.sort(function (p, q) { return p - q; });
    var x = (xs.length ? xs[0] + xs[xs.length - 1] : 0) / 2, ancho = -1, k;
    for (k = 0; k + 1 < xs.length; k += 2) {
      if (xs[k + 1] - xs[k] > ancho) { ancho = xs[k + 1] - xs[k]; x = (xs[k] + xs[k + 1]) / 2; }
    }
    return L.latLng(y, x);
  }

    function interactiva(geojson, filtro, nivel, idSel, conGrad) {
    var campoId = { dep: 'IDDPTO', prov: 'IDPROV', dist: 'IDDIST' }[nivel];
    var campoNom = { dep: 'DEPARTAMEN', prov: 'PROVINCIA', dist: 'DISTRITO' }[nivel];
    var eBase = conGrad ? ESTILO.gBase : ESTILO.base;
    var eHover = conGrad ? ESTILO.gHover : ESTILO.hover;
    var eSel = conGrad ? ESTILO.gSel : ESTILO.sel;
    var capa = L.geoJSON(geojson, {
      filter: filtro || function () { return true; },
      style: function (f) {
        var esSel = !sinResalte() && !rellenoOculto && idSel &&
                    String(f.properties[campoId]) === String(idSel);
        return esSel ? eSel : eBase;
      },
      onEachFeature: function (f, lyr) {
        var id = String(f.properties[campoId]);
        var nombre = String(f.properties[campoNom]);

               if (nivel === 'dep') {
          // Departamentos: sin nombre fijo; el nombre aparece al pasar el cursor
          lyr.bindTooltip(nombre, {
            sticky: true, direction: 'top', className: 'etq-unidad'
          });
        } else {
          // Provincias y distritos: etiqueta fija centrada en su polígono
          var pe = puntoEtiqueta(f);
          if (pe) { lyr.getCenter = function () { return pe; }; }

          lyr.bindTooltip(nombre, {
            permanent: true, direction: 'center', className: 'etq-unidad'
          });

          // Posición fija: se fuerza directamente en el tooltip al añadir la capa al mapa
          var fija = posicionFija(nombre);
          if (fija) {
            var colocar = function () {
              var t = lyr.getTooltip();
              if (t) { t.setLatLng(fija); }
            };
            lyr.on('add', colocar);
            lyr.on('tooltipopen', colocar);
          }
        }

        // Relleno al pasar el mouse. Tras girar la rueda o hacer zoom no se vuelve a pintar
        // la misma área; solo se pinta al pasar a otra.
        lyr.on('mouseover', function () {
          if (sinResalte()) { return; }
          if (rellenoOculto && (lyr === suprimida || Date.now() - tRueda < 700)) {
            suprimida = lyr;
            return;
          }
          suprimida = null;
          hoverActual = lyr;
          hoverCapa = capa;
          lyr.setStyle(eHover);
        });
        lyr.on('mouseout', function () {
          if (lyr === suprimida && Date.now() - tRueda >= 700) { suprimida = null; }
          if (hoverActual === lyr) { hoverActual = null; }
          capa.resetStyle(lyr);
        });
        lyr.on('click', function () {
          if (modoConsulta || dibujando) { return; }
          setTimeout(function () { navegarJS(nivel, id); }, 0);
        });
      }
    });
    capaInter = capa;
    return capa;
  }






// Sin etiqueta fija: el nombre aparece al pasar el mouse.
  function navegable(geojson, filtro, nivel, estiloLinea) {
    var campoId = { dep: 'IDDPTO', prov: 'IDPROV', dist: 'IDDIST' }[nivel];
    var campoNom = { dep: 'DEPARTAMEN', prov: 'PROVINCIA', dist: 'DISTRITO' }[nivel];
    var estilo = L.extend({}, estiloLinea, { fill: true, fillColor: '#0b5394', fillOpacity: 0 });
    var capa = L.geoJSON(geojson, {
      filter: filtro,
      style: function () { return estilo; },
      onEachFeature: function (f, lyr) {
        var id = String(f.properties[campoId]);
        lyr.bindTooltip(String(f.properties[campoNom]),
          { sticky: true, direction: 'top', className: 'etq-unidad' });
        lyr.on('mouseover', function () {
          if (sinResalte()) { return; }
          lyr.setStyle({ fillOpacity: 0.12 });
        });
        lyr.on('mouseout', function () { capa.resetStyle(lyr); });
        lyr.on('click', function () {
          if (modoConsulta || dibujando) { return; }
          setTimeout(function () { navegarJS(nivel, id); }, 0);
        });
      }
    });
    return capa;
  }




    function render() {
    if (!mapa || !DATA || !grupo) { return; }
    hoverActual = null; hoverCapa = null; suprimida = null; rellenoOculto = false;
    grupo.clearLayers();
    if (DATA.per) {
      agregar(contorno(DATA.per, null, ESTILO.peru));
    }
    if (DATA.pais) {
      agregar(contorno(DATA.pais, null, ESTILO.pais));
      sombraCrear();
      sombraProgramar();
    }
    var d = estado.dep, p = estado.prov, s = estado.dist;
    if (!d) {
      agregar(interactiva(DATA.dep, null, 'dep', null, false));
    } else if (!p) {
      // Otros departamentos: se puede hacer clic para ir a ellos
      agregar(navegable(DATA.dep, distinto('IDDPTO', d), 'dep', ESTILO.ctx));
      agregar(interactiva(DATA.prov, igual('IDDPTO', d), 'prov', null, true));
      // El contorno va al final para quedar encima de las líneas celestes
      agregar(selecc(DATA.dep, igual('IDDPTO', d)));
    } else {
      // Otros departamentos (debajo) y otras provincias del mismo departamento: con clic
      agregar(navegable(DATA.dep, distinto('IDDPTO', d), 'dep', ESTILO.ctx));
      agregar(navegable(DATA.prov, function (f) {
        return String(f.properties.IDDPTO) === String(d) &&
               String(f.properties.IDPROV) !== String(p);
      }, 'prov', ESTILO.gCtx));
      agregar(interactiva(DATA.dist, igual('IDPROV', p), 'dist', s, true));
      // El contorno va al final para quedar encima de las líneas celestes
      agregar(selecc(DATA.prov, igual('IDPROV', p)));
    }
    asegurarGradiente();
    diagEstado.limites = 'sí (' + (!d ? 'departamentos' : (!p ? 'provincias' : 'distritos')) + ')';
    diag();
  }

  function ajustar() {
    if (!mapa || !DATA) { return; }
    var b = null;
    if (estado.dist) { b = limitesDe(DATA.dist, igual('IDDIST', estado.dist)); }
    else if (estado.prov) { b = limitesDe(DATA.prov, igual('IDPROV', estado.prov)); }
    else if (estado.dep) { b = limitesDe(DATA.dep, igual('IDDPTO', estado.dep)); }
    if (b && b.isValid()) {
      mapa.fitBounds(b, { padding: [30, 30], maxZoom: 14 });
    } else {
      ajustarVistaInicial(true);
    }
  }

  function opcionesSel(id, vacio, items, valor) {
    var s = document.getElementById(id), h = '<option value="">' + vacio + '</option>';
    if (!s) { return; }
    items.forEach(function (it) {
      h += '<option value="' + escAttr(it[0]) + '">' + escHtml(it[1]) + '</option>';
    });
    s.innerHTML = h;
    s.value = valor || '';
  }
  function listaDe(gj, campoId, campoNom, filtro) {
    var vistos = {}, out = [];
    gj.features.forEach(function (f) {
      var p = f.properties, id = String(p[campoId]);
      if ((filtro && !filtro(p)) || vistos[id]) { return; }
      vistos[id] = 1;
      out.push([id, String(p[campoNom])]);
    });
    out.sort(function (a, b) { return a[1].localeCompare(b[1], 'es'); });
    return out;
  }
  function sincronizarListas() {
    if (!DATA || !DATA.dep || !DATA.prov || !DATA.dist) { return; }
    var d = estado.dep, p = estado.prov, s = estado.dist;
    opcionesSel('sel_dep', 'Todos', listaDe(DATA.dep, 'IDDPTO', 'DEPARTAMEN'), d);
    opcionesSel('sel_prov', 'Todas', d ? listaDe(DATA.prov, 'IDPROV', 'PROVINCIA',
      function (q) { return String(q.IDDPTO) === String(d); }) : [], p);
    opcionesSel('sel_dist', 'Todos', p ? listaDe(DATA.dist, 'IDDIST', 'DISTRITO',
      function (q) { return String(q.IDPROV) === String(p); }) : [], s);
    $('#sel_prov').prop('disabled', !d);
    $('#sel_dist').prop('disabled', !p);
  }

  function navegarJS(nivel, id) {
    var f;
    id = String(id);
    if (!DATA) { return; }
    if (nivel === 'dep') {
      estado = { dep: id, prov: '', dist: '' };
    } else if (nivel === 'prov') {
      f = DATA.prov.features.filter(function (x) { return String(x.properties.IDPROV) === id; })[0];
      if (!f) { return; }
      estado = { dep: String(f.properties.IDDPTO), prov: id, dist: '' };
    } else {
      f = DATA.dist.features.filter(function (x) { return String(x.properties.IDDIST) === id; })[0];
      if (!f) { return; }
      estado = { dep: String(f.properties.IDDPTO), prov: String(f.properties.IDPROV), dist: id };
    }
    intentar();
  }

  $(document).on('change', '#sel_dep', function () {
    estado = { dep: this.value, prov: '', dist: '' }; intentar();
  });
  $(document).on('change', '#sel_prov', function () {
    if (this.value) { navegarJS('prov', this.value); }
    else { estado.prov = ''; estado.dist = ''; intentar(); }
  });
  $(document).on('change', '#sel_dist', function () {
    if (this.value) { navegarJS('dist', this.value); }
    else { estado.dist = ''; intentar(); }
  });
  $(document).on('click', '#reset', function () {
    estado = { dep: '', prov: '', dist: '' };
    if (marcador && mapa) { mapa.removeLayer(marcador); marcador = null; }
    intentar();
  });

  function intentar() { sincronizarListas(); render(); ajustar(); construirMini(); actualizarMini(); }

  // ---- Ventana flotante de valores (se puede mover con el mouse o el dedo) ----
  var ventanaMovida = false, ventanaLatLng = null;

  function colocarVentana() {
    var $v = $('#ventana-valores');
    if (!$v.length || $v.css('display') === 'none') { return; }
    var $w = $('#mapa-wrap');
    var W = $w.innerWidth(), H = $w.innerHeight();
    var w = $v.outerWidth(), h = $v.outerHeight();
    var left, top;
    if (ventanaMovida) {
      left = parseFloat($v.css('left')) || 0;
      top = parseFloat($v.css('top')) || 0;
    } else if (ventanaLatLng && mapa) {
      var pt = mapa.latLngToContainerPoint(ventanaLatLng);
      left = pt.x - w / 2;
      top = pt.y - h - 18;
      if (top < 8) { top = pt.y + 18; }
    } else {
      left = 20; top = 20;
    }
    left = Math.max(8, Math.min(left, W - w - 8));
    top = Math.max(8, Math.min(top, H - h - 8));
    $v.css({ left: left + 'px', top: top + 'px' });
  }

  function mostrarVentana(html, latlng) {
    if (latlng) { ventanaLatLng = latlng; }
    $('#vv-cuerpo').html(html);
    $('#ventana-valores').css('display', 'block');
    $('#vv-cuerpo').css('max-height', Math.max(140, $('#mapa-wrap').innerHeight() - 40) + 'px');
    colocarVentana();
  }

  function cerrarVentana() { $('#ventana-valores').css('display', 'none'); }
  window.cerrarVentana = cerrarVentana;

  function actualizarVentana() { colocarVentana(); }
  function reubicarVentana() { if (!ventanaMovida) { colocarVentana(); } }
  $(window).on('resize', function () { colocarVentana(); });

  // Hace arrastrable una ventana; alSoltar se llama solo si realmente se movió
  function arrastrable(selector, alSoltar, noArrastre) {
    var el = document.querySelector(selector);
    if (!el) { return; }
    var inicio = null;
    el.addEventListener('pointerdown', function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      if ($(e.target).closest('input, select, textarea, button, a, label').length) { return; }
      if (e.offsetX > e.target.clientWidth && e.target.scrollHeight > e.target.clientHeight) {
        return;  // barra de desplazamiento
      }
      if (noArrastre && e.pointerType === 'mouse' && $(e.target).closest(noArrastre).length) { return; }  // texto seleccionable (solo mouse)
      var wrap = document.getElementById('mapa-wrap').getBoundingClientRect();
      var r = el.getBoundingClientRect();
      inicio = { x: e.clientX, y: e.clientY, l: r.left - wrap.left, t: r.top - wrap.top,
                 mov: false };
      try { el.setPointerCapture(e.pointerId); } catch (err) {}
      e.preventDefault();
    });
    el.addEventListener('pointermove', function (e) {
      if (!inicio) { return; }
      var dx = e.clientX - inicio.x, dy = e.clientY - inicio.y;
      if (!inicio.mov && Math.abs(dx) + Math.abs(dy) < 4) { return; }
      inicio.mov = true;
      el.classList.add('moviendo');
      var wrap = document.getElementById('mapa-wrap');
      var l = Math.max(0, Math.min(inicio.l + dx, wrap.clientWidth - el.offsetWidth));
      var t = Math.max(0, Math.min(inicio.t + dy, wrap.clientHeight - el.offsetHeight));
      el.style.left = l + 'px';
      el.style.top = t + 'px';
    });
    function soltar(e) {
      if (!inicio) { return; }
      var movio = inicio.mov;
      inicio = null;
      el.classList.remove('moviendo');
      try { el.releasePointerCapture(e.pointerId); } catch (err) {}
      if (movio && alSoltar) { alSoltar(); }
    }
    el.addEventListener('pointerup', soltar);
    el.addEventListener('pointercancel', soltar);
  }
  arrastrable('#ventana-valores', function () { ventanaMovida = true; }, '.vc-card, .vc-head');
  arrastrable('#panel-result', null, '.vc-card, .rs-total, .rs-parcela');
    // Achicar/agrandar las ventanas (valores, coordenadas y resultados):
  // arrastrar su franja superior hacia abajo las achica, hacia arriba las agranda
  (function () {
    var ini = null;
    var VENTANAS = [
      { id: 'ventana-valores', cuerpo: '#vv-cuerpo' },
      { id: 'ventana-coord',   cuerpo: '#ventana-coord .pc-form' },
      { id: 'panel-result',    cuerpo: '#pr-cuerpo' }
    ];

    function buscar(e) {
      if (e.target && e.target.closest && e.target.closest('button, input, select, textarea, a, label')) { return null; }
      var i, v, r;
      for (i = 0; i < VENTANAS.length; i++) {
        v = document.getElementById(VENTANAS[i].id);
        if (!v || getComputedStyle(v).display === 'none') { continue; }
        r = v.getBoundingClientRect();
        if (e.clientX < r.left || e.clientX > r.right) { continue; }
        if (e.clientY < r.top - 6 || e.clientY > r.top + 24) { continue; }
        return { v: v, def: VENTANAS[i] };
      }
      return null;
    }

    document.addEventListener('pointerdown', function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      var f = buscar(e);
      if (!f) { return; }
      var cuerpo = document.querySelector(f.def.cuerpo);
      if (!cuerpo) { return; }
      e.stopPropagation();
      e.preventDefault();
      var wrap = document.getElementById('mapa-wrap').getBoundingClientRect();
      var r = f.v.getBoundingClientRect();
      cuerpo.style.overflowY = 'auto';
      ini = { y: e.clientY, top: r.top - wrap.top, h: cuerpo.getBoundingClientRect().height,
              v: f.v, cuerpo: cuerpo, id: f.def.id,
              fijoAbajo: f.def.id === 'panel-result' && f.v.style.top === '' &&
                         window.matchMedia('(max-width: 820px)').matches };
    }, true);

    document.addEventListener('pointermove', function (e) {
      if (!ini) { return; }
      var natural = ini.cuerpo.scrollHeight;
      var nuevo = Math.max(60, Math.min(natural, ini.h - (e.clientY - ini.y)));
      ini.cuerpo.style.maxHeight = nuevo + 'px';
      if (!ini.fijoAbajo) { ini.v.style.top = (ini.top + (ini.h - nuevo)) + 'px'; }
      if (ini.id === 'ventana-valores') { ventanaMovida = true; }
      if (ini.id === 'ventana-coord') { coordMovida = true; }
      e.preventDefault();
    }, true);

    function fin() { ini = null; }
    document.addEventListener('pointerup', fin, true);
    document.addEventListener('pointercancel', fin, true);
  })();

  // Al copiar valores del cuadro de resultados, se copian como tabla ordenada
  // (texto separado por tabulaciones y tabla HTML) para pegar en Word o Excel
  function escHtml(t) {
    return String(t).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  document.addEventListener('copy', function (e) {
    var sel = window.getSelection();
    if (!sel || sel.isCollapsed || !sel.rangeCount || !e.clipboardData) { return; }
    if (!$(sel.anchorNode).closest('#panel-result').length) { return; }
    var raiz = document.querySelector('#pr-cuerpo .vc');
    if (!raiz) { return; }
    var rango = sel.getRangeAt(0);
    var bloques = [].slice.call(raiz.querySelectorAll('.rs-total, .rs-parcela, .vc-card[data-seccion]'));
    var elegidos = bloques.filter(function (b) { return rango.intersectsNode(b); });
    if (!elegidos.length) { elegidos = bloques; }
    var soloM2 = elegidos.every(function (b) { return b.getAttribute('data-modo') === 'm2'; });
    var enc = soloM2 ? ['Variable', 'Área (m²)', '%'] : ['Variable', 'Área (ha)', 'Área (m²)', '%'];
    function fila(nombre, ha, m2, pc) { return soloM2 ? [nombre, m2, pc] : [nombre, ha, m2, pc]; }
    var filas = [{ enc: true, c: enc }];
    elegidos.forEach(function (b) {
      var d = b.dataset;
      if (b.classList.contains('rs-total')) {
        filas.push({ c: fila('Área total', d.ha, d.m2, '100.0') });
      } else if (b.classList.contains('rs-parcela')) {
        filas.push({ c: fila(d.nombre, d.ha, d.m2, d.pc) });
      } else {
        filas.push({ sec: true,
          c: [d.seccion].concat(enc.slice(1).map(function () { return ''; })) });
        [].forEach.call(b.querySelectorAll('.rs-fila[data-etq]'), function (r) {
          var x = r.dataset;
          filas.push({ c: fila(x.etq, x.ha, x.m2, x.pc) });
        });
      }
    });
    var tsv = filas.map(function (f) { return f.c.join('\t'); }).join('\r\n');
    var html = '<table border="1" cellspacing="0" cellpadding="4">' +
      filas.map(function (f) {
        var tag = f.enc ? 'th' : 'td';
        return '<tr>' + f.c.map(function (v, i) {
          var t = escHtml(v);
          if (f.sec && i === 0) { t = '<b>' + t + '</b>'; }
          return '<' + tag + '>' + t + '</' + tag + '>';
        }).join('') + '</tr>';
      }).join('') + '</table>';
    e.clipboardData.setData('text/plain', tsv);
    e.clipboardData.setData('text/html', html);
    e.preventDefault();
  });

  // Enter dentro de los casilleros de coordenadas
  $('#ventana-valores').on('keydown', 'input', function (e) {
    if (e.which === 13) { window.irACoordenadas(); }
  });

  $('#ventana-coord').on('keydown', 'input', function (e) {
    if (e.which === 13) { window.irACoordenadas(); }
  });
  arrastrable('#ventana-coord', function () { coordMovida = true; });

  // Pone el marcador en un punto y pide los valores al servidor
  function consultarEn(latlng) {
    idClic += 1;
    var id = idClic;
    if (marcador) { mapa.removeLayer(marcador); }
    marcador = L.circleMarker(latlng, {
      radius: 7, color: '#ffffff', weight: 2, fillColor: '#d32f2f', fillOpacity: 1
    }).addTo(mapa);
    marcador._idc = id;
    mostrarVentana('<div class="vc"><div class="vc-caja"><div class="vc-head">' +
      'Consultando valores…</div></div></div>', latlng);
    Shiny.setInputValue('clic_mapa',
      { lat: latlng.lat, lng: latlng.lng, id: id }, { priority: 'event' });
  }

  function alClicMapa(e) {
    if (!modoConsulta) { return; }
    consultarEn(e.latlng);
  }

  window.centrarPunto = function (lat, lng) {
    if (mapa) { mapa.setView([lat, lng], Math.max(mapa.getZoom(), 15)); }
  };

  // ---- Ir a coordenadas (geográficas o UTM) ----
  var modoCoordActual = 'geo';

  function enPeru(lat, lon) {
    return lat >= -18.6 && lat <= 0.2 && lon >= -81.6 && lon <= -68.4;
  }

  // Acepta -7.377491, 7,377491 o 7°22′39″ S (grados, minutos y segundos)
  function leerAngulo(txt) {
    if (txt === null || txt === undefined) { return NaN; }
    var t = String(txt).toUpperCase().replace(/\u2212/g, '-').replace(/,/g, '.').trim();
    if (!t) { return NaN; }
    var nums = t.match(/\d+(?:\.\d+)?/g);
    if (!nums) { return NaN; }
    var neg = /^\s*-/.test(t) || /[SWO]/.test(t);
    var v = 0, div = 1, i;
    for (i = 0; i < nums.length && i < 3; i++) {
      v += parseFloat(nums[i]) / div;
      div *= 60;
    }
    return neg ? -v : v;
  }

  function leerMetros(txt) {
    var t = String(txt === null || txt === undefined ? '' : txt)
      .replace(/\s|'/g, '').replace(/,/g, '.');
    var v = parseFloat(t);
    return isFinite(v) ? v : NaN;
  }

  // UTM (WGS84) -> latitud y longitud
  function utmALatLon(este, norte, zona, sur) {
    var a = 6378137, f = 1 / 298.257223563, k0 = 0.9996;
    var e2 = f * (2 - f), ep2 = e2 / (1 - e2);
    var x = este - 500000, y = sur ? norte - 10000000 : norte;
    var M = y / k0;
    var mu = M / (a * (1 - e2 / 4 - 3 * e2 * e2 / 64 - 5 * e2 * e2 * e2 / 256));
    var e1 = (1 - Math.sqrt(1 - e2)) / (1 + Math.sqrt(1 - e2));
    var phi1 = mu + (3 * e1 / 2 - 27 * Math.pow(e1, 3) / 32) * Math.sin(2 * mu)
      + (21 * e1 * e1 / 16 - 55 * Math.pow(e1, 4) / 32) * Math.sin(4 * mu)
      + (151 * Math.pow(e1, 3) / 96) * Math.sin(6 * mu)
      + (1097 * Math.pow(e1, 4) / 512) * Math.sin(8 * mu);
    var s1 = Math.sin(phi1), c1 = Math.cos(phi1), t1 = Math.tan(phi1);
    var N1 = a / Math.sqrt(1 - e2 * s1 * s1);
    var T1 = t1 * t1, C1 = ep2 * c1 * c1;
    var R1 = a * (1 - e2) / Math.pow(1 - e2 * s1 * s1, 1.5);
    var D = x / (N1 * k0);
    var lat = phi1 - (N1 * t1 / R1) * (D * D / 2
      - (5 + 3 * T1 + 10 * C1 - 4 * C1 * C1 - 9 * ep2) * Math.pow(D, 4) / 24
      + (61 + 90 * T1 + 298 * C1 + 45 * T1 * T1 - 252 * ep2 - 3 * C1 * C1) * Math.pow(D, 6) / 720);
    var lon0 = (zona * 6 - 183) * Math.PI / 180;
    var lon = lon0 + (D - (1 + 2 * T1 + C1) * Math.pow(D, 3) / 6
      + (5 - 2 * C1 + 28 * T1 - 3 * C1 * C1 + 8 * ep2 + 24 * T1 * T1) * Math.pow(D, 5) / 120) / c1;
    return [lat * 180 / Math.PI, lon * 180 / Math.PI];
  }

  // ---- Menú "Consultar valor": Seleccionar punto, Ingresar coordenadas, Delimitar área ----
  var menuAbierto = false, coordAbierta = false, coordMovida = false;

  function actualizarBoton() {
    $('#btn_consulta').toggleClass('activo',
      menuAbierto || modoConsulta || dibujando || coordAbierta);
    $('#mi_punto').toggleClass('activo', modoConsulta);
    $('#mi_coord').toggleClass('activo', coordAbierta);
    $('#mi_area').toggleClass('activo', dibujando);
  }

  function cerrarMenu() {
    menuAbierto = false;
    $('#menu-consulta').css('display', 'none');
    actualizarBoton();
  }

  window.toggleMenuConsulta = function () {
    menuAbierto = !menuAbierto;
    $('#menu-consulta').css('display', menuAbierto ? 'block' : 'none');
    actualizarBoton();
  };

  $(document).on('click', function (e) {
    if (menuAbierto && !$(e.target).closest('.consulta-wrap').length) { cerrarMenu(); }
  });

  function cerrarCoord() {
    coordAbierta = false;
    $('#ventana-coord').css('display', 'none');
    actualizarBoton();
  }
  window.cerrarCoord = cerrarCoord;

  function abrirCoord() {
    if (modoConsulta) { window.toggleConsulta(); }
    if (dibujando) { detenerDibujo(true); }
    coordAbierta = true;
    var $c = $('#ventana-coord');
    $c.css('display', 'block');
    if (!coordMovida) {
      var W = $('#mapa-wrap').innerWidth();
      $c.css({ left: Math.max(8, W - $c.outerWidth() - 64) + 'px', top: '110px' });
    }
    $('#pc-msg').removeClass('ok').text('');
    actualizarBoton();
  }

  window.elegirHerramienta = function (h) {
    cerrarMenu();
    if (h === 'punto') {
      window.toggleConsulta();
    } else if (h === 'coord') {
      if (coordAbierta) { cerrarCoord(); } else { abrirCoord(); }
    } else if (h === 'area') {
      if (dibujando) { detenerDibujo(true); } else { iniciarDibujo(); }
    } else if (h === 'importar') {
      var inp = document.getElementById('archivo-aoi');
      inp.value = '';
      inp.click();
      avisoMapa('Elige tu archivo: SHP (con su .prj), GPKG, KML o KMZ. También puedes arrastrarlo al mapa.', 6000, false);
    }
  };

  window.modoCoord = function (m) {
    modoCoordActual = m;
    $('#pc-tab-geo').toggleClass('activo', m === 'geo');
    $('#pc-tab-utm').toggleClass('activo', m === 'utm');
    $('#pc-geo').toggle(m === 'geo');
    $('#pc-utm').toggle(m === 'utm');
    $('#pc-msg').removeClass('ok').text('');
  };

  window.irACoordenadas = function () {
    var $msg = $('#pc-msg');
    function fallo(t) { $msg.removeClass('ok').text(t); }
    if (!mapa) { fallo('El mapa todavía no está listo. Espera un momento.'); return; }
    var lat, lon, nota = '';
    if (modoCoordActual === 'geo') {
      lat = leerAngulo($('#pc-lat').val());
      lon = leerAngulo($('#pc-lon').val());
      if (isNaN(lat) || isNaN(lon)) { fallo('Escribe la latitud y la longitud.'); return; }
      if (Math.abs(lat) > 90 || Math.abs(lon) > 180) {
        fallo('Los valores están fuera de rango.'); return;
      }
      if (!enPeru(lat, lon)) {
        var lat2 = -Math.abs(lat), lon2 = -Math.abs(lon);
        if (enPeru(lat2, lon2)) {
          lat = lat2; lon = lon2;
          nota = 'Se interpretó como latitud Sur y longitud Oeste.';
        } else {
          fallo('El punto está fuera de Perú. Revisa los valores.'); return;
        }
      }
    } else {
      var este = leerMetros($('#pc-este').val());
      var norte = leerMetros($('#pc-norte').val());
      if (isNaN(este) || isNaN(norte)) { fallo('Escribe el Este y el Norte en metros.'); return; }
      var zona = parseInt($('#pc-zona').val(), 10);
      var sur = $('#pc-hem').val() === 'S';
      var ll = utmALatLon(este, norte, zona, sur);
      lat = ll[0]; lon = ll[1];
      if (!isFinite(lat) || !isFinite(lon) || !enPeru(lat, lon)) {
        fallo('El punto queda fuera de Perú. Revisa la zona, el hemisferio y los valores.');
        return;
      }
      nota = 'Coordenadas geográficas: Lat ' + lat.toFixed(6) + ' | Lon ' + lon.toFixed(6);
    }
    cerrarCoord();
    mapa.setView([lat, lon], 16, { animate: false });
    consultarEn(L.latLng(lat, lon));
    $msg.addClass('ok').text(nota || ('Lat ' + lat.toFixed(6) + ' | Lon ' + lon.toFixed(6)));
  };

  // ==INICIO_GOOGLE==
  // ---- Mapas base de Google (API oficial Map Tiles; necesita la clave GOOGLE_MAPS_API_KEY) ----
  var googleSesiones = {}, googleCapas = {}, googleActiva = null, googleAttr = '', googleT = null;
  var TIPOS_GOOGLE = [
    { id: 'mapa', nombre: 'Mapa (Google)', cuerpo: { mapType: 'roadmap' } },
    { id: 'sat', nombre: 'Satélite (Google)',
      cuerpo: { mapType: 'satellite', layerTypes: ['layerRoadmap'], overlay: false } },
    { id: 'relieve', nombre: 'Relieve (Google)',
      cuerpo: { mapType: 'terrain', layerTypes: ['layerRoadmap'] } }
  ];

  async function crearSesionGoogle(clave, cuerpo) {
    var r = await fetch('https://tile.googleapis.com/v1/createSession?key=' + encodeURIComponent(clave), {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(cuerpo)
    });
    if (!r.ok) { throw new Error('Google respondió ' + r.status); }
    var j = await r.json();
    if (!j.session) { throw new Error('Google no entregó la sesión del mapa.'); }
    return j.session;
  }

  function ponerAtribucionGoogle(texto) {
    var ac = mapa && mapa.attributionControl;
    if (!ac) { return; }
    if (googleAttr) { ac.removeAttribution(googleAttr); googleAttr = ''; }
    if (texto) { googleAttr = texto; ac.addAttribution(texto); }
  }

  // Google exige mostrar la atribución que devuelve su servicio para la vista actual
  function actualizarAtribucionGoogle() {
    clearTimeout(googleT);
    googleT = setTimeout(async function () {
      var id = googleActiva;
      if (!id || !googleCapas[id] || !mapa.hasLayer(googleCapas[id])) { ponerAtribucionGoogle(''); return; }
      var b = mapa.getBounds();
      var url = 'https://tile.googleapis.com/tile/v1/viewport?session=' + googleSesiones[id] +
        '&key=' + encodeURIComponent(window.GMAPS_KEY) +
        '&zoom=' + Math.round(mapa.getZoom()) +
        '&north=' + Math.min(89, b.getNorth()) + '&south=' + Math.max(-89, b.getSouth()) +
        '&east=' + Math.min(179, b.getEast()) + '&west=' + Math.max(-179, b.getWest());
      try {
        var r = await fetch(url);
        if (r.ok) { var j = await r.json(); ponerAtribucionGoogle(j.copyright || ''); }
      } catch (e) { /* sin atribución dinámica */ }
    }, 400);
  }

  // Selector propio de mapa base: botones siempre visibles (Mapa | Satélite | ...)
  function selectorBase(items, inicial) {
    var div = null;
    function poner(id) {
      items.forEach(function (it) {
        if (it.id === id) { if (!mapa.hasLayer(it.capa)) { it.capa.addTo(mapa); } }
        else if (mapa.hasLayer(it.capa)) { mapa.removeLayer(it.capa); }
      });
      if (div) {
        [].forEach.call(div.querySelectorAll('button'), function (b) {
          b.classList.toggle('activo', b.getAttribute('data-id') === id);
        });
      }
      googleActiva = id;
      actualizarAtribucionGoogle();
    }
    var Ctrl = L.Control.extend({
      options: { position: 'topright' },
      onAdd: function () {
        div = L.DomUtil.create('div', 'selector-base');
        items.forEach(function (it) {
          var b = L.DomUtil.create('button', '', div);
          b.type = 'button';
          b.textContent = it.nombre;
          b.setAttribute('data-id', it.id);
          L.DomEvent.on(b, 'click', function (e) { L.DomEvent.stop(e); poner(it.id); });
        });
        L.DomEvent.disableClickPropagation(div);
        return div;
      }
    });
    new Ctrl().addTo(mapa);
    poner(inicial);
  }

  // Sin clave oficial: mapas de Google (los mismos que usa Earth Engine), sin avisos
  function iniciarRespaldo(motivo) {
    if (!mapa) { return; }
    function gtile(lyrs) {
      return L.tileLayer('https://mt{s}.google.com/vt/lyrs=' + lyrs + '&hl=es&x={x}&y={y}&z={z}',
        { subdomains: '0123', maxZoom: 22, maxNativeZoom: 20, zIndex: 1, attribution: '' });
    }
    selectorBase([
      { id: 'mapa', nombre: 'Mapa', capa: gtile('m') },
      { id: 'sat', nombre: 'Satélite', capa: gtile('y') }
    ], 'mapa');
  }

  async function iniciarGoogle() {
    var clave = window.GMAPS_KEY;
    if (!mapa) { return; }
    if (!clave) {
      iniciarRespaldo('No hay clave GOOGLE_MAPS_API_KEY.');
      return;
    }
    try {
      var i, t, items = [], nombres = { mapa: 'Mapa', sat: 'Satélite', relieve: 'Relieve' };
      for (i = 0; i < TIPOS_GOOGLE.length; i++) {
        t = TIPOS_GOOGLE[i];
        var cuerpo = $.extend({ language: 'es', region: 'PE' }, t.cuerpo);
        var ses = await crearSesionGoogle(clave, cuerpo);
        googleSesiones[t.id] = ses;
        googleCapas[t.id] = L.tileLayer('https://tile.googleapis.com/v1/2dtiles/{z}/{x}/{y}?session=' +
          ses + '&key=' + encodeURIComponent(clave), { maxZoom: 22, tileSize: 256, zIndex: 1 });
        items.push({ id: t.id, nombre: nombres[t.id], capa: googleCapas[t.id] });
      }
      selectorBase(items, 'mapa');
      mapa.on('moveend', actualizarAtribucionGoogle);
    } catch (err) {
      if (window.console) { console.error('Visor: mapas de Google', err); }
      iniciarRespaldo('Google rechazó la clave o la API Map Tiles no está habilitada.');
    }
  }
  // ==FIN_GOOGLE==

  // ==INICIO_IMPORTAR==
  // Lectura de áreas de interés (KML, KMZ, SHP, ZIP con SHP, GPKG) directamente en el navegador.
  var AVISO_PSAD = ' · Datum PSAD56: la ubicación puede desplazarse unos cientos de metros.';
  var MAX_VERTICES = 20000;

  function extension(n) {
    var m = /\.([^.\/\\]+)$/.exec(n || '');
    return m ? m[1].toLowerCase() : '';
  }
  function baseNombre(n) {
    return String(n || '').split(/[\/\\]/).pop().replace(/\.[^.]*$/, '').toLowerCase();
  }
  function decodificar(buf) {
    return new TextDecoder('utf-8').decode(buf).replace(/^\uFEFF/, '');
  }

  // ---- ZIP (KMZ y shapefile comprimido) ----
  async function leerZip(buf) {
    var dv = new DataView(buf), u8 = new Uint8Array(buf), n = buf.byteLength, eocd = -1, i, k;
    for (i = n - 22; i >= Math.max(0, n - 65557); i--) {
      if (dv.getUint32(i, true) === 0x06054b50) { eocd = i; break; }
    }
    if (eocd < 0) { throw new Error('El archivo comprimido no es válido.'); }
    var total = dv.getUint16(eocd + 10, true), p = dv.getUint32(eocd + 16, true);
    var dec = new TextDecoder('utf-8'), archivos = {};
    for (k = 0; k < total; k++) {
      if (p + 46 > n || dv.getUint32(p, true) !== 0x02014b50) { break; }
      var metodo = dv.getUint16(p + 10, true), csize = dv.getUint32(p + 20, true);
      var nlen = dv.getUint16(p + 28, true), elen = dv.getUint16(p + 30, true);
      var clen = dv.getUint16(p + 32, true), lho = dv.getUint32(p + 42, true);
      var nombre = dec.decode(u8.subarray(p + 46, p + 46 + nlen));
      if (nombre.charAt(nombre.length - 1) !== '/') {
        archivos[nombre] = { metodo: metodo, csize: csize, lho: lho };
      }
      p += 46 + nlen + elen + clen;
    }
    return { dv: dv, u8: u8, archivos: archivos };
  }

  async function extraerZip(zip, nombre) {
    var e = zip.archivos[nombre];
    var nlen = zip.dv.getUint16(e.lho + 26, true), elen = zip.dv.getUint16(e.lho + 28, true);
    var ini = e.lho + 30 + nlen + elen;
    var datos = zip.u8.slice(ini, ini + e.csize);
    if (e.metodo === 0) { return datos.buffer; }
    if (e.metodo === 8) { return await inflarRaw(datos); }
    throw new Error('El archivo comprimido usa un método no admitido.');
  }

  // ---- Geometría básica ----
  function quitarCierre(a) {
    var n = a.length;
    if (n > 1 && a[0][0] === a[n - 1][0] && a[0][1] === a[n - 1][1]) { return a.slice(0, n - 1); }
    return a;
  }
  function areaFirmada(a) {
    var s = 0, i, j;
    for (i = 0, j = a.length - 1; i < a.length; j = i++) {
      s += (a[j][0] * a[i][1]) - (a[i][0] * a[j][1]);
    }
    return s / 2;
  }
  function puntoEnAnillo(pt, a) {
    var dentro = false, i, j;
    for (i = 0, j = a.length - 1; i < a.length; j = i++) {
      var xi = a[i][0], yi = a[i][1], xj = a[j][0], yj = a[j][1];
      if (((yi > pt[1]) !== (yj > pt[1])) &&
          (pt[0] < (xj - xi) * (pt[1] - yi) / (yj - yi) + xi)) { dentro = !dentro; }
    }
    return dentro;
  }
  // Un shapefile guarda anillos exteriores (horario) y huecos (antihorario)
  function agruparAnillos(anillos) {
    var polis = [], huecos = [];
    anillos.forEach(function (a) {
      if (a.length < 3) { return; }
      if (areaFirmada(a) <= 0) { polis.push([a]); } else { huecos.push(a); }
    });
    if (!polis.length) {
      huecos.forEach(function (h) { polis.push([h]); });
      return polis;
    }
    huecos.forEach(function (h) {
      var dueno = null, i;
      for (i = 0; i < polis.length; i++) {
        if (puntoEnAnillo(h[0], polis[i][0])) { dueno = polis[i]; break; }
      }
      (dueno || polis[polis.length - 1]).push(h);
    });
    return polis;
  }

  // ---- KML (cada Placemark es una parcela) ----
  function limpiarTexto(t) {
    return String(t).replace(/<!\[CDATA\[([\s\S]*?)\]\]>/g, '$1')
      .replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&quot;/g, '"')
      .replace(/&apos;/g, "'").replace(/&amp;/g, '&').trim();
  }

  function polisDeKML(bloque) {
    var polis = [], m;
    var re = /<(?:\w+:)?Polygon\b[\s\S]*?<\/(?:\w+:)?Polygon>/gi;
    while ((m = re.exec(bloque))) {
      var anillos = [], c;
      var re2 = /<(?:\w+:)?coordinates>([\s\S]*?)<\/(?:\w+:)?coordinates>/gi;
      while ((c = re2.exec(m[0]))) {
        var pts = c[1].trim().split(/\s+/).map(function (t) {
          var q = t.split(',');
          return [parseFloat(q[0]), parseFloat(q[1])];
        }).filter(function (q) { return isFinite(q[0]) && isFinite(q[1]); });
        pts = quitarCierre(pts);
        if (pts.length >= 3) { anillos.push(pts); }
      }
      if (anillos.length) { polis.push(anillos); }
    }
    return polis;
  }

  function parsearKML(texto) {
    var parcelas = [], m, hubo = false;
    var re = /<(?:\w+:)?Placemark\b[\s\S]*?<\/(?:\w+:)?Placemark>/gi;
    while ((m = re.exec(texto))) {
      hubo = true;
      var polis = polisDeKML(m[0]);
      if (polis.length) {
        var nm = /<(?:\w+:)?name>([\s\S]*?)<\/(?:\w+:)?name>/i.exec(m[0]);
        parcelas.push({ nombre: nm ? limpiarTexto(nm[1]) : '', polis: polis });
      }
    }
    if (!hubo) {
      var todos = polisDeKML(texto);
      if (todos.length) { parcelas.push({ nombre: '', polis: todos }); }
    }
    return parcelas;
  }

  // ---- Shapefile (.shp): cada registro es una parcela ----
  function parsearSHP(buf) {
    var dv = new DataView(buf), n = buf.byteLength, pos = 100, parcelas = [], reg = 0;
    if (n < 100 || dv.getInt32(0, false) !== 9994) { throw new Error('El archivo .shp no es válido.'); }
    while (pos + 12 <= n) {
      var bytes = dv.getInt32(pos + 4, false) * 2, ini = pos + 8;
      if (bytes <= 0) { break; }
      var tipo = dv.getInt32(ini, true);
      if (tipo === 5 || tipo === 15 || tipo === 25) {
        var nPartes = dv.getInt32(ini + 36, true), nPts = dv.getInt32(ini + 40, true);
        var partes = [], i, j;
        for (i = 0; i < nPartes; i++) { partes.push(dv.getInt32(ini + 44 + 4 * i, true)); }
        var base = ini + 44 + 4 * nPartes, anillos = [];
        for (i = 0; i < nPartes; i++) {
          var a = partes[i], b = (i + 1 < nPartes) ? partes[i + 1] : nPts, anillo = [];
          for (j = a; j < b; j++) {
            anillo.push([dv.getFloat64(base + 16 * j, true), dv.getFloat64(base + 16 * j + 8, true)]);
          }
          anillos.push(quitarCierre(anillo));
        }
        var polis = agruparAnillos(anillos);
        if (polis.length) { parcelas.push({ nombre: '', polis: polis, reg: reg }); }
      }
      reg += 1;
      pos = ini + bytes;
    }
    return parcelas;
  }

  // Nombres de las parcelas desde el .dbf (campo tipo nombre, lote, parcela, código...)
  var PREF_NOMBRE = /^(nombre|name|parcela|lote|codigo|cod|descripcio|propietario|titular)/i;

  function parsearDBF(buf, cpg) {
    var u8 = new Uint8Array(buf), dv = new DataView(buf);
    if (buf.byteLength < 33) { return []; }
    var nReg = dv.getUint32(4, true), hLen = dv.getUint16(8, true), rLen = dv.getUint16(10, true);
    var campos = [], p = 32, off = 1, k;
    while (p + 32 <= hLen && u8[p] !== 0x0d) {
      var nom = '';
      for (k = 0; k < 11 && u8[p + k] !== 0; k++) { nom += String.fromCharCode(u8[p + k]); }
      var len = u8[p + 16], tipo = String.fromCharCode(u8[p + 11]);
      campos.push({ nombre: nom.trim(), len: len, off: off, tipo: tipo });
      off += len;
      p += 32;
    }
    var col = null;
    campos.forEach(function (c) {
      if (!col && (c.tipo === 'C' || c.tipo === 'N') && PREF_NOMBRE.test(c.nombre)) { col = c; }
    });
    if (!col) { return []; }
    var dec;
    try { dec = new TextDecoder(/65001|UTF/i.test(cpg || '') ? 'utf-8' : 'windows-1252'); }
    catch (e) { dec = new TextDecoder('utf-8'); }
    var nombres = [], i, ini;
    for (i = 0; i < nReg; i++) {
      ini = hLen + i * rLen;
      if (ini + rLen > buf.byteLength) { break; }
      if (u8[ini] === 0x2a) { nombres.push(''); continue; }
      nombres.push(dec.decode(u8.subarray(ini + col.off, ini + col.off + col.len))
        .replace(/\0/g, '').trim());
    }
    return nombres;
  }

  // ---- Sistemas de coordenadas ----
  function crsDesdePRJ(wkt) {
    var t = String(wkt || '').toUpperCase();
    var psad = /PSAD|PROVISIONAL[_ ]S(?:OUTH)?[_ ]AMERICAN/.test(t);
    if (/^\s*(GEOGCS|GEOGCRS)/.test(t)) { return { tipo: 'geo', psad: psad }; }
    if (t.indexOf('PROJCS') >= 0 || t.indexOf('PROJCRS') >= 0) {
      var num = function (nombre) {
        var r = new RegExp(nombre + '"\\s*,\\s*(-?[\\d.]+)').exec(t);
        return r ? parseFloat(r[1]) : NaN;
      };
      var cm = num('CENTRAL_MERIDIAN'), fe = num('FALSE_EASTING');
      var fn = num('FALSE_NORTHING'), k = num('SCALE_FACTOR');
      if (Math.abs(k - 0.9996) < 1e-6 && Math.abs(fe - 500000) < 1) {
        return { tipo: 'utm', zona: Math.round((cm + 183) / 6), sur: fn > 1, psad: psad };
      }
      var z = /UTM[_ ]ZONE[_ ](\d{1,2})([NS])/.exec(t);
      if (z) { return { tipo: 'utm', zona: parseInt(z[1], 10), sur: z[2] === 'S', psad: psad }; }
      if (/MERCATOR_AUXILIARY_SPHERE|POPULAR[_ ]VISUALISATION|WEB[_ ]MERCATOR|PSEUDO[_ -]MERCATOR/.test(t)) {
        return { tipo: 'merc' };
      }
    }
    return null;
  }

  function crsDesdeEPSG(c) {
    if (c === 4326 || c === 4674 || c === 4019 || c === 4258) { return { tipo: 'geo' }; }
    if (c === 4248) { return { tipo: 'geo', psad: true }; }
    if (c >= 32601 && c <= 32660) { return { tipo: 'utm', zona: c - 32600, sur: false }; }
    if (c >= 32701 && c <= 32760) { return { tipo: 'utm', zona: c - 32700, sur: true }; }
    if (c >= 31977 && c <= 31979) { return { tipo: 'utm', zona: c - 31960, sur: true }; }
    if (c >= 24877 && c <= 24879) { return { tipo: 'utm', zona: c - 24860, sur: true, psad: true }; }
    if (c === 3857 || c === 900913) { return { tipo: 'merc' }; }
    return null;
  }

  function adivinarCRS(polis) {
    var ok = polis.every(function (p) {
      return p.every(function (a) {
        return a.every(function (c) { return Math.abs(c[0]) <= 180 && Math.abs(c[1]) <= 90; });
      });
    });
    return ok ? { tipo: 'geo' } : null;
  }

  function aLonLat(x, y, crs) {
    if (crs.tipo === 'utm') {
      var r = utmALatLon(x, y, crs.zona, crs.sur);
      return [r[1], r[0]];
    }
    if (crs.tipo === 'merc') {
      var R = 6378137;
      return [x / R * 180 / Math.PI, (2 * Math.atan(Math.exp(y / R)) - Math.PI / 2) * 180 / Math.PI];
    }
    return [x, y];
  }

  function convertir(polis, crs) {
    if (crs.tipo === 'geo') { return polis; }
    return polis.map(function (p) {
      return p.map(function (a) { return a.map(function (c) { return aLonLat(c[0], c[1], crs); }); });
    });
  }

  // ---- GeoPackage: lector mínimo de SQLite ----
  function varintSQL(u8, off) {
    var v = 0, i, b;
    for (i = 0; i < 8; i++) {
      b = u8[off + i];
      v = v * 128 + (b & 0x7f);
      if (!(b & 0x80)) { return { v: v, n: off + i + 1 }; }
    }
    return { v: v * 256 + u8[off + 8], n: off + 9 };
  }

  function enteroBE(b, off, len) {
    var v = 0, i;
    for (i = 0; i < len; i++) { v = v * 256 + b[off + i]; }
    if (len && (b[off] & 0x80)) { v -= Math.pow(2, 8 * len); }
    return v;
  }

  function SQLiteLector(buf) {
    var u8 = new Uint8Array(buf), dv = new DataView(buf);
    if (buf.byteLength < 100 || String.fromCharCode.apply(null, u8.subarray(0, 15)) !== 'SQLite format 3') {
      throw new Error('El archivo .gpkg no es una base de datos válida.');
    }
    var tam = dv.getUint16(16, false);
    if (tam === 1) { tam = 65536; }
    var U = tam - u8[20], X = U - 35, M = Math.floor((U - 12) * 32 / 255) - 23;
    var dec = new TextDecoder('utf-8');
    function pagina(n) { return (n - 1) * tam; }

    function leerPayload(off) {
      var r = varintSQL(u8, off), P = r.v;
      r = varintSQL(u8, r.n);
      var p = r.n;
      if (P <= X) { return u8.subarray(p, p + P); }
      var K = M + ((P - M) % (U - 4)), local = (K <= X) ? K : M;
      var partes = [u8.subarray(p, p + local)], restante = P - local;
      var sig = dv.getUint32(p + local, false);
      while (restante > 0 && sig) {
        var po = pagina(sig), tomar = Math.min(restante, U - 4);
        partes.push(u8.subarray(po + 4, po + 4 + tomar));
        restante -= tomar;
        sig = dv.getUint32(po, false);
      }
      var pl = new Uint8Array(P), o = 0;
      partes.forEach(function (q) { pl.set(q, o); o += q.length; });
      return pl;
    }

    function leerRegistro(pl) {
      var h = varintSQL(pl, 0), finH = h.v, p = h.n, tipos = [];
      while (p < finH) { var t = varintSQL(pl, p); tipos.push(t.v); p = t.n; }
      var dvp = new DataView(pl.buffer, pl.byteOffset, pl.byteLength), q = finH, vals = [];
      tipos.forEach(function (t) {
        var v = null, len = 0;
        if (t >= 1 && t <= 6) { len = [0, 1, 2, 3, 4, 6, 8][t]; v = enteroBE(pl, q, len); }
        else if (t === 7) { v = dvp.getFloat64(q, false); len = 8; }
        else if (t === 8) { v = 0; }
        else if (t === 9) { v = 1; }
        else if (t >= 12) {
          len = (t % 2 === 0) ? (t - 12) / 2 : (t - 13) / 2;
          v = (t % 2 === 0) ? pl.subarray(q, q + len) : dec.decode(pl.subarray(q, q + len));
        }
        vals.push(v);
        q += len;
      });
      return vals;
    }

    function recorrer(nPag, cb) {
      var po = pagina(nPag), hdr = (nPag === 1) ? po + 100 : po;
      var tipo = u8[hdr], nCeldas = dv.getUint16(hdr + 3, false), i;
      if (tipo === 0x0d) {
        for (i = 0; i < nCeldas; i++) {
          cb(leerRegistro(leerPayload(po + dv.getUint16(hdr + 8 + 2 * i, false))));
        }
      } else if (tipo === 0x05) {
        for (i = 0; i < nCeldas; i++) {
          recorrer(dv.getUint32(po + dv.getUint16(hdr + 12 + 2 * i, false), false), cb);
        }
        recorrer(dv.getUint32(hdr + 8, false), cb);
      }
    }
    return {
      recorrer: recorrer,
      filas: function (raiz) { var out = []; recorrer(raiz, function (f) { out.push(f); }); return out; }
    };
  }

  // WKB estándar / ISO / EWKB: guarda los polígonos en 'salida' y devuelve el siguiente offset
  function leerWKB(dv, off, salida) {
    var little = dv.getUint8(off) === 1, i, j;
    off += 1;
    var t = dv.getUint32(off, little), z = false, m = false;
    off += 4;
    if (t & 0x80000000) { z = true; }
    if (t & 0x40000000) { m = true; }
    var base = t & 0x0fffffff;
    if (t & 0x20000000) { off += 4; }
    if (base >= 3000) { z = true; m = true; }
    else if (base >= 2000) { m = true; }
    else if (base >= 1000) { z = true; }
    base = base % 1000;
    var dims = 2 + (z ? 1 : 0) + (m ? 1 : 0);
    if (base === 3) {
      var nr = dv.getUint32(off, little), anillos = [];
      off += 4;
      for (i = 0; i < nr; i++) {
        var np = dv.getUint32(off, little), a = [];
        off += 4;
        for (j = 0; j < np; j++) {
          a.push([dv.getFloat64(off, little), dv.getFloat64(off + 8, little)]);
          off += 8 * dims;
        }
        a = quitarCierre(a);
        if (a.length >= 3) { anillos.push(a); }
      }
      if (anillos.length) { salida.push(anillos); }
    } else if (base === 1) {
      off += 8 * dims;
    } else if (base === 2) {
      var nl = dv.getUint32(off, little);
      off += 4 + nl * 8 * dims;
    } else if (base === 4 || base === 5 || base === 6 || base === 7) {
      var ng = dv.getUint32(off, little);
      off += 4;
      for (i = 0; i < ng; i++) { off = leerWKB(dv, off, salida); }
    } else {
      throw new Error('El .gpkg usa un tipo de geometría no admitido.');
    }
    return off;
  }

  function leerGeomGPKG(b) {
    var dv = new DataView(b.buffer, b.byteOffset, b.byteLength);
    var flags = b[3], little = (flags & 1) === 1, env = (flags >> 1) & 7;
    if ((flags >> 4) & 1) { return null; }
    var srs = dv.getInt32(4, little);
    var off = 8 + ([0, 32, 48, 48, 64][env] || 0), polis = [];
    leerWKB(dv, off, polis);
    return { polis: polis, srs: srs };
  }

  function columnasDeSQL(sql) {
    sql = String(sql || '');
    var a = sql.indexOf('('), b = sql.lastIndexOf(')');
    if (a < 0 || b < a) { return []; }
    var cuerpo = sql.slice(a + 1, b), partes = [], prof = 0, ini = 0, i, ch;
    for (i = 0; i < cuerpo.length; i++) {
      ch = cuerpo.charAt(i);
      if (ch === '(') { prof++; }
      else if (ch === ')') { prof--; }
      else if (ch === ',' && prof === 0) { partes.push(cuerpo.slice(ini, i)); ini = i + 1; }
    }
    partes.push(cuerpo.slice(ini));
    var cols = [];
    partes.forEach(function (d) {
      var t = d.trim();
      if (/^(CONSTRAINT|PRIMARY|UNIQUE|FOREIGN|CHECK)\b/i.test(t)) { return; }
      var m = /^(?:"([^"]+)"|`([^`]+)`|\[([^\]]+)\]|(\S+))/.exec(t);
      cols.push(m ? (m[1] || m[2] || m[3] || m[4]) : '');
    });
    return cols;
  }

  // Cada fila de la capa es una parcela; el nombre sale de una columna tipo nombre/lote/parcela
  function parsearGPKG(buf) {
    var db = SQLiteLector(buf), raices = {}, sqls = {};
    db.filas(1).forEach(function (f) {
      if (f[0] === 'table') { raices[f[1]] = f[3]; sqls[f[1]] = f[4]; }
    });
    if (!raices.gpkg_geometry_columns) { throw new Error('El .gpkg no tiene capas con geometría.'); }
    var capas = db.filas(raices.gpkg_geometry_columns).filter(function (f) {
      return /POLYGON|GEOMETRY|SURFACE/i.test(String(f[2])) && raices[f[0]];
    });
    if (!capas.length) { throw new Error('El .gpkg no contiene capas de polígonos.'); }
    var parcelas = [], psad = false, sinSRS = null;
    capas.forEach(function (capa) {
      var cols = columnasDeSQL(sqls[capa[0]]), idxN = -1, i;
      for (i = 0; i < cols.length && idxN < 0; i++) {
        if (cols[i] !== capa[1] && PREF_NOMBRE.test(cols[i])) { idxN = i; }
      }
      db.recorrer(raices[capa[0]], function (fila) {
        var polisFila = [];
        fila.forEach(function (v) {
          if (v && typeof v === 'object' && v.length > 8 && v[0] === 0x47 && v[1] === 0x50) {
            var g = leerGeomGPKG(v);
            if (!g || !g.polis.length) { return; }
            var crs = crsDesdeEPSG(g.srs);
            if (!crs) {
              if (g.srs <= 0) { crs = adivinarCRS(g.polis); }
              if (!crs) { sinSRS = g.srs; return; }
            }
            if (crs.psad) { psad = true; }
            convertir(g.polis, crs).forEach(function (p) { polisFila.push(p); });
          }
        });
        if (polisFila.length) {
          var nom = (idxN >= 0 && typeof fila[idxN] === 'string') ? fila[idxN].trim() : '';
          parcelas.push({ nombre: nom, polis: polisFila });
        }
      });
    });
    if (!parcelas.length && sinSRS !== null) {
      throw new Error('El sistema de coordenadas del .gpkg (EPSG:' + sinSRS + ') no está admitido. Guárdalo en WGS84 (EPSG:4326).');
    }
    return { parcelas: parcelas, psad: psad };
  }

  // ---- Simplificación para no enviar demasiados vértices ----
  function distSeg(p, a, b) {
    var dx = b[0] - a[0], dy = b[1] - a[1], l2 = dx * dx + dy * dy, t = 0;
    if (l2 > 0) { t = Math.max(0, Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / l2)); }
    var x = a[0] + t * dx - p[0], y = a[1] + t * dy - p[1];
    return Math.sqrt(x * x + y * y);
  }
  function simplificarAnillo(a, tol) {
    var n = a.length;
    if (n <= 4) { return a; }
    var keep = new Uint8Array(n), pila = [[0, n - 1]], i, r = [];
    keep[0] = 1; keep[n - 1] = 1;
    while (pila.length) {
      var s = pila.pop(), dmax = 0, idx = -1;
      for (i = s[0] + 1; i < s[1]; i++) {
        var d = distSeg(a[i], a[s[0]], a[s[1]]);
        if (d > dmax) { dmax = d; idx = i; }
      }
      if (idx >= 0 && dmax > tol) { keep[idx] = 1; pila.push([s[0], idx], [idx, s[1]]); }
    }
    for (i = 0; i < n; i++) { if (keep[i]) { r.push(a[i]); } }
    return r.length >= 3 ? r : a;
  }
  function contarVertices(polis) {
    return polis.reduce(function (t, p) {
      return t + p.reduce(function (u, a) { return u + a.length; }, 0);
    }, 0);
  }
  function limpiarPolis(polis) {
    var salida = [];
    polis.forEach(function (p) {
      var anillos = p.map(function (a) {
        return quitarCierre(a.map(function (c) {
          return [Math.round(c[0] * 1e9) / 1e9, Math.round(c[1] * 1e9) / 1e9];
        }));
      }).filter(function (a) {
        return a.length >= 3 && a.every(function (c) { return isFinite(c[0]) && isFinite(c[1]); });
      });
      if (anillos.length) { salida.push(anillos); }
    });
    return salida;
  }

  function contarVerticesP(parcelas) {
    return parcelas.reduce(function (t, pc) { return t + contarVertices(pc.polis); }, 0);
  }

  // Limpia las parcelas, simplifica si hay demasiados vértices y pone nombre por defecto
  function limpiarParcelas(parcelas) {
    var salida = [];
    parcelas.forEach(function (pc) {
      var polis = limpiarPolis(pc.polis);
      if (polis.length) { salida.push({ nombre: pc.nombre || '', polis: polis }); }
    });
    var tol = 2e-5;
    while (contarVerticesP(salida) > MAX_VERTICES && tol < 1e-3) {
      salida.forEach(function (pc) {
        pc.polis = pc.polis.map(function (p) {
          return p.map(function (a) { return a.length > 50 ? simplificarAnillo(a, tol) : a; });
        });
      });
      tol *= 2;
    }
    var cifras = Math.max(2, String(salida.length).length);
    salida.forEach(function (pc, i) {
      pc.auto = !pc.nombre;
      pc.num = ('000' + (i + 1)).slice(-cifras);          // 01, 02, 03...
      if (!pc.nombre) { pc.nombre = 'Polígono ' + pc.num; }
    });
    return salida;
  }

  // ---- Reconoce el tipo de archivo(s) y devuelve los polígonos en longitud/latitud ----
  async function expandirZip(item) {
    var zip = await leerZip(await item.leer());
    return Object.keys(zip.archivos).map(function (n) {
      return { nombre: n, leer: function () { return extraerZip(zip, n); } };
    });
  }

  async function areaDesdeItems(items, nivel) {
    function de(e) { return items.filter(function (i) { return extension(i.nombre) === e; }); }
    var k, parcelas;
    if ((k = de('kml')).length) {
      return { parcelas: parsearKML(decodificar(await k[0].leer())), nombre: k[0].nombre, nota: '' };
    }
    if ((k = de('gpkg')).length) {
      var g = parsearGPKG(await k[0].leer());
      return { parcelas: g.parcelas, nombre: k[0].nombre, nota: g.psad ? AVISO_PSAD : '' };
    }
    if ((k = de('shp')).length) {
      var prjs = de('prj'), bn = baseNombre(k[0].nombre);
      var prj = prjs.filter(function (p) { return baseNombre(p.nombre) === bn; })[0] || prjs[0];
      parcelas = parsearSHP(await k[0].leer());
      var todas = [];
      parcelas.forEach(function (pc) { todas = todas.concat(pc.polis); });
      var crs;
      if (prj) {
        crs = crsDesdePRJ(decodificar(await prj.leer()));
        if (!crs) {
          throw new Error('No reconozco el sistema de coordenadas del .prj. Guarda el área en WGS84 (EPSG:4326) o UTM WGS84.');
        }
      } else {
        crs = adivinarCRS(todas);
        if (!crs) { throw new Error('Falta el archivo .prj del shapefile. Selecciónalo junto con el .shp.'); }
      }
      var dbfs = de('dbf'), dbf = dbfs.filter(function (d) { return baseNombre(d.nombre) === bn; })[0] || dbfs[0];
      if (dbf) {
        try {
          var cpgs = de('cpg');
          var nombres = parsearDBF(await dbf.leer(), cpgs.length ? decodificar(await cpgs[0].leer()) : '');
          parcelas.forEach(function (pc) { pc.nombre = nombres[pc.reg] || ''; });
        } catch (e) { /* sin nombres: se numeran las parcelas */ }
      }
      parcelas.forEach(function (pc) { pc.polis = convertir(pc.polis, crs); });
      return { parcelas: parcelas, nombre: k[0].nombre, nota: crs.psad ? AVISO_PSAD : '' };
    }
    if (nivel < 1) {
      k = de('kmz').concat(de('zip'));
      if (k.length) { return areaDesdeItems(await expandirZip(k[0]), nivel + 1); }
    }
    return null;
  }
  // ==FIN_IMPORTAR==

  // ==INICIO_RESULTADOS==
  // ---- Resultados del terreno: se arman aquí a partir de los datos que entrega el servidor ----
  function escAttr(t) { return escHtml(t).replace(/"/g, '&quot;'); }
  function fmtHa(m2) { return (m2 / 10000).toFixed(2); }
  function fmtM2(m2) { return String(Math.round(m2)).replace(/\B(?=(\d{3})+(?!\d))/g, ' '); }
  function fmtM2p(m2) { return String(Math.round(m2)); }
  function fmtPc(x) { return x.toFixed(1); }

  function tablaRangos(titulo, subtitulo, tema, areas, total, clases, enHa, seccion) {
    var filas = '', i;
    function fila(color, etq, a) {
      var pc = a / total * 100;
      var principal = enHa ? fmtHa(a) : fmtM2(a);
      var sec = enHa ? '<small class="rs-m2">(' + fmtM2(a) + ' m²)</small>' : '';
      return '<div class="rs-fila" data-etq="' + escAttr(etq) + '" data-ha="' + fmtHa(a) +
        '" data-m2="' + fmtM2p(a) + '" data-pc="' + fmtPc(pc) + '">' +
        '<span class="rs-sw" style="background:' + color + '"></span>' +
        '<span class="rs-et">' + escHtml(etq) + '</span>' +
        '<span class="rs-ar">' + principal + sec + '</span>' +
        '<span class="rs-pc">' + fmtPc(pc) + '</span>' +
        '<div class="rs-barra"><i style="width:' + Math.min(pc, 100).toFixed(1) +
        '%;background:' + color + '"></i></div></div>';
    }
    // solo se muestran los rangos que existen en el terreno
    for (i = 0; i < clases.length; i++) {
      if (areas[i + 1] / total * 100 >= 0.05) { filas += fila(clases[i][0], clases[i][1], areas[i + 1]); }
    }
    if (areas[0] / total * 100 >= 0.05) { filas += fila('#9aa5b1', 'Sin dato (nubes o sombras)', areas[0]); }
    return '<div class="vc-card ' + tema + '" data-modo="' + (enHa ? 'ha' : 'm2') +
      '" data-seccion="' + escAttr(seccion) + '">' +
      '<div class="rs-tit">' + titulo + '<span>' + subtitulo + '</span></div>' +
      '<div class="rs-fila rs-cab"><span class="rs-h1">Variable</span>' +
      '<span class="rs-h2">Área <span class="rs-u">' + (enHa ? '(ha)' : '(m²)') + '</span></span>' +
      '<span class="rs-h3">%</span></div>' + filas + '</div>';
  }

  function detalleParcela(p, total, CL) {
    var tp = p.total, enHa = tp > 5000;
    var resumen = enHa ? fmtHa(tp) + ' ha <small>(' + fmtM2(tp) + ' m²)</small>' : fmtM2(tp) + ' m²';
    return '<details class="rs-parcela" data-nombre="' + escAttr(p.nombre) + '" data-modo="' +
      (enHa ? 'ha' : 'm2') + '" data-ha="' + fmtHa(tp) + '" data-m2="' + fmtM2p(tp) +
      '" data-pc="' + fmtPc(tp / total * 100) + '">' +
      '<summary><span class="rs-pn">' + escHtml(p.nombre) + '</span><span class="rs-pa">' + resumen +
      '</span></summary><div class="rs-pcuerpo">' +
      tablaRangos('NDVI', 'Vigor vegetativo', 't-verde', p.ndvi, tp, CL.NDVI, enHa, p.nombre + ' - NDVI - Vigor vegetativo') +
      tablaRangos('NDMI', 'Humedad del suelo', 't-azul', p.ndmi, tp, CL.NDMI, enHa, p.nombre + ' - NDMI - Humedad del suelo') +
      '</div></details>';
  }

  function armarResultadoHTML(d, CL) {
    CL = CL || (typeof window !== 'undefined' && window.CLASES) || {};
    var aviso = function (t) { return '<div class="vc-aviso">' + t + '</div>'; };
    if (!d) { return aviso('No se pudo calcular. Revisa que el contorno no se cruce consigo mismo e inténtalo de nuevo.'); }
    if (d.error) {
      if (d.error === 'sin_imagenes') { return aviso('No hay imágenes disponibles en el periodo elegido. Amplía el rango de fechas.'); }
      if (d.error === 'grande') { return aviso('El terreno es demasiado grande (máximo 50 000 ha). Dibuja un área menor.'); }
      if (d.error === 'demasiadas') { return aviso('El archivo tiene demasiados polígonos (máximo 200).'); }
      return aviso('No se pudo calcular el área de este contorno. Dibújalo de nuevo.');
    }
    var total = d.total, parcelas = d.parcelas || [], multi = parcelas.length > 1;
    var enHa = total > 5000;
    var sub = [];
    if (enHa) { sub.push('(' + fmtM2(total) + ' m²)'); }
    if (multi) { sub.push(parcelas.length + ' polígonos'); }
    var html = '<div class="vc">' +
      '<div class="rs-total" data-nombre="Área total" data-modo="' + (enHa ? 'ha' : 'm2') +
      '" data-ha="' + fmtHa(total) + '" data-m2="' + fmtM2p(total) + '">' +
      '<div class="rs-tl">Área total:</div>' +
      '<div class="rs-n">' + (enHa ? fmtHa(total) + ' ha' : fmtM2(total) + ' m²') + '</div>' +
      (sub.length ? '<div class="rs-s">' + sub.join(' · ') + '</div>' : '') + '</div>' +
      tablaRangos('NDVI', 'Vigor vegetativo', 't-verde', d.ndvi, total, CL.NDVI, enHa,
        (multi ? 'Total de los polígonos - ' : '') + 'NDVI - Vigor vegetativo') +
      tablaRangos('NDMI', 'Humedad del suelo', 't-azul', d.ndmi, total, CL.NDMI, enHa,
        (multi ? 'Total de los polígonos - ' : '') + 'NDMI - Humedad del suelo');
    if (multi) {
      html += '<div class="rs-sec-tit">Resultados por polígono</div>';
      parcelas.forEach(function (p) { html += detalleParcela(p, total, CL); });
    }
    return html + '</div>';
  }
  // ==FIN_RESULTADOS==

  // ---- Importar un área de interés: se lee en el navegador, no se sube ni se guarda ----
  async function inflarRaw(u8) {
    if (typeof DecompressionStream === 'undefined') {
      throw new Error('Tu navegador no puede abrir archivos comprimidos. Actualiza Chrome o Edge, o importa un KML o GPKG.');
    }
    var flujo = new Blob([u8]).stream().pipeThrough(new DecompressionStream('deflate-raw'));
    return await new Response(flujo).arrayBuffer();
  }

  var avisoT = null, contArrastre = 0;

  function avisoMapa(texto, ms, error) {
    var $a = $('#aviso-mapa');
    $a.text(texto).toggleClass('error', !!error).css('display', 'block');
    clearTimeout(avisoT);
    if (ms) { avisoT = setTimeout(function () { $a.css('display', 'none'); }, ms); }
  }

  async function procesarArchivos(lista) {
    var archivos = [].slice.call(lista || []);
    if (!archivos.length) { return; }
    if (!mapa) { avisoMapa('El mapa todavía no está listo. Espera un momento.', 4000, true); return; }
    avisoMapa('Leyendo el archivo…', 0, false);
    try {
      archivos.forEach(function (f) {
        if (f.size > 40 * 1024 * 1024) { throw new Error('El archivo es muy pesado (máximo 40 MB).'); }
      });
      var items = archivos.map(function (f) {
        return { nombre: f.name, leer: function () { return f.arrayBuffer(); } };
      });
      var res = await areaDesdeItems(items, 0);
      if (!res) { throw new Error('Formato no admitido. Usa SHP (con su .prj), GPKG, KML o KMZ.'); }
      var parcelas = limpiarParcelas(res.parcelas);
      if (!parcelas.length) { throw new Error('El archivo no contiene polígonos.'); }
      if (parcelas.length > 200) {
        throw new Error('El archivo tiene ' + parcelas.length + ' polígonos; el máximo es 200. Divídelo en archivos más pequeños.');
      }
      var b = L.latLngBounds([]);
      parcelas.forEach(function (pc) {
        pc.polis.forEach(function (p) { p[0].forEach(function (c) { b.extend([c[1], c[0]]); }); });
      });
      var cen = b.getCenter();
      if (!enPeru(cen.lat, cen.lng)) {
        throw new Error('El área queda fuera de Perú. Revisa el sistema de coordenadas del archivo.');
      }
      importarParcelas(parcelas, res.nombre, res.nota);
    } catch (err) {
      avisoMapa((err && err.message) ? err.message : 'No se pudo leer el archivo.', 7000, true);
      if (window.console) { console.error('Visor: importar área', err); }
    }
  }

  // Deja las parcelas importadas como si se hubieran dibujado (solo en esta sesión del navegador)
  function importarParcelas(parcelas, nombre, nota) {
    if (modoConsulta) { window.toggleConsulta(); }
    if (dibujando) { detenerDibujo(true); }
    borrarPoligonoInterno();
    if (marcador) { mapa.removeLayer(marcador); marcador = null; }
    cerrarVentana();
    cerrarCoord();
    var n = parcelas.length, grupoP = L.featureGroup();
    parcelas.forEach(function (pc, i) {
      var anidado = pc.polis.map(function (p) {
        return p.map(function (a) { return a.map(function (c) { return [c[1], c[0]]; }); });
      });
      var capa = L.polygon(anidado, ESTILO_POLI);
      if (n > 1 && n <= 100) {
        capa.bindTooltip(pc.auto ? pc.num : pc.nombre.slice(0, 14),
          { permanent: true, direction: 'center', className: 'etq-parcela' });
      }
      grupoP.addLayer(capa);
    });
    publicarArea(grupoP, JSON.stringify({ tipo: 'parcelas', parcelas: parcelas.map(function (pc) {
      return { nombre: pc.nombre, coordinates: pc.polis };
    }) }));
    mapa.fitBounds(grupoP.getBounds(), { paddingTopLeft: [350, 40],
      paddingBottomRight: [40, 40], maxZoom: 17 });
    avisoMapa('Área importada: ' + nombre + ' (' + n + (n === 1 ? ' polígono)' : ' polígonos)') +
      (nota || ''), 7000, false);
  }

  function hayArchivos(e) {
    var t = e.dataTransfer && e.dataTransfer.types;
    return !!t && [].indexOf.call(t, 'Files') >= 0;
  }
  document.addEventListener('dragenter', function (e) {
    if (!hayArchivos(e)) { return; }
    e.preventDefault();
    contArrastre += 1;
    $('#zona-soltar').css('display', 'flex');
  });
  document.addEventListener('dragover', function (e) {
    if (!hayArchivos(e)) { return; }
    e.preventDefault();
    e.dataTransfer.dropEffect = 'copy';
  });
  document.addEventListener('dragleave', function (e) {
    if (!hayArchivos(e)) { return; }
    contArrastre = Math.max(0, contArrastre - 1);
    if (!contArrastre) { $('#zona-soltar').css('display', 'none'); }
  });
  document.addEventListener('drop', function (e) {
    if (!hayArchivos(e)) { return; }
    e.preventDefault();
    contArrastre = 0;
    $('#zona-soltar').css('display', 'none');
    procesarArchivos(e.dataTransfer.files);
  });
  $('#archivo-aoi').on('change', function () { procesarArchivos(this.files); });

  // ---- Dibujo del terreno y cálculo de áreas ----
  var puntos = [], capaDib = null, rubber = null, polyFinal = null, idPoli = 0;

  function actualizarAyuda(msg) {
    $('#dib-cont').text(puntos.length ? ('Vértices: ' + puntos.length) : '');
    $('#dib-msg').text(msg || '');
  }

  function redibujarBorrador() {
    if (!capaDib) { capaDib = L.layerGroup().addTo(mapa); }
    capaDib.clearLayers();
    if (puntos.length >= 3) {
      L.polygon(puntos, { color: '#fdc50f', weight: 3, fillColor: '#fdc50f',
        fillOpacity: 0.15, interactive: false }).addTo(capaDib);
    } else if (puntos.length === 2) {
      L.polyline(puntos, { color: '#fdc50f', weight: 3, interactive: false }).addTo(capaDib);
    }
    puntos.forEach(function (p, i) {
      L.circleMarker(p, { radius: i === 0 ? 7 : 5, color: '#083b6b', weight: 2,
        fillColor: i === 0 ? '#fdc50f' : '#ffffff', fillOpacity: 1,
        interactive: false }).addTo(capaDib);
    });
  }

  function alClicDibujo(e) {
    if (!dibujando) { return; }
    var pc = mapa.latLngToContainerPoint(e.latlng);
    // cerrar el terreno al pulsar sobre el primer punto
    if (puntos.length >= 3 &&
        mapa.latLngToContainerPoint(puntos[0]).distanceTo(pc) <= 10) {
      window.terminarDibujo();
      return;
    }
    // ignora el segundo clic de un doble clic
    if (puntos.length &&
        mapa.latLngToContainerPoint(puntos[puntos.length - 1]).distanceTo(pc) < 3) {
      return;
    }
    puntos.push(e.latlng);
    redibujarBorrador();
    actualizarAyuda('');
  }

  function alMoverDibujo(e) {
    if (!dibujando || !rubber || !puntos.length) { return; }
    rubber.setLatLngs([puntos[puntos.length - 1], e.latlng]);
  }

  function borrarPoligonoInterno() {
    if (polyFinal && mapa) { mapa.removeLayer(polyFinal); }
    polyFinal = null;
    render();
    $('#panel-result').css('display', 'none');
    idPoli += 1;
    Shiny.setInputValue('poligono_borrar', Math.random(), { priority: 'event' });
  }

  var ESTILO_POLI = { color: '#fdc50f', weight: 3, fillColor: '#fdc50f',
    fillOpacity: 0.08, interactive: false };

  // Muestra el área en el mapa y pide al servidor el cálculo de superficies
  function publicarArea(capa, geomJson) {
    if (polyFinal) { mapa.removeLayer(polyFinal); }
    polyFinal = capa.addTo(mapa);
    render();
    idPoli += 1;
    $('#pr-cuerpo').html('<div class="vc-aviso">Calculando áreas…</div>');
    $('#panel-result').css('display', 'flex');
    Shiny.setInputValue('poligono', { coords: geomJson, id: idPoli }, { priority: 'event' });
  }

  function mostrarPoligono(ll) {
    var coords = ll.map(function (p) { return [p.lng, p.lat]; });
    publicarArea(L.polygon(ll, ESTILO_POLI), JSON.stringify(coords));
  }

  function iniciarDibujo() {
    if (!mapa) { return; }
    if (modoConsulta) { window.toggleConsulta(); }
    borrarPoligonoInterno();
    if (marcador) { mapa.removeLayer(marcador); marcador = null; }
    cerrarVentana();
    cerrarCoord();
    dibujando = true;
    render();
    actualizarBoton();
    puntos = [];
    rubber = L.polyline([], { color: '#fdc50f', weight: 2, dashArray: '6 6',
      interactive: false }).addTo(mapa);
    mapa.doubleClickZoom.disable();
    $('#mapa-wrap').addClass('modo-dibujo');
    $('#dibujo-ayuda').css('display', 'flex');
    actualizarAyuda('');
  }

  function detenerDibujo(cancelar) {
    dibujando = false;
    if (rubber && mapa) { mapa.removeLayer(rubber); }
    rubber = null;
    if (capaDib) { capaDib.clearLayers(); }
    if (cancelar) { puntos = []; }
    if (mapa) { mapa.doubleClickZoom.enable(); }
    $('#mapa-wrap').removeClass('modo-dibujo');
    $('#dibujo-ayuda').css('display', 'none');
    render();
    actualizarBoton();
  }

  window.toggleDibujo = function () {
    if (dibujando) { detenerDibujo(true); } else { iniciarDibujo(); }
  };

  window.terminarDibujo = function () {
    if (!dibujando) { return; }
    if (puntos.length < 3) {
      actualizarAyuda('Marca al menos 3 puntos para cerrar el terreno.');
      return;
    }
    var ll = puntos.slice();
    detenerDibujo(false);
    mostrarPoligono(ll);
  };

  window.deshacerPunto = function () {
    if (!dibujando || !puntos.length) { return; }
    puntos.pop();
    redibujarBorrador();
    if (rubber) { rubber.setLatLngs([]); }
    actualizarAyuda('');
  };

  window.cancelarDibujo = function () { detenerDibujo(true); };
  window.borrarPoligono = borrarPoligonoInterno;

  // Escala gráfica segmentada: 0 - D/4 - D/2 - D
  function num(x) { return String(Math.round(x * 100) / 100); }

  function agregarEscala() {
    var Escala = L.Control.extend({
      options: { position: 'bottomleft' },
      onAdd: function (map) {
        this._map = map;
        var div = L.DomUtil.create('div', 'escala-ctrl');
        div.innerHTML = '<div class="escala-lab"></div>' +
          '<div class="escala-barra"><i></i><i></i><i></i></div>';
        this._lab = div.querySelector('.escala-lab');
        this._bar = div.querySelector('.escala-barra');
        L.DomEvent.disableClickPropagation(div);
        map.on('moveend zoomend resize', this._act, this);
        this._act();
        return div;
      },
      onRemove: function (map) {
        map.off('moveend zoomend resize', this._act, this);
      },
      _act: function () {
        var map = this._map, maxPx = 140;
        var movil = window.matchMedia('(max-width: 820px)').matches;
        // PC y celular: el recuadro blanco se ajusta a la barra (CSS: width:max-content),
        // así nada se sale del fondo ni quedan espacios vacíos
        maxPx = movil ? 110 : 180;
        var tam = map.getSize();
        if (!tam.x || !tam.y) { return; }
        var y = tam.y / 2;
        var d = map.containerPointToLatLng([10, y])
                   .distanceTo(map.containerPointToLatLng([10 + maxPx, y]));
        if (!isFinite(d) || d <= 0) { return; }
        var base = [1, 1.2, 1.6, 2, 2.4, 3, 4, 5, 6, 8], D = 1, k, i, c;
        for (k = 0; k < 9; k++) {
          for (i = 0; i < base.length; i++) {
            c = Math.round(base[i] * Math.pow(10, k) * 1e6) / 1e6;
            if (c <= d) { D = c; }
          }
        }
        var px = Math.round(D / d * maxPx);
        var f = D >= 1000 ? 1000 : 1, u = D >= 1000 ? 'km' : 'm';
        this._lab.innerHTML =
          '<span style="left:0">0</span>' +
          '<span style="left:25%;transform:translateX(-50%)">' + num(D / 4 / f) + '</span>' +
          '<span style="left:50%;transform:translateX(-50%)">' + num(D / 2 / f) + '</span>' +
          '<span style="left:100%;transform:translateX(-100%)">' + num(D / f) + ' ' + u + '</span>';
        this._lab.style.width = px + 'px';
        this._bar.style.width = px + 'px';
      }
    });
    new Escala().addTo(mapa);
  }

  // Minimapa de ubicación (Perú + área de estudio + vista actual / área elegida)
  var mini = null, miniRect = null, miniSel = null, boundsDZ3 = null;

  function actualizarMini() {
    if (!mini || !mapa || !DATA) { return; }
    var tam = mapa.getSize();
    if (miniSel) { mini.removeLayer(miniSel); miniSel = null; }

    // Área elegida (departamento, provincia o distrito) pintada de amarillo
    var d = estado.dep, p = estado.prov, s = estado.dist, sel = null;
    var opc = { interactive: false, style: {
      color: '#b58900', weight: 1.5, fillColor: '#fdc50f', fillOpacity: 1 } };
    if (s) { opc.filter = igual('IDDIST', s); sel = L.geoJSON(DATA.dist, opc); }
    else if (p) { opc.filter = igual('IDPROV', p); sel = L.geoJSON(DATA.prov, opc); }
    else if (d) { opc.filter = igual('IDDPTO', d); sel = L.geoJSON(DATA.dep, opc); }
    if (sel) { miniSel = sel.addTo(mini); }

    // Cuadro amarillo de la vista actual: solo aparece cuando el mapa ya está
    // acercado (al menos 1 nivel de zoom sobre la vista de todo el Perú).
    // En la vista alejada no se muestra, aunque se mueva el mapa arriba/abajo.
    var mostrar = false, vb = mapa.getBounds();
    if (!sel && tam.x && tam.y) {
      mostrar = mapa.getZoom() >= mapa.getMinZoom() + 0.5;
    }
    if (mostrar) {
      miniRect.setBounds(vb);
      if (!mini.hasLayer(miniRect)) { miniRect.addTo(mini); }
    } else if (mini.hasLayer(miniRect)) {
      mini.removeLayer(miniRect);
    }
  }

  function construirMini() {
    if (mini || !mapa || !DATA || !PERU || !window.L) { return; }
    var el = document.getElementById('minimapa');
    if (!el) { return; }
    try {
      mini = L.map(el, {
        zoomControl: false, attributionControl: false, dragging: false,
        scrollWheelZoom: false, doubleClickZoom: false, boxZoom: false,
        keyboard: false, touchZoom: false, zoomSnap: 0, zoomDelta: 0
      });
      mini.fitBounds(PERU, { animate: false, padding: [4, 4] });
      if (DATA.per) {
        L.geoJSON(DATA.per, { interactive: false, style: {
          color: '#9aa3ad', weight: 0.6, fillColor: '#f1f3f5', fillOpacity: 1 } }).addTo(mini);
      }
      L.geoJSON(DATA.dep, { interactive: false, style: {
        color: '#0b5394', weight: 1, fillColor: '#0b5394', fillOpacity: 0.55 } }).addTo(mini);
      boundsDZ3 = L.geoJSON(DATA.dep).getBounds();
      miniRect = L.rectangle(mapa.getBounds(), {
        color: '#fdc50f', weight: 2, fill: false, interactive: false });
      mapa.on('moveend', actualizarMini);
      mapa.on('zoomend', actualizarMini);
      setTimeout(function () {
        mini.invalidateSize();
        mini.fitBounds(PERU, { animate: false, padding: [4, 4] });
        actualizarMini();
      }, 300);
      actualizarMini();
    } catch (e) {
      if (window.console) { console.error('Visor: error en el minimapa', e); }
    }
  }

  // ---- iPhone/iPad (Safari): zoom con dos dedos propio ----
  // El Leaflet que trae el paquete de R es antiguo y no maneja bien los gestos de iOS reciente
  // (en Android sí funciona). Solo en iOS se desactiva el pellizco de Leaflet y se resuelve aquí
  // con los eventos táctiles nativos. En PC y Android no cambia nada.
  function pellizcoIOS() {
    var esIOS = /iP(hone|ad|od)/.test(navigator.userAgent) ||
                (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
    if (!esIOS || !mapa) { return; }
    try { mapa.touchZoom.disable(); } catch (e0) {}
    var c = mapa.getContainer(), ini = null, raf = 0, pend = null;
    function dist(t) {
      var dx = t[0].clientX - t[1].clientX, dy = t[0].clientY - t[1].clientY;
      return Math.sqrt(dx * dx + dy * dy);
    }
    function centro(t) {
      var r = c.getBoundingClientRect();
      return L.point((t[0].clientX + t[1].clientX) / 2 - r.left, (t[0].clientY + t[1].clientY) / 2 - r.top);
    }
    function aplicar() {
      raf = 0;
      if (!pend || !ini) { return; }
      var p = pend;
      pend = null;
      try {
        mapa._animatingZoom = false;   // por si un zoom anterior quedó a medias
        // 1) el mapa sigue el movimiento de los dedos; 2) se acerca/aleja alrededor de ellos
        var dx = ini.cen.x - p.cen.x, dy = ini.cen.y - p.cen.y;
        if (dx || dy) { mapa.panBy([dx, dy], { animate: false }); }
        mapa.setZoomAround(p.cen, p.z, { animate: false });
        ini.cen = p.cen;
      } catch (err) {
        if (window.console) { console.error('Visor: pellizco', err); }
      }
    }
    c.addEventListener('touchstart', function (e) {
      if (e.touches.length !== 2) { return; }
      ini = { d: dist(e.touches), z: mapa.getZoom(), cen: centro(e.touches) };
      pellizcando = true;
      if (sombraCanvas) { sombraCanvas.style.opacity = '0'; }
      e.preventDefault();
    }, { passive: false });
    c.addEventListener('touchmove', function (e) {
      if (!ini || e.touches.length !== 2 || !ini.d) { return; }
      e.preventDefault();
      pend = { cen: centro(e.touches), z: ini.z + Math.log(dist(e.touches) / ini.d) / Math.LN2 };
      if (!raf) { raf = requestAnimationFrame(aplicar); }
    }, { passive: false });
    function fin(e) {
      if (!ini || (e.touches && e.touches.length >= 2)) { return; }
      ini = null; pend = null; pellizcando = false;
      if (raf) { cancelAnimationFrame(raf); raf = 0; }
      sombraProgramar();
    }
    c.addEventListener('touchend', fin, { passive: true });
    c.addEventListener('touchcancel', fin, { passive: true });
  }

  function iniciarInterno(m) {
    mapa = m;
    diagEstado.mapa = 'sí';
    diag();
    PERU = L.latLngBounds([-18.6, -81.6], [0.2, -68.4]);
    grupo = L.layerGroup().addTo(mapa);
    agregarEscala();
    ajustarVistaInicial(false);
    setTimeout(function () { if (!estado.dep) { ajustarVistaInicial(false); } }, 500);
    setTimeout(function () { ajustarMinZoom(); }, 1500);
    mapa.on('click', alClicMapa);
    mapa.on('click', alClicDibujo);
    pellizcoIOS();
    // Al girar la rueda o hacer zoom, el relleno azul desaparece
    mapa.getContainer().addEventListener('wheel', quitarRelleno, { passive: true });
    mapa.on('zoomstart', quitarRelleno);
    mapa.on('mousemove', alMoverDibujo);
    mapa.on('dblclick', function () { if (dibujando) { window.terminarDibujo(); } });
    mapa.on('move zoomend', reubicarVentana);
    iniciarGoogle();
    mapa.on('locationfound', function (e) {
      if (ubic) { mapa.removeLayer(ubic); }
      ubic = L.circleMarker(e.latlng, {
        radius: 7, color: '#ffffff', weight: 2, fillColor: '#1976d2', fillOpacity: 1
      }).addTo(mapa);
      try { ubicarEnFiltros(e.latlng); } catch (err) {
        if (window.console) { console.error('Visor: no se pudo ubicar en los filtros', err); }
      }
    });
    mapa.on('locationerror', function () {
      alert('No se pudo obtener tu ubicación.');
    });
    window.addEventListener('resize', function () {
      mapa.invalidateSize();
      ajustarMinZoom();
    });
    var interactuo = false;
    ['mousedown', 'wheel', 'touchstart'].forEach(function (ev) {
      mapa.getContainer().addEventListener(ev, function () { interactuo = true; },
        { passive: true });
    });
    var contenedor = document.getElementById('mapa-wrap');
    if (window.ResizeObserver && contenedor) {
      new ResizeObserver(function () {
        if (!mapa) { return; }
        mapa.invalidateSize({ animate: false });
        ajustarMinZoom();
        if (!interactuo && !estado.dep) { ajustarVistaInicial(false); }
      }).observe(contenedor);
    }
    intentar();
  }

  function iniciar(m) {
    try {
      iniciarInterno(m);
    } catch (e) {
      if (window.console) { console.error('Visor: error al iniciar el mapa', e); }
    } finally {
      iniciado = true;
      if (window.console) { console.log('Visor: mapa iniciado'); }
      var pendientes = cola;
      cola = [];
      pendientes.forEach(function (f) {
        try { f(); } catch (e2) {
          if (window.console) { console.error('Visor: error en tarea pendiente', e2); }
        }
      });
    }
  }

  window.toggleConsulta = function () {
    if (dibujando) { detenerDibujo(true); }
    modoConsulta = !modoConsulta;
    if (modoConsulta) { cerrarCoord(); }
    $('#mapa-wrap').toggleClass('modo-consulta', modoConsulta);
    if (!modoConsulta && marcador && mapa) {
      mapa.removeLayer(marcador);
      marcador = null;
    }
    if (!modoConsulta) { cerrarVentana(); }
    render();
    actualizarBoton();
    Shiny.setInputValue('modo_consulta', modoConsulta);
  };

  var ICO_EXPANDIR = '<svg viewBox="0 0 24 24" width="18" height="18"><path fill="currentColor" d="M3 3h6v2H5v4H3V3zm12 0h6v6h-2V5h-4V3zM3 15h2v4h4v2H3v-6zm16 0h2v6h-6v-2h4v-4z"/></svg>';
  var ICO_CONTRAER = '<svg viewBox="0 0 24 24" width="18" height="18"><path fill="currentColor" d="M8 3h2v6H4V7h4V3zm6 0h2v4h4v2h-6V3zM4 15h6v6H8v-4H4v-2zm10 0h6v2h-4v4h-2v-6z"/></svg>';
  function estadoPantalla() {
    var b = document.getElementById('btn_pantalla');
    var activa = !!(document.fullscreenElement || document.webkitFullscreenElement ||
                    document.body.classList.contains('pseudo-completa'));
    if (b) {
      b.innerHTML = activa ? ICO_CONTRAER : ICO_EXPANDIR;
      b.title = activa ? 'Salir de pantalla completa' : 'Pantalla completa';
    }
    if (mapa) {
      setTimeout(function () {
        mapa.invalidateSize();
        if (mini) { mini.invalidateSize(); mini.fitBounds(PERU, { animate: false, padding: [4, 4] }); actualizarMini(); }
      }, 250);
    }
  }
  window.pantallaCompleta = function () {
    var el = document.getElementById('mapa-wrap');
    var movil = window.matchMedia('(max-width: 820px)').matches;
    var pedir = el.requestFullscreen || el.webkitRequestFullscreen;
    var salir = document.exitFullscreen || document.webkitExitFullscreen;
    var real = document.fullscreenElement || document.webkitFullscreenElement;
    if (real) { if (salir) { salir.call(document); } return; }
    if (movil || !pedir || !(document.fullscreenEnabled || document.webkitFullscreenEnabled)) {
      // Celular: el mapa cubre toda la ventana (así el navegador no muestra su aviso de pantalla completa)
      document.body.classList.toggle('pseudo-completa');
      estadoPantalla();
    } else {
      pedir.call(el);
    }
  };
  ['fullscreenchange', 'webkitfullscreenchange'].forEach(function (ev) {
    document.addEventListener(ev, estadoPantalla);
  });

  // ---- Mi ubicación: busca el departamento, provincia y distrito que contienen el punto ----
  function puntoEnGeom(lng, lat, g) {
    var polis = g.type === 'Polygon' ? [g.coordinates] : (g.type === 'MultiPolygon' ? g.coordinates : []);
    var i, k, hueco;
    for (i = 0; i < polis.length; i++) {
      if (!puntoEnAnillo([lng, lat], polis[i][0])) { continue; }
      hueco = false;
      for (k = 1; k < polis[i].length; k++) {
        if (puntoEnAnillo([lng, lat], polis[i][k])) { hueco = true; break; }
      }
      if (!hueco) { return true; }
    }
    return false;
  }
  function unidadEn(gj, lng, lat) {
    var i, f;
    for (i = 0; i < gj.features.length; i++) {
      f = gj.features[i];
      if (f.geometry && puntoEnGeom(lng, lat, f.geometry)) { return f.properties; }
    }
    return null;
  }
  // Cambia los filtros al lugar donde está la persona SIN mover el mapa (el mapa ya fue a su ubicación)
  function ubicarEnFiltros(ll) {
    if (!DATA || !DATA.dist) { return; }
    var u = unidadEn(DATA.dist, ll.lng, ll.lat);
    if (u) {
      estado = { dep: String(u.IDDPTO), prov: String(u.IDPROV), dist: String(u.IDDIST) };
    } else {
      estado = { dep: '', prov: '', dist: '' };
      avisoMapa('Tu ubicación está fuera de la zona de trabajo (DZ3).', 5000, false);
    }
    sincronizarListas(); render(); construirMini(); actualizarMini();
  }

  window.miUbicacion = function () {
    if (mapa) { mapa.locate({ setView: true, maxZoom: 13 }); }
  };

  // IMPORTANTE: Shiny exige que cada handler reciba exactamente UN argumento.
  function registrar() {
    Shiny.addCustomMessageHandler('cargar_limites', function (m) {
      DATA = {
        dep: JSON.parse(m.dep),
        prov: JSON.parse(m.prov),
        dist: JSON.parse(m.dist),
        per: m.per ? JSON.parse(m.per) : null,
        pais: (DATA && DATA.pais) ? DATA.pais : null
      };
      diagEstado.limites = 'recibidos';
      diag();
      intentar();
    });

    Shiny.addCustomMessageHandler('cargar_pais', function (m) {
      if (!DATA) { DATA = {}; }
      DATA.pais = m.pais ? soloContornoPais(JSON.parse(m.pais)) : null;
      if (DATA.dep) { render(); }
    });

    Shiny.addCustomMessageHandler('estado_mapa', function (m) {
      estado = { dep: m.dep || '', prov: m.prov || '', dist: m.dist || '' };
      intentar();
    });

    Shiny.addCustomMessageHandler('capa_gee', function (m) {
      cuandoListo(function () {
        if (capaGee) { mapa.removeLayer(capaGee); capaGee = null; }
        if (!m.url) { diagEstado.capa = 'sin imágenes'; diag(); ocultarCarga(); return; }
        // Ahorro de cuota de Earth Engine: cada mosaico pedido cuenta como cómputo.
        //  - bounds: no se piden mosaicos fuera de la zona de trabajo (DZ3)
        //  - updateWhenIdle / updateWhenZooming / keepBuffer: no se piden mosaicos mientras se arrastra
        //    o se anima el zoom, y se guarda menos margen fuera de pantalla
        var opcCapa = {
          opacity: m.opacidad, zIndex: 10, maxZoom: 22,
          updateWhenIdle: true, updateWhenZooming: false, keepBuffer: 1, attribution: ATRIB
        };
        try {
          if (!limitesAOI && DATA && DATA.dep) { limitesAOI = L.geoJSON(DATA.dep).getBounds().pad(0.05); }
          if (limitesAOI && limitesAOI.isValid()) { opcCapa.bounds = limitesAOI; }
        } catch (e) { /* si falla, la capa funciona igual, solo sin límite de zona */ }
        capaGee = L.tileLayer(m.url, opcCapa);
        capaGee.on('loading', mostrarCarga);
        capaGee.on('load', ocultarCarga);
        capaGee.addTo(mapa);
        diagEstado.capa = 'sí';
        diag();
      });
    });

    Shiny.addCustomMessageHandler('opacidad', function (v) {
      if (capaGee) { capaGee.setOpacity(v); }
    });

    Shiny.addCustomMessageHandler('habilitar', function (m) {
      $('#' + m.id).prop('disabled', !m.activo);
    });

    Shiny.addCustomMessageHandler('poligono_resultado', function (m) {
      if (m.id !== idPoli) { return; }
      var d = null;
      try { d = m.datos ? JSON.parse(m.datos) : null; } catch (err) { d = null; }
      $('#pr-cuerpo').html(armarResultadoHTML(d));
    });

    Shiny.addCustomMessageHandler('popup_valor', function (m) {
      if (marcador && marcador._idc === m.id) {
        mostrarVentana(m.html, marcador.getLatLng());
      }
    });

    // CORREGIDO: antes era function () {...} (sin argumento) y Shiny lanzaba
    // "handler must be a function that takes one argument".
    Shiny.addCustomMessageHandler('vista_inicial', function (m) {
      if (marcador && mapa) { mapa.removeLayer(marcador); marcador = null; }
      ajustarVistaInicial(true);
    });

  }

  // Primero se busca el mapa (no depende de que los handlers se registren bien)
  $('#sel_prov, #sel_dist').prop('disabled', true);
  diag();
  engancharLeaflet();
  conMapa(iniciar);

  // Después se registran los handlers de Shiny
  function registrarSeguro() {
    try {
      registrar();
    } catch (e) {
      if (window.console) { console.error('Visor: error al registrar handlers', e); }
    }
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    registrarSeguro();
  } else {
    $(document).on('shiny:connected', registrarSeguro);
  }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', arrancar);
  } else {
    arrancar();
  }
})();
)---"

# -----------------------------------------------------------------------------
# Móvil: botón ☰ que abre/cierra el menú lateral, mide la altura de los filtros,
# lista desplegable propia (oscura y translúcida) y calendario sin teclado
# -----------------------------------------------------------------------------
JS_MOVIL <- r"---(
(function () {
  function esMovil() { return window.matchMedia('(max-width: 820px)').matches; }

  function init() {
    var cab = document.querySelector('.cabecera'), panel = document.querySelector('.panel-lateral');
    if (!cab || !panel) { setTimeout(init, 200); return; }
    var b = document.createElement('button');
    b.type = 'button'; b.id = 'btn-menu'; b.className = 'btn-menu';
    b.setAttribute('aria-label', 'Menú');
    b.innerHTML = '<span></span><span></span><span></span>';
    cab.appendChild(b);
    var v = document.createElement('div'); v.className = 'velo'; document.body.appendChild(v);
    function abrir(a) { document.body.classList.toggle('menu-abierto', a); }
    b.addEventListener('click', function () {
      abrir(!document.body.classList.contains('menu-abierto'));
    });
    v.addEventListener('click', function () { abrir(false); });
    panel.addEventListener('change', function (e) {
      if (e.target && e.target.name === 'capa') { setTimeout(function () { abrir(false); }, 250); }
    });
    var f = document.querySelector('.filtros');
    function medir() {
      if (f) { document.documentElement.style.setProperty('--fh', f.offsetHeight + 'px'); }
    }
    medir(); setTimeout(medir, 800);
    window.addEventListener('resize', medir);
    if (window.ResizeObserver && f) { new ResizeObserver(medir).observe(f); }

    // ---- Lista desplegable propia (fondo oscuro translúcido, con check) ----
    // Se usa en celular y también en PC (en PC toma la transparencia de la pastilla de filtros)
    var menuSel = document.createElement('div');
    menuSel.className = 'menu-sel';
    document.body.appendChild(menuSel);
    var abiertoPara = null, tAbierto = 0;

    function cerrarSel() { menuSel.style.display = 'none'; abiertoPara = null; }

    function abrirSel(sel) {
      if (sel.disabled) { return; }
      var r = sel.getBoundingClientRect();
      menuSel.innerHTML = '';
      [].forEach.call(sel.options, function (o) {
        var d = document.createElement('div');
        d.textContent = o.text;
        if (o.value === sel.value) { d.className = 'sel'; }
        d.addEventListener('click', function (e) {
          e.stopPropagation();
          sel.value = o.value;
          if (window.jQuery) { window.jQuery(sel).trigger('change'); }
          else { sel.dispatchEvent(new Event('change', { bubbles: true })); }
          cerrarSel();
        });
        menuSel.appendChild(d);
      });
      menuSel.style.minWidth = Math.max(r.width, 150) + 'px';
      menuSel.style.columnCount = '';
      menuSel.style.maxHeight = '';
      menuSel.style.display = 'block';
      if (esMovil()) {
        // celular: lista con desplazamiento (sin cambios)
        menuSel.style.maxHeight = Math.max(120, Math.min(window.innerHeight * 0.46,
          window.innerHeight - r.bottom - 16)) + 'px';
      } else {
        // PC: todas las opciones en una sola ventana, sin barra de desplazamiento.
        // Si no caben en una columna, se reparten en varias columnas.
        var disp = Math.max(160, window.innerHeight - r.bottom - 20);
        var alto = menuSel.scrollHeight;
        if (alto > disp) {
          var cols = Math.min(4, Math.ceil(alto / disp));
          menuSel.style.columnCount = String(cols);
          menuSel.style.columnGap = '0';
        }
      }
      var left = Math.min(r.left, window.innerWidth - menuSel.offsetWidth - 8);
      menuSel.style.left = Math.max(8, left) + 'px';
      menuSel.style.top = (r.bottom + 4) + 'px';
      abiertoPara = sel;
      tAbierto = Date.now();
    }

    function interceptar(e) {
      if (!e.target || !e.target.closest) { return; }
      var sel = e.target.closest('.filtros select');
      if (!sel) { return; }
      e.preventDefault();
      if (abiertoPara === sel) {
        if (Date.now() - tAbierto > 500) { cerrarSel(); }
        return;
      }
      abrirSel(sel);
    }
    document.addEventListener('touchstart', interceptar, { passive: false });
    document.addEventListener('mousedown', interceptar);
    document.addEventListener('click', function (e) {
      if (!e.target.closest) { return; }
      if (!e.target.closest('.menu-sel') && !e.target.closest('.filtros select')) { cerrarSel(); }
    });
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Escape' && abiertoPara) { cerrarSel(); }
    });
    window.addEventListener('resize', cerrarSel);

    // ---- La leyenda se minimiza al tocar cualquier parte fuera de ella (mapa incluido) ----
    document.addEventListener('click', function (e) {
      var l = document.querySelector('.ley-movil');
      if (l && l.open && e.target && e.target.closest && !e.target.closest('.ley-movil')) { l.open = false; }
    }, true);

    // ---- Calendario: en celular no se abre el teclado al tocar las fechas ----
    function fechasSinTeclado() {
      if (!esMovil()) { return; }
      [].forEach.call(document.querySelectorAll('.filtros .input-daterange input'), function (i) {
        i.readOnly = true;
        i.setAttribute('inputmode', 'none');
      });
    }
    fechasSinTeclado(); setTimeout(fechasSinTeclado, 600); setTimeout(fechasSinTeclado, 1800);
    window.addEventListener('resize', fechasSinTeclado);
  }
  if (document.readyState === 'loading') { document.addEventListener('DOMContentLoaded', init); }
  else { init(); }
})();
)---"

# -----------------------------------------------------------------------------
# Interfaz
# -----------------------------------------------------------------------------
ICONO_PANTALLA <- '<svg viewBox="0 0 24 24" width="18" height="18"><path fill="currentColor" d="M3 3h6v2H5v4H3V3zm12 0h6v6h-2V5h-4V3zM3 15h2v4h4v2H3v-6zm16 0h2v6h-6v-2h4v-4z"/></svg>'
ICONO_UBICACION <- '<svg viewBox="0 0 24 24" width="18" height="18"><circle cx="12" cy="12" r="3.5" fill="currentColor"/><circle cx="12" cy="12" r="7.5" stroke="currentColor" stroke-width="2" fill="none"/><path d="M12 1.5v4M12 18.5v4M1.5 12h4M18.5 12h4" stroke="currentColor" stroke-width="2" fill="none"/></svg>'
ICONO_CONSULTA <- '<svg viewBox="0 0 24 24" width="16" height="16"><path fill="currentColor" d="M12 2a7 7 0 0 0-7 7c0 5 7 13 7 13s7-8 7-13a7 7 0 0 0-7-7zm0 9.5A2.5 2.5 0 1 1 12 6.5a2.5 2.5 0 0 1 0 5z"/></svg>'

ICONO_NORTE <- '<svg viewBox="0 0 24 24" width="16" height="16"><path fill="currentColor" d="M12 2l6 18-6-4-6 4z"/></svg><span>N</span>'

ICONO_POLIGONO <- '<svg viewBox="0 0 24 24" width="16" height="16"><path d="M5 8l7-4 7 5-2 10-10 1z" fill="none" stroke="currentColor" stroke-width="2" stroke-linejoin="round"/><circle cx="5" cy="8" r="2" fill="currentColor"/><circle cx="12" cy="4" r="2" fill="currentColor"/><circle cx="19" cy="9" r="2" fill="currentColor"/><circle cx="17" cy="19" r="2" fill="currentColor"/><circle cx="7" cy="20" r="2" fill="currentColor"/></svg>'

ICONO_COORD <- '<svg viewBox="0 0 24 24" width="16" height="16"><path d="M3 8h18M3 16h18M8 3v18M16 3v18" stroke="currentColor" stroke-width="1.6" fill="none"/><circle cx="16" cy="8" r="3" fill="currentColor" stroke="#fff" stroke-width="1.2"/></svg>'

ICONO_IMPORTAR <- '<svg viewBox="0 0 24 24" width="16" height="16"><path d="M12 15V4M7.5 8.5 12 4l4.5 4.5M4 15v4h16v-4" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>'

desactivar <- function(x) {
  tryCatch(
    htmltools::tagQuery(x)$find("select")$addAttrs(disabled = "disabled")$allTags(),
    error = function(e) x)
}


# -----------------------------------------------------------------------------
# Pantalla de carga (se adapta al modo claro/oscuro del usuario)
# -----------------------------------------------------------------------------
LOGO_OSC <- "logo_oscuro.png"   # se sirven desde www/ (el navegador los descarga aparte y los guarda en caché)
LOGO_CLA <- "logo_claro.png"

SPLASH_HTML <- paste0(
  '<div id="splash"><div class="sp-glow"></div>',
  '<img class="sp-img sp-osc" src="', LOGO_OSC, '" alt="SENAMHI">',
  '<img class="sp-img sp-cla" src="', LOGO_CLA, '" alt="SENAMHI">',
  '<div class="sp-sub">VISOR DE ÍNDICES ESPECTRALES · DZ3</div>',
  '<div class="sp-barra"><i id="sp-bar"></i></div>',
  '<div class="sp-txt"><span>CARGANDO</span><b class="sp-pts"><i>.</i><i>.</i><i>.</i></b></div>',
  '</div>')

SPLASH_CSS <- r"---(
#splash {position:fixed; inset:0; z-index:99999; display:flex; flex-direction:column;
  align-items:center; justify-content:center; background:#0f1722; color:#fff;
  font-family:"Roboto", "Helvetica Neue", Arial, sans-serif;
  transition:opacity .6s ease, visibility .6s ease;}
#splash * {font-family:inherit;}
#splash.fuera {opacity:0; visibility:hidden; pointer-events:none;}
.sp-glow {position:absolute; inset:0; pointer-events:none;
  background:radial-gradient(circle at 50% 42%, rgba(253,197,15,.16), transparent 52%);
  animation:sp-resp 3.2s ease-in-out infinite;}
.sp-img {position:relative; width:min(230px,60vw); height:auto;
  animation:sp-flota 3.2s ease-in-out infinite;}
.sp-cla {display:none;}
.sp-sub {margin-top:16px; font-size:12px; letter-spacing:2.5px; font-weight:700; color:#b9c8d6;
  text-align:center; padding:0 16px;}
.sp-barra {margin-top:22px; width:min(240px,62vw); height:4px; border-radius:4px; overflow:hidden;
  background:rgba(255,255,255,.14);}
.sp-barra i {display:block; position:relative; height:100%; width:8%; border-radius:4px;
  background:linear-gradient(90deg,#6dd5c8,#b8d68a 50%,#ffc40d); transition:width .7s ease; overflow:hidden;}
.sp-barra i:after {content:''; position:absolute; top:0; bottom:0; width:40%;
  background:linear-gradient(90deg,transparent,rgba(255,255,255,.65),transparent);
  animation:sp-brillo 1.4s ease-in-out infinite;}
.sp-txt {margin-top:16px; font-size:13px; font-weight:700; letter-spacing:2px; color:#e3eaf1;
  display:flex; align-items:baseline;}
.sp-pts {display:inline-flex;}
.sp-pts i {font-style:normal; opacity:.25; animation:sp-punto 1.4s ease-in-out infinite;}
.sp-pts i:nth-child(2) {animation-delay:.2s;}
.sp-pts i:nth-child(3) {animation-delay:.4s;}
@keyframes sp-punto {0%,100% {opacity:.25;} 40% {opacity:1;}}
@keyframes sp-flota {50% {transform:translateY(-6px);}}
@keyframes sp-resp {50% {opacity:.45;}}
@keyframes sp-brillo {from {left:-45%;} to {left:110%;}}
@media (prefers-color-scheme: light) {
  #splash {background:#eaf1f9; color:#020d1a;}
  .sp-osc {display:none;}
  .sp-cla {display:block;}
  .sp-glow {background:radial-gradient(circle at 50% 42%, rgba(11,83,148,.40), rgba(11,83,148,.16) 40%, transparent 68%);}
  .sp-sub {color:#020d1a;}
  .sp-barra {background:rgba(11,83,148,.22);}
  .sp-txt {color:#020d1a;}
}
/* Solo celular: el contenido sube un poco y el resplandor lo acompaña */
@media (max-width: 820px) {
  #splash {box-sizing:border-box; padding-bottom:calc(18vh - 76px);}
  .sp-glow {background:radial-gradient(circle at 50% 39%, rgba(253,197,15,.16), transparent 52%);}
}
@media (max-width: 820px) and (prefers-color-scheme: light) {
  .sp-glow {background:radial-gradient(circle at 50% 39%, rgba(11,83,148,.40), rgba(11,83,148,.16) 40%, transparent 68%);}
}
@media (prefers-reduced-motion: reduce) {
  .sp-img, .sp-glow, .sp-pts i {animation:none;}
  .sp-pts i {opacity:1;}
}
)---"

SPLASH_JS <- r"---(
(function () {
  var t0 = Date.now(), terminado = false;
  function el(id) { return document.getElementById(id); }

  // Solo avanza la barra; el texto siempre dice CARGANDO...
  function paso(m) {
    if (terminado) { return; }
    if (m.pct !== undefined && el('sp-bar')) { el('sp-bar').style.width = m.pct + '%'; }
  }

  window.splashFin = function () {
    if (terminado) { return; }
    terminado = true;
    if (el('sp-bar')) { el('sp-bar').style.width = '100%'; }
    var espera = Math.max(350, 1400 - (Date.now() - t0));  // se ve al menos 1.4 s
    setTimeout(function () {
      var s = el('splash');
      if (!s) { return; }
      s.classList.add('fuera');
      setTimeout(function () { if (s.parentNode) { s.parentNode.removeChild(s); } }, 900);
    }, espera);
  };

  // Seguro: si algo falla, la pantalla nunca se queda pegada
  setTimeout(function () { window.splashFin(); }, 90000);

  function registrar() {
    Shiny.addCustomMessageHandler('splash_paso', function (m) { paso(m); });
    Shiny.addCustomMessageHandler('splash_fin', function (m) { window.splashFin(); });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) { registrar(); }
  else { $(document).on('shiny:connected', registrar); }
})();
)---"




ui <- function(request) {
  hoy <- hoy_pe()
  fluidPage(
    title = "SENAMHI | Visor de Índices Espectrales",
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      # Icono de la pestaña del navegador: archivo www/favicon.png (cuadrado). El ?v=1 obliga a refrescarlo.
      tags$link(rel = "icon", type = "image/png", href = "favicon.png?v=1"),
      tags$link(rel = "apple-touch-icon", href = "favicon.png?v=1"),
      tags$style(HTML(CSS_CODIGO)),
      tags$script(HTML(paste0("window.GMAPS_KEY = ",
                              jsonlite::toJSON(Sys.getenv("GOOGLE_MAPS_API_KEY"), auto_unbox = TRUE),
                              ";"))),
      tags$script(HTML(paste0("window.CLASES = ",
                              jsonlite::toJSON(lapply(VIS, function(v) v$clases), auto_unbox = TRUE),
                              ";"))),
      tags$script(HTML(JS_CODIGO)),
      tags$script(HTML(JS_MOVIL)),
      tags$style(HTML(SPLASH_CSS)),
      tags$script(HTML(SPLASH_JS))
    ),
    HTML(SPLASH_HTML),
    div(class = "app-raiz",
        
        div(class = "cabecera",
            tags$img(src = "logos.png", style = "height:2.875rem")),
        
        div(class = "contenedor",
            
            div(class = "panel-lateral",
                div(class = "marca",
                    div(class = "marca-tit", "AGROMETEOROLOGÍA"),
                    div(class = "marca-sub", "SENAMHI - DZ3"),
                    div(class = "marca-det",
                        "Dirección Zonal 3 (Sur de Cajamarca y La Libertad)")),
                div(class = "etiqueta-temas", "TEMAS"),
                
                tags$details(open = NA,
                             tags$summary("MONITOREO"),
                             div(class = "contenido",
                                 radioButtons("capa", "Variable",
                                              choices = c("🌿 NDVI - Vigor vegetativo" = "NDVI",
                                                          "💧 NDMI - Humedad del suelo" = "NDMI"),
                                              selected = "NDVI"),
                                 uiOutput("info_periodo"))),
                
                tags$details(open = NA,
                             tags$summary("LEYENDA"),
                             div(class = "contenido", uiOutput("leyenda"))),
                
                tags$details(open = NA,
                             tags$summary("VISUALIZACIÓN"),
                             div(class = "contenido",
                                 sliderInput("opacidad", "Opacidad de la capa",
                                             min = 0.1, max = 1, value = 0.8, step = 0.1)))
            ),
            
            div(class = "zona-mapa",
                div(id = "mapa-wrap", class = "mapa-wrap",
                    leafletOutput("mapa", height = "100%"),
                    
                    # ---- Ventana de filtros (flotante sobre el mapa) ----
                    div(class = "filtros",
                        div(class = "f-grupo", div(class = "f-lab", "Departamento"),
                            selectInput("sel_dep", NULL, choices = c("Todos" = ""),
                                        selectize = FALSE, width = "165px")),
                        div(class = "f-grupo", div(class = "f-lab", "Provincia"),
                            desactivar(selectInput("sel_prov", NULL, choices = c("Todas" = ""),
                                                   selectize = FALSE, width = "135px"))),
                        div(class = "f-grupo", div(class = "f-lab", "Distrito"),
                            desactivar(selectInput("sel_dist", NULL, choices = c("Todos" = ""),
                                                   selectize = FALSE, width = "190px"))),
                        actionButton("reset", "Restablecer vista", class = "btn-reset"),
                        div(class = "f-sep"),
                        div(class = "f-grupo", div(class = "f-lab", "Periodo"),
                            dateRangeInput("fechas", NULL,
                                           start = hoy - 10, end = hoy, max = hoy,
                                           format = "dd/mm/yyyy", language = "es",
                                           separator = "–", weekstart = 1))
                    ),
                    
                    div(class = "spinner-mapa", "Calculando…"),
                    
                    # ---- Leyenda plegable (solo se ve en celular) ----
                    tags$details(class = "ley-movil",
                                 tags$summary("🎨 Leyenda"),
                                 div(class = "contenido-ley", uiOutput("leyenda_movil"))),
                    
                    div(class = "minimapa-wrap",
                        div(id = "minimapa"),
                        div(class = "norte", HTML(ICONO_NORTE))),
                    tags$input(id = "archivo-aoi", type = "file", multiple = NA, style = "display:none",
                               accept = ".shp,.shx,.dbf,.prj,.cpg,.zip,.gpkg,.kml,.kmz"),
                    div(id = "zona-soltar", class = "zona-soltar",
                        div("Suelta aquí tu archivo (SHP, GPKG, KML o KMZ)")),
                    div(id = "aviso-mapa", class = "aviso-mapa"),
                    div(id = "dibujo-ayuda", class = "dibujo-ayuda",
                        div(class = "dib-btns",
                            tags$button(type = "button", class = "dib-b", onclick = "deshacerPunto()", "Deshacer"),
                            tags$button(type = "button", class = "dib-b", onclick = "cancelarDibujo()", "Cancelar"),
                            tags$button(type = "button", class = "dib-b dib-ok", onclick = "terminarDibujo()", "Finalizar")),
                        div(id = "dib-msg", class = "dib-msg")),
                    div(id = "ventana-coord", class = "ventana-coord",
                        div(class = "asa", title = "Arrastra para mover la ventana"),
                        tags$button(type = "button", class = "vv-x", title = "Cerrar",
                                    onclick = "cerrarCoord()", HTML("&times;")),
                        div(class = "pr-cab", "Ingresar coordenadas"),
                        HTML(FORM_COORD)),
                    div(id = "ventana-valores", class = "ventana-valores",
                        div(class = "asa", title = "Arrastra para mover la ventana"),
                        tags$button(type = "button", class = "vv-x", title = "Cerrar",
                                    onclick = "cerrarVentana()", HTML("&times;")),
                        div(id = "vv-cuerpo", class = "vv-cuerpo")),
                    div(id = "panel-result", class = "panel-result",
                        div(class = "asa", title = "Arrastra para mover la ventana"),
                        tags$button(type = "button", class = "vv-x", title = "Cerrar",
                                    onclick = "borrarPoligono()", HTML("&times;")),
                        div(class = "pr-cab", "Distribución espacial y porcentual de variables"),
                        div(id = "pr-cuerpo")),
                    div(class = "ctrl-derecha",
                        div(class = "consulta-wrap",
                            tags$button(id = "btn_consulta", type = "button",
                                        class = "btn-consulta", onclick = "toggleMenuConsulta()",
                                        HTML(paste0(ICONO_CONSULTA, " Consultar valor"))),
                            div(id = "menu-consulta", class = "menu-consulta",
                                tags$button(id = "mi_punto", type = "button", class = "mi",
                                            onclick = "elegirHerramienta('punto')",
                                            HTML(paste0(ICONO_CONSULTA, "<span>Seleccionar punto</span>"))),
                                tags$button(id = "mi_coord", type = "button", class = "mi",
                                            onclick = "elegirHerramienta('coord')",
                                            HTML(paste0(ICONO_COORD, "<span>Ingresar coordenadas</span>"))),
                                tags$button(id = "mi_area", type = "button", class = "mi",
                                            onclick = "elegirHerramienta('area')",
                                            HTML(paste0(ICONO_POLIGONO, "<span>Delimitar área</span>"))),
                                tags$button(id = "mi_importar", type = "button", class = "mi",
                                            onclick = "elegirHerramienta('importar')",
                                            HTML(paste0(ICONO_IMPORTAR, "<span>Importar área de interés</span>"))))),
                        tags$button(id = "btn_pantalla", type = "button", class = "btn-icono",
                                    title = "Pantalla completa", onclick = "pantallaCompleta()",
                                    HTML(ICONO_PANTALLA)),
                        tags$button(type = "button", class = "btn-icono",
                                    title = "Mi ubicación", onclick = "miUbicacion()",
                                    HTML(ICONO_UBICACION)))
                )
            )
        )
    )
  )
}

# -----------------------------------------------------------------------------
# Servidor
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  gee_ok <- reactiveVal(FALSE)
  lim_ok <- reactiveVal(FALSE)
  
  seguro <- function(expr, avisar = TRUE) {
    tryCatch(expr,
             shiny.silent.error = function(e) NULL,
             error = function(e) {
               message("Error: ", conditionMessage(e))
               if (avisar) {
                 showNotification(paste("Error:", conditionMessage(e)),
                                  type = "error", duration = 10, session = session)
               }
               NULL
             })
  }
  
  sp <- function(pct, texto) {
    session$sendCustomMessage("splash_paso", list(pct = pct, texto = texto))
  }
  
  enviar_limites <- function(lim) {
    session$sendCustomMessage("cargar_limites", list(
      dep = lim$dep$geojson, prov = lim$prov$geojson,
      dist = lim$dist$geojson, per = lim$per$geojson))
    p <- CACHE$pais %||% leer_cache("pais_v2.rds")
    if (!is.null(p)) {
      CACHE$pais <- p
      session$sendCustomMessage("cargar_pais", list(pais = p))
    }
  }
  
  # ---- Conexión con Earth Engine (después de mostrar la página) -------------
  session$onFlushed(function() {
    t_log("Sesión nueva: página entregada")
    # Si hay caché, el mapa con los límites se dibuja YA, sin esperar a Earth Engine
    lim0 <- CACHE$lim %||% leer_cache("limites_v2.rds")
    if (!is.null(lim0)) {
      CACHE$lim <- lim0
      enviar_limites(lim0)
      lim_ok(TRUE)
      t_log("Límites enviados al navegador desde la caché")
      sp(60, "LÍMITES LISTOS")
      # El mapa ya es usable: se quita la pantalla de carga; la capa llega después
      session$sendCustomMessage("splash_fin", TRUE)
    }
    sp(20, "CONECTANDO CON EARTH ENGINE")
    ok <- tryCatch({ asegurar_gee(); TRUE }, error = function(e) {
      message("Error al conectar con Earth Engine: ", conditionMessage(e))
      showNotification(paste("No se pudo conectar con Earth Engine:",
                             conditionMessage(e)),
                       type = "error", duration = NULL, session = session)
      FALSE
    })
    t_log(if (ok) "Earth Engine conectado" else "Earth Engine NO conectó")
    if (ok) gee_ok(TRUE) else session$sendCustomMessage("splash_fin", TRUE)
  }, once = TRUE)
  
  # Solo si NO había caché (primera vez, en tu PC): se calculan los límites en Earth Engine
  observeEvent(gee_ok(), {
    req(gee_ok())
    if (lim_ok()) { sp(80, "CALCULANDO NDVI Y NDMI"); return() }
    sp(50, "CARGANDO LÍMITES ADMINISTRATIVOS")
    lim <- tryCatch(cargar_limites(), error = function(e) {
      message("Error al cargar límites: ", conditionMessage(e))
      showNotification(paste("No se pudieron cargar los límites:",
                             conditionMessage(e)),
                       type = "error", duration = NULL)
      NULL
    })
    if (is.null(lim)) session$sendCustomMessage("splash_fin", TRUE)
    req(lim)
    enviar_limites(lim)
    lim_ok(TRUE)
    sp(80, "CALCULANDO NDVI Y NDMI")
    # El contorno del Perú se calcula después de que el mapa ya muestra los límites
    session$onFlushed(function() {
      p <- cargar_pais()
      if (!is.null(p)) session$sendCustomMessage("cargar_pais", list(pais = p))
    }, once = TRUE)
  })
  
  # La navegación departamento > provincia > distrito se resuelve en el navegador (JavaScript)
  
  # ---- Mapa base: solo Google (se agrega desde JavaScript) -------------------
  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5, maxZoom = 20,
                                     attributionControl = FALSE)) |>
      setView(lng = -75, lat = -9, zoom = 5) |>
      htmlwidgets::onRender("function(el, x) { window.mapaLeaflet = this; }")
  })
  
  # ---- Cálculo de NDVI y NDMI (Earth Engine) ----------------------------------
  fechas_deb <- debounce(reactive(input$fechas), 700)
  
  comp <- reactive({
    req(gee_ok())
    f <- fechas_deb()
    req(length(f) == 2, !anyNA(f))
    clave <- paste(as.character(f), collapse = "_")
    if (!is.null(CACHE_COMP[[clave]])) return(CACHE_COMP[[clave]])
    if (as.numeric(f[2] - f[1]) > 92) {
      showNotification("Los periodos largos (más de 3 meses) tardan más en calcularse.",
                       type = "warning", duration = 6)
    }
    # La fecha final es inclusiva: se suma un día porque filterDate excluye el final
    t_log(paste("Calculando composición", f[1], "a", f[2]))
    r <- reticulate::py$compuesto(as.character(f[1]), as.character(f[2] + 1))
    t_log(paste("Composición lista:", r$n, "imágenes"))
    ks <- ls(CACHE_COMP)
    if (length(ks) >= 8) rm(list = ks[1], envir = CACHE_COMP)
    CACHE_COMP[[clave]] <- r
    r
  })
  
  url_capa <- reactive({
    r <- comp()
    capa <- input$capa
    req(capa, r$n > 0)
    f <- fechas_deb()
    clave <- paste0("url_", capa, "_", paste(as.character(f), collapse = "_"))
    if (!is.null(CACHE_COMP[[clave]])) return(CACHE_COMP[[clave]])
    v <- VIS[[capa]]
    u <- r$img$select(capa)$getMapId(
      list(min = v$min, max = v$max, palette = as.list(v$palette))
    )$tile_fetcher$url_format
    CACHE_COMP[[clave]] <- u
    u
  })
  
  observe({
    r <- seguro(comp())
    if (!is.null(r) && r$n == 0) {
      showNotification("No hay imágenes disponibles en el periodo elegido. Amplía el rango de fechas.",
                       type = "warning", duration = 8)
      session$sendCustomMessage("capa_gee", list(url = "", opacidad = 1))
    }
  })
  
  observe({
    u <- seguro(url_capa())
    if (!is.null(u)) {
      session$sendCustomMessage("capa_gee",
                                list(url = u, opacidad = isolate(input$opacidad)))
    }
  })
  
  observeEvent(input$opacidad, {
    session$sendCustomMessage("opacidad", input$opacidad)
  }, ignoreInit = TRUE)
  
  # ---- Consulta de valor en un punto ------------------------------------------
  observeEvent(input$clic_mapa, {
    x <- input$clic_mapa
    r <- seguro(comp(), avisar = FALSE)
    if (is.null(r) || r$n == 0) {
      html <- armar_aviso("No hay imágenes disponibles en el periodo elegido.", x$lat, x$lng)
    } else {
      v <- tryCatch(reticulate::py$valor_punto(r$img, x$lng, x$lat),
                    error = function(e) {
                      message("Error en la consulta: ", conditionMessage(e))
                      NULL
                    })
      html <- if (is.null(v)) {
        armar_aviso("No se pudo consultar el valor. Inténtalo de nuevo.", x$lat, x$lng)
      } else {
        armar_popup(v, x$lat, x$lng)
      }
    }
    session$sendCustomMessage("popup_valor", list(id = x$id, html = html))
  })
  
  # ---- Terreno dibujado: área total y área por rango de NDVI y NDMI -----------
  poli <- reactiveVal(NULL)
  
  observeEvent(input$poligono, { poli(input$poligono) })
  observeEvent(input$poligono_borrar, { poli(NULL) })
  
  observe({
    p <- poli()
    req(p)
    r <- seguro(comp(), avisar = FALSE)
    datos <- if (is.null(r) || r$n == 0) {
      '{"error":"sin_imagenes"}'
    } else {
      tryCatch(as.character(reticulate::py$analizar_poligono(r$img, p$coords)),
               error = function(e) {
                 message("Error al analizar el terreno: ", conditionMessage(e))
                 ""
               })
    }
    session$sendCustomMessage("poligono_resultado", list(id = p$id, datos = datos))
  })
  
  # ---- Panel lateral: leyenda (en el menú y en la leyenda plegable del celular) ----
  ley_ui <- reactive({
    v <- VIS[[input$capa %||% "NDVI"]]
    degradado <- paste0("linear-gradient(to right, ",
                        paste(v$palette, collapse = ","), ")")
    tagList(
      div(class = "ley-tit", v$titulo),
      div(class = "ley-barra", style = paste0("background:", degradado)),
      div(class = "ley-marcas", lapply(v$marcas, function(m) span(m))),
      lapply(v$clases, function(cl) {
        div(class = "ley-fila",
            span(class = "ley-sw", style = paste0("background:", cl[[1]])),
            cl[[2]])
      })
    )
  })
  output$leyenda       <- renderUI(ley_ui())
  output$leyenda_movil <- renderUI(ley_ui())
  
  output$info_periodo <- renderUI({
    f <- fechas_deb()
    texto_p <- if (length(f) == 2 && !anyNA(f)) {
      paste0(format(f[1], "%d/%m/%Y"), " a ", format(f[2], "%d/%m/%Y"))
    } else ""
    tags$div(class = "nota-info",
             tags$div(HTML(paste0("<b>Periodo:</b> ", texto_p))))
  })
}

shinyApp(ui, server)