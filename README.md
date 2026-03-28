# Econometría del Riesgo Financiero

## Valoración de Activos, Volatilidad y Gestión del Riesgo con R

**Autor:** Carlos de Anta Puig
Economista · Perito Financiero
Miembro del Colegio de Economistas de Madrid
Miembro del Instituto Español de Analistas Financieros (IEAF)
Profesor de Econometría y Microeconometría
carlos@cwconsultores.com

---

### Publicación

Este manual está publicado por **[Digital Reasons](https://www.digitalreasons.es)**.

---

### Índice

**Parte I — Fundamentos**

1. Introducción: Econometría y Mercados Financieros
2. Propiedades Estadísticas de los Rendimientos Financieros
3. Regresión Lineal en Contexto Financiero

**Parte II — Valoración de Activos**

4. El CAPM y su Contrastación Empírica
5. Modelos Multifactoriales
6. Estudios de Eventos

**Parte III — Volatilidad**

7. Modelos de Volatilidad Condicional: GARCH Univariante
8. Volatilidad Multivariante y Correlaciones Dinámicas
9. Volatilidad Implícita y Superficies

**Parte IV — Gestión del Riesgo**

10. Value at Risk
11. Expected Shortfall y Medidas Coherentes de Riesgo
12. Backtesting, Stress Testing y Validación

---

### Estructura del repositorio

- `data/` — Ficheros XLSX/CSV descargados desde desk.carlosdeanta.net
- `scripts/` — Scripts R autocontenidos por capítulo
- `scripts/deskR.R` — Funciones auxiliares para lectura de datos (`desk_read`, `desk_returns`, `desk_close`, `desk_returns_matrix`)

### Datos

Las series financieras se descargan manualmente desde **[desk.carlosdeanta.net](https://desk.carlosdeanta.net)**, el terminal financiero multi-mercado del autor. El portal ofrece datos OHLCV históricos de más de 1.000 activos en 16 mercados: acciones, divisas, criptomonedas, materias primas, índices de volatilidad, tipos de interés y curvas soberanas.

**Flujo de trabajo:**

1. Acceder a `desk.carlosdeanta.net`
2. Navegar al activo indicado en cada capítulo
3. Seleccionar el rango de fechas
4. Descargar el XLSX y guardarlo en `data/`
5. Ejecutar el script R correspondiente

Fuente complementaria: Kenneth French Data Library (factores, Cap. 5).

### Requisitos

- R >= 4.0
- Paquetes: `tidyverse`, `readxl`, `quantmod`, `rugarch`, `rmgarch`, `PerformanceAnalytics`, `fixest`, `sandwich`, `kableExtra`

---

*© Carlos de Anta Puig, 2026. Publicado por [Digital Reasons](https://www.digitalreasons.es). Todos los derechos reservados.*
