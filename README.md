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

- `data/` — Datasets CSV, reproducibles con semilla fija
- `scripts/` — Scripts R autocontenidos por capítulo
- `R/` — Paquete deskR: wrapper para la API de desk.carlosdeanta.net

### Datos

Todas las series financieras se obtienen a través de la API de **desk.carlosdeanta.net** mediante el paquete R `deskR` incluido en este repositorio. Fuente complementaria: Kenneth French Data Library (factores).

### Requisitos

- R >= 4.0
- Paquetes: `tidyverse`, `quantmod`, `rugarch`, `rmgarch`, `PerformanceAnalytics`, `fixest`, `sandwich`, `kableExtra`

---

*© Carlos de Anta Puig, 2026. Publicado por [Digital Reasons](https://www.digitalreasons.es). Todos los derechos reservados.*
