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

1. **Introducción: Econometría y Mercados Financieros**
   - Por qué este manual
   - El ciclo de investigación empírica en finanzas
   - El portal de datos: desk.carlosdeanta.net
   - El ecosistema R para finanzas
   - Las funciones auxiliares: deskR
   - Caso empírico: primer contacto con los datos
   - Reproducibilidad

2. **Propiedades Estadísticas de los Rendimientos Financieros**
   - Rendimientos simples y logarítmicos
   - Los cuatro activos de referencia
   - Hecho estilizado 1: Ausencia de autocorrelación lineal
   - Hecho estilizado 2: Colas pesadas
   - Hecho estilizado 3: Clusters de volatilidad
   - Hecho estilizado 4: Asimetría y efecto leverage
   - Distribuciones: más allá de la normal
   - Tests formales
   - Agregación temporal

3. **Regresión Lineal en Contexto Financiero**
   - El modelo de mercado
   - Estimación por MCO
   - La heterocedasticidad como fenómeno de mercado
   - Test formal de heterocedasticidad
   - Outliers e influencia
   - Tabla resumen tipo paper
   - La tensión entre predicción e inferencia

**Parte II — Valoración de Activos**

4. **El CAPM y su Contrastación Empírica**
   - La pregunta fundamental de las finanzas
   - Derivación intuitiva y contrastación en series temporales
   - Contrastación empírica con el IBEX-35
   - La Security Market Line
   - Betas rolling: ¿son estables?
   - Los límites del CAPM

5. **Modelos Multifactoriales**
   - Construcción de factores y portfolios sorted
   - Los factores de Kenneth French
   - Regresiones multifactoriales: del CAPM al modelo de cinco factores
   - Fama-MacBeth: estimación de primas de riesgo
   - Caso empírico: momentum, tamaño y modelo de tres factores
   - El debate: ¿riesgo o mispricing?

6. **Estudios de Eventos**
   - Diseño de un estudio de eventos
   - Caso empírico: el impacto del COVID-19 en el IBEX-35
   - Comparación de eventos: COVID vs. Lehman vs. Brexit
   - Análisis por sectores
   - CAR individual: las acciones más y menos afectadas
   - Problemas metodológicos: clustering, thin trading, confounding events

**Parte III — Volatilidad**

7. **Fundamentos de Series Temporales**
   - Procesos estocásticos y estacionariedad
   - La función de autocorrelación (ACF y PACF)
   - Procesos autoregresivos (AR) y de media móvil (MA)
   - Procesos ARMA
   - Identificación, selección y estimación de modelos
   - El paseo aleatorio: el modelo de los precios
   - Caso empírico: estructura temporal del EUR/USD

8. **Modelos de Volatilidad Condicional: GARCH Univariante**
   - Taxonomía de la volatilidad
   - ¿Por qué cambia la volatilidad? Razones económicas
   - De la homocedasticidad a la varianza condicional
   - El test ARCH-LM y la formalización ARCH → GARCH
   - Modelos asimétricos: EGARCH y GJR-GARCH
   - Caso empírico: volatilidad del EUR/USD
   - News impact curve
   - Predicción de volatilidad

9. **Volatilidad Multivariante y Correlaciones Dinámicas**
   - El modelo DCC-GARCH
   - Caso empírico: transmisión de volatilidad IBEX-DAX-S&P 500
   - Correlaciones dinámicas
   - Extensión: ¿el oro como refugio?

10. **Volatilidad Implícita y Superficies**
    - Dos formas de medir la volatilidad
    - El VIX: el índice del miedo
    - Caso empírico: la prima de varianza
    - VIX y tipos de interés

**Parte IV — Gestión del Riesgo**

11. **Value at Risk**
    - La cartera multi-activo
    - Método 1: VaR paramétrico
    - Método 2: VaR por simulación histórica
    - Método 3: VaR condicional (GARCH-VaR)
    - Comparación de métodos
    - Efecto diversificación

12. **Expected Shortfall y Medidas Coherentes de Riesgo**
    - Los problemas del VaR
    - Expected Shortfall: la pérdida esperada en la cola
    - Estimación del ES
    - Demostración de la no-subaditividad del VaR
    - VaR vs. ES en el tiempo

13. **Backtesting, Stress Testing y Validación**
    - Backtesting del VaR
    - Test de Kupiec (cobertura incondicional)
    - Test de Christoffersen (cobertura condicional)
    - Caso empírico: backtesting de la cartera multi-activo
    - Semáforos de Basilea
    - Stress testing: escenarios históricos
    - El cierre del ciclo

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
