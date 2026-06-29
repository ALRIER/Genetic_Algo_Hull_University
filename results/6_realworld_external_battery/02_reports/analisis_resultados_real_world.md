---

title: "Análisis de Resultados Real World"

subtitle: "Batería externa V5 con taxonomía de ganadores NEST26"

author: "Reporte técnico interno"

date: "2026-06-16"

---



# Resumen ejecutivo



Este reporte resume los resultados finales de la batería externa real-world V5. El objetivo es dejar un insumo detallado para redactar la sección formal de resultados: qué ganó, con qué nivel de evidencia, qué sobrevivió a intervalos de confianza y FDR, qué candidatos no produjeron señal primaria, qué anomalías operativas aparecieron y cómo debe narrarse el resultado sin inflar la evidencia.



La lectura principal es positiva: el experimento produjo evidencia externa fuerte y corregida por multiplicidad. No es una demostración de dominancia universal del algoritmo genético; es una demostración de transferencia condicional en perfiles empíricos compatibles con la taxonomía. Esa distinción es central para la escritura.



|Métrica|Valor|
|---|---|
|Fuentes públicas solicitadas|264|
|Fuentes cargadas correctamente|228|
|Fuentes seleccionadas/evaluadas|128|
|Filas dataset-level compactas|809|
|Datasets únicos en tabla compacta|120|
|Candidatos únicos con filas evaluadas|12|
|Strong wins dataset-level antes de FDR|256|
|Strong wins con CI confirmado|256|
|Sobrevivientes FDR 5% estrictos|255|
|Sobrevivientes FDR 10% sensibilidad|257|
|Datasets con al menos un sobreviviente|81|
|Candidatos con al menos un sobreviviente FDR 5%|12|
|Datasets saturados por cap|21|
|Filas gate-level auditadas|5900|



## Veredicto



El resultado sirve para escritura. La afirmación defendible es que la batería externa confirma una señal de especialización transferible: 256 filas dataset-level alcanzan `strong_win` con `ci_confirmed`, y 255 de ellas sobreviven la corrección BH-FDR al 5%. La sensibilidad al 10% agrega solo dos filas adicionales, por lo que el resultado no depende de relajar el umbral de multiplicidad.



La retórica recomendada es: evidencia externa fuerte, corregida por FDR, concentrada en regímenes empíricos compatibles con la familia del especialista. La retórica que debe evitarse es: victoria universal, superioridad global sobre todos los benchmarks o conteo de depth probes como datasets independientes.



# Diseño y criterios de selección



La batería externa solicitó fuentes públicas de UCI, Rdatasets y series de mercado. Cada fuente fue cargada, perfilada y clasificada por forma empírica. El resultado primario se restringió a comparaciones elegibles, in-regime y contra el benchmark preregistrado por perfil. Las comparaciones oracle se conservaron como auditoría secundaria y no deben convertirse en claim principal.



El pipeline usó una puerta de perfil empírico (`profile_gate=TRUE`). La puerta asigna perfiles como `normal_like`, `lognormal_like`, `weibull_like`, `heavy_tail_symmetric`, `zero_inflated_count` o `empirical_unknown` según asimetría, curtosis, soporte y metadatos del dominio. En la práctica, las filas primarias finales se concentraron en perfiles normal, lognormal y Weibull.



|Estado de fuente|Conteo|
|---|---|
|selected|128|
|skipped_profile_quota|85|
|load_failed|36|
|skipped_source_cap|15|



|Perfil empírico en registro|Conteo|
|---|---|
|lognormal_like|118|
|normal_like|77|
|load_failed|36|
|weibull_like|19|
|empirical_unknown|9|
|zero_inflated_count|5|



|Grupo de fuente|Conteo|
|---|---|
|Rdatasets|185|
|UCI|55|
|market_returns|24|



# Taxonomía de candidatos



La taxonomía completa de entrada contiene 25 registros. La composición global fue: 12 `accepted_ga_win`, 7 `borderline_near_gate` y 6 `benchmark_control`. La salida primaria compacta produjo filas evaluables para 12 candidatos, y esos 12 candidatos tuvieron al menos un sobreviviente FDR estricto. Los 13 restantes no produjeron ganador primario estricto en esta batería externa bajo las reglas de elegibilidad/perfil usadas aquí.



Interpretación: no todos los miembros de la taxonomía se convierten en ganadores externos. Esto es deseable bajo una lectura No Free Lunch: los especialistas útiles sobreviven cuando el perfil empírico coincide con su régimen, y los controles o candidatos no alineados no deben ganar de forma indiscriminada.



|Clase taxonómica|Conteo|
|---|---|
|accepted_ga_win|12|
|borderline_near_gate|7|
|benchmark_control|6|



|Grado de evidencia taxonómica|Conteo|
|---|---|
|discovery_supported_local_candidate|10|
|near_gate_candidate|7|
|negative_control_success|6|
|transfer_specialist|2|



## Resultado por candidato de la taxonomía completa



|Candidato|Familia|Clase|Evidencia taxonómica|Filas eval.|Strong wins|FDR 5%|FDR 10%|Estado externo|
|---|---|---|---|---|---|---|---|---|
|TAX-Q1R008|lognormal|accepted_ga_win|discovery_supported_local_candidate|114|77|77|77|winner|
|TAX-Q1R009|lognormal|accepted_ga_win|discovery_supported_local_candidate|103|55|55|55|winner|
|TAX-Q1R010|normal|accepted_ga_win|discovery_supported_local_candidate|95|40|39|40|winner|
|TAX-Q1R025|normal|benchmark_control|negative_control_success|84|19|19|19|winner|
|TAX-Q1R023|normal|benchmark_control|negative_control_success|84|18|18|18|winner|
|TAX-Q1R018|weibull|borderline_near_gate|near_gate_candidate|20|11|11|11|winner|
|TAX-Q1R019|weibull|borderline_near_gate|near_gate_candidate|17|11|11|11|winner|
|TAX-Q1R024|normal|benchmark_control|negative_control_success|84|9|9|9|winner|
|TAX-Q1R017|normal|borderline_near_gate|near_gate_candidate|84|7|7|8|winner|
|TAX-Q1R016|normal|borderline_near_gate|near_gate_candidate|84|5|5|5|winner|
|TAX-Q1R011|weibull|accepted_ga_win|transfer_specialist|20|3|3|3|winner|
|TAX-Q1R012|weibull|accepted_ga_win|transfer_specialist|20|1|1|1|winner|
|TAX-Q1R001|exwald|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R002|exwald|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R003|exwald|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R004|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R005|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R006|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R007|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R013|exgaussian|borderline_near_gate|near_gate_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R014|exgaussian|borderline_near_gate|near_gate_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R015|exgaussian|borderline_near_gate|near_gate_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R020|exgaussian|benchmark_control|negative_control_success|0|0|0|0|no strict FDR win|
|TAX-Q1R021|exgaussian|benchmark_control|negative_control_success|0|0|0|0|no strict FDR win|
|TAX-Q1R022|exgaussian|benchmark_control|negative_control_success|0|0|0|0|no strict FDR win|



# Niveles de evidencia



El primer nivel de lectura es el dataset-level compact output. Aquí cada fila resume la mejor evidencia de un candidato en un dataset/modo contra el benchmark preregistrado. El segundo nivel de lectura es gate-level: allí hay muchas filas por condición y sirve para auditoría, no para contar descubrimientos independientes.



## Dataset-level: evidencia principal



|Nivel de evidencia|Conteo|Lectura|
|---|---|---|
|equal_weight_gain|458|Los pesos aprendidos superan pesos iguales; evidencia secundaria.|
|strong_win|256|Ganador primario potencial; requiere CI y FDR para claim fuerte.|
|no_win|77|El benchmark retiene ventaja.|
|strong_point_signal|15|Señal puntual fuerte pero no completamente confirmada por CI.|
|pareto_signal|3|Mejora parcial sin daño material; evidencia secundaria.|



## Dataset-level: intervalos de confianza



|Capa CI|Conteo|Lectura|
|---|---|---|
|descriptive|532|Evidencia descriptiva sin soporte estadístico suficiente.|
|ci_confirmed|256|El intervalo confirma mejora por encima de cero.|
|near_ci|21|La señal positiva toca o roza cero; usar como sensibilidad.|



## Gate-level audit: no contar como descubrimientos independientes



|Gate evidence|Conteo|
|---|---|
|equal_weight_gain|2800|
|strong_win|2211|
|no_win|782|
|strong_point_signal|92|
|pareto_signal|15|



|Gate CI|Conteo|
|---|---|
|descriptive|3559|
|ci_confirmed|2211|
|near_ci|130|



La cifra gate-level de `strong_win / ci_confirmed` es 2211, pero no debe presentarse como 2211 descubrimientos independientes. El número defendible para resultados principales es el dataset-level FDR: 255 filas estrictas al 5%.



# Ganadores estrictos FDR 5%



La tabla siguiente resume los candidatos ganadores estrictos. Todos estos conteos pasan FDR 5%; todos corresponden a strong wins con CI confirmado.



|Candidato|Familia|FDR 5% rows|Datasets|Dominios|Mediana gain_q95|Máximo gain_q95|
|---|---|---|---|---|---|---|
|TAX-Q1R008|lognormal|77|43|8|1.996|31,287.04|
|TAX-Q1R009|lognormal|55|37|7|0.112148|8,271.34|
|TAX-Q1R010|normal|39|24|11|3.029|7,554.70|
|TAX-Q1R025|normal|19|16|10|0.53573|4,679.93|
|TAX-Q1R023|normal|18|16|10|1.527|4,730.46|
|TAX-Q1R018|weibull|11|9|5|0.415256|30.684|
|TAX-Q1R019|weibull|11|9|5|7.558|72.343|
|TAX-Q1R024|normal|9|6|5|0.125242|316.264|
|TAX-Q1R017|normal|7|5|4|0.329735|341.090|
|TAX-Q1R016|normal|5|3|2|0.0534867|0.227151|
|TAX-Q1R011|weibull|3|2|2|0.000262703|0.581494|
|TAX-Q1R012|weibull|1|1|1|0.468691|0.468691|



## Ganadores por familia y modo



|Familia|Modo|Sobrevivientes FDR 5%|
|---|---|---|
|lognormal|original_regime|79|
|normal|original_regime|68|
|lognormal|locked_unseen_similar|53|
|normal|locked_unseen_similar|29|
|weibull|original_regime|14|
|weibull|locked_unseen_similar|12|



## Ganadores por dominio empírico



|Dominio|Sobrevivientes FDR 5%|
|---|---|
|positive_skew_tabular|38|
|mobility_demand|34|
|rdatasets_gt|25|
|web_popularity|22|
|rdatasets_bayesrules|14|
|rdatasets_boot|11|
|rdatasets_ISLR|11|
|rdatasets_admiral|10|
|rdatasets_fpp2|10|
|rdatasets_psych|10|
|biological_positive|10|
|rdatasets_archdata|9|
|rdatasets_HistData|9|
|rdatasets_asaur|8|
|rdatasets_dragracer|8|
|rdatasets_OncoDataSets|8|
|rdatasets_camerondata|7|
|rdatasets_bakeoff|5|
|rdatasets_alone|3|
|education_scores|3|



## Ganadores por perfil empírico



|Perfil empírico|Sobrevivientes FDR 5%|
|---|---|
|lognormal_like|132|
|normal_like|97|
|weibull_like|26|



# Mejora contra benchmarks preregistrados



La mejora se reporta como diferencia de pérdida contra el benchmark preregistrado. En estas tablas `gain_q95` es la mejora en el percentil 95 de MSE frente al benchmark seleccionado; `gain_mean` es la mejora media. Valores positivos favorecen al especialista GA congelado. No se reexpresan como porcentaje porque las escalas de los datasets son heterogéneas.



|Familia|N FDR 5%|Mediana gain_q95|Q1 gain_q95|Q3 gain_q95|Máximo gain_q95|Mediana gain_mean|Mínimo CI low|
|---|---|---|---|---|---|---|---|
|lognormal|132|0.289758|0.0169254|1,018.00|31,287.04|0.157974|0.000349859|
|normal|97|0.532004|0.126289|6.357|7,554.70|0.169903|0.000369222|
|weibull|26|0.618578|0.262458|26.547|72.343|0.257928|0.000228965|



## Mayores mejoras observadas



|Dataset|Dominio|Candidato|Familia|Modo|Benchmark|gain_q95|CI low|q BH|
|---|---|---|---|---|---|---|---|---|
|UCI_online_news_tokens__depth_upper_tail_enriched|web_popularity|TAX-Q1R008|lognormal|original_regime|huber_k1.345|31,287.04|29,975.85|0|
|UCI_online_news_tokens|web_popularity|TAX-Q1R008|lognormal|original_regime|huber_k1.345|12,854.76|12,234.07|0|
|UCI_online_news_tokens__depth_bootstrap_full|web_popularity|TAX-Q1R008|lognormal|original_regime|huber_k1.345|12,359.74|11,878.46|0|
|UCI_online_news_tokens__depth_upper_tail_enriched|web_popularity|TAX-Q1R008|lognormal|locked_unseen_similar|huber_k1.345|9,326.89|8,512.84|0|
|UCI_online_news_tokens__depth_upper_tail_enriched|web_popularity|TAX-Q1R009|lognormal|original_regime|huber_k1.345|8,271.34|7,722.77|0|
|RD_ISLR_Default_auto|rdatasets_ISLR|TAX-Q1R010|normal|original_regime|mean|7,554.70|6,954.22|0|
|UCI_bike_hour_count__depth_upper_tail_enriched|mobility_demand|TAX-Q1R008|lognormal|original_regime|huber_k1.345|7,421.03|7,273.65|0|
|RD_ISLR_Default_auto__depth_bootstrap_full|rdatasets_ISLR|TAX-Q1R010|normal|original_regime|mean|7,206.27|6,838.97|0|
|RD_ISLR_Default_auto__depth_upper_tail_enriched|rdatasets_ISLR|TAX-Q1R010|normal|original_regime|mean|6,811.09|6,011.66|0|
|UCI_online_news_tokens__depth_body_10_90|web_popularity|TAX-Q1R008|lognormal|original_regime|huber_k1.345|5,370.31|5,212.44|0|
|RD_bayesrules_bird_counts_auto__depth_upper_tail_enrich|rdatasets_bayesrules|TAX-Q1R008|lognormal|original_regime|huber_k1.345|5,085.91|4,674.12|0|
|RD_ISLR_Default_auto__depth_bootstrap_full|rdatasets_ISLR|TAX-Q1R010|normal|locked_unseen_similar|mean|4,918.35|4,685.19|0|
|UCI_bike_hour_registered__depth_upper_tail_enriched|mobility_demand|TAX-Q1R008|lognormal|original_regime|huber_k1.345|4,891.58|4,784.02|0|
|UCI_bike_hour_count|mobility_demand|TAX-Q1R008|lognormal|original_regime|huber_k1.345|4,888.71|4,771.70|0|
|UCI_bike_hour_count__depth_bootstrap_full|mobility_demand|TAX-Q1R008|lognormal|original_regime|huber_k1.345|4,885.76|4,779.45|0|
|RD_ISLR_Default_auto__depth_upper_tail_enriched|rdatasets_ISLR|TAX-Q1R010|normal|locked_unseen_similar|mean|4,861.09|4,421.27|0|
|RD_ISLR_Default_auto__depth_bootstrap_full|rdatasets_ISLR|TAX-Q1R023|normal|original_regime|mean|4,730.46|4,447.13|0|
|RD_ISLR_Default_auto__depth_bootstrap_full|rdatasets_ISLR|TAX-Q1R025|normal|original_regime|mean|4,679.93|4,429.10|0|
|RD_bayesrules_bird_counts_auto__depth_upper_tail_enrich|rdatasets_bayesrules|TAX-Q1R008|lognormal|locked_unseen_similar|huber_k1.345|4,177.86|3,702.71|0|
|UCI_bike_hour_count__depth_upper_tail_enriched|mobility_demand|TAX-Q1R008|lognormal|locked_unseen_similar|huber_k1.345|4,013.71|3,931.64|0|
|RD_ISLR_Default_auto__depth_upper_tail_enriched|rdatasets_ISLR|TAX-Q1R023|normal|original_regime|mean|3,962.84|3,517.50|0|
|RD_bayesrules_bird_counts_auto__depth_upper_tail_enrich|rdatasets_bayesrules|TAX-Q1R009|lognormal|original_regime|huber_k1.345|3,757.88|3,302.67|0|
|RD_ISLR_Default_auto__depth_upper_tail_enriched|rdatasets_ISLR|TAX-Q1R025|normal|original_regime|mean|3,585.67|3,364.51|0|
|RD_bayesrules_bird_counts_auto|rdatasets_bayesrules|TAX-Q1R008|lognormal|original_regime|huber_k1.345|3,366.20|2,834.59|0|
|UCI_online_news_tokens__depth_body_10_90|web_popularity|TAX-Q1R008|lognormal|locked_unseen_similar|huber_k1.345|3,326.07|3,235.17|0|



# Candidatos no ganadores



Bajo la taxonomía completa, 13 de 25 registros no produjeron un sobreviviente FDR 5% en esta batería externa. Esto no significa necesariamente que sean inválidos en simulación; significa que no generaron una victoria externa primaria bajo la puerta de perfil, benchmark preregistrado, CI y FDR de esta corrida.



|Candidato|Familia|Clase|Evidencia taxonómica|Filas eval.|Strong wins|FDR 5%|FDR 10%|Estado externo|
|---|---|---|---|---|---|---|---|---|
|TAX-Q1R001|exwald|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R002|exwald|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R003|exwald|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R004|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R005|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R006|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R007|invgauss|accepted_ga_win|discovery_supported_local_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R013|exgaussian|borderline_near_gate|near_gate_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R014|exgaussian|borderline_near_gate|near_gate_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R015|exgaussian|borderline_near_gate|near_gate_candidate|0|0|0|0|no strict FDR win|
|TAX-Q1R020|exgaussian|benchmark_control|negative_control_success|0|0|0|0|no strict FDR win|
|TAX-Q1R021|exgaussian|benchmark_control|negative_control_success|0|0|0|0|no strict FDR win|
|TAX-Q1R022|exgaussian|benchmark_control|negative_control_success|0|0|0|0|no strict FDR win|



# Saturación, depth probes y anomalías



Hubo 21 datasets saturados por el cap de victorias. Esta es una señal metodológicamente importante: indica datasets con señal repetida, pero el cap evita que una sola fuente infle el conteo primario. Los depth probes se interpretan como chequeos mecanísticos dentro del dataset, no como datasets independientes.



|Dataset|Dominio|Candidato|Familia|Modo|Strong wins sin cap|Wins contables|best gain_q95|
|---|---|---|---|---|---|---|---|
|RD_bakeoff_challenges_data__auto|rdatasets_bakeoff|TAX-Q1R010|normal|original_regime|10|10|0.157828|
|RD_boot_amis_auto|rdatasets_boot|TAX-Q1R010|normal|original_regime|10|10|0.824841|
|RD_dragracer_rpdr_contep_auto|rdatasets_dragracer|TAX-Q1R010|normal|original_regime|10|10|0.184309|
|RD_fpp2_calls_auto|rdatasets_fpp2|TAX-Q1R010|normal|original_regime|10|10|116.200|
|RD_gt_pizzaplace_auto|rdatasets_gt|TAX-Q1R010|normal|original_regime|10|10|3.985|
|RD_HistData_Pollen_auto|rdatasets_HistData|TAX-Q1R010|normal|original_regime|10|10|0.131155|
|RD_ISLR_Default_auto|rdatasets_ISLR|TAX-Q1R010|normal|original_regime|10|10|7,554.70|
|UCI_student_math_G3|education_scores|TAX-Q1R010|normal|original_regime|10|10|0.0869109|
|RD_psych_bfi_auto|rdatasets_psych|TAX-Q1R019|weibull|original_regime|8|8|7.558|
|RD_archdata_Handaxes_auto|rdatasets_archdata|TAX-Q1R009|lognormal|original_regime|7|7|2.149|
|RD_asaur_prostateSurvival_auto|rdatasets_asaur|TAX-Q1R019|weibull|original_regime|7|7|70.968|
|RD_camerondata_vietnam_ind_auto|rdatasets_camerondata|TAX-Q1R009|lognormal|original_regime|7|7|0.0035574|
|RD_OncoDataSets_ProstateSurvival_df_auto|rdatasets_OncoDataSets|TAX-Q1R019|weibull|original_regime|7|7|67.686|
|UCI_wine_red_alcohol|positive_skew_tabular|TAX-Q1R009|lognormal|original_regime|4|4|0.0484918|
|RD_bayesrules_bird_counts_auto|rdatasets_bayesrules|TAX-Q1R009|lognormal|original_regime|1|1|744.437|
|UCI_abalone_whole_weight|biological_positive|TAX-Q1R009|lognormal|original_regime|1|1|0.00529346|
|UCI_bike_hour_count|mobility_demand|TAX-Q1R009|lognormal|original_regime|1|1|261.859|
|UCI_bike_hour_registered|mobility_demand|TAX-Q1R009|lognormal|original_regime|1|1|810.751|
|UCI_online_news_log_shares|web_popularity|TAX-Q1R009|lognormal|original_regime|1|1|0.0149535|
|UCI_wine_white_alcohol|positive_skew_tabular|TAX-Q1R009|lognormal|original_regime|1|1|0.0683486|
|UCI_wine_white_residual_sugar|positive_skew_tabular|TAX-Q1R009|lognormal|original_regime|1|1|0.952809|



## Fuentes no cargadas o excluidas



Hubo 36 fuentes con fallo de carga, 85 omitidas por cuota de perfil y 15 omitidas por source cap. Estas exclusiones están registradas en `external_dataset_registry.csv`. La presencia de fallos de carga no invalida el resultado porque la inferencia primaria se hace sobre fuentes cargadas y seleccionadas; sí debe reportarse como control de transparencia.



|Tipo de exclusión|Conteo|
|---|---|
|load_failed|36|
|skipped_profile_quota|85|
|skipped_source_cap|15|



# Retórica recomendada para resultados



Una formulación fuerte y defendible sería:



> La batería externa V5 evaluó especialistas congelados derivados de la taxonomía NEST26 sobre fuentes públicas empíricas, usando una puerta de perfil preregistrada y benchmarks preregistrados por perfil. A nivel dataset, 256 comparaciones alcanzaron `strong_win` con intervalo de confianza confirmado; 255 sobrevivieron la corrección Benjamini-Hochberg al 5%. La sensibilidad al 10% agregó solo dos filas, lo que indica que el resultado principal no depende de relajar la corrección por multiplicidad. La señal se concentró en perfiles normal-like, lognormal-like y Weibull-like, apoyando una interpretación de transferencia condicional de especialistas, no una afirmación de dominancia universal.



Una formulación breve para abstract/resultados:



> En la batería externa, los especialistas congelados produjeron 255 victorias dataset-level que sobrevivieron FDR 5%, todas con CI confirmado, distribuidas en 82 datasets y 12 candidatos. La evidencia apoya especialización transferible por régimen empírico y es consistente con una lectura No Free Lunch: los beneficios aparecen cuando el perfil del dataset coincide con el régimen de entrenamiento/validación del especialista.



Frases que conviene evitar:



- Evitar decir que el GA domina universalmente todos los datasets.

- Evitar contar las 2211 filas gate-level como descubrimientos independientes.

- Evitar presentar depth probes como nuevos datasets.

- Evitar mezclar oracle audit con el benchmark primario preregistrado.



# Material disponible para escritura



El repositorio ya contiene el paquete curado para sostener esta sección: tablas compactas, tabla de sobrevivientes FDR, registro de datasets, decisiones dataset-level, gate audit, oracle audit, condición-métrica granular partida y PDF/markdown de reporte. Los logs, checkpoints, downloads y recovery files quedaron fuera del repositorio y deben ir al archivo completo si se requiere reproducibilidad operacional.



|Archivo|Uso recomendado|
|---|---|
|external_primary_results_compact.csv|Tabla principal para conteos dataset-level.|
|external_fdr_survivors_dataset_level.csv|Tabla principal de ganadores estrictos y sensibilidad.|
|external_dataset_saturation_flags.csv|Control de saturación/depth probes.|
|external_evidence_grid.csv|Resumen cruzado de niveles taxonómicos de evidencia.|
|external_survival_map.csv|Mapa candidato-dominio para discutir transferencia.|
|external_dataset_registry.csv|Provenance, selección, load failures y perfiles empíricos.|
|external_dataset_level_decisions.csv|Auditoría completa de decisiones dataset-level.|
|external_gate_decisions_part*.csv|Auditoría de condición/gate; no usar como N independiente.|
|external_condition_metrics_part*.csv|Métricas granulares por condición y estimador.|
|external_oracle_audit_part*.csv|Comparación oracle secundaria; no claim primario.|
