/*******************************************************************************                          
								 Código 3:
							 Entropy Balance				
*******************************************************************************/

* -------------------------------------- *
* 0. Rutas
* -------------------------------------- *

global main "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/México/Paper_nietos_mex"

global out_folder  "$main/Output/EB"
global data_raw "$main/Data Raw"
global data_out "$main/Data Out"

* -------------------------------------- *
* 1. EB - Estimación por cohorte
* -------------------------------------- *

preserve
use "$data_out/estimacion_remesas_yr_coh2.dta", clear
keep inegi spanish_presence_1936_1955 spanish_presence_1956_1978
duplicates drop
tab spanish_presence_1936_1955 spanish_presence_1956_1978
tab spanish_presence_1936_1955
restore

use "$data_out/estimacion_remesas_yr_coh2.dta", clear

* Genero el promedio de remesas pre tratamiento
bys inegi: egen remesas_pre_avg = mean(total_remesas) if year<2022
bys inegi: egen log_remesas_pre_avg = mean(log_remesas) if year<2022

* Genero el log de la población
local poblacion poblacion_1990 poblacion_1995 poblacion_2000 poblacion_2005 poblacion_2010 poblacion_2015 poblacion_2020
foreach var of local poblacion {
    gen log_`var' = ln(`var')
}

* Genero el log de la densidad
gen log_densidad = ln(densidad)

keep inegi spanish_presence_1936_1955 spanish_presence_1956_1978 viv_emig_00 viv_emig_10 ///
hombres_pct mujeres_pct relacion_hm pea_total pea_pct log_densidad densidad sin_escolaridad_pct ///
esc_basica_pct esc_media_superior_pct esc_superior_pct ocupados_total ocupados_pct alfb_15_24_total ///
alfb_25mas_total alfb_15_24_pct alfb_25mas_pct ///
im_1990 im_1995 im_2000 im_2005 im_2010 im_2015 ///
poblacion_* log_poblacion_* ///
remesas_pre_avg log_remesas_pre_avg

duplicates drop
drop if remesas_pre_avg ==. & log_remesas_pre_avg ==.

tab spanish_presence_1936_1955 spanish_presence_1956_1978
tab spanish_presence_1936_1955
tab spanish_presence_1956_1978

*** 1.1) Balanceo presencia española por separado para cada ventanas
{
* 1.1.1) Presencia española la primera ventana

	* ALTERNATIVA 1: solo remesas
ebalance spanish_presence_1936_1955 log_remesas_pre_avg, targets(1) gen(w11_1)

	* ALTERNATIVA 2: parsimoniosa
ebalance spanish_presence_1936_1955 log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad, targets(1) gen(w11_2)

	* ALTERNATIVA 3: intermedia con temporalidad 
ebalance spanish_presence_1936_1955 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct, ///
         targets(1) gen(w11_3)

	* ALTERNATIVA 4: capital humano
ebalance spanish_presence_1936_1955 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct, targets(1) gen(w11_4)

	* --- Alternativas sin outcome pre --- *
	
	* ALTERNATIVA 5: marginalización 2010, densidad
ebalance spanish_presence_1936_1955 im_2010 log_densidad, targets(1) gen(w11_5)

	* ALTERNATIVA 6: marginalización 2010, densidad, migración 2010
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad, targets(1) gen(w11_6)
	
	* ALTERNATIVA 7: marginalización 2010, densidad, migracion 2010, poblacion 2010
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_poblacion_2010 log_densidad, targets(1) gen(w11_7)

	* ALTERNATIVA 8: marginalización 2010, densidad, migración 2010, pea
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad pea_pct, targets(1) gen(w11_8)

	* ALTERNATIVA 9: marginalización 2010, densidad, migración 2010, pea, escolaridad superior
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad pea_pct esc_superior_pct, targets(1) gen(w11_9)

	* ALTERNATIVA 10: marginalización 2010, densidad, migración 2010, pea, escolaridad media superior
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad pea_pct esc_media_superior_pct, targets(1) gen(w11_10)

	* ALTERNATIVA 11: marginalización 2010, densidad, migración 2010, pea, escolaridad básica
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad pea_pct esc_basica_pct, targets(1) gen(w11_11)

	* ALTERNATIVA 12: marginalización 2010, densidad, migración 2010, pea, sin escolaridad
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad pea_pct sin_escolaridad_pct, targets(1) gen(w11_12)

	* ALTERNATIVA 13: marginalización 2010, densidad, migración 2010, escolaridad superior
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad esc_superior_pct, targets(1) gen(w11_13)

	* ALTERNATIVA 14: marginalización 2010, densidad, migración 2010, escolaridad media superior
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad esc_media_superior_pct, targets(1) gen(w11_14)

	* ALTERNATIVA 15: marginalización 2010, densidad, migración 2010, escolaridad básica
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad esc_basica_pct, targets(1) gen(w11_15)

	* ALTERNATIVA 16: marginalización 2010, densidad, migración 2010, sin escolaridad
ebalance spanish_presence_1936_1955 viv_emig_10 im_2010 log_densidad sin_escolaridad_pct, targets(1) gen(w11_16)


* Tabla de mínimos y máximos de pesos de entropy balancing
tempfile resultados

postfile handle int alternativa str10 peso double min_peso max_peso ///
    using `resultados', replace

foreach i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 {
    
    capture confirm variable w11_`i'
    
    if _rc == 0 {
        quietly summarize w11_`i' if spanish_presence_1936_1955 == 0
        post handle (`i') ("w11_`i'") (r(min)) (r(max))
    }
    else {
        post handle (`i') ("w11_`i'") (.) (.)
    }
}

postclose handle

preserve
    use `resultados', clear

    label variable alternativa "Alternativa"
    label variable peso "Variable de peso"
    label variable min_peso "Mínimo"
    label variable max_peso "Máximo"

    export excel using "$out_folder/tabla_min_max_pesos_111.xlsx", ///
        firstrow(variables) replace
	
restore
	
* Análisis de las mejores alternativas (pesos máximos menos a 0.5)
	* Distribución de los pesos
histogram w11_5 if spanish_presence_1936_1955==0,  name(g1, replace) title("w 1.5") xtitle("") ytitle(Density)
histogram w11_6 if spanish_presence_1936_1955==0,  name(g2, replace) title("w 1.6") xtitle("") ytitle("")
histogram w11_8 if spanish_presence_1936_1955==0,  name(g3, replace) title("w 1.8") xtitle("") ytitle("")
histogram w11_9 if spanish_presence_1936_1955==0,  name(g4, replace) title("w 1.9") xtitle("") ytitle("")
histogram w11_10 if spanish_presence_1936_1955==0, name(g5, replace) title("w 1.10") xtitle(Entropy balancing weights) ytitle(Density)
histogram w11_11 if spanish_presence_1936_1955==0, name(g6, replace) title("w 1.11") xtitle(Entropy balancing weights) ytitle("")
histogram w11_12 if spanish_presence_1936_1955==0, name(g7, replace) title("w 1.12") xtitle(Entropy balancing weights) ytitle("")
histogram w11_14 if spanish_presence_1936_1955==0, name(g8, replace) title("w 1.14") xtitle(Entropy balancing weights) ytitle("")
graph combine g1 g2 g3 g4 g5 g6 g7 g8, col(4) ycommon title("Distribution of EB Weights – Spanish Presence, 1936–1955", size(*0.7))
graph export "$out_folder/weights_panels_111.png", replace

	* Estadísticas
estpost tabstat w11_5 w11_6 w11_8 w11_9 w11_10 w11_11 w11_12 w11_14, stats(mean min max v p1 p5 p10 p50 p90 p95 p99) columns(statistics)
esttab using "$out_folder/weights_summary_111.csv", ///
    cells("mean min max variance p1 p5 p10 p50 p90 p95 p99") ///
    noobs nonumber nomtitle nonote plain replace

	* Desbalance original
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(spanish_presence_1936_1955) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_111.csv", ///
    cells("mean(fmt(%9.3fc)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
    noobs nonumber nomtitle nonote plain unstack replace

* 1.1.1) Presencia española la SEGUNDA ventana

	* ALTERNATIVA 1: solo remesas
ebalance spanish_presence_1956_1978 log_remesas_pre_avg, targets(1) gen(w12_1)

	* ALTERNATIVA 2: parsimoniosa
ebalance spanish_presence_1956_1978 log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad, targets(1) gen(w12_2)

	* ALTERNATIVA 3: intermedia con temporalidad 
ebalance spanish_presence_1956_1978 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct, ///
         targets(1) gen(w12_3)

	* ALTERNATIVA 4: capital humano
ebalance spanish_presence_1956_1978 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct, targets(1) gen(w12_4)

	* --- Alternativas sin outcome pre --- *
	
	* ALTERNATIVA 5: marginalización 2010, densidad
ebalance spanish_presence_1956_1978 im_2010 log_densidad, targets(1) gen(w12_5)

	* ALTERNATIVA 6: marginalización 2010, densidad, migración 2010
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad, targets(1) gen(w12_6)
	
	* ALTERNATIVA 7: marginalización 2010, densidad, migracion 2010, poblacion 2010
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_poblacion_2010 log_densidad, targets(1) gen(w12_7)

	* ALTERNATIVA 8: marginalización 2010, densidad, migración 2010, pea
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad pea_pct, targets(1) gen(w12_8)

	* ALTERNATIVA 9: marginalización 2010, densidad, migración 2010, pea, escolaridad superior
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad pea_pct esc_superior_pct, targets(1) gen(w12_9)

	* ALTERNATIVA 10: marginalización 2010, densidad, migración 2010, pea, escolaridad media superior
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad pea_pct esc_media_superior_pct, targets(1) gen(w12_10)

	* ALTERNATIVA 11: marginalización 2010, densidad, migración 2010, pea, escolaridad básica
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad pea_pct esc_basica_pct, targets(1) gen(w12_11)

	* ALTERNATIVA 12: marginalización 2010, densidad, migración 2010, pea, sin escolaridad
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad pea_pct sin_escolaridad_pct, targets(1) gen(w12_12)

	* ALTERNATIVA 13: marginalización 2010, densidad, migración 2010, escolaridad superior
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad esc_superior_pct, targets(1) gen(w12_13)

	* ALTERNATIVA 14: marginalización 2010, densidad, migración 2010, escolaridad media superior
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad esc_media_superior_pct, targets(1) gen(w12_14)

	* ALTERNATIVA 15: marginalización 2010, densidad, migración 2010, escolaridad básica
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad esc_basica_pct, targets(1) gen(w12_15)

	* ALTERNATIVA 16: marginalización 2010, densidad, migración 2010, sin escolaridad
ebalance spanish_presence_1956_1978 viv_emig_10 im_2010 log_densidad sin_escolaridad_pct, targets(1) gen(w12_16)

* Tabla de mínimos y máximos de pesos de entropy balancing
tempfile resultados

postfile handle int alternativa str10 peso double min_peso max_peso ///
    using `resultados', replace

foreach i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 {
    
    capture confirm variable w12_`i'
    
    if _rc == 0 {
        quietly summarize w12_`i' if spanish_presence_1956_1978 == 0
        post handle (`i') ("w12_`i'") (r(min)) (r(max))
    }
    else {
        post handle (`i') ("w12_`i'") (.) (.)
    }
}

postclose handle

preserve
    use `resultados', clear

    label variable alternativa "Alternativa"
    label variable peso "Variable de peso"
    label variable min_peso "Mínimo"
    label variable max_peso "Máximo"

    export excel using "$out_folder/tabla_min_max_pesos_112.xlsx", ///
        firstrow(variables) replace
		
restore
	
	* Análisis de las mejores alternativas (pesos máximos menos a 0.5)
	* Distribución de los pesos
histogram w12_5 if spanish_presence_1956_1978==0,  name(g1, replace) title("w 1.5") xtitle("") ytitle(Density)
histogram w12_6 if spanish_presence_1956_1978==0,  name(g2, replace) title("w 1.6") xtitle("") ytitle(Density)
histogram w12_8 if spanish_presence_1956_1978==0,  name(g3, replace) title("w 1.8") xtitle("") ytitle("")
histogram w12_10 if spanish_presence_1956_1978==0, name(g4, replace) title("w 1.10") xtitle("") ytitle("")
histogram w12_11 if spanish_presence_1956_1978==0, name(g5, replace) title("w 1.11") xtitle(Entropy balancing weights) ytitle(Density)
histogram w12_12 if spanish_presence_1956_1978==0, name(g6, replace) title("w 1.12") xtitle(Entropy balancing weights) ytitle("")
histogram w12_16 if spanish_presence_1956_1978==0, name(g7, replace) title("w 1.16") xtitle(Entropy balancing weights) ytitle("")
graph combine g1 g2 g3 g4 g5 g6 g7, col(4) ycommon title("Distribution of EB Weights – Spanish Presence, 1956–1978", size(*0.7))
graph export "$out_folder/weights_panels_112.png", replace

	* Estadísticas
estpost tabstat w12_5 w12_6 w12_8 w12_10 w12_11 w12_12 w12_16, stats(mean min max v p1 p5 p10 p50 p90 p95 p99) columns(statistics)
esttab using "$out_folder/weights_summary_112.csv", ///
    cells("mean min max variance p1 p5 p10 p50 p90 p95 p99") ///
    noobs nonumber nomtitle nonote plain replace

	* Desbalance original
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(spanish_presence_1956_1978) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_112.csv", ///
    cells("mean(fmt(%9.3fc)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
    noobs nonumber nomtitle nonote plain unstack replace


}

*** 1.2) Balanceno presencia española (en cualquier ventana) vs sin presencia española
{
* Genero la dummy de presencia española en cualquiera de las dos ventanas
gen spanish_presence = (spanish_presence_1936_1955==1 | spanish_presence_1956_1978==1)

corr viv_emig_10 viv_emig_00 spanish_presence spanish_presence_1936_1955 spanish_presence_1956_1978
	
	* ALTERNATIVA 1: solo remesas
ebalance spanish_presence log_remesas_pre_avg, targets(1) gen(w2_1)

	* ALTERNATIVA 2: parsimoniosa
ebalance spanish_presence log_remesas_pre_avg viv_emig_10 im_2010  log_poblacion_2010 log_densidad, targets(1) gen(w2_2)

	* ALTERNATIVA 3: intermedia con temporalidad 
ebalance spanish_presence log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct, ///
targets(1) gen(w2_3)

	* ALTERNATIVA 4: capital humano
ebalance spanish_presence log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct, targets(1) gen(w2_4)

	* --- Alternativas sin outcome pre --- *
	
	* ALTERNATIVA 5: marginalización 2010, densidad
ebalance spanish_presence im_2010 log_densidad, targets(1) gen(w2_5)

	* ALTERNATIVA 6: marginalización 2010, densidad, migración 2010
ebalance spanish_presence viv_emig_10 im_2010 log_densidad, targets(1) gen(w2_6)

	* ALTERNATIVA 7: marginalización 2010, densidad, migracion 2010, poblacion 2010
ebalance spanish_presence viv_emig_10 im_2010 log_poblacion_2010 log_densidad, targets(1) gen(w2_7)
	
	* ALTERNATIVA 8: marginalización 2010, densidad, migración 2010, pea
ebalance spanish_presence viv_emig_10 im_2010 log_densidad pea_pct, targets(1) gen(w2_8)
	
	* ALTERNATIVA 9: marginalización 2010, densidad, migración 2010, pea, escolaridad superior
ebalance spanish_presence viv_emig_10 im_2010 log_densidad pea_pct esc_superior_pct, targets(1) gen(w2_9)

	* ALTERNATIVA 10: marginalización 2010, densidad, migración 2010, pea, escolaridad media superior
ebalance spanish_presence viv_emig_10 im_2010 log_densidad pea_pct esc_media_superior_pct, targets(1) gen(w2_10)

	* ALTERNATIVA 11: marginalización 2010, densidad, migración 2010, pea, escolaridad básica
ebalance spanish_presence viv_emig_10 im_2010 log_densidad pea_pct esc_basica_pct, targets(1) gen(w2_11)

	* ALTERNATIVA 12: marginalización 2010, densidad, migración 2010, pea, sin escolaridad
ebalance spanish_presence viv_emig_10 im_2010 log_densidad pea_pct sin_escolaridad_pct, targets(1) gen(w2_12)

	* ALTERNATIVA 13: marginalización 2010, densidad, migración 2010, escolaridad superior
ebalance spanish_presence viv_emig_10 im_2010 log_densidad esc_superior_pct, targets(1) gen(w2_13)

	* ALTERNATIVA 14: marginalización 2010, densidad, migración 2010, escolaridad media superior
ebalance spanish_presence viv_emig_10 im_2010 log_densidad esc_media_superior_pct, targets(1) gen(w2_14)

	* ALTERNATIVA 15: marginalización 2010, densidad, migración 2010, escolaridad básica
ebalance spanish_presence viv_emig_10 im_2010 log_densidad esc_basica_pct, targets(1) gen(w2_15)

	* ALTERNATIVA 16: marginalización 2010, densidad, migración 2010, sin escolaridad
ebalance spanish_presence viv_emig_10 im_2010 log_densidad sin_escolaridad_pct, targets(1) gen(w2_16)

* Tabla de mínimos y máximos de pesos de entropy balancing
tempfile resultados

postfile handle int alternativa str10 peso double min_peso max_peso ///
    using `resultados', replace

foreach i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 {
    
    capture confirm variable w2_`i'
    
    if _rc == 0 {
        quietly summarize w2_`i' if spanish_presence == 0
        post handle (`i') ("w2_`i'") (r(min)) (r(max))
    }
    else {
        post handle (`i') ("w2_`i'") (.) (.)
    }
}

postclose handle

preserve
    use `resultados', clear

    label variable alternativa "Alternativa"
    label variable peso "Variable de peso"
    label variable min_peso "Mínimo"
    label variable max_peso "Máximo"

    export excel using "$out_folder/tabla_min_max_pesos_12.xlsx", ///
        firstrow(variables) replace
	
restore
	
* Análisis de las mejores alternativas (pesos máximos menos a 0.5)
	* Distribución de los pesos
histogram w2_8 if spanish_presence==0, name(g1, replace) title("w 2.8") xtitle(Entropy balancing weights)
histogram w2_10 if spanish_presence==0, name(g2, replace) title("w 2.10") xtitle(Entropy balancing weights) ytitle("")
graph combine g1 g2, col(2) ycommon title("Distribution of EB Weights – Spanish Presence, Any Window", size(*0.7))
graph export "$out_folder/weights_panels_12_v2.png", replace

	* Estadísticas
estpost tabstat w2_8 w2_10, stats(mean min max v p1 p5 p10 p50 p90 p95 p99) columns(statistics)
esttab using "$out_folder/weights_summary_11_v2.csv", ///
    cells("mean min max variance p1 p5 p10 p50 p90 p95 p99") ///
    noobs nonumber nomtitle nonote plain replace

	* Desbalance original
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(spanish_presence) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_11.csv", ///
    cells("mean(fmt(%9.3fc)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
    noobs nonumber nomtitle nonote plain unstack replace
    
}

*** 1.3) Balanceno presencia española por ventanas (3 grupos)
{
	
/*
- Alternativas 1-4: version original trata de balancear con targets = 2 (media y varianza).
- Alternativas sin outcome pre para el balance: sumo las alternativas que resultaron mejor en 1.1. sin outcome pre para el balance. 
Intento balancear solo la media (targets = 1), siguiendo lo que hice en 1.1.

*/	
gen early_any = (spanish_presence_1936_1955==1)                                  // (20 municipios)
gen late_only = (spanish_presence_1936_1955==0 & spanish_presence_1956_1978==1)  // (7 municipios)  
gen never     = (spanish_presence_1936_1955==0 & spanish_presence_1956_1978==0)

	* ALTERNATIVA 1: solo remesas
		* Early any vs Never
ebalance early_any remesas_pre_avg if early_any==1 | never==1, targets(1) gen(w3_1_early) 
		* Late only vs Never
ebalance late_only remesas_pre_avg if late_only==1 | never==1, targets(1) gen(w3_1_late)

gen w3_1 = .
replace w3_1 = 1 if early_any==1 | late_only==1         // tratados pesan 1
replace w3_1 = (w3_1_early + w3_1_late)/2 if never==1  // controles promedian

	* ALTERNATIVA 2: parsimoniosa
		* Early any vs Never
ebalance early_any log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad if early_any==1 | never==1, ///
	targets(2) gen(w3_2_early) 
		* Late only vs Never
ebalance late_only log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad if late_only==1 | never==1, ///
	targets(2) gen(w3_2_late)
	
gen w3_2 = .
replace w3_2 = 1 if early_any==1 | late_only==1         // tratados pesan 1
replace w3_2 = (w3_2_early + w3_2_late)/2 if never==1  // controles promedian
	
	* ALTERNATIVA 3: intermedia con temporalidad 
		* Early any vs Never
*cap ebalance early_any log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
*	if early_any==1 | never==1, targets(2) gen(w3_3_early) 
ebalance early_any log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	if early_any==1 | never==1, targets(1) gen(w3_3_early)
		* Late only vs Never
ebalance late_only log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	if late_only==1 | never==1, targets(2) gen(w3_3_late)    // no converge
ebalance late_only log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	if late_only==1 | never==1, targets(1) gen(w3_3_late)	

gen w3_3 = .
replace w3_3 = 1 if early_any==1 | late_only==1         // tratados pesan 1
replace w3_3 = (w3_3_early + w3_3_late)/2 if never==1  // controles promedian
	
	* ALTERNATIVA 4: capital humano
		* Early any vs Never
*ebalance early_any log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
*	pea_pct sin_escolaridad_pct esc_basica_pct if early_any==1 | never==1, targets(2) gen(w3_4_early) 
ebalance early_any log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if early_any==1 | never==1, targets(1) gen(w3_4_early)
		* Late only vs Never
ebalance late_only log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if late_only==1 | never==1, targets(2) gen(w3_4_late)    // no converge
ebalance late_only log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if late_only==1 | never==1, targets(1) gen(w3_4_late)	

gen w3_4 = .
replace w3_4 = 1 if early_any==1 | late_only==1         // tratados pesan 1
replace w3_4 = (w3_4_early + w3_4_late)/2 if never==1  // controles promedian

// En este caso, todos los balances se lograron en el primer momento únicamente

* --- Alternativas sin outcome pre (las mejores de 1.1: 5 6 8 10 11 12)--- *

	* ALTERNATIVA 5: marginalización 2010, densidad
		* Early any vs Never
ebalance early_any im_2010 log_densidad if early_any==1 | never==1, targets(1) gen(w3_5_early)
		* Late only vs Never
ebalance late_only im_2010 log_densidad if late_only==1 | never==1, targets(1) gen(w3_5_late)

	* ALTERNATIVA 6: marginalización 2010, densidad, migración 2010
		* Early any vs Never
ebalance early_any viv_emig_10 im_2010 log_densidad if early_any==1 | never==1, targets(1) gen(w3_6_early)
		* Late only vs Never
ebalance late_only viv_emig_10 im_2010 log_densidad if late_only==1 | never==1, targets(1) gen(w3_6_late)

	* ALTERNATIVA 8: marginalización 2010, densidad, migración 2010, pea
		* Early any vs Never
ebalance early_any viv_emig_10 im_2010 log_densidad pea_pct if early_any==1 | never==1, targets(1) gen(w3_8_early)
		* Late only vs Never
ebalance late_only viv_emig_10 im_2010 log_densidad pea_pct if late_only==1 | never==1, targets(1) gen(w3_8_late)

	* ALTERNATIVA 10: marginalización 2010, densidad, migración 2010, pea, escolaridad media superior
		* Early any vs Never
ebalance early_any viv_emig_10 im_2010 log_densidad pea_pct esc_media_superior_pct if early_any==1 | never==1, targets(1) gen(w3_10_early)
		* Late only vs Never
ebalance late_only viv_emig_10 im_2010 log_densidad pea_pct esc_media_superior_pct if late_only==1 | never==1, targets(1) gen(w3_10_late)
		
	* ALTERNATIVA 11: marginalización 2010, densidad, migración 2010, pea, escolaridad básica
		* Early any vs Never
ebalance early_any viv_emig_10 im_2010 log_densidad pea_pct esc_basica_pct if early_any==1 | never==1, targets(1) gen(w3_11_early)
		* Late only vs Never
ebalance late_only viv_emig_10 im_2010 log_densidad pea_pct esc_basica_pct if late_only==1 | never==1, targets(1) gen(w3_11_late)
		
	* ALTERNATIVA 12: marginalización 2010, densidad, migración 2010, pea, sin escolaridad
		* Early any vs Never
ebalance early_any viv_emig_10 im_2010 log_densidad pea_pct sin_escolaridad_pct if early_any==1 | never==1, targets(1) gen(w3_12_early)
		* Late only vs Never
ebalance late_only viv_emig_10 im_2010 log_densidad pea_pct sin_escolaridad_pct if late_only==1 | never==1, targets(1) gen(w3_12_late)

* Análisis de las alternativas
	* Distribución de los pesos combinados
histogram w3_1, name(g1, replace) title("w 3.1") xtitle(Entropy balancing weights)
histogram w3_2, name(g2, replace) title("w 3.2") xtitle(Entropy balancing weights)
histogram w3_3, name(g3, replace) title("w 3.3") xtitle(Entropy balancing weights)
histogram w3_4, name(g4, replace) title("w 3.4") xtitle(Entropy balancing weights)
graph combine g1 g2 g3 g4, col(2)
graph export "$out_folder/weights_panels_13.png", replace

	* Distribución de los pesos EARLY vs NEVER de las alternativas sin outcome pre en el balance
histogram w3_5_early if early_any == 0, name(g1, replace) title("w 3.5") xtitle("") 
histogram w3_6_early if early_any == 0, name(g2, replace) title("w 3.6") xtitle("") ytitle("")
histogram w3_8_early if early_any == 0, name(g3, replace) title("w 3.8") xtitle("") ytitle("")
histogram w3_10_early if early_any == 0, name(g4, replace) title("w 3.10") xtitle(Entropy balancing weights) 
histogram w3_11_early if early_any == 0, name(g5, replace) title("w 3.11") xtitle(Entropy balancing weights) ytitle("")
histogram w3_12_early if early_any == 0, name(g6, replace) title("w 3.12") xtitle(Entropy balancing weights) ytitle("")
graph combine g1 g2 g3 g4 g5 g6, col(3) ycommon title("Distribution of EB Weights – Spanish Presence, Early vs Never", size(*0.7))
graph export "$out_folder/weights_panels_13_early.png", replace

	* Distribución de los pesos LATE vs NEVER de las alternativas sin outcome pre en el balance
histogram w3_5_late if late_only == 0, name(g1, replace) title("w 3.5") xtitle("") 
histogram w3_6_late if late_only == 0, name(g2, replace) title("w 3.6") xtitle("") ytitle("")
histogram w3_8_late if late_only == 0, name(g3, replace) title("w 3.8") xtitle("") ytitle("")
histogram w3_10_late if late_only == 0, name(g4, replace) title("w 3.10") xtitle(Entropy balancing weights) 
histogram w3_11_late if late_only == 0, name(g5, replace) title("w 3.11") xtitle(Entropy balancing weights) ytitle("")
histogram w3_12_late if late_only == 0, name(g6, replace) title("w 3.12") xtitle(Entropy balancing weights) ytitle("")
graph combine g1 g2 g3 g4 g5 g6, col(3) ycommon title("Distribution of EB Weights – Spanish Presence, Late vs Never", size(*0.7))
graph export "$out_folder/weights_panels_13_late.png", replace

	* Estadísticas de los pesos combinados
estpost tabstat w3_1 w3_2 w3_3 w3_4, stats(mean min max v p1 p5 p10 p50 p90 p95 p99) columns(statistics)
esttab using "$out_folder/weights_summary_13.csv", ///
    cells("mean min max variance p1 p5 p10 p50 p90 p95 p99") ///
    noobs nonumber nomtitle nonote plain replace

	* Estadísticas de los pesos EARLY vs NEVER de las alternativas sin outcome pre en el balance 
estpost tabstat w3_5_early w3_6_early w3_8_early w3_10_early w3_11_early w3_12_early, ///
		stats(mean min max v p1 p5 p10 p50 p90 p95 p99) columns(statistics)
esttab using "$out_folder/weights_summary_13_early.csv", ///
	   cells("mean min max variance p1 p5 p10 p50 p90 p95 p99") ///
       noobs nonumber nomtitle nonote plain replace
	
	* Estadísticas de los pesos EARLY vs NEVER de las alternativas sin outcome pre en el balance 
estpost tabstat w3_5_late w3_6_late w3_8_late w3_10_late w3_11_late w3_12_late, ///
		stats(mean min max v p1 p5 p10 p50 p90 p95 p99) columns(statistics)
esttab using "$out_folder/weights_summary_13_late.csv", ///
	   cells("mean min max variance p1 p5 p10 p50 p90 p95 p99") ///
       noobs nonumber nomtitle nonote plain replace
	
	* Desbalance original
gen early_any_never = .
	replace early_any_never = 1 if early_any==1 
	replace early_any_never = 0 if never==1
gen late_only_never = .
	replace late_only_never = 1 if late_only==1
	replace late_only_never = 0 if never==1
	
		* Early any vs never
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(early_any_never) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_13_early.csv", ///
			  cells("mean(fmt(%9.3fc)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
			  noobs nonumber nomtitle nonote plain unstack replace
		* Late only vs never
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(late_only_never) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_13_late.csv", ///
             cells("mean(fmt(%9.3fc)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
			 noobs nonumber nomtitle nonote plain unstack replace
			 
}

*** 1.4) Balanceno presencia española por ventanas (4 grupos)
// quedo viejo, cambaie los nombres para que hagan referencia a la seccion 1.4 si lo quiero volver a correr
if(0){
gen g1 = (spanish_presence_1936_1955==1 & spanish_presence_1956_1978==0)   // solo 36-55
gen g2 = (spanish_presence_1936_1955==0 & spanish_presence_1956_1978==1)   // solo 56-78
gen g3 = (spanish_presence_1936_1955==1 & spanish_presence_1956_1978==1)   // ambas
gen g0 = (spanish_presence_1936_1955==0 & spanish_presence_1956_1978==0)   // never-treated

	* ALTERNATIVA 1: solo remesas
		* G1 vs G0
ebalance g1 remesas_pre_avg if g1==1 | g0==1, targets(1) gen(w2_1_g1)
		* G2 vs G0  
ebalance g2 remesas_pre_avg if g2==1 | g0==1, targets(1) gen(w2_1_g2) 
		* G3 vs G0
ebalance g3 remesas_pre_avg if g3==1 | g0==1, targets(1) gen(w2_1_g3) 
	
gen w2_1 = .
replace w2_1 = 1 if g1==1 | g2==1 | g3==1                // tratados pesan 1
replace w2_1 = (w2_1_g1 + w2_1_g2 + w2_1_g3)/3 if g0==1  // controles promedian

	* ALTERNATIVA 2: parsimoniosa
		* G1 vs G0
ebalance g1 log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad if g1==1 | g0==1, targets(2) gen(w2_2_g1)
		* G2 vs G0  
ebalance g2 log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad if g2==1 | g0==1, targets(2) gen(w2_2_g2) // no converge
		* G3 vs G0
ebalance g3 log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad if g3==1 | g0==1, targets(2) gen(w2_2_g3) // no converge
ebalance g3 log_remesas_pre_avg viv_emig_10 im_2010 log_poblacion_2010 log_densidad if g3==1 | g0==1, targets(1) gen(w2_2_g3) // no converge

	* ALTERNATIVA 3: intermedia con temporalidad 
		* G1 vs G0
ebalance g1 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	densidad if g1==1 | g0==1, targets(2) gen(w2_3_g1)
		* G2 vs G0
ebalance g2 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	densidad if g2==1 | g0==1, targets(2) gen(w2_3_g2) // no converge
ebalance g2 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	densidad if g2==1 | g0==1, targets(1) gen(w2_3_g2) 
		* G3 vs G0
ebalance g3 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	densidad if g3==1 | g0==1, targets(2) gen(w2_3_g3) // no converge
ebalance g3 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 log_poblacion_2010 log_densidad pea_pct ///
	densidad if g3==1 | g0==1, targets(1) gen(w2_3_g3) // no converge
	
	* ALTERNATIVA 4: capital humano
		* G1 vs G0
ebalance g1 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if g1==1 | g0==1, targets(2) gen(w2_4_g1)
		* G2 vs G0
ebalance g2 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if g2==1 | g0==1, targets(2) gen(w2_4_g2)      // no converge
ebalance g2 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if g2==1 | g0==1, targets(1) gen(w2_4_g2)
		* G3 vs G0
ebalance g3 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if g3==1 | g0==1, targets(2) gen(w2_4_g3)      // no converge
ebalance g3 log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2010 log_poblacion_2010 log_densidad ///
	pea_pct sin_escolaridad_pct esc_basica_pct if g3==1 | g0==1, targets(1) gen(w2_4_g3)      // no converge

* Análisis de alternativa 1 (la única posible)
	* Distribución de los pesos
histogram w2_1, name(g1, replace) title("w 2.1") xtitle(Entropy balancing weights)
graph export "$out_folder/weights_panels_12.png", replace

	* Estadísticas
estpost tabstat w2_1, stats(mean min max v p1 p5 p10 p50 p90 p95 p99) columns(statistics)
esttab using "$out_folder/weights_summary_12.csv", ///
    cells("mean min max variance p1 p5 p10 p50 p90 p95 p99") ///
    noobs nonumber nomtitle nonote plain replace

	* Desbalance original
gen g1_g0 = .
	replace g1_g0 = 1 if g1==1
	replace g1_g0 = 0 if g0==1
gen g2_g0 = .
	replace g2_g0 = 1 if g2==1 
	replace g2_g0 = 0 if g0==1
gen g3_g0 = .
	replace g3_g0 = 1 if g3==1 
	replace g3_g0 = 0 if g0==1
	
		* g1 vs g0
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(g1_g0) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_12_g1.csv", ///
    cells("mean(fmt(%9.3f)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
    noobs nonumber nomtitle nonote plain unstack replace
		* g2 vs g0
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(g2_g0) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_12_g2.csv", ///
    cells("mean(fmt(%9.3f)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
    noobs nonumber nomtitle nonote plain unstack replace
		* g3 vs g0
estpost tabstat remesas_pre_avg log_remesas_pre_avg viv_emig_00 viv_emig_10 im_2000 im_2010 ///
                log_poblacion_2010 log_densidad pea_pct , ///
                by(g3_g0) stats(mean var N) columns(statistics)
esttab using "$out_folder/balance_summary_12_g3.csv", ///
    cells("mean(fmt(%9.3f)) variance(fmt(%20.3fc)) count(fmt(%9.0fc))") ///
    noobs nonumber nomtitle nonote plain unstack replace
}
	
*** Guardo la data con los pesos
keep inegi w*

merge 1:m inegi using "$data_out/estimacion_remesas_yr_coh2" // hay dos que no se unen porque no tienen data de remesas
drop _merge
order w*, last

save "$data_out/estimacion_remesas_yr_coh_eb.dta", replace


* -------------------------------------- *
* 2. EB - Estimación stock de españoles
* -------------------------------------- *
use "$data_out/estimacion_remesas_yr2.dta", clear
