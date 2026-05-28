/*******************************************************************************
								 Código 4:
						Synthetic Difference-in-Differences
*******************************************************************************/

/* -----------------------------------------------------------------------------
 NOTAS DE DISEÑO:

 1) TRATAMIENTO BINARIO. SDID necesita un tratamiento 0/1. Se usan las dummies
    de presencia española (spanish_presence_*). El ATT estimado es el efecto de
    TENER presencia española (margen extensivo).
	
 2) GRUPO DE CONTROL. En las ventanas c1 y c2 el contrafactual son SOLO los
    municipios never-treated puros: se excluyen los que tienen presencia
    únicamente en la OTRA ventana, para no mezclar en el contrafactual a los
    never-treated con tratados de la otra ventana.

 3) CONTROLES (versión "con controles"). En SDID las covariables X_it se
    parcializan (Y_res = Y - X*b) y deben VARIAR EN EL TIEMPO. viv_emig_10
    (migración a EEUU en 2010) es constante dentro del municipio -> la absorbe
    el efecto fijo de municipio y sdid la descarta. Para usarla como control hay
    que interactuarla con el año, igual que tu mejor especificación de DiD
    ( viv_emig_10[year] ). Eso genera regresores que sí varían en el tiempo.

 4) EVENT STUDY. SDID no produce el event study de forma automática: se arma con
    la receta de Clarke et al. (2024). El programa sdid_es (definido abajo)
    reestima sdid, recupera las series y los pesos temporales lambda, y construye
    el coeficiente por período con IC por bootstrap en bloques. El período de
    referencia NO es t-1: es el promedio pre ponderado por los lambda de SDID.
----------------------------------------------------------------------------- */

* -------------------------------------- *
* 0. Setup y rutas
* -------------------------------------- *
* Instalar paquetes una sola vez (descomentar):
* ssc install sdid, replace
* ssc install estout, replace

clear all
set more off

global main "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/México/Paper_nietos_mex"
global data_out   "$main/Data Out"
global out_folder "$main/Output/SynthDiD"
cap mkdir "$out_folder"

* -------------------------------------- *
* 0.1 Parámetros (modificables)
* -------------------------------------- *
global tyear   = 2022           // año de inicio del tratamiento (Ley de Nietos)
                                //   alternativa: 2021 (efecto anticipado)
global vcetype = "bootstrap"    // inferencia ATT: bootstrap | jackknife | placebo
global nreps   = 50             // repeticiones para el ATT (subir a 500+ p/ final)
global esreps  = 100            // repeticiones bootstrap del EVENT STUDY. Es la parte lenta (reestima sdid esreps
                                //   veces x 6 especificaciones). Bajar para probar.
global seed    = 1213

* -------------------------------------- *
* 1. Datos y panel balanceado
* -------------------------------------- *
use "$data_out/estimacion_remesas_yr_coh2.dta", clear

* id de grupo numérico (sdid lo necesita; inegi es string)
egen mun = group(inegi)

* sdid EXIGE panel BALANCEADO -> elimino municipios con años o con la
* variable dependiente faltante
drop if missing(log_remesas)
quietly summ year
global y0 = r(min)
global y1 = r(max)
global Ttot = $y1 - $y0 + 1
global Tpre = $tyear - $y0            // nº de períodos pre-tratamiento
bysort mun: gen _nobs = _N
keep if _nobs == $Ttot
drop _nobs

xtset mun year

* -------------------------------------- *
* 2. Controles time-varying: migración EEUU 2010 x año
* -------------------------------------- *
* Se omite el año de referencia (tyear-1) para evitar colinealidad con los
* efectos fijos de municipio.
global ref = `tyear' - 1
global migvars ""
forvalues y = $y0 / $y1 {
    if `y' != $ref {
        gen mig10_`y' = viv_emig_10 * (year == `y')
        global migvars $migvars mig10_`y'
    }
}

* -------------------------------------- *
* 2.5 Programa de EVENT STUDY estilo SDID  (receta de Clarke et al. 2024)
* -------------------------------------- *
capture program drop sdid_es
program define sdid_es
    syntax varlist(min=4 max=4), TYear(integer) TPre(integer) TTot(integer) ///
        Stub(string) [COVariates(string) B(integer 100) Seed(integer 1213) ///
        Lbl(string) YLABel(string)]

    tokenize `varlist'
    local y `1'
    local g `2'
    local t `3'
    local w `4'

    * opción de covariables (vacía => sin controles)
    local covopt
    if "`covariates'" != "" local covopt covariates(`covariates', projected)

    * opción de etiquetas del eje y (vacía => escala automática de Stata)
    local ylabopt
    if "`ylabel'" != "" local ylabopt ylabel(`ylabel')

    local yy0 = `tyear' - `tpre'
    local yy1 = `yy0' + `ttot' - 1

    *preserve
        * sdid dibuja un gráfico cada vez que se lo llama con la opción graph;
        * quietly NO suprime ese gráfico. set graphics off evita que se muestren
        * las (1 + B) corridas internas. Las matrices e() se calculan igual.
        set graphics off

        * --- 1. estimación puntual: series y pesos lambda ---
        quietly sdid `y' `g' `t' `w', vce(noinference) graph `covopt'
        matrix lambda = e(lambda)[1..`tpre',1]      // pesos temporales pre
        matrix yco    = e(series)[1..`tpre',2]      // serie control (pre)
        matrix ytr    = e(series)[1..`tpre',3]      // serie tratada  (pre)
        matrix aux    = -lambda'*(ytr - yco)        // baseline pre ponderado
        scalar meanpre_o = aux[1,1]
        matrix dif    = e(difference)[1..`ttot',1..2]
        svmat dif
        rename (dif1 dif2) (time d)
        quietly replace d = d + meanpre_o           // coef. del event study

        * --- 2. bootstrap en bloques (clúster = municipio) ---
        set seed `seed'
        local bb = 1
        di _n "  Event study bootstrap [`lbl']: " _continue
        while `bb' <= `b' {
            preserve
                bsample, cluster(`g') idcluster(_c2)
                quietly count if `w' == 0
                local r1 = r(N)
                quietly count if `w' != 0
                local r2 = r(N)
                if (`r1' != 0 & `r2' != 0) {
                    quietly sdid `y' _c2 `t' `w', vce(noinference) graph `covopt'
                    matrix lambda_b  = e(lambda)[1..`tpre',1]
                    matrix yco_b     = e(series)[1..`tpre',2]
                    matrix ytr_b     = e(series)[1..`tpre',3]
                    matrix aux_b     = -lambda_b'*(ytr_b - yco_b)
                    matrix meanpre_b = J(`ttot',1,aux_b[1,1])
                    matrix d`bb'     = e(difference)[1..`ttot',2] + meanpre_b
                    local ++bb
                    if mod(`bb',25)==0 di "`bb' " _continue
                }
            restore
        }

        * --- 3. IC al 95% y gráfico ---
        keep time d
        keep if time != .
        forvalues bb = 1/`b' {
            svmat d`bb'
        }
        egen rsd = rowsd(d11 - d`b'1)               // SE bootstrap por período
        gen LCI = d + invnormal(0.025)*rsd
        gen UCI = d - invnormal(0.025)*rsd

        * reactivo la pantalla para ver el gráfico final del event study
        set graphics on

        twoway rarea UCI LCI time, color(gray%40)                          ///
            || scatter d time, color(black) m(d)                           ///
            ||, xtitle("") ytitle("Log remittances (Tratment - Control)")        ///
               title("`lbl'", size(*0.7))                                              ///
               xlab(`yy0'(1)`yy1', angle(45))                              ///
               `ylabopt'                                                   ///
               legend(order(2 "Estimación puntual" 1 "IC 95%") pos(12) col(2)) ///
               xline(`tyear', lc(black) lp(solid))                         ///
               yline(0, lc(black) lp(shortdash)) scheme(stsj)
        graph export "`stub'_eventstudy.png", replace
    *restore
end

* -------------------------------------- *
* 3. Estimación SDID + event study
* -------------------------------------- *
* Tres definiciones de tratamiento:
*   pooled : presencia española en CUALQUIER ventana
*   c1     : presencia en la ventana 1936-1955
*   c2     : presencia en la ventana 1956-1978

* Trends
eststo clear

foreach spec in pooled c1 c2 {

    preserve

        * --- tratados y grupo de control ---
        if "`spec'" == "pooled" {
            gen byte _trt = (spanish_presence_1936_1955==1 | ///
                             spanish_presence_1956_1978==1)
			
			local name_spec "Tratment: Any Window"
        }
        else if "`spec'" == "c1" {
            * excluyo municipios con presencia SOLO en la ventana 1956-1978
            drop if spanish_presence_1936_1955==0 & spanish_presence_1956_1978==1
            gen byte _trt = (spanish_presence_1936_1955==1)
			
			local name_spec "Tratment: Window 1936-1955"
        }
        else if "`spec'" == "c2" {
            * excluyo municipios con presencia SOLO en la ventana 1936-1955
            drop if spanish_presence_1956_1978==0 & spanish_presence_1936_1955==1
            gen byte _trt = (spanish_presence_1956_1978==1)
			
			local name_spec "Tratment: Window 1956-1978"
        }

        * variable de tratamiento 0/1 acumulada que pide sdid
        gen byte treat = (_trt==1 & year >= $tyear )

        di _n(2) "{hline 72}"
        di "  SDID  |  spec = `spec'  |  tratamiento desde `tyear'"
        di "{hline 72}"

        * ===== (a) SIN controles =====
        eststo sdid_`spec'_nc: sdid log_remesas mun year treat, ///
            vce($vcetype ) seed($seed ) reps($nreps ) ///
            graph mattitles ///
            g2_opt(ytitle("Log remittances") ///
			      title("Remittances Trends - `name_spec'", size(*0.7)) ///
				  xtitle("Year") scheme(stsj)) ///
            graph_export("$out_folder/sdid_`spec'_sincontrol_", .png)

        matlist e(lambda), title("Pesos temporales lambda - `spec' sin controles")

        * sdid_es log_remesas mun year treat, ///
            tyear($tyear ) tpre($Tpre ) ttot($Ttot ) ///
            b($esreps ) seed($seed ) ///
            lbl("Event study SDID - `spec' (sin controles)") ///
            stub("$out_folder/sdid_`spec'_sincontrol")

        * ===== (b) CON controles (migración EEUU 2010 x año) =====
        eststo sdid_`spec'_c: sdid log_remesas mun year treat, ///
            vce($vcetype ) seed($seed ) reps($nreps ) ///
            covariates($migvars , projected) ///
            graph mattitles ///
            g2_opt(ytitle("Log remittances") ///
			       title("Remittances Trends - `name_spec' - With Controls", size(*0.7)) ///
				   xtitle("Year") scheme(stsj)) ///
            graph_export("$out_folder/sdid_`spec'_concontrol_", .png)

        matlist e(lambda), title("Pesos temporales lambda - `spec' con controles")

        * sdid_es log_remesas mun year treat, ///
            tyear($tyear) tpre($Tpre ) ttot($Ttot ) ///
            covariates($migvars ) b($esreps) seed(`seed ) ///
            lbl("Event study SDID - `spec' (con controles)") ///
            stub("$out_folder/sdid_`spec'_concontrol")

    restore
}


* Event Study
foreach spec in pooled c1 c2 {

    preserve

        * --- tratados y grupo de control ---
        if "`spec'" == "pooled" {
            gen byte _trt = (spanish_presence_1936_1955==1 | ///
                             spanish_presence_1956_1978==1)
			
			local name_spec "Tratment: Any Window"
        }
        else if "`spec'" == "c1" {
            * excluyo municipios con presencia SOLO en la ventana 1956-1978
            drop if spanish_presence_1936_1955==0 & spanish_presence_1956_1978==1
            gen byte _trt = (spanish_presence_1936_1955==1)
			
			local name_spec "Tratment: Window 1936-1955"
        }
        else if "`spec'" == "c2" {
            * excluyo municipios con presencia SOLO en la ventana 1936-1955
            drop if spanish_presence_1956_1978==0 & spanish_presence_1936_1955==1
            gen byte _trt = (spanish_presence_1956_1978==1)
			
			local name_spec "Tratment: Window 1956-1978"
        }

        * variable de tratamiento 0/1 acumulada que pide sdid
        gen byte treat = (_trt==1 & year >= $tyear )

        di _n(2) "{hline 72}"
        di "  SDID  |  spec = `spec'  |  tratamiento desde `tyear'"
        di "{hline 72}"

        * ===== (a) SIN controles =====
       sdid_es log_remesas mun year treat, ///
            tyear($tyear ) tpre($Tpre ) ttot($Ttot ) ///
            b($esreps ) seed($seed ) ///
            lbl("Event study SDID - `name_spec'") ///
            stub("$out_folder/sdid_`spec'_sincontrol")

        * ===== (b) CON controles (migración EEUU 2010 x año) =====
        * sdid_es log_remesas mun year treat, ///
            tyear($tyear) tpre($Tpre ) ttot($Ttot ) ///
            covariates($migvars ) b($esreps) seed(`seed ) ///
            lbl("Event study SDID - `spec' (con controles)") ///
            stub("$out_folder/sdid_`spec'_concontrol")

    restore
}


* -------------------------------------- *
* 4. Tabla resumen de resultados (ATT)
* -------------------------------------- *
esttab sdid_pooled_nc sdid_pooled_c sdid_c1_nc sdid_c1_c sdid_c2_nc sdid_c2_c ///
    using "$out_folder/sdid_resultados.csv", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) b(%9.4f) se(%9.4f) ///
    mtitles("Pool s/c" "Pool c/c" "V1 s/c" "V1 c/c" "V2 s/c" "V2 c/c") ///
    title("SDID - efecto Ley de Nietos sobre log remesas")

esttab sdid_pooled_nc sdid_pooled_c sdid_c1_nc sdid_c1_c sdid_c2_nc sdid_c2_c, ///
    se star(* 0.10 ** 0.05 *** 0.01) b(%9.4f) se(%9.4f) ///
    mtitles("Pool s/c" "Pool c/c" "V1 s/c" "V1 c/c" "V2 s/c" "V2 c/c")

* fin
