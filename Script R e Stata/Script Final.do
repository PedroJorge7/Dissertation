******************************************************************************** 
** RDPLOT
** DISSERTAÇÃO PPGE - 2019 
** Orientador: Jevuks Matheus
** Orientando: Pedro Jorge 
********************************************************************************
******************************************************************************** 
** RDROBUST Stata Package ** Do-file for Empirical Illustration
** Authors: Sebastian Calonico, Matias D. Cattaneo, Max H. Farrell and Rocio Titiunik 
********************************************************************************
** hlp2winpdf, cdn(rdrobust) append
** hlp2winpdf, cdn(rdbwselect) append
** hlp2winpdf, cdn(rdplot) append  
********************************************************************************
** net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) append
********************************************************************************

cd "F:\Pedro Jorge\UFPB\Mestrado\Dissertação\R"
set more off, permanently
use Base_de_Dados_Final.dta, clear

* Resultado principal: Ano eleitoral (2016)

drop if ano < 2013
drop if ano > 2016

* cd "F:\Pedro Jorge\UFPB\Mestrado\Dissertação\R\Resultados"

* Resultado Secundário: Gestão 2013-2016

*Generating the normalized percent score (pscore)
gen pscore=pop if pop>7500 & pop<=23772
replace pscore=((pop-10188)/10188) if pop>7500 & pop<=11800
replace pscore=((pop-13584)/13584) if pop>11800 & pop<=15100
replace pscore=((pop-16980)/16980) if pop>15100 & pop<=23772
replace pscore=pscore*100
la var pscore "X"

*Generating segment indicators
gen seg1=0
replace seg1=1 if pop>7500 & pop<=11800
gen seg2=0
replace seg2=1 if pop>11800 & pop<=15100
gen seg3=0
replace seg3=1 if pop>15100 & pop<=23772

*Generating segment variable
gen seg=1 if pop>7500 & pop<=11800
replace seg=2 if pop>11800 & pop<=15100
replace seg=3 if pop>15100 & pop<=23772

*Generating polynomials in the normalized running variable and interactions
gen aaa=0 if pop>7500 & pop<=44148
replace aaa=1 if pscore>0
la var aaa "I[X>0]"
gen pscore_a=pscore*aaa
gen pscore_sq=(pscore)^2
gen pscore_a_sq=pscore_sq*aaa
gen pscore_cu=(pscore)^3
gen pscore_a_cu=pscore_cu*aaa
gen pscore_qu=(pscore)^4
gen pscore_a_qu=pscore_qu*aaa

gen pscore_2=pscore*seg2
gen pscore_2_a=pscore*aaa*seg2
gen pscore_2_sq=pscore_2^2
gen pscore_2_a_sq=pscore_2_sq*aaa
gen pscore_2_cu=pscore_2^3
gen pscore_2_a_cu=pscore_2_cu*aaa
gen pscore_2_qu=pscore_2^4
gen pscore_2_a_qu=pscore_2_qu*aaa

gen pscore_3=pscore*seg3
gen pscore_3_a=pscore*aaa*seg3
gen pscore_3_sq=pscore_3^2
gen pscore_3_a_sq=pscore_3_sq*aaa
gen pscore_3_cu=pscore_3^3
gen pscore_3_a_cu=pscore_3_cu*aaa
gen pscore_3_qu=pscore_3^4
gen pscore_3_a_qu=pscore_3_qu*aaa

*generating dummies for pscore bins (1%)
gen e82_pscore_1=floor(pscore)+ 0.5 if pop>7500 & pop<=23772 

*Generating square 1980 covariates
*gen educ25_a80_sq = educ25_a80^2
*gen urban80_sq = urban80^2
*gen net_enrol7_1480_sq = net_enrol7_1480^2
*gen illiteracy15_a80_sq = illiteracy15_a80^2
*gen poverty80_sq = poverty80^2
*gen pcapincome80_sq = pcapincome80^2
*gen infantmortality80_sq = infantmortality80^2

******* Teste de placebo

*Generating the normalized percent score (pscore)
gen pscore_placebo=pop if pop>500 & pop<=23772
replace pscore_placebo=((pop-5000)/5000) if pop>500 & pop<=8000
replace pscore_placebo=((pop-15000)/15000) if pop>13000 & pop<=15000
replace pscore_placebo=((pop-20000)/20000) if pop>18000 & pop<=22772
replace pscore_placebo=pscore_placebo*100
la var pscore "X"

gen seg1_placebo=0
replace seg1_placebo=1 if pop>500 & pop<=7000
gen seg2_placebo=0
replace seg2_placebo=1 if pop>14000 & pop<=17000
gen seg3_placebo=0
replace seg3_placebo=1 if pop>18000 & pop<=23772

gen bbb=0 if pop>500 & pop<=44148
replace bbb=1 if pscore_placebo>0
la var bbb "I[X>0]"
gen pscore_placebo_a=pscore_placebo*bbb
gen pscore_placebo_sq=(pscore_placebo)^2
gen pscore_placebo_a_sq=pscore_placebo_sq*bbb
gen pscore_placebo_cu=(pscore_placebo)^3
gen pscore_placebo_a_cu=pscore_placebo_cu*bbb
gen pscore_placebo_qu=(pscore_placebo)^4
gen pscore_placebo_a_qu=pscore_placebo_qu*bbb

gen pscore_placebo_2=pscore_placebo*seg2
gen pscore_placebo_2_a=pscore_placebo*bbb*seg2
gen pscore_placebo_2_sq=pscore_placebo_2^2
gen pscore_placebo_2_a_sq=pscore_placebo_2_sq*bbb
gen pscore_placebo_2_cu=pscore_placebo_2^3
gen pscore_placebo_2_a_cu=pscore_placebo_2_cu*bbb
gen pscore_placebo_2_qu=pscore_placebo_2^4
gen pscore_placebo_2_a_qu=pscore_placebo_2_qu*bbb

gen pscore_placebo_3=pscore_placebo*seg3
gen pscore_placebo_3_a=pscore_placebo*bbb*seg3
gen pscore_placebo_3_sq=pscore_placebo_3^2
gen pscore_placebo_3_a_sq=pscore_placebo_3_sq*bbb
gen pscore_placebo_3_cu=pscore_placebo_3^3
gen pscore_placebo_3_a_cu=pscore_placebo_3_cu*bbb
gen pscore_placebo_3_qu=pscore_placebo_3^4
gen pscore_placebo_3_a_qu=pscore_placebo_3_qu*bbb

*generating dummies for pscore_placebo bins (1%)
gen e82_pscore_placebo_1=floor(pscore_placebo)+ 0.5 if pop>3000 & pop<=23772

cd "F:\Pedro Jorge\UFPB\Mestrado\Dissertação\R\Resultados principais"

* Teste

xi: reg fpm_real seg2_placebo seg3_placebo i.id if pscore_placebo>-10 & pscore_placebo<10 & seg1_placebo == 1,r
predict fpm_real_res,res
reg fpm_real_res bbb pscore_placebo pscore_placebo_a if pscore_placebo>-10 & pscore_placebo<10 & seg1_placebo == 1,r
predict fpm_real_hat
egen fpm_real_hat_1=mean(fpm_real_res)if pscore_placebo>-10 & pscore_placebo<10 & seg1_placebo == 1,by(e82_pscore_placebo_1)
scatter fpm_real_hat_1 e82_pscore_placebo_1 if pscore_placebo>-10 & pscore_placebo<10 & seg1_placebo == 1, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line fpm_real_hat pscore_placebo if pscore_placebo>-10 & pscore_placebo<10, title("Log Transfênrecias do FPM per capita") xtit("Distância") lwidth(medthick) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") sort saving(fpm_real_teste1,replace) legend(off) graphregion(fcol(white))  bgcolor(white)
drop fpm_real_res
drop fpm_real_hat
drop fpm_real_hat_1

xi: reg fpm_real seg2_placebo seg3_placebo i.id if pscore_placebo>-10 & pscore_placebo<10 & seg3_placebo == 1,r
predict fpm_real_res,res
reg fpm_real_res bbb pscore_placebo pscore_placebo_a if pscore_placebo>-10 & pscore_placebo<10 & seg3_placebo == 1,r
predict fpm_real_hat
egen fpm_real_hat_1=mean(fpm_real_res)if pscore_placebo>-10 & pscore_placebo<10 & seg3_placebo == 1,by(e82_pscore_placebo_1)
scatter fpm_real_hat_1 e82_pscore_placebo_1 if pscore_placebo>-10 & pscore_placebo<10 & seg3_placebo == 1, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line fpm_real_hat pscore_placebo if pscore_placebo>-10 & pscore_placebo<10, title("Log Transfênrecias do FPM per capita") xtit("Distância") lwidth(medthick) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") sort saving(fpm_real_teste2,replace) legend(off) graphregion(fcol(white))  bgcolor(white)
drop fpm_real_res
drop fpm_real_hat
drop fpm_real_hat_1

graph combine fpm_real_teste1.gph fpm_real_teste2.gph

***** Teste 2

xi: reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo i.id  if seg1_placebo == 1 & pscore_placebo>-1 & pscore_placebo<1
estimates store a1
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-5 & pscore_placebo<5
estimates store a5
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-10 & pscore_placebo<10
estimates store a10
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-15 & pscore_placebo<15
estimates store a15
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-20 & pscore_placebo<20
estimates store a20
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-25 & pscore_placebo<25
estimates store a25
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-30 & pscore_placebo<30
estimates store a30
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-35 & pscore_placebo<35
estimates store a35
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-40 & pscore_placebo<40
estimates store a40
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-45 & pscore_placebo<45
estimates store a45
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-50 & pscore_placebo<50
estimates store a50
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-55 & pscore_placebo<55
estimates store a55
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-60 & pscore_placebo<60
estimates store a60
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-65 & pscore_placebo<65
estimates store a65
xi: quietly reg fpm_real bbb pscore_placebo pscore_placebo_a seg2_placebo seg3_placebo  i.id  if seg1_placebo == 1 & pscore_placebo>-70 & pscore_placebo<70
estimates store a70
coefplot (a1) (a5) (a10) (a15) (a20) (a25) (a30) (a35) (a40) (a45) (a50) (a60) (a65) (a70), keep(bbb) xline(0)

** Estatística descritiva

cd "F:\Pedro Jorge\UFPB\Mestrado\Dissertação\R\Resultados"

foreach x in receita_pib despesa_pib fpm_pct outras_transf_pct estado_pct tributario_pct Previdencia_pct seg_publica_pct agricultura_pct encargos_esp_pct desporto_lazer_pct transporte_pct outras_transf_pct edu_pct saude_pct legislativo_pct adm_pct pessoal_pct capital_pct IFGF IFGF_autonomia IFGF_investimento IFGF_liquidez IFGF_pessoal IFDM pib empregados emp pib Poss_Rei {
		est clear
		eststo `x'col1: qui mean `x' if pop>7500 & pop<44148 & pscore > 0
		eststo `x'col2: qui mean `x' if pop>8500 & pop<18700 & pscore > 0
		eststo `x'col3: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 1 & pscore > 0
		eststo `x'col4: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 2 & pscore > 0
		eststo `x'col5: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 3 & pscore > 0
		eststo `x'col6: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 4 & pscore > 0
		eststo `x'col7: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 5 & pscore > 0
		estout `x'col*, cells("b(fmt(2))") mlabels(,none)
		}

foreach  x in receita_pib despesa_pib fpm_pct outras_transf_pct estado_pct tributario_pct Previdencia_pct seg_publica_pct agricultura_pct encargos_esp_pct desporto_lazer_pct transporte_pct outras_transf_pct edu_pct saude_pct legislativo_pct adm_pct pessoal_pct capital_pct IFGF IFGF_autonomia IFGF_investimento IFGF_liquidez IFGF_pessoal IFDM pib empregados emp pib Poss_Rei {
		est clear
		eststo `x'col1: qui mean `x' if pop>7500 & pop<44148 & pscore <= 0
		eststo `x'col2: qui mean `x' if pop>8500 & pop<18700 & pscore <= 0
		eststo `x'col3: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 1 & pscore <= 0
		eststo `x'col4: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 2 & pscore <= 0
		eststo `x'col5: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 3 & pscore <= 0
		eststo `x'col6: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 4 & pscore <= 0
		eststo `x'col7: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 5 & pscore <= 0
		estout `x'col*, cells("b(fmt(2))") mlabels(,none)
		}
		
foreach  x in receita_pib despesa_pib fpm_pct outras_transf_pct estado_pct tributario_pct Previdencia_pct seg_publica_pct agricultura_pct encargos_esp_pct desporto_lazer_pct transporte_pct outras_transf_pct edu_pct saude_pct legislativo_pct adm_pct pessoal_pct capital_pct IFGF IFGF_autonomia IFGF_investimento IFGF_liquidez IFGF_pessoal IFDM pib empregados emp pib Poss_Rei {
		est clear
		eststo `x'col1: qui mean `x' if pop>7500 & pop<44148
		eststo `x'col2: qui mean `x' if pop>8500 & pop<18700
		eststo `x'col3: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 1
		eststo `x'col4: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 2
		eststo `x'col5: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 3
		eststo `x'col6: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 4
		eststo `x'col7: qui mean `x' if pop>8500 & pop<18700 & cod_regiao == 5
		estout `x'col*, cells("b(fmt(2))") mlabels(,none)
		}

		* TESTE DE MANIPULAÇÃO
 
rddensity pop if pscore>-2 & pscore<2 , c(10188)
rddensity pop if pscore>-3 & pscore<3 , c(10188)
rddensity pop if pscore>-4 & pscore<4 , c(10188)
rddensity pop if pscore>-10 & pscore<10 , c(10188)
rddensity pop if pscore>-15 & pscore<15 , c(10188)

rddensity pop if pscore>-2 & pscore<2 , c(13584)
rddensity pop if pscore>-3 & pscore<3 , c(13584)
rddensity pop if pscore>-4 & pscore<4 , c(13584)
rddensity pop if pscore>-10 & pscore<10 , c(13584)
rddensity pop if pscore>-15 & pscore<15 , c(13584)

rddensity pop if pscore>-2 & pscore<2 , c(16980)
rddensity pop if pscore>-3 & pscore<3 , c(16980)
rddensity pop if pscore>-4 & pscore<4 , c(16980)
rddensity pop if pscore>-10 & pscore<10 , c(16980)
rddensity pop if pscore>-15 & pscore<15 , c(16980)

* GRÁFICO MANIPULAÇÃO

rddensity pop if pscore>-2 & pscore<2, plot c(10188)
graph save 1_2, append
rddensity pop if pscore>-3 & pscore<3, plot c(10188)
graph save 1_3, append
rddensity pop if pscore>-4 & pscore<4, plot c(10188)
graph save 1_4, append
rddensity pop if pscore>-10 & pscore<10 , plot c(10188)
graph save 1_10, append
rddensity pop if pscore>-15 & pscore<15 , plot c(10188)
graph save 1_15, append

graph combine 1_2.gph 1_3.gph 1_4.gph 1_10.gph 1_15.gph 
graph export graph_teste1.pdf	

rddensity pop if pscore>-2 & pscore<2 ,plot c(13584)
graph save 2_2, append
rddensity pop if pscore>-3 & pscore<3 ,plot c(13584)
graph save 2_3, append
rddensity pop if pscore>-4 & pscore<4 ,plot c(13584)
graph save 2_4, append
rddensity pop if pscore>-10 & pscore<10 ,plot c(13584)
graph save 2_10, append
rddensity pop if pscore>-15 & pscore<15 ,plot c(13584)
graph save 2_15, append

graph combine 2_2.gph 2_3.gph 2_4.gph 2_10.gph 2_15.gph 
graph export graph_teste2.pdf

rddensity pop if pscore>-2 & pscore<2 ,plot c(16980)
graph save 3_2, append
rddensity pop if pscore>-3 & pscore<3 ,plot c(16980)
graph save 3_3, append
rddensity pop if pscore>-4 & pscore<4 ,plot c(16980)
graph save 3_4, append
rddensity pop if pscore>-10 & pscore<10 ,plot c(16980)
graph save 3_10, append
rddensity pop if pscore>-15 & pscore<15 ,plot c(16980)
graph save 3_15, append

graph combine 3_2.gph 3_3.gph 3_4.gph 3_10.gph 3_15.gph 
graph export graph_teste3.pdf

		
* FPM
	
xi: reg fpm_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict fpm_real_res,res
reg fpm_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict fpm_real_hat
egen fpm_real_hat_1=mean(fpm_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter fpm_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line fpm_real_hat pscore if pscore>-10 & pscore<10, title("Log Transferências do FPM per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(fpm_real,replace) graphregion(fcol(white))  bgcolor(white)
drop fpm_real_res
drop fpm_real_hat
drop fpm_real_hat_1

* Estado

xi: reg estado_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict estado_real_res,res
reg estado_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict estado_real_hat
egen estado_real_hat_1=mean(estado_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter estado_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line estado_real_hat pscore if pscore>-10 & pscore<10, title("Log Transferências estaduais per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(estado_real,replace) graphregion(fcol(white))  bgcolor(white)
drop estado_real_res
drop estado_real_hat
drop estado_real_hat_1

* Não FPM

xi: reg outras_transf_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict outras_transf_real_res,res
reg outras_transf_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict outras_transf_real_hat
egen outras_transf_real_hat_1=mean(outras_transf_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter outras_transf_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line outras_transf_real_hat pscore if pscore>-10 & pscore<10, title("Log Outras Transferências per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(outras_transf_real,replace) graphregion(fcol(white))  bgcolor(white)
drop outras_transf_real_res
drop outras_transf_real_hat
drop outras_transf_real_hat_1

* Tributario

xi: reg tributario_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict tributario_real_res,res
reg tributario_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict tributario_real_hat
egen tributario_real_hat_1=mean(tributario_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter tributario_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line tributario_real_hat pscore if pscore>-10 & pscore<10, title("Log Receita Própria per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(tributario_real,replace) graphregion(fcol(white))  bgcolor(white)
drop tributario_real_res
drop tributario_real_hat
drop tributario_real_hat_1

graph combine fpm_real.gph estado_real.gph outras_transf_real.gph tributario_real.gph
graph export graph1.pdf		
		
* Gráficos Iniciais

xi: reg receita_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict receita_real_res,res
reg receita_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict receita_real_hat
egen receita_real_hat_1=mean(receita_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter receita_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line receita_real_hat pscore if pscore>-10 & pscore<10, title("Log Receita Total per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(receita_real,replace) graphregion(fcol(white))  bgcolor(white)
drop receita_real_res
drop receita_real_hat
drop receita_real_hat_1

xi: reg despesa_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict despesa_real_res,res
reg despesa_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict despesa_real_hat
egen despesa_real_hat_1=mean(despesa_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter despesa_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line despesa_real_hat pscore if pscore>-10 & pscore<10, title("Log Despesa Total per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(despesa_real,replace) graphregion(fcol(white))  bgcolor(white)
drop despesa_real_res
drop despesa_real_hat
drop despesa_real_hat_1

graph combine receita_real.gph despesa_real.gph

* Gráficos : Principais Resultados

* Edu

xi: reg edu_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict edu_real_res,res
reg edu_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict edu_real_hat
egen edu_real_hat_1=mean(edu_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter edu_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line edu_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Educação per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(edu_real,replace) graphregion(fcol(white))  bgcolor(white)
drop edu_real_res
drop edu_real_hat
drop edu_real_hat_1

* Saúde

xi: reg saude_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict saude_real_res,res
reg saude_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict saude_real_hat
egen saude_real_hat_1=mean(saude_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter saude_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line saude_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Saúde per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(saude_real,replace) graphregion(fcol(white))  bgcolor(white)
drop saude_real_res
drop saude_real_hat
drop saude_real_hat_1

* Admnistrativo

xi: reg adm_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict adm_real_res,res
reg adm_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict adm_real_hat
egen adm_real_hat_1=mean(adm_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter adm_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line adm_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Administrativo per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(adm_real,replace) graphregion(fcol(white))  bgcolor(white)
drop adm_real_res
drop adm_real_hat
drop adm_real_hat_1

* Legislativo

xi: reg legislativo_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict legislativo_real_res,res
reg legislativo_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict legislativo_real_hat
egen legislativo_real_hat_1=mean(legislativo_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter legislativo_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line legislativo_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Legislativo per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(legislativo_real,replace) graphregion(fcol(white))  bgcolor(white)
drop legislativo_real_res
drop legislativo_real_hat
drop legislativo_real_hat_1

* Urbanismo

xi: reg urbanismo_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict urbanismo_real_res,res
reg urbanismo_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict urbanismo_real_hat
egen urbanismo_real_hat_1=mean(urbanismo_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter urbanismo_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line urbanismo_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Urbanismo per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(urbanismo_real,replace) graphregion(fcol(white))  bgcolor(white)
drop urbanismo_real_res
drop urbanismo_real_hat
drop urbanismo_real_hat_1


* Cultura

xi: reg cultura_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict cultura_real_res,res
reg cultura_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict cultura_real_hat
egen cultura_real_hat_1=mean(cultura_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter cultura_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line cultura_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Cultura per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(cultura_real,replace) graphregion(fcol(white))  bgcolor(white)
drop cultura_real_res
drop cultura_real_hat
drop cultura_real_hat_1

* Desporto Lazer

xi: reg desporto_lazer_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict desporto_lazer_real_res,res
reg desporto_lazer_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict desporto_lazer_real_hat
egen desporto_lazer_real_hat_1=mean(desporto_lazer_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter desporto_lazer_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line desporto_lazer_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Desp e Lazer per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(desporto_lazer_real,replace) graphregion(fcol(white))  bgcolor(white)
drop desporto_lazer_real_res
drop desporto_lazer_real_hat
drop desporto_lazer_real_hat_1

* Transporte

xi: reg transporte_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict transporte_real_res,res
reg transporte_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict transporte_real_hat
egen transporte_real_hat_1=mean(transporte_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter transporte_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line transporte_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Transporte per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(transporte_real,replace) graphregion(fcol(white))  bgcolor(white)
drop transporte_real_res
drop transporte_real_hat
drop transporte_real_hat_1

* Escargos Especiais

xi: reg encargos_esp_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict encargos_esp_real_res,res
reg encargos_esp_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict encargos_esp_real_hat
egen encargos_esp_real_hat_1=mean(encargos_esp_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter encargos_esp_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line encargos_esp_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Encargos Especiais per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(encargos_esp_real,replace) graphregion(fcol(white))  bgcolor(white)
drop encargos_esp_real_res
drop encargos_esp_real_hat
drop encargos_esp_real_hat_1

* Previdencia

xi: reg Previdencia_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict Previdencia_real_res,res
reg Previdencia_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict Previdencia_real_hat
egen Previdencia_real_hat_1=mean(Previdencia_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter Previdencia_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line Previdencia_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Previdência per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(Previdencia_real,replace) graphregion(fcol(white))  bgcolor(white)
drop Previdencia_real_res
drop Previdencia_real_hat
drop Previdencia_real_hat_1

graph combine edu_real.gph saude_real.gph adm_real.gph legislativo_real.gph
graph combine urbanismo_real.gph cultura_real.gph desporto_lazer_real.gph transporte_real.gph
graph combine encargos_esp_real.gph previdencia_real.gph

* Pessoal

xi: reg pessoal_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict pessoal_real_res,res
reg pessoal_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict pessoal_real_hat
egen pessoal_real_hat_1=mean(pessoal_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter pessoal_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line pessoal_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Pessoal per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(pessoal_real,replace) graphregion(fcol(white))  bgcolor(white)
drop pessoal_real_res
drop pessoal_real_hat
drop pessoal_real_hat_1

* Capital

xi: reg capital_real seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict capital_real_res,res
reg capital_real_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict capital_real_hat
egen capital_real_hat_1=mean(capital_real_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter capital_real_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line capital_real_hat pscore if pscore>-10 & pscore<10, title("Log Gasto com Capital per capita") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(capital_real,replace) graphregion(fcol(white))  bgcolor(white)
drop capital_real_res
drop capital_real_hat
drop capital_real_hat_1

graph combine pessoal_real.gph capital_real.gph

*** Complemento

** IFGF

xi: reg IFGF seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict IFGF_res,res
reg IFGF_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict IFGF_hat
egen IFGF_hat_1=mean(IFGF_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter IFGF_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line IFGF_hat pscore if pscore>-10 & pscore<10, title("IFGF") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(IFGF,replace) graphregion(fcol(white))  bgcolor(white)
drop IFGF_res
drop IFGF_hat
drop IFGF_hat_1

* Autonomia

xi: reg IFGF_autonomia seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict IFGF_autonomia_res,res
reg IFGF_autonomia_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict IFGF_autonomia_hat
egen IFGF_autonomia_hat_1=mean(IFGF_autonomia_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter IFGF_autonomia_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line IFGF_autonomia_hat pscore if pscore>-10 & pscore<10, title("IFGF Autonomia") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(IFGF_autonomia,replace) graphregion(fcol(white))  bgcolor(white)
drop IFGF_autonomia_res
drop IFGF_autonomia_hat
drop IFGF_autonomia_hat_1

* Pessoal

xi: reg IFGF_pessoal seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict IFGF_pessoal_res,res
reg IFGF_pessoal_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict IFGF_pessoal_hat
egen IFGF_pessoal_hat_1=mean(IFGF_pessoal_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter IFGF_pessoal_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line IFGF_pessoal_hat pscore if pscore>-10 & pscore<10, title("IFGF Pessoal") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(IFGF_pessoal,replace) graphregion(fcol(white))  bgcolor(white)
drop IFGF_pessoal_res
drop IFGF_pessoal_hat
drop IFGF_pessoal_hat_1

* Liquidez

xi: reg IFGF_liquidez seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict IFGF_liquidez_res,res
reg IFGF_liquidez_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict IFGF_liquidez_hat
egen IFGF_liquidez_hat_1=mean(IFGF_liquidez_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter IFGF_liquidez_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line IFGF_liquidez_hat pscore if pscore>-10 & pscore<10, title("IFGF Liquidez") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(IFGF_liquidez,replace) graphregion(fcol(white))  bgcolor(white)
drop IFGF_liquidez_res
drop IFGF_liquidez_hat
drop IFGF_liquidez_hat_1

* Investimento

xi: reg IFGF_investimento seg2 seg3 i.id i.ano if pscore>-10 & pscore<10,r
predict IFGF_investimento_res,res
reg IFGF_investimento_res aaa pscore pscore_a if pscore>-10 & pscore<10,r
predict IFGF_investimento_hat
egen IFGF_investimento_hat_1=mean(IFGF_investimento_res)if pscore>-10 & pscore<10,by(e82_pscore_1)
scatter IFGF_investimento_hat_1 e82_pscore_1 if pscore>-10 & pscore<10, xline(0,lwidth(medthick) lcolor(black) lpattern(solid)) msize(medium) mfcolor(blue)|| line IFGF_investimento_hat pscore if pscore>-10 & pscore<10, title("IFGF Investimento") xtit("Distância") lwidth(medthick) ylab(-0.3 -0.2 -0.1 0 0.1 0.2 0.3, nogrid) xlab(-10 "-10%" -8 "-8% "-6 "-6%" -4 "-4%" -2 "-2%" 0 "0" 2 "2%" 4 "4%" 6 "6%" 8 "8%" 10 "10%") legend(off) sort saving(IFGF_investimento,replace) graphregion(fcol(white))  bgcolor(white)
drop IFGF_investimento_res
drop IFGF_investimento_hat
drop IFGF_investimento_hat_1

graph combine IFGF_autonomia.gph IFGF_pessoal.gph IFGF_liquidez.gph IFGF_investimento.gph

*** Resultados econométricos

*Figure 1 Online se r2 keep(aaa) appendix
set scheme s1mono
rddensity pop if pop<44148, breakpoint(10188) generate(Xj Yj r0 fhat se_fhat) graphname(DC1.eps)
graph save DC1, append

drop Xj Yj r0 fhat se_fhat
DCdensity pop if pop<44148, breakpoint(13584) generate(Xj Yj r0 fhat se_fhat) graphname(DC2.eps)
graph save DC2, append

drop Xj Yj r0 fhat se_fhat
DCdensity pop if pop<44148, breakpoint(16980) generate(Xj Yj r0 fhat se_fhat) graphname(DC3.eps)
graph save DC3, append

drop Xj Yj r0 fhat se_fhat
DCdensity pop if pop<44148, breakpoint(23772) generate(Xj Yj r0 fhat se_fhat) graphname(DC4.eps)
graph save DC4, append

drop Xj Yj r0 fhat se_fhat
DCdensity pop if pop<44148, breakpoint(30564) generate(Xj Yj r0 fhat se_fhat) graphname(DC5.eps)
graph save DC5, append

drop Xj Yj r0 fhat se_fhat
DCdensity pop if pop<44148, breakpoint(37356) generate(Xj Yj r0 fhat se_fhat) graphname(DC6.eps)
graph save DC6, append

graph combine DC1.gph DC2.gph DC3.gph DC4.gph DC5.gph DC6.gph, graphregion(fcol(white)) note("{it:Notes:} These density plots are for 1982 official municipality population based on the 1980 census. The estimates""(standard errors) of the McCrary (2008) test are, for the first to sixth cutoffs respectively, -0.072 (0.095), 0.011 (0.111),""0.180 (0.136), 0.054 (0.174), -0.011 (0.269), 0.350 (0.357).") 

gen dist = dist1

* Teste de covariadas

foreach x in pib empregados emp pib Poss_Rei {
		est clear

		xi: reg `x' aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
		outreg2 using `x'1.tex, replace ctitle(4%) keep(aaa) dec(3)
		xi: reg `x' aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 3,r
		outreg2 using `x'2.tex, replace ctitle(5%) keep(aaa) dec(3)
		xi: reg `x' aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 3,r
		outreg2 using `x'3.tex, replace ctitle(10%) keep(aaa) dec(3)
		xi: reg `x' aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>15 & pscore<15 & seg <= 3,r
		outreg2 using `x'4.tex, replace ctitle(15%) keep(aaa) dec(3)
		}


* Regressão

** Receita Total

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="receita_real_lag"

* 1-3 cutoff

xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using receita_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using receita_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using receita_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using receita_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using receita_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using receita_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using receita_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using receita_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using receita_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using receita_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using receita_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using receita_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using receita_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using receita_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using receita_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using receita_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using receita_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using receita_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using receita_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using receita_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using receita_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using receita_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using receita_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using receita_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using receita_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using receita_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using receita_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using receita_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using receita_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using receita_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using receita_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using receita_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using receita_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using receita_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using receita_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using receita_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using receita_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using receita_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using receita_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg receita_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using receita_real5.tex, append ctitle(15%) keep(aaa) dec(3)

** Despesa Total 

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="despesa_real_lag"

* 1-3 cutoff

xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using despesa_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using despesa_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using despesa_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using despesa_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using despesa_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using despesa_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using despesa_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using despesa_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using despesa_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using despesa_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using despesa_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using despesa_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using despesa_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using despesa_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using despesa_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using despesa_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using despesa_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using despesa_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using despesa_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using despesa_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using despesa_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using despesa_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using despesa_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using despesa_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using despesa_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using despesa_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using despesa_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using despesa_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using despesa_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using despesa_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using despesa_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using despesa_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using despesa_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using despesa_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using despesa_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using despesa_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using despesa_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using despesa_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using despesa_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg despesa_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using despesa_real5.tex, append ctitle(15%) keep(aaa) dec(3)

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="adm_real_lag"

** Administração

* 1-3 cutoff

xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using adm_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using adm_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using adm_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using adm_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using adm_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using adm_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using adm_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using adm_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using adm_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using adm_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using adm_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using adm_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using adm_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using adm_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using adm_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using adm_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using adm_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using adm_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using adm_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using adm_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using adm_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using adm_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using adm_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using adm_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using adm_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using adm_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using adm_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using adm_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using adm_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using adm_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using adm_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using adm_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using adm_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using adm_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using adm_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using adm_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using adm_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using adm_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using adm_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg adm_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using adm_real5.tex, append ctitle(15%) keep(aaa) dec(3)

** Saúde


		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="saude_real_lag"

* 1-3 cutoff

xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using saude_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using saude_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using saude_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using saude_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using saude_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using saude_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using saude_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using saude_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using saude_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using saude_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using saude_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using saude_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using saude_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using saude_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using saude_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using saude_real2.tex, append ctitle(15%) keep(aaa) dec(3)

* 1 cutoff

xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using adm_rea3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using saude_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using saude_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using saude_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using saude_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using saude_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using saude_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using saude_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using saude_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using saude_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using saude_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using saude_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using saude_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using saude_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using saude_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using saude_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using saude_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using saude_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using saude_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using saude_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using saude_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using saude_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using saude_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg saude_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using saude_real5.tex, append ctitle(15%) keep(aaa) dec(3)


* Edu

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="edu_real_lag"

xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using edu_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using edu_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using edu_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using edu_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using edu_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using edu_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using edu_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using edu_real1.tex, append ctitle(15%) keep(aaa) dec(3)

* 1-2 cutoff

xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using edu_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using edu_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using edu_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using edu_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using edu_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using edu_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using edu_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using edu_real2.tex, append ctitle(15%) keep(aaa) dec(3)

* 1 cutoff

xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using edu_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using edu_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using edu_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using edu_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using edu_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using edu_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using edu_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using edu_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using edu_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using edu_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using edu_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using edu_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using edu_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using edu_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using edu_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using edu_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using edu_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using edu_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using edu_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using edu_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using edu_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using edu_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using edu_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg edu_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using edu_real5.tex, append ctitle(15%) keep(aaa) dec(3)

* Segurança Pública



		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="seg_publica_lag"

xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using seg_publica_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using seg_publica_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using seg_publica_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using seg_publica_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using seg_publica_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using seg_publica_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using seg_publica_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using seg_publica_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using seg_publica_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using seg_publica_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using seg_publica_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using seg_publica_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using seg_publica_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using seg_publica_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using seg_publica_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using seg_publica_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using seg_publica_rea3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using seg_publica_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using seg_publica_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using seg_publica_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using seg_publica_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using seg_publica_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using seg_publica_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using seg_publica_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using seg_publica_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using seg_publica_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using seg_publica_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using seg_publica_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using seg_publica_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using seg_publica_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using seg_publica_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using seg_publica_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using seg_publica_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using seg_publica_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using seg_publica_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using seg_publica_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using seg_publica_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using seg_publica_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using seg_publica_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg seg_publica_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using seg_publica_real5.tex, append ctitle(15%) keep(aaa) dec(3)


* Desporto Lazer



		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="desporto_lazer_real_lag"

xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using desporto_lazer_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using desporto_lazer_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using desporto_lazer_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using desporto_lazer_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using desporto_lazer_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using desporto_lazer_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using desporto_lazer_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using desporto_lazer_real1.tex, append ctitle(15%) keep(aaa) dec(3)

* 1-2 cutoff

xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using desporto_lazer_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using desporto_lazer_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using desporto_lazer_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using desporto_lazer_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using desporto_lazer_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using desporto_lazer_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using desporto_lazer_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using desporto_lazer_real2.tex, append ctitle(15%) keep(aaa) dec(3)

* 1 cutoff

xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using desporto_lazer_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using desporto_lazer_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using desporto_lazer_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using desporto_lazer_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using desporto_lazer_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using desporto_lazer_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using desporto_lazer_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using desporto_lazer_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using desporto_lazer_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using desporto_lazer_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using desporto_lazer_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using desporto_lazer_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using desporto_lazer_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using desporto_lazer_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using desporto_lazer_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using desporto_lazer_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using desporto_lazer_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using desporto_lazer_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using desporto_lazer_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using desporto_lazer_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using desporto_lazer_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using desporto_lazer_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using desporto_lazer_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg desporto_lazer_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using desporto_lazer_real5.tex, append ctitle(15%) keep(aaa) dec(3)


* Encargos Especiais


		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="encargos_esp_lag"

xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using encargos_esp_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using encargos_esp_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using encargos_esp_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using encargos_esp_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using encargos_esp_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using encargos_esp_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using encargos_esp_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using encargos_esp_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using encargos_esp_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using encargos_esp_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using encargos_esp_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using encargos_esp_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using encargos_esp_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using encargos_esp_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using encargos_esp_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using encargos_esp_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using encargos_esp_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using encargos_esp_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using encargos_esp_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using encargos_esp_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using encargos_esp_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using encargos_esp_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using encargos_esp_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using encargos_esp_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using encargos_esp_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using encargos_esp_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using encargos_esp_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using encargos_esp_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using encargos_esp_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using encargos_esp_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using encargos_esp_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using encargos_esp_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using encargos_esp_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using encargos_esp_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using encargos_esp_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using encargos_esp_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using encargos_esp_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using encargos_esp_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using encargos_esp_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg encargos_esp_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using encargos_esp_real5.tex, append ctitle(15%) keep(aaa) dec(3)


* Transf Estado

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="estado_lag"

xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using estado_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using estado_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using estado_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using estado_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using estado_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using estado_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using estado_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using estado_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using estado_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using estado_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using estado_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using estado_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using estado_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using estado_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using estado_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using estado_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using estado_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using estado_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using estado_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using estado_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using estado_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using estado_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using estado_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using estado_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using estado_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using estado_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using estado_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using estado_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using estado_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using estado_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using estado_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using estado_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using estado_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using estado_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using estado_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using estado_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using estado_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using estado_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using estado_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg estado_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using estado_real5.tex, append ctitle(15%) keep(aaa) dec(3)



* Previdência

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="Previdencia_lag"

xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using Previdencia_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using Previdencia_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using Previdencia_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using Previdencia_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using Previdencia_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using Previdencia_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using Previdencia_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using Previdencia_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using Previdencia_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using Previdencia_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using Previdencia_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using Previdencia_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using Previdencia_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using Previdencia_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using Previdencia_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using Previdencia_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using Previdencia_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using Previdencia_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using Previdencia_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using Previdencia_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using Previdencia_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using Previdencia_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using Previdencia_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using Previdencia_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using Previdencia_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using Previdencia_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using Previdencia_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using Previdencia_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using Previdencia_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using Previdencia_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using Previdencia_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using Previdencia_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using Previdencia_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using Previdencia_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using Previdencia_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using Previdencia_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using Previdencia_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using Previdencia_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using Previdencia_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Previdencia_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using Previdencia_real5.tex, append ctitle(15%) keep(aaa) dec(3)


* Tributario


		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="tributario_lag"



xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using tributario_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using tributario_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using tributario_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using tributario_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using tributario_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using tributario_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using tributario_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using tributario_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using tributario_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using tributario_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using tributario_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using tributario_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using tributario_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using tributario_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using tributario_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using tributario_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using tributario_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using tributario_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using tributario_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using tributario_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using tributario_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using tributario_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using tributario_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using tributario_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using tributario_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using tributario_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using tributario_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using tributario_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using tributario_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using tributario_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using tributario_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using tributario_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using tributario_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using tributario_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using tributario_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using tributario_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using tributario_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using tributario_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using tributario_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg tributario_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using tributario_real5.tex, append ctitle(15%) keep(aaa) dec(3)



* Legislativo 

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="legislativo_lag"

xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using legislativo_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using legislativo_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using legislativo_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using legislativo_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using legislativo_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using legislativo_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using legislativo_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using legislativo_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using legislativo_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using legislativo_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using legislativo_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using legislativo_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using legislativo_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using legislativo_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using legislativo_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using legislativo_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using legislativo_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using legislativo_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using legislativo_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using legislativo_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using legislativo_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using legislativo_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using legislativo_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using legislativo_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using legislativo_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using legislativo_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using legislativo_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using legislativo_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using legislativo_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using legislativo_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using legislativo_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using legislativo_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using legislativo_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using legislativo_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using legislativo_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using legislativo_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using legislativo_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using legislativo_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using legislativo_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg legislativo_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using legislativo_real5.tex, append ctitle(15%) keep(aaa) dec(3)



* Pessoal

		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="pessoal_lag"

xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using pessoal_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using pessoal_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using pessoal_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using pessoal_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using pessoal_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using pessoal_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using pessoal_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using pessoal_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using pessoal_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using pessoal_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using pessoal_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using pessoal_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using pessoal_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using pessoal_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using pessoal_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using pessoal_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using pessoal_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using pessoal_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using pessoal_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using pessoal_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using pessoal_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using pessoal_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using pessoal_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using pessoal_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using pessoal_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using pessoal_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using pessoal_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using pessoal_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using pessoal_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using pessoal_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using pessoal_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using pessoal_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using pessoal_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using pessoal_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using pessoal_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using pessoal_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using pessoal_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using pessoal_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using pessoal_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg pessoal_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using pessoal_real5.tex, append ctitle(15%) keep(aaa) dec(3)


* Capital 


		local covariates="dist empregados emp pib Poss_Rei"
		local covariates_esp ="capital_lag"

xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using capital_real1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using capital_real1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using capital_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using capital_real1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using capital_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using capital_real1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using capital_real1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using capital_real1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using capital_real2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using capital_real2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using capital_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using capital_real2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using capital_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using capital_real2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using capital_real2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using capital_real2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using capital_real3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using capital_real3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using capital_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using capital_real3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using capital_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using capital_real3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using capital_real3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using capital_real3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using capital_real4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using capital_real4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using capital_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using capital_real4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using capital_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using capital_real4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using capital_real4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using capital_real4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using capital_real5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using capital_real5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using capital_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using capital_real5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using capital_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using capital_real5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using capital_real5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg capital_real aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using capital_real5.tex, append ctitle(15%) keep(aaa) dec(3)

*** Complemento

** IFGF


		local covariates="dist empregados emp pib Poss_Rei Exp_gest  IFDM"
		
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using IFGF1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using IFGF1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using IFGF1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using IFGF1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using IFGF1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using IFGF2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using IFGF2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using IFGF2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using IFGF2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using IFGF3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using IFGF3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using IFGF3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using IFGF3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using IFGF3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using IFGF3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using IFGF4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using IFGF4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using IFGF4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using IFGF4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using IFGF4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using IFGF4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using IFGF5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using IFGF5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using IFGF5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using IFGF5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using IFGF5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using IFGF5.tex, append ctitle(15%) keep(aaa) dec(3)

* Autonomia 


		local covariates="dist empregados emp pib Poss_Rei Exp_gest  IFDM"

xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using IFGF_autonomia1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using IFGF_autonomia1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_autonomia1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using IFGF_autonomia1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_autonomia1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using IFGF_autonomia1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_autonomia1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using IFGF_autonomia1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using IFGF_autonomia2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using IFGF_autonomia2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_autonomia2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using IFGF_autonomia2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_autonomia2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_autonomia2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_autonomia2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using IFGF_autonomia2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using IFGF_autonomia3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using IFGF_autonomia3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using IFGF_autonomia3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using IFGF_autonomia3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_autonomia3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_autonomia3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using IFGF_autonomia3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using IFGF_autonomia3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using IFGF_autonomia4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using IFGF_autonomia4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using IFGF_autonomia4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using IFGF_autonomia4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_autonomia4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_autonomia4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using IFGF_autonomia4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using IFGF_autonomia4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using IFGF_autonomia5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using IFGF_autonomia5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using IFGF_autonomia5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using IFGF_autonomia5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_autonomia5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_autonomia5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using IFGF_autonomia5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_autonomia aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using IFGF_autonomia5.tex, append ctitle(15%) keep(aaa) dec(3)

* Pessoal

		local covariates="dist empregados emp pib Poss_Rei Exp_gest  IFDM"
		
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using IFGF_pessoal1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using IFGF_pessoal1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_pessoal1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using IFGF_pessoal1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_pessoal1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using IFGF_pessoal1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_pessoal1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using IFGF_pessoal1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using IFGF_pessoal2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using IFGF_pessoal2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_pessoal2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using IFGF_pessoal2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_pessoal2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_pessoal2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_pessoal2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using IFGF_pessoal2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using IFGF_pessoal3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using IFGF_pessoal3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using IFGF_pessoal3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using IFGF_pessoal3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_pessoal3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_pessoal3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using IFGF_pessoal3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using IFGF_pessoal3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using IFGF_pessoal4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using IFGF_pessoal4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using IFGF_pessoal4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using IFGF_pessoal4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_pessoal4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_pessoal4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using IFGF_pessoal4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using IFGF_pessoal4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using IFGF_pessoal5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using IFGF_pessoal5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using IFGF_pessoal5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using IFGF_pessoal5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_pessoal5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_pessoal5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using IFGF_pessoal5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_pessoal aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using IFGF_pessoal5.tex, append ctitle(15%) keep(aaa) dec(3)


* Liquidez


		local covariates="dist empregados emp pib Poss_Rei Exp_gest  IFDM "
		
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using IFGF_liquidez1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using IFGF_liquidez1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_liquidez1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using IFGF_liquidez1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_liquidez1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using IFGF_liquidez1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_liquidez1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using IFGF_liquidez1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using IFGF_liquidez2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using IFGF_liquidez2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_liquidez2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using IFGF_liquidez2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_liquidez2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_liquidez2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_liquidez2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using IFGF_liquidez2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using IFGF_liquidez3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using IFGF_liquidez3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using IFGF_liquidez3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using IFGF_liquidez3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_liquidez3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_liquidez3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using IFGF_liquidez3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using IFGF_liquidez3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using IFGF_liquidez4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using IFGF_liquidez4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using IFGF_liquidez4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using IFGF_liquidez4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_liquidez4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_liquidez4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using IFGF_liquidez4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using IFGF_liquidez4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using IFGF_liquidez5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using IFGF_liquidez5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using IFGF_liquidez5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using IFGF_liquidez5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_liquidez5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_liquidez5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using IFGF_liquidez5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_liquidez aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using IFGF_liquidez5.tex, append ctitle(15%) keep(aaa) dec(3)

* Investimento


		local covariates="dist empregados emp pib Poss_Rei Exp_gest  IFDM"

xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using IFGF_investimento1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using IFGF_investimento1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_investimento1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using IFGF_investimento1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_investimento1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using IFGF_investimento1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_investimento1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using IFGF_investimento1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using IFGF_investimento2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using IFGF_investimento2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFGF_investimento2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using IFGF_investimento2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_investimento2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFGF_investimento2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFGF_investimento2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using IFGF_investimento2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using IFGF_investimento.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using IFGF_investimento3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using IFGF_investimento3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using IFGF_investimento3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_investimento3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFGF_investimento3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using IFGF_investimento3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using IFGF_investimento3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using IFGF_investimento4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using IFGF_investimento4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using IFGF_investimento4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using IFGF_investimento4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_investimento4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFGF_investimento4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using IFGF_investimento4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using IFGF_investimento4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using IFGF_investimento5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using IFGF_investimento5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using IFGF_investimento5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using IFGF_investimento5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_investimento5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFGF_investimento5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using IFGF_investimento5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFGF_investimento aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using IFGF_investimento5.tex, append ctitle(15%) keep(aaa) dec(3)

** IFDM

	covariates="dist empregados emp pib Poss_Rei "

xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using IFDM1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using IFDM1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFDM1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using IFDM1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFDM1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using IFDM1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFDM1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using IFDM1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using IFDM2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using IFDM2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using IFDM2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using IFDM2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFDM2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using IFDM2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using IFDM2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using IFDM2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using IFDM3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using IFDM3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using IFDM3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using IFDM3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFDM3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using IFDM3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using IFDM3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using IFDM3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using IFDM4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using IFDM4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using IFDM4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using IFDM4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFDM4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using IFDM4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using IFDM4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using IFDM4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using IFDM5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using IFDM5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using IFDM5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using IFDM5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFDM5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using IFDM5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using IFDM5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg IFDM aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using IFDM5.tex, append ctitle(15%) keep(aaa) dec(3)


* Educação

xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using educa1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using educa1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using educa1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using educa1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using educa1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using educa1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using educa1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using educa1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using educa2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using educa2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using educa2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using educa2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using educa2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using educa2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using educa2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using educa2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using educa3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using educa3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using educa3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using educa3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using educa3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using educa3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using educa3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using educa3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using educa4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using educa4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using educa4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using educa4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using educa4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using educa4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using educa4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using educa4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using educa5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using educa5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using educa5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using educa5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using educa5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using educa5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using educa5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg educa aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using educa5.tex, append ctitle(15%) keep(aaa) dec(3)


* Saúde


xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using Saude1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using Saude1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using Saude1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using Saude1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using Saude1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using Saude1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using Saude1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using Saude1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using Saude2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using Saude2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using Saude2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using Saude2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using Saude2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using Saude2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using Saude2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using Saude2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using Saude3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using Saude3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using Saude3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using Saude3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using Saude3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using Saude3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using Saude3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using Saude3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using Saude4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using Saude4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using Saude4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using Saude4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using Saude4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using Saude4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using Saude4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using Saude4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using Saude5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using Saude5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using Saude5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using Saude5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using Saude5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using Saude5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using Saude5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg Saude aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using Saude5.tex, append ctitle(15%) keep(aaa) dec(3)


* Renda

xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 3,r
est store A0
outreg2 using renda1.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 3,r
est store A0C
outreg2 using renda1.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using renda1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 3,r
est store B0C
outreg2 using renda1.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using renda1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 3,r
est store C0
outreg2 using renda1.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using renda1.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 3,r
est store D0C
outreg2 using renda1.tex, append ctitle(15%) keep(aaa) dec(3)


* 1-2 cutoff

xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg <= 2,r
est store A0
outreg2 using renda2.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg <= 2,r
est store A0C
outreg2 using renda2.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg <= 2,r
est store B0
outreg2 using renda2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg <= 2,r
est store B0C
outreg2 using renda2.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using renda2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg <= 2,r
est store C0
outreg2 using renda2.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg <= 2,r
est store D0
outreg2 using renda2.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg <= 2,r
est store D0C
outreg2 using renda2.tex, append ctitle(15%) keep(aaa) dec(3)


* 1 cutoff

xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 1,r
est store A0
outreg2 using renda3.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 1,r
est store A0C
outreg2 using renda3.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 1,r
est store B0
outreg2 using renda3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 1,r
est store B0C
outreg2 using renda3.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using renda3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 1,r
est store C0
outreg2 using renda3.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 1,r
est store D0
outreg2 using renda3.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 1,r
est store D0C
outreg2 using renda3.tex, append ctitle(15%) keep(aaa) dec(3)

* 2 cutoff

xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 2,r
est store A0
outreg2 using renda4.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 2,r
est store A0C
outreg2 using renda4.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 2,r
est store B0
outreg2 using renda4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 2,r
est store B0C
outreg2 using renda4.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using renda4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 2,r
est store C0
outreg2 using renda4.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 2,r
est store D0
outreg2 using renda4.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 2,r
est store D0C
outreg2 using renda4.tex, append ctitle(15%) keep(aaa) dec(3)

* 3 cutoff

xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-4 & pscore<4 & seg == 3,r
est store A0
outreg2 using renda5.tex, replace ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-4 & pscore<4 & seg == 3,r
est store A0C
outreg2 using renda5.tex, append ctitle(4%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-5 & pscore<5 & seg == 3,r
est store B0
outreg2 using renda5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-5 & pscore<5 & seg == 3,r
est store B0C
outreg2 using renda5.tex, append ctitle(5%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using renda5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-10 & pscore<10 & seg == 3,r
est store C0
outreg2 using renda5.tex, append ctitle(10%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano if pscore>-15 & pscore<15 & seg == 3,r
est store D0
outreg2 using renda5.tex, append ctitle(15%) keep(aaa) dec(3)
xi: reg renda aaa seg2 seg3 pscore pscore_a  pscore_2 pscore_2_a  pscore_3 pscore_3_a  i.id i.ano `covariates' `covariates_esp' if pscore>-15 & pscore<15 & seg == 3,r
est store D0C
outreg2 using renda5.tex, append ctitle(15%) keep(aaa) dec(3)
