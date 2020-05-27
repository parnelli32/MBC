# MBC
Materials for PFMers in MBC

####### Workforce ########

### 25 Year Perspective: Wage Growth vs CPI Growth
montco.fop.gwas <- c(.027,	.029,	.030,	.028,	.020,	.025,	.027,	.055,	.040,	.020,	.020,	.028,	.030,	.075,	.040,	.000,	.000,	.000,
                     .000,	.021,	.021,	.020,	.010,	.020,	.020,	.000)
series.numbers <- c("SUUR0000SA0", "CWUR0000SA0", "CWURS35ASA0", "CUURS35ASA0")
montco.analysis <- workforce_cpi_25(gwas = montco.fop.gwas, cpi.series = series.numbers, month = "June")
montco.analysis
