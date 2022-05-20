gdata <- list(
##### Infection
# n_ctcH_PSA = 2,
# t_ctcH_PSA = strptime("00:10", "%R"),
# n_ctcP_PSA = 0,
# t_ctcP_PSA = strptime("00:05", "%R"),
# n_ctcH_PW = 4,
# t_ctcH_PW = strptime("00:15", "%R"),
# n_ctcP_PW = 4,
# t_ctcP_PW = strptime("00:30", "%R"),
# n_ctcH_H = 5,
# t_ctcH_H = strptime("00:03", "%R"),
# t_ctcV_PW = strptime("00:20", "%R"),
# daily incidence (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
I = 185,
# disease duration (days)
d = 10,
#  basic reproduction number
R0 = 1.29, # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
# tSA  = strptime("02:00", "%R"), # average duration before full admission (in screening area for clinical exam, administrative procedure, etc)
tIC  = 15,   # average duration of stay in intensive care
tSL  = 14,   # average duration of sick leave
tESL = 28,   # average duration of extended sick leave
tE  = 5, # duration epidemiological state E
tEA = 2, # duration epidemiological state EA
tES = 2, # duration epidemiological state ES
tIA = 7, # duration epidemiological state IA
tIM = 8, # duration epidemiological state IM
tIS = 9, # duration epidemiological state IS
tLI = 60, # duration of partial immunity before return to non immune status
tHI = 150, # duration of full immunity before return to partial immune status
# patient protection
# epsPPSA = 0.1,
# epsHPSA = 0.1,
# epsHPW = 0.1,
# epsPPW = 0.1,
# epsVPW = 0.1,
# # healthcare workers protection
# epsPHSA = 0.1, #from patient in SA
# epsPHW = 0.1, #from patient in W
# epsHHW = 0.1, #from HW in W
## Test in ward
# ttestSA = 2/24, # test duration in screening area
# ttestPW = 2/24, # test duration in ward for screening patients
# ttestHW = 2/24, # test duration in ward for screening professionals
# ttestsymp = 2/24, # test duration for symptomatic
#
# tbtwtestP = 14, # duration between two tests for patient
# tbtwtestH = 30, # duration between two tests for HCWS
#
# tbeftestPsymp = 2/24, # duration before test of symp patient
# tbeftestHsymp = 1, # duration before test of symp HCWS

psympNI = 0.5, # probability to be symptomatic when non immune
psympLI = 0.2, # probability to be symptomatic when partially immune
psympHI = 0.1, # probability to be symptomatic when fully immune

psevNI = 0.5, # probability to develop severe symptoms when non immune
psevLI = 0.3, # probability to develop severe symptoms when partially immune
psevHI = 0.1, # probability to develop severe symptoms when fully immune

pSL = 0.3, # probability to take sick leave
pESL = 1, # probability to take extended sick leave
pSLT = 0.01, # probability to take EL/ESL after positive test

pIC = 0.3, # probability to be transfer in intensive care
pdieIC = 0.005, # probability to die in intensive care

###################################
pLI = 0.40, # probability to be PI at the admission (proportion of PI in the population)
pHI = 0.5, # probability to be FI at the admission (proportion of FI in the population)
hNI2LI = 1/30, # daily probability to become partially immune
hLI2HI = 1/60, # daily probability to become fully immune

rinfLI = 0.7, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
rinfHI = 0.5, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

rsymp = 1, # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
rsev = 1, # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)

# ptestPSAsymp = 1, # probability to test symptomatic patients in the screening area
# ptestPSANI = .75,  # probability to test NI patients in the screening area
# ptestPSALI = 0.50, # probability to test PI patients in the screening area
# ptestPSAHI = 0.1, # probability to test FI patients in the screening area
#
# ptestPWsymp = 0.95, # probability to test symptomatic patients in the ward
# ptestPWNI = 0.75,# probability to test NI patients in the ward
# ptestPWLI = 0.50, # probability to test PI patients in the ward
# ptestPWHI = 0.10, # probability to test FI patients in the ward
#
# ptestHsymp = 0.85, # probability to test symptomatic HCWS in the ward
# ptestHNI = 0.75, # probability to test NI HCWS
# ptestHLI = 0.50, # probability to test PI HCWS
# ptestHHI = 0.20, # probability to test FI HCWS

sensAg = 0.85,
speAg = 0.95,
sensPCR = 0.85,
spePCR = 0.95)

saveRDS(gdata, file = "./data/O1.rds")
