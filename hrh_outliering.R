############################################################################################################
## Name: USERNAME
## Purpose: Outliering for HRH models (to be called by different scripts)
## R Version: FILEPATH
## source("FILEPATH",echo=T)
###########################################################################################################

## HRH ANY
df[me_name == "hrh_any" & nid %in% c(22152,313371,81005,22152,seq(342142,342146),313400,142804,
                                     322482,322364,312531,142769,312464,313741,313767,322500),is_outlier := 1]
df[me_name == "hrh_any" & nid == 294537 & location_id %in% c(4850,43917,43884,43916,43900),is_outlier := 1]

## HRH AMB
df[me_name == "hrh_amb" & nid %in% c(282109,313158,313320,4679,313767,313400,310630,81005,311265,
                                     224096),is_outlier := 1]
df[me_name == "hrh_amb" & nid == 294537 & location_id %in% c(43884,43900),is_outlier := 1]

## HRH AUDIO
df[me_name == "hrh_audio" & nid %in% c(46924,31887,865,313618,142803,313460,322372,313158,322405,
                                       313338,313452,322393,287209,287210,313355,312526,151802,142797,
                                       322485,421342,313503),is_outlier := 1]
df[me_name == "hrh_audio" & nid == 227131 & location_id %in% c(44861),is_outlier := 1]

## HRH CLINIC
df[me_name == "hrh_clinic" & nid %in% c(313433,313372,313678,313439,313221,313098,313180,313295,
                                        322348,313524,322483,322393,313524,322388),is_outlier := 1]
df[me_name == "hrh_clinic" & nid == 152602 & location_id != 163,is_outlier := 1]

## HRH DENT
df[me_name == "hrh_dent" & nid %in% c(35578,342923,342924,313280,322363,81005,seq(342961,342964),313158,
                                      322498,342923,342924,seq(342961,342964),313666,310635,142823,
                                      313450,313767,148346,322393,39455,313755,313390),is_outlier := 1]

## HRH DENTASS
df[me_name == "hrh_dentass" & nid %in% c(313158,313476,142786,322501,313395,46613,322330,322348),is_outlier := 1]
df[me_name == "hrh_dentass" & nid == 152601 & location_id %in% c(44538,44539,44540),is_outlier := 1]

## HRH DIET
df[me_name == "hrh_diet" & nid %in% c(282109,313413,310623,310670,322363,313295,313225,
                                      322372,5042,322504,322496,313366,313754,313417,322376),is_outlier := 1]

## HRH ENVIR
df[me_name == "hrh_envir" & nid %in% c(322501,322387,seq(342653,342655),seq(342243,342248),39455),is_outlier := 1]

## HRH MEDTECH
df[me_name == "hrh_medtech" & nid %in% c(313460,310620,322358,322399,
                                         322467,306360,306359,313380,310626,313443,313534,
                                         313481,151317,313541,313192,865,81005,243012,
                                         224096,313094,322473),is_outlier := 1]
df[me_name == "hrh_medtech" & nid == 152601 & location_id == 43874,is_outlier := 1]
df[me_name == "hrh_medtech" & nid == 5293 & location_id %in% c(43936,4869),is_outlier := 1]

## HRH MIDASS
df[me_name == "hrh_midass" & nid %in% c(105800,43146,43152,seq(341955,341959),105800,142837,
                                        313108,313278,313364,322347,322389,322478,342995,
                                        342996,342999,313395,39455),is_outlier := 1]
df[me_name == "hrh_midass" & nid == 294537 & location_id %in% c(4850,43917,43881,43884,43900),is_outlier := 1]
df[me_name == "hrh_midass" & nid == 5292 & location_id == 43873,is_outlier := 1]
df[me_name == "hrh_midass" & nid == 5293 & location_id %in% c(43873,43900),is_outlier := 1]
df[me_name == "hrh_midass" & nid == 5294 & location_id == 43905,is_outlier := 1]
df[me_name == "hrh_midass" & nid == 5285 & location_id %in% c(43900,4874,43941),is_outlier := 1]
df[me_name == "hrh_midass" & nid == 152602 & location_id %in% c(43930,43905),is_outlier := 1]
df[me_name == "hrh_midass" & nid == 74309 & location_id %in% c(4850,43917,43900),is_outlier := 1]


## HRH NURSEASS
df[me_name == "hrh_nurseass" & nid %in% c(12146,313158,313759,313084,312148, 313278,313364,151805,
                                          322350,313413,322358,322399,322467,seq(341955,341960),
                                          313349,3183,30434,30437,313531,322372,313448,342995,
                                          342996,342999),is_outlier := 1]
df[me_name == "hrh_nurseass" & nid == 294537 & location_id %in% c(4850,43917,43881),is_outlier := 1]
df[me_name == "hrh_nurseass" & nid == 74309 & location_id %in% c(4850,43917),is_outlier := 1]


## HRH NURSEPROF
df[me_name == "hrh_nurseprof" & nid == 5294 & location_id == 43895,is_outlier := 1]
df[me_name == "hrh_nurseprof" & nid == 152602 & location_id %in% c(4842,43930,43909),is_outlier := 1]
df[me_name == "hrh_nurseprof" & nid %in% c(322472,322379,342141,342142,342143,342144,342145,342146,310625,
                                           342857,342858,342859,342588,342589,342899,342366,342367,342368,
                                           342403,342404,342405,342406,342407,342408,313084,313153,312148,
                                           322498,322350,322392,322482,322487,313201,313371, 313545,313225,
                                           313413,322358,322399,322467,seq(341955,341960),43146,43158,
                                           313400,142804,322368,322486,322406),is_outlier := 1]

## HRH OPT
df[me_name == "hrh_opt" & nid == 152601 & location_id == 43884,is_outlier := 1]
df[me_name == "hrh_opt" & nid == 43299 & location_id == 564,is_outlier := 1]
df[me_name == "hrh_opt" & nid == 294537 & location_id %in% c(4853,43920,43884),is_outlier := 1]
df[me_name == "hrh_opt" & nid %in% c(313660,313840,313315,313413,3183,313352,313759,
                                     265082,310635,313369,56476,210378,322372,142803),is_outlier := 1]

## HRH PCARE
df[me_name == "hrh_pcare" & nid %in% c(282109,265193,310630,312521,310620,310626,322497,310631,
                                       313767,322388,310634,310641,210378,322368,12652,243012,
                                       313748,313433,81005,311265),is_outlier := 1]
df[me_name == "hrh_pcare" & nid == 129718 & location_id == 43900,is_outlier := 1]

## HRH PHARM
df[me_name == "hrh_pharm" & nid %in% c(81005,313103,313158,313190,313620,313371,313764,313348,
                                       142803,322397,313347,313215,313521,342923,342924,342961,
                                       342962,342963,342964,322498,313767,165101,342923,342924,
                                       seq(342961,342964),39455,313295,310640,313646,265194,
                                       322363),is_outlier := 1]
df[me_name == "hrh_pharm" & nid == 294537 & location_id == 43917,is_outlier := 1]
df[me_name == "hrh_pharm" & nid == 151802 & location_id == 25333,is_outlier := 1]
df[me_name == "hrh_pharm" & nid == 5292 & location_id %in% c(43873,43884),is_outlier := 1]
df[me_name == "hrh_pharm" & nid == 5285 & location_id %in% c(43884),is_outlier := 1]
df[me_name == "hrh_pharm" & nid == 5294 & location_id %in% c(4874,43892,43905),is_outlier := 1]
df[me_name == "hrh_pharm" & nid == 152602 & location_id %in% c(4850,43881,44538,44539),is_outlier := 1]

## HRH PHARMTECH
df[me_name == "hrh_pharmtech" & nid %in% c(333019,310675,313159,313192,310670,313397,313235,
                                           142827,313622),is_outlier := 1]
df[me_name == "hrh_pharmtech" & nid == 152601 & location_id %in% c(4864,43895),is_outlier := 1]
df[me_name == "hrh_pharmtech" & nid == 5292 & location_id == 43900,is_outlier := 1]
df[me_name == "hrh_pharmtech" & nid == 5294 & location_id == 43893,is_outlier := 1]
df[me_name == "hrh_pharmtech" & nid == 129718 & location_id == 43883,is_outlier := 1]


## HRH PHYS
df[me_name == "hrh_phys" & nid %in% c(46580,151805,81005,310635,342923,342924,342961,342962,342963,
                                      342964,313158,313199,313366,142768,313421,313767,322500,39455,
                                      342923,342924,seq(342961,342964),313224,142819,313280,322363,
                                      313400,313767,30758,30907,30842,31050,165631,313190),is_outlier := 1]
df[me_name == "hrh_phys" & nid == 131333 & location_id == 44533,is_outlier := 1]
df[me_name == "hrh_phys" & nid == 5293 & location_id %in% c(44540,44538),is_outlier := 1]
df[me_name == "hrh_phys" & nid == 5285 & location_id == 43895,is_outlier := 1]
df[me_name == "hrh_phys" & nid == 152601 & location_id == 43873,is_outlier := 1]

## HRH PSYCH
df[me_name == "hrh_psych" & nid %in% c(310641,163120,342539,313381,313390,322363,43142,322368,
                                       313303,322372,313683,142836,313529,313313,287210,322312),is_outlier := 1]

## HRH RADIO
df[me_name == "hrh_radio" & nid %in% c(313767,313315,313158,313546,322496,11117,322393,280075,
                                       12105),is_outlier := 1]
df[me_name == "hrh_radio" & nid == 294537 & location_id == 43900,is_outlier := 1]
df[me_name == "hrh_radio" & nid == 152601 & location_id %in% c(43916,44538,44539,44540),is_outlier := 1]
df[me_name == "hrh_radio" & nid == 5292 & location_id %in% c(43873,44538,44540),is_outlier := 1]
df[me_name == "hrh_radio" & nid == 5293 & location_id %in% c(43873,43893),is_outlier := 1]
df[me_name == "hrh_radio" & nid == 152602 & location_id %in% c(43874,43899,43893),is_outlier := 1]
df[me_name == "hrh_radio" & nid == 5285 & location_id == 43905,is_outlier := 1]
df[me_name == "hrh_radio" & nid == 129718 & location_id %in% c(43900,43936),is_outlier := 1]


## HRH THERAP
df[me_name == "hrh_therap" & nid %in% c(163120,313411,322467,322399,313375,310669,322379,
                                        310625,322496,313153,310670,322393,322483,322388,
                                        322476,seq(342653,342655),seq(342243,342248),265082,
                                        142804,264959),is_outlier := 1]

## HRH TRAD
df[me_name == "hrh_trad" & nid %in% c(21421,142943,282152,341942,105306,106684,313103,322477,322387,
                                      280216,322389) ,is_outlier := 1]
df[me_name == "hrh_trad" & nid == 152602 & location_id %in% c(43905,4874,43941,43905),is_outlier := 1]
df[me_name == "hrh_trad" & nid == 5285 & location_id == 43898,is_outlier := 1]

## ISCO 88 244 OTHER
df[me_name == "isco_88_244_other" & nid %in% c(322350),is_outlier := 1]

## ISCO 88 313 OTHER
df[me_name == "isco_88_313_other" & nid %in% c(313331,313753),is_outlier := 1]

## ISCO 88 321 OTHER
df[me_name == "isco_88_321_other" & nid %in% c(313105,58660,151802,142829,313285,313366),is_outlier := 1]

## ISCO 88 324 OTHER
df[me_name == "isco_88_324_other" & nid %in% c(20394,260397,280812,280891),is_outlier := 1]

## ISCO 88 513 OTHER
df[me_name == "isco_88_513_other" & nid %in% c(142786),is_outlier := 1]

## ISCO 08 263 OTHER
df[me_name == "isco_08_263_other" & nid %in% c(322392),is_outlier := 1]

## ISCO 08 314 OTHER
df[me_name == "isco_08_314_321_other" & nid %in% c(322468,322505,351564),is_outlier := 1]

## ISCO 08 224 OTHER
df[me_name == "isco_08_224_226_325_532_other" & nid %in% c(322486),is_outlier := 1]
