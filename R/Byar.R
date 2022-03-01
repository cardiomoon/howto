#' Byar prostate cancer data set
#'
#' A data set consisting of variables of mixed type measured on a group of prostate
#' cancer patients. Patients have either stage 3 or stage 4 prostate cancer.
#'
#' @format A data.frame with 502 rows and 30 variables
#' \describe{
#' \item{patno}{Patient Number}
#' \item{stage}{stage}
#' \item{rx}{1=placebo 2=0.2mg extrogen 3=1.0mg estrogen 4=5.0mg estrogen}
#' \item{dtime}{Months of follow-up}
#' \item{status}{1=alive 2=dead-Prostate ca 3=dead-heart or vascular 4=dead-cerebrovascular
#' 5=dead - pulmonary embolus 6=dead-other ca 7=dead-respiratory disease
#' 8=dead - other specific non-ca 9=dead - unspecified non-ca 10=dead - unknown cause}
#' \item{age}{Age in years}
#' \item{wt}{Weight Index = wt(kg)-ht(cm)+200}
#' \item{pf}{performance status 1=normal activity 2=in bed < 50% daytime 3=in bed >50% day time
#' 4=confined to bed}
#' \item{hx}{History of Cardiovascular Disease}
#' \item{sbp}{Systolic Blood Pressure/10}
#' \item{dbp}{Diastolic Blood Pressure/10}
#' \item{ekg}{1=normal 2=benign 3=rhythmic disturb & electrolyte ch 4=heart block or conduction def
#'  5=heart strain 6=old MI 7=recent MI}
#' \item{hg}{Serum Hemoglobin (g/dL}
#' \item{sz}{Size of Primary Tumor (cm^2)}
#' \item{sg}{Combined Index of Stage and Hist. Grade}
#' \item{ap}{Serum Prostatic Acid Phosphatase}
#' \item{bm}{Bone Metastases}
#' \item{sdate}{Date on study}
#' \item{Rx}{0=placebo or 0.2mg DES 1=1 or 5mg DES}
#' \item{Age}{0 <=74years 1=75-79 years 2 >=80 years}
#' \item{Wt}{Standardized weight 0 >=100 1=80-99 2 <80}
#' \item{PF}{0=normal 1=limitaion of activity}
#' \item{Hx}{History of CVD 0=No 1=Yes}
#' \item{Hb}{Serum Hemoglobin 0 > 12 g/dL 1=9-11.9 g/dL 2 < 9 g/dL}
#' \item{SZ}{Size of primary lesion 0 <30 cm2 1 >= 30 cm2}
#' \item{SC}{Gleeson Socre(index of tumor invasiveness and aggresiveness)  0 <=10  1 >10}
#' \item{statusCancer}{1=dead-Prostate ca 0=censored. 1 if original status=2}
#' \item{statusCVD}{1=dead-from CVD 0=censored. 1 if original status=3-5}
#' \item{statusOther}{1=dead-from Other 0=censored. 1 if original status=6-10}
#' \item{status2}{0=alive 1=dead-prostate ca 2=dead-CVD 3=dead-Others}
#' }
#' @source
#' Byar, D.P. and Green, S.B. (1980). The choice of treatment for cancer patients
#' based on covariate information: applications to prostate cancer. Bulletin du Cancer 67: 477-490.
#'
#' Downloaded at https://hbiostat.org/data/. Accessed on 2022-Mar-1.
#'
#' David G. Kleinbaum and Mitchel Klein. Survival Analysis. A Self-Learning Text(3rd ed)
#' (Springer,2012) ISBN: 978-1441966452 p443
"Byar"
