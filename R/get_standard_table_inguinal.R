#' Get an individual standard table for inguinal patients for AHSQC
#'
#' Retrieves the code for the standard AHSQC table for inguinal patients specified and returns it in either
#' a separate file or through a text string. Data must have appropriate derived variables.
#' @param tbl NULL or integer (1-9) to identify which table to return.
#' @param data a data table
#' @param print a boolean. If TRUE, the code for specified table will be returned
#' in a tet string. If FALSE, the code for specified table will be written to a
#' separate .R file in the working directory with \code{tbl}n\code{.R} nomenclature.
#' @param overwrite a boolean. If a .R file already exists for the table specified and set to TRUE, the file will be overwritten with original code.
#' @details If \code{overwrite} = FALSE and a .R file exists for the specified table, the function will throw an error.
#'
#'If \code{print} = TRUE, the \code{overwrite} argument will be ignored.
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' # d0 <- ahs_get_data()
#' # d1 <- d0[["analyticview"]] %>% filter(flg_opdetails_inguinal %in% "Yes")
#' # get_table(tbl = 1, d1, print = TRUE) ## will print code as text
#' # get_table(tbl = 9, d1, print = FALSE) ## will write tbl9.R
#' # get_table(tbl = 9, d1, print = FALSE, overwrite = TRUE) ## will overwrite tbl9.R

get_standard_table_inguinal <- function(tbl = NULL
                                        , data
                                        , print = FALSE
                                        , overwrite = FALSE
                                        , pval = FALSE){
  
  if(is.character(data)) stop("data should be a data table, not a character string")
  
  if(is.null(tbl)) return(message("tbl is NULL"))
  
  dt <- deparse(substitute(data))
  
  table_list <- list(
    "tbl1 <- list() %>%
    n_unique(patientid, xlab = \"N (patients)\") %>%
  cat_entry(
  ing_hernia_laterality_e %>% factor(levels = c(\"Bilateral\", \"Unilateral Left\", \"Unilateral Right\"))
  , xlab = \"Laterality\"
  , pvalue = FALSE
  ) %>%
  cat_entry(operative_approach_lateral %>% factor(levels = c(\"Hybrid\", \"Laparoscopic\", \"MIS Converted to Open\", \"Mixed-approach (bilateral)\", \"Open\", \"Robotic\"))
  , pvalue = FALSE
  , xlab = \"Operative approach\") %>% 
  cat_entry(recurrent %>% factor(levels = c(\"Primary\", \"Recurrent\"))
    , pvalue = FALSE
    , xlab = \"Primary or recurrent hernia\") %>%
  cat_entry(
    prior_repairs %>% factor(levels = c(\"1\", \"2\", \"3\", \"4\", \"5 +\"))
    , xlab = \"Number of prior repairs\"
    , dt = data %>% filter(recurrent == \"Recurrent\")
    , pvalue = FALSE
    ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"Cohort volume and prior repairs\")"
    
    , "tbl2 <- list() %>%
  n_unique(id_surgeon
  , xlab = \"Surgeons contributing data\") %>%
  n_unique(id_site
  , xlab = \"Sites contributing data\") %>%
  cat_entry(
  e_surg_affiliation
  , dt = data %>% filter(!base:::duplicated(id_surgeon %|% initial_approach))
  , xlab = \"Primary surgeon affiliation\"
  , pvalue = FALSE
  ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"Surgeon and site volume\")"
    
    , "tbl3 <- list() %>%
  cont_entry(
  val_age_new
  , xlab = \"Age (years; capped at 90)\"
  , pvalue = FALSE
    ) %>%
  binary_entry(female
  , xlab = \"Female\"
  , pvalue = FALSE) %>%
  cat_entry(e_race
  , xlab = \"Race\"
  , pvalue = FALSE) %>%
  cont_entry(
  val_calc_bmi2
  , xlab = \"BMI (kg/m<sup>2</sup>; capped at 15, 60)\"
  , pvalue = FALSE
  ) %>%
  cat_entry(bmi_cat %>% factor(levels = c(\"< 30\", \">= 30\"))
        , xlab = \"BMI categories\"
        , pvalue = FALSE) %>%
  cat_entry(e_asaclass
  , xlab = \"ASA Class\"
  , pvalue = FALSE) %>%
  cat_entry(
  wound_class %>% factor(levels = c(\"Clean (Class 1)\", \"Clean-contaminated (Class 2)\", \"Contaminated (Class 3)\", \"Dirty/Infected (Class 4)\"))
  , xlab = \"Wound class distribution\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  vhwg %>% factor(levels = c(\"Grade 1\", \"Grade 2\", \"Grade 3\"))
  , xlab = \"Hernia Grade (VHRI)\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_left_hernia_femoral_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Left femoral hernia width\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_right_hernia_femoral_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Right femoral hernia width\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_left_hernia_medial_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Left medial hernia width\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_right_hernia_medial_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Right femoral hernia width\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_left_hernia_lateral_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Left lateral hernia width\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_right_hernia_lateral_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Right lateral hernia width\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_left_obturator_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Left obturator hernia width\"
  , pvalue = FALSE
  ) %>%
  cat_entry(
  ing_right_obturator_e %>% factor(levels = c(\"I (<1.5cm or <1 fingertip)\", \"II (1.5-3cm or 1-2 fingertips)\", \"III (>3cm or >2 fingertips)\", \"No Hernia\"))
  , xlab = \"Right obturator hernia width\"
  , pvalue = FALSE
  ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"Demographics\")"
    
    , "tbl4 <- list() %>%
    binary_entry(any_comorbidities %>% factor(0:1)
  , xlab = \"Prevalence of comborbidities\"
  , pvalue = FALSE
  ) %>%
  empty_entry(fill = c(\"Comorbidities\",\"N\")) %>%
  binary_entry(
  flg_cmb_steroids %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Immunosuppressant\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  smoking_one_year %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Smoking (within 1 year)\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  nicotine_one_year %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Nicotine use (within 1 year)\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmb_hypertension %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Hypertension\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmb_diabetes %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Diabetes mellitus\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmb_dyspnea %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Dyspnea\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmb_copd %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@COPD\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmb_abdominal_wall_ssi_hx %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@History of abdominal wall surgical site infection\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmb_component_separation_hx %>% factor(levels = c(\"No\",\"Yes\"))
  , xlab = \"@@History of component separation\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmb_open_abdomen_hx %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@History of open abdomen\"
  , pvalue = FALSE
  , fmt = count_fmt
  , level = c(\"Yes\", \"1\")
  ) %>%
  binary_entry(
  current_steroids %>% factor(0:1)
  , xlab = \"@@Current steriod use\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"Comorbidities\")"
    
    
    , "tbl5 <- list() %>%
  cat_entry(operative_time
  , pvalue_fmt = garbage_pvalue
  , pvalue = FALSE
  , xlab = \"Operative time\") %>%
  binary_entry(planned_concomitant_procedure %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Planned concomitant procedure\"
    , pvalue = FALSE) %>%
  binary_entry(flg_concomitant_proc %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"Any concomitant procedure\"
  , pvalue = FALSE) %>%
  empty_entry(
  fill = c(
  \"Concomitant procedures<sup>cata</sup>\",
  \"N\")
  ) %>%
  binary_entry(
  array_other_procedures_system_hernia %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Hernia\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  array_other_procedures_system_foregutendocrine %>% factor(levels = c(\"No\",\"Yes\"))
  , xlab = \"@@Foregut/Endocrine\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  array_other_procedures_system_hepatobiliarypancreatic %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Hepatobiliary/Pancreatic\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  array_other_procedures_system_small_intestine %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Small intestine\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  array_other_procedures_system_colorectal %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Colorectal\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  array_other_procedures_system_obstetricgynecologic %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Obstetric/Gynecologic\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  array_other_procedures_system_urologic %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Urologic\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  )%>%
  binary_entry(
  array_other_procedures_system_vascular %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Vascular\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  array_other_procedures_system_soft_tissueplastics %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Soft tissue/plastics\"
  , dt = data %>% filter(flg_concomitant_proc == \"Yes\")
  , fmt = count_fmt
  #, pvalue = FALSE
  ) %>%
   binary_entry(
  ing_left_mesh_e %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"Left mesh\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  ing_right_mesh_e %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"Right mesh\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  
  empty_entry(fill = c(\"Mesh fixation type<sup>cata</sup>\",\"N\")) %>%
  
  binary_entry(
  ing_left_mesh_fixation_e_none %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@No Left Fixation\"
  , dt = data 
  , fmt = count_fmt
  ) %>%
  binary_entry(
  ing_left_mesh_fixation_e_adhesives %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Left Adhesives\"
  , dt = data 
  , fmt = count_fmt
  ) %>%
   binary_entry(
  ing_left_mesh_fixation_e_staples %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Left Staples\"
  , dt = data
  , fmt = count_fmt
  ) %>%
   binary_entry(
  ing_left_mesh_fixation_e_suture %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Left Sutures\"
  , dt = data 
  , fmt = count_fmt
  ) %>%
   binary_entry(
  ing_left_mesh_fixation_e_tacks %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Left Tacks\"
  , dt = data
  , fmt = count_fmt
  ) %>%
  
    binary_entry(
  ing_right_mesh_fixation_e_none %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@No Right Fixation\"
  , dt = data 
  , fmt = count_fmt
  ) %>%
    binary_entry(
  ing_right_mesh_fixation_e_adhesives %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Right Adhesives\"
  , dt = data 
  , fmt = count_fmt
  ) %>%
   binary_entry(
  ing_right_mesh_fixation_e_staples %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Right Staples\"
  , dt = data 
  , fmt = count_fmt
  ) %>%
   binary_entry(
  ing_right_mesh_fixation_e_suture %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Right Sutures\"
  , dt = data
  , fmt = count_fmt
  ) %>%
   binary_entry(
  ing_right_mesh_fixation_e_tacks %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Right Tacks\"
  , dt = data 
  , fmt = count_fmt
  ) %>%

  cont_entry(val_util_los
  , xlab = \"Length of stay (days)\"
  , pvalue = FALSE) %>%

  rbindlist
  tbl5[V1 == \"Conversion to open\", V2 := \"N (%)\"]
  tbl5[V1 == \"Conversion to open\", V4 := \"\"]
  tbl5 <- tbl5 %>% as.data.frame %>%
  `attr<-`(\"title\",\"Operative characteristics\")"
    
    , "tbl6 <- list() %>%
    binary_entry(
  flg_intraop_complication %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"Any intra-op complications\"
  , pvalue = FALSE
  ) %>%
  empty_entry(
  fill = c(
  \"Specific complications<sup>cata</sup>\",
  \"N (%)\")
  ) %>%
  binary_entry(
  array_intraop_complication_type_hemorrhage_requiring_transfusion %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Hemorrhage requiring transfusion\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_intraop_complication_type_peritoneal_access_injury %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Peritoneal access injury\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_intraop_complication_type_bowel_injury %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Bowel injury\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_intraop_complication_type_bladder_injury %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Bladder injury\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_intraop_complication_type_liver_injury %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Liver injury\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_intraop_complication_type_gastric_injury %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Gastric injury\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_intraop_complication_type_major_vascular_injury_requiring_operative_intervention %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Major vascular injury requiring intervention\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_intraop_complication_type_other %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Other\"
  , dt = data %>% filter(flg_intraop_complication == \"Yes\")
  , fmt = count_fmt
  ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"Intra-operative complications\")"
    
    , "tbl7 <- list() %>%
    binary_entry(
  flg_cmp_postop_any %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"Subjects reporting any complication\"
  , pvalue = FALSE
  ) %>%
  binary_entry(
  flg_other_nsqip_comp 
  , xlab = \"Non-wound/other complications\"
  , pvalue = FALSE
  ) %>%
  empty_entry(
  fill = c(\"Specific non-wound/other complication\",\"N\")
  ) %>%
  binary_entry(
  flg_pe %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Pulmonary embolism\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_stroke %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Stroke\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_dvt%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@DVT\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_sepsis%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Sepsis\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_septic_shock%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Septic shock\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_mi%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@MI\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cardiac_arrest%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Cardiac arrest\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_uti%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@UTI\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_renal_insuff%>% factor(levels=c(0:1))
  , xlab = \"@@Renal insufficiency\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_renal_failure%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Acute renal failure\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_pneumonia%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Pneumonia\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_endotracheal%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Respiratory failure requiring intubation\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_ventilator %>% factor(levels=c(\"FALSE\",\"TRUE\"))
  , xlab = \"@@Ventilator > 48 hrs\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_coma%>% factor(levels=c(\"FALSE\",\"TRUE\"))
  , xlab = \"@@Coma > 24 hrs\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_nerve_injury%>% factor(levels=c(\"FALSE\",\"TRUE\"))
  , xlab = \"@@Peripheral nerve injury\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_bleeding_transfusion%>% factor(levels=c(\"FALSE\",\"TRUE\"))
  , xlab = \"@@Post-op bleeding transfusion\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_graft_prosthesis_flap_fail%>% factor(levels=c(\"FALSE\",\"TRUE\"))
  , xlab = \"@@Graft/prosthesis/flap failure\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_pain%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Pain requiring intervention\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_other_comp%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Other\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  cat_entry(
  death_30_days %>% factor(levels = c(\"Actively following\", \"Follow up ended\", \"Death after 30 days\", \"Death (no date reported)\", \"Death within 30 days\"))
  , pvalue = FALSE
  , xlab = \"Death 30 days\"
  ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"Post-operative complications, mortality, follow-up\")"
    
    ,"tbl8 <- list() %>%
    binary_entry(
  flg_cmp_postop_ssi %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"SSI\"
  , pvalue = FALSE
  ) %>%
  empty_entry(
  fill = c(\"Infection type\", \"N\")
  ) %>%
  binary_entry(
  array_ssi_comp_type_superficial_surgical_site_infection %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Superficial SSI\"
  , dt = data %>% filter(flg_cmp_postop_ssi == \"Yes\")
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_comp_type_deep_incisional_surgical_site_infection %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Deep incisional SSI\"
  , dt = data %>% filter(flg_cmp_postop_ssi == \"Yes\")
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_comp_type_organ_space_surgical_site_infection %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Organ space SSI\"
  , dt = data %>% filter(flg_cmp_postop_ssi == \"Yes\")
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  ssi_treatment  %>% factor(levels = c(0,1))
  , xlab = \"Surgical site infection requiring treatment\"
  , dt = data %>% filter(flg_cmp_postop_ssi == \"Yes\")
  , pvalue = FALSE
  #, fmt = count_fmt
  ) %>%
  binary_entry(
  ssi_pi %>% factor(levels = c(0,1))
  , xlab = \"Surgical site infection requiring procedural intervention\"
  , dt = data %>% filter(flg_cmp_postop_ssi == \"Yes\")
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  empty_entry(fill = c(\"Treatments administered for SSI<sup>cata</sup>\",\"N\")
  ) %>%
  binary_entry(
  array_ssi_treatments_oral_antibiotics %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Oral antibiotics\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_iv_antibiotics %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@IV antibiotics\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_wound_opening %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Wound opening\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_wound_debridement %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Wound debridement\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_suture_excision %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Suture excision\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_percutaneous_drainage %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Percutaneous drainage\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_partial_mesh_removal %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Partial mesh removal\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_complete_mesh_removal %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Complete mesh removal\"
  , dt = data %>% filter(ssi_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_sso_comps %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"Surgical site occurrences exclusive of SSI (SSO-EI)\"
  , pvalue = FALSE
  ) %>%
  empty_entry(
  fill = c(\"SSO-EI complication type<sup>cata</sup>\", \"N\")
  ) %>%
  binary_entry(
  array_sso_comp_type_seroma %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Seroma\"
  , dt = data %>% filter(flg_sso_comps == 'Yes')
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_comp_type_infected_seroma %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Infected Seroma\"
  , dt = data %>% filter(flg_sso_comps == 'Yes')
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_comp_type_hematoma %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Hematoma\"
  , dt = data %>% filter(flg_sso_comps == 'Yes')
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_comp_type_infected_hematoma %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Infected Hematoma\"
  , dt = data %>% filter(flg_sso_comps == 'Yes')
  , fmt = count_fmt
  ) %>%
  binary_entry(
  sso_treatment %>% factor(levels = c(0,1))
  , xlab = \"SSO-EI requiring treatment\"
  , dt = data %>% filter(flg_sso_comps == \"Yes\")
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  sso_pi %>% factor(levels = c(0,1))
  , xlab = \"SSO-EI requiring procedural intervention\"
  , dt = data %>% filter(flg_sso_comps == \"Yes\")
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  empty_entry(fill = c(\"Treatments administered for SSO-EI<sup>cata</sup>\",\"N\")
  ) %>%
  binary_entry(
  array_sso_treatments_oral_antibiotics%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Oral antibiotics\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_iv_antibiotics%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@IV antibiotics\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_wound_opening%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Wound opening\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_wound_debridement%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Wound debridement\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_suture_excision%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Suture excision\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_percutaneous_drainage%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Percutaneous drainage\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_partial_mesh_removal%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Partial mesh removal\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_complete_mesh_removal%>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"@@Complete mesh removal\"
  , dt = data %>% filter(sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmp_postop_sso_ssi %>% factor(levels=c(\"No\",\"Yes\"))
  , xlab = \"Surgical site infection or occurance (SSI/O)\"
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  ssi_sso_treatment %>% factor(levels = c(0,1))
  , xlab = \"SSI/O requiring treatment\"
  , pvalue = FALSE
  , dt = data %>% filter(flg_cmp_postop_sso_ssi == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  ssi_sso_pi %>% factor(levels = c(0,1))
  , xlab = \"SSI/O requiring procedural intervention\"
  , pvalue = FALSE
  , dt = data %>% filter(flg_cmp_postop_sso_ssi == \"Yes\")
  , fmt = count_fmt
  ) %>%
  empty_entry(fill = c(\"Treatments administered for SSI/O<sup>cata</sup>\",\"N\")
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_oral_antibiotics %in% \"Yes\" |
  array_ssi_treatments_oral_antibiotics %in% \"Yes\"))%>% factor(levels = c(0,1))
  , xlab = \"@@Oral antibiotics\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_iv_antibiotics %in% \"Yes\" |
  array_ssi_treatments_iv_antibiotics %in% \"Yes\"))%>% factor(levels = c(0,1))
  , xlab = \"@@IV antibiotics\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_wound_opening %in% \"Yes\" |
  array_ssi_treatments_wound_opening %in% \"Yes\"))%>% factor(levels = c(0,1))
  , xlab = \"@@Wound opening\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_wound_debridement %in% \"Yes\" |
  array_ssi_treatments_wound_debridement %in% \"Yes\"))%>% factor(levels = c(0,1))
  , xlab = \"@@Wound debridement\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_suture_excision %in% \"Yes\" |
  array_ssi_treatments_suture_excision %in% \"Yes\"))%>% factor(levels = c(0,1))
  , xlab = \"@@Suture excision\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_percutaneous_drainage %in% \"Yes\" |
  array_ssi_treatments_percutaneous_drainage %in% \"Yes\"))%>% factor(levels = c(0,1))
  , xlab = \"@@Percutaneous drainage\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_partial_mesh_removal %in% \"Yes\" |
  array_ssi_treatments_partial_mesh_removal %in% \"Yes\"))%>% factor(levels = c(0,1))
  , xlab = \"@@Partial mesh removal\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_complete_mesh_removal %in% \"Yes\" |
  array_ssi_treatments_complete_mesh_removal %in% \"Yes\")) %>% factor(levels = c(0,1))
  , xlab = \"@@Complete mesh removal\"
  , dt = data %>% filter(ssi_sso_treatment == 1)
  , fmt = count_fmt
  ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"SSI/SSO outcomes\")"
    
    , "tbl9 <- list() %>%
    empty_entry(
  fill = c(\"Subject re-encounters<sup>cata</sup>\",\"N (%)\")
  ) %>%
  binary_entry(
  e_between_visit_clinic %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Clinic\"
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  e_between_visit_emergency_room %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Emergency room\"
  #, pvalue = FALSE
  ) %>%
  binary_entry(
  flg_readmission %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"Re-admission within 30 days\"
  , pvalue = FALSE
  ) %>%
  empty_entry(fill = c(\"Reported reasons for re-admission<sup>cata</sup>\",\"N\")
  ) %>%
  binary_entry(
  array_readmission_reason_pain %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Pain\"
  , dt = data %>% filter(flg_readmission == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_readmission_reason_prosthetic_related_complication%>% factor(levels = c(\"No\",\"Yes\"))
  , xlab = \"@@Prosthetic related complication\"
  , dt = data %>% filter(flg_readmission == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_readmission_reason_wound_complication %>% factor(levels = c(\"No\",\"Yes\"))
  , xlab = \"@@Wound complication\"
  , dt = data %>% filter(flg_readmission == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_readmission_reason_bleeding_complication %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Bleeding complication\"
  , dt = data %>% filter(flg_readmission == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_readmission_reason_thrombotic_complication_noncardiac %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Thrombotic complication (non-cardiac)\"
  , dt = data %>% filter(flg_readmission == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_readmission_reason_gastrointestinal_complication %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Gastrointestinal complication\"
  , dt = data %>% filter(flg_readmission == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_reoperation %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"Reoperation\"
  , pvalue = FALSE
  ) %>%
  empty_entry(fill = c(\"Reoperation type<sup>cata</sup>\", \"N\")
  ) %>%
  binary_entry(
  array_reop_type_unrecognized_bowel_injury %>% factor(levels = c(\"No\",\"Yes\"))
  , xlab = \"@@Unrecognized bowel injury\"
  , dt = data %>% filter(flg_reoperation == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_reop_type_major_wound_complication %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Major wound complication\"
  , dt = data %>% filter(flg_reoperation == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_reop_type_postoperative_bleeding %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Postoperative bleeding\"
  , dt = data %>% filter(flg_reoperation == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_reop_type_early_recurrence %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Early recurrence\"
  , dt = data %>% filter(flg_reoperation == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_reop_type_bowel_obstruction %>% factor(levels = c(\"No\", \"Yes\"))
  , xlab = \"@@Bowl obstruction\"
  , dt = data %>% filter(flg_reoperation == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_reop_type_mesh_excision %>% factor(levels = c(\"No\",\"Yes\"))
  , xlab = \"@@Mesh excision\"
  , dt = data %>% filter(flg_reoperation == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_reop_type_unrelated_intraabdominal_pathology%>% factor(levels = c(\"No\",\"Yes\"))
  , xlab = \"@@Unrelated intraabdominal pathology\"
  , dt = data %>% filter(flg_reoperation == \"Yes\")
  , fmt = count_fmt
  ) %>%
  binary_entry(
    flg_recurrence %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Recurrence\"
    , pvalue_fmt = garbage_pvalue
    , pvalue = FALSE
    ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"Post-operative through 30 day outcomes\")"
  )
  
  
  table_list <- lapply(table_list, gsub, pattern = "= dt", replacement = paste0("= ", dt))
  table_list <- lapply(table_list, gsub, pattern = "pvalue = FALSE", replacement = paste0("pvalue = ", pval," "))
  
  
  if(print == FALSE){
    assign(paste0("ing_tbl",tbl), as.character(table_list[tbl]))
    if((paste0("ing_tbl",tbl,".R") %in% list.files()) & overwrite == FALSE){
      stop(paste0("ing_tbl",tbl,".R already exists in your list of files. To overwrite this file, specify overwrite = TRUE."))
    }else{
      write(get(paste0("ing_tbl",tbl)), file = paste0("ing_tbl",tbl,".R"))
    }
  }
  if(print == TRUE){
    return(as.character(table_list[tbl]))
  }
}




