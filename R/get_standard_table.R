#' Get an individual standard table for AHSQC
#'
#' Retrieves the code for the standard AHSQC table specified and returns it in either
#' a separate file or through a text string.
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
#' # d1 <- d0[["analyticview]]
#' # get_table(tbl = 1, d1, print = TRUE) ## will print code as text
#' # get_table(tbl = 9, d1, print = FALSE) ## will write tbl9.R
#' # get_table(tbl = 9, d1, print = FALSE, overwrite = TRUE) ## will overwrite tbl9.R

get_standard_table <- function(tbl = NULL
                      , data
                      , print = FALSE
                      , overwrite = FALSE){

  if(is.character(data)) stop("data should be a data table, not a character string")
  
  if(is.null(tbl)) return(message("tbl is NULL"))
  
  dt <- deparse(substitute(data))
  table_list <- list(
    "tbl1 <- list() %>%
    n_unique(patientid, xlab = \"N\") %>% 
    #cat_entry(e_procedure_category) %>% 
    cat_entry(recurrent) %>% 
    cat_entry(
    prior_repairs
    ) %>% 
    rbindlist %>% 
    as.data.frame %>% 
    `attr<-`(\"title\",\"Cohort volume and prior repairs\")"
    
    , "tbl2 <- list() %>%
    n_unique(id_surgeon, xlab = \"Surgeons contributing data\") %>%
    n_unique(id_site, xlab = \"Sites contributing data\") %>%
    cat_entry(
    e_surg_affiliation,
    , xlab = \"Primary surgeon affiliation\"
    , pvalue = FALSE
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Surgeon and site volume\")"
    
    , "tbl3 <- list() %>% 
    cont_entry(val_age_new, xlab = \"Age (years; capped at 90)\") %>% 
    cat_entry(e_gender) %>% 
    cont_entry(val_calc_bmi2, xlab = \"BMI (kg/m<sup>2</sup>; capped at 15, 60)\") %>% 
    cat_entry(bmi_cat, xlab = \"BMI categories\") %>% 
    cat_entry(e_asaclass, xlab = \"ASA class\") %>% 
    cat_entry(wound_class, xlab = \"Wound class distribution\") %>% 
    cat_entry(vhwg, xlab = \"Hernia Grade\") %>% 
    cont_entry(val_hern_width, xlab = \"Hernia width (cm)\") %>% 
    rbindlist %>% 
    as.data.frame %>% 
    `attr<-`(\"title\",\"Demographics\")"
    
    , "tbl4 <- list() %>% 
    binary_entry(any_comorbidities, xlab = \"Prevalence of comorbidities\") %>% 
    empty_entry(fill = c(\"Comorbidities\",\"N\")) %>% 
    binary_entry(
    flg_cmb_steroids
    , xlab = \"@@Immunosuppressant\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    smoking_one_year
    , xlab = \"@@Smoking (within 1 year)\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    nicotine_one_year
    , xlab = \"@@Nicotine use (within 1 year)\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_cmb_hypertension
    , xlab = \"@@Hypertension\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_cmb_diabetes
    , xlab = \"@@Diabetes mellitus\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_cmb_dyspnea
    , xlab = \"@@Dyspnea\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_cmb_copd
    , xlab = \"@@COPD\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_cmb_abdominal_wall_ssi_hx
    , xlab = \"@@History of abdominal wall surgical site infection\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_cmb_component_separation_hx
    , xlab = \"@@History of component separation\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_cmb_open_abdomen_hx
    , xlab = \"@@History of open abdomen\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    current_steroids
    , xlab = \"@@Current steroid use\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    rbindlist %>% 
    as.data.frame %>% 
    `attr<-`(\"title\",\"Comorbidities\")"
    
    , "tbl5 <- list() %>% 
    cat_entry(operative_time, pvalue_fmt = garbage_pvalue) %>%
    binary_entry(planned_concomitant_procedure) %>% 
    binary_entry(flg_concomitant_proc, xlab = \"Any concomitant procedure\") %>% 
    empty_entry(
    fill = c(
    \"Concomitant procedures<sup>cata</sup>\",
    \"N\")
    ) %>% 
    binary_entry(
    array_other_procedures_system_hernia 
    , xlab = \"@@Hernia\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_foregutendocrine 
    , xlab = \"@@Foregut/Endocrine\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_hepatobiliarypancreatic
    , xlab = \"@@Hepatobiliary/Pancreatic\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_small_intestine 
    , xlab = \"@@Small intestine\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_colorectal
    , xlab = \"@@Colorectal\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_obstetricgynecologic
    , xlab = \"@@Obstetric/Gynecologic\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_urologic
    , xlab = \"@@Urologic\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_vascular
    , xlab = \"@@Vascular\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(
    array_other_procedures_system_soft_tissueplastics
    , xlab = \"@@Soft tissue/plastics\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    binary_entry(flg_mesh_used, xlab = \"Mesh used\") %>% 
    binary_entry(
    flg_fixation
    , xlab = \"Mesh fixation (among repairs using mesh)\"
    , dt = data %>% dplyr:::filter(flg_mesh_used == \"Yes\")
    ) %>%
    empty_entry(fill = c(\"Mesh fixation type<sup>cata</sup>\",\"N\")) %>%
    binary_entry(
    e_fixation_type_adhesives
    , xlab = \"@@Adhesives\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry(
    e_fixation_type_staples
    , xlab = \"@@Staples\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry(
    e_fixation_type_sutures
    , xlab = \"@@Sutures\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry(
    e_fixation_type_tacks
    , xlab = \"@@Tacks\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>% 
    cont_entry(
    val_util_los
    , xlab = \"Length of stay (days)\"
    , pvalue_fmt = garbage_pvalue
    ) %>% 
    binary_entry(convert_to_open, xlab = \"Conversion to open\", pvalue_fmt = garbage_pvalue) %>% 
    rbindlist 
    
    tbl5[V1 == \"Conversion to open\", V2 := \"N (%)\"]
    tbl5[V1 == \"Conversion to open\", V4 := \"N/A\"]
    tbl5[V1 == \"Conversion to open\", V6 := \"N/A\"]
    
    
    
    tbl5 <- tbl5 %>% as.data.frame %>% 
    `attr<-`(\"title\",\"Operative characteristics\")"
    
    , "tbl6 <- list() %>% 
    binary_entry(
    flg_intraop_complication
    , xlab = \"Any intra-op complications\"
    ) %>% 
    empty_entry(
    fill = c(
    \"Specific complications<sup>cata</sup>\",
    \"N (%)\")
    ) %>% 
    binary_entry(
    array_intraop_complication_type_hemorrhage_requiring_transfusion %>% 
    factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Hemorrhage requiring transfusion\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_intraop_complication_type_peritoneal_access_injury
    , xlab = \"@@Peritoneal access injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    array_intraop_complication_type_bowel_injury
    , xlab = \"@@Bowel injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_intraop_complication_type_bladder_injury
    , xlab = \"@@Bladder injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_intraop_complication_type_liver_injury %>% 
    factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Liver injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_intraop_complication_type_gastric_injury %>% 
    factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Gastric injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_intraop_complication_type_major_vascular_injury_requiring_operative_intervention 
    %>% factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Major vascular injury requiring intervention\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_intraop_complication_type_other
    , xlab = \"@@Other\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    rbindlist %>% 
    as.data.frame %>% 
    `attr<-`(\"title\",\"Intra-operative complications\")"
    
    , "tbl7 <- list() %>% 
    binary_entry(
    flg_cmp_postop_any
    , xlab = \"Subjects reporting any complication\"
    , pvalue_fmt = garbage_pvalue
    ) %>% 
    binary_entry(
    flg_other_nsqip_comp
    , xlab = \"Non-wound/other complications\"
    , pvalue_fmt = garbage_pvalue
    ) %>% 
    empty_entry(
    fill = c(\"Specific non-wound/other complication\",\"N\")
    ) %>% 
    binary_entry(
    flg_pe
    , xlab = \"@@Pulmonary embolism\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_stroke
    , xlab = \"@@Stroke\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_dvt
    , xlab = \"@@DVT\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_sepsis
    , xlab = \"@@Sepsis\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_septic_shock
    , xlab = \"@@Septic shock\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_mi
    , xlab = \"@@MI\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_cardiac_arrest
    , xlab = \"@@Cardiac arrest\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_uti
    , xlab = \"@@UTI\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_renal_insuff
    , xlab = \"@@Renal insufficiency\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_renal_failure
    , xlab = \"@@Acute renal failure\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_pneumonia
    , xlab = \"@@Pneumonia\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_endotracheal
    , xlab = \"@@Respiratory failure requiring intubation\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_ventilator
    , xlab = \"@@Ventilator > 48 hrs\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    binary_entry(
    flg_coma
    , xlab = \"@@Coma > 24 hrs\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_nerve_injury
    , xlab = \"@@Peripheral nerve injury\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_bleeding_transfusion
    , xlab = \"@@Post-op bleeding transfusion\"
    , pvalue = FALSE
    , fmt = count_fmt
    , pvalue_fmt = garbage_pvalue
    ) %>%
    binary_entry(
    flg_graft_prosthesis_flap_fail
    , xlab = \"@@Graft/prosthesis/flap failure\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_pain
    , xlab = \"@@Pain requiring intervention\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_other_comp
    , xlab = \"@@Other\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>% 
    cat_entry(
    death_30_days
    , pvalue = FALSE
    ) %>%
    rbindlist %>% 
    as.data.frame %>% 
    `attr<-`(\"title\",\"Post-operative complications, mortality, follow-up\")"
    
    
    , "tbl8 <- list() %>%
    binary_entry(
    flg_cmp_postop_ssi
    , pvalue_fmt = garbage_pvalue
    ) %>%
    empty_entry(
    fill = c(\"Infection type\", \"N\")
    ) %>%
    binary_entry(
    array_ssi_comp_type_superficial_surgical_site_infection
    , xlab = \"@@Superficial SSI\"
    , dt = data %>% dplyr:::filter(flg_cmp_postop_ssi == \"Yes\)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_comp_type_deep_incisional_surgical_site_infection
    , xlab = \"@@Deep incisional SSI\"
    , dt = data %>% dplyr:::filter(flg_cmp_postop_ssi == \"Yes\)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_comp_type_organ_space_surgical_site_infection
    , xlab = \"@@Organ space SSI\"
    , dt = data %>% dplyr:::filter(flg_cmp_postop_ssi == \"Yes\)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
     ssi_treatment
     , xlab = \"Surgical site infection requiring treatment\"
     , dt = data %>%
      dplyr:::mutate(ssi_treatment = nazero(ssi_treatment) %>% samena(flg_cmp_postop_ssi))
     #, pvalue = FALSE
     #, fmt = count_fmt
     , pvalue_fmt = garbage_pvalue
     ) %>%
     binary_entry(
     ssi_pi
     , xlab = \"Surgical site infection requiring procedural intervention\"
     , dt = data %>%
      dplyr:::mutate(ssi_pi = nazero(ssi_pi) %>% samena(flg_cmp_postop_ssi))
     #, pvalue = FALSE
     #, fmt = count_fmt
     , pvalue_fmt = garbage_pvalue
     ) %>%
    binary_entry(
     ssi_treatment
     , xlab = \"Surgical site infection requiring treatment\"
     , dt = data %>%
        dplyr:::mutate(ssi_treatment = nazero(ssi_treatment) %>% samena(flg_cmp_postop_ssi))
     #, pvalue = FALSE
     #, fmt = count_fmt
     , pvalue_fmt = garbage_pvalue
     ) %>%
     binary_entry(
     ssi_pi
     , xlab = \"Surgical site infection requiring procedural intervention\"
     , dt = data %>%
        dplyr:::mutate(ssi_pi = nazero(ssi_pi) %>% samena(flg_cmp_postop_ssi))
     #, pvalue = FALSE
     #, fmt = count_fmt
     , pvalue_fmt = garbage_pvalue
     ) %>%
    empty_entry(
    fill = c(\"Treatments administered for SSI<sup>cata</sup>\",\"N\")) %>%
    binary_entry(
    array_ssi_treatments_oral_antibiotics
    , xlab = \"@@Oral antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_treatments_iv_antibiotics
    , xlab = \"@@IV antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_treatments_wound_opening
    , xlab = \"@@Wound opening\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_treatments_wound_debridement
    , xlab = \"@@Wound debridement\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_treatments_suture_excision
    , xlab = \"@@Suture excision\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_treatments_percutaneous_drainage
    , xlab = \"@@Percutaneous drainage\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_treatments_partial_mesh_removal
    , xlab = \"@@Partial mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_ssi_treatments_complete_mesh_removal
    , xlab = \"@@Complete mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_sso_comps
    , xlab = \"Surgical site occurrences exclusive of SSI (SSO-EI)\"
    , pvalue_fmt = garbage_pvalue
    ) %>%
    empty_entry(
    fill = c(\"SSO-EI complication type<sup>cata</sup>\", \"N\")
    ) %>%
    binary_entry(
    array_sso_comp_type_seroma
    , xlab = \"@@Seroma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_comp_type_infected_seroma
    , xlab = \"@@Infected Seroma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_comp_type_hematoma
    , xlab = \"@@Hematoma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
      sso_treatment
     , xlab = \"SSO-EI requiring treatment\"
     , dt = data %>%
       dplyr:::mutate(sso_treatment = nazero(sso_treatment) %>% samena(flg_sso_comps))
    # #, pvalue = FALSE
    # #, fmt = count_fmt
     , pvalue_fmt = garbage_pvalue
     ) %>%
     binary_entry(
     sso_pi
     , xlab = \"SSO-EI requiring procedural intervention\"
     , dt = data %>% dplyr:::mutate(sso_pi = samena(nazero(sso_pi),flg_sso_comps))
     #, pvalue = FALSE
     #, fmt = count_fmt
     , pvalue_fmt = garbage_pvalue
     ) %>%
    binary_entry(
    array_sso_comp_type_infected_hematoma
    , xlab = \"@@Infected Hematoma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    empty_entry(
    fill = c(\"Treatments administered for SSO-EI<sup>cata</sup>\",\"N\")) %>%
    binary_entry(
    array_sso_treatments_oral_antibiotics
    , xlab = \"@@Oral antibiotics\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_treatments_iv_antibiotics
    , xlab = \"@@IV antibiotics\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_treatments_wound_opening
    , xlab = \"@@Wound opening\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_treatments_wound_debridement
    , xlab = \"@@Wound debridement\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_treatments_suture_excision
    , xlab = \"@@Suture excision\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_treatments_percutaneous_drainage
    , xlab = \"@@Percutaneous drainage\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_treatments_partial_mesh_removal
    , xlab = \"@@Partial mesh removal\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_sso_treatments_complete_mesh_removal
    , xlab = \"@@Complete mesh removal\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_cmp_postop_sso_ssi
    , xlab = \"Surgical site infection or occurrence (SSI/O)\"
    , pvalue_fmt = garbage_pvalue
    ) %>%
     binary_entry(
      ssi_sso_treatment
     , xlab = \"SSI/O requiring treatment\"
     #, pvalue = FALSE
     , dt = data %>%
      dplyr:::mutate(ssi_sso_treatment = samena(nazero(ssi_sso_treatment),flg_cmp_postop_sso_ssi))
    # #, fmt = count_fmt
    # , pvalue_fmt = garbage_pvalue
     ) %>%
     binary_entry(
     ssi_sso_pi
     , xlab = \"SSI/O requiring procedural intervention\"
     #, pvalue = FALSE
     , dt = data %>%
     mutate(ssi_sso_pi = nazero(ssi_sso_pi) %>% samena(flg_cmp_postop_sso_ssi))
     #, fmt = count_fmt
     , pvalue_fmt = garbage_pvalue
     ) %>%
    empty_entry(
    fill = c(\"Treatments administered for SSI/O<sup>cata</sup>\",\"N\")) %>%
    binary_entry(
    (1*(array_sso_treatments_oral_antibiotics %in% \"Yes\" |
    array_ssi_treatments_oral_antibiotics %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Oral antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    (1*(array_sso_treatments_iv_antibiotics %in% \"Yes\" |
    array_ssi_treatments_iv_antibiotics %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@IV antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    (1*(array_sso_treatments_wound_opening %in% \"Yes\" |
    array_ssi_treatments_wound_opening %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Wound opening\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    (1*(array_sso_treatments_wound_debridement %in% \"Yes\" |
    array_ssi_treatments_wound_debridement %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Wound debridement\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    (1*(array_sso_treatments_suture_excision %in% \"Yes\" |
    array_ssi_treatments_suture_excision %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Suture excision\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    (1*(array_sso_treatments_percutaneous_drainage %in% \"Yes\" |
    array_ssi_treatments_percutaneous_drainage %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Percutaneous drainage\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    (1*(array_sso_treatments_partial_mesh_removal %in% \"Yes\" |
    array_ssi_treatments_partial_mesh_removal %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Partial mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    (1*(array_sso_treatments_complete_mesh_removal %in% \"Yes\" |
    array_ssi_treatments_complete_mesh_removal %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Complete mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1])
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"SSI/SSO outcomes\")"
    
    , "tbl9 <- list() %>%
    empty_entry(fill = c(\"Subject re-encounters<sup>cata</sup>\",\"N (%)\")) %>%
    binary_entry(
    e_between_visit_clinic
    , xlab = \"@@Clinic\"
    , pvalue_fmt = garbage_pvalue
    ) %>%
    binary_entry(
    e_between_visit_emergency_room
    , xlab = \"@@Emergency room\"
    , pvalue_fmt = garbage_pvalue
    ) %>%
    binary_entry(
    flg_readmission
    , xlab = \"Re-admission within 30 days\"
    , pvalue_fmt = garbage_pvalue
    ) %>%
    empty_entry(fill = c(\"Reported reasons for re-admission<sup>cata</sup>\",\"N\")) %>%
    binary_entry(
    array_readmission_reason_pain
    , xlab = \"@@Pain\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_readmission_reason_prosthetic_related_complication
    , xlab = \"@@Prosthetic related complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_readmission_reason_wound_complication
    , xlab = \"@@Wound complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_readmission_reason_bleeding_complication
    , xlab = \"@@Bleeding complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_readmission_reason_thrombotic_complication_noncardiac
    , xlab = \"@@Thrombotic complication (non-cardiac)\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_readmission_reason_gastrointestinal_complication
    , xlab = \"@@Gastrointestinal complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    empty_entry(
    fill = \"@@Other (NOT AVAILABLE)\") %>%
    binary_entry(
    flg_reoperation
    , xlab = \"Reoperation\"
    , pvalue_fmt = garbage_pvalue
    ) %>%
    empty_entry(
    fill = c(\"Reoperation type<sup>cata</sup>\", \"N\")) %>%
    binary_entry(
    array_reop_type_unrecognized_bowel_injury
    , xlab = \"@@Unrecognized bowel injury\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_reop_type_major_wound_complication
    , xlab = \"@@Major wound complication\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_reop_type_postoperative_bleeding
    , xlab = \"@@Postoperative bleeding\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_reop_type_early_recurrence
    , xlab = \"@@Early recurrence\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_reop_type_bowel_obstruction
    , xlab = \"@@Bowel obstruction\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_reop_type_mesh_excision
    , xlab = \"@@Mesh excision\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    array_reop_type_unrelated_intraabdominal_pathology
    , xlab = \"@@Unrelated intraabdominal pathology\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry(
    flg_recurrence
    , pvalue_fmt = garbage_pvalue
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Post-operative through 30 day outcomes\")"
    )
  
  issues_with_tbl_8 <- "tbl8 <- list() %>%
    binary_entry(
  flg_cmp_postop_ssi
  , pvalue_fmt = garbage_pvalue
  ) %>%
  empty_entry(
  fill = c(\"Infection type\", \"N\")
  ) %>%
  binary_entry(
  array_ssi_comp_type_superficial_surgical_site_infection
  , xlab = \"@@Superficial SSI\"
  , dt = dt[flg_cmp_postop_ssi == \"Yes\"]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_comp_type_deep_incisional_surgical_site_infection
  , xlab = \"@@Deep incisional SSI\"
  , dt = dt[flg_cmp_postop_ssi == \"Yes\"]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_comp_type_organ_space_surgical_site_infection
  , xlab = \"@@Organ space SSI\"
  , dt = dt[flg_cmp_postop_ssi == \"Yes\"]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  # binary_entry(
  # ssi_treatment
  # , xlab = \"Surgical site infection requiring treatment\"
  # , dt = dt %>%
  # mutate(ssi_treatment = nazero(ssi_treatment) %>% samena(flg_cmp_postop_ssi))
  # #, pvalue = FALSE
  # #, fmt = count_fmt
  # , pvalue_fmt = garbage_pvalue
  # ) %>%
  # binary_entry(
  # ssi_pi
  # , xlab = \"Surgical site infection requiring procedural intervention\"
  # , dt = dt %>%
  # mutate(ssi_pi = nazero(ssi_pi) %>% samena(flg_cmp_postop_ssi))
  # #, pvalue = FALSE
  # #, fmt = count_fmt
  # , pvalue_fmt = garbage_pvalue
  # ) %>%
  empty_entry(
  fill = c(\"Treatments administered for SSI<sup>cata</sup>\",\"N\")) %>%
  binary_entry(
  array_ssi_treatments_oral_antibiotics
  , xlab = \"@@Oral antibiotics\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_iv_antibiotics
  , xlab = \"@@IV antibiotics\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_wound_opening
  , xlab = \"@@Wound opening\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_wound_debridement
  , xlab = \"@@Wound debridement\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_suture_excision
  , xlab = \"@@Suture excision\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_percutaneous_drainage
  , xlab = \"@@Percutaneous drainage\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_partial_mesh_removal
  , xlab = \"@@Partial mesh removal\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_ssi_treatments_complete_mesh_removal
  , xlab = \"@@Complete mesh removal\"
  , dt = dt[ssi_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_sso_comps
  , xlab = \"Surgical site occurrences exclusive of SSI (SSO-EI)\"
  , pvalue_fmt = garbage_pvalue
  ) %>%
  empty_entry(
  fill = c(\"SSO-EI complication type<sup>cata</sup>\", \"N\")
  ) %>%
  binary_entry(
  array_sso_comp_type_seroma
  , xlab = \"@@Seroma\"
  , dt = dt[flg_sso_comps == 'Yes']
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_comp_type_infected_seroma
  , xlab = \"@@Infected Seroma\"
  , dt = dt[flg_sso_comps == 'Yes']
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_comp_type_hematoma
  , xlab = \"@@Hematoma\"
  , dt = dt[flg_sso_comps == 'Yes']
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_comp_type_infected_hematoma
  , xlab = \"@@Infected Hematoma\"
  , dt = dt[flg_sso_comps == 'Yes']
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  # sso_treatment
  # , xlab = \"SSO-EI requiring treatment\"
  # , dt = dt %>%
  # mutate(sso_treatment = nazero(sso_treatment) %>% samena(flg_sso_comps))
  # #, pvalue = FALSE
  # #, fmt = count_fmt
  # , pvalue_fmt = garbage_pvalue
  # ) %>%
  # binary_entry(
  # sso_pi
  # , xlab = \"SSO-EI requiring procedural intervention\"
  # , dt = dt %>% mutate(sso_pi = samena(nazero(sso_pi),flg_sso_comps))
  # #, pvalue = FALSE
  # #, fmt = count_fmt
  # , pvalue_fmt = garbage_pvalue
  # ) %>%
  empty_entry(
  fill = c(\"Treatments administered for SSO-EI<sup>cata</sup>\",\"N\")) %>%
  binary_entry(
  array_sso_treatments_oral_antibiotics
  , xlab = \"@@Oral antibiotics\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_iv_antibiotics
  , xlab = \"@@IV antibiotics\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_wound_opening
  , xlab = \"@@Wound opening\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_wound_debridement
  , xlab = \"@@Wound debridement\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_suture_excision
  , xlab = \"@@Suture excision\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_percutaneous_drainage
  , xlab = \"@@Percutaneous drainage\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_partial_mesh_removal
  , xlab = \"@@Partial mesh removal\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  array_sso_treatments_complete_mesh_removal
  , xlab = \"@@Complete mesh removal\"
  , dt = dt[sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  flg_cmp_postop_sso_ssi
  , xlab = \"Surgical site infection or occurrence (SSI/O)\"
  , pvalue_fmt = garbage_pvalue
  ) %>%
  # binary_entry(
  # ssi_sso_treatment
  # , xlab = \"SSI/O requiring treatment\"
  # #, pvalue = FALSE
  # , dt = dt %>%
  # mutate(ssi_sso_treatment = samena(nazero(ssi_sso_treatment),flg_cmp_postop_sso_ssi))
  # #, fmt = count_fmt
  # , pvalue_fmt = garbage_pvalue
  # ) %>%
  # binary_entry(
  # ssi_sso_pi
  # , xlab = \"SSI/O requiring procedural intervention\"
  # #, pvalue = FALSE
  # , dt = dt %>%
  # mutate(ssi_sso_pi = nazero(ssi_sso_pi) %>% samena(flg_cmp_postop_sso_ssi))
  # #, fmt = count_fmt
  # , pvalue_fmt = garbage_pvalue
  # ) %>%
  empty_entry(
  fill = c(\"Treatments administered for SSI/O<sup>cata</sup>\",\"N\")) %>%
  binary_entry(
  (1*(array_sso_treatments_oral_antibiotics %in% \"Yes\" |
  array_ssi_treatments_oral_antibiotics %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@Oral antibiotics\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_iv_antibiotics %in% \"Yes\" |
  array_ssi_treatments_iv_antibiotics %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@IV antibiotics\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_wound_opening %in% \"Yes\" |
  array_ssi_treatments_wound_opening %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@Wound opening\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_wound_debridement %in% \"Yes\" |
  array_ssi_treatments_wound_debridement %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@Wound debridement\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_suture_excision %in% \"Yes\" |
  array_ssi_treatments_suture_excision %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@Suture excision\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_percutaneous_drainage %in% \"Yes\" |
  array_ssi_treatments_percutaneous_drainage %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@Percutaneous drainage\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_partial_mesh_removal %in% \"Yes\" |
  array_ssi_treatments_partial_mesh_removal %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@Partial mesh removal\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  binary_entry(
  (1*(array_sso_treatments_complete_mesh_removal %in% \"Yes\" |
  array_ssi_treatments_complete_mesh_removal %in% \"Yes\")) %>% factor(0:1)
  , xlab = \"@@Complete mesh removal\"
  , dt = dt[ssi_sso_treatment == 1]
  , pvalue = FALSE
  , fmt = count_fmt
  ) %>%
  rbindlist %>%
  as.data.frame %>%
  `attr<-`(\"title\",\"SSI/SSO outcomes\")"
  
  table_list <- lapply(table_list, gsub, pattern = "= dt", replacement = paste0("= ", dt))
  
  if(print == FALSE){
    assign(paste0("tbl",tbl), as.character(table_list[tbl]))
    if((paste0("tbl",tbl,".R") %in% list.files()) & overwrite == FALSE){
      stop(paste0("tbl",tbl,".R already exists in your list of files. To overwrite this file, specify overwrite = TRUE."))
    }else{
      write(get(paste0("tbl",tbl)), file = paste0("tbl",tbl,".R"))
    }
  }
  if(print == TRUE){
    return(as.character(table_list[tbl]))
  }
}




