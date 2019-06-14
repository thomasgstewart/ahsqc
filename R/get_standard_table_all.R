#' Get an individual standard table for AHSQC
#'
#' Retrieves the code for the standard AHSQC table specified and returns it in either
#' a separate file or through a text string.
#' @param tbl NULL or integer 1-9 to identify which table to return.
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

get_standard_table_all <- function(tbl = NULL
                               , data
                               , print = FALSE
                               , overwrite = FALSE
                               ){
  
  if(is.character(data)) stop("data should be a data table, not a character string")
  
  if(is.null(tbl)) return(message("tbl is NULL"))
  dt <- deparse(substitute(data))
  
  table_list <- list(
    "tbl1 <- list() %>%
    n_unique_all(patientid, xlab = \"N\") %>%
    cat_entry_all(e_procedure_category, pvalue = FALSE) %>%
    cat_entry_all(recurrent, pvalue = FALSE) %>%
    cat_entry_all(
    prior_repairs
    , xlab = \"Number of prior repairs\"
    , dt = data %>% filter(recurrent == \"Recurrent\")
    , pvalue = FALSE
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Cohort volume and prior repairs\")"
    
    , "tbl2 <- list() %>%
    n_unique_all(id_surgeon, xlab = \"Surgeons contributing data\") %>%
    n_unique_all(id_site, xlab = \"Sites contributing data\") %>%
    cat_entry_all(
    e_surg_affiliation,
    , xlab = \"Primary surgeon affiliation\"
    , pvalue = FALSE
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Surgeon and site volume\")"
    
    , "tbl3 <- list() %>%
    cont_entry_all(val_age_new, xlab = \"Age (years; capped at 90)\", pvalue = FALSE) %>%
    cat_entry_all(e_gender, pvalue = FALSE) %>%
    cont_entry_all(val_calc_bmi2, xlab = \"BMI (kg/m<sup>2</sup>; capped at 15, 60)\", pvalue = FALSE) %>%
    cat_entry_all(bmi_cat, xlab = \"BMI categories\", pvalue = FALSE) %>%
    cat_entry_all(e_asaclass, xlab = \"ASA class\", pvalue = FALSE) %>%
    cat_entry_all(wound_class, xlab = \"Wound class distribution\", pvalue = FALSE) %>%
    cat_entry_all(vhwg, xlab = \"Hernia Grade\", pvalue = FALSE) %>%
    cont_entry_all(val_hern_width, xlab = \"Hernia width (cm)\", pvalue = FALSE) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Demographics\")"
    
    , "tbl4 <- list() %>%
    binary_entry_all(any_comorbidities %>% factor(levels = c(0,1)), xlab = \"Prevalence of comorbidities\") %>%
    empty_entry_all(fill = c(\"Comorbidities\",\"N\")) %>%
    binary_entry_all(
    flg_cmb_steroids %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Immunosuppressant\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    smoking_one_year %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Smoking (within 1 year)\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    nicotine_one_year %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Nicotine use (within 1 year)\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmb_hypertension %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Hypertension\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmb_diabetes %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Diabetes mellitus\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmb_dyspnea %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Dyspnea\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmb_copd %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@COPD\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmb_abdominal_wall_ssi_hx %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@History of abdominal wall surgical site infection\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmb_component_separation_hx %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@History of component separation\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmb_open_abdomen_hx %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@History of open abdomen\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    current_steroids %>% factor(levels = c(0,1))
    , xlab = \"@@Current steroid use\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Comorbidities\")"
    
    , "tbl5 <- list() %>%
    cat_entry_all(operative_time, pvalue = FALSE) %>%
    binary_entry_all(planned_concomitant_procedure %>% factor(levels = c(\"No\", \"Yes\")), pvalue = FALSE, 
    xlab = \"Planned concomitant procedure\") %>%
    binary_entry_all(flg_concomitant_proc %>% factor(levels = c(\"No\", \"Yes\")), xlab = \"Any concomitant procedure\", pvalue = FALSE) %>%
    empty_entry_all(
    fill = c(
    \"Concomitant procedures<sup>cata</sup>\",
    \"N\")
    ) %>%
    binary_entry_all(
    array_other_procedures_system_hernia %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Hernia\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_foregutendocrine %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Foregut/Endocrine\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_hepatobiliarypancreatic %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Hepatobiliary/Pancreatic\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_small_intestine %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Small intestine\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_colorectal %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Colorectal\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_obstetricgynecologic %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Obstetric/Gynecologic\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_urologic %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Urologic\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_vascular %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Vascular\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    array_other_procedures_system_soft_tissueplastics %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Soft tissue/plastics\"
    , dt = data %>% dplyr:::filter(flg_concomitant_proc == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(flg_mesh_used %>% factor(levels = c(\"No\", \"Yes\")), xlab = \"Mesh used\") %>%
    binary_entry_all(
    flg_fixation %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Mesh fixation (among repairs using mesh)\"
    , dt = data %>% dplyr:::filter(flg_mesh_used == \"Yes\")
    ) %>%
    empty_entry_all(fill = c(\"Mesh fixation type<sup>cata</sup>\",\"N\")) %>%
    binary_entry_all(
    e_fixation_type_adhesives %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Adhesives\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    e_fixation_type_staples %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Staples\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    e_fixation_type_sutures %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Sutures\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    binary_entry_all(
    e_fixation_type_tacks %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Tacks\"
    , dt = data %>% dplyr:::filter(flg_fixation == \"Yes\")
    , fmt = count_fmt
    , pvalue = FALSE
    ) %>%
    cont_entry_all(
    val_util_los
    , xlab = \"Length of stay (days)\"
    ) %>%
    binary_entry_all(convert_to_open %>% factor(levels = c(0,1)), xlab = \"Conversion to open\", pvalue = FALSE) %>%
    rbindlist
    
    tbl5[V1 == \"Conversion to open\", V2 := \"N (%)\"]
    tbl5[V1 == \"Conversion to open\", V4 := \"N/A\"]
    tbl5[V1 == \"Conversion to open\", V6 := \"N/A\"]
    
    
    
    tbl5 <- tbl5 %>% as.data.frame %>%
    `attr<-`(\"title\",\"Operative characteristics\")"
    
    , "tbl6 <- list() %>%
    binary_entry_all(
    flg_intraop_complication %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Any intra-op complications\", pvalue = FALSE
    ) %>%
    empty_entry_all(
    fill = c(
    \"Specific complications<sup>cata</sup>\",
    \"N (%)\")
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_hemorrhage_requiring_transfusion %>%
    factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Hemorrhage requiring transfusion\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_peritoneal_access_injury %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Peritoneal access injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_bowel_injury %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Bowel injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_bladder_injury %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Bladder injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_liver_injury %>%
    factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Liver injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_gastric_injury %>%
    factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Gastric injury\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_major_vascular_injury_requiring_operative_intervention
    %>% factor(levels=c(\"No\",\"Yes\"))
    , xlab = \"@@Major vascular injury requiring intervention\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_intraop_complication_type_other %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Other\"
    , dt = data %>% dplyr:::filter(flg_intraop_complication == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Intra-operative complications\")"
    
    , "tbl7 <- list() %>%
    binary_entry_all(
    flg_cmp_postop_any %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Subjects reporting any complication\"
    , pvalue = FALSE
    ) %>%
    empty_entry_all(
    fill = c(\"Specific non-wound/other complication\",\"N\")
    ) %>%
    binary_entry_all(
    flg_pe %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Pulmonary embolism\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_stroke %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Stroke\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_dvt %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@DVT\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_sepsis %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Sepsis\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_septic_shock %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Septic shock\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_mi %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@MI\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cardiac_arrest %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Cardiac arrest\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_uti %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@UTI\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_renal_insuff %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Renal insufficiency\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_renal_failure %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Acute renal failure\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_pneumonia %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Pneumonia\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_endotracheal %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Respiratory failure requiring intubation\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_ventilator 
    , xlab = \"@@Ventilator > 48 hrs\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_coma 
    , xlab = \"@@Coma > 24 hrs\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_nerve_injury
    , xlab = \"@@Peripheral nerve injury\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_bleeding_transfusion
    , xlab = \"@@Post-op bleeding transfusion\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_graft_prosthesis_flap_fail
    , xlab = \"@@Graft/prosthesis/flap failure\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_pain %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Pain requiring intervention\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_other_comp %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Other\"
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    cat_entry_all(
    death_30_days %>% factor(levels = c(\"Actively following\", \"Follow up ended\", \"Death after 30 days\", \"Death (no date reported)\", \"Death within 30 days\"))
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Post-operative complications, mortality, follow-up\")"
    
    
    , "tbl8 <- list() %>%
    binary_entry_all(
    flg_cmp_postop_ssi %>% factor(levels = c(\"No\", \"Yes\"))
    ) %>%
    empty_entry_all(
    fill = c(\"Infection type\", \"N\")
    ) %>%
    binary_entry_all(
    array_ssi_comp_type_superficial_surgical_site_infection %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Superficial SSI\"
    , dt = data %>% dplyr:::filter(flg_cmp_postop_ssi == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_comp_type_deep_incisional_surgical_site_infection %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Deep incisional SSI\"
    , dt = data %>% dplyr:::filter(flg_cmp_postop_ssi == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_comp_type_organ_space_surgical_site_infection %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Organ space SSI\"
    , dt = data %>% dplyr:::filter(flg_cmp_postop_ssi == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    ssi_treatment %>% factor(levels = c(0,1))
    , xlab = \"Surgical site infection requiring treatment\"
    , dt = data %>%
    dplyr:::mutate(ssi_treatment = nazero(ssi_treatment) %>% samena(flg_cmp_postop_ssi))
    #, pvalue = FALSE
    #, fmt = count_fmt
    ) %>%
    binary_entry_all(
    ssi_pi %>% factor(levels = c(0,1))
    , xlab = \"Surgical site infection requiring procedural intervention\"
    , dt = data %>%
    dplyr:::mutate(ssi_pi = nazero(ssi_pi) %>% samena(flg_cmp_postop_ssi))
    #, pvalue = FALSE
    #, fmt = count_fmt
    ) %>%
    binary_entry_all(
    ssi_treatment %>% factor(levels = c(0,1))
    , xlab = \"Surgical site infection requiring treatment\"
    , dt = data %>%
    dplyr:::mutate(ssi_treatment = nazero(ssi_treatment) %>% samena(flg_cmp_postop_ssi))
    #, pvalue = FALSE
    #, fmt = count_fmt
    ) %>%
    binary_entry_all(
    ssi_pi %>% factor(levels = c(0,1))
    , xlab = \"Surgical site infection requiring procedural intervention\"
    , dt = data %>%
    dplyr:::mutate(ssi_pi = nazero(ssi_pi) %>% samena(flg_cmp_postop_ssi))
    #, pvalue = FALSE
    #, fmt = count_fmt
    ) %>%
    empty_entry_all(
    fill = c(\"Treatments administered for SSI<sup>cata</sup>\",\"N\")) %>%
    binary_entry_all(
    array_ssi_treatments_oral_antibiotics %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Oral antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_treatments_iv_antibiotics %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@IV antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_treatments_wound_opening %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Wound opening\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_treatments_wound_debridement %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Wound debridement\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_treatments_suture_excision %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Suture excision\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_treatments_percutaneous_drainage %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Percutaneous drainage\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_treatments_partial_mesh_removal %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Partial mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_ssi_treatments_complete_mesh_removal %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Complete mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_sso_comps %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Surgical site occurrences exclusive of SSI (SSO-EI)\"
    ) %>%
    empty_entry_all(
    fill = c(\"SSO-EI complication type<sup>cata</sup>\", \"N\")
    ) %>%
    binary_entry_all(
    array_sso_comp_type_seroma %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Seroma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_comp_type_infected_seroma %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Infected Seroma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_comp_type_hematoma %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Hematoma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    sso_treatment %>% factor(levels = c(0,1))
    , xlab = \"SSO-EI requiring treatment\"
    , dt = data %>%
    dplyr:::mutate(sso_treatment = nazero(sso_treatment) %>% samena(flg_sso_comps))
    # #, pvalue = FALSE
    # #, fmt = count_fmt
    ) %>%
    binary_entry_all(
    sso_pi %>% factor(levels = c(0,1))
    , xlab = \"SSO-EI requiring procedural intervention\"
    , dt = data %>% dplyr:::mutate(sso_pi = samena(nazero(sso_pi),flg_sso_comps))
    #, pvalue = FALSE
    #, fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_comp_type_infected_hematoma %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Infected Hematoma\"
    , dt = data %>% dplyr:::filter(flg_sso_comps == 'Yes')
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    empty_entry_all(
    fill = c(\"Treatments administered for SSO-EI<sup>cata</sup>\",\"N\")) %>%
    binary_entry_all(
    array_sso_treatments_oral_antibiotics %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Oral antibiotics\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_treatments_iv_antibiotics %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@IV antibiotics\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_treatments_wound_opening %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Wound opening\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_treatments_wound_debridement %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Wound debridement\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_treatments_suture_excision %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Suture excision\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_treatments_percutaneous_drainage %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Percutaneous drainage\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_treatments_partial_mesh_removal %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Partial mesh removal\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_sso_treatments_complete_mesh_removal %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Complete mesh removal\"
    , dt = data %>% dplyr:::filter(sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_cmp_postop_sso_ssi %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Surgical site infection or occurrence (SSI/O)\"
    ) %>%
    binary_entry_all(
    ssi_sso_treatment %>% factor(levels = c(0,1))
    , xlab = \"SSI/O requiring treatment\"
    #, pvalue = FALSE
    , dt = data %>%
    dplyr:::mutate(ssi_sso_treatment = samena(nazero(ssi_sso_treatment),flg_cmp_postop_sso_ssi))
    # #, fmt = count_fmt
    ) %>%
    binary_entry_all(
    ssi_sso_pi %>% factor(levels = c(0,1))
    , xlab = \"SSI/O requiring procedural intervention\"
    #, pvalue = FALSE
    , dt = data %>%
    mutate(ssi_sso_pi = nazero(ssi_sso_pi) %>% samena(flg_cmp_postop_sso_ssi))
    #, fmt = count_fmt
    ) %>%
    empty_entry_all(
    fill = c(\"Treatments administered for SSI/O<sup>cata</sup>\",\"N\")) %>%
    binary_entry_all(
    (1*(array_sso_treatments_oral_antibiotics %in% \"Yes\" |
    array_ssi_treatments_oral_antibiotics %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Oral antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    (1*(array_sso_treatments_iv_antibiotics %in% \"Yes\" |
    array_ssi_treatments_iv_antibiotics %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@IV antibiotics\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    (1*(array_sso_treatments_wound_opening %in% \"Yes\" |
    array_ssi_treatments_wound_opening %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Wound opening\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    (1*(array_sso_treatments_wound_debridement %in% \"Yes\" |
    array_ssi_treatments_wound_debridement %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Wound debridement\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    (1*(array_sso_treatments_suture_excision %in% \"Yes\" |
    array_ssi_treatments_suture_excision %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Suture excision\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    (1*(array_sso_treatments_percutaneous_drainage %in% \"Yes\" |
    array_ssi_treatments_percutaneous_drainage %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Percutaneous drainage\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    (1*(array_sso_treatments_partial_mesh_removal %in% \"Yes\" |
    array_ssi_treatments_partial_mesh_removal %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Partial mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    (1*(array_sso_treatments_complete_mesh_removal %in% \"Yes\" |
    array_ssi_treatments_complete_mesh_removal %in% \"Yes\")) %>% factor(0:1)
    , xlab = \"@@Complete mesh removal\"
    , dt = data %>% dplyr:::filter(ssi_sso_treatment == 1)
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"SSI/SSO outcomes\")"
    
    , "tbl9 <- list() %>%
    empty_entry_all(fill = c(\"Subject re-encounters<sup>cata</sup>\",\"N (%)\")) %>%
    binary_entry_all(
    e_between_visit_clinic %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Clinic\"
    ) %>%
    binary_entry_all(
    e_between_visit_emergency_room %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Emergency room\"
    ) %>%
    binary_entry_all(
    flg_readmission %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Re-admission within 30 days\"
    ) %>%
    empty_entry_all(fill = c(\"Reported reasons for re-admission<sup>cata</sup>\",\"N\")) %>%
    binary_entry_all(
    array_readmission_reason_pain %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Pain\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_readmission_reason_prosthetic_related_complication %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Prosthetic related complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_readmission_reason_wound_complication %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Wound complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_readmission_reason_bleeding_complication %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Bleeding complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_readmission_reason_thrombotic_complication_noncardiac %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Thrombotic complication (non-cardiac)\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_readmission_reason_gastrointestinal_complication %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Gastrointestinal complication\"
    , dt = data %>% dplyr:::filter(flg_readmission == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_reoperation %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"Reoperation\"
    ) %>%
    empty_entry_all(
    fill = c(\"Reoperation type<sup>cata</sup>\", \"N\")) %>%
    binary_entry_all(
    array_reop_type_unrecognized_bowel_injury %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Unrecognized bowel injury\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_reop_type_major_wound_complication %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Major wound complication\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_reop_type_postoperative_bleeding %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Postoperative bleeding\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_reop_type_early_recurrence %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Early recurrence\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_reop_type_bowel_obstruction %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Bowel obstruction\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_reop_type_mesh_excision %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Mesh excision\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    array_reop_type_unrelated_intraabdominal_pathology %>% factor(levels = c(\"No\", \"Yes\"))
    , xlab = \"@@Unrelated intraabdominal pathology\"
    , dt = data %>% dplyr:::filter(flg_reoperation == \"Yes\")
    , pvalue = FALSE
    , fmt = count_fmt
    ) %>%
    binary_entry_all(
    flg_recurrence %>% factor(levels = c(\"No\", \"Yes\"))
    ) %>%
    rbindlist %>%
    as.data.frame %>%
    `attr<-`(\"title\",\"Post-operative through 30 day outcomes\")"
    )
  
  
  
  # table_list <- lapply(table_list, gsub, pattern = "= dt", replacement = paste0("= ", dt))
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




