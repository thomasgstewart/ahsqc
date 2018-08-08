#' Get ORACLE formula
#'
#' Returns the ORACLE regression formula
#' @details Returns the formula used in the ORACLE regression models
#' @param y Dependent variable
#' @keywords AHSQC, ORACLE
#' @export
#' @examples
#' # Not run:
#' # d0 <- ahs_get_data()

get_ORACLE_formula <- function(y){

    vars <- Cs(
      1 
    , female
    , flg_cmb_recurrent_incisional
    , indication_bowel_obstruction
    , indication_enlarging_hernia
    , indication_fistula
    , indication_infected_mesh
    , indication_pain
    , flg_cmb_open_abdomen_hx
    , flg_cmb_dialysis
    , flg_cmb_diabetes
    , flg_cmb_copd
    , nicotine_use
    , flg_cmb_steroids
    , flg_myofascial_release
    , planned_concomitant_procedure
    , flg_stoma_present
    , e_procedure_category
    , e_asaclass
    , e_cmb_functionalstatus
    , e_mesh_location
    , e_race
    , rcs(val_age_new,3)
    , rcs(val_calc_bmi2,3)
    , rcs(val_hern_width,3)
    , flg_cmb_recurrent_incisional:female
    , flg_cmb_recurrent_incisional:rcs(val_age_new,3)
    , female:e_procedure_category
  )
  
  formula <- as.formula(paste(y,"~", paste(vars, collapse="+")))
  
 return(formula)
}

