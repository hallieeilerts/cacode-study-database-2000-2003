
fn_aggCODbySex <- function(DAT, KEY_COD_RECLASS){

  # !!!! RELIES ON DPLYR FOR SOME OPERATIONS. SHOULD FIX THIS
  
  #' @title Aggregates CODs and total deaths for males and females
  # 
  #' @description Creates a temporary ID underwhich CODs and total deaths will be aggregated.
  #' Determines if records need to be aggregated by checking if both sexes reported for same ages/years
  #' The COD is reported in both data points, they are summed
  #' If the COD is NA is both, it is left NA
  #' If the COD is reported in one and NA in the other, both are assigned to the appropriate "other" category (e.g, communicable, non-communicable, injuries)
  #
  #' @param DAT Study data with an idtemp column made specifically for the aggregation
  #' @param KEY_COD_RECLASS Data frame with reported CODs in the data in column 1 and the reclassified categories for "other" in column 2
  #' @return Data frame with aggregated data points where called for 
  
  # For testing function
  #DAT <- datWide
  #KEY_COD_RECLASS <- key[,c("cod_reclass","cod_level2")]
  
  # Rename COD reclassification columns
  names(KEY_COD_RECLASS) <- c("col1", "col2")
  
  # Create a temporary id that does not include sex
  DAT$idtemp <- paste(DAT$ref_id, 
                      DAT$iso3, 
                      DAT$year_mid, 
                      DAT$age_lb_m, DAT$age_ub_m, sep = '-')
  
 
  # Identify records that have male and female data point reported for same study
  dat_prep <- DAT %>% 
    group_by(idtemp) %>%
    mutate(n_idtemp = n(),
           n_sex = length(unique(sex)),
           malefemale = all(sexLabels[2:3] %in% sex)) %>%
    ungroup()
  
  # Separate data points that need aggregating from those that don't
  dat_needsAgg <- subset(dat_prep, n_idtemp == 2 & n_sex == 2 & malefemale == TRUE)
  
  if(nrow(dat_needsAgg) > 0){
    
    # Save data points that don't need aggregating
    dat_other <- subset(dat_prep, !(idtemp %in% dat_needsAgg$idtemp))
    dat_other <- dat_other[!(names(dat_other) %in% c("n_idtemp","n_sex","malefemale"))]
    dat_needsAgg <- dat_needsAgg[!(names(dat_needsAgg) %in% c("n_idtemp","n_sex","malefemale"))]
    
    # Reported CODs in data
    v_cod_reported <- KEY_COD_RECLASS$col1
    v_cod_reported <- v_cod_reported[!is.na(v_cod_reported)]
    v_cod_reported <- unique(v_cod_reported)
    # Columns that should be aggregated
    v_col_agg <- c("totdeaths", v_cod_reported)

    # Causes aggregated differently depending on whether 
    # (i) both causes reported, 
    # (ii) both causes missing,
    # (iii) one reported and one missing
    
    # Reshape to long and see whether cods are reported or missing in both sexes
    df_ncodrep <- dat_needsAgg %>%
      pivot_longer(cols = any_of(v_col_agg),
                   names_to = "agg") %>% 
      arrange(idtemp, agg) %>%
      group_by(idtemp, agg) %>%
      mutate(n_missing = sum(is.na(value)))
    df_bothrep <- df_ncodrep %>% filter(n_missing == 0) %>% select(-n_missing)
    df_bothmiss <- df_ncodrep %>% filter(n_missing == 2) %>% select(-n_missing)
    df_onerep <- df_ncodrep %>% filter(n_missing == 1) %>% select(-n_missing)
    
    ### If both reported, pivot wide and aggregate by idtemp, and pivot back long and drop NA causes created from wide pivot
    df_bothrepA <- df_bothrep %>%
      group_by(idtemp) %>%
      pivot_wider(
        id_cols = idtemp,
        names_from = agg,
        values_from = value,
        values_fn=sum
      ) %>% 
      pivot_longer(cols = any_of(v_col_agg),
                   names_to = "agg") %>% 
      filter(!is.na(value))
    # Keep one row of id columns for new combined data point
    df_bothrepB <- df_bothrep %>%
      group_by(idtemp) %>%
      select(-c(agg, value)) %>%
      slice(1)
    # Merge aggregate cods back with id columns
    df_bothrep <- merge(df_bothrepA, df_bothrepB, by = "idtemp")
    
    ### If both are missing, only keep one row
    df_bothmiss <- df_bothmiss %>%
      group_by(idtemp, agg) %>% 
      slice(1)
    
    ### If one is missing and the other is reported, keep the non-missing
    # Recode the original cause as NA and re-categorize it as the appropriate "other" category
    df_onerep <- df_onerep %>%
      filter(!is.na(value)) %>%
      left_join(., KEY_COD_RECLASS, by = c("agg" = "col1"))
    df_onerepA <- df_onerep %>% mutate(value = NA)
    df_onerepB <- df_onerep %>% mutate(agg = col2)
    df_onerep <- rbind(df_onerepA, df_onerepB) %>%
      select(-col2)
    
    ### Combine all aggregated data
    # Pivot wide and aggregate again (for the new "other" category created in df_onerep)
    dat_aggA <- rbind(df_bothrep, df_bothmiss, df_onerep) %>%
      group_by(idtemp) %>%
      pivot_wider(
        id_cols = idtemp,
        names_from = agg,
        values_from = value,
        values_fn=sum
      ) 
    # Keep one row of id columns for combined data points
    dat_aggB <- rbind(df_bothrep, df_bothmiss, df_onerep) %>%
      group_by(idtemp) %>%
      select(-c(agg, value)) %>%
      slice(1)
    # Merge aggregate cods back with id columns
    dat_agg <- merge(dat_aggA, dat_aggB, by = "idtemp") %>%
      mutate(sex = sexLabels[1])
    # Recode letter for sex in id var
    dat_agg$id <- sapply(dat_agg$id, function(x){
                          id_minus1 <- substr(x, 1, nchar(x)-1)
                          paste(id_minus1, substr(sexLabels[1],1,1), sep = "")  
                          })

    
    ### Combine aggregated with data points that didn't need to be aggregated
    dat_agg_sex <- rbind(dat_other, dat_agg)
    dat_agg_sex <- dat_agg_sex[!(names(dat_agg_sex) %in% "idtemp")]
    dat_agg_sex <- dat_agg_sex[order(dat_agg_sex$id),]

    return(dat_agg_sex)
  }else{
    DAT <- DAT[!(names(DAT) %in% "idtemp")]
    return(DAT)
  }
}

