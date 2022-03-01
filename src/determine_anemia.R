library(here)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)

dat <- read_rds(here("data/database.rds"))
# Differences between the hematocrit values obtained by centrifuge or runrun
dat <- dat %>% pivot_wider(names_from = method, values_from = hematocrit)

#' Diagnosis of anemia based on the hematocrit values
#' 
#' This function includes the processes of transforming hematocrit values to
#' hemoglobin values and using the tables of normal values (that require age
#' and sex) to diagnose anemia.
#'
#' @param hematocrit Numeric vector with the hematocrit value.
#' @param age Numeric vector with the age.
#' @param sex Character vector with the sex ("F" or "M").
#'
#' @return A logical vector with the result if anemia is diagnosed (`TRUE`) or
#'   not (`FALSE`).
#' @export
#'
#' @examples
anemia_with_hematocrit <- function(hematocrit, age, sex) {
  
  hematocrit_to_hemoglobin <- function(hematocrit) {
    hematocrit / 3
  }
  hemoglobin <- hematocrit_to_hemoglobin(hematocrit)
  
  adjust_hemoglobin_to_altitude <- function(hemoglobin) {
    # 2459 meters above sea level (Characato)
    hemoglobin - 1.2
  }
  adjusted_hemoglobin <- adjust_hemoglobin_to_altitude(hemoglobin)
  
  
  detect_anemia <- function(hemoglobin, age, sex) {
    if (is.na(hemoglobin)) {
      return(NA)
    }
    
    if (age < 12) {
      if (hemoglobin < 11.5) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else if (age >= 12 & age <= 14) {
      if (hemoglobin < 12) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else if (age >= 15) {
      if (sex == "F") {
        if (hemoglobin < 12) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else if (sex == "M") {
        if (hemoglobin < 13) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    }
  }
  
  detect_anemia(hemoglobin = adjusted_hemoglobin, age = age, sex = sex)
}

dat$anemia_centrifuge <- pmap_lgl(
  select(dat, hematocrit = centrifuge, age, sex),
  anemia_with_hematocrit
)
dat$anemia_runrun <- pmap_lgl(
  select(dat, hematocrit = runrun, age, sex),
  anemia_with_hematocrit
)
