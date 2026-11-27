# Generated orchestration around verbatim Rmd call and export expressions.

detect_original_IVT_fixation <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_IVT <-           eyemovements::IVT(
            data = d,
            vel_threshold = v,
            min_fix_dur = min_fix_dur,
            return_saccades = FALSE,
            dva_x = 0.01865554,
            dva_y = 0.01919689
          )

if(is.null(algo_IVT) || nrow(algo_IVT)==0) return(NULL)
        ev <- algo_IVT %>%
          dplyr::transmute(
            method = "IVT",
            event = "fixation",
            smoothing = s,
            parameter = paste0("vel_", v),
            sub = current_sub,
            Trial_Id = current_trial,
            fix_start = fix_start,
            fix_end = fix_end,
            duration = fix_end - fix_start
          )

ev
}

detect_original_IDT_fixation <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_IDT <-           eyemovements::IDT(
            data = d,
            disp_threshold = disp,
            window_threshold = window_threshold,
            return_saccades = FALSE,
            dva_x = 0.01865554,
            dva_y = 0.01919689
          )

if(is.null(algo_IDT) || nrow(algo_IDT)==0) return(NULL)
        ev <- algo_IDT %>%
          dplyr::transmute(
            method = "IDT",
            event = "fixation",
            smoothing = s,
            parameter = paste0("disp_", disp),
            sub = current_sub,
            Trial_Id = current_trial,
            fix_start = fix_start,
            fix_end = fix_end,
            duration = fix_end - fix_start
          )

ev
}

detect_original_EK03_fixation <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_EK <-           eyemovements::EngbertKliegl03(
            data = d,
            lambda = l,
            min_fix_dur = 50,
            return_saccades = FALSE,
            dva_x = 0.01865554,
            dva_y = 0.01919689
          )

if(is.null(algo_EK) || nrow(algo_EK)==0) return(NULL)
        ev <- algo_EK %>%
          dplyr::transmute(
            method = "EK03",
            event = "fixation",
            smoothing = s,
            parameter = paste0("lambda_", l),
            sub = current_sub,
            Trial_Id = current_trial,
            fix_start = fix_start,
            fix_end = fix_end,
            duration = fix_end - fix_start
          )

ev
}

detect_original_IHMM_fixation <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_IHMM <-         eyemovements::IHMM(
          data = d,
          min_fix_dur = 50,
          return_saccades = FALSE,
          dva_x = 0.01865554,
          dva_y = 0.01919689
        )

if(is.null(algo_IHMM) || nrow(algo_IHMM)==0) return(NULL)
      ev <- algo_IHMM %>%
        dplyr::transmute(
          method = "IHMM",
          event = "fixation",
          smoothing = s,
          parameter = "none",
          sub = current_sub,
          Trial_Id = current_trial,
          fix_start = fix_start,
          fix_end = fix_end,
          duration = fix_end - fix_start
        )

ev
}

detect_original_IVT_saccade <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_IVT <-           eyemovements::IVT(
            data = d,
            vel_threshold = v,
            min_fix_dur = min_fix_dur,
            return_saccades = TRUE,
            dva_x = 0.01865554,
            dva_y = 0.01919689
          )

if(is.null(algo_IVT) || nrow(algo_IVT)==0) return(NULL)
        ev <- algo_IVT %>%
          dplyr::transmute(
            method = "IVT",
            event = "saccade",
            smoothing = s,
            parameter = paste0("vel_", v),
            sub = current_sub,
            Trial_Id = current_trial,
            sacc_start = sacc_start,
            sacc_end = sacc_end,
            duration = sacc_end - sacc_start
          )

ev
}

detect_original_IDT_saccade <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_IDT <-           eyemovements::IDT(
            data = d,
            disp_threshold = disp,
            window_threshold = window_threshold,
            return_saccades = TRUE,
            dva_x = 0.01865554,
            dva_y = 0.01919689
          )

if(is.null(algo_IDT) || nrow(algo_IDT)==0) return(NULL)
        ev <- algo_IDT %>%
          dplyr::transmute(
            method = "IDT",
            event = "saccade",
            smoothing = s,
            parameter = paste0("disp_", disp),
            sub = current_sub,
            Trial_Id = current_trial,
            sacc_start = sacc_start,
            sacc_end = sacc_end,
            duration = sacc_end - sacc_start
          )

ev
}

detect_original_EK03_saccade <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_EK <-           eyemovements::EngbertKliegl03(
            data = d,
            lambda = l,
            min_fix_dur = 50,
            return_saccades = TRUE,
            dva_x = 0.01865554,
            dva_y = 0.01919689
          )

if(is.null(algo_EK) || nrow(algo_EK)==0) return(NULL)
        ev <- algo_EK %>%
          dplyr::transmute(
            method = "EK03",
            event = "saccade",
            smoothing = s,
            parameter = paste0("lambda_", l),
            sub = current_sub,
            Trial_Id = current_trial,
            sacc_start = sacc_start,
            sacc_end = sacc_end,
            duration = sacc_end - sacc_start
          )

ev
}

detect_original_IHMM_saccade <- function(d,current_sub,current_trial,s,v=NULL,disp=NULL,l=NULL,min_fix_dur=50,window_threshold=50) {
algo_IHMM <-         eyemovements::IHMM(
          data = d,
          min_fix_dur = 50,
          return_saccades = TRUE,
          dva_x = 0.01865554,
          dva_y = 0.01919689
        )

if(is.null(algo_IHMM) || nrow(algo_IHMM)==0) return(NULL)
      ev <- algo_IHMM %>%
        dplyr::transmute(
          method = "IHMM",
          event = "saccade",
          smoothing = s,
          parameter = "none",
          sub = current_sub,
          Trial_Id = current_trial,
          sacc_start = sacc_start,
          sacc_end = sacc_end,
          duration = sacc_end - sacc_start
        )

ev
}
