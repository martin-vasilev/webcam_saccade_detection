library(devtools)
library(readr)
library(dplyr)
library(ggplot2)

load_all()

#dat <- readr::read_csv(file.choose(), show_col_types = FALSE)

library(eyemovements)

I_VT<- dat %>%
  transmute(time = time_start, x = x, y = y,
            sub= sub,Trial_Id= Trial_Id)%>%
  group_by(sub, Trial_Id)%>%
  group_modify(~ DetectFixations(data = .x, method = "I-VT",
              vel_threshold = 30, min_fix_dur = 50))%>%
  ungroup()%>%
  mutate(method = "I-VT")

  
I_DT<- dat %>%
  transmute(time = time_start, x = x, y = y,
            sub= sub,Trial_Id= Trial_Id)%>%
  group_by(sub, Trial_Id)%>%
  group_modify(~ DetectFixations(data = .x, method = "I-DT",
                                 window_threshold = 100, 
                                 disp_threshold = 3, min_fix_dur = 50))%>%
  ungroup()%>%
  mutate(method = "I-DT")


mean(I_VT$fix_dur)
mean(I_DT$fix_dur)


trial_df <- dat %>%
  filter(sub == 11, Trial_Id == 14, Task_Name == "Single_line_sentences") %>%
  

fix_ivt <- DetectFixations(trial_df, method = "I-VT", vel_threshold = 40, min_fix_dur = 50) %>%
  mutate(method = "I-VT")

fix_idt <- DetectFixations(trial_df, method = "I-DT", window_threshold = 100, disp_threshold = 3, min_fix_dur = 50) %>%
  mutate(method = "I-DT")

fix_all <- bind_rows(fix_ivt, fix_idt)

ggplot() +
  geom_line(data = trial_df, aes(time, x), alpha = 0.6) +
  geom_rect(
    data = fix_all,
    aes(xmin = fix_start, xmax = fix_end, ymin = -Inf, ymax = Inf, fill = method),
    alpha = 0.15
  ) +
  theme_minimal() +
  labs(title = "I-VT vs I-DT (overlay)", x = "time (ms)", y = "x (px)")


