require(tidyverse)
require(devtools)
require(readr)
require(xgboost)
require(caret)
require(bbd)
require("plotrix")
require("MASS")
require("data.table")
require(Boruta)

#Obtaining 
all_2022 = bbd::statcast(start = "2022-04-07",
                         end = "2022-10-05",
                         process = TRUE,
                         names = TRUE,
                         verbose = TRUE)
write_csv(all_2022,"all2022.csv")

all_2022 <- read_csv("~/all2022.csv")

#Unique pitches thrown by every pitcher
#As well as pitcher sums of total thrown pitches per type
pitcher_sums_2022 <- all_2022 %>%
  drop_na(pitch_type) %>%
  group_by(pitcher_name) %>%
  mutate(total_pitches = n()) %>%
  filter((pitch_type == "FF" | pitch_type == "CU" | pitch_type == "FC" |
            pitch_type == "SI" | pitch_type == "SL" | pitch_type == "CH" | pitch_type == "FS")) %>%
  group_by(pitcher_name, pitch_type) %>%
  mutate(pitches_by_type = n()) %>%ungroup() %>%
  group_by(pitcher_name) %>%
  mutate(pitches_on_two_strikes = ifelse(strikes == 2, 1, 0)) %>%
  mutate(pitches_on_two_strikes = sum(pitches_on_two_strikes)) %>%
  summarise(pitches_by_type, pitch_type, total_pitches, pitches_on_two_strikes) %>%
  distinct()



#Standardize pitchers who  face hitters who are left handed so outside/inside pitches have same values.
#Standardize pitchers who are left handed so release position is the same.
pitcher_dat_2022 <- all_2022 %>%
  left_join(pitcher_sums_2022, by = c("pitcher_name" = "pitcher_name", "pitch_type" = "pitch_type")) %>%
  filter((pitch_type == "FF" | pitch_type == "CU" | pitch_type == "FC" |
            pitch_type == "SI" | pitch_type == "SL" | pitch_type == "CH" | pitch_type == "FS")) %>%
  mutate(swStr_binary = ifelse(description %in% c("swinging_strike"), 1, 0)) %>%
  mutate(pitch_pct = pitches_by_type/total_pitches) %>%
  mutate(release_pos_x = ifelse(p_throws == "L", release_pos_x * -1, release_pos_x))


#Dataframe that contains pitches that were swinging strikes
swstr_2022 <- pitcher_dat_2022 %>%
  group_by(pitcher_name, pitch_type) %>%
  mutate(pitch_thrown_2s_type = ifelse(strikes == 2, 1, 0)) %>%
  mutate(pitch_thrown_2s_type = sum(pitch_thrown_2s_type)) %>%
  filter(description %in% c("swinging_strike")) %>%
  group_by(pitcher_name, pitch_type, description) %>%
  mutate(SS_by_Pitch = n()) %>%
  spread(description, SS_by_Pitch) %>%
  mutate(pitch_pct = round(pitches_by_type/total_pitches, 4),
         swStr = round(swinging_strike/pitches_by_type, 4),
         two_strike_opps = round(pitch_thrown_2s_type/pitches_on_two_strikes,4)) %>%
  left_join(pitcher_sums, by = c("pitch_type", "pitcher_name", "pitches_by_type", "total_pitches")) %>%
  dplyr::select(pitcher_name, pitch_type, pitch_pct,p_throws, pfx_x, pfx_z, pitch_thrown_2s_type,pitches_on_two_strikes,total_pitches,
         release_pos_x, release_pos_z, pitch_pct, swStr,swStr_binary,two_strike_opps,
         swinging_strike, pitches_by_type, release_speed, release_extension,
         release_spin_rate, plate_x, plate_z)


#Creating velocity, horizontal movement and vertical movement differences for offspeed pitches.
four_seam_fastballs_averages_2022 <- all_2022 %>%
  filter((pitch_type == "FF")) %>%
  group_by(pitcher_name, pitch_type) %>%
  summarise(pitches = n(),
            avg_fb_velo = mean(release_speed, na.rm = TRUE),
            avg_fb_hmov = mean(pfx_x, na.rm = TRUE),
            avg_fb_vmov = mean(pfx_z, na.rm= TRUE),
            avg_fb_bauer_unit = mean(release_spin_rate/avg_fb_velo, na.rm = TRUE)) %>%
  distinct()

off_speed_peripherals_2022 <- all_2022 %>%
  filter((pitch_type == "FC" | pitch_type == "SL" | 
            pitch_type == "CH" | pitch_type == "FS" | pitch_type == "CU")) %>%
  group_by(pitcher_name, pitch_type) %>%
  summarise(pitches = n(),
            avg_os_velo = mean(release_speed, na.rm = TRUE),
            avg_os_hmov = mean(pfx_x, na.rm=TRUE),
            avg_os_vmov = mean(pfx_z, na.rm = TRUE),
            avg_os_release_pos_x = mean(release_pos_x, na.rm = TRUE),
            avg_os_release_pos_z = mean(release_pos_z, na.rm = TRUE),
            avg_os_release_extension = mean(release_extension, na.rm = TRUE),
            avg_os_spin = mean(release_spin_rate, na.rm = TRUE),
            avg_os_loc_x = mean(plate_x, na.rm = TRUE),
            avg_os_loc_z = mean(plate_z, na.rm = TRUE),
            avg_os_bauer_unit = mean(release_spin_rate/release_speed, na.rm = TRUE)) %>%
  left_join(four_seam_fastballs_averages_2022 %>% dplyr::select(pitcher_name, avg_fb_velo,
                                                         avg_fb_hmov, avg_fb_vmov, avg_fb_bauer_unit), by = c("pitcher_name")) %>%
  mutate(velo_difference = avg_fb_velo - avg_os_velo,
         hmov_difference = avg_fb_hmov - avg_os_hmov,
         vmov_difference = avg_fb_vmov - avg_os_vmov,
         bauer_difference = avg_fb_bauer_unit - avg_os_bauer_unit) %>%
  distinct() %>%
  dplyr::select(-avg_os_velo, -avg_os_hmov, -avg_os_vmov) %>%
  left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr, pitch_pct), by = c("pitcher_name", "pitch_type")) %>%
  mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
  distinct()


#Getting pitch average pitch metrics for each pitcher and pitch type
############################################################################################################################

#Four-seam fastball
  fb_info_2022 <- pitcher_dat_2022 %>%
    filter((pitch_type == "FF")) %>%
    group_by(pitcher_name, pitch_type) %>%
    summarise(pitches = pitches_by_type,
              pitch_pct,
              avg_fb_velo = mean(release_speed, na.rm = T),
              avg_fb_hmov = mean(pfx_x, na.rm = T),
              avg_fb_vmov = mean(pfx_z, na.rm = T),
              avg_fb_release_x = mean(release_pos_x, na.rm = T),
              avg_fb_release_z = mean(release_pos_z, na.rm = T),
              avg_fb_release_ext = mean(release_extension, na.rm = T),
              avg_fb_bauer = mean(release_spin_rate, na.rm = T)/mean(release_speed, na.rm= T),
              avg_fb_spin = mean(release_spin_rate, na.rm = T),
              avg_fb_loc_x = mean(plate_x, na.rm = T),
              avg_fb_loc_z = mean(plate_z, na.rm = T),
              avg_fb_spin_axis = mean(spin_axis, na.rm= T)) %>%
    left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr, two_strike_opps, pitch_thrown_2s_type),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    mutate(two_strike_opps = ifelse(is.na(two_strike_opps), 0, two_strike_opps)) %>%
    filter(pitches >= 57) %>%
    distinct()

#Sinker
  si_info_2022 <- pitcher_dat_2022 %>%
    filter(pitch_type == "SI") %>%
    group_by(pitcher_name, pitch_type) %>%
    summarise(pitches = pitches_by_type,
              pitch_pct,
              avg_si_velo = mean(release_speed, na.rm = T),
              avg_si_hmov = mean(pfx_x, na.rm = T),
              avg_si_vmov = mean(pfx_z, na.rm = T),
              avg_si_release_x = mean(release_pos_x, na.rm = T),
              avg_si_release_z = mean(release_pos_z, na.rm = T),
              avg_si_release_ext = mean(release_extension, na.rm = T),
              avg_si_bauer = mean(release_spin_rate, na.rm = T)/mean(release_speed, na.rm= T),
              avg_si_spin = mean(release_spin_rate, na.rm = T),
              avg_si_loc_x = mean(plate_x, na.rm = T),
              avg_si_loc_z = mean(plate_z, na.rm = T)) %>%
    left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr, two_strike_opps, pitch_thrown_2s_type),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    filter(pitches >= 50) %>%
    distinct()
    

#Sliders
  sl_info_2022 <- pitcher_dat_2022 %>%
    filter((pitch_type == "SL")) %>%
    group_by(pitcher_name, pitch_type) %>%
    summarise(pitches = pitches_by_type,
              pitch_pct,
              avg_sl_velo = mean(release_speed, na.rm = T),
              avg_sl_hmov = mean(pfx_x, na.rm = T),
              avg_sl_vmov = mean(pfx_z, na.rm = T),
              avg_sl_release_x = mean(release_pos_x, na.rm = T),
              avg_sl_release_z = mean(release_pos_z, na.rm = T),
              avg_sl_release_ext = mean(release_extension, na.rm = T),
              avg_sl_bauer = mean(release_spin_rate, na.rm = T)/mean(release_speed, na.rm= T),
              avg_sl_spin = mean(release_spin_rate, na.rm = T),
              avg_sl_loc_x = mean(plate_x, na.rm = T),
              avg_sl_loc_z = mean(plate_z, na.rm = T)) %>%
    left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr,
                                           two_strike_opps, pitch_thrown_2s_type),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    left_join(off_speed_peripherals_2022 %>% dplyr::select(pitcher_name, pitch_type, 
                                                           velo_difference, hmov_difference, vmov_difference),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    mutate(velo_difference = ifelse(is.na(velo_difference),0, velo_difference),
           hmov_difference = ifelse(is.na(hmov_difference),0, hmov_difference),
           vmov_difference = ifelse(is.na(vmov_difference),0, vmov_difference))%>%
    filter(pitches >= 50) %>%
    distinct()
  
#Cutters
  fc_info_2022 <- pitcher_dat_2022 %>%
    filter(pitch_type == "FC") %>%
    group_by(pitcher_name, pitch_type) %>%
    summarise(pitches = pitches_by_type,
              pitch_pct,
              avg_fc_velo = mean(release_speed, na.rm = T),
              avg_fc_hmov = mean(pfx_x, na.rm = T),
              avg_fc_vmov = mean(pfx_z, na.rm = T),
              avg_fc_release_x = mean(release_pos_x, na.rm = T),
              avg_fc_release_z = mean(release_pos_z, na.rm = T),
              avg_fc_release_ext = mean(release_extension, na.rm = T),
              avg_fc_bauer = mean(release_spin_rate, na.rm = T)/mean(release_speed, na.rm= T),
              avg_fc_spin = mean(release_spin_rate, na.rm = T),
              avg_fc_loc_x = mean(plate_x, na.rm = T),
              avg_fc_loc_z = mean(plate_z, na.rm = T)) %>%
    left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr,
                                           two_strike_opps, pitch_thrown_2s_type),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    left_join(off_speed_peripherals_2022 %>% dplyr::select(pitcher_name, pitch_type, 
                                                           velo_difference, hmov_difference, vmov_difference),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    mutate(velo_difference = ifelse(is.na(velo_difference),0, velo_difference),
           hmov_difference = ifelse(is.na(hmov_difference),0, hmov_difference),
           vmov_difference = ifelse(is.na(vmov_difference),0, vmov_difference)) %>%
    filter(pitches >= 50) %>%
    distinct()
  
#Changeups
  ch_info_2022 <- pitcher_dat_2022 %>%
    filter(pitch_type == "CH") %>%
    group_by(pitcher_name, pitch_type) %>%
    summarise(pitches = pitches_by_type,
              pitch_pct,
              avg_ch_velo = mean(release_speed, na.rm = T),
              avg_ch_hmov = mean(pfx_x, na.rm = T),
              avg_ch_vmov = mean(pfx_z, na.rm = T),
              avg_ch_release_x = mean(release_pos_x, na.rm = T),
              avg_ch_release_z = mean(release_pos_z, na.rm = T),
              avg_ch_release_ext = mean(release_extension, na.rm = T),
              avg_ch_bauer = mean(release_spin_rate, na.rm = T)/mean(release_speed, na.rm= T),
              avg_ch_spin = mean(release_spin_rate, na.rm = T),
              avg_ch_loc_x = mean(plate_x, na.rm = T),
              avg_ch_loc_z = mean(plate_z, na.rm = T)) %>%
    left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr,
                                           two_strike_opps, pitch_thrown_2s_type),
              by = c("pitcher_name", "pitch_type")) %>%
    left_join(off_speed_peripherals_2022 %>% dplyr::select(pitcher_name, pitch_type, 
                                                           velo_difference, hmov_difference, vmov_difference),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    mutate(velo_difference = ifelse(is.na(velo_difference),0, velo_difference),
           hmov_difference = ifelse(is.na(hmov_difference),0, hmov_difference),
           vmov_difference = ifelse(is.na(vmov_difference),0, vmov_difference)) %>%
    filter(pitches >= 50) %>%
    distinct()
  
#Curveballs
  cu_info_2022 <- pitcher_dat_2022 %>%
    filter(pitch_type == "CU") %>%
    group_by(pitcher_name, pitch_type) %>%
    summarise(pitches = pitches_by_type,
              pitch_pct,
              avg_cu_velo = mean(release_speed, na.rm = T),
              avg_cu_hmov = mean(pfx_x, na.rm = T),
              avg_cu_vmov = mean(pfx_z, na.rm = T),
              avg_cu_release_x = mean(release_pos_x, na.rm = T),
              avg_cu_release_z = mean(release_pos_z, na.rm = T),
              avg_cu_release_ext = mean(release_extension, na.rm = T),
              avg_cu_bauer = mean(release_spin_rate, na.rm = T)/mean(release_speed, na.rm= T),
              avg_cu_spin = mean(release_spin_rate, na.rm = T),
              avg_cu_loc_x = mean(plate_x, na.rm = T),
              avg_cu_loc_z = mean(plate_z, na.rm = T)) %>%
    left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr,
                                           two_strike_opps, pitch_thrown_2s_type),
              by = c("pitcher_name", "pitch_type")) %>%
    left_join(off_speed_peripherals_2022 %>% dplyr::select(pitcher_name, pitch_type, 
                                                           velo_difference, hmov_difference, vmov_difference),
              by = c("pitcher_name", "pitch_type")) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    mutate(velo_difference = ifelse(is.na(velo_difference),0, velo_difference),
           hmov_difference = ifelse(is.na(hmov_difference),0, hmov_difference),
           vmov_difference = ifelse(is.na(vmov_difference),0, vmov_difference)) %>%
    mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
    filter(pitches >= 50) %>%
    distinct()
  
#Splitter was removed due to the lack of sample size causing issues with modeling.  
# #Splitter
#   fs_info_2022 <- pitcher_dat_2022 %>%
#     filter(pitch_type == "FS") %>%
#     group_by(pitcher_name, pitch_type) %>%
#     summarise(pitches = pitches_by_type,
#               pitch_pct,
#               avg_fs_velo = mean(release_speed, na.rm = T),
#               avg_fs_hmov = mean(pfx_x, na.rm = T),
#               avg_fs_vmov = mean(pfx_z, na.rm = T),
#               avg_fs_release_x = mean(release_pos_x, na.rm = T),
#               avg_fs_release_z = mean(release_pos_z, na.rm = T),
#               avg_fs_release_ext = mean(release_extension, na.rm = T),
#               avg_fs_bauer = mean(release_spin_rate, na.rm = T)/mean(release_speed, na.rm= T),
#               avg_fs_spin = mean(release_spin_rate, na.rm = T),
#               avg_fs_loc_x = mean(plate_x, na.rm = T),
#               avg_fs_loc_z = mean(plate_z, na.rm = T)) %>%
#     left_join(swstr_2022 %>% dplyr::select(pitcher_name, pitch_type, swStr,
#                                            two_strike_opps, pitch_thrown_2s_type),
#               by = c("pitcher_name", "pitch_type")) %>%
#     left_join(off_speed_peripherals_2022 %>% dplyr::select(pitcher_name, pitch_type, 
#                                                            velo_difference, hmov_difference, vmov_difference),
#               by = c("pitcher_name", "pitch_type")) %>%
#     mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
#     mutate(velo_difference = ifelse(is.na(velo_difference),0, velo_difference),
#            hmov_difference = ifelse(is.na(hmov_difference),0, hmov_difference),
#            vmov_difference = ifelse(is.na(vmov_difference),0, vmov_difference)) %>%
#     mutate(swStr = ifelse(is.na(swStr),0, swStr)) %>%
#     filter(pitches >= 41) %>%
#     distinct()
  
#Only 57 pitchers for splitters before filtering, results should be taken lighter
    
  
  
  
############################################################################################################################
#Create the baseline models with every variable included.
  
  set.seed(42)
  
#4-Seam Fastballs  
  fb_lm_smp_size <- floor(.7 * nrow(fb_info_2022))
  fb_lm_train_indexes <- sample(seq_len(nrow(fb_info_2022)), size = fb_lm_smp_size)
  fb_lm_train <- fb_info_2022[fb_lm_train_indexes,]
  fb_lm_test <- head(fb_info_2022[-fb_lm_train_indexes,], fb_lm_smp_size)

  #Fastball info with linear model
  
  fb_lm_2022 <- lm(swStr ~  avg_fb_velo + avg_fb_hmov + avg_fb_vmov + avg_fb_release_x
               + avg_fb_release_z + avg_fb_release_ext + avg_fb_spin +
                 avg_fb_loc_x + avg_fb_loc_z, data = fb_lm_train)
  
  fb_predicted_lm_swstr <- fb_lm_2022 %>% predict(fb_lm_test)

  fb_rmse <- RMSE(fb_predicted_lm_swstr, fb_lm_test$swStr)
  fb_sd <- sd(fb_predicted_lm_swstr)
  
#Sinkers
  si_lm_smp_size <- floor(.7 * nrow(si_info_2022))
  si_lm_train_indexes <- sample(seq_len(nrow(si_info_2022)), size = si_lm_smp_size)
  si_lm_train <- si_info_2022[si_lm_train_indexes,]
  si_lm_test <- head(si_info_2022[-si_lm_train_indexes,], si_lm_smp_size)
  
  si_lm_2022 <- lm(swStr ~  avg_si_velo + avg_si_hmov + avg_si_vmov + avg_si_release_x
                   + avg_si_release_z + avg_si_release_ext + avg_si_spin +
                     avg_si_loc_x + avg_si_loc_z, data = si_lm_train)
  
  si_predicted_lm_swstr <- si_lm_2022 %>% predict(si_lm_test)
  si_rmse <- RMSE(si_predicted_lm_swstr, si_lm_test$swStr)
  si_sd <- sd(si_predicted_lm_swstr)
  
  
#Sliders
  sl_lm_smp_size <- floor(.7 * nrow(sl_info_2022))
  sl_lm_train_indexes <- sample(seq_len(nrow(sl_info_2022)), size = sl_lm_smp_size)
  sl_lm_train <- sl_info_2022[sl_lm_train_indexes,]
  sl_lm_test <- head(sl_info_2022[-sl_lm_train_indexes,], sl_lm_smp_size)
  
  sl_lm_2022 <- lm(swStr ~  avg_sl_velo + avg_sl_hmov + avg_sl_vmov + avg_sl_release_x
                   + avg_sl_release_z + avg_sl_release_ext + avg_sl_spin +
                     avg_sl_loc_x + avg_sl_loc_z, data = sl_lm_train)
  
  sl_predicted_lm_swstr <- sl_lm_2022 %>% predict(sl_lm_test)
  sl_rmse <- RMSE(sl_predicted_lm_swstr, sl_lm_test$swStr)
  sl_sd <- sd(sl_predicted_lm_swstr)
  
#Cutters
  fc_lm_smp_size <- floor(.7 * nrow(fc_info_2022))
  fc_lm_train_indexes <- sample(seq_len(nrow(fc_info_2022)), size = fc_lm_smp_size)
  fc_lm_train <- fc_info_2022[fc_lm_train_indexes,]
  fc_lm_test <- head(fc_info_2022[-fc_lm_train_indexes,], fc_lm_smp_size)
  
  fc_lm_2022 <- lm(swStr ~  avg_fc_velo + avg_fc_hmov + avg_fc_vmov + avg_fc_release_x
                   + avg_fc_release_z + avg_fc_release_ext + avg_fc_spin +
                     avg_fc_loc_x + avg_fc_loc_z, data = fc_lm_train)
  
  fc_predicted_lm_swstr <- fc_lm_2022 %>% predict(fc_lm_test)
  fc_rmse <- RMSE(fc_predicted_lm_swstr, fc_lm_test$swStr)
  fc_sd <- sd(fc_predicted_lm_swstr)

#Changeups
  ch_lm_smp_size <- floor(.7 * nrow(ch_info_2022))
  ch_lm_train_indexes <- sample(seq_len(nrow(ch_info_2022)), size = ch_lm_smp_size)
  ch_lm_train <- ch_info_2022[ch_lm_train_indexes,]
  ch_lm_test <- head(ch_info_2022[-ch_lm_train_indexes,], ch_lm_smp_size)
  
  ch_lm_2022 <- lm(swStr ~  avg_ch_velo + avg_ch_hmov + avg_ch_vmov + avg_ch_release_x
                   + avg_ch_release_z + avg_ch_release_ext + avg_ch_spin +
                     avg_ch_loc_x + avg_ch_loc_z, data = ch_lm_train)
  
  ch_predicted_lm_swstr <- ch_lm_2022 %>% predict(ch_lm_test)
  ch_rmse <- RMSE(ch_predicted_lm_swstr, ch_lm_test$swStr)
  ch_sd <- sd(ch_predicted_lm_swstr)
  
#Curveballs
  
  cu_lm_smp_size <- floor(.7 * nrow(cu_info_2022))
  cu_lm_train_indexes <- sample(seq_len(nrow(cu_info_2022)), size = cu_lm_smp_size)
  cu_lm_train <- cu_info_2022[cu_lm_train_indexes,]
  cu_lm_test <- head(cu_info_2022[-cu_lm_train_indexes,], cu_lm_smp_size)
  
  cu_lm_2022 <- lm(swStr ~  avg_cu_velo + avg_cu_hmov + avg_cu_vmov + avg_cu_release_x
                   + avg_cu_release_z + avg_cu_release_ext + avg_cu_spin +
                     avg_cu_loc_x + avg_cu_loc_z, data = cu_lm_train)
  
  cu_predicted_lm_swstr <- cu_lm_2022 %>% predict(cu_lm_test)
  cu_rmse <- RMSE(cu_predicted_lm_swstr, cu_lm_test$swStr)
  cu_sd <- sd(cu_predicted_lm_swstr)
  
# Splitters
#   fs_lm_smp_size <- floor(.7 * nrow(fs_info_2022))
#   fs_lm_train_indexes <- sample(seq_len(nrow(fs_info_2022)), size = fs_lm_smp_size)
#   fs_lm_train <- fs_info_2022[fs_lm_train_indexes,]
#   fs_lm_test <- head(fs_info_2022[-fs_lm_train_indexes,], fs_lm_smp_size)
#   
#   fs_lm_2022 <- lm(swStr ~  avg_fs_velo + avg_fs_hmov + avg_fs_vmov + avg_fs_release_x
#                    + avg_fs_release_z + avg_fs_release_ext + avg_fs_spin +
#                      avg_fs_loc_x + avg_fs_loc_z, data = fs_lm_train)
#   
#   fs_predicted_lm_swstr <- fs_lm_2022 %>% predict(fs_lm_test)
#   fs_rmse <- RMSE(fs_predicted_lm_swstr, fs_lm_test$swStr)
#   fs_sd <- sd(fs_predicted_lm_swstr)  
  
RMSE_df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(RMSE_df) <-c("Pitch Types", "RMSE", "SD")

pitch_rmse_list <- list(fb_rmse, si_rmse, sl_rmse, fc_rmse, ch_rmse, cu_rmse)

RMSE_df[1,] <- c("Four-seam Fastball", fb_rmse, fb_sd)
RMSE_df[2,] <- c("Sinker", si_rmse, si_sd)
RMSE_df[3,] <- c("Slider", sl_rmse, sl_sd)
RMSE_df[4,] <- c("Cutter", fc_rmse, fc_sd)
RMSE_df[5,] <- c("Change-up", ch_rmse, ch_sd)
RMSE_df[6,] <- c("Curve-ball", cu_rmse, cu_sd)

View(RMSE_df)
  
############################################################################################################################

#Creating the XGBoost model
  
  fb_train_components_2022 <- fb_lm_train %>%
    ungroup() %>%
    dplyr::select(swStr, avg_fb_velo , avg_fb_vmov  , 
                  avg_fb_release_z, avg_fb_spin, avg_fb_loc_z, avg_fb_hmov,
                  avg_fb_release_x, avg_fb_loc_x, avg_fb_release_ext)
  
  fb_test_components_2022 <- fb_lm_test %>%
    ungroup() %>%
    dplyr::select(swStr, avg_fb_velo , avg_fb_vmov  , 
                  avg_fb_release_z,avg_fb_spin, avg_fb_loc_z, avg_fb_hmov,
                  avg_fb_release_x, avg_fb_loc_x, avg_fb_release_ext)
  
  fb_full_components_2022 <- fb_info_2022 %>%
    ungroup() %>%
    dplyr::select(swStr, avg_fb_velo , avg_fb_vmov  , 
                  avg_fb_release_z , avg_fb_spin, avg_fb_loc_z, avg_fb_hmov,
                  avg_fb_release_x, avg_fb_loc_x, avg_fb_release_ext)
  

  fb_xgb_train_2022 = xgb.DMatrix(data = as.matrix(fb_train_components_2022[,-1]), label = fb_train_components_2022$swStr)

  fb_xgb_test_2022 = xgb.DMatrix(data = as.matrix(fb_test_components_2022[,-1]), label = fb_test_components_2022$swStr)
  
  fb_xgb_total_2022 = xgb.DMatrix(data = as.matrix(fb_full_components_2022[,-1]), label = fb_info_2022$swStr)

  fb_2022_watchlist = list(train=fb_xgb_train_2022, test=fb_xgb_test_2022)
  
  fb_xgb_model = xgb.train(data = fb_xgb_train_2022, booster = "gbtree" , eta = .3, watchlist=fb_2022_watchlist, nrounds = 300)
  
  which.min(fb_xgb_model$evaluation_log$test_rmse)
  
  final_fb_xgb_2022 = xgboost(data = fb_xgb_test_2022, booster = "gbtree" , nrounds = 18, verbose = 0)
  
  predicted_fb_xswstr_2022 = predict(final_fb_xgb_2022, fb_xgb_test_2022)  

  fb_info_xSwStr_2022 <- fb_info_2022 %>%
    ungroup() %>%
    mutate(xswStr = predict(final_fb_xgb_2022, fb_xgb_total_2022)) %>%
    mutate(swStr_differences = round(xswStr - swStr, 3)) %>%
    filter(pitches >= 350) %>%
    dplyr::select(pitcher_name, xswStr,swStr)

  
  sqrt(mean((fb_test_components_2022$swStr - predicted_fb_xswstr_2022)^2))
  
  fb_xgb_rmse <- caret::RMSE(fb_test_components_2022$swStr,  predicted_fb_xswstr_2022)
  
  fb_importance_matrix_2022 <-xgb.importance(colnames(fb_xgb_model), model = final_fb_xgb_2022)  
  
  xgb.plot.importance(fb_importance_matrix_2022[1:9,])
  
  xgb.ggplot.importance(fb_importance_matrix_2022[1:9,], col = "blue",
                      xlab = "XGB Feature Importance", n_clusters = 4) +
    ggtitle("Four-Seam Fastball Feature Importance")

#Sinker
  
  si_train_components_2022 <- si_lm_train %>%
    ungroup() %>%
    dplyr::select(swStr, avg_si_velo , avg_si_vmov  , 
                  avg_si_release_z , avg_si_loc_z, avg_si_hmov, avg_si_spin,
                  avg_si_release_x, avg_si_loc_x, avg_si_release_ext)
  
  si_test_components_2022 <- si_lm_test %>%
    ungroup() %>%
    dplyr::select(swStr, avg_si_velo , avg_si_vmov  , 
                  avg_si_release_z , avg_si_loc_z, avg_si_hmov, avg_si_spin,
                  avg_si_release_x, avg_si_loc_x, avg_si_release_ext)
  
  si_full_components_2022 <- si_info_2022 %>%
    ungroup() %>%
    dplyr::select(swStr, avg_si_velo , avg_si_vmov  , 
                  avg_si_release_z , avg_si_loc_z, avg_si_hmov, avg_si_spin,
                  avg_si_release_x, avg_si_loc_x, avg_si_release_ext)
  
  si_xgb_train_2022 = xgb.DMatrix(data = as.matrix(si_train_components_2022[,-1]), label = si_train_components_2022$swStr)
  si_xgb_test_2022 = xgb.DMatrix(data = as.matrix(si_test_components_2022[,-1]), label = si_test_components_2022$swStr)
  
  si_xgb_total_2022 = xgb.DMatrix(data = as.matrix(si_full_components_2022[,-1]), label = si_info_2022$swStr)
  
  si_2022_watchlist = list(train=si_xgb_train_2022, test=si_xgb_test_2022)
  
  si_xgb_model = xgb.train(data = si_xgb_train_2022, booster = "gbtree" , eta = .3, watchlist=fb_2022_watchlist, nrounds = 300)

  which.min(si_xgb_model$evaluation_log$test_rmse)
  
  final_si_xgb_2022 = xgboost(data = si_xgb_test_2022, booster = "gbtree" , nrounds = 21, verbose = 0)
  
  predicted_si_xswstr_2022 = predict(final_si_xgb_2022, si_xgb_test_2022)
  
  si_info_xSwStr_2022 <- si_info_2022 %>%
    ungroup() %>%
    mutate(xswStr = predict(final_si_xgb_2022, si_xgb_total_2022)) %>%
    mutate(swStr_differences =round(xswStr - swStr, 3)) %>%
    filter(pitches >= 350) %>%
    dplyr::select(pitcher_name, xswStr,swStr)
  
  si_xgb_rmse <- caret::RMSE(si_test_components_2022$swStr,  predicted_si_xswstr_2022)
  

  si_importance_matrix_2022 <-xgb.importance(colnames(si_xgb_model), model = final_si_xgb_2022)  
  
  xgb.plot.importance(si_importance_matrix_2022[1:14,])
  

#Slider

  sl_train_components_2022 <- sl_lm_train %>%
    ungroup() %>%
    dplyr::select(swStr, avg_sl_velo , avg_sl_vmov  , 
                  avg_sl_release_z , avg_sl_loc_z, avg_sl_hmov, avg_sl_spin,
                  avg_sl_release_x, avg_sl_loc_x, avg_sl_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  sl_test_components_2022 <- sl_lm_test %>%
    ungroup() %>%
    dplyr::select(swStr, avg_sl_velo , avg_sl_vmov  , 
                  avg_sl_release_z , avg_sl_loc_z, avg_sl_hmov, avg_sl_spin,
                  avg_sl_release_x, avg_sl_loc_x, avg_sl_release_ext, velo_difference, 
                  hmov_difference, vmov_difference, two_strike_opps)
  
  sl_full_components_2022 <- sl_info_2022 %>%
    ungroup() %>%
    dplyr::select(swStr, avg_sl_velo , avg_sl_vmov  , 
                  avg_sl_release_z , avg_sl_loc_z, avg_sl_hmov, avg_sl_spin,
                  avg_sl_release_x, avg_sl_loc_x, avg_sl_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  sl_xgb_train_2022 = xgb.DMatrix(data = as.matrix(sl_train_components_2022[,-1]), label = sl_train_components_2022$swStr)
  sl_xgb_test_2022 = xgb.DMatrix(data = as.matrix(sl_test_components_2022[,-1]), label = sl_test_components_2022$swStr)
  
  sl_xgb_total_2022 = xgb.DMatrix(data = as.matrix(sl_full_components_2022[,-1]), label = sl_info_2022$swStr)
  
  sl_2022_watchlist = list(train=si_xgb_train_2022, test=si_xgb_test_2022)
  
  sl_xgb_model = xgb.train(data = si_xgb_train_2022, booster = "gbtree" , eta = .3, watchlist=sl_2022_watchlist, nrounds = 300)
  
  which.min(sl_xgb_model$evaluation_log$test_rmse)
  
  #Model to get rmse onto testing set
  final_sl_xgb_2022 = xgboost(data = sl_xgb_test_2022, booster = "gbtree" , nrounds = 14, verbose = 0)
  
  predicted_sl_xswstr_2022 = predict(final_sl_xgb_2022, sl_xgb_test_2022)

  sl_xgb_rmse <- caret::RMSE(sl_test_components_2022$swStr,  predicted_sl_xswstr_2022)
  
  sl_info_xSwStr_2022 <- sl_info_2022 %>%
    ungroup() %>%
    mutate(xswStr = predict(final_sl_xgb_2022, sl_xgb_total_2022)) %>%
    mutate(swStr_differences =round(xswStr - swStr, 3)) %>%
    mutate(lm_xswStr = predict(sl_lm_2022, sl_info_2022)) %>%
    filter(pitches >= 350) %>%
    dplyr::select(pitcher_name, xswStr,swStr)
  
  sl_importance_matrix_2022 <-xgb.importance(colnames(sl_xgb_model), model = final_sl_xgb_2022)  
  
  xgb.plot.importance(sl_importance_matrix_2022[1:15,])

  
#Cutters
  
  fc_train_components_2022 <- fc_lm_train %>%
    ungroup() %>%
    dplyr::select(swStr, avg_fc_velo , avg_fc_vmov  , 
                  avg_fc_release_z, avg_fc_spin , avg_fc_loc_z, avg_fc_hmov,
                  avg_fc_release_x, avg_fc_loc_x, avg_fc_release_ext)
  
  fc_test_components_2022 <- fc_lm_test %>%
    ungroup() %>%
    dplyr::select(swStr, avg_fc_velo , avg_fc_vmov  , 
                  avg_fc_release_z, avg_fc_spin , avg_fc_loc_z, avg_fc_hmov,
                  avg_fc_release_x, avg_fc_loc_x, avg_fc_release_ext)
  
  fc_full_components_2022 <- fc_info_2022 %>%
    ungroup() %>%
    dplyr::select(swStr, avg_fc_velo , avg_fc_vmov  , 
                  avg_fc_release_z, avg_fc_spin , avg_fc_loc_z, avg_fc_hmov,
                  avg_fc_release_x, avg_fc_loc_x, avg_fc_release_ext)
  
  fc_xgb_train_2022 = xgb.DMatrix(data = as.matrix(fc_train_components_2022[,-1]), label = fc_train_components_2022$swStr)
  fc_xgb_test_2022 = xgb.DMatrix(data = as.matrix(fc_test_components_2022[,-1]), label = fc_test_components_2022$swStr)
  
  fc_xgb_total_2022 = xgb.DMatrix(data = as.matrix(fc_full_components_2022[,-1]), label = fc_info_2022$swStr)
  
  fc_2022_watchlist = list(train=fc_xgb_train_2022, test=fc_xgb_test_2022)
  
  fc_xgb_model = xgb.train(data = fc_xgb_train_2022, booster = "gbtree" , eta = .3, watchlist=fc_2022_watchlist, nrounds = 300)
  
  which.min(fc_xgb_model$evaluation_log$test_rmse)
  
  #Model to get rmse onto testing set
  final_fc_xgb_2022 = xgboost(data = fc_xgb_test_2022, booster = "gbtree" , nrounds = 28, verbose = 0)
  
  predicted_fc_xswstr_2022 = predict(final_fc_xgb_2022, fc_xgb_test_2022)
  
  fc_xgb_rmse <- caret::RMSE(fc_test_components_2022$swStr,  predicted_fc_xswstr_2022)
  
  fc_info_xSwStr_2022 <- fc_info_2022 %>%
    ungroup() %>%
    mutate(xswStr = predict(final_fc_xgb_2022, fc_xgb_total_2022)) %>%
    mutate(swStr_differences =round(xswStr - swStr, 3)) %>%
    filter(pitches >= 350) %>%
    dplyr::select(pitcher_name, xswStr,swStr)  
 
  fc_importance_matrix_2022 <-xgb.importance(colnames(fc_xgb_model), model = final_fc_xgb_2022)  
  
  xgb.plot.importance(fc_importance_matrix_2022[1:15,]) 

#Changeups
  
  ch_train_components_2022 <- ch_lm_train %>%
    ungroup() %>%
    dplyr::select(swStr, avg_ch_velo , avg_ch_vmov  , 
                  avg_ch_release_z, avg_ch_spin , avg_ch_loc_z, avg_ch_hmov,
                  avg_ch_release_x, avg_ch_loc_x, avg_ch_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  ch_test_components_2022 <- ch_lm_test %>%
    ungroup() %>%
    dplyr::select(swStr, avg_ch_velo , avg_ch_vmov  , 
                  avg_ch_release_z, avg_ch_spin , avg_ch_loc_z, avg_ch_hmov,
                  avg_ch_release_x, avg_ch_loc_x, avg_ch_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  
  ch_full_components_2022 <- ch_info_2022 %>%
    ungroup() %>%
    dplyr::select(swStr, avg_ch_velo , avg_ch_vmov  , 
                  avg_ch_release_z, avg_ch_spin , avg_ch_loc_z, avg_ch_hmov,
                  avg_ch_release_x, avg_ch_loc_x, avg_ch_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  
  ch_xgb_train_2022 = xgb.DMatrix(data = as.matrix(ch_train_components_2022[,-1]), label = ch_train_components_2022$swStr)
  ch_xgb_test_2022 = xgb.DMatrix(data = as.matrix(ch_test_components_2022[,-1]), label = ch_test_components_2022$swStr)
  
  ch_xgb_total_2022 = xgb.DMatrix(data = as.matrix(ch_full_components_2022[,-1]), label = ch_info_2022$swStr)
  
  ch_2022_watchlist = list(train=ch_xgb_train_2022, test=ch_xgb_test_2022)
  
  ch_xgb_model = xgb.train(data = ch_xgb_train_2022, booster = "gbtree" , eta = .3, watchlist=ch_2022_watchlist, nrounds = 300)
  
  which.min(ch_xgb_model$evaluation_log$test_rmse)
  
  #Model to get rmse onto testing set
  final_ch_xgb_2022 = xgboost(data = ch_xgb_test_2022, booster = "gbtree" , nrounds = 19, verbose = 0)
  
  predicted_ch_xswstr_2022 = predict(final_ch_xgb_2022, ch_xgb_test_2022)
  
  ch_xgb_rmse <- caret::RMSE(ch_test_components_2022$swStr,  predicted_ch_xswstr_2022)
  
  ch_info_xSwStr_2022 <- ch_info_2022 %>%
    ungroup() %>%
    mutate(xswStr = predict(final_ch_xgb_2022, ch_xgb_total_2022)) %>%
    mutate(swStr_differences =round(xswStr - swStr, 3)) %>%
    filter(pitches >= 350) %>%
    dplyr::select(pitcher_name, xswStr,swStr)  
  
  ch_importance_matrix_2022 <-xgb.importance(colnames(ch_xgb_model), model = final_ch_xgb_2022)  
  
  xgb.plot.importance(ch_importance_matrix_2022[1:15,]) 

#Curveball
  
  cu_train_components_2022 <- cu_lm_train %>%
    ungroup() %>%
    dplyr::select(swStr, avg_cu_velo , avg_cu_vmov  , 
                  avg_cu_release_z, avg_cu_spin , avg_cu_loc_z, avg_cu_hmov,
                  avg_cu_release_x, avg_cu_loc_x, avg_cu_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  cu_test_components_2022 <- cu_lm_test %>%
    ungroup() %>%
    dplyr::select(swStr, avg_cu_velo , avg_cu_vmov  , 
                  avg_cu_release_z, avg_cu_spin , avg_cu_loc_z, avg_cu_hmov,
                  avg_cu_release_x, avg_cu_loc_x, avg_cu_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  
  cu_full_components_2022 <- cu_info_2022 %>%
    ungroup() %>%
    dplyr::select(swStr, avg_cu_velo , avg_cu_vmov  , 
                  avg_cu_release_z, avg_cu_spin , avg_cu_loc_z, avg_cu_hmov,
                  avg_cu_release_x, avg_cu_loc_x, avg_cu_release_ext, velo_difference,
                  hmov_difference, vmov_difference, two_strike_opps)
  
  
  cu_xgb_train_2022 = xgb.DMatrix(data = as.matrix(cu_train_components_2022[,-1]), label = cu_train_components_2022$swStr)
  cu_xgb_test_2022 = xgb.DMatrix(data = as.matrix(cu_test_components_2022[,-1]), label = cu_test_components_2022$swStr)
  
  cu_xgb_total_2022 = xgb.DMatrix(data = as.matrix(cu_full_components_2022[,-1]), label = cu_info_2022$swStr)
  
  cu_2022_watchlist = list(train=cu_xgb_train_2022, test=cu_xgb_test_2022)
  
  cu_xgb_model = xgb.train(data = cu_xgb_train_2022, booster = "gbtree" , eta = .3, watchlist=cu_2022_watchlist, nrounds = 300)
  
  which.min(cu_xgb_model$evaluation_log$test_rmse)
  
  final_cu_xgb_2022 = xgboost(data = cu_xgb_test_2022, booster = "gbtree" , nrounds = 19, verbose = 0)
  
  predicted_cu_xswstr_2022 = predict(final_cu_xgb_2022, cu_xgb_test_2022)
  
  cu_xgb_rmse <- caret::RMSE(cu_test_components_2022$swStr,  predicted_cu_xswstr_2022)
  
  cu_info_xSwStr_2022 <- cu_info_2022 %>%
    ungroup() %>%
    mutate(xswStr = predict(final_cu_xgb_2022, cu_xgb_total_2022)) %>%
    mutate(swStr_differences =round(xswStr - swStr, 3)) %>%
    filter(pitches >= 200) %>%
    dplyr::select(pitcher_name, xswStr,swStr)  
  
  cu_importance_matrix_2022 <-xgb.importance(colnames(cu_xgb_model), model = final_cu_xgb_2022)  
  
  xgb.plot.importance(cu_importance_matrix_2022[1:15,]) 

#Splitter
  # 
  # 
  # fs_train_components_2022 <- fs_lm_train %>%
  #   ungroup() %>%
  #   dplyr::select(swStr, avg_fs_velo , avg_fs_vmov  , 
  #                 avg_fs_release_z, avg_fs_spin , avg_fs_loc_z, avg_fs_hmov,
  #                 avg_fs_release_x, avg_fs_loc_x, avg_fs_release_ext, velo_difference,
  #                 hmov_difference, vmov_difference, two_strike_opps)
  # 
  # fs_test_components_2022 <- fs_lm_test %>%
  #   ungroup() %>%
  #   dplyr::select(swStr, avg_fs_velo , avg_fs_vmov  , 
  #                 avg_fs_release_z, avg_fs_spin , avg_fs_loc_z, avg_fs_hmov,
  #                 avg_fs_release_x, avg_fs_loc_x, avg_fs_release_ext, velo_difference,
  #                 hmov_difference, vmov_difference, two_strike_opps)
  # 
  # 
  # fs_full_components_2022 <- fs_info_2022 %>%
  #   ungroup() %>%
  #   dplyr::select(swStr, avg_fs_velo , avg_fs_vmov  , 
  #                 avg_fs_release_z, avg_fs_spin , avg_fs_loc_z, avg_fs_hmov,
  #                 avg_fs_release_x, avg_fs_loc_x, avg_fs_release_ext, velo_difference,
  #                 hmov_difference, vmov_difference, two_strike_opps)
  # 
  # 
  # fs_xgb_train_2022 = xgb.DMatrix(data = as.matrix(fs_train_components_2022[,-1]), label = fs_train_components_2022$swStr)
  # fs_xgb_test_2022 = xgb.DMatrix(data = as.matrix(fs_test_components_2022[,-1]), label = fs_test_components_2022$swStr)
  # 
  # fs_xgb_total_2022 = xgb.DMatrix(data = as.matrix(fs_full_components_2022[,-1]), label = fs_info_2022$swStr)
  # 
  # fs_2022_watchlist = list(train=fs_xgb_train_2022, test=fs_xgb_test_2022)
  # 
  # fs_xgb_model = xgb.train(data = fs_xgb_train_2022, booster = "gbtree" , eta = .3, watchlist=fs_2022_watchlist, nrounds = 300)
  # 
  # which.min(fs_xgb_model$evaluation_log$test_rmse)
  # 
  # final_fs_xgb_2022 = xgboost(data = fs_xgb_test_2022, booster = "gbtree" , nrounds = 16, verbose = 0)
  # 
  # predicted_fs_xswstr_2022 = predict(final_fs_xgb_2022, fs_xgb_test_2022)
  # 
  # fs_xgb_rmse <- caret::RMSE(fs_test_components_2022$swStr,  predicted_fs_xswstr_2022)
  # 
  # fs_info_xSwStr_2022 <- fs_info_2022 %>%
  #   ungroup() %>%
  #   mutate(xswStr = predict(final_fs_xgb_2022, fs_xgb_total_2022)) %>%
  #   mutate(swStr_differences =round(xswStr - swStr, 3)) %>%
  #   dplyr::select(pitcher_name,xswStr,swStr)  
  # 
  # fs_importance_matrix_2022 <-xgb.importance(colnames(fs_xgb_model), model = final_fs_xgb_2022)  
  # 
  # xgb.plot.importance(fs_importance_matrix_2022[1:13,]) 
  # 
  
  xgb_RMSE_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(xgb_RMSE_df) <-c("Pitch Types", "RMSE")
  
  pitch_rmse_list <- list(fb_xgb_rmse, si_xgb_rmse, sl_xgb_rmse, fc_xgb_rmse, ch_xgb_rmse, cu_xgb_rmse)
  
  xgb_RMSE_df[1,] <- c("Four-seam Fastball", fb_xgb_rmse)
  xgb_RMSE_df[2,] <- c("Sinker", si_xgb_rmse)
  xgb_RMSE_df[3,] <- c("Slider", sl_xgb_rmse)
  xgb_RMSE_df[4,] <- c("Cutter", fc_xgb_rmse)
  xgb_RMSE_df[5,] <- c("Change-up", ch_xgb_rmse)
  xgb_RMSE_df[6,] <- c("Curve-ball", cu_xgb_rmse)
  
  ############################################################################################################################  
  
  #Stepwise regression to find better variables.
  
  #################
  #4-Seam Fastballs
  #################
  
  fb_train.control <- trainControl(method = "cv", number = 10)
  
  fb_step.model <- train(swStr ~ (avg_fb_velo + avg_fb_hmov + avg_fb_vmov + avg_fb_release_x
                                  + avg_fb_release_z + avg_fb_release_ext + avg_fb_spin +
                                    avg_fb_loc_x + avg_fb_loc_z + avg_fb_spin_axis)^2,
                         data = fb_lm_train, method = "leapBackward", 
                         tuneGrid = data.frame(nvmax = 1:10), trControl = fb_train.control)
  
  fb_step.model$results
  
  fb_step.model$bestTune
  
  coef(fb_step.model$finalModel, 4)
  
  fb_interactions_lm_2022 <- lm(swStr ~ avg_fb_vmov + avg_fb_velo:avg_fb_vmov + 
                                avg_fb_vmov:avg_fb_loc_z + avg_fb_vmov:avg_fb_release_z,
                                data = fb_lm_test)
  plot(fb_interactions_lm_2022)
  
  predicted_interaction_lm_fb_xswstr_2022 = predict(fb_interactions_lm_2022, fb_lm_test)
  
  caret::RMSE(fb_lm_test$swStr, predicted_interaction_lm_fb_xswstr_2022)
  
  ############################################################################################################################  
  
  #Graphing fb interactions and xswstr
  
  fb_info_new_lm_2022 <- fb_info_2022 %>%
    ungroup() %>%
    mutate(lm_swstr = predict(fb_interactions_lm_2022, fb_info_2022)) %>%
    dplyr::select(pitcher_name,avg_fb_hmov, lm_swstr) 

  fb_graph_xSwStr_2022 <- fb_info_xSwStr_2022 %>%
    dplyr::select(pitcher_name, xswStr)
  
  fb_graph_lmswStr_2022 <- fb_info_new_lm_2022 %>%
    dplyr::select(pitcher_name, lm_swstr)
  
  fb_graph_both_swstr_2022 <- fb_graph_xSwStr_2022 %>%
    left_join(fb_graph_lmswStr_2022, by = "pitcher_name")

  graph_fb_models <- ggplot(fb_graph_both_swstr_2022, aes(lm_swstr, xswStr)) + 
    geom_point() + 
    geom_smooth() +
    geom_text_repel(data = filter(fb_graph_both_swstr_2022, (lm_swstr < 0.025) | 
                                    (xswStr > .15) | (lm_swstr > .13) | (lm_swstr > .12 & xswStr < .1)),
                    aes(lm_swstr, xswStr, label = pitcher_name)) + 
    scale_x_continuous("Interpretive SwStr", limits = c(.05,.135)) + scale_y_continuous("XGBoost SwStr", limits = c(.05,.175))
  
  cor(fb_graph_both_swstr_2022$xswStr, fb_graph_both_swstr_2022$lm_swstr, method = c("pearson"))
  

  
  
  
  
  
  
  