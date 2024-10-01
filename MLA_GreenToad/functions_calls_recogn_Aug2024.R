#the source file for the self-written functions used 

# Function to convert MP3 to WAV
convert_mp3_to_wav <- function(mp3_file, destination_folder) {
  require("av")
  destination_wav <- file.path(destination_folder, paste0(tools::file_path_sans_ext(basename(mp3_file)), ".wav"))
  av_audio_convert(mp3_file, destination_wav)
}


# Function to resample a Wave object to the target sampling rate - helper function for green_detection
resample_wave <- function(wave, target_sampling_rate) {
  if (wave@samp.rate==target_sampling_rate) {return(wave) } else
  {resampled_wave <- resamp(wave, f = wave@samp.rate,g = target_sampling_rate,  output = "Wave")
  return(resampled_wave)}
}


#rearranging - helper function for green_detection
rearranging_both <-  function(large_matrix, offset){
  
  large_matrix_list <- list()  # this is the file that needs to be filled in in the loop below 
  
  offset=offset # how many increments we need before and after each of the timepoints - needs to be samller than nrow(large_matrix)
  
  #large_matrix is from a spectrogramm where frequency is on the y, but here I need frequency on the x and time on the y
  # transpose the matrix
  large_matrix<-t(large_matrix)
  
  for (i in 1:offset) {
    ncol_data <- ncol(large_matrix)
    nrow_data <- nrow(large_matrix)
    
    spacing<-matrix(NA, nrow = i, ncol = ncol_data)
    
    large_matrix_temp1 <- rbind(spacing,large_matrix)[1:nrow_data,]
    large_matrix_temp2 <- rbind(large_matrix,spacing)[(i+1):(nrow_data+i),]
    
    large_matrix_list[[i]] <- data.frame(large_matrix_temp1)
    large_matrix_list[[i+offset]]<- data.frame(large_matrix_temp2)
  }
  
  large_data_frame<-data.frame(large_matrix,do.call(cbind,large_matrix_list))
  # Scale each row separately
  large_data_frame_scaled<- t(apply(large_data_frame, 1, scale))
  
  large_data_frame_both<-na.omit(data.frame(large_data_frame,large_data_frame_scaled))
  
  return(large_data_frame_both)
}


#### the function below may need some more work
#the file extension in ouput changes the type of file. Need to supported by the av package. 

green_detection <- function(file_dir,
                            save_movie=F,only_text=T,
                            xgboostdir="xgbst_green_toad_calls_maxdepth100_nrounds100_Final") {
  require(seewave)
  require(tidyverse)
  require(tuneR)
  require(ggplot2)
  require(xgboost)
  require(av)
  
  # Set-up the result list to be filled with the evaluation results
  TSD_list<-list()
  TSD<-vector()
  
  for (i in 1:length(file_dir)) {
    
    plotdir=paste0(tools::file_path_sans_ext(basename(file_dir[i])), "_output.pdf")
    output_dir=paste0(tools::file_path_sans_ext(basename(file_dir[i])), "_output.mp4")
    
    # Load the MLA; must be in the working directory 
    xgbst<-xgb.load(xgboostdir)
    
    # Read in wave file 
    wave_file<-readWave(file_dir[i])
    
    # Extract sampling frequency 
    samp.rate1<-wave_file@samp.rate
    
    #######################
    # Resample, filter and then rearrange the file in the same way as the training data set 
    
    wave_file<-resample_wave(wave_file,44100)
    
    samp.rate<-wave_file@samp.rate
    
    wave_file_filtered<-seewave::ffilter(wave_file, from = 1150, to = 1650) # Filter frequencies, set in the example at  1150-1650
    
    # Make quick spectrogram, without plotting it
    spectro_<-seewave::spectro(wave_file_filtered, f = samp.rate, flim = c(0.8,2),fastdisp = T,plot=F)
    
    
    matrix_MLA_result<-spectro_[["amp"]]
    colnames(matrix_MLA_result) <- spectro_[["time"]]
    rownames(matrix_MLA_result) <- spectro_[["freq"]]
    
    wave_file_rearranged<-rearranging_both(matrix_MLA_result,offset=18)
    
    # Run the predictions
    
    # Remove NAs
    wave_file_rearranged<-na.omit(wave_file_rearranged)
    
    if (nrow(wave_file_rearranged)==0) {TSD[i]<-sprintf("%s", paste(file_dir[i], "No sound detected", sep="|")); print(TSD[i]);TSD_list[[i]]<-TSD[i]} else {
      
      x<-as.numeric(rownames(wave_file_rearranged)) #saving the rownumbers -> needed for plotting 
      
      wave_file_rearranged <- xgb.DMatrix(data = as.matrix(wave_file_rearranged), label = rep(1,nrow(wave_file_rearranged))) 
      
      pwave_file <- predict(xgbst, wave_file_rearranged) 
      
      pwave_file <-factor(ifelse(as.numeric(pwave_file > 0.5)==1,"Green toad call","other"),levels=c("Green toad call","other")) 
      
      pwave_file_df <- as.data.frame(pwave_file)
      
      # Assign a unique color to each level
      colors <- c( "green4","darkgrey")  # Change or expand colors as needed
      
      # Map factor levels to colors
      y_colors <- colors[as.numeric(pwave_file_df$pwave_file)]
      
      y<-relevel(pwave_file_df$pwave_file,ref="other")
      
      timesr<-ifelse(max(x)<60*2,"Time (s)","Time (min)")
      fact<-ifelse(max(x)<60*2,1,1/60)
      
      df_<-data.frame(x*fact,y)
      other=0.9;greent=1.7
      df_$y<-ifelse(df_$y=="other",other,greent)
      df_$y_colors<-ifelse(df_$y==other,NA,"green4") # Removes the "other" 
      
      # Add the length for animations 
      by__<-mean((df_ %>%mutate(Diff = x...fact - lag(x...fact)))$Diff,na.rm = T)  # Average of times between values 
      length_<-seq(0,max(df_$x...fact),by=by__) # Creates time axis 
      
      # Remove the "others" 
      df_<-na.omit(df_)
      
      suppressWarnings(
        suppressMessages(
          p <-ggspectro(wave_file_filtered, f=samp.rate , ovlp=50) + ylim(0.8,1.8)+
            stat_contour(geom="polygon", aes(fill=..level..), bins=30)+
            scale_fill_gradientn(name="Amplitude\n(dB)\n", limits=c(-60,0),
                                 na.value="transparent", colours = spectro.colors(60))+
            theme_minimal()+ theme(legend.position="none")+
            geom_point(data = df_, alpha = 5/10,
                       color=df_$y_colors, shape=18,size=5,
                       mapping = aes(x = x...fact, y = y+0.04, z=rep(1,nrow(df_))))))
      if (only_text==F) {suppressMessages(suppressWarnings(ggsave(plotdir,p)))}
      
      if (save_movie == T) {
        
        ##### Function to make a video 
        make_video <- function(){
          for (i in length_) {
            
            df_2<-df_[df_$x...fact<=i,] # Select the points at times smaller than the vline
            df_3<- data.frame(i,max(length_),-Inf,Inf);colnames(df_3)<-colnames(df_3) <- c("xmin", "xmax", "ymin", "ymax")
            
            
            suppressWarnings(
              suppressMessages(
                p <-ggspectro(wave_file_filtered, f=samp.rate , ovlp=50) + ylim(0.8,1.8)+
                  stat_contour(geom="polygon", aes(fill=..level..), bins=30)+
                  scale_fill_gradientn(name="Amplitude\n(dB)\n", limits=c(-60,0),
                                       na.value="transparent", colours = spectro.colors(60))+
                  theme_minimal()+ theme(legend.position="none")+
                  geom_point(data = df_2, alpha = 5/10,
                             color=df_2$y_colors, shape=18,size=5,
                             mapping = aes(x = x...fact, y = y+0.04, z=rep(1,nrow(df_2))))+
                  ggplot2::geom_rect(data = df_3,inherit.aes = F,color = NA,
                                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,color = 'grey', alpha=0.2)) ))
            suppressWarnings(suppressMessages(print(p)))
            
          }}
        
        # Make the movie 
        framrate_<-(length(length_))/max(length_)
        
        av_capture_graphics(
          make_video(),
          output = output_dir,
          width = 720,
          height = 480,
          framerate = framrate_,
          audio = file_dir) # 
      } # End of movie production
      
      rle_result <- rle(as.character(pwave_file_df$pwave_file)) #Compute the lengths and values of runs of equal values in a vector 
      
      occ_counts <- data.frame(
        factor_level = rle_result$values,
        count = rle_result$lengths)
      
      occ_counts_targetsp_frames <- occ_counts[occ_counts$factor_level=="Green toad call",]
      max_count<-sort(occ_counts_targetsp_frames$count, decreasing=T,na.last = TRUE)[1]
      if (is.na(max_count)){max_count<-0 }
      
      if (max_count==length(pwave_file_df$pwave_file)) {TSD[i]<- sprintf("%s", paste(file_dir[i], "All time points identified as green toads, check potential false positives!", sep="|"))} else{
        
      if (max_count<2) {TSD[i]<- sprintf("%s", paste(file_dir[i], "No green toads detected", sep="|"))} else {
        
        # If more than 10 consecutive occurrences -> then we consider this a positive match
        TSD[i]<-ifelse(max(occ_counts_targetsp_frames$count)>=10,
                       sprintf("%s", paste(file_dir[i], "Green toads detected", sep="|")),
                       sprintf("%s", paste(file_dir[i], "Non-definitive result; double check", sep="|")))}}
       
        time_points_TS<-df_$x...fact
        TSD_list[[i]]<-list(TSD[i],time_points_TS,xgboostdir) # If sound detected 
      print(paste("File", i,"of", length(file_dir), TSD[i], sep=" "))}}
  
  if(only_text==T) {return(TSD) } else {return(TSD_list)}}

