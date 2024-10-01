#Example usage of the green detection function

#Note: the following files need to be in the working directory:
        #audio files: 951321.wav, 965118.wav and 964935.wav inside the folder "Audios_Usage_Example"
        #script: functions_calls_recogn_Aug2024.R
        #MLA: xgbst_green_toad_calls_maxdepth100_nrounds100_Final

#load the functions needed 
source("functions_calls_recogn_Aug2024.R")

#list all wav files in the folder "Audios_Usage_Example"
wav_files<-list.files(path="Audios_Usage_Example", pattern = ".wav",full.names = T)

#use the green toad detection algorithm on all three files and save graphs in the working directory 
results_<-green_detection(wav_files,only_text=F)

#results are 
#File 1 of 3 Audios_Usage_Example/951321.wav|Green toads detected"
#File 2 of 3 Audios_Usage_Example/964935.wav|No green toads detected"
#File 3 of 3 Audios_Usage_Example/965118.wav|Non-definitive result; double check
        
