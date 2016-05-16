echo Start Tuning Step 1
call RScript C:\RStudioWorkspace\BetLab\scripts\execModelTuning.R tune1.properties
echo Finished Tuning Step 1
echo Start Tuning Step 2
call RScript C:\RStudioWorkspace\BetLab\scripts\execModelTuning.R tune2.properties
echo Finished Tuning Step 2