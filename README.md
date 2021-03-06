End product columns:
[1] "Activity"                                              
 [2] "Subject"                                               
 [3] "Time: Body Acceleration, Mean X Axis"                  
 [4] "Time: Body Acceleration, Mean Y Axis"                  
 [5] "Time: Body Acceleration, Mean Z Axis"                  
 [6] "Time: Gravity Acceleration, Mean X Axis"               
 [7] "Time: Gravity Acceleration, Mean Y Axis"               
 [8] "Time: Gravity Acceleration, Mean Z Axis"               
 [9] "Time: Body Acceleration Jerk, Mean X Axis"             
[10] "Time: Body Acceleration Jerk, Mean Y Axis"             
[11] "Time: Body Acceleration Jerk, Mean Z Axis"             
[12] "Time: Gyro, Mean X Axis"                               
[13] "Time: Gyro, Mean Y Axis"                               
[14] "Time: Gyro, Mean Z Axis"                               
[15] "Time: Gyro Jerk, Mean X Axis"                          
[16] "Time: Gyro Jerk, Mean Y Axis"                          
[17] "Time: Gyro Jerk, Mean Z Axis"                          
[18] "Time: Body Acceleration Magnitude, Mean"               
[19] "Time: Gravity Acceleration Magnitude, Mean"            
[20] "Time: Body Acceleration Jerk Magnitude, Mean"          
[21] "Time: Gyro Magnitude, Mean"                            
[22] "Time: Gyro Jerk Magnitude, Mean"                       
[23] "Freq: Body Acceleration, Mean X Axis"                  
[24] "Freq: Body Acceleration, Mean Y Axis"                  
[25] "Freq: Body Acceleration, Mean Z Axis"                  
[26] "Freq: Body Acceleration, Mean Freq X Axis"             
[27] "Freq: Body Acceleration, Mean Freq Y Axis"             
[28] "Freq: Body Acceleration, Mean Freq Z Axis"             
[29] "Freq: Body Acceleration Jerk, Mean X Axis"             
[30] "Freq: Body Acceleration Jerk, Mean Y Axis"             
[31] "Freq: Body Acceleration Jerk, Mean Z Axis"             
[32] "Freq: Body Acceleration Jerk, Mean Freq X Axis"        
[33] "Freq: Body Acceleration Jerk, Mean Freq Y Axis"        
[34] "Freq: Body Acceleration Jerk, Mean Freq Z Axis"        
[35] "Freq: Gyro, Mean X Axis"                               
[36] "Freq: Gyro, Mean Y Axis"                               
[37] "Freq: Gyro, Mean Z Axis"                               
[38] "Freq: Gyro, Mean Freq X Axis"                          
[39] "Freq: Gyro, Mean Freq Y Axis"                          
[40] "Freq: Gyro, Mean Freq Z Axis"                          
[41] "Freq: Body Acceleration Magnitude, Mean"               
[42] "Freq: Body Acceleration Magnitude, Mean Freq"          
[43] "Freq: Body/Body Acceleration Jerk Magnitude, Mean"     
[44] "Freq: Body/Body Acceleration Jerk Magnitude, Mean Freq"
[45] "Freq: Body/Gyro Magnitude, Mean"                       
[46] "Freq: Body/Gyro Magnitude, Mean Freq"                  
[47] "Freq: Body/Gyro Jerk Magnitude, Mean"                  
[48] "Freq: Body/Gyro Jerk Magnitude, Mean Freq"             
[49] "Angle: Time Body Acceleration Mean, Gravity "          
[50] "Angle: Time Body Acceleration JerkMean, Gravity Mean"  
[51] "Angle: Time Gyro Mean, Gravity Mean"                   
[52] "Angle: Time Gyro JerkMean, Gravity Mean"               
[53] "Angle: X Axis, Gravity Mean"                           
[54] "Angle: Y Axis, Gravity Mean"                           
[55] "Angle: Z Axis, Gravity Mean"                           
[56] "Time: Body Acceleration, Std Dev X Axis"               
[57] "Time: Body Acceleration, Std Dev Y Axis"               
[58] "Time: Body Acceleration, Std Dev Z Axis"               
[59] "Time: Gravity Acceleration, Std Dev X Axis"            
[60] "Time: Gravity Acceleration, Std Dev Y Axis"            
[61] "Time: Gravity Acceleration, Std Dev Z Axis"            
[62] "Time: Body Acceleration Jerk, Std Dev X Axis"          
[63] "Time: Body Acceleration Jerk, Std Dev Y Axis"          
[64] "Time: Body Acceleration Jerk, Std Dev Z Axis"          
[65] "Time: Gyro, Std Dev X Axis"                            
[66] "Time: Gyro, Std Dev Y Axis"                            
[67] "Time: Gyro, Std Dev Z Axis"                            
[68] "Time: Gyro Jerk, Std Dev X Axis"                       
[69] "Time: Gyro Jerk, Std Dev Y Axis"                       
[70] "Time: Gyro Jerk, Std Dev Z Axis"                       
[71] "Time: Body Acceleration Magnitude, Std Dev "           
[72] "Time: Gravity Acceleration Magnitude, Std Dev "        
[73] "Time: Body Acceleration Jerk Magnitude, Std Dev "      
[74] "Time: Gyro Magnitude, Std Dev "                        
[75] "Time: Gyro Jerk Magnitude, Std Dev "                   
[76] "Freq: Body Acceleration, Std Dev X Axis"               
[77] "Freq: Body Acceleration, Std Dev Y Axis"               
[78] "Freq: Body Acceleration, Std Dev Z Axis"               
[79] "Freq: Body Acceleration Jerk, Std Dev X Axis"          
[80] "Freq: Body Acceleration Jerk, Std Dev Y Axis"          
[81] "Freq: Body Acceleration Jerk, Std Dev Z Axis"          
[82] "Freq: Gyro, Std Dev X Axis"                            
[83] "Freq: Gyro, Std Dev Y Axis"                            
[84] "Freq: Gyro, Std Dev Z Axis"                            
[85] "Freq: Body Acceleration Magnitude, Std Dev "           
[86] "Freq: Body/Body Acceleration Jerk Magnitude, Std Dev " 
[87] "Freq: Body/Gyro Magnitude, Std Dev "                   
[88] "Freq: Body/Gyro Jerk Magnitude, Std Dev "  

Summary of transformation:

1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement.
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive variable names. 
5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Processing reads in all data files, combines in a dataframe adding subject and activity (text derived from activity code).

Select the mean and std columns.

Clean up the labelling to make it human readable.

Summarize into a tidy data set based on group_by for activity and sujbect.

Full description of raw data at:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
