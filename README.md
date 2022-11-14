# ClimateVelocity_shared
Climatic velocity codes for collaborating.  

1. In the "TwoLayerResist_FVoCC_TestForFragmented.R", I split the whole spatial data frame into different different batches and each time I put one batch to run, and then merge the output from all the batches, but the downside is everytime you submit, you have to change the batch number manually by yourself.  
2. The "TwoLayerResistance_FVoCC_withoutBatches_HPC.R" is the version without splitting the whole data frame into batches, and is also converted to a version that can work on HPC.  
3. Under the "Data" folder, there are some sample data I cropped for testing the R scripts.  
4. Some paper for climate change velocity: https://www.nature.com/articles/ncomms12349  http://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.16072  https://onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13295  
