# equiv_mean_extremum produces expected errors and warnings

    Code
      equiv_mean_extremum(alpha = 0.05, n_sample = 9, data_qual = runif(28),
      mean_qual = 9.24, sd_qual = 0.162)
    Warning <simpleWarning>
      Both data_qual and mean_qual were supplied. mean_qual ignored.
      Both data_qual and sd_qual were supplied. sd_qual ignored.
    Output
      
      Call:
      equiv_mean_extremum(data_qual = runif(28), mean_qual = 9.24, 
          sd_qual = 0.162, n_sample = 9, alpha = 0.05)
      
      For alpha = 0.05 and n = 9 
      ( k1 = 2.741054 and k2 = 0.6410852 )
                        Min Individual    Sample Mean    
           Thresholds:    -0.1330291       0.3528538     

---

    Code
      equiv_mean_extremum(alpha = 0.05, n_sample = 9, data_qual = runif(28),
      mean_qual = 9.24, sd_qual = 0.162)
    Warning <simpleWarning>
      Both data_qual and mean_qual were supplied. mean_qual ignored.
      Both data_qual and sd_qual were supplied. sd_qual ignored.
    Output
      
      Call:
      equiv_mean_extremum(data_qual = runif(28), mean_qual = 9.24, 
          sd_qual = 0.162, n_sample = 9, alpha = 0.05)
      
      For alpha = 0.05 and n = 9 
      ( k1 = 2.741054 and k2 = 0.6410852 )
                        Min Individual    Sample Mean    
           Thresholds:    -0.2267868       0.3552348     

# equiv_change_mean produces expected errors and warnings

    Code
      equiv_change_mean(alpha = 0.05, data_sample = runif(9), n_sample = 9,
      mean_sample = 9.02, sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
      sd_qual = 0.162)
    Warning <simpleWarning>
      Both data_sample and n_sample supplied. n_sample ignored.
      Both data_sample and mean_sample supplied. mean_sample ignored
      Both data_sample and sd_sample supplied. sd_sample ignored
    Output
      
      Call:
      equiv_change_mean(n_qual = 28, mean_qual = 9.24, sd_qual = 0.162, 
          data_sample = runif(9), n_sample = 9, mean_sample = 9.02, 
          sd_sample = 0.15785, alpha = 0.05)
      
      For alpha = 0.05 
                        Qualification        Sample      
                Number        28               9         
                  Mean       9.24          0.4010531     
                    SD      0.162           0.295591     
                Result               FAIL               
         Passing Range       9.084001 to 9.395999       

---

    Code
      equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02, sd_sample = 0.15785,
        data_qual = runif(28), n_qual = 28, mean_qual = 9.24, sd_qual = 0.162)
    Warning <simpleWarning>
      Both data_qual and n_qual supplied. n_qual ignored.
      Both data_qual and mean_qual supplied. mean_qual ignored
      Both data_qual and sd_qual supplied. sd_qual ignored
    Output
      
      Call:
      equiv_change_mean(data_qual = runif(28), n_qual = 28, mean_qual = 9.24, 
          sd_qual = 0.162, n_sample = 9, mean_sample = 9.02, sd_sample = 0.15785, 
          alpha = 0.05)
      
      For alpha = 0.05 
                        Qualification        Sample      
                Number        28               9         
                  Mean    0.5073368           9.02       
                    SD    0.3088989         0.15785      
                Result               FAIL               
         Passing Range      0.288275 to 0.7263987       

---

    Code
      equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02, sd_sample = 0.15785,
        data_qual = runif(28), n_qual = 28, mean_qual = 9.24, sd_qual = 0.162)
    Warning <simpleWarning>
      Both data_qual and n_qual supplied. n_qual ignored.
      Both data_qual and mean_qual supplied. mean_qual ignored
      Both data_qual and sd_qual supplied. sd_qual ignored
    Output
      
      Call:
      equiv_change_mean(data_qual = runif(28), n_qual = 28, mean_qual = 9.24, 
          sd_qual = 0.162, n_sample = 9, mean_sample = 9.02, sd_sample = 0.15785, 
          alpha = 0.05)
      
      For alpha = 0.05 
                        Qualification        Sample      
                Number        28               9         
                  Mean    0.5596046           9.02       
                    SD     0.30128          0.15785      
                Result               FAIL               
         Passing Range      0.3455532 to 0.7736559      

