simulation_plus_analysis <- function(
  mean_sham_described_hypnosis_embedded,
  mean_sham_described_hypnosis_whitenoise,
  mean_true_described_hypnosis_relaxation,
  mean_true_described_hypnosis_confusion,
  
  mean_sham_described_control_embedded,
  mean_sham_described_control_whitenoise,
  mean_true_described_control_relaxation,
  mean_true_described_control_confusion,
  
  sd,
  
  r_described_hypnosiss,
  r_described_controls,
  r_hypnosiss_with_controls,
  
  total_N,
  
  rscale
){
  data = 
    simulate_data(
      mean_sham_described_hypnosis_embedded,
      mean_sham_described_hypnosis_whitenoise,
      mean_true_described_hypnosis_relaxation,
      mean_true_described_hypnosis_confusion,
      
      mean_sham_described_control_embedded,
      mean_sham_described_control_whitenoise,
      mean_true_described_control_relaxation,
      mean_true_described_control_confusion,
      
      sd,
      
      r_described_hypnosiss,
      r_described_controls,
      r_hypnosiss_with_controls,
      
      total_N
    )
  
  bf = analysis_compiler(data = data, rscale = rscale)
  
  return(bf)
  
  
  iterations = 100
  
  bfs = replicate(n = iterations, simulation_plus_analysis(
    mean_sham_described_hypnosis_embedded = 4.8,
    mean_sham_described_hypnosis_whitenoise = 4.8,
    mean_true_described_hypnosis_relaxation = 4.8,
    mean_true_described_hypnosis_confusion = 4.8,
    
    mean_sham_described_control_embedded = 2.3,
    mean_sham_described_control_whitenoise = 2.3,
    mean_true_described_control_relaxation = 2.3,
    mean_true_described_control_confusion = 2.3,
    
    sd = 2.35,
    
    r_described_hypnosiss = 0.63,
    r_described_controls = 0.63,
    r_hypnosiss_with_controls = 0,
    
    total_N = 13*4,
    
  ))
}