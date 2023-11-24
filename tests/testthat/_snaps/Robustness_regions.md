# Robustness regions display

    Code
      print(rr)
    Output
      Prior robustness analysis:
      
        Alternative prior
        -----------------
          Family
            normal
          Parameters
            mean: 0
            sd: 0.25
            range: 0 to Inf
      
        Null prior
        -----------------
          Family
            point
          Parameters
            point: 0
      
        Likelihood
        -----------------
          Family
            student_t
          Parameters
            mean: 0.3526918
            sd: 0.121580130304388
            df: 49
      
       The original parameters gave a Bayes factor of 20.56.
      Using the cutoff value of 6, this result provides evidence for H1.
      
      2 parameters of the alternative prior were varied for the robutness analysis:
        The mean was varied from -2 to 2 (step size: 0.5)
        The sd was varied from 0 to 2 (step size: 0.5)
      
      Outcome
      -----------------
      3 of 45 (0.07) tested priors provided evidence for H0 (BF < 0.17)
      
      27 of 45 (0.6) tested priors provided evidence for H1 (BF > 6)
      
      15 of 45 (0.33) tested priors were inconclusive (0.17 < BF < 6)
      
       

