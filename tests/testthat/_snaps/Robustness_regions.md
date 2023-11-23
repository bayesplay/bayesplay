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
      
       2 parameters of the alternative prior were
       varied for the robutness analysis:
        The mean was varied from -2 to 2 (step size: 0.5)
        The sd was varied from 0 to 2 (step size: 0.5)
      
      Outcome
      -----------------
      27 of 45 (0.6) checked priors were consistent
      with the orignal conclusision.
      (drew the same conclusion).
      
      3 of 45 (0.07) checked priors were inconsistent
      with the orignal conclusision.
      (drew the opposite conclusion).
      
      15 of 45 (0.33) checked priors were inconclusive.
      (did not find support for H1 or H0).
      
       

