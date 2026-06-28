# New Experiment Results Report - 2026-06-11

Discovery roots: `/srv/cluster/ga_project/results/GA_REGIME_FIRST_MULTI_SEED_20260606_125040/GA_REGIME_FIRST_20260606_125040` and `/srv/cluster/ga_project/results/GA_REGIME_FIRST_MULTI_SEED_20260606_125040/GA_REGIME_FIRST_20260609_005515`.

## Executive Summary
- Final regimes evaluated: 36
- GA wins: 12
- Benchmark wins: 24
- Strongest recurring families: invgauss, exwald, weibull.
- Benchmark-dominant family: exgaussian.
- Legacy external priors entered lognormal, but final winners were not direct external-prior survivals.

## Family Summary Across Seeds
     family ga_wins benchmark_wins ga_rel_improvement_q95
   invgauss       4              2             0.38200457
     exwald       3              3             0.20615627
  lognormal       2              4             0.03856476
    weibull       2              4             0.23613401
     normal       1              5             0.06992317
 exgaussian       0              6             0.00397897
 ga_rel_improvement_mean
             0.522122789
             0.247784155
             0.167639172
             0.290603342
             0.016772973
            -0.003866169

## GA Winner Candidates For Post-Discovery Fixed-Weight Validation
    seed    family specialist_regime_id ga_rel_improvement_q95
 seed101    exwald                REG03             0.11489435
 seed101    exwald                REG01             0.10444779
 seed202    exwald                REG03             0.20615627
 seed101  invgauss                REG01             0.38200457
 seed101  invgauss                REG02             0.16943619
 seed202  invgauss                REG03             0.08337598
 seed202  invgauss                REG02             0.05321284
 seed101 lognormal                REG01             0.03856476
 seed101 lognormal                REG03             0.03821516
 seed202    normal                REG02             0.06992317
 seed101   weibull                REG01             0.07319687
 seed202   weibull                REG02             0.23613401
 ga_rel_improvement_mean           global_audit_interpretation
              0.23729604 regime_specific_profile_no_free_lunch
              0.04690451 regime_specific_profile_no_free_lunch
              0.24778416 regime_specific_profile_no_free_lunch
              0.52212279  profile_matched_generalization_bonus
              0.32153965  profile_matched_generalization_bonus
              0.17589334 regime_specific_profile_no_free_lunch
              0.17531957 regime_specific_profile_no_free_lunch
              0.16763917 regime_specific_profile_no_free_lunch
              0.13649133 regime_specific_profile_no_free_lunch
              0.01677297 regime_specific_profile_no_free_lunch
              0.08296707 regime_specific_profile_no_free_lunch
              0.29060334 regime_specific_profile_no_free_lunch
                                                                                                           regime_key
        exwald | first_passage_right_skewed | heavy_first_passage_tail | upper_tail | clustered | extreme | low_scale
 exwald | first_passage_right_skewed | heavy_first_passage_tail | bimodal | bimodal_mixture | extreme | extreme_scale
  exwald | first_passage_right_skewed | heavy_first_passage_tail | point_mass | point_mass | moderate | extreme_scale
          invgauss | right_skewed_duration | heavy_right_tail | upper_tail | tail_outlier | moderate | mid_high_scale
              invgauss | right_skewed_duration | heavy_right_tail | upper_tail | clustered | moderate | extreme_scale
                   invgauss | right_skewed_duration | heavy_right_tail | upper_tail | tail_outlier | high | low_scale
            invgauss | right_skewed_duration | heavy_right_tail | point_mass | point_mass | moderate | mid_high_scale
                           lognormal | right_skewed | heavy_right_tail | upper_tail | clustered | extreme | low_scale
                         lognormal | right_skewed | heavy_right_tail | upper_tail | clustered | high | mid_high_scale
                                 normal | symmetric | light_tail | symmetric | heavy_tailed_t | high | mid_high_scale
         weibull | right_skewed_flexible_shape | flexible_tail | upper_tail | tail_outlier | moderate | extreme_scale
           weibull | right_skewed_flexible_shape | flexible_tail | point_mass | point_mass | moderate | extreme_scale

## Interpretation
The second discovery stage supports a regime-specialist / No Free Lunch narrative: GA candidates win in selected regimes, while benchmark dominance remains in other regimes and families. Post-Discovery Fixed-Weight Validation should now evaluate fixed discovered weights without rerunning GA search.
