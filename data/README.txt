AE_list.pdf is the list of quadruplets used in the experiment
AsPredicted#9637.pdf is the preregistration of the experiment

Reproducing the results:

1) step1_Create_decoy_target.R
input: target_decoy_input.RData
Inputs: the chosen 806 target-decoy pairs based on the semantic analysis and the genre info of the movies. Based on the genre information, it creates a further 2,271 target-decoy pairs. Output is the unique chosen 3,011 target-decoy pairs from semantic and genre criteria
Outputs: unique decoy pairs
output: step1_Create_decoy_target_output.RData

2) step2_Create_all_quadruplets.R
input: step2_Create_all_quadruplets_input.RData
Inputs: the mean similarity ratings for the 3,011 target-decoy candidates, genre-info and a list of the titles of the movies. Selects 253 target-decoy pairs with a similarity rating above 4.5 and pairs up the target-decoy pairs that do not share any genre category (two target-decoy pairs that do not share any genre make up a quadruplet). Output is the list of all possible quadruplets (of which there are 20,022).
Outputs: the list of all possible quadruplets
output: step2_Create_all_quadruplets_output.RData

3) step3_Create_bespoke_quadruplets.R
input: step3_Create_bespoke_quadruplets_input.RData
Inputs: the 20,022 quadruplets, and the preference ratings for all participants (of which there were 297). Based on the ratings, it selects all possible quadruplets for each participant. Keeps only participants for whom we could create at least 3 quadruplets. Output is the list of bespoke quadruplets.
Outputs: list of bespoke quadruplets for each participant
output: step3_Create_bespoke_quadruplets_output.RData

4) step4_Prepare_choice_data.R
input: step4_Prepare_choice_data_input.RData
Inputs: the list of bespoke quadruplets, choice results and similarity ratings. Excludes subjects filtered by the exclusion criteria. Creates a final dataset with all choice trials, chosen movie, preference and similarity ratings.
output:  step4_Prepare_choice_data_output.RData

5) step5_AE_results.R
input: step5_AE_results_input.RData
Inputs: the final dataset and decoy_pair ratings for plots and analysis.
