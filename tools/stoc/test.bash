#!/bin/sh

ic="TRUE FALSE"
smooth="TRUE FALSE"
method="gam glmmtmb"

for m in $method; do
  for i in $ic;do
    for s in $smooth;do
      printf "\n\n\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
      printf "Run test : method = "$m", smooth= "$s", ic = "$i".\n$ Rscript FunctExeTemporalAnalysisofIndicatorTrait2.r test-data/Datafilteredfortrendanalysis2.tabular tabSpecies.csv species_indicateur_fonctionnel.tabular coordCarreSTOCfaux.tabular \"ssi\" \"csi\" "$m" \"\" \"idindicatortrait\" "$s" "$i" FunctTrendSTOCGalaxy.r\n\n\n"

      Rscript FunctExeTemporalAnalysisofIndicatorTrait2.r test-data/Datafilteredfortrendanalysis2.tabular tabSpecies.csv species_indicateur_fonctionnel.tabular coordCarreSTOCfaux.tabular "ssi" "csi" $m "" "idindicatortrait" $s $i FunctTrendSTOCGalaxy.r

      cd Output
      if [ $m = "gam" ]; then
        mv csi_gammCOMPLET_France.tabular ${m}_${s}_${i}_csi_gammCOMPLET_France.tabular
        mv csi_gammParannee_France.tabular ${m}_${s}_${i}_csi_gammParannee_France.tabular
        mv figcsi_carre_France.png ${m}_${s}_${i}_figcsi_carre_France.png
        if [ $s = "TRUE" ]; then
          mv csi_gammsmoothFrance.tabular ${m}_${s}_${i}_csi_gammsmoothFrance.tabular
          mv figcsi_plotFrance.png ${m}_${s}_${i}_figcsi_plotFrance.png
        fi
      else
        mv csi_glmmTMB_France.png ${m}_${s}_${i}_csi_glmmTMB_France.png
        mv ggdata_csiFrance.tabular ${m}_${s}_${i}_ggdata_csiFrance.tabular
        mv GlmmTMB_coefficient_csiFrance.tabular ${m}_${s}_${i}_GlmmTMB_coefficient_csiFrance.tabular
      fi
      cd ../
      
    done
  done
done

echo "done, exit"
