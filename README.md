After knitting and rendering `bird_covariateTranslator.Rmd`, overwrite this file with a symlink to `bird_covariateTranslator.md`.

E.g., on Linux/macOS from terminal:

```bash
cd /home/sdas/bird_NRV/modules/bird_covariateTranslator
rm README.md && ln -s bird_covariateTranslator.md README.md
```

## Canopy-model cache reuse

The translator prepares canopy height and closure models from PSP/SCANFI data;
these are not the fitted BAM bird-abundance models.

Fitted canopy models use the project's shared `cachePath(sim)`. Training figures
stay under `outputs/figs/xgb_height` and `outputs/figs/xgb_closure`, with
`outputs` beside the shared cache directory. This retains the project's original
figure paths even when bird predictions use ELF/replicate-specific output folders.
Training data, selected predictors, response, folds, seed and the trainer
function still determine model reuse. Unchanged training inputs can therefore
share a model across landscape replicates and bird-species selections.

The original `Cache(trainCanopyXGB, ...)` calls still include `figDir` in their
keys; no custom cache wrapper, hard-coded hashes, ELF identifiers, or migration
is used. Existing fits are reused only when the original key matches. Changed
training data/settings, a different cache root, or changed trainer code can still
require training. A cache miss runs the existing training workflow; the final fit
is covered by the outer trainer cache, not a separate new cache.

Cache hits do not recreate diagnostic figures. Module/event `.useCache` and
these explicit model-cache calls are separate. As before, trainer cache entries
do not independently track the source of every nested training helper.

Already running simulations retain their loaded functions. Restart an interrupted
replay with freshly loaded module code to use this fix; editing files does not
patch a live debugger frame. No full simulation or training is needed to run:

```r
# From this module directory, with its dependencies in .libPaths():
# Rscript --vanilla tests/test-canopy-cache.R
```
