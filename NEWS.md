# bird_covariateTranslator 0.0.1.9000 (19 November 2026)

- Keep the original direct canopy-model Cache calls and their figure-directory
  keys. Save training figures under the shared outputs/figs folder beside the
  project cache, not ELF/replicate prediction folders. Existing matching global
  height/closure fits can therefore be reused without retraining or migration.
- Remove the unconditional debugger pause after model-matrix preparation.
- Addresses CT-M3/C06 and the XGBoost pause from CT-H1/G07.

# bird_covariateTranslator 0.0.1 (02 September 2025)

- initial module version
