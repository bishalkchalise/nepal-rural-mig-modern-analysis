# First-stage table

Effect of the standardised FX shock × log(2001 migration intensity) on three first-stage outcomes, across four sample restrictions on baseline migrant count.

|                                       | (1)                | (2)                       | (3)                          |
|---|---:|---:|---:|
|                                       | **Migrant-HH share** | **log(intl remit, Rs)**     | **log(# intl migrants)**       |
|                                       | Census panel       | HRVS HH panel             | HRVS HH panel                |
|                                       | muni × year        | HH × year (intensive)     | HH × year (intensive)        |
| **k ≥ 0**  (no threshold)             |  0.0060 ***        |  −0.614                   |  0.0295                      |
|                                       | (0.0021)           | (0.461)                   | (0.0224)                     |
|                                       | [+2.8 % of mean]   | [−8.8 % of mean]          | [+5.6 % of mean]             |
| **k ≥ 25**                            |  0.0063 **         |  0.497                    |  0.0368 *                    |
|                                       | (0.0028)           | (0.345)                   | (0.0190)                     |
|                                       | [+2.7 %]           | [+6.8 %]                  | [+6.9 %]                     |
| **k ≥ 50**                            |  0.0076 **         |  0.832 **                 |  0.0556 ***                  |
|                                       | (0.0034)           | (0.340)                   | (0.0180)                     |
|                                       | [+3.0 %]           | [+11.2 %]                 | [+10.2 %]                    |
| **k ≥ 100**                           |  0.0134 ***        |  1.073 ***                |  0.0525 ***                  |
|                                       | (0.0039)           | (0.333)                   | (0.0187)                     |
|                                       | [+5.0 %]           | [+13.9 %]                 | [+9.3 %]                     |
| Observations (k=0)                    | 2,142              | 7,899                     | 7,899                        |
| Clusters (muni, k=0)                  | 714                | 281                       | 281                          |
| Unique HHs (k=0)                      | —                  | 3,294                     | 3,294                        |
| Fixed effects                         | muni + year        | HH + year                 | HH + year                    |
| Controls                              | Block A, year × FX, year × MigInt | Block A, year × FX, year × MigInt | Block A, year × FX, year × MigInt |
| Cluster                               | muni (lgcode)      | muni (lgcode)             | muni (lgcode)                |

**Notes.** Reported coefficient is the interaction `fx_z × log(mig_int_z)` from the saturated regression `y_it = β·(fx_z·log(mig_int_z)) + Σ_t λ_{1,t}·mig_int_z·1{t} + Σ_t λ_{2,t}·fx_z·1{t} + Σ_k δ_k·X_k(year) + α_i + γ_t + ε_it`, where X^A are the destination-weighted Khanna Block-A covariates (six region migrant shares + 2001 destination-weighted GDP per capita, all interacted with year FE). Standard errors clustered at the municipality (lgcode) in parentheses. % of outcome mean in square brackets. *** p<0.01, ** p<0.05, * p<0.1. Treatments z-scored on the muni-year working sample after threshold filter.

**Reading.** Column (1) is the muni-level first stage: the FX-driven shock raises the share of households with an absent member by 0.6–1.3 percentage points per 1 SD of the standardised treatment, monotonically larger as the sample is restricted to munis with more 2001 baseline migrants. Columns (2) and (3) are the HH-level intensive-margin first stages: conditional on having any migrant, the same shock raises the count of international migrants per HH and the international remittance receipts per HH. Both rupee and count margins switch from null at k=0 to highly significant at k≥50, consistent with the identification strategy being binding among munis where baseline migration intensity is meaningful.
