"""
Destination-country composition: how similar are top-10 destination sets
across districts, and how stable are those patterns across years?

Produces:

  output/fig/dofe_top10_national_share_by_year.png
      Stacked area: each year's national migration broken into top-10
      countries + "all others". Shows time evolution of where Nepal
      sends migrants.

  output/fig/dofe_country_rank_lines.png
      For the top 10 destinations (by full-period total), line of
      national rank by year.

  output/fig/dofe_district_share_heatmap_full.png
      Heatmap (75 districts x top-12 destinations) of share-of-migrants
      over the FULL period. Shows whether districts have similar
      portfolios.

  output/fig/dofe_district_share_heatmap_byyear.png
      Same heatmap repeated for 2009 vs 2015 vs 2023, side-by-side, to
      show stability.

  output/tab/dofe_district_similarity.csv
      Jaccard / Spearman summary: how similar each district's top-10
      set is to the national top-10. Also pairwise district-district
      Jaccard mean / median.
"""

import os, numpy as np, pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors

os.makedirs("district-analysis/output/fig", exist_ok=True)
os.makedirs("district-analysis/output/tab", exist_ok=True)

# ---------------------------------------------------------------------------
# Load + tidy
# ---------------------------------------------------------------------------

dofe = pd.read_csv("district-analysis/data/clean/foreign_migration_district_country_annual.csv")

dofe_to_census = {"CHITWAN":"Chitawan","DHANUSHA":"Dhanusa","KAPILVASTU":"Kapilbastu",
                  "MAKAWANPUR":"Makwanpur","TANAHUN":"Tanahu","TEHRATHUM":"Terhathum",
                  "KABHREPALANCHOK":"Kavrepalanchok"}
def to_dname(s):
    u = str(s).strip().upper()
    return dofe_to_census.get(u, str(s).strip().title())

dofe = (dofe.groupby(["district_rename","country","year"]).total_migrants.sum()
              .reset_index()
              .assign(dname=lambda d: d.district_rename.map(to_dname)))

# Drop India + Nepal (consistent with rest of pipeline)
dofe = dofe[~dofe.country.isin(["Nepal","India"])].copy()

# ---------------------------------------------------------------------------
# 1. National top-10 destinations (over full period)
# ---------------------------------------------------------------------------

nat_total = (dofe.groupby("country").total_migrants.sum()
                  .sort_values(ascending=False))
TOP10 = nat_total.head(10).index.tolist()
TOP12 = nat_total.head(12).index.tolist()
print("Top 10 destinations (full period):")
for i, c in enumerate(TOP10, 1):
    print(f"  {i:>2}. {c:<22} {int(nat_total[c]):>10,}")

# ---------------------------------------------------------------------------
# 2. Stacked area: top-10 share by year (national)
# ---------------------------------------------------------------------------

yearly_country = dofe.groupby(["year","country"]).total_migrants.sum().unstack(fill_value=0)
yearly_total   = yearly_country.sum(axis=1)
yearly_top10   = yearly_country[TOP10]
yearly_other   = (yearly_country.sum(axis=1) - yearly_top10.sum(axis=1)).rename("Other (incl. India dropped)")

# Stacked area
fig, ax = plt.subplots(figsize=(11, 6))
ax.stackplot(yearly_top10.index, yearly_top10.T.values,
             labels=yearly_top10.columns, alpha=0.85)
ax.set_xlabel("Year"); ax.set_ylabel("Total migrants (national, top 10)")
ax.set_title("National DOFE migration to top-10 destinations, by year",
             fontweight="bold")
ax.legend(loc="upper left", fontsize=8, ncol=2)
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig("district-analysis/output/fig/dofe_top10_national_share_by_year.png",
            dpi=140, bbox_inches="tight")
plt.close()

# ---------------------------------------------------------------------------
# 3. Country rank lines (rank within each year)
# ---------------------------------------------------------------------------

ranks = yearly_country.rank(axis=1, ascending=False, method="min")
top10_ranks = ranks[TOP10]

fig, ax = plt.subplots(figsize=(11, 6))
colors = plt.cm.tab10(np.linspace(0, 1, 10))
for c, color in zip(TOP10, colors):
    ax.plot(top10_ranks.index, top10_ranks[c], marker="o",
            color=color, label=c, linewidth=1.8, markersize=4)
ax.invert_yaxis()    # rank 1 at top
ax.set_xlabel("Year"); ax.set_ylabel("National rank within year")
ax.set_yticks(range(1, 16))
ax.set_title("Top-10 destinations: national rank by year", fontweight="bold")
ax.legend(loc="center left", bbox_to_anchor=(1.0, 0.5), fontsize=9)
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig("district-analysis/output/fig/dofe_country_rank_lines.png",
            dpi=140, bbox_inches="tight")
plt.close()

# ---------------------------------------------------------------------------
# 4. District x country share heatmap (full period)
# ---------------------------------------------------------------------------

dist_country_full = (dofe.groupby(["dname","country"]).total_migrants.sum()
                          .unstack(fill_value=0))
dist_totals_full = dist_country_full.sum(axis=1).replace(0, np.nan)
dist_share_full = dist_country_full.div(dist_totals_full, axis=0)

# Sort districts by total migration (so heatmap reads top-down by size)
dist_order = dist_totals_full.sort_values(ascending=False).index
shares_for_heatmap = dist_share_full.loc[dist_order, TOP12]

fig, ax = plt.subplots(figsize=(10, 14))
im = ax.imshow(shares_for_heatmap.values, aspect="auto", cmap="YlOrRd",
               vmin=0, vmax=0.5)
ax.set_xticks(range(len(TOP12))); ax.set_xticklabels(TOP12, rotation=60, ha="right")
ax.set_yticks(range(len(dist_order))); ax.set_yticklabels(dist_order, fontsize=7)
ax.set_xlabel("Destination (top 12 nationally)")
ax.set_title("District × destination share of migration, full period 2009-2024",
             fontweight="bold")
fig.colorbar(im, ax=ax, shrink=0.8, label="share of district's migration")
plt.tight_layout()
plt.savefig("district-analysis/output/fig/dofe_district_share_heatmap_full.png",
            dpi=140, bbox_inches="tight")
plt.close()

# ---------------------------------------------------------------------------
# 5. Year-by-year heatmaps (2009, 2015, 2023)
# ---------------------------------------------------------------------------

SNAPSHOTS = [2009, 2015, 2023]
fig, axes = plt.subplots(1, 3, figsize=(20, 12), sharey=True)
for ax, yr in zip(axes, SNAPSHOTS):
    yr_dc = (dofe[dofe.year == yr].groupby(["dname","country"]).total_migrants.sum()
             .unstack(fill_value=0))
    totals = yr_dc.sum(axis=1).replace(0, np.nan)
    shares = yr_dc.div(totals, axis=0)
    # Reindex on full district list and TOP12
    shares = shares.reindex(index=dist_order, columns=TOP12, fill_value=0)
    im = ax.imshow(shares.values, aspect="auto", cmap="YlOrRd", vmin=0, vmax=0.5)
    ax.set_title(f"{yr}", fontweight="bold")
    ax.set_xticks(range(len(TOP12)))
    ax.set_xticklabels(TOP12, rotation=60, ha="right", fontsize=8)
    if ax is axes[0]:
        ax.set_yticks(range(len(dist_order)))
        ax.set_yticklabels(dist_order, fontsize=6)
fig.colorbar(im, ax=axes.tolist(), shrink=0.6, label="share")
fig.suptitle("District × destination share — three snapshot years",
             fontweight="bold", y=0.998)
plt.savefig("district-analysis/output/fig/dofe_district_share_heatmap_byyear.png",
            dpi=140, bbox_inches="tight")
plt.close()

# ---------------------------------------------------------------------------
# 6. Similarity to national top-10
# ---------------------------------------------------------------------------

# For each district, compute its own top-10 destinations (full period)
district_top10 = {}
for d in dist_share_full.index:
    s = dist_share_full.loc[d].sort_values(ascending=False)
    district_top10[d] = s.head(10).index.tolist()

# Jaccard with national top-10
national_set = set(TOP10)
jaccard_nat = {}
for d, tops in district_top10.items():
    s = set(tops)
    jaccard_nat[d] = len(s & national_set) / len(s | national_set)

# What fraction of district migration goes to national top-10
share_to_nat_top10 = dist_share_full[TOP10].sum(axis=1)

# Pairwise district Jaccard (mean / median)
districts = list(district_top10.keys())
jacc_pairs = []
for i, d1 in enumerate(districts):
    for d2 in districts[i+1:]:
        a, b = set(district_top10[d1]), set(district_top10[d2])
        jacc_pairs.append(len(a & b) / len(a | b))
jacc_pairs = np.array(jacc_pairs)

# Spearman: each district's rank vector vs national rank vector (over all countries)
nat_rank = nat_total.rank(ascending=False)
spearman_to_nat = {}
for d in dist_share_full.index:
    d_rank = dist_country_full.loc[d].rank(ascending=False)
    common = nat_rank.index.intersection(d_rank.index)
    rho = pd.concat([nat_rank.loc[common], d_rank.loc[common]], axis=1).corr(method="spearman").iloc[0,1]
    spearman_to_nat[d] = rho

summary = pd.DataFrame({
    "dname": list(district_top10.keys()),
    "top10_district": [", ".join(district_top10[d][:5]) + " ..." for d in district_top10],
    "share_to_national_top10":     [round(share_to_nat_top10[d], 3) for d in district_top10],
    "jaccard_with_national_top10": [round(jaccard_nat[d], 3) for d in district_top10],
    "spearman_with_national_full": [round(spearman_to_nat[d], 3) for d in district_top10],
}).sort_values("share_to_national_top10", ascending=False)

summary.to_csv("district-analysis/output/tab/dofe_district_similarity.csv", index=False)

print("\n=== District similarity to national top-10 ===")
print(f"Pairwise district top-10 Jaccard:   mean = {jacc_pairs.mean():.3f},  median = {np.median(jacc_pairs):.3f}")
print(f"District-vs-national Jaccard:       mean = {np.mean(list(jaccard_nat.values())):.3f}")
print(f"District-vs-national Spearman:      mean = {np.mean(list(spearman_to_nat.values())):.3f}")
print(f"Share of district migration in nat top-10: mean = {share_to_nat_top10.mean():.3f}, min = {share_to_nat_top10.min():.3f}")

# Show 5 most similar and 5 least similar
print("\nTop 5 most similar to national top-10:")
print(summary.head(5).to_string(index=False))
print("\nTop 5 LEAST similar (smallest top-10 coverage):")
print(summary.tail(5).to_string(index=False))

print("\nSaved:")
print("  output/fig/dofe_top10_national_share_by_year.png")
print("  output/fig/dofe_country_rank_lines.png")
print("  output/fig/dofe_district_share_heatmap_full.png")
print("  output/fig/dofe_district_share_heatmap_byyear.png")
print("  output/tab/dofe_district_similarity.csv")
