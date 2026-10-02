# =============================================================================
# report.R : builds index.html from the objects created by analysis.R
# Sourced at the end of analysis.R. Every number on the page comes from R.
# =============================================================================

esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
tidy <- function(x) sub("\\s+$", "", gsub("\t", "    ", x))      # no tabs or trailing blanks in printed R output
outp <- function(title) paste0("<pre class='out'>", paste(esc(tidy(sec[[title]])), collapse = "\n"), "</pre>")
code <- function(...) paste0("<pre class='code'>", paste(esc(c(...)), collapse = "\n"), "</pre>")
f1 <- function(x) formatC(x, format = "f", digits = 1)
f0 <- function(x) formatC(round(x), format = "d", big.mark = " ")
pval <- function(p) if (p < 0.001) "p &lt; 0.001" else if (p > 0.99) "p &gt; 0.99" else paste0("p = ", formatC(p, format = "f", digits = 3))

html_table <- function(m, first = "") {
  head <- paste0("<tr><th>", first, "</th>", paste0("<th>", colnames(m), "</th>", collapse = ""), "</tr>")
  rows <- sapply(seq_len(nrow(m)), function(i) paste0("<tr><td>", rownames(m)[i], "</td>", paste0("<td>", m[i, ], "</td>", collapse = ""), "</tr>"))
  paste0("<div class='tw'><table>", head, paste(rows, collapse = ""), "</table></div>")
}

fig <- function(file, caption, cls = "fig") {
  x <- paste(readLines(file, warn = FALSE), collapse = "\n")
  x <- sub("^<\\?xml[^>]*\\?>\\s*", "", x)
  x <- sub("(?s)<defs>\\s*<style.*?</style>\\s*</defs>\\s*", "", x, perl = TRUE)
  x <- sub("<rect width='100%' height='100%'[^>]*/>\\s*", "", x)
  x <- sub(" width='[0-9.]+pt' height='[0-9.]+pt'", "", x)
  x <- gsub(" textLength='[0-9.]+px' lengthAdjust='spacingAndGlyphs'", "", x)
  x <- gsub(" font-family: \"Liberation Sans\";", "", x, fixed = TRUE)
  x <- gsub(" font-family: \"Arial\";", "", x, fixed = TRUE)
  x <- gsub("([0-9]+\\.[0-9])[0-9]+(?![^<>]*</text>)", "\\1", x, perl = TRUE)   # one decimal for coordinates, labels untouched
  # many small circles become one path of round dots, which keeps the page light
  pts <- regmatches(x, gregexpr("<circle cx='[0-9.]+' cy='[0-9.]+' r='1.6' style='[^']*' />", x))[[1]]
  if (length(pts) > 50) {
    xy <- gsub("<circle cx='([0-9.]+)' cy='([0-9.]+)'.*", "M\\1 \\2h0", pts)
    x <- sub(pts[1], paste0("<path class='p' d='", paste(xy, collapse = ""), "'/>"), x, fixed = TRUE)
    for (p in unique(pts[-1])) x <- gsub(paste0(p, "\n"), "", x, fixed = TRUE)
  }
  x <- gsub("\n+", "\n", x)
  paste0("<figure class='", cls, "'>", x, "<figcaption>", caption, "</figcaption></figure>")
}

biz_m <- as.matrix(biz[, -1]); rownames(biz_m) <- biz$Estimate
biz_m[, "MWh_year"] <- f0(biz$MWh_year); biz_m[, "EUR_year"] <- f0(biz$EUR_year)
colnames(biz_m) <- c("kWh per tonne", "MWh per year", "EUR per year", "Payback, years")

css <- "
:root{--paper:#f6f4ee;--panel:#fff;--ink:#13212b;--ink2:#44535e;--ink3:#74828c;--line:#e1dcd0;--teal:#0e6b66;--teal2:#0a4f4c;--soft:#e2f0ee;--amber:#c4741f;--navy:#0f1d27}
*{box-sizing:border-box}
body{margin:0;background:var(--paper);color:var(--ink);font:17px/1.65 Inter,system-ui,-apple-system,'Segoe UI',Roboto,Arial,sans-serif;-webkit-font-smoothing:antialiased}
.wrap{max-width:860px;margin:0 auto;padding:0 20px}
a{color:var(--teal)}
h1,h2{font-family:'Source Serif 4',Georgia,serif;line-height:1.15;letter-spacing:-.01em;text-wrap:balance}
h1{font-size:clamp(2rem,5vw,3.1rem);margin:14px 0 16px}
h2{font-size:1.6rem;margin:56px 0 14px;padding-top:20px;border-top:1px solid var(--line)}
h2 span{color:var(--amber);font-size:1rem;display:block;font-family:Inter,system-ui,sans-serif;letter-spacing:.12em;text-transform:uppercase;font-weight:700;margin-bottom:6px}
.top{border-bottom:1px solid var(--line);font-size:.9rem}
.top .wrap{display:flex;justify-content:space-between;align-items:center;height:56px}
.top a{text-decoration:none;font-weight:600}
.eyebrow{font-size:.76rem;font-weight:700;letter-spacing:.14em;text-transform:uppercase;color:var(--teal)}
header.hero{padding:48px 0 8px}
.lede{font-size:1.14rem;color:var(--ink2)}
.tags{display:flex;flex-wrap:wrap;gap:8px;margin:18px 0 0;padding:0;list-style:none}
.tags li{font-size:.76rem;font-weight:600;background:var(--soft);color:var(--teal2);padding:5px 11px;border-radius:999px}
.answer{background:var(--navy);color:#dfe8ec;border-radius:18px;padding:26px;margin:34px 0 0}
.answer h3{margin:0 0 6px;color:#fff;font-family:'Source Serif 4',Georgia,serif;font-size:1.3rem}
.answer p{margin:0;color:#c9d6dc;font-size:.98rem}
.kpis{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin:18px 0}
.kpi{background:rgba(255,255,255,.07);border-radius:12px;padding:14px}
.kpi small{display:block;font-size:.68rem;letter-spacing:.08em;text-transform:uppercase;color:#8fa3ad}
.kpi b{display:block;font-size:1.5rem;color:#fff;font-variant-numeric:tabular-nums;line-height:1.3}
.kpi span{font-size:.8rem;color:#a9bac3}
pre{overflow-x:auto;border-radius:12px;padding:16px 18px;font:13px/1.55 ui-monospace,SFMono-Regular,Consolas,'Liberation Mono',monospace;margin:16px 0}
pre.code{background:var(--navy);color:#d7e3e8}
pre.out{background:var(--panel);border:1px solid var(--line);color:var(--ink)}
.fig{margin:22px 0;background:var(--panel);border:1px solid var(--line);border-radius:14px;padding:16px}
.fig svg{width:100%;height:auto;display:block}
.fig.narrow svg{max-width:440px;margin:0 auto}
.fig svg line,.fig svg polyline,.fig svg polygon,.fig svg path,.fig svg rect{fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round}
.fig svg text{font-family:Inter,Arial,sans-serif;white-space:pre}
.fig .p{stroke:#0e6b66;stroke-width:3.2;stroke-linecap:round;fill:none;opacity:.7}
figcaption{font-size:.86rem;color:var(--ink2);margin-top:10px}
.tw{overflow-x:auto;margin:16px 0}
table{border-collapse:collapse;width:100%;font-size:.9rem;background:var(--panel);font-variant-numeric:tabular-nums}
th,td{padding:9px 12px;border-bottom:1px solid var(--line);text-align:right;white-space:nowrap}
th:first-child,td:first-child{text-align:left}
th{font-size:.72rem;letter-spacing:.06em;text-transform:uppercase;color:var(--ink3)}
.note{background:var(--soft);border-radius:12px;padding:16px 18px;font-size:.95rem;color:var(--teal2);margin:18px 0}
.note b{color:var(--ink)}
ol.rec{padding-left:22px}
ol.rec li{margin-bottom:12px}
footer{margin-top:64px;background:var(--navy);color:#8fa3ad;font-size:.84rem;padding:24px 0}
footer a{color:#c9d6dc}
@media(max-width:640px){body{font-size:16px}.kpis{grid-template-columns:1fr}}
"

page <- c(
"<!doctype html>",
"<html lang='en'>",
"<head>",
"<meta charset='utf-8'>",
"<meta name='viewport' content='width=device-width, initial-scale=1'>",
"<title>Did the retrofit pay off? An energy analysis in R | Rajan Kumar V K</title>",
"<meta name='description' content='Hypothesis testing, difference in differences, regression diagnostics and logistic regression in R, applied to an energy efficiency investment in a packaging plant.'>",
"<link rel='preconnect' href='https://fonts.googleapis.com'>",
"<link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>",
"<link href='https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&amp;family=Source+Serif+4:opsz,wght@8..60,600;8..60,700&amp;display=swap' rel='stylesheet'>",
paste0("<style>", css, "</style>"),
"</head>",
"<body>",
"<div class='top'><div class='wrap'><a href='https://rajan56.github.io/RajanKVK03Portfolio/'>&larr; Portfolio of Rajan Kumar V K</a><a href='https://github.com/Rajan56/DataAnalyticsWith-R'>Source code</a></div></div>",
"<header class='hero'><div class='wrap'>",
"<span class='eyebrow'>Data analytics with R</span>",
"<h1>Did the heat recovery retrofit pay off?</h1>",
"<p class='lede'>A packaging plant fitted a heat recovery unit to one of its two board lines. Energy per tonne fell afterwards, and the project team reported the full drop as the saving. This analysis asks how much of it the retrofit truly caused, and what that means for the payback.</p>",
"<ul class='tags'><li>Welch t test</li><li>Difference in differences</li><li>Multiple regression</li><li>Robust standard errors</li><li>Model diagnostics</li><li>Logistic regression</li><li>Cross-validation</li></ul>",
"<div class='answer'>",
"<h3>The answer first</h3>",
sprintf("<p>The retrofit works, but it saves about %s %% less than the before and after comparison suggests. The rest of the visible drop came from warmer weather, which lowered energy use on the untouched line as well.</p>", f0(100 * (1 - eff["Estimate"] / naive))),
"<div class='kpis'>",
sprintf("<div class='kpi'><small>Causal saving</small><b>%s kWh/t</b><span>95 %% CI %s to %s</span></div>", f1(eff["Estimate"]), f1(eff["CI_low"]), f1(eff["CI_high"])),
sprintf("<div class='kpi'><small>Worth per year</small><b>EUR %s</b><span>range %s to %s</span></div>", f0(biz$EUR_year[2]), f0(eur_ci[1]), f0(eur_ci[2])),
sprintf("<div class='kpi'><small>Payback</small><b>%s years</b><span>not %s, as first reported</span></div>", f1(biz$Payback_y[2]), f1(biz$Payback_y[1])),
"</div>",
sprintf("<p>Quality did not suffer, and about %s tonnes of CO&#8322; are avoided each year. The recommendation is to extend the retrofit to line A, with the business case built on the corrected figure.</p>", f0(co2_t)),
"</div>",
"</div></header>",
"<main class='wrap'>",

"<h2><span>Step 1</span>The business question</h2>",
sprintf("<p>Line B received a heat recovery unit on its dryer section at the start of week %d. The investment was EUR %s. Management now has to decide whether to fit the same unit to line A. Three questions follow:</p>", retrofit_week, f0(investment)),
"<ol class='rec'><li>How much did the retrofit reduce energy intensity, separated from season and product mix?</li><li>Which process factors drive energy per tonne, and by how much?</li><li>Did the retrofit affect product quality?</li></ol>",
"<div class='note'><b>About the data.</b> The company is fictional and the data are simulated in R with a fixed seed. A true retrofit effect of -38 kWh per tonne was built into the simulation, which makes it possible to check whether each method recovers the right answer. The methods are the ones I apply to real plant data.</div>",

"<h2><span>Step 2</span>Data and quality check</h2>",
sprintf("<p>Daily records for two lines over %d weeks: energy intensity, throughput, raw material moisture, outdoor temperature, product grade, number of changeovers and whether the day had a quality deviation.</p>", weeks),
outp("Rows read, rows used"),
outp("Summary of numeric variables"),

"<h2><span>Step 3</span>The obvious test, and why it misleads</h2>",
"<p>The first instinct is to compare line B before and after the retrofit with a two-sample t test.</p>",
code("b   <- d[d$line == \"B\", ]", "t_b <- t.test(energy_kwh_t ~ period, data = b)   # Welch test, unequal variances"),
outp("Welch t test, line B, before against after"),
sprintf("<p>The drop is %s kWh per tonne and highly significant. The problem appears when the same test is run on line A, which had no retrofit:</p>", f1(-naive)),
outp("Welch t test, line A (no retrofit), before against after"),
sprintf("<p>Line A also fell, by %s kWh per tonne. The before period was winter and the after period was summer. A significant t test shows that something changed. It does not show what caused it.</p>", f1(t_a$estimate[1] - t_a$estimate[2])),
fig("figures/fig1_weekly.svg", "Figure 1. Weekly mean energy intensity by line. Both lines drift down as the weather warms. The retrofit effect is the extra gap that opens on line B."),

"<h2><span>Step 4</span>Difference in differences</h2>",
"<p>Line A serves as the control. The change on line B minus the change on line A removes everything the two lines share, such as season and energy mix. In a regression this is the interaction between line and period. Adding the process covariates removes day to day differences in what each line was producing.</p>",
code("did_adj <- lm(energy_kwh_t ~ line * period + throughput_tph + moisture_pct +",
     "                outdoor_temp_c + grade + changeovers, data = d)",
     "robust_table(did_adj)      # HC3 heteroskedasticity robust standard errors"),
outp("Difference in differences with process covariates (robust SE)"),
outp("Model fit, adjusted model"),
sprintf("<p>The row <b>lineB:periodAfter</b> is the retrofit effect: %s kWh per tonne (95 %% CI %s to %s, %s). That is very close to the true value of -38 built into the data. Without covariates the same design gives %s, which is less precise because the two lines happened to run a different product mix in the two periods.</p>", f1(eff["Estimate"]), f1(eff["CI_low"]), f1(eff["CI_high"]), pval(eff["p"]), f1(eff_s["Estimate"])),
fig("figures/fig2_estimates.svg", "Figure 2. Three estimates of the same effect. The naive comparison overstates the saving. The adjusted difference in differences is both closer to the truth and the most precise."),

"<h2><span>Step 5</span>What drives energy per tonne</h2>",
"<p>The same model answers the second question. Read from the table above:</p>",
"<ul>",
sprintf("<li><b>Throughput.</b> Each extra tonne per hour lowers energy intensity by %s kWh per tonne. Running the line full is an energy measure.</li>", f1(-tab_adj["throughput_tph", "Estimate"])),
sprintf("<li><b>Moisture.</b> Each percentage point of raw material moisture adds %s kWh per tonne. Supplier specifications have an energy price.</li>", f1(tab_adj["moisture_pct", "Estimate"])),
sprintf("<li><b>Changeovers.</b> Each changeover in a day adds %s kWh per tonne, which gives scheduling a measurable energy value.</li>", f1(tab_adj["changeovers", "Estimate"])),
sprintf("<li><b>Grade.</b> Heavy grade needs %s kWh per tonne more than standard, so a fair energy KPI must be adjusted for mix.</li>", f1(tab_adj["gradeHeavy", "Estimate"])),
sprintf("<li><b>Weather.</b> Each degree of outdoor temperature lowers intensity by %s kWh per tonne.</li>", f1(-tab_adj["outdoor_temp_c", "Estimate"])),
"</ul>",
sprintf("<p>Together these explain %s %% of the daily variation.</p>", f0(100 * summary(did_adj)$r.squared)),

"<h2><span>Step 6</span>Can the model be trusted?</h2>",
"<p>A result that will carry an investment decision deserves checks. Four were run.</p>",
"<p><b>Parallel trends.</b> Difference in differences assumes the lines moved together before the retrofit. A line by week interaction in the pre period tests this:</p>",
outp("Parallel trends check: line by week interaction in the pre period"),
sprintf("<p>No evidence of diverging trends (%s).</p>", pval(pt_row["Pr(>|t|)"])),
"<p><b>Multicollinearity.</b> Variance inflation factors are all below 5:</p>",
outp("Variance inflation factors"),
"<p><b>Residuals.</b> Normality and constant variance are not rejected, and the residual plot shows no pattern:</p>",
outp("Residual checks"),
fig("figures/fig3_residuals.svg", "Figure 3. Residuals against fitted values for the adjusted model, with a smoothed trend line. The flat band supports the linear specification."),

"<h2><span>Step 7</span>Did quality suffer?</h2>",
"<p>An energy saving that raises scrap is no saving. Quality deviation days on line B, before and after:</p>",
outp("Quality deviations on line B, before and after"),
sprintf("<p>The deviation rate is unchanged (chi-squared test, %s). A logistic regression across both lines confirms it while controlling for the process conditions:</p>", pval(q_test$p.value)),
code("logit_m <- glm(quality_dev ~ moisture_pct + changeovers + grade + throughput_tph + treated,",
     "               data = d, family = binomial)"),
outp("Logistic regression for a quality deviation day (odds ratios)"),
sprintf("<p>The retrofit has no detectable effect on quality (odds ratio %s, %s). What does raise the risk is moisture, changeovers and heavy grade. Under ten-fold cross-validation the model reaches an AUC of %s, against %s in sample. That is useful for ranking risky days and too weak to automate a decision, which is worth saying plainly.</p>", formatC(or_tab["treated", "Odds_ratio"], format = "f", digits = 2), pval(or_tab["treated", "p"]), formatC(auc_cv, format = "f", digits = 2), formatC(auc_in, format = "f", digits = 2)),
fig("figures/fig4_roc.svg", "Figure 4. ROC curve from out-of-fold predictions. Cross-validation gives the figure to expect on new data.", "fig narrow"),

"<h2><span>Step 8</span>From coefficient to euros</h2>",
sprintf("<p>Line B produces about %s tonnes a year. At EUR %d per MWh:</p>", f0(tonnes), energy_price),
html_table(biz_m, "Estimate"),
sprintf("<p>The corrected saving is EUR %s a year, with a 95 %% range of EUR %s to %s, and about %s tonnes of CO&#8322; avoided at an assumed %s kg per kWh. Payback moves from %s to %s years. The investment still clears a normal hurdle, yet a business case for line A written on the naive figure would have overstated the benefit by EUR %s a year.</p>", f0(biz$EUR_year[2]), f0(eur_ci[1]), f0(eur_ci[2]), f0(co2_t), formatC(co2_factor, format = "f", digits = 2), f1(biz$Payback_y[1]), f1(biz$Payback_y[2]), f0(biz$EUR_year[1] - biz$EUR_year[2])),

"<h2><span>Step 9</span>Recommendations</h2>",
"<ol class='rec'>",
sprintf("<li><b>Approve the retrofit for line A</b> on the corrected saving of about %s kWh per tonne, and report the range, not a single number.</li>", f1(-eff["Estimate"])),
"<li><b>Change the energy KPI</b> to a weather and mix adjusted figure. The raw kWh per tonne rewards summer and punishes heavy grade.</li>",
"<li><b>Treat scheduling and moisture as energy levers.</b> Fewer changeovers and tighter incoming moisture cost little and are visible in the model.</li>",
"<li><b>Use the quality model as a watch list</b> for high-risk days, and collect more data before relying on it further.</li>",
"</ol>",

"<h2><span>Notes</span>Limits and how to reproduce</h2>",
"<p>The data are simulated, so real plant data would bring autocorrelation, sensor drift and unrecorded events that need their own treatment. With one treated line and one control, the design cannot separate the retrofit from any other change made to line B in the same week. Days are treated as independent observations.</p>",
"<p>To reproduce every number and figure on this page, download <a href='https://github.com/Rajan56/DataAnalyticsWith-R'>the two R scripts</a> into one folder and run:</p>",
code("source(\"analysis.R\")"),
sprintf("<p>This page was generated by %s. No packages beyond base R are required; svglite is used for the figures when it is installed.</p>", R.version.string),
"</main>",
"<footer><div class='wrap'>&copy; 2026 Rajan Kumar V K, D.Sc. (Tech.). Fictional company, simulated data. <a href='https://rajan56.github.io/RajanKVK03Portfolio/'>Back to portfolio</a></div></footer>",
"</body>",
"</html>"
)
writeLines(page, "index.html")
