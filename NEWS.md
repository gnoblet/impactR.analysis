# impactR.analysis 0.0.5

* New: added `auto_group_remove` parameter to `kobo_analysis()`, made to automatically remove grouping columns from the analysis (#21).
* New: added `kobo_interact()` function, a wrapper around `svy_interact()` to calculate proportions of interactions between variables.
* Change: `kobo_analysis()` and `kobo_analysis_from_dap()` calculates interactions between vars when `analysis = "interact"`.

# impactR.analysis 0.0.4

* Update: the default choices_sep is now "/".
* Update: analysis key has been updated (see #11).
* New: `kobo_analysis_from_dap_group()` is a wrapper around the `kobo_analysis_from_dap()` function, over a list of grouping vectors (#15).

# impactR.analysis 0.0.3

* Breaking change and new: the `kobo_*()` family functions make use of the `svy_*()` family and Kobo tools. Previous functions have been renamed and updated `prop_*`.
* New: `auto_kobo_analysis()` provides a very basic automated analysis, while `kobo_analysis_from_dap()` allows for providing a data analysis plan.

# impactR.analysis 0.0.2

* Breaking change: all `svy_*()` functions now uses non regex key separators if `ak = TRUE`, and this `ak` parameter gives an analysis key. It does not apply to `svy_quantile()` and `svy_test_*()` functions.

# impactR.analysis 0.0.1

* Breaking change: all `svy_*()` functions now needs quoted vectors, not `svy_test_*()` functions.
* Addendum: the `svy_*()` family now allows for multiple variables at once with arg `vars`. It does not concern `svy_test_*()` functions.(#1)
* Addendum: `proportion_select_one` also allows for multiple select ones.
* New: `svy_analysis()` is a wrapper around the `svy_*()` family. It does not concern `svy_test_*()` functions. (#2)
* New: `auto_svy_analysis()` for small and dirty automation (#2).
