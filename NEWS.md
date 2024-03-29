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
