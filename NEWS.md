# impactR.analysis 0.0.1

* Breaking change: all `svy_*()` functions now needs quoted vectors, not `svy_test_*()` functions.
* Addendum: the `svy_*()` family now allows for multiple variables at once with arg `vars`. It does not concern `svy_test_*()` functions.(#1)
* Addendum: `proportion_select_one` also allows for multiple select ones.
* New: `svy_analysis()` is a wrapper around the `svy_*()` family. It does not concern `svy_test_*()` functions. (#2)
* New: `auto_svy_analysis()` for small and dirty automation (#2).
