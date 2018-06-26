# Change Log

## [0.8.10](https://github.com/eproxus/meck/tree/0.8.10) (2018-06-26)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.9...0.8.10)

**Merged pull requests:**

- Test meck on travis-ci with Erlang/OTP 21.0 [\#190](https://github.com/eproxus/meck/pull/190) ([michalwski](https://github.com/michalwski))
- Add meck:expects/1,2 [\#187](https://github.com/eproxus/meck/pull/187) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.8.9](https://github.com/eproxus/meck/tree/0.8.9) (2017-11-27)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.8...0.8.9)

**Closed issues:**

- Meck doesn't support modules built with '+deterministic' compile option [\#184](https://github.com/eproxus/meck/issues/184)
- How do I run coverage results after using meck? [\#181](https://github.com/eproxus/meck/issues/181)
- Migrate to Rebar 3 and rebar3\_hex [\#155](https://github.com/eproxus/meck/issues/155)

**Merged pull requests:**

- Support running meck with modules built with '+deterministic' compile… [\#185](https://github.com/eproxus/meck/pull/185) ([nablaa](https://github.com/nablaa))
- Fix for Rebar 2.x.y on secondary arches [\#183](https://github.com/eproxus/meck/pull/183) ([lemenkov](https://github.com/lemenkov))

## [0.8.8](https://github.com/eproxus/meck/tree/0.8.8) (2017-08-29)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.7...0.8.8)

**Closed issues:**

- `{:error, {:no\_abstract\_code, ...}}` with Elixir 1.5.0-rc.0 and Erlang 20.0 [\#179](https://github.com/eproxus/meck/issues/179)

**Merged pull requests:**

- Always add debug\_info to compile opts for mocks [\#180](https://github.com/eproxus/meck/pull/180) ([ericentin](https://github.com/ericentin))

## [0.8.7](https://github.com/eproxus/meck/tree/0.8.7) (2017-06-29)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.6...0.8.7)

**Closed issues:**

- Remove dialyzer attributes when creating mock \(Erlang 20.0\) [\#178](https://github.com/eproxus/meck/issues/178)

## [0.8.6](https://github.com/eproxus/meck/tree/0.8.6) (2017-06-28)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.5...0.8.6)

## [0.8.5](https://github.com/eproxus/meck/tree/0.8.5) (2017-06-28)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.4...0.8.5)

**Implemented enhancements:**

- Add meck:get\_state/0, meck:reset\_state/0 [\#125](https://github.com/eproxus/meck/issues/125)
- Deprecate history and provide history digging functions instead  [\#85](https://github.com/eproxus/meck/issues/85)
- Mock a module only from the perspective of a specific module [\#34](https://github.com/eproxus/meck/issues/34)
- meck:app [\#16](https://github.com/eproxus/meck/issues/16)
- Mock a module only from the perspective of a specific caller \(calling process\) [\#3](https://github.com/eproxus/meck/issues/3)
- Add 19.2 to versions tested by TravisCI [\#168](https://github.com/eproxus/meck/pull/168) ([RackerJohnMadrid](https://github.com/RackerJohnMadrid))
- Add `meck:result/4-5' that returns the result value of a particular function [\#163](https://github.com/eproxus/meck/pull/163) ([amutake](https://github.com/amutake))
- fix \#88 [\#162](https://github.com/eproxus/meck/pull/162) ([yutopp](https://github.com/yutopp))

**Fixed bugs:**

- breaks on R20 [\#175](https://github.com/eproxus/meck/issues/175)
- Mocked module will return empty function clause error [\#167](https://github.com/eproxus/meck/issues/167)
- Deleting an expectation in passthrough mode does not restore original function [\#88](https://github.com/eproxus/meck/issues/88)
- Passthrough crashes without +debug\_info [\#14](https://github.com/eproxus/meck/issues/14)
- fix behavio\(u\)r attributes validation on Erlang R20 [\#176](https://github.com/eproxus/meck/pull/176) ([RoadRunnr](https://github.com/RoadRunnr))
- Fix errors in capture/5, capture/6 documentation [\#172](https://github.com/eproxus/meck/pull/172) ([marco-m](https://github.com/marco-m))
- Fix eunit compile failure on Erlang 17+ \(hamcrest\) [\#161](https://github.com/eproxus/meck/pull/161) ([marco-m](https://github.com/marco-m))

## [0.8.4](https://github.com/eproxus/meck/tree/0.8.4) (2015-12-29)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.3...0.8.4)

**Implemented enhancements:**

- Add merge\_expects option to meck\_proc [\#153](https://github.com/eproxus/meck/pull/153) ([edgurgel](https://github.com/edgurgel))

**Fixed bugs:**

- FIX: optional\_callbacks [\#151](https://github.com/eproxus/meck/pull/151) ([soranoba](https://github.com/soranoba))
- Fix race condition between meck:unload/1 and calls to the mocked module [\#150](https://github.com/eproxus/meck/pull/150) ([dszoboszlay](https://github.com/dszoboszlay))

**Merged pull requests:**

- Update 'problematic modules list' [\#156](https://github.com/eproxus/meck/pull/156) ([lilrooness](https://github.com/lilrooness))
- Document the caveat with mocking module-local calls. [\#145](https://github.com/eproxus/meck/pull/145) ([bpuzon](https://github.com/bpuzon))

## [0.8.3](https://github.com/eproxus/meck/tree/0.8.3) (2015-06-09)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.2...0.8.3)

**Implemented enhancements:**

- Update meck for OTP 18.0 [\#139](https://github.com/eproxus/meck/pull/139) ([gomoripeti](https://github.com/gomoripeti))
- Allow hiding 'on\_load' attribute. [\#131](https://github.com/eproxus/meck/pull/131) ([rlipscombe](https://github.com/rlipscombe))

**Fixed bugs:**

- Please document that modules can be not meck'able [\#135](https://github.com/eproxus/meck/issues/135)
- crypto module [\#59](https://github.com/eproxus/meck/issues/59)
- Removed test and doc from target all in Makefile [\#126](https://github.com/eproxus/meck/pull/126) ([jfacorro](https://github.com/jfacorro))

**Merged pull requests:**

- Fix typo [\#143](https://github.com/eproxus/meck/pull/143) ([derek121](https://github.com/derek121))
- Run tests in travis [\#138](https://github.com/eproxus/meck/pull/138) ([gomoripeti](https://github.com/gomoripeti))
- Fix variable exported from case [\#128](https://github.com/eproxus/meck/pull/128) ([hazardfn](https://github.com/hazardfn))

## [0.8.2](https://github.com/eproxus/meck/tree/0.8.2) (2014-05-05)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8.1...0.8.2)

**Implemented enhancements:**

- Implicit new [\#80](https://github.com/eproxus/meck/issues/80)
- Should return compilation errors [\#33](https://github.com/eproxus/meck/issues/33)
- Allow meck to build under Erlang 17.0-rc2 and Erlang R16B03-1 [\#118](https://github.com/eproxus/meck/pull/118) ([myers](https://github.com/myers))

**Fixed bugs:**

- Put non-strict option in the README.md [\#117](https://github.com/eproxus/meck/issues/117)
- With a bogus test instantiator, meck fails with {error, enoent} in meck\_cover:read\_cover\_file/1 [\#114](https://github.com/eproxus/meck/issues/114)
- Unable to mock lists module [\#87](https://github.com/eproxus/meck/issues/87)
- Do not consider a 3-tuple return value as an exception [\#113](https://github.com/eproxus/meck/pull/113) ([lucafavatella](https://github.com/lucafavatella))

**Closed issues:**

- Split tests into several test suites [\#83](https://github.com/eproxus/meck/issues/83)
- Better documentation [\#79](https://github.com/eproxus/meck/issues/79)

**Merged pull requests:**

- Add Erlang 17.0 to the test matrix [\#122](https://github.com/eproxus/meck/pull/122) ([myers](https://github.com/myers))

## [0.8.1](https://github.com/eproxus/meck/tree/0.8.1) (2013-08-29)
[Full Changelog](https://github.com/eproxus/meck/compare/0.8...0.8.1)

**Fixed bugs:**

- Attribute errors [\#110](https://github.com/eproxus/meck/pull/110) ([twonds](https://github.com/twonds))

## [0.8](https://github.com/eproxus/meck/tree/0.8) (2013-08-17)
[Full Changelog](https://github.com/eproxus/meck/compare/0.7.2...0.8)

**Implemented enhancements:**

- Warning from reltool on unexpected item `build\_dependencies` [\#92](https://github.com/eproxus/meck/issues/92)
- Capture argument [\#86](https://github.com/eproxus/meck/issues/86)
- Wait for a number of function calls [\#81](https://github.com/eproxus/meck/issues/81)
- Mocking of parameterized modules [\#4](https://github.com/eproxus/meck/issues/4)
- Allow calling original function from within expect fun [\#2](https://github.com/eproxus/meck/issues/2)
- Make remote\_setup more robust [\#109](https://github.com/eproxus/meck/pull/109) ([i11](https://github.com/i11))
- Implement 'implicit new' feature \#80 [\#104](https://github.com/eproxus/meck/pull/104) ([horkhe](https://github.com/horkhe))
- Fix warning in R16B [\#100](https://github.com/eproxus/meck/pull/100) ([rufrozen](https://github.com/rufrozen))
- Wait for a number of calls feature \(\#81\) [\#99](https://github.com/eproxus/meck/pull/99) ([horkhe](https://github.com/horkhe))
- Feature/capture [\#97](https://github.com/eproxus/meck/pull/97) ([horkhe](https://github.com/horkhe))
- Make `undefined\_module` error contain module name [\#96](https://github.com/eproxus/meck/pull/96) ([horkhe](https://github.com/horkhe))
- \(Cont.\) Make `passthrough/1' and `func/1` into a `ret\_spec`and func [\#91](https://github.com/eproxus/meck/pull/91) ([horkhe](https://github.com/horkhe))
- Introduce support for matchers: [\#89](https://github.com/eproxus/meck/pull/89) ([horkhe](https://github.com/horkhe))
- Feature/file bif passthrough [\#84](https://github.com/eproxus/meck/pull/84) ([horkhe](https://github.com/horkhe))
- Refactor meck into smaller functional modules [\#82](https://github.com/eproxus/meck/pull/82) ([horkhe](https://github.com/horkhe))
- Two new options for meck [\#77](https://github.com/eproxus/meck/pull/77) ([norton](https://github.com/norton))
- Feature/honest mocks [\#75](https://github.com/eproxus/meck/pull/75) ([horkhe](https://github.com/horkhe))
- Feature/new exception syntax [\#74](https://github.com/eproxus/meck/pull/74) ([horkhe](https://github.com/horkhe))
- Extended expect syntax and more [\#73](https://github.com/eproxus/meck/pull/73) ([horkhe](https://github.com/horkhe))
- Introduce 'stub\_all' option [\#78](https://github.com/eproxus/meck/pull/78) ([horkhe](https://github.com/horkhe))
- Support for location included in stack traces in Erlang R15 [\#52](https://github.com/eproxus/meck/pull/52) ([bjnortier](https://github.com/bjnortier))

**Fixed bugs:**

- http://eproxus.github.io/meck 404 [\#103](https://github.com/eproxus/meck/issues/103)
- R16A preview - parameterized modules are no longer supported [\#94](https://github.com/eproxus/meck/issues/94)
- meck eunit tests fail on R15B [\#51](https://github.com/eproxus/meck/issues/51)
- meck:new fails if running in embedded mode and module not loaded [\#35](https://github.com/eproxus/meck/issues/35)
- Support meck:expect with improper list mock data [\#102](https://github.com/eproxus/meck/pull/102) ([adbl](https://github.com/adbl))
- Remove unsupported option from the app.src file [\#101](https://github.com/eproxus/meck/pull/101) ([amiramix](https://github.com/amiramix))
- Remove parametrized module test [\#95](https://github.com/eproxus/meck/pull/95) ([norton](https://github.com/norton))

**Closed issues:**

- Add helper function to mock modules [\#53](https://github.com/eproxus/meck/issues/53)
- Move examples to wiki [\#10](https://github.com/eproxus/meck/issues/10)
- Write tutorial [\#9](https://github.com/eproxus/meck/issues/9)

**Merged pull requests:**

- Fix failing build. [\#98](https://github.com/eproxus/meck/pull/98) ([cmeiklejohn](https://github.com/cmeiklejohn))
- fix path of rebar [\#69](https://github.com/eproxus/meck/pull/69) ([yamt](https://github.com/yamt))

## [0.7.2](https://github.com/eproxus/meck/tree/0.7.2) (2012-05-06)
[Full Changelog](https://github.com/eproxus/meck/compare/0.7.1...0.7.2)

**Implemented enhancements:**

- Mocking of sticky modules [\#7](https://github.com/eproxus/meck/issues/7)
- Rz passthrough cover [\#56](https://github.com/eproxus/meck/pull/56) ([rzezeski](https://github.com/rzezeski))
- Mock parametrized modules [\#55](https://github.com/eproxus/meck/pull/55) ([shino](https://github.com/shino))
- Clean test directory [\#50](https://github.com/eproxus/meck/pull/50) ([norton](https://github.com/norton))
- New features - pid in history and count\_calls and wildcard\_count\_calls functions [\#40](https://github.com/eproxus/meck/pull/40) ([daha](https://github.com/daha))
- Include meck:new/2 arguments in errors [\#39](https://github.com/eproxus/meck/pull/39) ([legoscia](https://github.com/legoscia))
- .travis.yml config without rebar [\#38](https://github.com/eproxus/meck/pull/38) ([wardbekker](https://github.com/wardbekker))
- Filter out parse\_transforms from compilation options [\#32](https://github.com/eproxus/meck/pull/32) ([djnym](https://github.com/djnym))

**Fixed bugs:**

- dialyzer warnings with meck \(73c0b3e\) [\#58](https://github.com/eproxus/meck/issues/58)
- Inconsistency in documentation [\#49](https://github.com/eproxus/meck/issues/49)
- meck:unload/0 sometimes crashes [\#48](https://github.com/eproxus/meck/issues/48)
- Add test/cover\_test\_module.beam to rebar.config's clean files [\#47](https://github.com/eproxus/meck/issues/47)
- Fix typo in no\_passthrough\_cover atom [\#62](https://github.com/eproxus/meck/pull/62) ([garret-smith](https://github.com/garret-smith))

**Merged pull requests:**

- remove repetition; typo [\#57](https://github.com/eproxus/meck/pull/57) ([Erkan-Yilmaz](https://github.com/Erkan-Yilmaz))
- Update list of OTP releases in .travis.yml  [\#54](https://github.com/eproxus/meck/pull/54) ([michaelklishin](https://github.com/michaelklishin))
- Remove IDE project artifacts [\#46](https://github.com/eproxus/meck/pull/46) ([xenolinguist](https://github.com/xenolinguist))
- Making all the test funs in the foreach in meck\_test\_/0 fully qualified funs [\#44](https://github.com/eproxus/meck/pull/44) ([daha](https://github.com/daha))
- Improved tests: Added an ok in the end of the tests that use a helper function with asserts [\#43](https://github.com/eproxus/meck/pull/43) ([daha](https://github.com/daha))
- Verify history/2 returns events in the correct order & fix to flaky history\_by\_pid\_/1 test [\#42](https://github.com/eproxus/meck/pull/42) ([daha](https://github.com/daha))

## [0.7.1](https://github.com/eproxus/meck/tree/0.7.1) (2011-07-18)
[Full Changelog](https://github.com/eproxus/meck/compare/0.7...0.7.1)

**Fixed bugs:**

- Can I call original function with different arguments? [\#30](https://github.com/eproxus/meck/issues/30)

## [0.7](https://github.com/eproxus/meck/tree/0.7) (2011-07-13)
[Full Changelog](https://github.com/eproxus/meck/compare/0.6.3...0.7)

**Implemented enhancements:**

- Enable mocking of sticky modules \(not used by code\_server\) [\#29](https://github.com/eproxus/meck/pull/29) ([xenolinguist](https://github.com/xenolinguist))

## [0.6.3](https://github.com/eproxus/meck/tree/0.6.3) (2011-06-30)
[Full Changelog](https://github.com/eproxus/meck/compare/0.6.2...0.6.3)

**Implemented enhancements:**

- Interface inconsistency [\#8](https://github.com/eproxus/meck/issues/8)

**Fixed bugs:**

- Interface inconsistency [\#8](https://github.com/eproxus/meck/issues/8)

## [0.6.2](https://github.com/eproxus/meck/tree/0.6.2) (2011-06-09)
[Full Changelog](https://github.com/eproxus/meck/compare/0.6.1...0.6.2)

## [0.6.1](https://github.com/eproxus/meck/tree/0.6.1) (2011-06-08)
[Full Changelog](https://github.com/eproxus/meck/compare/0.6...0.6.1)

**Implemented enhancements:**

- use localhost for remote test rather than hostname [\#27](https://github.com/eproxus/meck/pull/27) ([joewilliams](https://github.com/joewilliams))

**Fixed bugs:**

- Makefile requires local rebar and documentation says rebar on path [\#28](https://github.com/eproxus/meck/issues/28)

## [0.6](https://github.com/eproxus/meck/tree/0.6) (2011-05-25)
[Full Changelog](https://github.com/eproxus/meck/compare/0.5.1...0.6)

## [0.5.1](https://github.com/eproxus/meck/tree/0.5.1) (2011-05-23)
[Full Changelog](https://github.com/eproxus/meck/compare/0.5...0.5.1)

## [0.5](https://github.com/eproxus/meck/tree/0.5) (2011-04-12)
**Implemented enhancements:**

- Add meck:received/3 API for easier history checking [\#23](https://github.com/eproxus/meck/pull/23) ([mbbx6spp](https://github.com/mbbx6spp))

**Closed issues:**

- dialyzer unmatched return errors [\#24](https://github.com/eproxus/meck/issues/24)



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*