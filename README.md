

talos.config

* source_dir: dir to search for .erl files to compile to memory

{source_dir, "src"}.

* code_paths: list of strings that will be prepended to code path. May include wild-card?

{code_paths, [string()]}

* results_dir: location where test run output will be captured (in sequential increasing subdirs)

{results_dir, "results"}.

* tests: list of test modules to run (order-sensitive)

{tests, [mod1, mod2]}.

* hosts: list of host + creds; verified at startup? Override from command line?

{hosts, [{Host::atom(), Username::string(), Password::string()},
         {Host::atom(), Ip::ip(), Username::string(), Password::string()}]}.

* platforms: list of regexs, modules that select a module for given host; ordered-list
{platforms, [{"