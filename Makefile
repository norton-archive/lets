
REBAR?=./rebar

hets_erl_files := $(wildcard src/hets_*.erl)
lets_erl_files := $(patsubst src/hets_%,src/lets_%,$(hets_erl_files))
rets_erl_files := $(patsubst src/hets_%,src/rets_%,$(hets_erl_files))
hets_cxx_files := $(wildcard c_src/hets_*.cc c_src/hets_*.h)
lets_cxx_files := $(patsubst c_src/hets_%,c_src/lets_%,$(hets_cxx_files))
rets_cxx_files := $(patsubst c_src/hets_%,c_src/rets_%,$(hets_cxx_files))

.PHONY: all clean distclean deps compile xref doc test eunit eqc proper \
	compile-for-eunit compile-for-eqc compile-for-proper \
	realclean

all: compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean -r

distclean: clean
	rm -rf $(lets_erl_files) $(lets_cxx_files) $(rets_erl_files) $(rets_cxx_files)

compile: $(lets_erl_files) $(lets_cxx_files) $(rets_erl_files) $(rets_cxx_files)
	$(REBAR) compile

xref:
	$(REBAR) xref skip_deps=true

doc:
	@rm -rf README.md doc/edoc-info doc/*.md
	$(REBAR) -C rebar.config.doc get-deps compile
	$(REBAR) -C rebar.config.doc doc skip_deps=true

test: eunit

eunit: compile-for-eunit
	$(REBAR) eunit skip_deps=true

eqc: compile-for-eqc
	$(REBAR) eqc skip_deps=true

proper: compile-for-proper
	@echo "rebar does not implement a 'proper' command" && false

compile-for-eunit: $(lets_erl_files) $(lets_cxx_files) $(rets_erl_files) $(rets_cxx_files)
	$(REBAR) compile eunit compile_only=true

compile-for-eqc: $(lets_erl_files) $(lets_cxx_files) $(rets_erl_files) $(rets_cxx_files)
	$(REBAR) -D QC -D QC_EQC compile eqc compile_only=true

compile-for-proper: $(lets_erl_files) $(lets_cxx_files) $(rets_erl_files) $(rets_cxx_files)
	$(REBAR) -D QC -D QC_PROPER compile qc compile_only=true

realclean: clean
	rm -f $(lets_erl_files) $(lets_cxx_files) $(rets_erl_files) $(rets_cxx_files)

$(lets_erl_files): $(hets_erl_files)
	cp -f $(patsubst src/lets_%,src/hets_%,$@) $@
	@perl -i -pe 's/hets/lets/g;' $@

$(lets_cxx_files): $(hets_cxx_files)
	cp -f $(patsubst c_src/lets_%,c_src/hets_%,$@) $@
	@perl -i -pe 's/hets/lets/g;' $@
	@perl -i -pe 's/HETS/LETS/g;' $@
	@perl -i -pe 's/hyperleveldb/leveldb/g;' $@

$(rets_erl_files): $(hets_erl_files)
	cp -f $(patsubst src/rets_%,src/hets_%,$@) $@
	@perl -i -pe 's/hets/rets/g;' $@

$(rets_cxx_files): $(hets_cxx_files)
	cp -f $(patsubst c_src/rets_%,c_src/hets_%,$@) $@
	@perl -i -pe 's/hets/rets/g;' $@
	@perl -i -pe 's/HETS/RETS/g;' $@
	@perl -i -pe 's/hyperleveldb/rocksdb/g;' $@
	@perl -i -pe 's/leveldb/rocksdb/g;' $@
