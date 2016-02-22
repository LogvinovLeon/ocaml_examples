all: build

build: gen
	corebuild -pkg core_extended,yojson,atdgen github_core_info.native

gen: deps
	atdgen -t github.atd
	atdgen -j github.atd
	atdgen -t github_org.atd
	atdgen -j github_org.atd

deps:
	opam install core_extended yojson atdgen core

clean:
	rm github_j* github_t* github_org_j* github_org_t*
