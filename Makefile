build:
	ocaml pkg/build.ml native=true native-dynlink=true

test: build
	rm _build/src_test/ -rf
	ocamlbuild -classic-display -use-ocamlfind src_test/test_ppx_import.byte --

clean:
	ocamlbuild -clean

.PHONY: build test clean

release:
	@if [ -z "$(VERSION)" ] || [ -z "$(OPAMREPO)" ]; then \
		echo "Usage: make release VERSION=1.0 OPAMREPO=~/opam-repository"; exit 1; fi
	@echo ">>>>>> Releasing the package"
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	@echo ">>>>>> Updating OPAM repository"
	repo="$$(git config remote.origin.url | cut -d: -f2)" \
	 user="$$(echo $${repo} | cut -d/ -f1)" \
	 pkg="$$(echo $${repo} | cut -d/ -f2 | sed s/.git//)" \
	 url="https://github.com/$${repo}/archive/v$(VERSION).tar.gz" \
	 opam="$(OPAMREPO)/packages/$${pkg}/$${pkg}.$(VERSION)"; \
	 set +e; \
	 mkdir -p $${opam}; \
	 cp descr opam $${opam}; \
	 echo "archive: \"$${url}\"" >$${opam}/url; \
	 echo "checksum: \"$$(curl -sL $${url} | md5sum | cut -d' ' -f1)\"" >>$${opam}/url; \
	 git -C $(OPAMREPO) pull --rebase git@github.com:ocaml/opam-repository master; \
	 git -C $(OPAMREPO) add packages/$${pkg}/$${pkg}.$(VERSION); \
	 git -C $(OPAMREPO) commit -m "+$${pkg}.$(VERSION)"; \
	 git -C $(OPAMREPO) push --force origin master; \
	 x-www-browser "https://github.com/$${user}/opam-repository/compare/ocaml:master...master"

.PHONY: release
